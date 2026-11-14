{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Database.DuckDB.Simple.Result
-- Description : Shared result decoding and cursor ownership for both execution modes.
module Database.DuckDB.Simple.Result (
  collectRows,
  collectParsedRows,
  foldStatementWith,
  nextRowWith,
  resetStatementStream,
) where

import Control.Exception (bracket, evaluate, finally, mask, mask_, onException, throwIO)
import Control.Monad (foldM, forM, when, zipWithM)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Database.DuckDB.Simple.FromField (Field (..), FieldValue)
import Database.DuckDB.Simple.FromRow (RowParser, parseRow, rowErrorsToSqlError)
import Database.DuckDB.Simple.Internal
import Database.DuckDB.Simple.Materialize (FallbackReaders (..), prepareGeometryReader, prepareVariantReader, prepareVectorReader)
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.TypeCache (withTypeConnection)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

-- | Fold rows from one statement and release its result on every exit path.
foldStatementWith :: ResultMode -> RowParser row -> Statement -> a -> (a -> row -> IO a) -> IO a
foldStatementWith mode parser stmt initial step =
  let loop acc = do
        nextVal <- nextRowWith mode parser stmt
        case nextVal of
          Nothing -> pure acc
          Just row -> do
            acc' <- step acc row
            acc' `seq` loop acc'
  in  loop initial `finally` resetStatementStream stmt

-- | Read one row. The first fetch selects execution until the cursor resets.
nextRowWith :: ResultMode -> RowParser r -> Statement -> IO (Maybe r)
nextRowWith mode parser stmt@Statement{statementStream} =
  withStatementHandle stmt \_ -> mask \restore -> do
    state <- readIORef statementStream
    case state of
      StatementStreamExhausted -> pure Nothing
      StatementStreamIdle -> do
        newStream <- startStatementStream mode stmt
        case newStream of
          Nothing -> writeIORef statementStream StatementStreamExhausted >> pure Nothing
          Just stream -> do
            writeIORef statementStream (StatementStreamActive stream)
            restore (consumeStream statementStream parser stmt stream)
              `onException` exhaustStatementStream statementStream
      StatementStreamActive stream ->
        restore (consumeStream statementStream parser stmt stream)
          `onException` exhaustStatementStream statementStream

-- | Release an active cursor and allow the statement to execute again.
resetStatementStream :: Statement -> IO ()
resetStatementStream stmt@Statement{statementStream} =
  cleanupStatementStreamRef statementStream `finally` clearStatementSchemas stmt

consumeStream :: IORef StatementStreamState -> RowParser r -> Statement -> StatementStream -> IO (Maybe r)
consumeStream streamRef parser stmt stream = mask \restore -> do
  loaded <- case statementStreamChunk stream of
    Nothing -> restore (fetchChunk streamRef (statementConnection stmt) (statementQuery stmt) stream)
    Just _ -> pure stream
  writeIORef streamRef (StatementStreamActive loaded)
  case statementStreamChunk loaded of
    Nothing -> exhaustStatementStream streamRef >> pure Nothing
    Just chunk -> do
      fields <-
        restore $ buildMaterializedRow (statementStreamColumns loaded) (statementStreamChunkReaders chunk) (statementStreamChunkIndex chunk)
      parsed <- restore (evaluate (parseRow parser fields))
      case parsed of
        Errors rowErr -> throwIO $ rowErrorsToSqlError (statementQuery stmt) rowErr
        Ok value -> do
          let nextIndex = statementStreamChunkIndex chunk + 1
          if nextIndex < statementStreamChunkSize chunk
            then writeIORef streamRef (StatementStreamActive loaded{statementStreamChunk = Just chunk{statementStreamChunkIndex = nextIndex}})
            else do
              writeIORef streamRef (StatementStreamActive loaded{statementStreamChunk = Nothing})
              finalizeChunk chunk
          pure (Just value)

-- | Release the cursor and retain its exhausted state until an explicit reset.
exhaustStatementStream :: IORef StatementStreamState -> IO ()
exhaustStatementStream ref = mask_ do
  state <- atomicModifyIORef' ref (StatementStreamExhausted,)
  case state of
    StatementStreamActive stream -> finalizeStream stream
    _ -> pure ()

startStatementStream :: ResultMode -> Statement -> IO (Maybe StatementStream)
startStatementStream mode stmt = withQueryError (statementQuery stmt) . withResultWindow (statementConnection stmt) (mapM_ finalizeStream) $ mask_ do
  -- A materialized result closes here. A stream stays open after the lock is released.
  result <- executePreparedResult stmt
  startResultStream mode (statementConnection stmt) result

-- | Take ownership of a cursor and retain its chunks in the selected mode.
-- A changed-row result gives its row with the count, as a query result gives its rows.
startResultStream :: ResultMode -> Connection -> DuckDBV2ResultHandle -> IO (Maybe StatementStream)
startResultStream mode conn result = mask_ do
  owned <- newIORef []
  cursor <- newIORef result
  -- Take each handle out before its release, so that a second call releases no handle again.
  let destroyResult = atomicModifyIORef' cursor (nullHandle,) >>= destroy c_duckdb_v2_result_destroy
      release = (atomicModifyIORef' owned ([],) >>= releaseItems destroyDataChunk) `finally` destroyResult
      fetch = fetchResultChunk conn result
      gather = do
        chunk <- fetch
        when (chunk /= nullHandle) do
          atomicModifyIORef' owned (\chunks -> (chunk : chunks, ()))
          gather
  flip onException release do
    -- Expanded statements expose metadata after the first native fetch.
    first <- fetch
    when (first /= nullHandle) $
      atomicModifyIORef' owned (\chunks -> (first : chunks, ()))
    resultType <- output (c_duckdb_v2_result_get_result_type result)
    if resultType /= DuckDBV2ResultTypeQueryResult && resultType /= DuckDBV2ResultTypeChangedRows
      then do
        _ <- runInterruptibleQuery conn (output (c_duckdb_v2_result_drain result))
        release
        pure Nothing
      else do
        columns <- collectResultColumns result
        case mode of
          MaterializedResult -> do
            gather
            chunks <- reverse <$> readIORef owned
            destroyResult
            pure (Just (StatementStream nullHandle columns chunks Nothing))
          StreamingResult -> do
            chunks <- reverse <$> readIORef owned
            pure (Just (StatementStream result columns chunks Nothing))

fetchChunk :: IORef StatementStreamState -> Connection -> Query -> StatementStream -> IO StatementStream
fetchChunk ref conn queryText stream = withQueryError queryText $ mask \restore -> do
  (chunk, remaining) <- case statementStreamPendingChunks stream of
    next : rest -> pure (next, rest)
    [] | statementStreamResult stream == nullHandle -> pure (nullHandle, [])
    [] -> (,[]) <$> fetchResultChunk conn (statementStreamResult stream)
  let nextStream = stream{statementStreamPendingChunks = remaining}
  if chunk == nullHandle
    then pure nextStream
    else do
      -- Transfer ownership before native metadata or reader setup can fail.
      let owned = nextStream{statementStreamChunk = Just (StatementStreamChunk chunk 0 0 [])}
      writeIORef ref (StatementStreamActive owned)
      rowCount <- fromIntegral <$> output (c_duckdb_v2_data_chunk_get_size chunk)
      if rowCount <= 0
        then do
          writeIORef ref (StatementStreamActive nextStream)
          destroyDataChunk chunk
          restore (fetchChunk ref conn queryText nextStream)
        else do
          readers <- restore (prepareChunkReaders conn chunk (statementStreamColumns stream))
          pure nextStream{statementStreamChunk = Just (StatementStreamChunk chunk rowCount 0 readers)}

-- | Prepare readers that borrow the vectors in an owned chunk. The readers read
-- each type from its vector. The header of @duckdb_v2_result_get_schema@ calls
-- the result schema the authoritative source of the column types, and says that
-- vectors do not carry their own type. But the header of
-- @duckdb_v2_vector_get_logical_type@ says that a vector has the type that was
-- set on creation, and the header of @duckdb_v2_vector_get_view@ says that the
-- data has the layout that the logical type of the vector implies. So the
-- readers use the vector type.
prepareChunkReaders :: Connection -> DuckDBV2DataChunkHandle -> [StatementStreamColumn] -> IO [Int -> IO FieldValue]
prepareChunkReaders conn chunk columns = withTypeCache conn \cache -> withConnectionHandle conn \native -> withConnectionFactory native \factory -> do
  let readVariant vector = withTypeConnection cache (\helper state -> prepareVariantReader helper state vector)
  forM columns \StatementStreamColumn{statementStreamColumnIndex} ->
    output (c_duckdb_v2_data_chunk_get_vector chunk (fromIntegral statementStreamColumnIndex))
      >>= prepareVectorReader (FallbackReaders readVariant (prepareGeometryReader factory))

-- | Decode all rows after the statement has materialized its native chunks.
collectRows :: Statement -> IO [[Field]]
collectRows stmt = collectStreamRows (statementConnection stmt) (statementQuery stmt) (startStatementStream MaterializedResult stmt)

-- | Execute a parsed statement directly and materialize its owned native chunks.
collectParsedRows :: Connection -> Query -> DuckDBV2SqlStatementHandle -> IO [[Field]]
collectParsedRows conn query parsed = collectStreamRows conn query . withResultWindow conn (mapM_ finalizeStream) $ mask_ do
  result <- executeParsedResult conn query parsed
  startResultStream MaterializedResult conn result

-- | Decode owned materialized chunks and release them on every exit path.
-- Destroy each chunk after its rows are decoded.
collectStreamRows :: Connection -> Query -> IO (Maybe StatementStream) -> IO [[Field]]
collectStreamRows conn query acquire = withQueryError query $
  bracket (acquire >>= traverse adopt) (mapM_ release) \case
    Nothing -> pure []
    Just (owned, pending) -> do
      -- Both loops keep a constant stack. Each c_duckdb_v2 binding is a
      -- safe foreign call. At a safe call, the GHC runtime (threadPaused)
      -- reads the frames of the current stack chunk. A loop that keeps a
      -- frame for each element thus makes each native call slower as the
      -- list grows.
      --
      -- The chunk loop must keep a constant stack, because the code for
      -- each chunk makes native calls.
      --
      -- The reference holds the chunks that the fold has not reached, in
      -- the order of the fold. Each step takes its chunk out of the
      -- reference and destroys that chunk after its rows. The bracket of
      -- the step thus owns the chunk while its rows are decoded, and the
      -- release of this function destroys only the chunks in the reference.
      let readChunk done expected = bracket (takeChunk pending expected) destroyDataChunk \chunk -> do
            rowCount <- fromIntegral <$> output (c_duckdb_v2_data_chunk_get_size chunk)
            readers <- prepareChunkReaders conn chunk (statementStreamColumns owned)
            -- The row loop reads the rows from the last to the first, and
            -- puts each row in front of the list. The list then needs no
            -- reverse. The constant stack of this loop is necessary for a
            -- reader that makes a native call for each row: a BIGNUM
            -- column, and the first use of a UNION member.
            let readFrom row decoded
                  | row < 0 = pure decoded
                  | otherwise = buildMaterializedRow (statementStreamColumns owned) readers row >>= \fields -> readFrom (row - 1) (fields : decoded)
            (: done) <$> readFrom (rowCount - 1) []
      -- The fold gives the chunks from the last to the first.
      rows <- foldM readChunk [] (statementStreamPendingChunks owned)
      pure (concat (reverse rows))
  where
    adopt stream = (,) stream <$> newIORef (statementStreamPendingChunks stream)
    release (stream, pending) = do
      remaining <- atomicModifyIORef' pending ([],)
      finalizeStream stream{statementStreamPendingChunks = remaining}
    -- Take the first chunk out of the reference. It must be the chunk of the
    -- fold step. Otherwise leave the reference as it is, so that the release
    -- destroys each chunk in it.
    takeChunk pending expected =
      atomicModifyIORef' pending (\case chunk : rest | chunk == expected -> (rest, Just chunk); chunks -> (chunks, Nothing))
        >>= maybe (throwIO (userError "duckdb-simple: internal error: the owned chunks do not match the chunk fold")) pure

collectResultColumns :: DuckDBV2ResultHandle -> IO [StatementStreamColumn]
collectResultColumns result = bracket (output (c_duckdb_v2_result_get_schema result)) (destroy c_duckdb_v2_schema_destroy) \schema -> do
  count <- fromIntegral <$> output (c_duckdb_v2_schema_get_count schema)
  forM [0 .. count - 1] \idx -> alloca \name -> do
    _ <- output (c_duckdb_v2_schema_get_field schema (fromIntegral idx) name)
    columnName <- peek name >>= readIdentifier
    pure (StatementStreamColumn idx columnName)

buildMaterializedRow :: [StatementStreamColumn] -> [Int -> IO FieldValue] -> Int -> IO [Field]
buildMaterializedRow columns readers rowIdx =
  zipWithM (buildMaterializedField rowIdx) columns readers

buildMaterializedField :: Int -> StatementStreamColumn -> (Int -> IO FieldValue) -> IO Field
buildMaterializedField rowIdx column readValue = do
  value <- readValue rowIdx
  pure
    Field
      { fieldName = statementStreamColumnName column
      , fieldIndex = statementStreamColumnIndex column
      , fieldValue = value
      }
