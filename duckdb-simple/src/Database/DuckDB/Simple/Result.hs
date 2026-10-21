{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

-- | Shared result decoding and cursor ownership for both execution modes.
module Database.DuckDB.Simple.Result (
    collectRows,
    collectParsedRows,
    foldStatementWith,
    nextRowWith,
    resetStatementStream,
    cleanupStatementStreamRef,
) where

import Control.Exception (bracket, evaluate, finally, mask, mask_, onException, throwIO)
import Control.Monad (forM, when, zipWithM)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Database.DuckDB.Simple.FromField (Field (..), FieldValue)
import Database.DuckDB.Simple.FromRow (RowParser, parseRow, rowErrorsToSqlError)
import Database.DuckDB.Simple.Internal
import Database.DuckDB.Simple.Materialize (prepareVariantReader, prepareVectorReader)
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
     in loop initial `finally` resetStatementStream stmt

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
            fields <- restore $ buildMaterializedRow (statementStreamColumns loaded) (statementStreamChunkReaders chunk) (statementStreamChunkIndex chunk)
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
startStatementStream mode stmt = withQueryError (statementQuery stmt) $ mask_ do
    result <- executePreparedResult stmt
    startResultStream mode (statementConnection stmt) result

-- | Take ownership of a cursor and retain its chunks in the selected mode.
startResultStream :: ResultMode -> Connection -> DuckDBV2ResultHandle -> IO (Maybe StatementStream)
startResultStream mode conn result = mask_ do
    owned <- newIORef []
    let release = (readIORef owned >>= mapM_ destroyDataChunk) `finally` destroy c_duckdb_v2_result_destroy result
        fetch = fetchResultChunk mode conn result
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
        if resultType /= DuckDBV2ResultTypeQueryResult
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
                        destroy c_duckdb_v2_result_destroy result
                        pure (Just (StatementStream nullHandle columns chunks Nothing mode))
                    StreamingResult -> do
                        chunks <- reverse <$> readIORef owned
                        pure (Just (StatementStream result columns chunks Nothing mode))

fetchChunk :: IORef StatementStreamState -> Connection -> Query -> StatementStream -> IO StatementStream
fetchChunk ref conn queryText stream = withQueryError queryText $ mask \restore -> do
    (chunk, remaining) <- case statementStreamPendingChunks stream of
        next : rest -> pure (next, rest)
        [] | statementStreamResult stream == nullHandle -> pure (nullHandle, [])
        [] -> (,[]) <$> fetchResultChunk (statementStreamMode stream) conn (statementStreamResult stream)
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

-- | Prepare readers that borrow the vectors in an owned chunk.
prepareChunkReaders :: Connection -> DuckDBV2DataChunkHandle -> [StatementStreamColumn] -> IO [Int -> IO FieldValue]
prepareChunkReaders conn chunk columns = withTypeCache conn \cache ->
    forM columns \StatementStreamColumn{statementStreamColumnIndex} ->
        output (c_duckdb_v2_data_chunk_get_vector chunk (fromIntegral statementStreamColumnIndex)) >>= prepareVectorReader (\vector -> withTypeConnection cache (\helper -> prepareVariantReader helper vector))

finalizeStream :: StatementStream -> IO ()
finalizeStream StatementStream{statementStreamResult, statementStreamChunk, statementStreamPendingChunks} =
    maybe (pure ()) finalizeChunk statementStreamChunk
        `finally` (mapM_ destroyDataChunk statementStreamPendingChunks `finally` destroy c_duckdb_v2_result_destroy statementStreamResult)

finalizeChunk :: StatementStreamChunk -> IO ()
finalizeChunk StatementStreamChunk{statementStreamChunkPtr} = destroyDataChunk statementStreamChunkPtr

-- | Decode all rows after the statement has materialized its native chunks.
collectRows :: Statement -> IO [[Field]]
collectRows stmt = collectStreamRows (statementConnection stmt) (statementQuery stmt) (startStatementStream MaterializedResult stmt)

-- | Execute a parsed statement directly and materialize its owned native chunks.
collectParsedRows :: Connection -> Query -> DuckDBV2SqlStatementHandle -> IO [[Field]]
collectParsedRows conn query parsed = collectStreamRows conn query $ mask_ do
    result <- executeParsedResult conn query parsed
    startResultStream MaterializedResult conn result

-- | Decode owned materialized chunks and release them on every exit path.
collectStreamRows :: Connection -> Query -> IO (Maybe StatementStream) -> IO [[Field]]
collectStreamRows conn query acquire = withQueryError query $
    bracket acquire (mapM_ finalizeStream) \case
        Nothing -> pure []
        Just owned -> do
            rows <- forM (statementStreamPendingChunks owned) \chunk -> do
                rowCount <- fromIntegral <$> output (c_duckdb_v2_data_chunk_get_size chunk)
                readers <- prepareChunkReaders conn chunk (statementStreamColumns owned)
                mapM (buildMaterializedRow (statementStreamColumns owned) readers) [0 .. rowCount - 1]
            pure (concat rows)

collectResultColumns :: DuckDBV2ResultHandle -> IO [StatementStreamColumn]
collectResultColumns result = bracket (output (c_duckdb_v2_result_get_schema result)) (destroy c_duckdb_v2_schema_destroy) \schema -> do
    count <- fromIntegral <$> output (c_duckdb_v2_schema_get_count schema)
    forM [0 .. count - 1] \idx -> alloca \name -> do
        dtype <- output (c_duckdb_v2_schema_get_field schema (fromIntegral idx) name)
        columnName <- peek name >>= readIdentifier
        typeId <- output (c_duckdb_v2_logical_type_get_id dtype)
        pure (StatementStreamColumn idx columnName typeId)

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
