{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

-- | Shared result decoding and cursor ownership for both execution modes.
module Database.DuckDB.Simple.Result (
    collectRows,
    foldStatementWith,
    nextRowWith,
    resetStatementStream,
    cleanupStatementStreamRef,
) where

import Control.Exception (bracket, evaluate, finally, mask, mask_, onException, throwIO)
import Control.Monad (forM, when, zipWithM)
import Data.IORef (IORef, atomicModifyIORef', readIORef, writeIORef)
import qualified Data.Text as Text
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..))
import Database.DuckDB.Simple.FromRow (RowParser, parseRow, rowErrorsToSqlError)
import Database.DuckDB.Simple.Internal
import Database.DuckDB.Simple.Materialize (materializeValue, prepareGeometryDecoder)
import Database.DuckDB.Simple.Ok (Ok (..))
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (sizeOf)

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
resetStatementStream Statement{statementStream} =
    cleanupStatementStreamRef statementStream

consumeStream :: IORef StatementStreamState -> RowParser r -> Statement -> StatementStream -> IO (Maybe r)
consumeStream streamRef parser stmt stream = mask \restore -> do
    loaded <- case statementStreamChunk stream of
        Nothing -> fetchChunk (statementConnection stmt) (statementQuery stmt) stream
        Just _ -> pure stream
    writeIORef streamRef (StatementStreamActive loaded)
    case statementStreamChunk loaded of
        Nothing -> exhaustStatementStream streamRef >> pure Nothing
        Just chunk -> do
            fields <- restore $ buildMaterializedRow (statementStreamColumns loaded) (statementStreamChunkVectors chunk) (statementStreamChunkIndex chunk)
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
    finalizeStreamState state

startStatementStream :: ResultMode -> Statement -> IO (Maybe StatementStream)
startStatementStream mode stmt =
    withStatementHandle stmt \handle -> do
        resultPtr <- malloc
        fillBytes resultPtr 0 (sizeOf (undefined :: DuckDBResult))
        let release = c_duckdb_destroy_result resultPtr `finally` free resultPtr
        flip onException release do
            rc <- runInterruptibleQuery (statementConnection stmt) (executePreparedResult mode handle resultPtr)
            when (rc /= DuckDBSuccess) do
                (errMsg, errType) <- fetchResultError resultPtr
                throwIO $ mkExecuteError (statementQuery stmt) errMsg errType
            resultType <- c_duckdb_result_return_type resultPtr
            if resultType /= DuckDBResultTypeQueryResult
                then release >> pure Nothing
                else do
                    columns <- collectResultColumns resultPtr
                    pure (Just (StatementStream resultPtr columns Nothing mode))

fetchChunk :: Connection -> Query -> StatementStream -> IO StatementStream
fetchChunk conn queryText stream@StatementStream{statementStreamResult} = do
    chunk <- fetchResultChunk (statementStreamMode stream) conn statementStreamResult
    if chunk == nullPtr
        then do
            throwResultError queryText statementStreamResult
            pure stream
        else do
            rawSize <- c_duckdb_data_chunk_get_size chunk
            let rowCount = fromIntegral rawSize :: Int
            if rowCount <= 0
                then do
                    destroyDataChunk chunk
                    fetchChunk conn queryText stream
                else do
                    vectors <-
                        prepareChunkVectors chunk (statementStreamColumns stream)
                            `onException` destroyDataChunk chunk
                    let chunkState =
                            StatementStreamChunk
                                { statementStreamChunkPtr = chunk
                                , statementStreamChunkSize = rowCount
                                , statementStreamChunkIndex = 0
                                , statementStreamChunkVectors = vectors
                                }
                    pure stream{statementStreamChunk = Just chunkState}

prepareChunkVectors :: DuckDBDataChunk -> [StatementStreamColumn] -> IO [StatementStreamChunkVector]
prepareChunkVectors chunk columns =
    forM columns \StatementStreamColumn{statementStreamColumnIndex, statementStreamColumnType} -> do
        vector <- c_duckdb_data_chunk_get_vector chunk (fromIntegral statementStreamColumnIndex)
        dataPtr <- c_duckdb_vector_get_data vector
        validity <- c_duckdb_vector_get_validity vector
        geometry <- if statementStreamColumnType == DuckDBTypeGeometry then Just <$> prepareGeometryDecoder vector dataPtr validity else pure Nothing
        pure
            StatementStreamChunkVector
                { statementStreamChunkVectorHandle = vector
                , statementStreamChunkVectorData = dataPtr
                , statementStreamChunkVectorValidity = validity
                , statementStreamChunkVectorGeometry = geometry
                }

-- | Use readers whose borrowed buffers have the same lifetime as this chunk.
materializeChunkValue :: DuckDBType -> StatementStreamChunkVector -> Int -> IO FieldValue
materializeChunkValue dtype vector row =
    case statementStreamChunkVectorGeometry vector of
        Just decode -> maybe FieldNull FieldGeometry <$> decode row
        Nothing -> materializeValue dtype (statementStreamChunkVectorHandle vector) (statementStreamChunkVectorData vector) (statementStreamChunkVectorValidity vector) row

-- | Clear cursor ownership before releasing native resources.
cleanupStatementStreamRef :: IORef StatementStreamState -> IO ()
cleanupStatementStreamRef ref = mask_ do
    state <- atomicModifyIORef' ref (StatementStreamIdle,)
    finalizeStreamState state

finalizeStreamState :: StatementStreamState -> IO ()
finalizeStreamState = \case
    StatementStreamIdle -> pure ()
    StatementStreamExhausted -> pure ()
    StatementStreamActive stream -> finalizeStream stream

finalizeStream :: StatementStream -> IO ()
finalizeStream StatementStream{statementStreamResult, statementStreamChunk} = do
    maybe (pure ()) finalizeChunk statementStreamChunk
    c_duckdb_destroy_result statementStreamResult
    free statementStreamResult

finalizeChunk :: StatementStreamChunk -> IO ()
finalizeChunk StatementStreamChunk{statementStreamChunkPtr} =
    destroyDataChunk statementStreamChunkPtr

-- | Copy all rows from a materialized native result.
collectRows :: Query -> Ptr DuckDBResult -> IO [[Field]]
collectRows queryText resPtr = do
    columns <- collectResultColumns resPtr
    collectChunks columns []
  where
    collectChunks columns acc = do
        fetched <- bracket (c_duckdb_fetch_chunk resPtr) destroyDataChunk \chunk ->
            if chunk == nullPtr
                then throwResultError queryText resPtr >> pure Nothing
                else Just <$> decodeChunk columns chunk
        case fetched of
            Nothing -> pure (concat (reverse acc))
            Just rows -> do
                let acc' = maybe acc (: acc) rows
                collectChunks columns acc'

    decodeChunk columns chunk = do
        rawSize <- c_duckdb_data_chunk_get_size chunk
        let rowCount = fromIntegral rawSize :: Int
        if rowCount <= 0
            then pure Nothing
            else
                if null columns
                    then pure (Just (replicate rowCount []))
                    else do
                        vectors <- prepareChunkVectors chunk columns
                        rows <- mapM (buildMaterializedRow columns vectors) [0 .. rowCount - 1]
                        pure (Just rows)

collectResultColumns :: Ptr DuckDBResult -> IO [StatementStreamColumn]
collectResultColumns resPtr = do
    rawCount <- c_duckdb_column_count resPtr
    let cc = fromIntegral rawCount :: Int
    forM [0 .. cc - 1] \columnIndex -> do
        namePtr <- c_duckdb_column_name resPtr (fromIntegral columnIndex)
        name <-
            if namePtr == nullPtr
                then pure (Text.pack ("column" <> show columnIndex))
                else peekUtf8CString namePtr
        dtype <- c_duckdb_column_type resPtr (fromIntegral columnIndex)
        pure
            StatementStreamColumn
                { statementStreamColumnIndex = columnIndex
                , statementStreamColumnName = name
                , statementStreamColumnType = dtype
                }

buildMaterializedRow :: [StatementStreamColumn] -> [StatementStreamChunkVector] -> Int -> IO [Field]
buildMaterializedRow columns vectors rowIdx =
    zipWithM (buildMaterializedField rowIdx) columns vectors

buildMaterializedField :: Int -> StatementStreamColumn -> StatementStreamChunkVector -> IO Field
buildMaterializedField rowIdx column vector = do
    value <- materializeChunkValue (statementStreamColumnType column) vector rowIdx
    pure
        Field
            { fieldName = statementStreamColumnName column
            , fieldIndex = statementStreamColumnIndex column
            , fieldValue = value
            }
