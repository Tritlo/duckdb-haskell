{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Database.DuckDB.Simple.Arrow.Internal
Description : Shared ownership and conversion for Arrow result batches.
-}
module Database.DuckDB.Simple.Arrow.Internal (
    foldArrowWith,
) where

import Control.Exception (bracket, bracket_, throwIO)
import Control.Monad (forM, when)
import Database.DuckDB.FFI
import Database.DuckDB.Simple (bind, withStatement)
import Database.DuckDB.Simple.Internal (Connection, Query, ResultMode, SQLError (..), destroyDataChunk, destroyLogicalType, executePreparedResult, fetchResultChunk, peekUtf8CString, throwResultError, withResult, withStatementHandle)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fillBytes, withMany)
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (sizeOf), peek, poke)

-- | Fold over borrowed Arrow batches with the selected native execution mode.
foldArrowWith :: (ToRow q) => ResultMode -> Connection -> Query -> q -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrowWith mode conn queryText params initial step =
    withStatement conn queryText \stmt -> do
        bind stmt (toRow params)
        withStatementHandle stmt \handle ->
            withResult conn queryText (executePreparedResult mode handle) \result ->
                bracket (c_duckdb_result_get_arrow_options result) destroyArrowOptions \options ->
                    withResultSchema queryText result options \schema ->
                        loop result options schema initial
  where
    loop result options schema acc = do
        next <- bracket (fetchResultChunk mode conn result) destroyDataChunk \chunk ->
            if chunk == nullPtr
                then do
                    throwResultError queryText result
                    pure Nothing
                else do
                    rowCount <- c_duckdb_data_chunk_get_size chunk
                    if rowCount == 0
                        then pure (Just acc)
                        else withArrowArray \array -> do
                            checkArrowError queryText (c_duckdb_data_chunk_to_arrow options chunk array)
                            nextAcc <- step acc schema array
                            nextAcc `seq` pure (Just nextAcc)
        case next of
            Nothing -> pure acc
            Just nextAcc -> loop result options schema nextAcc

-- | Convert the executed result's schema and release it after the action.
withResultSchema :: Query -> Ptr DuckDBResult -> DuckDBArrowOptions -> (Ptr ArrowSchema -> IO a) -> IO a
withResultSchema queryText result options action = do
    count <- c_duckdb_column_count result
    let indices = if count == 0 then [] else [0 .. count - 1]
    withMany (\idx -> bracket (c_duckdb_column_logical_type result idx) destroyLogicalType) indices \types -> do
        names <- forM indices (c_duckdb_column_name result)
        withArray types \typeArray ->
            withArray names \nameArray ->
                alloca \schema ->
                    bracket_ (fillBytes schema 0 (sizeOf (undefined :: ArrowSchema))) (releaseSchema schema) do
                        checkArrowError queryText (c_duckdb_to_arrow_schema options typeArray nameArray count schema)
                        action schema

-- | Allocate an empty Arrow array and release its contents after the action.
withArrowArray :: (Ptr ArrowArray -> IO a) -> IO a
withArrowArray action =
    alloca \array ->
        bracket_ (fillBytes array 0 (sizeOf (undefined :: ArrowArray))) (releaseArray array) (action array)

-- | Convert and release an owned Arrow conversion error.
checkArrowError :: Query -> IO DuckDBErrorData -> IO ()
checkArrowError queryText makeError =
    bracket makeError destroyError \err ->
        when (err /= nullPtr) do
            failed <- c_duckdb_error_data_has_error err
            when (failed /= 0) do
                messagePtr <- c_duckdb_error_data_message err
                message <- if messagePtr == nullPtr then pure "DuckDB Arrow conversion failed" else peekUtf8CString messagePtr
                errorType <- c_duckdb_error_data_error_type err
                throwIO (SQLError message (Just errorType) (Just queryText))

-- | Release an Arrow schema if its producer still owns buffers.
releaseSchema :: Ptr ArrowSchema -> IO ()
releaseSchema ptr = do
    schema <- peek ptr
    when (arrowSchemaRelease schema /= nullFunPtr) (mkArrowSchemaRelease (arrowSchemaRelease schema) ptr)

-- | Release an Arrow array if its producer still owns buffers.
releaseArray :: Ptr ArrowArray -> IO ()
releaseArray ptr = do
    array <- peek ptr
    when (arrowArrayRelease array /= nullFunPtr) (mkArrowArrayRelease (arrowArrayRelease array) ptr)

-- | Destroy the result's Arrow conversion options.
destroyArrowOptions :: DuckDBArrowOptions -> IO ()
destroyArrowOptions options = alloca \ptr -> poke ptr options >> c_duckdb_destroy_arrow_options ptr

-- | Destroy an Arrow conversion error handle.
destroyError :: DuckDBErrorData -> IO ()
destroyError err = alloca \ptr -> poke ptr err >> c_duckdb_destroy_error_data ptr
