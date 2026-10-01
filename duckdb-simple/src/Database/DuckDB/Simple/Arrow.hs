{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Database.DuckDB.Simple.Arrow
Description : Scoped Arrow export through the DuckDB C Data Interface.

These functions execute a query and visit its Arrow batches. DuckDB materializes
the native result before the first callback. The Haskell code converts and
releases one batch at a time. Native result memory can grow with the query size.

The callback borrows the schema and array. Read them during the callback only.
Do not retain, change, release, or transfer either object. Copy data that must
outlive the callback. All native resources are released if the callback throws.

This module uses the schema and chunk conversion API. The older query and scan
functions in @Database.DuckDB.FFI.Deprecated@ are deprecated by DuckDB.
-}
module Database.DuckDB.Simple.Arrow (
    foldArrow,
    foldArrow_,
) where

import Control.Exception (bracket, bracket_, throwIO)
import Control.Monad (forM, when)
import Database.DuckDB.FFI
import Database.DuckDB.Simple (bind, withStatement)
import Database.DuckDB.Simple.Internal (Connection, Query, SQLError (..), destroyLogicalType, peekUtf8CString, throwResultError, withConnectionHandle, withResult, withStatementHandle)
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fillBytes, withMany)
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (sizeOf), peek, poke)

{- | Execute a parameterized query and fold over borrowed Arrow batches.

The schema includes the executed result's column names and types. The callback
runs once per nonempty batch and never runs for an empty result. The schema and
array pointers are valid only during the callback. The accumulator is evaluated
to weak head normal form after each callback.

DuckDB materializes the native result before this fold starts. This function
does not provide bounded-memory query execution.
-}
foldArrow :: (ToRow q) => Connection -> Query -> q -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrow conn queryText params initial step =
    withStatement conn queryText \stmt -> do
        bind stmt (toRow params)
        withStatementHandle stmt \handle ->
            withResult conn queryText (c_duckdb_execute_prepared handle) \result ->
                bracket (c_duckdb_result_get_arrow_options result) destroyArrowOptions \options ->
                    withResultSchema queryText result options \schema ->
                        loop result options schema initial
  where
    loop result options schema acc = do
        next <- withConnectionHandle conn \_ -> bracket (c_duckdb_fetch_chunk result) destroyChunk \chunk ->
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

-- | Fold over borrowed Arrow batches from a query without parameters.
foldArrow_ :: Connection -> Query -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrow_ conn queryText = foldArrow conn queryText ()

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

-- | Destroy a fetched native chunk.
destroyChunk :: DuckDBDataChunk -> IO ()
destroyChunk chunk = alloca \ptr -> poke ptr chunk >> c_duckdb_destroy_data_chunk ptr

-- | Destroy an Arrow conversion error handle.
destroyError :: DuckDBErrorData -> IO ()
destroyError err = alloca \ptr -> poke ptr err >> c_duckdb_destroy_error_data ptr
