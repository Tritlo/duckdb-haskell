{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | V2 Arrow execution and ownership of exported schemas and arrays.
module Database.DuckDB.Simple.Arrow.Internal (
    foldArrowWith,
    releaseArrowSchema,
    releaseArrowArray,
    releaseArrowStream,
) where

import Control.Exception (finally, mask, mask_, onException)
import Control.Monad (when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Database.DuckDB.Simple (bind, withStatement)
import Database.DuckDB.Simple.Internal
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullFunPtr)
import Foreign.Storable (peek, sizeOf)
import HsBindgen.Runtime.Support.FunPtr (fromFunPtr)

-- | Owned root storage for an Arrow schema and array.
type ArrowBatch = (Ptr ArrowSchema, Ptr ArrowArray)

-- | Fold over owned Arrow batches with materialized or incremental execution.
foldArrowWith :: (ToRow q) => ResultMode -> Connection -> Query -> q -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrowWith mode conn queryText params initial step = withStatement conn queryText \stmt -> do
    bind stmt (toRow params)
    withQueryError queryText $ withArrowResult stmt \result -> mask \restore -> do
        let deliver acc (schema, array) = withStatementHandle stmt \_ -> do
                next <- restore (step acc schema array)
                next `seq` pure next
            stream acc = withStatementHandle stmt \_ -> do
                next <- fetchBatch stmt result
                case next of
                    Nothing -> pure acc
                    Just batch -> do
                        acc' <- deliver acc batch `finally` releaseBatch batch
                        stream acc'
        case mode of
            StreamingResult -> stream initial
            MaterializedResult -> do
                owned <- newIORef []
                let gather =
                        fetchBatch stmt result >>= \case
                            Nothing -> pure ()
                            Just batch -> atomicModifyIORef' owned (\batches -> (batch : batches, ())) >> gather
                    consume acc =
                        atomicModifyIORef' owned (\case [] -> ([], Nothing); batch : rest -> (rest, Just batch)) >>= \case
                            Nothing -> pure acc
                            Just batch -> do
                                acc' <- deliver acc batch `finally` releaseBatch batch
                                consume acc'
                    release = readIORef owned >>= mapM_ releaseBatch
                flip finally release do
                    gather
                    atomicModifyIORef' owned (\batches -> (reverse batches, ()))
                    clearArrowResult stmt
                    consume initial

-- | Fetch one owned nonempty batch and its independent schema.
fetchBatch :: Statement -> DuckDBV2ArrowResultHandle -> IO (Maybe ArrowBatch)
fetchBatch stmt result = mask_ do
    schema <- malloc
    fillBytes schema 0 (sizeOf (undefined :: ArrowSchema))
    array <- malloc `onException` free schema
    fillBytes array 0 (sizeOf (undefined :: ArrowArray))
    let batch = (schema, array)
    exported <- flip onException (releaseBatch batch) do
        runInterruptibleQuery (statementConnection stmt) (checked (c_duckdb_v2_arrow_result_fetch_array result array))
        exported <- peek array
        when (exported.release /= nullFunPtr && exported.length /= 0) $
            checked (c_duckdb_v2_arrow_result_get_schema result schema)
        pure exported
    if
        | exported.release == nullFunPtr -> releaseBatch batch >> pure Nothing
        | exported.length == 0 -> releaseBatch batch >> fetchBatch stmt result
        | otherwise -> pure (Just batch)

-- | Release both Arrow objects and free their root storage.
releaseBatch :: ArrowBatch -> IO ()
releaseBatch (schema, array) =
    mask_ $
        (releaseArrowArray array `finally` free array) `finally` (releaseArrowSchema schema `finally` free schema)

{- | Release an initialized schema if its release callback is present.
The caller still owns the storage for the structure.
-}
releaseArrowSchema :: Ptr ArrowSchema -> IO ()
releaseArrowSchema ptr = mask_ do
    schema <- peek ptr
    when (schema.release /= nullFunPtr) $ fromFunPtr schema.release ptr

{- | Release an initialized array if its release callback is present.
The caller still owns the storage for the structure.
-}
releaseArrowArray :: Ptr ArrowArray -> IO ()
releaseArrowArray ptr = mask_ do
    array <- peek ptr
    when (array.release /= nullFunPtr) $ fromFunPtr array.release ptr

{- | Release an initialized stream if its release callback is present.
The caller still owns the storage for the structure.
-}
releaseArrowStream :: Ptr ArrowArrayStream -> IO ()
releaseArrowStream ptr = mask_ do
    stream <- peek ptr
    when (stream.release /= nullFunPtr) $ fromFunPtr stream.release ptr
