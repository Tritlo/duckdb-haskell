{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Database.DuckDB.Simple.Arrow.Internal
-- Description : V2 Arrow execution and ownership of exported schemas and arrays.
module Database.DuckDB.Simple.Arrow.Internal (
  foldArrowWith,
  releaseArrowSchema,
  releaseArrowArray,
  releaseArrowStream,
) where

import Control.Exception (bracket, finally, mask, mask_, onException)
import Control.Monad (when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Proxy (Proxy (..))
import Database.DuckDB.Simple (bind, withStatement)
import Database.DuckDB.Simple.Internal
import Database.DuckDB.Simple.ToRow (ToRow (..))
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullFunPtr)
import Foreign.Storable (peek)
import HsBindgen.Runtime.Marshal (staticSizeOf)
import HsBindgen.Runtime.Support.FunPtr (fromFunPtr)

-- | Owned root storage for an Arrow schema and array.
type ArrowBatch = (Ptr ArrowSchema, Ptr ArrowArray)

-- | Fold over owned Arrow batches with materialized or incremental execution.
foldArrowWith :: (ToRow q) => ResultMode -> Connection -> Query -> q -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
foldArrowWith mode conn queryText params initial step = withStatement conn queryText \stmt -> do
  bind stmt (toRow params)
  mask \restore -> do
    -- The native calls of the fold attach its query to an error. The step runs
    -- outside them, so that an error of the step keeps its own query, as in
    -- the row folds.
    let native = withQueryError queryText
        deliver acc (schema, array) = do
          -- A step that closes the connection stops the later steps.
          withStatementHandle stmt (\_ -> pure ())
          next <- restore (step acc schema array)
          next `seq` pure next
    case mode of
      StreamingResult ->
        -- Serialize the execution call. The stream stays open after it. See
        -- Note [Connection lock] in "Database.DuckDB.Simple.Internal".
        bracket (native (withConnectionLock conn (executePreparedArrowResult stmt))) (const (native (clearArrowResult stmt))) \result ->
          -- Recurse outside the handle scope, so that the stack does not grow for each batch.
          -- Each fetch holds the connection lock. The step runs after the
          -- fetch, without the lock.
          let stream acc =
                withStatementHandle stmt (\_ -> withConnectionLock conn (fetchBatch stmt result)) >>= \case
                  Nothing -> pure acc
                  Just batch -> (deliver acc batch `finally` releaseBatch batch) >>= stream
          in  stream initial
      MaterializedResult -> do
        owned <- newIORef []
        let gather result =
              fetchBatch stmt result >>= \case
                Nothing -> pure ()
                Just batch -> atomicModifyIORef' owned (\batches -> (batch : batches, ())) >> gather result
            consume acc =
              atomicModifyIORef' owned (\case [] -> ([], Nothing); batch : rest -> (rest, Just batch)) >>= \case
                Nothing -> pure acc
                Just batch -> do
                  acc' <- deliver acc batch `finally` releaseBatch batch
                  consume acc'
            release = readIORef owned >>= mapM_ releaseBatch
        flip finally release do
          -- Hold the lock until the native result closes. The callbacks run after that.
          -- The batches are in storage that this thread releases. See
          -- Note [Result window] in "Database.DuckDB.Simple.Internal".
          native (withResultWindow conn (const (pure ())) (withArrowResult stmt gather))
          atomicModifyIORef' owned (\batches -> (reverse batches, ()))
          consume initial

-- | Fetch one owned nonempty batch and its independent schema. See
-- Note [Interruptible native calls] in "Database.DuckDB.Simple.Internal".
fetchBatch :: Statement -> DuckDBV2ArrowResultHandle -> IO (Maybe ArrowBatch)
fetchBatch stmt result = mask_ do
  schema <- malloc
  fillBytes schema 0 (staticSizeOf (Proxy @ArrowSchema))
  array <- malloc `onException` free schema
  fillBytes array 0 (staticSizeOf (Proxy @ArrowArray))
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

-- | Release an initialized schema if its release callback is present.
-- The caller still owns the storage for the structure.
releaseArrowSchema :: Ptr ArrowSchema -> IO ()
releaseArrowSchema ptr = mask_ do
  schema <- peek ptr
  when (schema.release /= nullFunPtr) $ fromFunPtr schema.release ptr

-- | Release an initialized array if its release callback is present.
-- The caller still owns the storage for the structure.
releaseArrowArray :: Ptr ArrowArray -> IO ()
releaseArrowArray ptr = mask_ do
  array <- peek ptr
  when (array.release /= nullFunPtr) $ fromFunPtr array.release ptr

-- | Release an initialized stream if its release callback is present.
-- The caller still owns the storage for the structure.
releaseArrowStream :: Ptr ArrowArrayStream -> IO ()
releaseArrowStream ptr = mask_ do
  stream <- peek ptr
  when (stream.release /= nullFunPtr) $ fromFunPtr stream.release ptr
