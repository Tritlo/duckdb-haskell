{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.DuckDB.Simple.TypeScope
-- Description : Scoped ownership of native field types.
module Database.DuckDB.Simple.TypeScope (
  TypeScope,
  newTypeScope,
  closeTypeScope,
  withTypeScope,
  scopedTypeRep,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception (bracket, bracketOnError, evaluate, finally, throwIO, uninterruptibleMask_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Database.DuckDB.Simple.Conversion (Conversion, liftConversion)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep)
import Database.DuckDB.Simple.Native (DuckDBV2LogicalTypeHandle, SQLError (..), destroyLogicalType, releaseItems)
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeToRep)

-- | The owner of native field types for one result or callback.
-- See Note [Scoped field types].
newtype TypeScope = TypeScope (MVar (Maybe [(DuckDBV2LogicalTypeHandle, IORef (Maybe LogicalTypeRep))]))

{- Note [Scoped field types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A scope owns each native type and its optional Haskell snapshot. The MVar
serializes acquisition, type reads, and closure. A closed scope rejects each
read, including a cache hit. A snapshot already returned to the caller is pure
and remains valid after closure.

Only internal native metadata operations run while the scope is locked. The
acquisition action must not run user code. logicalTypeToRep reads type ids,
names, and parameters through native getters. These getters do not run SQL or
callbacks. A failed or interrupted read leaves the native type available for
another attempt. Only a successful read fills the snapshot cache.

Closure waits for an active metadata operation, then destroys every native
type and clears every snapshot cache. Cleanup uses only internal metadata
operations. It is uninterruptible so cancellation cannot leave an owned type
behind. No user decoder or action runs under this mask. The scope stays locked
until cleanup ends, and becomes closed even if a destructor raises an
exception. Thus a concurrent reader or closer cannot use or destroy a released
handle.
-}

-- | Start a scope with no native types.
newTypeScope :: IO TypeScope
newTypeScope = TypeScope <$> newMVar (Just [])

-- | Release each native type and reject future type reads. Repeated calls are safe.
-- See Note [Scoped field types].
closeTypeScope :: TypeScope -> IO ()
closeTypeScope (TypeScope scope) = uninterruptibleMask_ do
  owned <- takeMVar scope
  flip finally (putMVar scope Nothing) $
    case owned of
      Nothing -> pure ()
      Just entries -> releaseItems release entries
  where
    release (native, snapshot) = destroyLogicalType native `finally` writeIORef snapshot Nothing

-- | Run an action in a type scope and close it on every exit path.
withTypeScope :: (TypeScope -> IO a) -> IO a
withTypeScope = bracket newTypeScope closeTypeScope

-- | Acquire an owned native type and read its Haskell snapshot on demand.
-- The acquisition action must use only native metadata operations.
-- See Note [Scoped field types].
scopedTypeRep :: TypeScope -> IO DuckDBV2LogicalTypeHandle -> IO (Conversion LogicalTypeRep)
scopedTypeRep (TypeScope scope) create = modifyMVarMasked scope \case
  Nothing -> closedTypeScope
  Just entries -> bracketOnError create destroyLogicalType \native -> do
    snapshot <- newIORef Nothing
    let readType = withMVar scope \case
          Nothing -> closedTypeScope
          Just _ ->
            readIORef snapshot >>= \case
              Just logical -> pure logical
              Nothing -> do
                logical <- logicalTypeToRep native >>= evaluate
                writeIORef snapshot (Just logical)
                pure logical
    pure (Just ((native, snapshot) : entries), liftConversion readType)

-- | Reject an operation after its metadata owner has closed.
closedTypeScope :: IO a
closedTypeScope = throwIO (SQLError "duckdb-simple: field type scope is closed" Nothing Nothing Nothing)
