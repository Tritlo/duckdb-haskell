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
import Database.DuckDB.Simple.Native (DuckDBV2FactoryHandle, DuckDBV2LogicalTypeHandle, SQLError (..), destroyLogicalType, releaseItems)
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeToRep)

-- | The owner of native field types for one result or callback.
-- See Note [Scoped field types].
newtype TypeScope = TypeScope (MVar (Maybe [(DuckDBV2LogicalTypeHandle, IORef (Either (IO DuckDBV2FactoryHandle) LogicalTypeRep))]))
  deriving (Eq)

{- Note [Scoped field types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A scope owns each native type and its optional Haskell snapshot. The MVar
serializes acquisition, type reads, and closure. A closed scope rejects each
read, including a cache hit. A snapshot already returned to the caller is pure
and remains valid after closure.

Only internal native operations run while the scope is locked. The type and
factory acquisition actions must not run user code. An uncached read needs a
live factory owner. A result reads the factory from the connection at that
point, so a decoder that closed the connection gets an error. An extension
aggregate can initialize its registration-owned helper at that point.
logicalTypeToRep reads type ids, names, and parameters, and compares temporary
alias copies. These operations do not run SQL or callbacks. A failed or
interrupted read leaves the native type available for another attempt. Only
a successful read fills the snapshot cache. Until then, the cache holds the
factory acquisition action. Closure drops that action too, so an escaped field
cannot retain the connection that it used for the read.

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
    release (native, snapshot) = destroyLogicalType native `finally` writeIORef snapshot (Left closedTypeScope)

-- | Run an action in a type scope and close it on every exit path.
withTypeScope :: (TypeScope -> IO a) -> IO a
withTypeScope = bracket newTypeScope closeTypeScope

-- | Acquire an owned native type and read its Haskell snapshot on demand.
-- The acquisition action must use only native metadata operations.
-- See Note [Scoped field types].
scopedTypeRep :: TypeScope -> IO DuckDBV2FactoryHandle -> IO DuckDBV2LogicalTypeHandle -> IO (Conversion LogicalTypeRep)
scopedTypeRep (TypeScope scope) acquireFactory create = modifyMVarMasked scope \case
  Nothing -> closedTypeScope
  Just entries -> bracketOnError create destroyLogicalType \native -> do
    snapshot <- newIORef (Left acquireFactory)
    let readType = withMVar scope \case
          Nothing -> closedTypeScope
          Just _ ->
            readIORef snapshot >>= \case
              Right logical -> pure logical
              Left acquire -> do
                factory <- acquire
                logical <- logicalTypeToRep factory native >>= evaluate
                writeIORef snapshot (Right logical)
                pure logical
    pure (Just ((native, snapshot) : entries), liftConversion readType)

-- | Reject an operation after its metadata owner has closed.
closedTypeScope :: IO a
closedTypeScope = throwIO (SQLError "duckdb-simple: field type scope is closed" Nothing Nothing Nothing)
