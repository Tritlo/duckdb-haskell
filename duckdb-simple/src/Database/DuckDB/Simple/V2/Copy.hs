{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Database.DuckDB.Simple.V2.Copy
-- Description : Managed V2 COPY callbacks and multi-file table functions.
--
-- Callback info and context handles are borrowed and must not escape a callback.
-- The registration user-data slot is reserved for callback cleanup. Use closures
-- for registration data and 'setCallbackState' for bind, init, and batch state.
-- Transfer state to each slot once. Use an @IORef@ to change existing state.
-- Batch preparation may run concurrently. Flushes for one output file are serial.
-- Destroy a collection acquired with @copy_to_batch_take_input@ after use.
-- Multi-file functions use the callbacks of a registered single-file table
-- function.
module Database.DuckDB.Simple.V2.Copy (
  CopyToCallbacks (..),
  copyToCallbacks,
  CopyFromCallbacks (..),
  copyFromCallbacks,
  registerCopyFunction,
  registerCopyFunctionWith,
  registerCopyToCallbacks,
  registerCopyFromCallbacks,
  registerMultiFileFunctionWith,
  RegistrationTarget (..),
  ContextCallback,
  setCallbackState,
  getCallbackState,
) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (Allocator, attachOpaque, guardContext, install, installOptional, withCallbackResources, withTarget)
import Database.DuckDB.Simple.Internal (Connection)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.V2.Registration (ContextCallback, RegistrationTarget (..), getCallbackState, setCallbackState)

-- | Every COPY TO callback in the pinned V2 API. Batch and flush are required.
data CopyToCallbacks = CopyToCallbacks
  { copyToBindCallback :: Maybe (ContextCallback DuckDBV2CopyToBindInfoHandle)
  , copyToBatchSizeCallback :: Maybe (ContextCallback DuckDBV2CopyToBatchSizeInfoHandle)
  , copyToInitCallback :: Maybe (ContextCallback DuckDBV2CopyToInitInfoHandle)
  , copyToBatchCallback :: ContextCallback DuckDBV2CopyToBatchInfoHandle
  , copyToFlushCallback :: ContextCallback DuckDBV2CopyToFlushInfoHandle
  , copyToFinalizeCallback :: Maybe (ContextCallback DuckDBV2CopyToFinalizeInfoHandle)
  , copyToStatisticsCallback :: Maybe (ContextCallback DuckDBV2CopyToStatisticsInfoHandle)
  }

-- | Use batch and flush callbacks with no optional COPY TO callbacks.
copyToCallbacks :: ContextCallback DuckDBV2CopyToBatchInfoHandle -> ContextCallback DuckDBV2CopyToFlushInfoHandle -> CopyToCallbacks
copyToCallbacks batch flush = CopyToCallbacks Nothing Nothing Nothing batch flush Nothing Nothing

-- | Every COPY FROM callback in the pinned V2 API. Bind and exec are required.
data CopyFromCallbacks = CopyFromCallbacks
  { copyFromBindCallback :: ContextCallback DuckDBV2CopyFromBindInfoHandle
  , copyFromInitGlobalCallback :: Maybe (ContextCallback DuckDBV2CopyFromInitGlobalInfoHandle)
  , copyFromInitLocalCallback :: Maybe (ContextCallback DuckDBV2CopyFromInitLocalInfoHandle)
  , copyFromExecCallback :: ContextCallback DuckDBV2CopyFromExecInfoHandle
  , copyFromProgressCallback :: Maybe (ContextCallback DuckDBV2CopyFromProgressInfoHandle)
  }

-- | Use bind and exec callbacks with no optional COPY FROM callbacks.
copyFromCallbacks :: ContextCallback DuckDBV2CopyFromBindInfoHandle -> ContextCallback DuckDBV2CopyFromExecInfoHandle -> CopyFromCallbacks
copyFromCallbacks bind execute = CopyFromCallbacks bind Nothing Nothing execute Nothing

-- | Register either direction or both. DuckDB owns the allocated callbacks.
registerCopyFunction :: Connection -> Text -> Maybe CopyToCallbacks -> Maybe CopyFromCallbacks -> IO ()
registerCopyFunction connection name to from = do
  when (Text.null name || Text.any (== '\0') name) $ throwInvalidArgument "invalid copy function name"
  registerCopyFunctionWith
    (ConnectionTarget connection)
    (\function -> withIdentifier name (checked . c_duckdb_v2_copy_function_set_name function))
    to
    from

-- | Register COPY callbacks on a connection or a borrowed extension target.
registerCopyFunctionWith
  :: RegistrationTarget -> (DuckDBV2CopyFunctionHandle -> IO ()) -> Maybe CopyToCallbacks -> Maybe CopyFromCallbacks -> IO ()
registerCopyFunctionWith target configure to from =
  withTarget
    target
    c_duckdb_v2_copy_function_create
    c_duckdb_v2_copy_function_destroy
    c_duckdb_v2_connection_register_copy_function
    c_duckdb_v2_extension_register_copy_function
    \function register ->
      withCallbackResources
        (\allocate -> sequence_ <$> sequence (toInstallers allocate function to <> fromInstallers allocate function from))
        (attachOpaque (c_duckdb_v2_copy_function_set_user_data function))
        \installCallbacks -> do
          configure function
          installCallbacks
          register

-- | Register the advanced COPY TO callback interface.
registerCopyToCallbacks :: Connection -> Text -> CopyToCallbacks -> IO ()
registerCopyToCallbacks connection name callbacks = registerCopyFunction connection name (Just callbacks) Nothing

-- | Register the advanced COPY FROM callback interface.
registerCopyFromCallbacks :: Connection -> Text -> CopyFromCallbacks -> IO ()
registerCopyFromCallbacks connection name callbacks = registerCopyFunction connection name Nothing (Just callbacks)

-- | Allocate the configured COPY TO callbacks before installing them.
toInstallers :: Allocator -> DuckDBV2CopyFunctionHandle -> Maybe CopyToCallbacks -> [IO (IO ())]
toInstallers _ _ Nothing = []
toInstallers allocate function (Just callbacks) =
  [ installOptional (copyToBindCallback callbacks) $ \callback ->
      install
        allocate
        DuckDBV2CopyToBindCallbackFn
        (DuckDBV2CopyToBindCallbackFn_Aux (guardContext callback))
        (c_duckdb_v2_copy_to_set_bind_callback function)
  , installOptional (copyToBatchSizeCallback callbacks) $ \callback ->
      install
        allocate
        DuckDBV2CopyToBatchSizeCallbackFn
        (DuckDBV2CopyToBatchSizeCallbackFn_Aux (guardContext callback))
        (c_duckdb_v2_copy_to_set_batch_size_callback function)
  , installOptional (copyToInitCallback callbacks) $ \callback ->
      install
        allocate
        DuckDBV2CopyToInitCallbackFn
        (DuckDBV2CopyToInitCallbackFn_Aux (guardContext callback))
        (c_duckdb_v2_copy_to_set_init_callback function)
  , install
      allocate
      DuckDBV2CopyToBatchCallbackFn
      (DuckDBV2CopyToBatchCallbackFn_Aux (guardContext (copyToBatchCallback callbacks)))
      (c_duckdb_v2_copy_to_set_batch_callback function)
  , install
      allocate
      DuckDBV2CopyToFlushCallbackFn
      (DuckDBV2CopyToFlushCallbackFn_Aux (guardContext (copyToFlushCallback callbacks)))
      (c_duckdb_v2_copy_to_set_flush_callback function)
  , installOptional (copyToFinalizeCallback callbacks) $ \callback ->
      install
        allocate
        DuckDBV2CopyToFinalizeCallbackFn
        (DuckDBV2CopyToFinalizeCallbackFn_Aux (guardContext callback))
        (c_duckdb_v2_copy_to_set_finalize_callback function)
  , installOptional (copyToStatisticsCallback callbacks) $ \callback ->
      install
        allocate
        DuckDBV2CopyToStatisticsCallbackFn
        (DuckDBV2CopyToStatisticsCallbackFn_Aux (guardContext callback))
        (c_duckdb_v2_copy_to_set_statistics_callback function)
  ]

-- | Allocate the configured COPY FROM callbacks before installing them.
fromInstallers :: Allocator -> DuckDBV2CopyFunctionHandle -> Maybe CopyFromCallbacks -> [IO (IO ())]
fromInstallers _ _ Nothing = []
fromInstallers allocate function (Just callbacks) =
  [ install
      allocate
      DuckDBV2CopyFromBindCallbackFn
      (DuckDBV2CopyFromBindCallbackFn_Aux (guardContext (copyFromBindCallback callbacks)))
      (c_duckdb_v2_copy_from_set_bind_callback function)
  , installOptional (copyFromInitGlobalCallback callbacks) $ \callback ->
      install
        allocate
        DuckDBV2CopyFromInitGlobalCallbackFn
        (DuckDBV2CopyFromInitGlobalCallbackFn_Aux (guardContext callback))
        (c_duckdb_v2_copy_from_set_init_global_callback function)
  , installOptional (copyFromInitLocalCallback callbacks) $ \callback ->
      install
        allocate
        DuckDBV2CopyFromInitLocalCallbackFn
        (DuckDBV2CopyFromInitLocalCallbackFn_Aux (guardContext callback))
        (c_duckdb_v2_copy_from_set_init_local_callback function)
  , install
      allocate
      DuckDBV2CopyFromExecCallbackFn
      (DuckDBV2CopyFromExecCallbackFn_Aux (guardContext (copyFromExecCallback callbacks)))
      (c_duckdb_v2_copy_from_set_exec_callback function)
  , installOptional (copyFromProgressCallback callbacks) $ \callback ->
      install
        allocate
        DuckDBV2CopyFromProgressCallbackFn
        (DuckDBV2CopyFromProgressCallbackFn_Aux (guardContext callback))
        (c_duckdb_v2_copy_from_set_progress_callback function)
  ]

-- | Configure a multi-file wrapper on a connection or borrowed extension target.
registerMultiFileFunctionWith :: RegistrationTarget -> (DuckDBV2MultiFileFunctionHandle -> IO ()) -> IO ()
registerMultiFileFunctionWith target configure =
  withTarget
    target
    c_duckdb_v2_multi_file_function_create
    c_duckdb_v2_multi_file_function_destroy
    c_duckdb_v2_connection_register_multi_file_function
    c_duckdb_v2_extension_register_multi_file_function
    \function register -> do
      configure function
      register
