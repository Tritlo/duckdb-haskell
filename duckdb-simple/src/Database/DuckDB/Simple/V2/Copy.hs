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
--
-- Use this module when the row-oriented functions of "Database.DuckDB.Simple.Copy" are not sufficient, for
-- example to get a batch as a native column data collection, to set the batch size, or to give the
-- statistics for @RETURN_STATS@. Make a record of callbacks with 'copyToCallbacks' or 'copyFromCallbacks'.
-- Then register it with 'registerCopyToCallbacks' or 'registerCopyFromCallbacks', or register both sides
-- with 'registerCopyFunction'. 'registerMultiFileFunctionWith' adds file lists, globs, and hive partitions to
-- a registered single-file table function.
--
-- The examples use the imports and the definitions of the module header of
-- "Database.DuckDB.Simple.V2.Registration".
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
import Database.DuckDB.Simple.Callback (
  Allocator,
  attachOpaque,
  guardContext,
  install,
  installAll,
  installOptional,
  withCallbackResources,
  withTarget,
 )
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

-- | Register either direction or both. The library allocates the callback pointers. It gives their cleanup
-- to DuckDB in the user data of the native object. DuckDB runs the cleanup when it releases the object. See
-- Note [Callback ownership] in "Database.DuckDB.Simple.Callback".
registerCopyFunction :: Connection -> Text -> Maybe CopyToCallbacks -> Maybe CopyFromCallbacks -> IO ()
registerCopyFunction connection name to from = do
  when (Text.null name || Text.any (== '\0') name) $ throwInvalidArgument "invalid copy function name"
  registerCopyFunctionWith
    (ConnectionTarget connection)
    (\function -> withIdentifier name (checked . c_duckdb_v2_copy_function_set_name function))
    to
    from

-- | Register COPY callbacks on a connection or a borrowed extension target.
-- The configuration action sets the name of the format:
--
-- > V2Copy.registerCopyFunctionWith
-- >   target
-- >   (\function -> R.withIdentifier "managed_output" (R.checked . c_duckdb_v2_copy_function_set_name function))
-- >   (Just callbacks)
-- >   Nothing
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
        (\allocate -> installAll (toInstallers allocate function to <> fromInstallers allocate function from))
        (attachOpaque (c_duckdb_v2_copy_function_set_user_data function))
        \installCallbacks -> do
          configure function
          installCallbacks
          register

-- | Register the advanced COPY TO callback interface.
--
-- The example registers the format @managed_output@, which counts the rows. The init callback makes the
-- output file and stores a counter as init data. The batch callback takes the rows of a batch and stores
-- their count as batch data. The flush callback adds that count to the counter. The example does not write
-- the rows. A format that writes rows converts them in the batch callback and writes them in the flush
-- callback.
--
-- > withConnection ":memory:" \connection -> do
-- >   total <- newIORef (0 :: DuckDBIdx)
-- >   let initialize info _ = do
-- >         path <- R.output (c_duckdb_v2_copy_to_init_get_file_path info) >>= R.readText
-- >         writeFile (Text.unpack path) ""
-- >         R.setCallbackState (c_duckdb_v2_copy_to_init_set_init_data info) total
-- >       batch info _ = do
-- >         count <-
-- >           bracket
-- >             (R.output (c_duckdb_v2_copy_to_batch_take_input info))
-- >             (R.destroy c_duckdb_v2_column_data_collection_destroy)
-- >             (R.output . c_duckdb_v2_column_data_collection_row_count)
-- >         R.setCallbackState (c_duckdb_v2_copy_to_batch_set_batch_data info) count
-- >       flush info _ = do
-- >         count <- R.getCallbackState (c_duckdb_v2_copy_to_flush_get_batch_data info) :: IO DuckDBIdx
-- >         state <- R.getCallbackState (c_duckdb_v2_copy_to_flush_get_init_data info) :: IO (IORef DuckDBIdx)
-- >         atomicModifyIORef' state (\n -> (n + count, ()))
-- >       callbacks = (V2Copy.copyToCallbacks batch flush){V2Copy.copyToInitCallback = Just initialize}
-- >   V2Copy.registerCopyToCallbacks connection "managed_output" callbacks
-- >   _ <- execute_ connection "COPY (SELECT i FROM range(5000) t(i)) TO 'numbers.out' (FORMAT managed_output)"
-- >   readIORef total >>= print -- DuckDBIdx {unwrap = 5000}
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
-- 'Database.DuckDB.Simple.Copy.registerMultiFileFunction' sets the same fields without a native handle.
--
-- The example registers the table function @managed_single@, which reads the integer in one file. Then it
-- registers the multi-file function @managed_files@, which calls @managed_single@ for each file. The
-- configuration action sets the two names. DuckDB can run the exec callback of the single-file function on
-- more than one thread for one scan. Thus the exec callback takes its state with @atomicModifyIORef\'@.
--
-- > withConnection ":memory:" \connection -> do
-- >   let bind input result context = do
-- >         path <- bracket (R.output (c_duckdb_v2_function_bind_get_arg_value input 0)) (R.destroy c_duckdb_v2_value_destroy) \value ->
-- >           R.output (c_duckdb_v2_value_get_varchar value) >>= R.readText
-- >         value <- read <$> readFile (Text.unpack path) :: IO Int64
-- >         R.setCallbackState (c_duckdb_v2_function_bind_set_bind_data input) value
-- >         factory <- R.output (c_duckdb_v2_context_get_factory context)
-- >         R.withTypeDeclaration factory bigint \dtype ->
-- >           R.withIdentifier "value" \name -> R.checked (c_duckdb_v2_table_function_bind_add_result_column result name dtype)
-- >       initialize info _ = do
-- >         value <- R.getCallbackState (c_duckdb_v2_table_function_init_global_get_bind_data info)
-- >         state <- newIORef (Just (value :: Int64))
-- >         R.setCallbackState (c_duckdb_v2_table_function_init_global_set_global_state info) state
-- >       executeRows info _ = do
-- >         state <- R.getCallbackState (c_duckdb_v2_table_function_exec_get_global_state info) :: IO (IORef (Maybe Int64))
-- >         value <- atomicModifyIORef' state (\previous -> (Nothing, previous))
-- >         chunk <- R.output (c_duckdb_v2_table_function_exec_get_output_chunk info)
-- >         vector <- R.output (c_duckdb_v2_data_chunk_get_vector chunk 0)
-- >         writeIntegers vector (maybe [] pure value)
-- >   R.registerTableFunction
-- >     connection
-- >     "managed_single"
-- >     [R.parameter "path" (R.TypeExpression "VARCHAR")]
-- >     (R.tableCallbacks bind executeRows){R.tableInitGlobalCallback = Just initialize}
-- >   V2Copy.registerMultiFileFunctionWith (R.ConnectionTarget connection) \function -> do
-- >     R.withText "managed_files" \(ConstPtr view) -> R.checked (c_duckdb_v2_multi_file_function_set_name function view)
-- >     R.withText "managed_single" \(ConstPtr view) -> R.checked (c_duckdb_v2_multi_file_function_set_single_file_function function view)
-- >   writeFile "first.mock" "7"
-- >   writeFile "second.mock" "11"
-- >   rows <- query_ connection "SELECT value FROM managed_files('*.mock') ORDER BY value"
-- >   print (map fromOnly rows :: [Int64]) -- [7,11]
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
