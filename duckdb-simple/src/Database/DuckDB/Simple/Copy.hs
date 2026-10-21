{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Managed V2 COPY callbacks and multi-file table functions.
Callback info and context handles are borrowed and must not escape a callback.
The registration user-data slot is reserved for callback cleanup. Use closures
for registration data and 'setCallbackState' for bind, init, and batch state.
Transfer state to each slot once. Use an @IORef@ to change existing state.
Batch preparation may run concurrently. Flushes for one output file are serial.
Destroy a collection acquired with @copy_to_batch_take_input@ after use.
Multi-file functions use the callbacks of a registered single-file table function.
-}
module Database.DuckDB.Simple.Copy (
    CopyBindInfo (..),
    CopyInitInfo (..),
    CopySinkInfo (..),
    CopyFinalizeInfo (..),
    registerCopyToFunction,
    CopyToCallbacks (..),
    copyToCallbacks,
    CopyFromCallbacks (..),
    copyFromCallbacks,
    registerCopyFunction,
    registerCopyFunctionWith,
    registerCopyToCallbacks,
    registerCopyFromFunction,
    MultiFileOptions (..),
    defaultMultiFileOptions,
    registerMultiFileFunction,
    registerMultiFileFunctionWith,
    RegistrationTarget (..),
    ContextCallback,
    setCallbackState,
    getCallbackState,
) where

import Control.Exception (bracket, finally, mask, mask_, onException)
import Control.Monad (forM, forM_, when)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Database.DuckDB.Simple.Callback (attachOpaque, ignoreCallbackExceptions, runCallback, withCallbackResources)
import Database.DuckDB.Simple.FromField (Field (..))
import Database.DuckDB.Simple.Internal (Connection, throwRegistrationError, withConnectionHandle, withDatabaseHandle)
import Database.DuckDB.Simple.LogicalRep (destroyLogicalType)
import Database.DuckDB.Simple.Materialize (VariantReader, prepareVariantReader, prepareVectorReader)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Registration (ContextCallback, RegistrationTarget (..), getCallbackState, setCallbackState)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (Storable)
import HsBindgen.Runtime.Support.FunPtr (ToFunPtr, toFunPtr)

-- | Bind-phase metadata for a custom COPY TO function.
data CopyBindInfo = CopyBindInfo
    { copyBindColumnTypes :: ![DuckDBV2LogicalTypeId]
    }
    deriving (Eq, Show)

-- | Init-phase inputs for one output file or partition.
data CopyInitInfo bindState = CopyInitInfo
    { copyInitBindState :: !bindState
    , copyInitFilePath :: !FilePath
    }

-- | Flush-phase state passed to the row-oriented sink callback.
data CopySinkInfo bindState globalState = CopySinkInfo
    { copySinkBindState :: !bindState
    , copySinkGlobalState :: !globalState
    }

-- | Finalize-phase inputs for one output file or partition.
data CopyFinalizeInfo bindState globalState = CopyFinalizeInfo
    { copyFinalizeBindState :: !bindState
    , copyFinalizeGlobalState :: !globalState
    }

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
    when (Text.null name || Text.any (== '\0') name) $ throwRegistrationError "invalid copy function name"
    registerCopyFunctionWith
        (ConnectionTarget connection)
        (\function -> withIdentifier name (checked . c_duckdb_v2_copy_function_set_name function))
        to
        from

-- | Register COPY callbacks on a connection or a borrowed extension target.
registerCopyFunctionWith :: RegistrationTarget -> (DuckDBV2CopyFunctionHandle -> IO ()) -> Maybe CopyToCallbacks -> Maybe CopyFromCallbacks -> IO ()
registerCopyFunctionWith target configure to from =
    withTarget target c_duckdb_v2_copy_function_create c_duckdb_v2_copy_function_destroy c_duckdb_v2_connection_register_copy_function c_duckdb_v2_extension_register_copy_function \function register ->
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
registerCopyFromFunction :: Connection -> Text -> CopyFromCallbacks -> IO ()
registerCopyFromFunction connection name callbacks = registerCopyFunction connection name Nothing (Just callbacks)

-- | Allocate a callback pointer tracked by the native registration owner.
type Allocator = forall a. IO (FunPtr a) -> IO (FunPtr a)

-- | Allocate the configured COPY TO callbacks before installing them.
toInstallers :: Allocator -> DuckDBV2CopyFunctionHandle -> Maybe CopyToCallbacks -> [IO (IO ())]
toInstallers _ _ Nothing = []
toInstallers allocate function (Just callbacks) =
    [ optional (copyToBindCallback callbacks) $ \callback -> install allocate DuckDBV2CopyToBindCallbackFn (DuckDBV2CopyToBindCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_copy_to_set_bind_callback function)
    , optional (copyToBatchSizeCallback callbacks) $ \callback -> install allocate DuckDBV2CopyToBatchSizeCallbackFn (DuckDBV2CopyToBatchSizeCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_copy_to_set_batch_size_callback function)
    , optional (copyToInitCallback callbacks) $ \callback -> install allocate DuckDBV2CopyToInitCallbackFn (DuckDBV2CopyToInitCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_copy_to_set_init_callback function)
    , install allocate DuckDBV2CopyToBatchCallbackFn (DuckDBV2CopyToBatchCallbackFn_Aux (guardContext (copyToBatchCallback callbacks))) (c_duckdb_v2_copy_to_set_batch_callback function)
    , install allocate DuckDBV2CopyToFlushCallbackFn (DuckDBV2CopyToFlushCallbackFn_Aux (guardContext (copyToFlushCallback callbacks))) (c_duckdb_v2_copy_to_set_flush_callback function)
    , optional (copyToFinalizeCallback callbacks) $ \callback -> install allocate DuckDBV2CopyToFinalizeCallbackFn (DuckDBV2CopyToFinalizeCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_copy_to_set_finalize_callback function)
    , optional (copyToStatisticsCallback callbacks) $ \callback -> install allocate DuckDBV2CopyToStatisticsCallbackFn (DuckDBV2CopyToStatisticsCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_copy_to_set_statistics_callback function)
    ]

-- | Allocate the configured COPY FROM callbacks before installing them.
fromInstallers :: Allocator -> DuckDBV2CopyFunctionHandle -> Maybe CopyFromCallbacks -> [IO (IO ())]
fromInstallers _ _ Nothing = []
fromInstallers allocate function (Just callbacks) =
    [ install allocate DuckDBV2CopyFromBindCallbackFn (DuckDBV2CopyFromBindCallbackFn_Aux (guardContext (copyFromBindCallback callbacks))) (c_duckdb_v2_copy_from_set_bind_callback function)
    , optional (copyFromInitGlobalCallback callbacks) $ \callback -> install allocate DuckDBV2CopyFromInitGlobalCallbackFn (DuckDBV2CopyFromInitGlobalCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_copy_from_set_init_global_callback function)
    , optional (copyFromInitLocalCallback callbacks) $ \callback -> install allocate DuckDBV2CopyFromInitLocalCallbackFn (DuckDBV2CopyFromInitLocalCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_copy_from_set_init_local_callback function)
    , install allocate DuckDBV2CopyFromExecCallbackFn (DuckDBV2CopyFromExecCallbackFn_Aux (guardContext (copyFromExecCallback callbacks))) (c_duckdb_v2_copy_from_set_exec_callback function)
    , optional (copyFromProgressCallback callbacks) $ \callback -> install allocate DuckDBV2CopyFromProgressCallbackFn (DuckDBV2CopyFromProgressCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_copy_from_set_progress_callback function)
    ]

-- | Allocate one generated callback and defer its checked setter.
install :: (ToFunPtr auxiliary) => Allocator -> (FunPtr auxiliary -> callback) -> auxiliary -> (callback -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO (IO ())
install allocate wrap callback setter = do
    pointer <- wrap <$> allocate (toFunPtr callback)
    pure (checked (setter pointer))

-- | Allocate an optional callback when it is present.
optional :: Maybe a -> (a -> IO (IO ())) -> IO (IO ())
optional Nothing _ = pure (pure ())
optional (Just value) action = action value

-- | Translate a callback exception through its native error channel.
guardContext :: ContextCallback info -> info -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
guardContext callback info context err = runCallback err (callback info context)

-- | Register row-oriented COPY TO callbacks. Native batch buffers are copied.
registerCopyToFunction ::
    forall bindState globalState.
    Connection ->
    Text ->
    (CopyBindInfo -> IO bindState) ->
    (CopyInitInfo bindState -> IO globalState) ->
    (CopySinkInfo bindState globalState -> [[Field]] -> IO ()) ->
    (CopyFinalizeInfo bindState globalState -> IO ()) ->
    IO ()
registerCopyToFunction connection name bindFn initFn sinkFn finalizeFn = withDatabaseHandle connection \instanceHandle ->
    registerCopyToCallbacks
        connection
        name
        (copyToCallbacks (batch instanceHandle) flush)
            { copyToBindCallback = Just bind
            , copyToInitCallback = Just initialize
            , copyToFinalizeCallback = Just finalize
            }
  where
    bind info _ = mask \restore -> do
        count <- output (c_duckdb_v2_copy_to_bind_get_column_count info)
        types <- acquireTypes info count [] 0
        state <- restore (mapM (output . c_duckdb_v2_logical_type_get_id) types >>= bindFn . CopyBindInfo) `onException` mapM_ destroyLogicalType types
        stable <- newStablePtr state `onException` mapM_ destroyLogicalType types
        let release = freeStablePtr stable >> mapM_ destroyLogicalType types
        storage <- newStablePtr (CopyBindingStorage (castStablePtrToPtr stable) types) `onException` release
        attachOpaque (c_duckdb_v2_copy_to_bind_set_bind_data info) (castPtr (castStablePtrToPtr storage)) copyBindingDestructor
            `onException` (freeStablePtr storage >> release)
    initialize info _ = do
        (state, _) <- readCopyBinding (c_duckdb_v2_copy_to_init_get_bind_data info)
        path <- output (c_duckdb_v2_copy_to_init_get_file_path info) >>= readText
        global <- initFn (CopyInitInfo state (Text.unpack path))
        setCallbackState (c_duckdb_v2_copy_to_init_set_init_data info) global
    batch instanceHandle info context = withContextFactory context $ \factory -> do
        (_, types) <- readCopyBinding (c_duckdb_v2_copy_to_batch_get_bind_data info) :: IO (bindState, [DuckDBV2LogicalTypeHandle])
        rows <- bracket (output (c_duckdb_v2_copy_to_batch_take_input info)) (destroy c_duckdb_v2_column_data_collection_destroy) (materializeCollection factory (readVariant instanceHandle) types)
        setCallbackState (c_duckdb_v2_copy_to_batch_set_batch_data info) rows
    readVariant instanceHandle vector =
        bracket (output (c_duckdb_v2_connection_create instanceHandle)) (destroy c_duckdb_v2_connection_destroy) \helper ->
            prepareVariantReader helper vector
    flush info _ = do
        (state, _) <- readCopyBinding (c_duckdb_v2_copy_to_flush_get_bind_data info)
        global <- getCallbackState (c_duckdb_v2_copy_to_flush_get_init_data info)
        rows <- getCallbackState (c_duckdb_v2_copy_to_flush_get_batch_data info)
        sinkFn (CopySinkInfo state global) rows
    finalize info _ = do
        (state, _) <- readCopyBinding (c_duckdb_v2_copy_to_finalize_get_bind_data info)
        global <- getCallbackState (c_duckdb_v2_copy_to_finalize_get_init_data info)
        finalizeFn (CopyFinalizeInfo state global)

-- | A bind state and its owned logical descriptors share one native lifetime.
data CopyBindingStorage = CopyBindingStorage !(Ptr ()) ![DuckDBV2LogicalTypeHandle]

-- | Retain the owned bind descriptors and release them on acquisition failure.
acquireTypes :: DuckDBV2CopyToBindInfoHandle -> DuckDBIdx -> [DuckDBV2LogicalTypeHandle] -> DuckDBIdx -> IO [DuckDBV2LogicalTypeHandle]
acquireTypes info count taken index
    | index == count = pure (reverse taken)
    | otherwise = do
        logical <- output (c_duckdb_v2_copy_to_bind_get_column_type info index) `onException` mapM_ destroyLogicalType taken
        acquireTypes info count (logical : taken) (index + 1)

-- | Read the row wrapper state with its retained logical descriptors.
readCopyBinding :: (Ptr (Ptr Void) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO (a, [DuckDBV2LogicalTypeHandle])
readCopyBinding getter = do
    CopyBindingStorage raw types <- getCallbackState getter
    state <- deRefStablePtr (castPtrToStablePtr raw)
    pure (state, types)

-- | Release native bind descriptors before their database closes.
releaseCopyBinding :: Ptr Void -> IO ()
releaseCopyBinding raw =
    mask_
        $ ignoreCallbackExceptions
        $ if raw == nullPtr
            then pure ()
            else do
                let storage = castPtrToStablePtr (castPtr raw) :: StablePtr CopyBindingStorage
                ( do
                        CopyBindingStorage state types <- deRefStablePtr storage
                        mapM_ destroyLogicalType types `finally` freeStablePtr (castPtrToStablePtr state)
                    )
                    `finally` freeStablePtr storage

foreign export ccall "duckdb_simple_release_copy_binding" releaseCopyBinding :: Ptr Void -> IO ()
foreign import ccall "&duckdb_simple_release_copy_binding" copyBindingDestructorAddress :: FunPtr (Ptr Void -> IO ())

-- | Use a static destructor address for native bind-state ownership.
copyBindingDestructor :: DuckDBV2OpaqueDestroyFn
copyBindingDestructor = DuckDBV2OpaqueDestroyFn (castFunPtr copyBindingDestructorAddress)

-- | Copy each scanned chunk before the scan invalidates its borrowed buffers.
materializeCollection :: DuckDBV2FactoryHandle -> VariantReader -> [DuckDBV2LogicalTypeHandle] -> DuckDBV2ColumnDataCollectionHandle -> IO [[Field]]
materializeCollection factory variantReader types collection =
    withArray types \typePtr ->
        bracket (output (c_duckdb_v2_data_chunk_create factory (ConstPtr typePtr) (fromIntegral (length types)))) (destroy c_duckdb_v2_data_chunk_destroy) \chunk ->
            bracket (output (c_duckdb_v2_column_data_collection_shared_scan_state_create collection)) (destroy c_duckdb_v2_column_data_collection_shared_scan_state_destroy) \shared ->
                bracket (output (c_duckdb_v2_column_data_collection_worker_scan_state_create collection)) (destroy c_duckdb_v2_column_data_collection_worker_scan_state_destroy) \worker ->
                    let loop groups = do
                            produced <- output (c_duckdb_v2_column_data_collection_scan collection shared worker chunk)
                            if produced == 0
                                then pure (concat (reverse groups))
                                else do
                                    rows <- materializeChunkRows variantReader chunk
                                    loop (rows : groups)
                     in loop []

-- | Copy a complete chunk before its collection scan advances.
materializeChunkRows :: VariantReader -> DuckDBV2DataChunkHandle -> IO [[Field]]
materializeChunkRows variantReader chunk = do
    columns <- output (c_duckdb_v2_data_chunk_get_vector_count chunk)
    readers <- forM [0 .. fromIntegral columns - 1] \index -> do
        vector <- output (c_duckdb_v2_data_chunk_get_vector chunk (fromIntegral (index :: Int)))
        readValue <- prepareVectorReader variantReader vector
        pure \row -> Field (Text.pack ("column" <> show index)) index <$> readValue row
    rows <- output (c_duckdb_v2_data_chunk_get_size chunk)
    forM [0 .. fromIntegral rows - 1] \row -> mapM ($ row) readers

-- | Options supplied to DuckDB's multi-file wrapper.
data MultiFileOptions = MultiFileOptions
    { multiFileReaderType :: Maybe Text
    , multiFileExtension :: Maybe Text
    }
    deriving (Eq, Show)

-- | Use native defaults for reader descriptions and directory expansion.
defaultMultiFileOptions :: MultiFileOptions
defaultMultiFileOptions = MultiFileOptions Nothing Nothing

-- | Register globbing, file lists, filename columns, and hive partition support.
registerMultiFileFunction :: Connection -> Text -> Text -> MultiFileOptions -> IO ()
registerMultiFileFunction connection name singleFile MultiFileOptions{multiFileReaderType, multiFileExtension} = do
    when (any (\value -> Text.null value || Text.any (== '\0') value) [name, singleFile]) $ throwRegistrationError "invalid multi-file function name"
    registerMultiFileFunctionWith (ConnectionTarget connection) \function -> do
        withText name (\(ConstPtr view) -> checked (c_duckdb_v2_multi_file_function_set_name function view))
        withText singleFile (\(ConstPtr view) -> checked (c_duckdb_v2_multi_file_function_set_single_file_function function view))
        forM_ multiFileReaderType \value -> withText value (\(ConstPtr view) -> checked (c_duckdb_v2_multi_file_function_set_reader_type function view))
        forM_ multiFileExtension \value -> withText value (\(ConstPtr view) -> checked (c_duckdb_v2_multi_file_function_set_file_extension function view))

-- | Configure a multi-file wrapper on a connection or borrowed extension target.
registerMultiFileFunctionWith :: RegistrationTarget -> (DuckDBV2MultiFileFunctionHandle -> IO ()) -> IO ()
registerMultiFileFunctionWith target configure =
    withTarget target c_duckdb_v2_multi_file_function_create c_duckdb_v2_multi_file_function_destroy c_duckdb_v2_connection_register_multi_file_function c_duckdb_v2_extension_register_multi_file_function \function register -> do
        configure function
        register

-- | Keep the target alive while borrowing its factory and registration builder.
withTarget ::
    (Storable handle) =>
    RegistrationTarget ->
    (DuckDBV2FactoryHandle -> Ptr handle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    (Ptr handle -> IO DuckDBV2Error) ->
    (DuckDBV2ConnectionHandle -> handle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    (DuckDBV2ExtensionHandle -> handle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    (handle -> IO () -> IO a) ->
    IO a
withTarget target create release registerConnection registerExtension action = case target of
    ConnectionTarget connection -> withConnectionHandle connection $ \native ->
        withConnectionFactory native $ \factory ->
            bracket (output (create factory)) (destroy release) $ \handle -> action handle (checked (registerConnection native handle))
    ExtensionTarget extension context -> withContextFactory context $ \factory ->
        bracket (output (create factory)) (destroy release) $ \handle -> action handle (checked (registerExtension extension handle))
