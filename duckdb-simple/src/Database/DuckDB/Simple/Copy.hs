{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Register COPY formats and multi-file table functions with Haskell
functions. The functions get and give Haskell values. Batch preparation can run
concurrently. The sink calls for one output file run one at a time. A
multi-file function uses the callbacks of a registered single-file table
function. "Database.DuckDB.Simple.V2.Copy" has the native callback interface.
-}
module Database.DuckDB.Simple.Copy (
    CopyBindInfo (..),
    CopyInitInfo (..),
    CopySinkInfo (..),
    CopyFinalizeInfo (..),
    registerCopyToFunction,
    registerCopyToFunctionOn,
    MultiFileOptions (..),
    defaultMultiFileOptions,
    registerMultiFileFunction,
    registerMultiFileFunctionOn,
) where

import Control.Exception (bracket, finally, mask, mask_, onException, throwIO)
import Control.Monad (forM, forM_, when)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Database.DuckDB.Simple.Callback (attachOpaque, ignoreCallbackExceptions, readCallbackState, transferCallbackState)
import Database.DuckDB.Simple.FromField (Field (..))
import Database.DuckDB.Simple.Internal (Connection, withDatabaseHandle)
import Database.DuckDB.Simple.Materialize (VariantReader, castReader, prepareVariantReader, prepareVectorReader)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.V2.Copy (CopyFromCallbacks, CopyToCallbacks (..), RegistrationTarget (..), copyToCallbacks, registerCopyFunctionWith, registerMultiFileFunctionWith)
import Database.DuckDB.Simple.VariantCodec (destroyVariantState, newVariantState)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)

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

-- | Register row-oriented COPY TO callbacks. Native batch buffers are copied.
registerCopyToFunction ::
    Connection ->
    Text ->
    (CopyBindInfo -> IO bindState) ->
    (CopyInitInfo bindState -> IO globalState) ->
    (CopySinkInfo bindState globalState -> [[Field]] -> IO ()) ->
    (CopyFinalizeInfo bindState globalState -> IO ()) ->
    IO ()
registerCopyToFunction = registerCopyToFunctionOn . ConnectionTarget

{- | Register row-oriented COPY TO callbacks on a connection or a loading
extension. The rules of 'registerCopyToFunction' apply. A COPY TO function of
an extension cannot read a VARIANT column.
-}
registerCopyToFunctionOn ::
    RegistrationTarget ->
    Text ->
    (CopyBindInfo -> IO bindState) ->
    (CopyInitInfo bindState -> IO globalState) ->
    (CopySinkInfo bindState globalState -> [[Field]] -> IO ()) ->
    (CopyFinalizeInfo bindState globalState -> IO ()) ->
    IO ()
registerCopyToFunctionOn target name bindFn initFn sinkFn finalizeFn =
    withVariantReader target \readVariant ->
        registerCopyOn target name (Just (copyToSide readVariant bindFn initFn sinkFn finalizeFn)) Nothing

-- | Check the format name and register the native callbacks of each side.
registerCopyOn :: RegistrationTarget -> Text -> Maybe CopyToCallbacks -> Maybe CopyFromCallbacks -> IO ()
registerCopyOn target name to from = do
    when (Text.null name || Text.any (== '\0') name) $ throwInvalidArgument "invalid copy function name"
    registerCopyFunctionWith target (\function -> withIdentifier name (checked . c_duckdb_v2_copy_function_set_name function)) to from

{- | Give the VARIANT reader of a registration target to an action. On a
connection, the reader decodes VARIANT through a helper connection of the
database. An extension target has no instance handle, so its reader raises an
error.
-}
withVariantReader :: RegistrationTarget -> (VariantReader -> IO a) -> IO a
withVariantReader target action = case target of
    ConnectionTarget connection -> withDatabaseHandle connection (action . readVariant)
    ExtensionTarget _ _ -> action \_ -> throwIO (userError "duckdb-simple: a COPY TO function of an extension cannot read VARIANT")
  where
    readVariant instanceHandle vector =
        bracket (output (c_duckdb_v2_connection_create instanceHandle)) (destroy c_duckdb_v2_connection_destroy) \helper ->
            bracket newVariantState destroyVariantState \state -> prepareVariantReader helper state vector

-- | The native COPY TO callbacks of the row-oriented functions.
copyToSide ::
    forall bindState globalState.
    VariantReader ->
    (CopyBindInfo -> IO bindState) ->
    (CopyInitInfo bindState -> IO globalState) ->
    (CopySinkInfo bindState globalState -> [[Field]] -> IO ()) ->
    (CopyFinalizeInfo bindState globalState -> IO ()) ->
    CopyToCallbacks
copyToSide readVariant bindFn initFn sinkFn finalizeFn =
    (copyToCallbacks batch flush)
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
        transferCallbackState (attachOpaque (c_duckdb_v2_copy_to_init_set_init_data info)) global
    batch info context = withContextFactory context $ \factory -> do
        (_, types) <- readCopyBinding (c_duckdb_v2_copy_to_batch_get_bind_data info) :: IO (bindState, [DuckDBV2LogicalTypeHandle])
        rows <- bracket (output (c_duckdb_v2_copy_to_batch_take_input info)) (destroy c_duckdb_v2_column_data_collection_destroy) (materializeCollection factory readVariant types)
        transferCallbackState (attachOpaque (c_duckdb_v2_copy_to_batch_set_batch_data info)) rows
    flush info _ = do
        (state, _) <- readCopyBinding (c_duckdb_v2_copy_to_flush_get_bind_data info)
        global <- readCallbackState (c_duckdb_v2_copy_to_flush_get_init_data info)
        rows <- readCallbackState (c_duckdb_v2_copy_to_flush_get_batch_data info)
        sinkFn (CopySinkInfo state global) rows
    finalize info _ = do
        (state, _) <- readCopyBinding (c_duckdb_v2_copy_to_finalize_get_bind_data info)
        global <- readCallbackState (c_duckdb_v2_copy_to_finalize_get_init_data info)
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
    CopyBindingStorage raw types <- readCallbackState getter
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
                                    rows <- materializeChunkRows (castReader factory variantReader) chunk
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
registerMultiFileFunction connection = registerMultiFileFunctionOn (ConnectionTarget connection)

{- | Register a multi-file wrapper on a connection or a loading extension. The
second name is the registered single-file table function. Both names must not
be empty and must not contain NUL.
-}
registerMultiFileFunctionOn :: RegistrationTarget -> Text -> Text -> MultiFileOptions -> IO ()
registerMultiFileFunctionOn target name singleFile MultiFileOptions{multiFileReaderType, multiFileExtension} = do
    when (any (\value -> Text.null value || Text.any (== '\0') value) [name, singleFile]) $ throwInvalidArgument "invalid multi-file function name"
    registerMultiFileFunctionWith target \function -> do
        withText name (\(ConstPtr view) -> checked (c_duckdb_v2_multi_file_function_set_name function view))
        withText singleFile (\(ConstPtr view) -> checked (c_duckdb_v2_multi_file_function_set_single_file_function function view))
        forM_ multiFileReaderType \value -> withText value (\(ConstPtr view) -> checked (c_duckdb_v2_multi_file_function_set_reader_type function view))
        forM_ multiFileExtension \value -> withText value (\(ConstPtr view) -> checked (c_duckdb_v2_multi_file_function_set_file_extension function view))
