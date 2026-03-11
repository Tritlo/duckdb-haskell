{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.DuckDB.Simple.Copy (
    CopyBindInfo (..),
    CopyInitInfo (..),
    CopySinkInfo (..),
    CopyFinalizeInfo (..),
    registerCopyToFunction,
) where

import Control.Exception (SomeException, bracket, displayException, onException, throwIO, try)
import Control.Monad (forM, when)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (Field (..))
import Database.DuckDB.Simple.Internal (Connection, SQLError (..), withConnectionHandle)
import Database.DuckDB.Simple.Materialize (materializeValue)
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (poke)

data CopyBindInfo = CopyBindInfo
    { copyBindColumnTypes :: ![DuckDBType]
    }
    deriving (Eq, Show)

data CopyInitInfo bindState = CopyInitInfo
    { copyInitBindState :: !bindState
    , copyInitFilePath :: !FilePath
    }

data CopySinkInfo bindState globalState = CopySinkInfo
    { copySinkBindState :: !bindState
    , copySinkGlobalState :: !globalState
    }

data CopyFinalizeInfo bindState globalState = CopyFinalizeInfo
    { copyFinalizeBindState :: !bindState
    , copyFinalizeGlobalState :: !globalState
    }

data CopyFunctionResources = CopyFunctionResources
    { copyBindPtr :: !DuckDBCopyFunctionBindFun
    , copyInitPtr :: !DuckDBCopyFunctionGlobalInitFun
    , copySinkPtr :: !DuckDBCopyFunctionSinkFun
    , copyFinalizePtr :: !DuckDBCopyFunctionFinalizeFun
    , copyStateDestroyPtr :: !DuckDBDeleteCallback
    }

registerCopyToFunction ::
    forall bindState globalState.
    Connection ->
    Text ->
    (CopyBindInfo -> IO bindState) ->
    (CopyInitInfo bindState -> IO globalState) ->
    (CopySinkInfo bindState globalState -> [[Field]] -> IO ()) ->
    (CopyFinalizeInfo bindState globalState -> IO ()) ->
    IO ()
registerCopyToFunction conn name bindFn initFn sinkFn finalizeFn = do
    stateDestroyCb <- mkDeleteCallback releaseStablePtrData
    bindPtr <- mkCopyBindFun (copyBindHandler stateDestroyCb bindFn)
    initPtr <- mkCopyGlobalInitFun (copyGlobalInitHandler stateDestroyCb initFn)
    sinkPtr <- mkCopySinkFun (copySinkHandler sinkFn)
    finalizePtr <- mkCopyFinalizeFun (copyFinalizeHandler finalizeFn)
    resources <-
        newStablePtr
            CopyFunctionResources
                { copyBindPtr = bindPtr
                , copyInitPtr = initPtr
                , copySinkPtr = sinkPtr
                , copyFinalizePtr = finalizePtr
                , copyStateDestroyPtr = stateDestroyCb
                }
    destroyCb <- mkDeleteCallback releaseCopyResources
    let release =
            freeHaskellFunPtr bindPtr
                >> freeHaskellFunPtr initPtr
                >> freeHaskellFunPtr sinkPtr
                >> freeHaskellFunPtr finalizePtr
                >> freeHaskellFunPtr stateDestroyCb
                >> freeStablePtr resources
                >> freeHaskellFunPtr destroyCb
    bracket c_duckdb_create_copy_function destroyCopyFunction \copyFun ->
        (`onException` release) $ do
            TextForeign.withCString name \cName ->
                c_duckdb_copy_function_set_name copyFun cName
            c_duckdb_copy_function_set_bind copyFun bindPtr
            c_duckdb_copy_function_set_global_init copyFun initPtr
            c_duckdb_copy_function_set_sink copyFun sinkPtr
            c_duckdb_copy_function_set_finalize copyFun finalizePtr
            c_duckdb_copy_function_set_extra_info copyFun (castStablePtrToPtr resources) destroyCb
            withConnectionHandle conn \connPtr -> do
                rc <- c_duckdb_register_copy_function connPtr copyFun
                if rc == DuckDBSuccess
                    then pure ()
                    else throwRegistrationError "register copy function"

copyBindHandler ::
    forall bindState.
    DuckDBDeleteCallback ->
    (CopyBindInfo -> IO bindState) ->
    DuckDBCopyFunctionBindInfo ->
    IO ()
copyBindHandler destroyCb bindFn info = do
    outcome <- try do
        copyBindColumnTypes <- fetchColumnTypes info
        bindState <- bindFn CopyBindInfo{copyBindColumnTypes}
        stable <- newStablePtr bindState
        c_duckdb_copy_function_bind_set_bind_data info (castStablePtrToPtr stable) destroyCb
    reportCopyError c_duckdb_copy_function_bind_set_error info outcome

copyGlobalInitHandler ::
    forall bindState globalState.
    DuckDBDeleteCallback ->
    (CopyInitInfo bindState -> IO globalState) ->
    DuckDBCopyFunctionGlobalInitInfo ->
    IO ()
copyGlobalInitHandler destroyCb initFn info = do
    outcome <- try do
        rawBindState <- c_duckdb_copy_function_global_init_get_bind_data info
        when (rawBindState == nullPtr) $
            throwRegistrationError "missing copy bind state"
        bindState <- deRefStablePtr (castPtrToStablePtr rawBindState :: StablePtr bindState)
        pathPtr <- c_duckdb_copy_function_global_init_get_file_path info
        filePath <-
            if pathPtr == nullPtr
                then pure ""
                else peekCString pathPtr
        globalState <- initFn CopyInitInfo{copyInitBindState = bindState, copyInitFilePath = filePath}
        stable <- newStablePtr globalState
        c_duckdb_copy_function_global_init_set_global_state info (castStablePtrToPtr stable) destroyCb
    reportCopyError c_duckdb_copy_function_global_init_set_error info outcome

copySinkHandler ::
    forall bindState globalState.
    (CopySinkInfo bindState globalState -> [[Field]] -> IO ()) ->
    DuckDBCopyFunctionSinkInfo ->
    DuckDBDataChunk ->
    IO ()
copySinkHandler sinkFn info chunk = do
    outcome <- try do
        bindState <- readStablePtrState c_duckdb_copy_function_sink_get_bind_data info
        globalState <- readStablePtrState c_duckdb_copy_function_sink_get_global_state info
        rows <- materializeChunkRows chunk
        sinkFn CopySinkInfo{copySinkBindState = bindState, copySinkGlobalState = globalState} rows
    reportCopyError c_duckdb_copy_function_sink_set_error info outcome

copyFinalizeHandler ::
    forall bindState globalState.
    (CopyFinalizeInfo bindState globalState -> IO ()) ->
    DuckDBCopyFunctionFinalizeInfo ->
    IO ()
copyFinalizeHandler finalizeFn info = do
    outcome <- try do
        bindState <- readStablePtrState c_duckdb_copy_function_finalize_get_bind_data info
        globalState <- readStablePtrState c_duckdb_copy_function_finalize_get_global_state info
        finalizeFn CopyFinalizeInfo{copyFinalizeBindState = bindState, copyFinalizeGlobalState = globalState}
    reportCopyError c_duckdb_copy_function_finalize_set_error info outcome

reportCopyError :: (i -> CString -> IO ()) -> i -> Either SomeException () -> IO ()
reportCopyError _ _ (Right ()) = pure ()
reportCopyError setError info (Left err) =
    TextForeign.withCString (Text.pack (displayException err)) \cMsg ->
        setError info cMsg

fetchColumnTypes :: DuckDBCopyFunctionBindInfo -> IO [DuckDBType]
fetchColumnTypes info = do
    count <- c_duckdb_copy_function_bind_get_column_count info
    let indices = [0 .. fromIntegral count - 1] :: [Int]
    forM indices \idx -> do
        logical <- c_duckdb_copy_function_bind_get_column_type info (fromIntegral idx)
        dtype <- c_duckdb_get_type_id logical
        destroyLogicalType logical
        pure dtype

readStablePtrState :: forall a i. (i -> IO (Ptr ())) -> i -> IO a
readStablePtrState getter info = do
    rawPtr <- getter info
    if rawPtr == nullPtr
        then throwRegistrationError "missing copy callback state"
        else deRefStablePtr (castPtrToStablePtr rawPtr :: StablePtr a)

materializeChunkRows :: DuckDBDataChunk -> IO [[Field]]
materializeChunkRows chunk = do
    rawColumnCount <- c_duckdb_data_chunk_get_column_count chunk
    let columnCount = fromIntegral rawColumnCount :: Int
    readers <- mapM (makeColumnReader chunk) [0 .. columnCount - 1]
    rawRowCount <- c_duckdb_data_chunk_get_size chunk
    let rowCount = fromIntegral rawRowCount :: Int
    forM [0 .. rowCount - 1] \row ->
        forM readers \reader ->
            reader (fromIntegral row)

type ColumnReader = DuckDBIdx -> IO Field

makeColumnReader :: DuckDBDataChunk -> Int -> IO ColumnReader
makeColumnReader chunk columnIndex = do
    vector <- c_duckdb_data_chunk_get_vector chunk (fromIntegral columnIndex)
    logical <- c_duckdb_vector_get_column_type vector
    dtype <- c_duckdb_get_type_id logical
    destroyLogicalType logical
    dataPtr <- c_duckdb_vector_get_data vector
    validity <- c_duckdb_vector_get_validity vector
    let name = Text.pack ("column" <> show columnIndex)
    pure \rowIdx -> do
        fieldValue <- materializeValue dtype vector dataPtr validity (fromIntegral rowIdx)
        pure Field{fieldName = name, fieldIndex = columnIndex, fieldValue}

releaseCopyResources :: Ptr () -> IO ()
releaseCopyResources rawPtr =
    when (rawPtr /= nullPtr) $ do
        let stablePtr = castPtrToStablePtr rawPtr :: StablePtr CopyFunctionResources
        CopyFunctionResources{copyBindPtr, copyInitPtr, copySinkPtr, copyFinalizePtr, copyStateDestroyPtr} <- deRefStablePtr stablePtr
        freeHaskellFunPtr copyBindPtr
        freeHaskellFunPtr copyInitPtr
        freeHaskellFunPtr copySinkPtr
        freeHaskellFunPtr copyFinalizePtr
        freeHaskellFunPtr copyStateDestroyPtr
        freeStablePtr stablePtr

releaseStablePtrData :: Ptr () -> IO ()
releaseStablePtrData rawPtr =
    when (rawPtr /= nullPtr) $
        freeStablePtr (castPtrToStablePtr rawPtr :: StablePtr ())

destroyCopyFunction :: DuckDBCopyFunction -> IO ()
destroyCopyFunction copyFun =
    alloca \ptr -> poke ptr copyFun >> c_duckdb_destroy_copy_function ptr

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType logicalType =
    alloca \ptr -> poke ptr logicalType >> c_duckdb_destroy_logical_type ptr

throwRegistrationError :: String -> IO a
throwRegistrationError label =
    throwIO
        SQLError
            { sqlErrorMessage = Text.pack ("duckdb-simple: " <> label <> " failed")
            , sqlErrorType = Nothing
            , sqlErrorQuery = Nothing
            }

foreign import ccall "wrapper"
    mkCopyBindFun :: (DuckDBCopyFunctionBindInfo -> IO ()) -> IO DuckDBCopyFunctionBindFun

foreign import ccall "wrapper"
    mkCopyGlobalInitFun :: (DuckDBCopyFunctionGlobalInitInfo -> IO ()) -> IO DuckDBCopyFunctionGlobalInitFun

foreign import ccall "wrapper"
    mkCopySinkFun :: (DuckDBCopyFunctionSinkInfo -> DuckDBDataChunk -> IO ()) -> IO DuckDBCopyFunctionSinkFun

foreign import ccall "wrapper"
    mkCopyFinalizeFun :: (DuckDBCopyFunctionFinalizeInfo -> IO ()) -> IO DuckDBCopyFunctionFinalizeFun

foreign import ccall "wrapper"
    mkDeleteCallback :: (Ptr () -> IO ()) -> IO DuckDBDeleteCallback
