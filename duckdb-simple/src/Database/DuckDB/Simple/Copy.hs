{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.Copy
Description : High-level wrappers for DuckDB custom COPY functions.
-}
module Database.DuckDB.Simple.Copy (
    CopyBindInfo (..),
    CopyInitInfo (..),
    CopySinkInfo (..),
    CopyFinalizeInfo (..),
    registerCopyToFunction,
) where

import Control.Exception (bracket)
import Control.Monad (forM, when)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Callback (runCallback, transferCallbackState, withCallbackResources)
import Database.DuckDB.Simple.FromField (Field (..))
import Database.DuckDB.Simple.Internal (Connection, destroyLogicalType, peekUtf8CString, throwRegistrationError, withConnectionHandle)
import Database.DuckDB.Simple.Materialize (prepareValueReader)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, deRefStablePtr)
import Foreign.Storable (poke)

-- | Bind-phase metadata for a custom `COPY ... TO` function.
data CopyBindInfo = CopyBindInfo
    { copyBindColumnTypes :: ![DuckDBType]
    }
    deriving (Eq, Show)

-- | Init-phase inputs for a custom `COPY ... TO` function.
data CopyInitInfo bindState = CopyInitInfo
    { copyInitBindState :: !bindState
    , copyInitFilePath :: !FilePath
    }

-- | Sink-phase inputs for a custom `COPY ... TO` function.
data CopySinkInfo bindState globalState = CopySinkInfo
    { copySinkBindState :: !bindState
    , copySinkGlobalState :: !globalState
    }

-- | Finalize-phase inputs for a custom `COPY ... TO` function.
data CopyFinalizeInfo bindState globalState = CopyFinalizeInfo
    { copyFinalizeBindState :: !bindState
    , copyFinalizeGlobalState :: !globalState
    }

data CopyFunctionResources = CopyFunctionResources
    { copyBindPtr :: !DuckDBCopyFunctionBindFun
    , copyInitPtr :: !DuckDBCopyFunctionGlobalInitFun
    , copySinkPtr :: !DuckDBCopyFunctionSinkFun
    , copyFinalizePtr :: !DuckDBCopyFunctionFinalizeFun
    }

-- | Register a custom `COPY ... TO` implementation backed by Haskell callbacks.
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
    when (Text.null name || Text.any (== '\0') name) $
        throwRegistrationError "invalid copy function name"
    bracket c_duckdb_create_copy_function destroyCopyFunction \copyFun -> do
        when (copyFun == nullPtr) $ throwRegistrationError "allocate copy function"
        withCallbackResources
            ( \allocate -> do
                copyBindPtr <- allocate (mkCopyBindFun (copyBindHandler bindFn))
                copyInitPtr <- allocate (mkCopyGlobalInitFun (copyGlobalInitHandler initFn))
                copySinkPtr <- allocate (mkCopySinkFun (copySinkHandler sinkFn))
                copyFinalizePtr <- allocate (mkCopyFinalizeFun (copyFinalizeHandler finalizeFn))
                pure CopyFunctionResources{copyBindPtr, copyInitPtr, copySinkPtr, copyFinalizePtr}
            )
            (c_duckdb_copy_function_set_extra_info copyFun)
            \CopyFunctionResources{copyBindPtr, copyInitPtr, copySinkPtr, copyFinalizePtr} -> do
                TextForeign.withCString name $ c_duckdb_copy_function_set_name copyFun
                c_duckdb_copy_function_set_bind copyFun copyBindPtr
                c_duckdb_copy_function_set_global_init copyFun copyInitPtr
                c_duckdb_copy_function_set_sink copyFun copySinkPtr
                c_duckdb_copy_function_set_finalize copyFun copyFinalizePtr
                withConnectionHandle conn \connPtr -> do
                    rc <- c_duckdb_register_copy_function connPtr copyFun
                    when (rc /= DuckDBSuccess) $ throwRegistrationError "register copy function"

copyBindHandler ::
    forall bindState.
    (CopyBindInfo -> IO bindState) ->
    DuckDBCopyFunctionBindInfo ->
    IO ()
copyBindHandler bindFn info =
    runCallback (c_duckdb_copy_function_bind_set_error info) do
        copyBindColumnTypes <- fetchColumnTypes info
        bindState <- bindFn CopyBindInfo{copyBindColumnTypes}
        transferCallbackState (c_duckdb_copy_function_bind_set_bind_data info) bindState

copyGlobalInitHandler ::
    forall bindState globalState.
    (CopyInitInfo bindState -> IO globalState) ->
    DuckDBCopyFunctionGlobalInitInfo ->
    IO ()
copyGlobalInitHandler initFn info =
    runCallback (c_duckdb_copy_function_global_init_set_error info) do
        rawBindState <- c_duckdb_copy_function_global_init_get_bind_data info
        when (rawBindState == nullPtr) $
            throwRegistrationError "missing copy bind state"
        bindState <- deRefStablePtr (castPtrToStablePtr rawBindState :: StablePtr bindState)
        pathPtr <- c_duckdb_copy_function_global_init_get_file_path info
        filePath <-
            if pathPtr == nullPtr
                then pure ""
                else Text.unpack <$> peekUtf8CString pathPtr
        globalState <- initFn CopyInitInfo{copyInitBindState = bindState, copyInitFilePath = filePath}
        transferCallbackState (c_duckdb_copy_function_global_init_set_global_state info) globalState

copySinkHandler ::
    forall bindState globalState.
    (CopySinkInfo bindState globalState -> [[Field]] -> IO ()) ->
    DuckDBCopyFunctionSinkInfo ->
    DuckDBDataChunk ->
    IO ()
copySinkHandler sinkFn info chunk =
    runCallback (c_duckdb_copy_function_sink_set_error info) do
        bindState <- readStablePtrState c_duckdb_copy_function_sink_get_bind_data info
        globalState <- readStablePtrState c_duckdb_copy_function_sink_get_global_state info
        rows <- materializeChunkRows chunk
        sinkFn CopySinkInfo{copySinkBindState = bindState, copySinkGlobalState = globalState} rows

copyFinalizeHandler ::
    forall bindState globalState.
    (CopyFinalizeInfo bindState globalState -> IO ()) ->
    DuckDBCopyFunctionFinalizeInfo ->
    IO ()
copyFinalizeHandler finalizeFn info =
    runCallback (c_duckdb_copy_function_finalize_set_error info) do
        bindState <- readStablePtrState c_duckdb_copy_function_finalize_get_bind_data info
        globalState <- readStablePtrState c_duckdb_copy_function_finalize_get_global_state info
        finalizeFn CopyFinalizeInfo{copyFinalizeBindState = bindState, copyFinalizeGlobalState = globalState}

fetchColumnTypes :: DuckDBCopyFunctionBindInfo -> IO [DuckDBType]
fetchColumnTypes info = do
    count <- c_duckdb_copy_function_bind_get_column_count info
    let indices = [0 .. fromIntegral count - 1] :: [Int]
    forM indices \idx -> do
        bracket
            (c_duckdb_copy_function_bind_get_column_type info (fromIntegral idx))
            destroyLogicalType
            c_duckdb_get_type_id

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
    dtype <- bracket (c_duckdb_vector_get_column_type vector) destroyLogicalType c_duckdb_get_type_id
    dataPtr <- c_duckdb_vector_get_data vector
    validity <- c_duckdb_vector_get_validity vector
    readValue <- prepareValueReader dtype vector dataPtr validity
    let name = Text.pack ("column" <> show columnIndex)
    pure \rowIdx -> do
        fieldValue <- readValue (fromIntegral rowIdx)
        pure Field{fieldName = name, fieldIndex = columnIndex, fieldValue}

destroyCopyFunction :: DuckDBCopyFunction -> IO ()
destroyCopyFunction copyFun =
    alloca \ptr -> poke ptr copyFun >> c_duckdb_destroy_copy_function ptr

foreign import ccall "wrapper"
    mkCopyBindFun :: (DuckDBCopyFunctionBindInfo -> IO ()) -> IO DuckDBCopyFunctionBindFun

foreign import ccall "wrapper"
    mkCopyGlobalInitFun :: (DuckDBCopyFunctionGlobalInitInfo -> IO ()) -> IO DuckDBCopyFunctionGlobalInitFun

foreign import ccall "wrapper"
    mkCopySinkFun :: (DuckDBCopyFunctionSinkInfo -> DuckDBDataChunk -> IO ()) -> IO DuckDBCopyFunctionSinkFun

foreign import ccall "wrapper"
    mkCopyFinalizeFun :: (DuckDBCopyFunctionFinalizeInfo -> IO ()) -> IO DuckDBCopyFunctionFinalizeFun
