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
import Data.Void (Void)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Callback (runCallback, transferCallbackState, withCallbackResources)
import Database.DuckDB.Simple.FromField (Field (..))
import Database.DuckDB.Simple.Internal (Connection, destroyLogicalType, peekUtf8CString, throwRegistrationError, withConnectionHandle)
import Database.DuckDB.Simple.Materialize (prepareVectorReader)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, deRefStablePtr)
import Foreign.Storable (poke)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)

-- | Bind-phase metadata for a custom `COPY ... TO` function.
data CopyBindInfo = CopyBindInfo
    { copyBindColumnTypes :: ![DUCKDB_TYPE]
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
    { copyBindPtr :: !Duckdb_copy_function_bind_t
    , copyInitPtr :: !Duckdb_copy_function_global_init_t
    , copySinkPtr :: !Duckdb_copy_function_sink_t
    , copyFinalizePtr :: !Duckdb_copy_function_finalize_t
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
    bracket duckdb_create_copy_function destroyCopyFunction \copyFun -> do
        when (copyFun == Duckdb_copy_function nullPtr) $ throwRegistrationError "allocate copy function"
        withCallbackResources
            ( \allocate -> do
                copyBindPtr <- Duckdb_copy_function_bind_t <$> allocate (toFunPtr (Duckdb_copy_function_bind_t_Aux (copyBindHandler bindFn)))
                copyInitPtr <- Duckdb_copy_function_global_init_t <$> allocate (toFunPtr (Duckdb_copy_function_global_init_t_Aux (copyGlobalInitHandler initFn)))
                copySinkPtr <- Duckdb_copy_function_sink_t <$> allocate (toFunPtr (Duckdb_copy_function_sink_t_Aux (copySinkHandler sinkFn)))
                copyFinalizePtr <- Duckdb_copy_function_finalize_t <$> allocate (toFunPtr (Duckdb_copy_function_finalize_t_Aux (copyFinalizeHandler finalizeFn)))
                pure CopyFunctionResources{copyBindPtr, copyInitPtr, copySinkPtr, copyFinalizePtr}
            )
            (duckdb_copy_function_set_extra_info copyFun)
            \CopyFunctionResources{copyBindPtr, copyInitPtr, copySinkPtr, copyFinalizePtr} -> do
                TextForeign.withCString name $ duckdb_copy_function_set_name copyFun . ConstPtr
                duckdb_copy_function_set_bind copyFun copyBindPtr
                duckdb_copy_function_set_global_init copyFun copyInitPtr
                duckdb_copy_function_set_sink copyFun copySinkPtr
                duckdb_copy_function_set_finalize copyFun copyFinalizePtr
                withConnectionHandle conn \connPtr -> do
                    rc <- duckdb_register_copy_function connPtr copyFun
                    when (rc /= DuckDBSuccess) $ throwRegistrationError "register copy function"

copyBindHandler ::
    forall bindState.
    (CopyBindInfo -> IO bindState) ->
    Duckdb_copy_function_bind_info ->
    IO ()
copyBindHandler bindFn info =
    runCallback (duckdb_copy_function_bind_set_error info) do
        copyBindColumnTypes <- fetchColumnTypes info
        bindState <- bindFn CopyBindInfo{copyBindColumnTypes}
        transferCallbackState (duckdb_copy_function_bind_set_bind_data info) bindState

copyGlobalInitHandler ::
    forall bindState globalState.
    (CopyInitInfo bindState -> IO globalState) ->
    Duckdb_copy_function_global_init_info ->
    IO ()
copyGlobalInitHandler initFn info =
    runCallback (duckdb_copy_function_global_init_set_error info) do
        rawBindState <- duckdb_copy_function_global_init_get_bind_data info
        when (rawBindState == nullPtr) $
            throwRegistrationError "missing copy bind state"
        bindState <- deRefStablePtr (castPtrToStablePtr (castPtr rawBindState) :: StablePtr bindState)
        pathPtr <- duckdb_copy_function_global_init_get_file_path info
        filePath <-
            if pathPtr == ConstPtr nullPtr
                then pure ""
                else Text.unpack <$> peekUtf8CString pathPtr
        globalState <- initFn CopyInitInfo{copyInitBindState = bindState, copyInitFilePath = filePath}
        transferCallbackState (duckdb_copy_function_global_init_set_global_state info) globalState

copySinkHandler ::
    forall bindState globalState.
    (CopySinkInfo bindState globalState -> [[Field]] -> IO ()) ->
    Duckdb_copy_function_sink_info ->
    Duckdb_data_chunk ->
    IO ()
copySinkHandler sinkFn info chunk =
    runCallback (duckdb_copy_function_sink_set_error info) do
        bindState <- readStablePtrState duckdb_copy_function_sink_get_bind_data info
        globalState <- readStablePtrState duckdb_copy_function_sink_get_global_state info
        rows <- materializeChunkRows chunk
        sinkFn CopySinkInfo{copySinkBindState = bindState, copySinkGlobalState = globalState} rows

copyFinalizeHandler ::
    forall bindState globalState.
    (CopyFinalizeInfo bindState globalState -> IO ()) ->
    Duckdb_copy_function_finalize_info ->
    IO ()
copyFinalizeHandler finalizeFn info =
    runCallback (duckdb_copy_function_finalize_set_error info) do
        bindState <- readStablePtrState duckdb_copy_function_finalize_get_bind_data info
        globalState <- readStablePtrState duckdb_copy_function_finalize_get_global_state info
        finalizeFn CopyFinalizeInfo{copyFinalizeBindState = bindState, copyFinalizeGlobalState = globalState}

fetchColumnTypes :: Duckdb_copy_function_bind_info -> IO [DUCKDB_TYPE]
fetchColumnTypes info = do
    count <- duckdb_copy_function_bind_get_column_count info
    let indices = [0 .. fromIntegral count - 1] :: [Int]
    forM indices \idx -> do
        bracket
            (duckdb_copy_function_bind_get_column_type info (fromIntegral idx))
            destroyLogicalType
            (fmap (\(Duckdb_type dtype) -> dtype) . duckdb_get_type_id)

readStablePtrState :: forall a i. (i -> IO (Ptr Void)) -> i -> IO a
readStablePtrState getter info = do
    rawPtr <- getter info
    if rawPtr == nullPtr
        then throwRegistrationError "missing copy callback state"
        else deRefStablePtr (castPtrToStablePtr (castPtr rawPtr) :: StablePtr a)

materializeChunkRows :: Duckdb_data_chunk -> IO [[Field]]
materializeChunkRows chunk = do
    rawColumnCount <- duckdb_data_chunk_get_column_count chunk
    let columnCount = fromIntegral rawColumnCount :: Int
    readers <- mapM (makeColumnReader chunk) [0 .. columnCount - 1]
    rawRowCount <- duckdb_data_chunk_get_size chunk
    let rowCount = fromIntegral rawRowCount :: Int
    forM [0 .. rowCount - 1] \row ->
        forM readers \reader ->
            reader (fromIntegral row)

type ColumnReader = Idx_t -> IO Field

makeColumnReader :: Duckdb_data_chunk -> Int -> IO ColumnReader
makeColumnReader chunk columnIndex = do
    readValue <- duckdb_data_chunk_get_vector chunk (fromIntegral columnIndex) >>= prepareVectorReader
    let name = Text.pack ("column" <> show columnIndex)
    pure \rowIdx -> do
        fieldValue <- readValue (fromIntegral rowIdx)
        pure Field{fieldName = name, fieldIndex = columnIndex, fieldValue}

destroyCopyFunction :: Duckdb_copy_function -> IO ()
destroyCopyFunction copyFun =
    alloca \ptr -> poke ptr copyFun >> duckdb_destroy_copy_function ptr
