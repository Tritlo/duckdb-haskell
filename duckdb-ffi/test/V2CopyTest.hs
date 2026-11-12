{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module V2CopyTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM, void)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import Database.DuckDB.FFI.V2
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import GHC.Records (getField)
import HsBindgen.Runtime.Prelude (toFunPtr)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (borrowedOpaque, callbackGuard, checked, consume, output, readView, removeTestFile, withConnection, withFactory, withHandle, withResult, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 COPY functions"
        [ testCase "COPY TO callbacks read the user data, the bind data, the init data and the statement" testCopyTo
        ]

-- | Read the name that a getter writes at an index.
readName :: (DuckDBIdx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> DuckDBIdx -> IO String
readName getName index = alloca $ \name -> do
    checked (getName index name)
    peek name >>= readView . coerce

-- | Call a function that must fail. The function gets no error slot.
expectFailure :: String -> (Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO ()
expectFailure what call = do
    code <- call nullPtr
    assertBool (what <> " fails") (code /= DuckDBV2ErrorNone)

-- | Check the columns of a COPY bind: amount BIGINT, then part INTEGER.
expectColumns ::
    (Ptr DuckDBIdx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    (DuckDBIdx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    (DuckDBIdx -> Ptr DuckDBV2LogicalTypeHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    IO ()
expectColumns getCount getName getType = do
    output getCount >>= (@?= 2)
    mapM (readName getName) [0, 1] >>= (@?= ["amount", "part"])
    forM [0, 1] (\i -> withHandle (getType i) c_duckdb_v2_logical_type_destroy (output . c_duckdb_v2_logical_type_get_id))
        >>= (@?= [DuckDBV2LogicalTypeIdBigInt, DuckDBV2LogicalTypeIdInteger])
    alloca $ \name -> expectFailure "a column name after the last column" (getName 2 name)
    alloca $ \slot -> do
        poke slot (coerce nullPtr)
        expectFailure "a column type after the last column" (getType 2 slot) `finally` c_duckdb_v2_logical_type_destroy slot

{- | Check the options of a COPY bind. The statement gives tag 'x', pair (1, 'two') and flag, and the getters
return them ordered by name. The function does not get the FORMAT option and the options of the engine.
-}
expectOptions ::
    (Ptr DuckDBIdx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    (DuckDBIdx -> Ptr DuckDBV2IdentifierT -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    (DuckDBIdx -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) ->
    IO ()
expectOptions getCount getName getValue = do
    output getCount >>= (@?= 3)
    -- Option names are identifiers that match without case, so the test compares them in lower case.
    mapM (fmap (map toLower) . readName getName) [0, 1, 2] >>= (@?= ["flag", "pair", "tag"])
    let value index = withHandle (getValue index) c_duckdb_v2_value_destroy
        text handle = output (c_duckdb_v2_value_get_varchar handle) >>= readView
    -- A bare option is the BOOLEAN true.
    value 0 (output . c_duckdb_v2_value_get_bool) >>= (@?= CBool 1)
    -- A list is a tuple with one child for each element, in order.
    value 1 $ \pair -> do
        output (c_duckdb_v2_value_get_child_count pair) >>= (@?= 2)
        withHandle (c_duckdb_v2_value_get_child pair 0) c_duckdb_v2_value_destroy (output . c_duckdb_v2_value_get_bigint) >>= (@?= 1)
        withHandle (c_duckdb_v2_value_get_child pair 1) c_duckdb_v2_value_destroy text >>= (@?= "two")
    value 2 text >>= (@?= "x")
    alloca $ \name -> expectFailure "an option name after the last option" (getName 3 name)
    alloca $ \slot -> do
        poke slot (coerce nullPtr)
        expectFailure "an option value after the last option" (getValue 3 slot) `finally` c_duckdb_v2_value_destroy slot

-- | Add a value to the front of a list. Callbacks can run on more than one thread.
record :: IORef [a] -> a -> IO ()
record ref value = atomicModifyIORef' ref (\values -> (value : values, ()))

-- | Get a path that does not exist yet: the name of a new temporary file, which the function removes.
freshPath :: String -> IO String
freshPath template = do
    (path, handle) <- openBinaryTempFile "." template
    hClose handle
    withCString path $ \name -> removeTestFile name >>= (@?= 0)
    pure path

testCopyTo :: IO ()
testCopyTo = do
    path <- freshPath "duckdb-v2-copy-to"
    directory <- freshPath "duckdb-v2-copy-dir"
    failures <- newIORef []
    invoked <- newIORef []
    boundPaths <- newIORef []
    filePaths <- newIORef []
    batchRows <- newIORef []
    -- The user data, the bind data and the init data point to three different cells, so an accessor that returns
    -- another one fails.
    allocaBytes 1 $ \user -> allocaBytes 1 $ \bound -> allocaBytes 1 $ \file -> do
        let note name = record invoked name
            expectData getUser getBind = do
                output getUser >>= (@?= user)
                output getBind >>= (@?= bound)
            expectFileData getUser getBind getInit = do
                expectData getUser getBind
                output getInit >>= (@?= file)
            bind info _context err = callbackGuard failures err $ do
                note "bind"
                output (c_duckdb_v2_copy_to_bind_get_user_data info) >>= (@?= user)
                output (c_duckdb_v2_copy_to_bind_get_file_path info) >>= readView >>= record boundPaths
                expectColumns (c_duckdb_v2_copy_to_bind_get_column_count info) (c_duckdb_v2_copy_to_bind_get_column_name info) (c_duckdb_v2_copy_to_bind_get_column_type info)
                expectOptions (c_duckdb_v2_copy_to_bind_get_option_count info) (c_duckdb_v2_copy_to_bind_get_option_name info) (c_duckdb_v2_copy_to_bind_get_option_value info)
                with (borrowedOpaque bound) $ \opaque -> checked (c_duckdb_v2_copy_to_bind_set_bind_data info opaque)
            batchSize info _context err = callbackGuard failures err $ do
                note "batch size"
                expectData (c_duckdb_v2_copy_to_batch_size_get_user_data info) (c_duckdb_v2_copy_to_batch_size_get_bind_data info)
                -- The target is larger than the input, so each file gets one batch.
                checked (c_duckdb_v2_copy_to_batch_size_set_target info 100000)
            initialize info _context err = callbackGuard failures err $ do
                note "init"
                expectData (c_duckdb_v2_copy_to_init_get_user_data info) (c_duckdb_v2_copy_to_init_get_bind_data info)
                output (c_duckdb_v2_copy_to_init_get_file_path info) >>= readView >>= record filePaths
                with (borrowedOpaque file) $ \opaque -> checked (c_duckdb_v2_copy_to_init_set_init_data info opaque)
            batch info _context err = callbackGuard failures err $ do
                note "batch"
                expectFileData (c_duckdb_v2_copy_to_batch_get_user_data info) (c_duckdb_v2_copy_to_batch_get_bind_data info) (c_duckdb_v2_copy_to_batch_get_init_data info)
                withHandle (c_duckdb_v2_copy_to_batch_take_input info) c_duckdb_v2_column_data_collection_destroy $ \input ->
                    output (c_duckdb_v2_column_data_collection_row_count input) >>= record batchRows
            flush info _context err = callbackGuard failures err $ do
                note "flush"
                expectFileData (c_duckdb_v2_copy_to_flush_get_user_data info) (c_duckdb_v2_copy_to_flush_get_bind_data info) (c_duckdb_v2_copy_to_flush_get_init_data info)
            finalize info _context err = callbackGuard failures err $ do
                note "finalize"
                expectFileData (c_duckdb_v2_copy_to_finalize_get_user_data info) (c_duckdb_v2_copy_to_finalize_get_bind_data info) (c_duckdb_v2_copy_to_finalize_get_init_data info)
            statistics info _context err = callbackGuard failures err $ do
                note "statistics"
                expectFileData (c_duckdb_v2_copy_to_statistics_get_user_data info) (c_duckdb_v2_copy_to_statistics_get_bind_data info) (c_duckdb_v2_copy_to_statistics_get_init_data info)
        bracket (DuckDBV2CopyToBindCallbackFn <$> toFunPtr (DuckDBV2CopyToBindCallbackFn_Aux bind)) (freeHaskellFunPtr . getField @"unwrap") $ \bindPtr ->
            bracket (DuckDBV2CopyToBatchSizeCallbackFn <$> toFunPtr (DuckDBV2CopyToBatchSizeCallbackFn_Aux batchSize)) (freeHaskellFunPtr . getField @"unwrap") $ \batchSizePtr ->
                bracket (DuckDBV2CopyToInitCallbackFn <$> toFunPtr (DuckDBV2CopyToInitCallbackFn_Aux initialize)) (freeHaskellFunPtr . getField @"unwrap") $ \initializePtr ->
                    bracket (DuckDBV2CopyToBatchCallbackFn <$> toFunPtr (DuckDBV2CopyToBatchCallbackFn_Aux batch)) (freeHaskellFunPtr . getField @"unwrap") $ \batchPtr ->
                        bracket (DuckDBV2CopyToFlushCallbackFn <$> toFunPtr (DuckDBV2CopyToFlushCallbackFn_Aux flush)) (freeHaskellFunPtr . getField @"unwrap") $ \flushPtr ->
                            bracket (DuckDBV2CopyToFinalizeCallbackFn <$> toFunPtr (DuckDBV2CopyToFinalizeCallbackFn_Aux finalize)) (freeHaskellFunPtr . getField @"unwrap") $ \finalizePtr ->
                                bracket (DuckDBV2CopyToStatisticsCallbackFn <$> toFunPtr (DuckDBV2CopyToStatisticsCallbackFn_Aux statistics)) (freeHaskellFunPtr . getField @"unwrap") $ \statisticsPtr ->
                                    withConnection $ \conn -> withFactory conn $ \factory -> do
                                        withHandle (c_duckdb_v2_copy_function_create factory) c_duckdb_v2_copy_function_destroy $ \function -> do
                                            withView "v2_writer" $ \name -> checked (c_duckdb_v2_copy_function_set_name function (coerce name))
                                            -- The second extension replaces the first, and an extension with a leading dot is rejected.
                                            withView "first" $ \extension -> checked (c_duckdb_v2_copy_function_set_file_extension function extension)
                                            withView "v2out" $ \extension -> checked (c_duckdb_v2_copy_function_set_file_extension function extension)
                                            withView ".dotted" $ \extension -> expectFailure "an extension with a leading dot" (c_duckdb_v2_copy_function_set_file_extension function extension)
                                            with (borrowedOpaque user) $ \opaque -> checked (c_duckdb_v2_copy_function_set_user_data function opaque)
                                            checked (c_duckdb_v2_copy_to_set_bind_callback function bindPtr)
                                            checked (c_duckdb_v2_copy_to_set_batch_size_callback function batchSizePtr)
                                            checked (c_duckdb_v2_copy_to_set_init_callback function initializePtr)
                                            checked (c_duckdb_v2_copy_to_set_batch_callback function batchPtr)
                                            checked (c_duckdb_v2_copy_to_set_flush_callback function flushPtr)
                                            checked (c_duckdb_v2_copy_to_set_finalize_callback function finalizePtr)
                                            checked (c_duckdb_v2_copy_to_set_statistics_callback function statisticsPtr)
                                            checked (c_duckdb_v2_connection_register_copy_function conn function)
                                        -- One thread writes, so each statement writes one file, and the order of the callbacks is fixed.
                                        withResult conn "SET threads = 1" consume
                                        let source = "COPY (SELECT i::BIGINT AS amount, i::INTEGER AS part FROM range(5000) t(i)) TO '"
                                            options = "' (FORMAT v2_writer, tag 'x', pair (1, 'two'), flag, "
                                            -- Run a statement and give the callbacks that it invoked, in order.
                                            run sql = do
                                                writeIORef invoked []
                                                withResult conn sql consume
                                                reverse <$> readIORef invoked
                                        -- RETURN_STATS invokes the statistics callback after the finalize callback.
                                        run (source <> path <> options <> "USE_TMP_FILE FALSE, RETURN_STATS)")
                                            >>= (@?= ["bind", "batch size", "init", "batch", "flush", "finalize", "statistics"])
                                        -- DuckDB names the files of PER_THREAD_OUTPUT itself, with the extension of the function.
                                        -- The statement does not ask for statistics.
                                        writeIORef filePaths []
                                        flip finally (withCString directory (void . removeTestFile)) $
                                            run (source <> directory <> options <> "PER_THREAD_OUTPUT)")
                                                >>= (@?= ["bind", "batch size", "init", "batch", "flush", "finalize"])
                                        readIORef filePaths >>= \paths ->
                                            assertBool ("each file name has the extension of the function: " <> show paths) (not (null paths) && all (".v2out" `isSuffixOf`) paths)
    readIORef failures >>= (@?= [])
    reverse <$> readIORef boundPaths >>= (@?= [path, directory])
    readIORef batchRows >>= (@?= [5000, 5000])
