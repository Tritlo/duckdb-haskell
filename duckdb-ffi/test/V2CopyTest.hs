{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module V2CopyTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM, unless, void, when)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
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
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import V2Test (borrowedOpaque, callbackGuard, checked, consume, output, readIntegers, readView, record, removeTestFile, withConnection, withFactory, withHandle, withResult, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 COPY functions"
        [ testCase "COPY TO callbacks read the user data, the bind data, the init data and the statement" testCopyTo
        , testCase "COPY FROM callbacks read the user data, the bind data, the global state and the statement" testCopyFrom
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

-- | Get a path that does not exist yet: the name of a new temporary file, which the function removes.
freshPath :: String -> IO String
freshPath template = do
    (path, handle) <- openBinaryTempFile "." template
    hClose handle
    withCString path $ \name -> removeTestFile name >>= (@?= 0)
    pure path

-- | Read the percentage of the query that runs on a connection.
queryProgress :: DuckDBV2ConnectionHandle -> IO Double
queryProgress conn = alloca $ \percentage -> alloca $ \processed -> alloca $ \total -> do
    checked (c_duckdb_v2_connection_progress_get conn percentage processed total)
    realToFrac <$> peek percentage

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
                -- The engine invokes this callback because the statements do not set BATCH_SIZE.
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

testCopyFrom :: IO ()
testCopyFrom = do
    failures <- newIORef []
    invoked <- newIORef []
    boundPaths <- newIORef []
    rows <- newIORef (0 :: Int64)
    -- The progress callback records how many rows the read has given at each call.
    progressAt <- newIORef []
    -- The exec callback records the progress of the query on the connection, which the test opens later.
    connection <- newIORef Nothing
    percentages <- newIORef []
    -- The user data, the bind data and the global state point to three different cells, so an accessor that
    -- returns another one fails.
    allocaBytes 1 $ \user -> allocaBytes 1 $ \bound -> allocaBytes 1 $ \global -> do
        let note name = record invoked name
            expectData getUser getBind = do
                output getUser >>= (@?= user)
                output getBind >>= (@?= bound)
            bind info _context err = callbackGuard failures err $ do
                note "bind"
                output (c_duckdb_v2_copy_from_bind_get_user_data info) >>= (@?= user)
                output (c_duckdb_v2_copy_from_bind_get_file_path info) >>= readView >>= record boundPaths
                expectColumns (c_duckdb_v2_copy_from_bind_get_column_count info) (c_duckdb_v2_copy_from_bind_get_column_name info) (c_duckdb_v2_copy_from_bind_get_column_type info)
                expectOptions (c_duckdb_v2_copy_from_bind_get_option_count info) (c_duckdb_v2_copy_from_bind_get_option_name info) (c_duckdb_v2_copy_from_bind_get_option_value info)
                with (borrowedOpaque bound) $ \opaque -> checked (c_duckdb_v2_copy_from_bind_set_bind_data info opaque)
            initGlobal info _context err = callbackGuard failures err $ do
                note "init global"
                expectData (c_duckdb_v2_copy_from_init_global_get_user_data info) (c_duckdb_v2_copy_from_init_global_get_bind_data info)
                with (borrowedOpaque global) $ \opaque -> checked (c_duckdb_v2_copy_from_init_global_set_global_state info opaque)
            initLocal info _context err = callbackGuard failures err $ do
                note "init local"
                expectData (c_duckdb_v2_copy_from_init_local_get_user_data info) (c_duckdb_v2_copy_from_init_local_get_bind_data info)
                output (c_duckdb_v2_copy_from_init_local_get_global_state info) >>= (@?= global)
            progress info _context err = callbackGuard failures err $ do
                given <- readIORef rows
                record progressAt given
                expectData (c_duckdb_v2_copy_from_progress_get_user_data info) (c_duckdb_v2_copy_from_progress_get_bind_data info)
                output (c_duckdb_v2_copy_from_progress_get_global_state info) >>= (@?= global)
                -- The callback reports progress only after the read has given a row. Before that, the read has no progress.
                when (given > 0) $ checked (c_duckdb_v2_copy_from_progress_set_progress info 0.5)
            -- The read gives one row for each call until the progress callback runs after the first row. Then the read ends.
            execute info _context err = callbackGuard failures err $ do
                note "exec"
                expectData (c_duckdb_v2_copy_from_exec_get_user_data info) (c_duckdb_v2_copy_from_exec_get_bind_data info)
                readIORef connection >>= mapM_ (\conn -> queryProgress conn >>= record percentages)
                reported <- any (> 0) <$> readIORef progressAt
                given <- readIORef rows
                when (given >= 1000) $ assertFailure "the progress callback did not run during 1000 rows of the read"
                unless reported $ do
                    chunk <- output (c_duckdb_v2_copy_from_exec_get_output_chunk info)
                    amounts <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                    parts <- output (c_duckdb_v2_data_chunk_get_vector chunk 1)
                    output (c_duckdb_v2_vector_get_data_mutable amounts) >>= \raw -> poke (coerce raw) given
                    output (c_duckdb_v2_vector_get_data_mutable parts) >>= \raw -> poke (coerce raw) (fromIntegral given :: Int32)
                    checked (c_duckdb_v2_vector_set_size amounts 1)
                    writeIORef rows (given + 1)
        bracket (DuckDBV2CopyFromBindCallbackFn <$> toFunPtr (DuckDBV2CopyFromBindCallbackFn_Aux bind)) (freeHaskellFunPtr . getField @"unwrap") $ \bindPtr ->
            bracket (DuckDBV2CopyFromInitGlobalCallbackFn <$> toFunPtr (DuckDBV2CopyFromInitGlobalCallbackFn_Aux initGlobal)) (freeHaskellFunPtr . getField @"unwrap") $ \initGlobalPtr ->
                bracket (DuckDBV2CopyFromInitLocalCallbackFn <$> toFunPtr (DuckDBV2CopyFromInitLocalCallbackFn_Aux initLocal)) (freeHaskellFunPtr . getField @"unwrap") $ \initLocalPtr ->
                    bracket (DuckDBV2CopyFromExecCallbackFn <$> toFunPtr (DuckDBV2CopyFromExecCallbackFn_Aux execute)) (freeHaskellFunPtr . getField @"unwrap") $ \executePtr ->
                        bracket (DuckDBV2CopyFromProgressCallbackFn <$> toFunPtr (DuckDBV2CopyFromProgressCallbackFn_Aux progress)) (freeHaskellFunPtr . getField @"unwrap") $ \progressPtr ->
                            withConnection $ \conn -> withFactory conn $ \factory -> do
                                withHandle (c_duckdb_v2_copy_function_create factory) c_duckdb_v2_copy_function_destroy $ \function -> do
                                    withView "v2_reader" $ \name -> checked (c_duckdb_v2_copy_function_set_name function (coerce name))
                                    with (borrowedOpaque user) $ \opaque -> checked (c_duckdb_v2_copy_function_set_user_data function opaque)
                                    checked (c_duckdb_v2_copy_from_set_bind_callback function bindPtr)
                                    checked (c_duckdb_v2_copy_from_set_init_global_callback function initGlobalPtr)
                                    checked (c_duckdb_v2_copy_from_set_init_local_callback function initLocalPtr)
                                    checked (c_duckdb_v2_copy_from_set_exec_callback function executePtr)
                                    checked (c_duckdb_v2_copy_from_set_progress_callback function progressPtr)
                                    checked (c_duckdb_v2_connection_register_copy_function conn function)
                                writeIORef connection (Just conn)
                                withResult conn "CREATE TABLE v2_target (amount BIGINT, part INTEGER)" consume
                                -- The engine asks for progress only with the progress bar on. The thread that steps the result asks
                                -- after each partial execution of a task. With one thread, that thread also runs the read, so it
                                -- asks while the read runs.
                                withResult conn "SET threads = 1" consume
                                withResult conn "SET enable_progress_bar = true" consume
                                withResult conn "SET enable_progress_bar_print = false" consume
                                -- The engine does not expand the glob and does not look for the file.
                                withResult conn "COPY v2_target FROM 'v2-absent/*.probe' (FORMAT v2_reader, tag 'x', pair (1, 'two'), flag)" $ \result -> do
                                    inserted <- readIntegers result
                                    readIORef rows >>= \given -> inserted @?= [Just given]
    readIORef failures >>= (@?= [])
    readIORef boundPaths >>= (@?= ["v2-absent/*.probe"])
    given <- readIORef rows
    -- One thread reads, so the exec callback runs once for each row and once more for the empty batch at the end.
    reverse <$> readIORef invoked >>= (@?= ["bind", "init global", "init local"] <> replicate (fromIntegral given + 1) "exec")
    readIORef progressAt >>= \seen -> assertBool ("the progress callback ran after the read gave rows: " <> show seen) (any (> 0) seen)
    -- The query shows no progress before the read reports progress, and shows progress after the read reports 0.5.
    reverse <$> readIORef percentages >>= \seen ->
        assertBool ("the query progress follows the reported progress: " <> show seen) (not (null seen) && all (<= 0) (take 1 seen) && all (> 0) (take 1 (reverse seen)))
