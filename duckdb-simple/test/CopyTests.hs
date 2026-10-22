{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Native integration checks for managed COPY and multi-file callbacks.
module CopyTests (copyTests) where

import Control.Exception (bracket, try)
import Control.Monad (forM_, void)
import Data.Array (listArray)
import Data.IORef
import Data.Int (Int64)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple hiding (bind)
import qualified Database.DuckDB.Simple.Copy as Copy
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..))
import qualified Database.DuckDB.Simple.Registration as R
import Database.DuckDB.Simple.Variant (variantObject)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (pokeElemOff)
import System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Exercise the available COPY directions and multi-file wrapper.
copyTests :: TestTree
copyTests =
    testGroup
        "managed COPY"
        [ testCase "COPY TO scans batches and reports statistics" testCopyTo
        , testCase "row callbacks receive per-partition file paths" testPartitionedCopy
        , testCase "row callbacks decode VARIANT after the registering connection closes" testVariantCopy
        , testCase "COPY FROM runs bind global local and exec callbacks" testCopyFrom
        , testCase "callback exceptions become SQL errors and permit reuse" testCopyFailure
        , testCase "multi-file wrappers support lists globs filename and hive columns" testMultiFile
        ]

-- | Run every COPY TO callback and check the reported file statistics.
testCopyTo :: Assertion
testCopyTo = withDirectory \directory -> withConnection ":memory:" \connection -> do
    seen <- newIORef ([] :: [Text])
    total <- newIORef (0 :: DuckDBIdx)
    let mark phase = atomicModifyIORef' seen (\phases -> (phase : phases, ()))
        bind info _ = do
            mark "bind"
            R.output (c_duckdb_v2_copy_to_bind_get_column_count info) >>= (@?= 1)
            R.setCallbackState (c_duckdb_v2_copy_to_bind_set_bind_data info) ()
        batchSize info _ = mark "batch size" >> R.checked (c_duckdb_v2_copy_to_batch_size_set_target info 1024)
        initialize info _ = do
            mark "init"
            path <- R.output (c_duckdb_v2_copy_to_init_get_file_path info) >>= R.readText
            writeFile (Text.unpack path) ""
            R.setCallbackState (c_duckdb_v2_copy_to_init_set_init_data info) total
        batch info _ = do
            mark "batch"
            count <- bracket (R.output (c_duckdb_v2_copy_to_batch_take_input info)) (R.destroy c_duckdb_v2_column_data_collection_destroy) (R.output . c_duckdb_v2_column_data_collection_row_count)
            R.setCallbackState (c_duckdb_v2_copy_to_batch_set_batch_data info) count
        flush info _ = do
            mark "flush"
            count <- R.getCallbackState (c_duckdb_v2_copy_to_flush_get_batch_data info) :: IO DuckDBIdx
            state <- R.getCallbackState (c_duckdb_v2_copy_to_flush_get_init_data info) :: IO (IORef DuckDBIdx)
            atomicModifyIORef' state (\n -> (n + count, ()))
        finalize _ _ = mark "finalize"
        statistics info _ = do
            mark "statistics"
            state <- R.getCallbackState (c_duckdb_v2_copy_to_statistics_get_init_data info) :: IO (IORef DuckDBIdx)
            count <- readIORef state
            R.checked (c_duckdb_v2_copy_to_statistics_set_row_count info count)
            R.checked (c_duckdb_v2_copy_to_statistics_set_file_size info 17)
        callbacks =
            (Copy.copyToCallbacks batch flush)
                { Copy.copyToBindCallback = Just bind
                , Copy.copyToBatchSizeCallback = Just batchSize
                , Copy.copyToInitCallback = Just initialize
                , Copy.copyToFinalizeCallback = Just finalize
                , Copy.copyToStatisticsCallback = Just statistics
                }
    Copy.registerCopyToCallbacks connection "managed_output" callbacks
    rows <- query connection "COPY (SELECT i FROM range(5000) t(i)) TO ? (FORMAT managed_output, RETURN_STATS)" (Only (Text.pack (directory </> "stats"))) :: IO [[FieldValue]]
    readIORef total >>= (@?= 5000)
    phases <- readIORef seen
    forM_ ["bind", "batch size", "init", "batch", "flush", "finalize", "statistics"] \phase -> assertBool (Text.unpack phase) (phase `elem` phases)
    case rows of
        [FieldText _ : count : size : _] -> do
            number count @?= 5000
            number size @?= 17
        _ -> assertFailure (show rows)

-- | Preserve the row-oriented API while native COPY creates partition files.
testPartitionedCopy :: Assertion
testPartitionedCopy = withDirectory \directory -> withConnection ":memory:" \connection -> do
    files <- newIORef ([] :: [FilePath])
    values <- newIORef ([] :: [Int64])
    Copy.registerCopyToFunction
        connection
        "managed_partitions"
        (\info -> Copy.copyBindColumnTypes info @?= [DuckDBV2LogicalTypeIdBigInt])
        ( \info -> do
            let path = Copy.copyInitFilePath info
            writeFile path ""
            atomicModifyIORef' files (\paths -> (path : paths, ()))
            pure path
        )
        ( \info rows -> do
            let decoded = [value | [Field{fieldValue = FieldInt64 value}] <- rows]
            atomicModifyIORef' values (\previous -> (decoded <> previous, ()))
            appendFile (Copy.copySinkGlobalState info) (unlines (map show decoded))
        )
        (const (pure ()))
    void (execute connection "COPY (SELECT i, i%2 AS part FROM range(12) t(i)) TO ? (FORMAT managed_partitions, PARTITION_BY(part))" (Only (Text.pack (directory </> "partitioned"))))
    readIORef values >>= (@?= [0 .. 11]) . sort
    paths <- readIORef files
    length paths @?= 2
    assertBool "partition paths" (all (Text.isInfixOf "part=" . Text.pack) paths)

-- | COPY registrations share the instance and copy nested VARIANT payloads.
testVariantCopy :: Assertion
testVariantCopy = withDirectory \directory -> withEnvironment \environment -> withInstance environment [] \instance' -> do
    received <- newIORef ([] :: [FieldValue])
    withConnectionOn instance' \connection ->
        Copy.registerCopyToFunction
            connection
            "managed_variant"
            (\info -> Copy.copyBindColumnTypes info @?= [DuckDBV2LogicalTypeIdVariant])
            (\info -> writeFile (Copy.copyInitFilePath info) "")
            (\_ rows -> atomicModifyIORef' received (\previous -> (previous <> [fieldValue entry | [entry] <- rows], ())))
            (const (pure ()))
    withConnectionOn instance' \connection -> do
        void (execute connection "COPY (SELECT {'n': i, 'values': [i, NULL]}::VARIANT FROM range(3) t(i)) TO ? (FORMAT managed_variant)" (Only (Text.pack (directory </> "variant"))))
        readIORef received >>= (@?= [variantObject [("n", FieldInt64 i), ("values", FieldList [FieldInt64 i, FieldNull])] | i <- [0 .. 2]])

-- | Fill one output vector from state created in each native COPY FROM phase.
testCopyFrom :: Assertion
testCopyFrom = withConnection ":memory:" \connection -> do
    phases <- newIORef ([] :: [Text])
    let mark phase = atomicModifyIORef' phases (\previous -> (phase : previous, ()))
        bind info _ = do
            mark "bind"
            R.output (c_duckdb_v2_copy_from_bind_get_column_count info) >>= (@?= 1)
            R.setCallbackState (c_duckdb_v2_copy_from_bind_set_bind_data info) (7 :: Int64)
            R.checked (c_duckdb_v2_copy_from_bind_set_cardinality info 3 1)
        initializeGlobal info _ = do
            mark "global"
            start <- R.getCallbackState (c_duckdb_v2_copy_from_init_global_get_bind_data info)
            state <- newIORef (Just (start :: Int64))
            R.setCallbackState (c_duckdb_v2_copy_from_init_global_set_global_state info) state
            R.checked (c_duckdb_v2_copy_from_init_global_set_max_threads info 1)
        initializeLocal info _ = do
            mark "local"
            R.setCallbackState (c_duckdb_v2_copy_from_init_local_set_local_state info) ()
        executeRows info _ = do
            mark "exec"
            () <- R.getCallbackState (c_duckdb_v2_copy_from_exec_get_local_state info)
            state <- R.getCallbackState (c_duckdb_v2_copy_from_exec_get_global_state info) :: IO (IORef (Maybe Int64))
            start <- atomicModifyIORef' state (\value -> (Nothing, value))
            chunk <- R.output (c_duckdb_v2_copy_from_exec_get_output_chunk info)
            vector <- R.output (c_duckdb_v2_data_chunk_get_vector chunk 0)
            writeIntegers vector (maybe [] (\first -> [first .. first + 2]) start)
        -- DuckDB calls this only to display progress, so the test checks
        -- registration and does not require a call.
        progress info _ = R.checked (c_duckdb_v2_copy_from_progress_set_progress info 0.5)
        callbacks =
            (Copy.copyFromCallbacks bind executeRows)
                { Copy.copyFromInitGlobalCallback = Just initializeGlobal
                , Copy.copyFromInitLocalCallback = Just initializeLocal
                , Copy.copyFromProgressCallback = Just progress
                }
    Copy.registerCopyFromFunction connection "managed_input" callbacks
    void (execute_ connection "CREATE TABLE imported (value BIGINT)")
    void (execute_ connection "COPY imported FROM 'unused' (FORMAT managed_input)")
    query_ connection "SELECT value FROM imported ORDER BY value" >>= (@?= map Only ([7, 8, 9] :: [Int64]))
    called <- readIORef phases
    forM_ ["bind", "global", "local", "exec"] \phase -> assertBool (Text.unpack phase) (phase `elem` called)

-- | Contain exceptions from native COPY callbacks.
testCopyFailure :: Assertion
testCopyFailure = withConnection ":memory:" \connection -> do
    Copy.registerCopyFromFunction connection "managed_failure" (Copy.copyFromCallbacks (\_ _ -> pure ()) (\_ _ -> ioError (userError "managed COPY failure")))
    void (execute_ connection "CREATE TABLE failed_import (value BIGINT)")
    result <- try (execute_ connection "COPY failed_import FROM 'unused' (FORMAT managed_failure)")
    case result of
        Left SQLError{sqlErrorMessage} -> assertBool "callback diagnostic" ("managed COPY failure" `Text.isInfixOf` sqlErrorMessage)
        Right _ -> assertFailure "expected callback failure"
    query_ connection "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Wrap a managed one-file table reader with native file discovery.
testMultiFile :: Assertion
testMultiFile = withDirectory \directory -> withConnection ":memory:" \connection -> do
    let first = directory </> "first.mock"
        second = directory </> "second.mock"
    writeFile first "7"
    writeFile second "11"
    bindInfoCalls <- registerSingleFileReader connection
    Copy.registerMultiFileFunction connection "managed_files" "managed_single" (Copy.MultiFileOptions (Just "managed fixture") (Just "mock"))
    query connection "SELECT value FROM managed_files(?) ORDER BY value" (Only (Text.pack (directory </> "*.mock"))) >>= (@?= map Only ([7, 11] :: [Int64]))
    let paths = listArray (0 :: Int, 1) [Text.pack first, Text.pack second]
    query connection "SELECT value, filename FROM managed_files(?::VARCHAR[], filename=true) ORDER BY value" (Only paths) >>= (@?= [(7 :: Int64, Text.pack first), (11, Text.pack second)])
    forM_ [("left", 3), ("right", 5 :: Int)] \(part, value) -> do
        let path = directory </> ("part=" <> part)
        createDirectory path
        writeFile (path </> "data.mock") (show value)
    query connection "SELECT value,part FROM managed_files(?,hive_partitioning=true,union_by_name=true) ORDER BY value" (Only (Text.pack (directory </> "part=*" </> "*.mock"))) >>= (@?= [(3 :: Int64, "left" :: Text), (5, "right")])
    readIORef bindInfoCalls >>= assertBool "table get-bind-info callback" . (> 0)

-- | Register a table function that returns the integer stored in one text file.
registerSingleFileReader :: Connection -> IO (IORef Int)
registerSingleFileReader connection = do
    calls <- newIORef 0
    R.registerTableFunction
        connection
        "managed_single"
        [R.parameter "path" (R.TypeExpression "VARCHAR")]
        (R.tableCallbacks bind executeRows){R.tableInitGlobalCallback = Just initialize, R.tableGetBindInfoCallback = Just (getBindInfo calls)}
    pure calls
  where
    bind input result context = do
        path <- bracket (R.output (c_duckdb_v2_function_bind_get_arg_value input 0)) (R.destroy c_duckdb_v2_value_destroy) \value -> R.output (c_duckdb_v2_value_get_varchar value) >>= R.readText
        value <- read <$> readFile (Text.unpack path) :: IO Int64
        R.setCallbackState (c_duckdb_v2_function_bind_set_bind_data input) value
        bracket (R.output (c_duckdb_v2_context_get_factory context) >>= \factory -> R.withText "BIGINT" (R.output . c_duckdb_v2_factory_create_type_from_text factory)) (R.destroy c_duckdb_v2_logical_type_destroy) \dtype ->
            R.withIdentifier "value" \name -> R.checked (c_duckdb_v2_table_function_bind_add_result_column result name dtype)
    getBindInfo calls info context = do
        modifyIORef' calls (+ 1)
        value <- R.getCallbackState (c_duckdb_v2_table_function_get_bind_info_get_bind_data info) :: IO Int64
        factory <- R.output (c_duckdb_v2_context_get_factory context)
        bracket (R.withText "value" (R.output . c_duckdb_v2_value_create_varchar factory)) (R.destroy c_duckdb_v2_value_destroy) $ \identifier ->
            R.checked (c_duckdb_v2_table_function_get_bind_info_set_column_identifier info 0 (ConstPtr nullPtr) 0 identifier)
        bracket (R.output (c_duckdb_v2_value_create_bigint factory value)) (R.destroy c_duckdb_v2_value_destroy) $ \native ->
            R.withText "fixture_value" $ \(ConstPtr key) -> R.checked (c_duckdb_v2_table_function_get_bind_info_set_option info key native)
    initialize info _ = do
        value <- R.getCallbackState (c_duckdb_v2_table_function_init_global_get_bind_data info)
        state <- newIORef (Just (value :: Int64))
        R.setCallbackState (c_duckdb_v2_table_function_init_global_set_global_state info) state
    executeRows info _ = do
        state <- R.getCallbackState (c_duckdb_v2_table_function_exec_get_global_state info) :: IO (IORef (Maybe Int64))
        value <- atomicModifyIORef' state (\previous -> (Nothing, previous))
        chunk <- R.output (c_duckdb_v2_table_function_exec_get_output_chunk info)
        vector <- R.output (c_duckdb_v2_data_chunk_get_vector chunk 0)
        writeIntegers vector (maybe [] pure value)

-- | Fill native BIGINT vector storage and set the row count.
writeIntegers :: DuckDBV2VectorHandle -> [Int64] -> IO ()
writeIntegers vector values = do
    R.checked (c_duckdb_v2_vector_set_size vector (fromIntegral (length values)))
    raw <- R.output (c_duckdb_v2_vector_get_data_mutable vector)
    forM_ (zip [0 ..] values) \(index, value) -> pokeElemOff (castPtr raw) index value

-- | Read a nonnegative integer from native statistics.
number :: FieldValue -> Integer
number (FieldWord64 value) = toInteger value
number (FieldInt64 value) = toInteger value
number value = error ("unexpected statistics value " <> show value)

-- | Create and remove a unique directory for native file operations.
withDirectory :: (FilePath -> IO a) -> IO a
withDirectory action = bracket create removePathForcibly action
  where
    create = do
        root <- getTemporaryDirectory
        (path, handle) <- openTempFile root "duckdb-managed-copy"
        hClose handle
        removeFile path
        createDirectory path
        pure path
