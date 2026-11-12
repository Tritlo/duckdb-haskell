module V2TableFunctionTest (deliberateFailure, statementError, tests) where

import Control.Exception (bracket, finally, onException, throwIO)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Coerce (coerce)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (elemIndex, isInfixOf, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca, calloc, finalizerFree, free)
import Foreign.Marshal.Array (mallocArray, pokeArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable, peek, peekElemOff, poke, pokeElemOff)
import HsBindgen.Runtime.Prelude (ToFunPtr, toFunPtr)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import V2Test (borrowedOpaque, callbackGuard, checked, consume, output, readIntegers, readView, record, removeTestFile, withConnection, withFactory, withHandle, withResult, withStatement, withType, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 table function callbacks"
        [ testCase "init and exec callbacks read the data of their call and the declared column of each scanned column" testColumns
        , testCase "claim batch callbacks hand out batches until a claim fails" testClaimBatch
        , testCase "order preservation and batch indexes decide whether a scan runs in parallel" testParallelScan
        , testCase "partitioning callbacks let a grouped aggregate take its groups from partition values" testPartitioning
        , testCase "progress callbacks give the progress of the running query" testProgress
        , testCase "a multi-file function asks the function it wraps for the bind info of each file" testBindInfo
        , testCase "filter pushdown callbacks read the predicates of the query, and an accepted predicate leaves the query" testFilterPushdown
        , testCase "a callback that reports an error fails the statement, and DuckDB destroys the states set before" testFailingCallbacks
        , testCase "registration refuses an incomplete table or multi-file function and a function of another database" testRegistration
        ]

{- | Run a statement that must fail, and return the code and the text of its error.
The execution runs the query lazily, so the error can come from the execution or from a later fetch.
-}
statementError :: DuckDBV2ConnectionHandle -> String -> IO (DuckDBV2Error, String)
statementError conn sql = withStatement conn sql $ \statement -> alloca $ \result -> alloca $ \err -> do
    poke result (coerce nullPtr)
    poke err (coerce nullPtr)
    let fetch = alloca $ \chunk -> do
            poke chunk (coerce nullPtr)
            live <- peek result
            code <- c_duckdb_v2_result_fetch_chunk live chunk err
            next <- peek chunk
            void (c_duckdb_v2_data_chunk_destroy chunk)
            if code == DuckDBV2ErrorNone && next /= coerce nullPtr then fetch else pure code
    flip finally (c_duckdb_v2_result_destroy result >> c_duckdb_v2_error_info_destroy err) $ do
        executed <- c_duckdb_v2_statement_execute conn statement (coerce nullPtr) (coerce nullPtr) 0 result err
        code <- if executed == DuckDBV2ErrorNone then fetch else pure executed
        when (code == DuckDBV2ErrorNone) $ assertFailure ("the statement succeeds: " <> sql)
        info <- peek err
        text <- alloca $ \view -> do
            c_duckdb_v2_error_info_get_text info view >>= (@?= DuckDBV2ErrorNone)
            peek view >>= readView
        pure (code, text)

{- | Throw the deliberate failure of a callback. The callback guard records it as the text "user error (TEXT)", and
gives the same text to DuckDB.
-}
deliberateFailure :: String -> IO ()
deliberateFailure text = throwIO (userError text)

-- | Create a function pointer for the duration of the action.
withFunPtr :: (ToFunPtr a) => a -> (FunPtr a -> IO b) -> IO b
withFunPtr callback = bracket (toFunPtr callback) freeHaskellFunPtr

-- | Create a table function, configure it, and register it on the connection.
registerTable :: DuckDBV2ConnectionHandle -> String -> (DuckDBV2TableFunctionHandle -> IO ()) -> IO ()
registerTable conn name configure = withFactory conn $ \factory ->
    withHandle (c_duckdb_v2_table_function_create factory) c_duckdb_v2_table_function_destroy $ \function -> do
        withView name $ \view -> checked (c_duckdb_v2_table_function_set_name function (coerce view))
        configure function
        checked (c_duckdb_v2_connection_register_table_function conn function)

-- | Declare BIGINT result columns with the given names.
addColumns :: DuckDBV2TableFunctionBindInfoHandle -> DuckDBV2ContextHandle -> [String] -> IO ()
addColumns result context names = do
    factory <- output (c_duckdb_v2_context_get_factory context)
    withType factory "BIGINT" $ \ty -> forM_ names $ \name ->
        withView name $ \view -> checked (c_duckdb_v2_table_function_bind_add_result_column result (coerce view) ty)

-- | Pass a pointer as an opaque handle without a destructor. The caller keeps the memory.
lend :: Ptr a -> (Ptr DuckDBV2Opaque -> IO b) -> IO b
lend cells = with (borrowedOpaque (castPtr cells))

{- | Allocate cells with the given values and give them to DuckDB through the setter.
The opaque handle frees the cells with the C @free@ when DuckDB destroys it.
-}
give :: [Int64] -> (Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO (Ptr Void)
give values setter = do
    cells <- mallocArray (length values)
    pokeArray cells values
    let opaque = DuckDBV2Opaque (castPtr cells) (DuckDBV2OpaqueDestroyFn (castFunPtr finalizerFree)) (DuckDBV2OpaqueEqualsFn nullFunPtr)
    with opaque (checked . setter) `onException` free cells
    pure (castPtr cells)

-- | Assert that a callback reads a value that an earlier callback recorded.
expectRecorded :: (Eq a) => String -> a -> IORef [a] -> IO ()
expectRecorded what value ref = readIORef ref >>= assertBool what . elem value

-- | Write the values into the vector at the given position of the chunk.
writeColumn :: DuckDBV2DataChunkHandle -> DuckDBIdx -> [Int64] -> IO ()
writeColumn chunk position values = do
    vector <- output (c_duckdb_v2_data_chunk_get_vector chunk position)
    raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
    forM_ (zip [0 ..] values) $ \(row, value) -> pokeElemOff (coerce raw) row value

-- | Declare the number of rows of the chunk on its first vector.
setRows :: DuckDBV2DataChunkHandle -> Int -> IO ()
setRows chunk count = do
    first <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
    checked (c_duckdb_v2_vector_set_size first (fromIntegral count))

{- | Read the declared column for each index below the count that the count getter gives.
The index getter must reject the index that equals the count.
-}
declaredColumns :: String -> (Ptr DuckDBIdx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> (DuckDBIdx -> Ptr DuckDBIdx -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO [DuckDBIdx]
declaredColumns phase getCount getIndex = do
    count <- output getCount
    indexes <- forM (take (fromIntegral count) [0 ..]) (output . getIndex)
    code <- alloca $ \slot -> getIndex count slot nullPtr
    assertBool (phase <> " rejects the index that equals the column count") (code /= DuckDBV2ErrorNone)
    pure indexes

{- | Two functions declare the columns a, b and c, one with projection pushdown and one without.
The exec callback writes 100 * (declared column) + (row) into each scanned column.
-}
testColumns :: IO ()
testColumns = do
    failures <- newIORef []
    binds <- newIORef [] -- (user data, bind data) of each bound call
    globals <- newIORef [] -- (bind data, global state) of each scan
    locals <- newIORef [] -- (global state, local state) of each thread
    columns <- newIORef [] -- (phase, user data, declared column of each scanned column)
    let bind info result context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_function_bind_get_user_data info)
            -- A result column carries data, so the type ANY is rejected. The next call adds the column as a BIGINT.
            factory <- output (c_duckdb_v2_context_get_factory context)
            withHandle (c_duckdb_v2_factory_create_type_from_id factory DuckDBV2LogicalTypeIdAny (coerce nullPtr) (coerce nullPtr) 0) c_duckdb_v2_logical_type_destroy $ \ty ->
                withView "a" $ \name ->
                    c_duckdb_v2_table_function_bind_add_result_column result (coerce name) ty nullPtr
                        >>= assertBool "a result column of type ANY is rejected" . (/= DuckDBV2ErrorNone)
            addColumns result context ["a", "b", "c"]
            bound <- give [0] (c_duckdb_v2_function_bind_set_bind_data info)
            record binds (user, bound)
        initGlobal info _context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_table_function_init_global_get_user_data info)
            bound <- output (c_duckdb_v2_table_function_init_global_get_bind_data info)
            expectRecorded "global init reads the user data and the bind data of its call" (user, bound) binds
            indexes <- declaredColumns "global init" (c_duckdb_v2_table_function_init_global_get_column_count info) (c_duckdb_v2_table_function_init_global_get_column_index info)
            record columns ("global init", user, indexes)
            global <- give [0] (c_duckdb_v2_table_function_init_global_set_global_state info)
            record globals (bound, global)
        initLocal info _context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_table_function_init_local_get_user_data info)
            bound <- output (c_duckdb_v2_table_function_init_local_get_bind_data info)
            global <- output (c_duckdb_v2_table_function_init_local_get_global_state info)
            expectRecorded "local init reads the user data and the bind data of its call" (user, bound) binds
            expectRecorded "local init reads the global state of its scan" (bound, global) globals
            indexes <- declaredColumns "local init" (c_duckdb_v2_table_function_init_local_get_column_count info) (c_duckdb_v2_table_function_init_local_get_column_index info)
            record columns ("local init", user, indexes)
            local <- give [0] (c_duckdb_v2_table_function_init_local_set_local_state info)
            record locals (global, local)
        execute info _context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_table_function_exec_get_user_data info)
            bound <- output (c_duckdb_v2_table_function_exec_get_bind_data info)
            global <- output (c_duckdb_v2_table_function_exec_get_global_state info)
            local <- output (c_duckdb_v2_table_function_exec_get_local_state info)
            expectRecorded "exec reads the user data and the bind data of its call" (user, bound) binds
            expectRecorded "exec reads the global state of its scan" (bound, global) globals
            expectRecorded "exec reads the local state of its thread" (global, local) locals
            indexes <- declaredColumns "exec" (c_duckdb_v2_table_function_exec_get_column_count info) (c_duckdb_v2_table_function_exec_get_column_index info)
            record columns ("exec", user, indexes)
            chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
            emitted <- peek (castPtr local :: Ptr Int64)
            if emitted == 0
                then do
                    forM_ (zip [0 ..] indexes) $ \(position, declared) ->
                        writeColumn chunk position [100 * fromIntegral declared + row | row <- [0 .. 2]]
                    setRows chunk 3
                    poke (castPtr local) (1 :: Int64)
                else setRows chunk 0
    with (1 :: Int64) $ \projectedUser -> with (2 :: Int64) $ \unprojectedUser ->
        withFunPtr (DuckDBV2TableFunctionBindCallbackFn_Aux bind) $ \bindPtr ->
            withFunPtr (DuckDBV2TableFunctionInitGlobalCallbackFn_Aux initGlobal) $ \initGlobalPtr ->
                withFunPtr (DuckDBV2TableFunctionInitLocalCallbackFn_Aux initLocal) $ \initLocalPtr ->
                    withFunPtr (DuckDBV2TableFunctionExecCallbackFn_Aux execute) $ \executePtr -> withConnection $ \conn -> do
                        let register name user pushdown document = registerTable conn name $ \function -> do
                                lend user (checked . c_duckdb_v2_table_function_set_user_data function)
                                checked (c_duckdb_v2_table_function_set_bind_callback function (DuckDBV2TableFunctionBindCallbackFn bindPtr))
                                checked (c_duckdb_v2_table_function_set_init_global_callback function (DuckDBV2TableFunctionInitGlobalCallbackFn initGlobalPtr))
                                checked (c_duckdb_v2_table_function_set_init_local_callback function (DuckDBV2TableFunctionInitLocalCallbackFn initLocalPtr))
                                checked (c_duckdb_v2_table_function_set_exec_callback function (DuckDBV2TableFunctionExecCallbackFn executePtr))
                                checked (c_duckdb_v2_table_function_set_projection_pushdown function (CBool pushdown))
                                output (c_duckdb_v2_table_function_get_docs function) >>= document
                            describe docs = do
                                withView "Returns three columns." $ \description -> checked (c_duckdb_v2_function_docs_set_description docs description)
                                forM_ ["SELECT a FROM v2_projected()", "SELECT c FROM v2_projected()"] $ \example ->
                                    withView "example" $ \title -> withView example $ \text -> checked (c_duckdb_v2_function_docs_add_example docs title text)
                                withView "table" $ \category -> checked (c_duckdb_v2_function_docs_add_category docs category)
                        register "v2_projected" projectedUser 1 describe
                        register "v2_unprojected" unprojectedUser 0 (\_ -> pure ())
                        withResult conn "SELECT c - a FROM v2_projected()" $ \result -> readIntegers result >>= (@?= replicate 3 (Just 200))
                        withResult conn "SELECT c - a FROM v2_unprojected()" $ \result -> readIntegers result >>= (@?= replicate 3 (Just 200))
                        let documented =
                                "SELECT count(*)::BIGINT FROM duckdb_functions() WHERE function_name = 'v2_projected' AND description = 'Returns three columns.'"
                                    <> " AND examples = ['SELECT a FROM v2_projected()', 'SELECT c FROM v2_projected()'] AND categories = ['table']"
                        withResult conn documented $ \result -> readIntegers result >>= (@?= [Just 1])
                        readIORef failures >>= (@?= [])
                        recorded <- readIORef columns
                        let phasesOf user = [(phase, indexes) | (phase, owner, indexes) <- recorded, owner == castPtr user]
                        forM_ [projectedUser, unprojectedUser] $ \user ->
                            nub (sort (map fst (phasesOf user))) @?= ["exec", "global init", "local init"]
                        -- The engine chooses the order of the scanned columns. All phases of one scan must report the same order.
                        let projected = nub (map snd (phasesOf projectedUser))
                        length projected @?= 1
                        map sort projected @?= [[0, 2]]
                        nub (map snd (phasesOf unprojectedUser)) @?= [[0, 1, 2]]

{- | One thread scans three batches of two rows each. The log records each claim and each exec call.
The header describes the sequence for a thread: a claim before the first exec, a claim after each empty chunk,
and the end of the scan after a failed claim.
-}
testClaimBatch :: IO ()
testClaimBatch = do
    failures <- newIORef []
    events <- newIORef []
    binds <- newIORef [] -- (user data, bind data) of each bound call
    globals <- newIORef [] -- (bind data, global state) of each scan
    locals <- newIORef [] -- (global state, local state) of each thread
    let bind info result context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_function_bind_get_user_data info)
            addColumns result context ["value"]
            bound <- give [0] (c_duckdb_v2_function_bind_set_bind_data info)
            record binds (user, bound)
        -- The global state holds the number of the next batch. Only one thread claims batches, so it needs no lock.
        initGlobal info _context err = callbackGuard failures err $ do
            bound <- output (c_duckdb_v2_table_function_init_global_get_bind_data info)
            global <- give [0] (c_duckdb_v2_table_function_init_global_set_global_state info)
            record globals (bound, global)
        -- The local state holds the claimed batch, or -1, and whether exec wrote the rows of the batch.
        initLocal info _context err = callbackGuard failures err $ do
            global <- output (c_duckdb_v2_table_function_init_local_get_global_state info)
            local <- give [-1, 0] (c_duckdb_v2_table_function_init_local_set_local_state info)
            record locals (global, local)
        claim info _context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_table_function_claim_batch_get_user_data info)
            bound <- output (c_duckdb_v2_table_function_claim_batch_get_bind_data info)
            global <- output (c_duckdb_v2_table_function_claim_batch_get_global_state info)
            local <- output (c_duckdb_v2_table_function_claim_batch_get_local_state info)
            expectRecorded "claim batch reads the user data and the bind data of its call" (user, bound) binds
            expectRecorded "claim batch reads the global state of its scan" (bound, global) globals
            expectRecorded "claim batch reads the local state of its thread" (global, local) locals
            next <- peek (castPtr global :: Ptr Int64)
            if next < 3
                then do
                    poke (castPtr global) (next + 1)
                    pokeArray (castPtr local) [next, 0 :: Int64]
                    checked (c_duckdb_v2_table_function_claim_batch_set_claimed info (CBool 1))
                    record events ("claim " <> show next)
                else do
                    -- A callback that does not call set_claimed claims nothing.
                    pokeArray (castPtr local) [-1, 0 :: Int64]
                    record events "claim none"
        execute info _context err = callbackGuard failures err $ do
            local <- castPtr <$> output (c_duckdb_v2_table_function_exec_get_local_state info)
            batch <- peekElemOff local 0
            written <- peekElemOff local 1
            chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
            if batch >= 0 && written == 0
                then do
                    writeColumn chunk 0 [10 * batch, 10 * batch + 1 :: Int64]
                    setRows chunk 2
                    pokeElemOff local 1 (1 :: Int64)
                    record events ("rows " <> show batch)
                else do
                    setRows chunk 0
                    record events ("empty " <> show batch)
    with (7 :: Int64) $ \user ->
        withFunPtr (DuckDBV2TableFunctionBindCallbackFn_Aux bind) $ \bindPtr ->
            withFunPtr (DuckDBV2TableFunctionInitGlobalCallbackFn_Aux initGlobal) $ \initGlobalPtr ->
                withFunPtr (DuckDBV2TableFunctionInitLocalCallbackFn_Aux initLocal) $ \initLocalPtr ->
                    withFunPtr (DuckDBV2TableFunctionClaimBatchCallbackFn_Aux claim) $ \claimPtr ->
                        withFunPtr (DuckDBV2TableFunctionExecCallbackFn_Aux execute) $ \executePtr -> withConnection $ \conn -> do
                            registerTable conn "v2_batches" $ \function -> do
                                lend user (checked . c_duckdb_v2_table_function_set_user_data function)
                                checked (c_duckdb_v2_table_function_set_bind_callback function (DuckDBV2TableFunctionBindCallbackFn bindPtr))
                                checked (c_duckdb_v2_table_function_set_init_global_callback function (DuckDBV2TableFunctionInitGlobalCallbackFn initGlobalPtr))
                                checked (c_duckdb_v2_table_function_set_init_local_callback function (DuckDBV2TableFunctionInitLocalCallbackFn initLocalPtr))
                                checked (c_duckdb_v2_table_function_set_claim_batch_callback function (DuckDBV2TableFunctionClaimBatchCallbackFn claimPtr))
                                checked (c_duckdb_v2_table_function_set_exec_callback function (DuckDBV2TableFunctionExecCallbackFn executePtr))
                            withResult conn "SELECT value FROM v2_batches()" $ \result ->
                                readIntegers result >>= (@?= map Just [0, 1, 10, 11, 20, 21])
    readIORef failures >>= (@?= [])
    -- The default maximum of one thread gives one local state.
    length <$> readIORef locals >>= (@?= 1)
    reverse <$> readIORef events
        >>= (@?= ["claim 0", "rows 0", "empty 0", "claim 1", "rows 1", "empty 1", "claim 2", "rows 2", "empty 2", "claim none"])

{- | Each exec call takes the next of eight batches of three rows from a shared counter, with at most two threads.
The functions take the order preservation as their argument: 0 is NO_ORDER and 1 is INSERTION_ORDER.
The test instance has two threads, so the engine starts two scan tasks when the scan can run in parallel.
-}
testParallelScan :: IO ()
testParallelScan = do
    failures <- newIORef []
    next <- newIORef (0 :: Int64)
    binds <- newIORef [] -- (user data, bind data) of each bound call
    globals <- newIORef [] -- (bind data, global state) of each scan
    locals <- newIORef [] -- (global state, local state) of each thread
    requests <- newIORef [] -- (batch index, partition columns, partition column count) that each partition data call requires
    let bind info result context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_function_bind_get_user_data info)
            order <- withHandle (c_duckdb_v2_function_bind_get_arg_value info 0) c_duckdb_v2_value_destroy (output . c_duckdb_v2_value_get_bigint)
            c_duckdb_v2_table_function_bind_set_order_preservation result (DuckDBV2OrderPreservation 99) nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
            checked (c_duckdb_v2_table_function_bind_set_order_preservation result (DuckDBV2OrderPreservation (fromIntegral order)))
            addColumns result context ["value"]
            bound <- give [0] (c_duckdb_v2_function_bind_set_bind_data info)
            record binds (user, bound)
        initGlobal info _context err = callbackGuard failures err $ do
            bound <- output (c_duckdb_v2_table_function_init_global_get_bind_data info)
            global <- give [0] (c_duckdb_v2_table_function_init_global_set_global_state info)
            record globals (bound, global)
            checked (c_duckdb_v2_table_function_init_global_set_max_threads info 2)
        -- The local state holds the batch of the last chunk that the thread wrote.
        initLocal info _context err = callbackGuard failures err $ do
            global <- output (c_duckdb_v2_table_function_init_local_get_global_state info)
            local <- give [-1] (c_duckdb_v2_table_function_init_local_set_local_state info)
            record locals (global, local)
        execute info _context err = callbackGuard failures err $ do
            local <- output (c_duckdb_v2_table_function_exec_get_local_state info)
            batch <- atomicModifyIORef' next $ \value -> (value + 1, value)
            chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
            if batch < 8
                then do
                    writeColumn chunk 0 [10 * batch + row | row <- [0 .. 2]]
                    setRows chunk 3
                    poke (castPtr local) batch
                else setRows chunk 0
        partitionData info _context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_table_function_partition_data_get_user_data info)
            bound <- output (c_duckdb_v2_table_function_partition_data_get_bind_data info)
            global <- output (c_duckdb_v2_table_function_partition_data_get_global_state info)
            local <- output (c_duckdb_v2_table_function_partition_data_get_local_state info)
            expectRecorded "partition data reads the user data and the bind data of its call" (user, bound) binds
            expectRecorded "partition data reads the global state of its scan" (bound, global) globals
            expectRecorded "partition data reads the local state of the thread that wrote the chunk" (global, local) locals
            batchIndex <- output (c_duckdb_v2_table_function_partition_data_requires_batch_index info)
            partitionColumns <- output (c_duckdb_v2_table_function_partition_data_requires_partition_columns info)
            count <- output (c_duckdb_v2_table_function_partition_data_get_partition_column_count info)
            record requests (batchIndex, partitionColumns, count)
            code <- alloca $ \slot -> c_duckdb_v2_table_function_partition_data_get_partition_column_index info count slot nullPtr
            assertBool "partition data rejects the index that equals the partition column count" (code /= DuckDBV2ErrorNone)
            c_duckdb_v2_table_function_partition_data_set_batch_index info (10 ^ (14 :: Int)) nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
            batch <- peek (castPtr local :: Ptr Int64)
            checked (c_duckdb_v2_table_function_partition_data_set_batch_index info (fromIntegral batch))
    with (3 :: Int64) $ \user ->
        withFunPtr (DuckDBV2TableFunctionBindCallbackFn_Aux bind) $ \bindPtr ->
            withFunPtr (DuckDBV2TableFunctionInitGlobalCallbackFn_Aux initGlobal) $ \initGlobalPtr ->
                withFunPtr (DuckDBV2TableFunctionInitLocalCallbackFn_Aux initLocal) $ \initLocalPtr ->
                    withFunPtr (DuckDBV2TableFunctionExecCallbackFn_Aux execute) $ \executePtr ->
                        withFunPtr (DuckDBV2TableFunctionPartitionDataCallbackFn_Aux partitionData) $ \partitionDataPtr -> withConnection $ \conn -> do
                            let register name configure = registerTable conn name $ \function -> do
                                    lend user (checked . c_duckdb_v2_table_function_set_user_data function)
                                    withFactory conn $ \factory -> withType factory "BIGINT" $ \ty -> withView "order" $ \parameter -> do
                                        signature <- output (c_duckdb_v2_table_function_get_signature function)
                                        checked (c_duckdb_v2_function_signature_add_parameter signature (coerce parameter) ty (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
                                    checked (c_duckdb_v2_table_function_set_bind_callback function (DuckDBV2TableFunctionBindCallbackFn bindPtr))
                                    checked (c_duckdb_v2_table_function_set_init_global_callback function (DuckDBV2TableFunctionInitGlobalCallbackFn initGlobalPtr))
                                    checked (c_duckdb_v2_table_function_set_init_local_callback function (DuckDBV2TableFunctionInitLocalCallbackFn initLocalPtr))
                                    checked (c_duckdb_v2_table_function_set_exec_callback function (DuckDBV2TableFunctionExecCallbackFn executePtr))
                                    configure function
                                -- Return the rows of the query and the number of local states that the scan created.
                                scan sql = do
                                    writeIORef next 0
                                    before <- length <$> readIORef locals
                                    rows <- withResult conn sql readIntegers
                                    after <- length <$> readIORef locals
                                    pure (rows, after - before)
                                expected = map Just [10 * batch + row | batch <- [0 .. 7], row <- [0 .. 2]]
                            register "v2_scan" (\_ -> pure ())
                            register "v2_batched" $ \function ->
                                checked (c_duckdb_v2_table_function_set_partition_data_callback function (DuckDBV2TableFunctionPartitionDataCallbackFn partitionDataPtr))
                            (unordered, unorderedThreads) <- scan "SELECT value FROM v2_scan(0)"
                            sort unordered @?= expected
                            assertBool "NO_ORDER lets the scan run on more than one thread" (unorderedThreads > 1)
                            (ordered, orderedThreads) <- scan "SELECT value FROM v2_scan(1)"
                            ordered @?= expected
                            orderedThreads @?= 1
                            (batched, batchedThreads) <- scan "SELECT value FROM v2_batched(1)"
                            batched @?= expected
                            assertBool "batch indexes let an ordered scan run on more than one thread" (batchedThreads > 1)
    readIORef failures >>= (@?= [])
    seen <- readIORef requests
    assertBool "the partition data callback was invoked" (not (null seen))
    nub seen @?= [(CBool 1, CBool 0, 0)]

{- | The function declares the columns pad, v and k, with projection pushdown. Batch b holds b + 1 rows with k = b.
The query does not read pad, so the scan position of k is not its declared column 2.
In DuckDB 2.0.0 only an aggregate with a cluster update, such as count(v), can use the partitioned aggregate.
Four functions share the callbacks. Their user data holds the mode of the partition data callback.
-}
testPartitioning :: IO ()
testPartitioning = do
    failures <- newIORef []
    next <- newIORef (0 :: Int64)
    binds <- newIORef [] -- (user data, bind data) of each bound call
    globals <- newIORef [] -- (bind data, global state) of each scan
    locals <- newIORef [] -- (global state, local state) of each thread
    candidates <- newIORef [] -- the declared columns of each candidate GROUP BY column set
    requests <- newIORef [] -- (partition columns required, declared partition columns) of each partition data call
    let bind info result context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_function_bind_get_user_data info)
            addColumns result context ["pad", "v", "k"]
            bound <- give [0] (c_duckdb_v2_function_bind_set_bind_data info)
            record binds (user, bound)
        initGlobal info _context err = callbackGuard failures err $ do
            bound <- output (c_duckdb_v2_table_function_init_global_get_bind_data info)
            global <- give [0] (c_duckdb_v2_table_function_init_global_set_global_state info)
            record globals (bound, global)
        -- The local state holds the batch of the last chunk that the thread wrote.
        initLocal info _context err = callbackGuard failures err $ do
            global <- output (c_duckdb_v2_table_function_init_local_get_global_state info)
            local <- give [-1] (c_duckdb_v2_table_function_init_local_set_local_state info)
            record locals (global, local)
        execute info _context err = callbackGuard failures err $ do
            local <- output (c_duckdb_v2_table_function_exec_get_local_state info)
            indexes <- declaredColumns "exec" (c_duckdb_v2_table_function_exec_get_column_count info) (c_duckdb_v2_table_function_exec_get_column_index info)
            batch <- atomicModifyIORef' next $ \value -> (value + 1, value)
            chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
            let value declared row = case declared of
                    1 -> 10 * batch + row
                    2 -> batch
                    _ -> -1
            if batch < 3
                then do
                    forM_ (zip [0 ..] indexes) $ \(position, declared) -> writeColumn chunk position [value declared row | row <- [0 .. batch]]
                    setRows chunk (fromIntegral batch + 1)
                    poke (castPtr local) batch
                else setRows chunk 0
        partitioning info _context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_table_function_partitioning_get_user_data info)
            bound <- output (c_duckdb_v2_table_function_partitioning_get_bind_data info)
            expectRecorded "partitioning reads the user data and the bind data of its call" (user, bound) binds
            indexes <- declaredColumns "partitioning" (c_duckdb_v2_table_function_partitioning_get_partition_column_count info) (c_duckdb_v2_table_function_partitioning_get_partition_column_index info)
            record candidates indexes
            c_duckdb_v2_table_function_partitioning_set_partition_info info (DuckDBV2TablePartitionInfo 99) nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
            checked . c_duckdb_v2_table_function_partitioning_set_partition_info info $
                if indexes == [2] then DuckDBV2TablePartitionInfoSingleValuePartitions else DuckDBV2TablePartitionInfoNotPartitioned
        partitionData info context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_table_function_partition_data_get_user_data info)
            bound <- output (c_duckdb_v2_table_function_partition_data_get_bind_data info)
            global <- output (c_duckdb_v2_table_function_partition_data_get_global_state info)
            local <- output (c_duckdb_v2_table_function_partition_data_get_local_state info)
            expectRecorded "partition data reads the user data and the bind data of its call" (user, bound) binds
            expectRecorded "partition data reads the global state of its scan" (bound, global) globals
            expectRecorded "partition data reads the local state of the thread that wrote the chunk" (global, local) locals
            required <- output (c_duckdb_v2_table_function_partition_data_requires_partition_columns info)
            indexes <- declaredColumns "partition data" (c_duckdb_v2_table_function_partition_data_get_partition_column_count info) (c_duckdb_v2_table_function_partition_data_get_partition_column_index info)
            record requests (required, indexes)
            batch <- peek (castPtr local :: Ptr Int64)
            -- The user data holds the mode of the function. Mode 0 obeys the rules of the batch index. Mode 1 sets no
            -- batch index, mode 2 sets a decreasing batch index, and mode 3 keeps the batch index while the partition
            -- value changes.
            mode <- peek (castPtr user :: Ptr Int64)
            case mode of
                1 -> pure ()
                2 -> checked (c_duckdb_v2_table_function_partition_data_set_batch_index info (fromIntegral (10 - batch)))
                3 -> checked (c_duckdb_v2_table_function_partition_data_set_batch_index info 0)
                _ -> checked (c_duckdb_v2_table_function_partition_data_set_batch_index info (fromIntegral batch))
            factory <- output (c_duckdb_v2_context_get_factory context)
            withView "text" $ \text -> withHandle (c_duckdb_v2_value_create_varchar factory text) c_duckdb_v2_value_destroy $ \wrong ->
                c_duckdb_v2_table_function_partition_data_set_partition_value info 0 wrong nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
            withHandle (c_duckdb_v2_value_create_bigint factory batch) c_duckdb_v2_value_destroy $ \partition -> do
                c_duckdb_v2_table_function_partition_data_set_partition_value info (fromIntegral (length indexes)) partition nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
                checked (c_duckdb_v2_table_function_partition_data_set_partition_value info 0 partition)
    with (0 :: Int64) $ \valid -> with (1 :: Int64) $ \unindexed -> with (2 :: Int64) $ \decreasing -> with (3 :: Int64) $ \unchanged ->
        withFunPtr (DuckDBV2TableFunctionBindCallbackFn_Aux bind) $ \bindPtr ->
            withFunPtr (DuckDBV2TableFunctionInitGlobalCallbackFn_Aux initGlobal) $ \initGlobalPtr ->
                withFunPtr (DuckDBV2TableFunctionInitLocalCallbackFn_Aux initLocal) $ \initLocalPtr ->
                    withFunPtr (DuckDBV2TableFunctionExecCallbackFn_Aux execute) $ \executePtr ->
                        withFunPtr (DuckDBV2TableFunctionPartitioningCallbackFn_Aux partitioning) $ \partitioningPtr ->
                            withFunPtr (DuckDBV2TableFunctionPartitionDataCallbackFn_Aux partitionData) $ \partitionDataPtr -> withConnection $ \conn -> do
                                let register name user = registerTable conn name $ \function -> do
                                        lend user (checked . c_duckdb_v2_table_function_set_user_data function)
                                        checked (c_duckdb_v2_table_function_set_bind_callback function (DuckDBV2TableFunctionBindCallbackFn bindPtr))
                                        checked (c_duckdb_v2_table_function_set_init_global_callback function (DuckDBV2TableFunctionInitGlobalCallbackFn initGlobalPtr))
                                        checked (c_duckdb_v2_table_function_set_init_local_callback function (DuckDBV2TableFunctionInitLocalCallbackFn initLocalPtr))
                                        checked (c_duckdb_v2_table_function_set_exec_callback function (DuckDBV2TableFunctionExecCallbackFn executePtr))
                                        checked (c_duckdb_v2_table_function_set_partitioning_callback function (DuckDBV2TableFunctionPartitioningCallbackFn partitioningPtr))
                                        checked (c_duckdb_v2_table_function_set_partition_data_callback function (DuckDBV2TableFunctionPartitionDataCallbackFn partitionDataPtr))
                                        checked (c_duckdb_v2_table_function_set_projection_pushdown function (CBool 1))
                                    query name = "SELECT k * 100 + count(v) FROM " <> name <> "() GROUP BY k ORDER BY k"
                                forM_ [("v2_partitioned", valid), ("v2_unindexed", unindexed), ("v2_decreasing", decreasing), ("v2_unchanged", unchanged)] (uncurry register)
                                withResult conn (query "v2_partitioned") $ \result -> readIntegers result >>= (@?= map Just [1, 102, 203])
                                -- Each other function breaks one rule of the batch index, so the query fails after its partition
                                -- data callback returns.
                                forM_ ["v2_unindexed", "v2_decreasing", "v2_unchanged"] $ \name -> do
                                    writeIORef next 0
                                    void (statementError conn (query name))
    readIORef failures >>= (@?= [])
    asked <- readIORef candidates
    assertBool "the partitioning callback was invoked" (not (null asked))
    nub asked @?= [[2]]
    seen <- readIORef requests
    assertBool "the partition data callback was invoked" (not (null seen))
    nub seen @?= [(CBool 1, [2])]

{- | The function takes a percentage and reports it as the progress of its scan. The scan writes one row in each of
20 chunks. The test reads the progress of the connection after the first chunk of each result.
The connection publishes progress only when the enable_progress_bar option is set.
-}
testProgress :: IO ()
testProgress = do
    failures <- newIORef []
    calls <- newIORef (0 :: Int)
    binds <- newIORef [] -- (user data, bind data) of each bound call
    globals <- newIORef [] -- (bind data, global state) of each scan
    let bind info result context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_function_bind_get_user_data info)
            percent <- withHandle (c_duckdb_v2_function_bind_get_arg_value info 0) c_duckdb_v2_value_destroy (output . c_duckdb_v2_value_get_bigint)
            addColumns result context ["value"]
            bound <- give [percent] (c_duckdb_v2_function_bind_set_bind_data info)
            record binds (user, bound)
        -- The global state holds the number of rows written. Only one thread writes rows.
        initGlobal info _context err = callbackGuard failures err $ do
            bound <- output (c_duckdb_v2_table_function_init_global_get_bind_data info)
            global <- give [0] (c_duckdb_v2_table_function_init_global_set_global_state info)
            record globals (bound, global)
        execute info _context err = callbackGuard failures err $ do
            global <- castPtr <$> output (c_duckdb_v2_table_function_exec_get_global_state info)
            written <- peek global
            chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
            if written < (20 :: Int64)
                then writeColumn chunk 0 [written] >> setRows chunk 1 >> poke global (written + 1)
                else setRows chunk 0
        progress info _context err = callbackGuard failures err $ do
            atomicModifyIORef' calls $ \count -> (count + 1, ())
            user <- output (c_duckdb_v2_table_function_progress_get_user_data info)
            bound <- output (c_duckdb_v2_table_function_progress_get_bind_data info)
            global <- output (c_duckdb_v2_table_function_progress_get_global_state info)
            expectRecorded "progress reads the user data and the bind data of its call" (user, bound) binds
            expectRecorded "progress reads the global state of its scan" (bound, global) globals
            percent <- peek (castPtr bound :: Ptr Int64)
            checked (c_duckdb_v2_table_function_progress_set_progress info (fromIntegral percent / 100))
    with (5 :: Int64) $ \user ->
        withFunPtr (DuckDBV2TableFunctionBindCallbackFn_Aux bind) $ \bindPtr ->
            withFunPtr (DuckDBV2TableFunctionInitGlobalCallbackFn_Aux initGlobal) $ \initGlobalPtr ->
                withFunPtr (DuckDBV2TableFunctionExecCallbackFn_Aux execute) $ \executePtr ->
                    withFunPtr (DuckDBV2TableFunctionProgressCallbackFn_Aux progress) $ \progressPtr -> withConnection $ \conn -> do
                        registerTable conn "v2_progress" $ \function -> do
                            lend user (checked . c_duckdb_v2_table_function_set_user_data function)
                            withFactory conn $ \factory -> withType factory "BIGINT" $ \ty -> withView "percent" $ \parameter -> do
                                signature <- output (c_duckdb_v2_table_function_get_signature function)
                                checked (c_duckdb_v2_function_signature_add_parameter signature (coerce parameter) ty (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
                            checked (c_duckdb_v2_table_function_set_bind_callback function (DuckDBV2TableFunctionBindCallbackFn bindPtr))
                            checked (c_duckdb_v2_table_function_set_init_global_callback function (DuckDBV2TableFunctionInitGlobalCallbackFn initGlobalPtr))
                            checked (c_duckdb_v2_table_function_set_exec_callback function (DuckDBV2TableFunctionExecCallbackFn executePtr))
                            checked (c_duckdb_v2_table_function_set_progress_callback function (DuckDBV2TableFunctionProgressCallbackFn progressPtr))
                        let current = alloca $ \percentage -> alloca $ \processed -> alloca $ \total -> do
                                checked (c_duckdb_v2_connection_progress_get conn percentage processed total)
                                (,,) <$> peek percentage <*> peek processed <*> peek total
                            -- Read the progress after the first chunk, while the query is active.
                            during percent = withResult conn ("SELECT value FROM v2_progress(" <> show (percent :: Int) <> ")") $ \result -> do
                                alloca $ \chunk -> do
                                    poke chunk (coerce nullPtr)
                                    checked (c_duckdb_v2_result_fetch_chunk result chunk)
                                    peek chunk >>= assertBool "the query returns a first chunk" . (/= coerce nullPtr)
                                    c_duckdb_v2_data_chunk_destroy chunk >>= (@?= DuckDBV2ErrorNone)
                                sample <- current
                                consume result
                                pure sample
                            checkRows (value, processed, total) = do
                                assertBool ("rows processed " <> show processed <> " exceed the total " <> show total) (processed <= total)
                                pure value
                        current >>= (@?= (-1, 0, 0))
                        during 25 >>= (@?= (-1, 0, 0))
                        withResult conn "SET enable_progress_bar_print = false" consume
                        withResult conn "SET enable_progress_bar = true" consume
                        -- Executor::GetPipelinesProgress adds the progress of all pipelines of the query. Thus the test
                        -- does not expect the percentage of the scan, only a higher query progress for a higher scan progress.
                        none <- during 0 >>= checkRows
                        quarter <- during 25 >>= checkRows
                        full <- during 100 >>= checkRows
                        assertBool ("the query progress grows with the scan progress: " <> show [none, quarter, full]) (0 <= none && none < quarter && quarter < full && full <= 100)
                        -- The engine clamps a fraction outside [0, 1].
                        during 400 >>= checkRows >>= (@?= full)
                        during (-300) >>= checkRows >>= (@?= none)
                        current >>= (@?= (-1, 0, 0))
    readIORef failures >>= (@?= [])
    readIORef calls >>= assertBool "the progress callback was invoked" . (> 0)

-- | A multi-file function wraps a function that reads one file. The wrapped function writes one row for each file.
testBindInfo :: IO ()
testBindInfo =
    bracket
        ( do
            (path, handle) <- openBinaryTempFile "." "duckdb-v2-table"
            hClose handle
            pure path
        )
        (\path -> withCString path $ \name -> removeTestFile name >>= (@?= 0))
        $ \path -> do
            failures <- newIORef []
            users <- newIORef [] -- the user data that each get bind info call read
            let bind _info result context err = callbackGuard failures err (addColumns result context ["value"])
                -- The global state of the scan of a file holds whether exec wrote the row of the file.
                initGlobal info _context err = callbackGuard failures err (() <$ give [0] (c_duckdb_v2_table_function_init_global_set_global_state info))
                execute info _context err = callbackGuard failures err $ do
                    global <- castPtr <$> output (c_duckdb_v2_table_function_exec_get_global_state info)
                    written <- peek global
                    chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
                    if written == (0 :: Int64)
                        then writeColumn chunk 0 [42] >> setRows chunk 1 >> poke global 1
                        else setRows chunk 0
                getBindInfo info _context err =
                    callbackGuard failures err $
                        output (c_duckdb_v2_table_function_get_bind_info_get_user_data info) >>= record users
            with (6 :: Int64) $ \user ->
                withFunPtr (DuckDBV2TableFunctionBindCallbackFn_Aux bind) $ \bindPtr ->
                    withFunPtr (DuckDBV2TableFunctionInitGlobalCallbackFn_Aux initGlobal) $ \initGlobalPtr ->
                        withFunPtr (DuckDBV2TableFunctionExecCallbackFn_Aux execute) $ \executePtr ->
                            withFunPtr (DuckDBV2TableFunctionGetBindInfoCallbackFn_Aux getBindInfo) $ \getBindInfoPtr -> do
                                withConnection $ \conn -> do
                                    registerTable conn "v2_file" $ \function -> do
                                        lend user (checked . c_duckdb_v2_table_function_set_user_data function)
                                        withFactory conn $ \factory -> withType factory "VARCHAR" $ \ty -> withView "path" $ \parameter -> do
                                            signature <- output (c_duckdb_v2_table_function_get_signature function)
                                            checked (c_duckdb_v2_function_signature_add_parameter signature (coerce parameter) ty (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
                                        checked (c_duckdb_v2_table_function_set_bind_callback function (DuckDBV2TableFunctionBindCallbackFn bindPtr))
                                        checked (c_duckdb_v2_table_function_set_init_global_callback function (DuckDBV2TableFunctionInitGlobalCallbackFn initGlobalPtr))
                                        checked (c_duckdb_v2_table_function_set_exec_callback function (DuckDBV2TableFunctionExecCallbackFn executePtr))
                                        checked (c_duckdb_v2_table_function_set_get_bind_info_callback function (DuckDBV2TableFunctionGetBindInfoCallbackFn getBindInfoPtr))
                                    withFactory conn $ \factory ->
                                        withHandle (c_duckdb_v2_multi_file_function_create factory) c_duckdb_v2_multi_file_function_destroy $ \files -> do
                                            withView "v2_files" $ \name -> checked (c_duckdb_v2_multi_file_function_set_name files (coerce name))
                                            withView "v2_file" $ \name -> checked (c_duckdb_v2_multi_file_function_set_single_file_function files (coerce name))
                                            checked (c_duckdb_v2_connection_register_multi_file_function conn files)
                                    withResult conn ("SELECT value FROM v2_files('" <> path <> "')") $ \result -> readIntegers result >>= (@?= [Just 42])
                                readIORef failures >>= (@?= [])
                                seen <- readIORef users
                                assertBool "the get bind info callback was invoked" (not (null seen))
                                nub seen @?= [castPtr user]

-- | A node of a filter predicate, as the expression functions report it.
data Node = Node DuckDBV2ExpressionType DuckDBV2LogicalTypeId Detail [Node]
    deriving (Eq, Ord, Show)

{- | What the accessor of the node type reads: the declared column of a column reference, the value of a constant,
the qualified name of a function call, or the mode of a cast.
-}
data Detail = Column DuckDBIdx | Constant (Maybe Int64) | Function [String] | Cast DuckDBV2CastMode | Other
    deriving (Eq, Ord, Show)

-- | Assert that a call with an output slot fails. The slot starts as zero bytes, thus as a null handle.
rejected :: (Storable a) => String -> (Ptr a -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO ()
rejected what call = bracket calloc free $ \slot -> call slot nullPtr >>= assertBool what . (/= DuckDBV2ErrorNone)

-- | Return the declared columns that the column references of a predicate point at.
columnsOf :: Node -> [DuckDBIdx]
columnsOf (Node _ _ detail children) = [column | Column column <- [detail]] <> concatMap columnsOf children

{- | Read the qualified name of the function that a node calls, and check that its last part is the name of the function.
The test reads the borrowed name after it destroys the qualified name, so that Valgrind checks the lifetime of the name.
-}
functionName :: DuckDBV2ExpressionHandle -> IO [String]
functionName node = do
    name <- output (c_duckdb_v2_expression_function_get_name node)
    parts <- withHandle (c_duckdb_v2_expression_function_get_qname node) c_duckdb_v2_qname_destroy $ \qname -> do
        count <- output (c_duckdb_v2_qname_get_part_count qname)
        forM (take (fromIntegral count) [0 ..]) $ \index -> output (c_duckdb_v2_qname_get_part qname index) >>= readView . coerce
    text <- readView (coerce name)
    assertBool ("the qualified name " <> show parts <> " ends with the name " <> text) (drop (length parts - 1) parts == [text])
    pure parts

{- | Read a predicate node and its children through the expression functions.
The walk also checks that each accessor rejects a node of a type that it does not apply to.
-}
readNode :: DuckDBV2TableFunctionFilterPushdownInfoHandle -> DuckDBV2ExpressionHandle -> IO Node
readNode info node = do
    kind <- output (c_duckdb_v2_expression_get_type node)
    returned <- withHandle (c_duckdb_v2_expression_get_return_type node) c_duckdb_v2_logical_type_destroy (output . c_duckdb_v2_logical_type_get_id)
    count <- output (c_duckdb_v2_expression_get_child_count node)
    children <- forM (take (fromIntegral count) [0 ..]) $ \index -> output (c_duckdb_v2_expression_get_child node index) >>= readNode info
    rejected "get_child rejects the index that equals the child count" (c_duckdb_v2_expression_get_child node count)
    let comparisons =
            [ DuckDBV2ExpressionTypeCompareEqual
            , DuckDBV2ExpressionTypeCompareNotequal
            , DuckDBV2ExpressionTypeCompareLessthan
            , DuckDBV2ExpressionTypeCompareGreaterthan
            , DuckDBV2ExpressionTypeCompareLessthanorequalto
            , DuckDBV2ExpressionTypeCompareGreaterthanorequalto
            , DuckDBV2ExpressionTypeCompareDistinctFrom
            , DuckDBV2ExpressionTypeCompareNotDistinctFrom
            ]
        calls = DuckDBV2ExpressionTypeBoundFunction : DuckDBV2ExpressionTypeCompareBetween : DuckDBV2ExpressionTypeOperatorCast : comparisons
    unless (kind == DuckDBV2ExpressionTypeBoundColumnRef) $
        rejected "column_ref_get_index rejects a node that is not a column reference" (c_duckdb_v2_expression_column_ref_get_index node)
    -- The call destroys a value that constant_get_value creates all the same.
    unless (kind == DuckDBV2ExpressionTypeValueConstant) $
        rejected "constant_get_value rejects a node that is not a constant" (\slot err -> c_duckdb_v2_expression_constant_get_value node slot err <* c_duckdb_v2_value_destroy slot)
    unless (kind == DuckDBV2ExpressionTypeOperatorCast) $
        rejected "cast_get_mode rejects a node that is not a cast" (c_duckdb_v2_expression_cast_get_mode node)
    -- The header does not tell if IN is a comparison. Thus the walk does not examine the name functions on IN.
    unless (kind `elem` calls || kind == DuckDBV2ExpressionTypeCompareIn) $ do
        rejected "function_get_name rejects a node that is not a function call" (c_duckdb_v2_expression_function_get_name node)
        rejected "function_get_qname rejects a node that is not a function call" (\slot err -> c_duckdb_v2_expression_function_get_qname node slot err <* c_duckdb_v2_qname_destroy slot)
    name <- if kind `elem` calls then functionName node else pure []
    detail <- case kind of
        DuckDBV2ExpressionTypeBoundColumnRef -> do
            index <- output (c_duckdb_v2_expression_column_ref_get_index node)
            Column <$> output (c_duckdb_v2_table_function_filter_pushdown_get_column_index info index)
        DuckDBV2ExpressionTypeValueConstant ->
            withHandle (c_duckdb_v2_expression_constant_get_value node) c_duckdb_v2_value_destroy $ \value -> do
                isNull <- output (c_duckdb_v2_value_is_null value)
                if isNull /= 0 then pure (Constant Nothing) else Constant . Just <$> output (c_duckdb_v2_value_get_bigint value)
        DuckDBV2ExpressionTypeOperatorCast -> Cast <$> output (c_duckdb_v2_expression_cast_get_mode node)
        -- The name of BETWEEN is internal.
        _
            | kind `elem` calls && kind /= DuckDBV2ExpressionTypeCompareBetween -> pure (Function name)
            | otherwise -> pure Other
    pure (Node kind returned detail children)

{- | Two functions declare the columns a, b and c, with projection pushdown. The exec callback writes
100 * (declared column) + (row) into each scanned column, for five rows, and applies no filter.
The user data holds the declared column whose predicates the filter pushdown callback accepts:
-1 for v2_declining and 2 for v2_accepting.
-}
testFilterPushdown :: IO ()
testFilterPushdown = do
    failures <- newIORef []
    binds <- newIORef [] -- (user data, bind data) of each bound call
    offers <- newIORef [] -- (declared columns, predicates) of each filter pushdown call
    scans <- newIORef [] -- the number of filter pushdown calls that each scan reads from its bind data
    let bind info result context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_function_bind_get_user_data info)
            addColumns result context ["a", "b", "c"]
            bound <- give [0] (c_duckdb_v2_function_bind_set_bind_data info)
            record binds (user, bound)
        -- The global state holds whether exec wrote the rows. Only one thread writes rows.
        initGlobal info _context err = callbackGuard failures err $ do
            bound <- output (c_duckdb_v2_table_function_init_global_get_bind_data info)
            peek (castPtr bound :: Ptr Int64) >>= record scans
            () <$ give [0] (c_duckdb_v2_table_function_init_global_set_global_state info)
        execute info _context err = callbackGuard failures err $ do
            global <- castPtr <$> output (c_duckdb_v2_table_function_exec_get_global_state info)
            written <- peek global
            indexes <- declaredColumns "exec" (c_duckdb_v2_table_function_exec_get_column_count info) (c_duckdb_v2_table_function_exec_get_column_index info)
            chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
            if written == (0 :: Int64)
                then do
                    forM_ (zip [0 ..] indexes) $ \(position, declared) -> writeColumn chunk position [100 * fromIntegral declared + row | row <- [0 .. 4]]
                    setRows chunk 5
                    poke global 1
                else setRows chunk 0
        pushdown info _context err = callbackGuard failures err $ do
            user <- output (c_duckdb_v2_table_function_filter_pushdown_get_user_data info)
            bound <- output (c_duckdb_v2_table_function_filter_pushdown_get_bind_data info)
            expectRecorded "filter pushdown reads the user data and the bind data of its call" (user, bound) binds
            -- Count the call in the bind data. The init callback reads the count from its own bind data.
            peek (castPtr bound) >>= poke (castPtr bound) . (+ (1 :: Int64))
            columns <- declaredColumns "filter pushdown" (c_duckdb_v2_table_function_filter_pushdown_get_column_count info) (c_duckdb_v2_table_function_filter_pushdown_get_column_index info)
            count <- output (c_duckdb_v2_table_function_filter_pushdown_get_filter_count info)
            rejected "get_filter rejects the index that equals the filter count" (c_duckdb_v2_table_function_filter_pushdown_get_filter info count)
            c_duckdb_v2_table_function_filter_pushdown_accept info count nullPtr >>= assertBool "accept rejects the index that equals the filter count" . (/= DuckDBV2ErrorNone)
            predicates <- forM (take (fromIntegral count) [0 ..]) $ \index ->
                output (c_duckdb_v2_table_function_filter_pushdown_get_filter info index) >>= readNode info
            accepted <- peek (castPtr user :: Ptr Int64)
            forM_ (zip [0 ..] predicates) $ \(index, predicate) ->
                when (accepted `elem` map fromIntegral (columnsOf predicate)) $ checked (c_duckdb_v2_table_function_filter_pushdown_accept info index)
            record offers (sort columns, sort predicates)
    with (-1 :: Int64) $ \declining -> with (2 :: Int64) $ \accepting ->
        withFunPtr (DuckDBV2TableFunctionBindCallbackFn_Aux bind) $ \bindPtr ->
            withFunPtr (DuckDBV2TableFunctionInitGlobalCallbackFn_Aux initGlobal) $ \initGlobalPtr ->
                withFunPtr (DuckDBV2TableFunctionExecCallbackFn_Aux execute) $ \executePtr ->
                    withFunPtr (DuckDBV2TableFunctionFilterPushdownCallbackFn_Aux pushdown) $ \pushdownPtr -> withConnection $ \conn -> do
                        let register name user = registerTable conn name $ \function -> do
                                lend user (checked . c_duckdb_v2_table_function_set_user_data function)
                                checked (c_duckdb_v2_table_function_set_bind_callback function (DuckDBV2TableFunctionBindCallbackFn bindPtr))
                                checked (c_duckdb_v2_table_function_set_init_global_callback function (DuckDBV2TableFunctionInitGlobalCallbackFn initGlobalPtr))
                                checked (c_duckdb_v2_table_function_set_exec_callback function (DuckDBV2TableFunctionExecCallbackFn executePtr))
                                checked (c_duckdb_v2_table_function_set_filter_pushdown_callback function (DuckDBV2TableFunctionFilterPushdownCallbackFn pushdownPtr))
                                checked (c_duckdb_v2_table_function_set_projection_pushdown function (CBool 1))
                            -- Return the values of a and the different (declared columns, predicates) of the filter pushdown calls.
                            query name condition = do
                                writeIORef offers []
                                writeIORef scans []
                                rows <- withResult conn ("SELECT a FROM " <> name <> "()" <> condition) readIntegers
                                seen <- readIORef offers
                                readIORef scans >>= (@?= [fromIntegral (length seen)])
                                pure (rows, nub seen)
                            -- The engine applies each predicate that the callback does not accept.
                            declined condition rows columns predicates =
                                query "v2_declining" (" WHERE " <> condition) >>= (@?= (map Just rows, [(columns, sort predicates)]))
                            -- The engine does not apply an accepted predicate, and the scan does not apply it either.
                            accepted condition rows columns predicates = do
                                (result, seen) <- query "v2_accepting" (" WHERE " <> condition)
                                result @?= map Just rows
                                assertBool ("the filter pushdown callback reads the predicates of " <> condition) ((columns, sort predicates) `elem` seen)
                            column declared = Node DuckDBV2ExpressionTypeBoundColumnRef DuckDBV2LogicalTypeIdBigInt (Column declared) []
                            constant ty value = Node DuckDBV2ExpressionTypeValueConstant ty (Constant value) []
                            bigint value = constant DuckDBV2LogicalTypeIdBigInt (Just value)
                            boolean kind = Node kind DuckDBV2LogicalTypeIdBoolean
                            comparison kind name left right = boolean kind (Function [name]) [left, right]
                            equal = comparison DuckDBV2ExpressionTypeCompareEqual "="
                            less = comparison DuckDBV2ExpressionTypeCompareLessthan "<"
                            greater = comparison DuckDBV2ExpressionTypeCompareGreaterthan ">"
                            call name = Node DuckDBV2ExpressionTypeBoundFunction DuckDBV2LogicalTypeIdBigInt (Function ["system", "main", name])
                            cast ty mode child = Node DuckDBV2ExpressionTypeOperatorCast ty (Cast mode) [child]
                        register "v2_declining" declining
                        register "v2_accepting" accepting
                        -- The engine does not call the callback for a query without a predicate.
                        query "v2_declining" "" >>= (@?= (map Just [0 .. 4], []))
                        declined "c > 202" [3, 4] [0, 2] [greater (column 2) (bigint 202)]
                        -- The engine splits a conjunction at the top into separate predicates.
                        declined "c > 201 AND a < 4" [2, 3] [0, 2] [greater (column 2) (bigint 201), less (column 0) (bigint 4)]
                        declined "greatest_common_divisor(b - 100, 4) = 2" [2] [0, 1] [equal (call "greatest_common_divisor" [call "-" [column 1, bigint 100], bigint 4]) (bigint 2)]
                        declined "CAST(c AS INTEGER) = 203" [3] [0, 2] [equal (cast DuckDBV2LogicalTypeIdInteger DuckDBV2CastModeNormal (column 2)) (constant DuckDBV2LogicalTypeIdInteger (Just 203))]
                        declined "TRY_CAST(c AS VARCHAR) = '203'" [3] [0, 2] [equal (cast DuckDBV2LogicalTypeIdVarchar DuckDBV2CastModeTry (column 2)) (constant DuckDBV2LogicalTypeIdVarchar (Just 203))]
                        declined
                            "(a = 1 AND c = 201) OR b = 103"
                            [1, 3]
                            [0, 1, 2]
                            [boolean DuckDBV2ExpressionTypeConjunctionOr Other [boolean DuckDBV2ExpressionTypeConjunctionAnd Other [equal (column 0) (bigint 1), equal (column 2) (bigint 201)], equal (column 1) (bigint 103)]]
                        declined "c BETWEEN 201 AND 203" [1, 2, 3] [0, 2] [boolean DuckDBV2ExpressionTypeCompareBetween Other [column 2, bigint 201, bigint 203]]
                        declined "c IN (201, 203)" [1, 3] [0, 2] [boolean DuckDBV2ExpressionTypeCompareIn Other [column 2, bigint 201, bigint 203]]
                        declined
                            "CASE WHEN a = 1 THEN NULL ELSE c END IS NULL"
                            [1]
                            [0, 2]
                            [boolean DuckDBV2ExpressionTypeOperatorIsNull Other [Node DuckDBV2ExpressionTypeCaseExpr DuckDBV2LogicalTypeIdBigInt Other [equal (column 0) (bigint 1), constant DuckDBV2LogicalTypeIdBigInt Nothing, column 2]]]
                        accepted "c > 202" [0 .. 4] [0, 2] [greater (column 2) (bigint 202)]
                        accepted "c > 201 AND a < 4" [0 .. 3] [0, 2] [greater (column 2) (bigint 201), less (column 0) (bigint 4)]
                        accepted "a < 4 AND c > 201" [0 .. 3] [0, 2] [less (column 0) (bigint 4), greater (column 2) (bigint 201)]
    readIORef failures >>= (@?= [])

-- | The phases of v2_failing, in the order of the index that its states hold.
failingPhases :: [String]
failingPhases = ["none", "bind", "columns", "pushdown", "init", "exec"]

{- | The argument of v2_failing names the callback that reports an error: bind, pushdown (the filter pushdown
callback), init (the global init callback) or exec. With columns, the bind callback declares no result column. With
none, no callback fails and the query returns the rows 2 and 3.
The bind data and the global state hold their kind (1 or 2) and the index of the phase. Their destructor records the
kind, so the test sees which states DuckDB destroyed.
-}
testFailingCallbacks :: IO ()
testFailingCallbacks = do
    failures <- newIORef []
    destroyed <- newIORef []
    let destroy ptr = peek (castPtr ptr :: Ptr Int64) >>= record destroyed >> free ptr
    withFunPtr (DuckDBV2OpaqueDestroyFn_Aux destroy) $ \destroyPtr -> do
        let
            -- Give DuckDB cells that hold the kind, the phase index and a flag, with the destructor that records the kind.
            own kind phase setter = do
                cells <- mallocArray 3
                pokeArray cells [kind, fromIntegral (fromMaybe (-1) (elemIndex phase failingPhases)), 0 :: Int64]
                let opaque = DuckDBV2Opaque (castPtr cells) (DuckDBV2OpaqueDestroyFn destroyPtr) (DuckDBV2OpaqueEqualsFn nullFunPtr)
                with opaque (checked . setter) `onException` free cells
            phaseOf state = (failingPhases !!) . fromIntegral <$> peekElemOff (castPtr state :: Ptr Int64) 1
            failIn phase name = when (phase == name) $ deliberateFailure ("v2 deliberate " <> name <> " failure")
            bind info result context err = callbackGuard failures err $ do
                phase <- withHandle (c_duckdb_v2_function_bind_get_arg_value info 0) c_duckdb_v2_value_destroy (\value -> output (c_duckdb_v2_value_get_varchar value) >>= readView)
                -- The callback sets the bind data before it fails, so DuckDB owns the bind data and must destroy it.
                own 1 phase (c_duckdb_v2_function_bind_set_bind_data info)
                failIn phase "bind"
                unless (phase == "columns") $ addColumns result context ["value"]
            pushdown info _context err =
                callbackGuard failures err $
                    output (c_duckdb_v2_table_function_filter_pushdown_get_bind_data info) >>= phaseOf >>= (`failIn` "pushdown")
            initGlobal info _context err = callbackGuard failures err $ do
                phase <- output (c_duckdb_v2_table_function_init_global_get_bind_data info) >>= phaseOf
                own 2 phase (c_duckdb_v2_table_function_init_global_set_global_state info)
                failIn phase "init"
            execute info _context err = callbackGuard failures err $ do
                output (c_duckdb_v2_table_function_exec_get_bind_data info) >>= phaseOf >>= (`failIn` "exec")
                global <- castPtr <$> output (c_duckdb_v2_table_function_exec_get_global_state info)
                written <- peekElemOff global 2
                chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
                if written == (0 :: Int64)
                    then writeColumn chunk 0 [1, 2, 3] >> setRows chunk 3 >> pokeElemOff global 2 1
                    else setRows chunk 0
        withFunPtr (DuckDBV2TableFunctionBindCallbackFn_Aux bind) $ \bindPtr ->
            withFunPtr (DuckDBV2TableFunctionFilterPushdownCallbackFn_Aux pushdown) $ \pushdownPtr ->
                withFunPtr (DuckDBV2TableFunctionInitGlobalCallbackFn_Aux initGlobal) $ \initGlobalPtr ->
                    withFunPtr (DuckDBV2TableFunctionExecCallbackFn_Aux execute) $ \executePtr -> withConnection $ \conn -> do
                        registerTable conn "v2_failing" $ \function -> do
                            withFactory conn $ \factory -> withType factory "VARCHAR" $ \ty -> withView "phase" $ \parameter -> do
                                signature <- output (c_duckdb_v2_table_function_get_signature function)
                                checked (c_duckdb_v2_function_signature_add_parameter signature (coerce parameter) ty (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
                            checked (c_duckdb_v2_table_function_set_bind_callback function (DuckDBV2TableFunctionBindCallbackFn bindPtr))
                            checked (c_duckdb_v2_table_function_set_filter_pushdown_callback function (DuckDBV2TableFunctionFilterPushdownCallbackFn pushdownPtr))
                            checked (c_duckdb_v2_table_function_set_init_global_callback function (DuckDBV2TableFunctionInitGlobalCallbackFn initGlobalPtr))
                            checked (c_duckdb_v2_table_function_set_exec_callback function (DuckDBV2TableFunctionExecCallbackFn executePtr))
                        let query phase = "SELECT value FROM v2_failing('" <> phase <> "') WHERE value > 1"
                            -- Return the kinds of the states that DuckDB destroyed since the last call.
                            takeDestroyed = sort <$> atomicModifyIORef' destroyed (\kinds -> ([], kinds))
                            succeeds = do
                                withResult conn (query "none") readIntegers >>= (@?= [Just 2, Just 3])
                                takeDestroyed >>= (@?= [1, 2])
                            -- The statement fails with the text of each recorded failure, and DuckDB destroyed the states of the given kinds.
                            -- Then the same statement without a failure succeeds on the same connection.
                            fails phase recorded kinds = do
                                (code, text) <- statementError conn (query phase)
                                code @?= DuckDBV2ErrorInputInvalid
                                forM_ recorded $ \failure -> assertBool ("the error contains the text of the callback: " <> text) (failure `isInfixOf` text)
                                atomicModifyIORef' failures (\seen -> ([], seen)) >>= (@?= map (\failure -> "user error (" <> failure <> ")") recorded)
                                takeDestroyed >>= (@?= kinds)
                                succeeds
                        succeeds
                        fails "bind" ["v2 deliberate bind failure"] [1]
                        fails "columns" [] [1]
                        fails "pushdown" ["v2 deliberate pushdown failure"] [1]
                        fails "init" ["v2 deliberate init failure"] [1, 2]
                        fails "exec" ["v2 deliberate exec failure"] [1, 2]
    readIORef failures >>= (@?= [])

{- | Each change makes a table function incomplete: no name, no bind callback, no exec callback, a partitioning
callback without a partition data callback, a return type, or the factory of another database. Each change makes a
multi-file function incomplete: no name, no single-file function, or the factory of another database. A multi-file
function also needs a single-file function that is registered and takes a path. The function without a change
registers.
-}
testRegistration :: IO ()
testRegistration =
    withFunPtr (DuckDBV2TableFunctionBindCallbackFn_Aux (\_ _ _ _ -> pure ())) $ \bindPtr ->
        withFunPtr (DuckDBV2TableFunctionExecCallbackFn_Aux (\_ _ _ -> pure ())) $ \executePtr ->
            withFunPtr (DuckDBV2TableFunctionPartitioningCallbackFn_Aux (\_ _ _ -> pure ())) $ \partitioningPtr ->
                withFunPtr (DuckDBV2TableFunctionPartitionDataCallbackFn_Aux (\_ _ _ -> pure ())) $ \partitionDataPtr ->
                    withConnection $ \conn -> withConnection $ \other -> do
                        let factoryFor change = if change == "another database" then other else conn
                            -- Register a table function with one parameter of the given type, with the change applied.
                            registerFunction name parameter change = withFactory (factoryFor change) $ \factory ->
                                withHandle (c_duckdb_v2_table_function_create factory) c_duckdb_v2_table_function_destroy $ \function -> do
                                    unless (change == "no name") $ withView name $ \view -> checked (c_duckdb_v2_table_function_set_name function (coerce view))
                                    signature <- output (c_duckdb_v2_table_function_get_signature function)
                                    withType factory parameter $ \ty -> withView "input" $ \view ->
                                        checked (c_duckdb_v2_function_signature_add_parameter signature (coerce view) ty (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
                                    when (change == "a return type") $ withType factory "BIGINT" (checked . c_duckdb_v2_function_signature_set_return_type signature)
                                    unless (change == "no bind callback") $ checked (c_duckdb_v2_table_function_set_bind_callback function (DuckDBV2TableFunctionBindCallbackFn bindPtr))
                                    unless (change == "no exec callback") $ checked (c_duckdb_v2_table_function_set_exec_callback function (DuckDBV2TableFunctionExecCallbackFn executePtr))
                                    checked (c_duckdb_v2_table_function_set_partitioning_callback function (DuckDBV2TableFunctionPartitioningCallbackFn partitioningPtr))
                                    unless (change == "no partition data callback") $
                                        checked (c_duckdb_v2_table_function_set_partition_data_callback function (DuckDBV2TableFunctionPartitionDataCallbackFn partitionDataPtr))
                                    c_duckdb_v2_connection_register_table_function conn function nullPtr
                            -- Register a multi-file function that wraps the single-file function, with the change applied.
                            registerFiles single change = withFactory (factoryFor change) $ \factory ->
                                withHandle (c_duckdb_v2_multi_file_function_create factory) c_duckdb_v2_multi_file_function_destroy $ \files -> do
                                    unless (change == "no name") $ withView "v2_files" $ \view -> checked (c_duckdb_v2_multi_file_function_set_name files (coerce view))
                                    unless (change == "no single-file function") $
                                        withView single $
                                            \view -> checked (c_duckdb_v2_multi_file_function_set_single_file_function files (coerce view))
                                    c_duckdb_v2_connection_register_multi_file_function conn files nullPtr
                            refused what code = assertBool (what <> " is refused: " <> show code) (code /= DuckDBV2ErrorNone)
                        forM_ ["no name", "no bind callback", "no exec callback", "no partition data callback", "a return type", "another database"] $ \change ->
                            registerFunction "v2_refused" "VARCHAR" change >>= refused ("a table function with " <> change)
                        registerFunction "v2_path" "VARCHAR" "" >>= (@?= DuckDBV2ErrorNone)
                        registerFunction "v2_number" "BIGINT" "" >>= (@?= DuckDBV2ErrorNone)
                        forM_ [("v2_path", "no name"), ("v2_path", "no single-file function"), ("v2_absent", ""), ("v2_number", ""), ("v2_path", "another database")] $ \(single, change) ->
                            registerFiles single change >>= refused ("a multi-file function over " <> single <> " with " <> show change)
                        registerFiles "v2_path" "" >>= (@?= DuckDBV2ErrorNone)
