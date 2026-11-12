module V2TableFunctionTest (tests) where

import Control.Exception (bracket, onException)
import Control.Monad (forM, forM_)
import Data.Coerce (coerce)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Void (Void)
import Database.DuckDB.FFI.V2
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca, finalizerFree, free)
import Foreign.Marshal.Array (mallocArray, pokeArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff)
import HsBindgen.Runtime.Prelude (ToFunPtr, toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (callbackGuard, checked, output, readIntegers, withConnection, withFactory, withHandle, withResult, withType, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 table function callbacks"
        [ testCase "init and exec callbacks read the data of their call and the declared column of each scanned column" testColumns
        , testCase "claim batch callbacks hand out batches until a claim fails" testClaimBatch
        , testCase "order preservation and batch indexes decide whether a scan runs in parallel" testParallelScan
        ]

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
lend cells = with (DuckDBV2Opaque (castPtr cells) (DuckDBV2OpaqueDestroyFn nullFunPtr) (DuckDBV2OpaqueEqualsFn nullFunPtr))

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

-- | Add a value to a list that callbacks on several threads share.
record :: IORef [a] -> a -> IO ()
record ref value = atomicModifyIORef' ref $ \values -> (value : values, ())

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
