module V2TableFunctionTest (tests) where

import Control.Exception (bracket, onException)
import Control.Monad (forM, forM_)
import Data.Coerce (coerce)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Void (Void)
import Database.DuckDB.FFI.V2
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca, finalizerFree, free)
import Foreign.Marshal.Array (mallocArray, pokeArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, poke, pokeElemOff)
import HsBindgen.Runtime.Prelude (ToFunPtr, toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (callbackGuard, checked, output, readIntegers, withConnection, withFactory, withHandle, withResult, withType, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 table function callbacks"
        [ testCase "init and exec callbacks read the data of their call and the declared column of each scanned column" testColumns
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
