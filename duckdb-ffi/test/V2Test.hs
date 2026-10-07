{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module V2Test (tests) where

import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (forM, forM_, unless, void)
import Data.Bits (testBit)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import qualified Database.DuckDB.FFI as Helpers
import Database.DuckDB.FFI.V2
import Foreign.C.String (CString, peekCStringLen, withCString, withCStringLen)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes, free, malloc)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import GHC.Stack (HasCallStack)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "DuckDB V2"
        [ testCase "environment refuses destruction while an instance is alive" testEnvironment
        , testCase "streamed query exposes schema, selection and validity" testQuery
        , testCase "statement binding exposes output and parameter schemas" testBind
        , testCase "live results reject another execution and cancellation releases the cursor" testCancellation
        , testCase "prepared execution binds named values repeatedly" testPrepared
        , testCase "length-delimited strings retain interior null bytes" testStrings
        , testCase "numeric structures round-trip through values" testNumericStructures
        , testCase "result errors own their diagnostic text" testError
        , testCase "scalar callbacks write result vectors" testScalar
        , testCase "table callbacks own scan state after the builder is destroyed" testTable
        , testCase "aggregate callbacks traverse grouped state arrays" testAggregate
        , testCase "custom catalog types invoke registered cast callbacks" testCast
        , testCase "writable vectors expose constants, sequences, nulls and string storage" testVectorMutation
        , testCase "connection options distinguish local and global settings" testOptions
        , testCase "COPY callbacks consume input and release their owned state" testCopy
        , testCase "native file I/O preserves positions and closes owned handles" testFileSystem
        , testCase "callback logging respects the connection log configuration" testLogging
        , testCase "native C text sink receives the rendered result" testTextSink
        , testCase "Arrow stream transfers result ownership and yields rows" testArrow
        ]

checked :: (HasCallStack) => (Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO ()
checked action = alloca $ \err -> do
    poke err nullPtr
    code <- action err
    info <- peek err
    message <-
        if info == nullPtr
            then pure ""
            else alloca $ \text -> do
                textCode <- c_duckdb_v2_error_info_get_text info text
                if textCode == DuckDBV2ErrorNone
                    then peek text >>= readView
                    else pure ("diagnostic failed: " <> show textCode)
    void (c_duckdb_v2_error_info_destroy err)
    unless (code == DuckDBV2ErrorNone) $ assertFailure (show code <> ": " <> message)

withHandle :: (HasCallStack) => (Ptr (Ptr a) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> (Ptr (Ptr a) -> IO DuckDBV2Error) -> (Ptr a -> IO b) -> IO b
withHandle create destroy action = alloca $ \slot -> do
    poke slot nullPtr
    checked (create slot)
    handle <- peek slot
    assertBool "created handle is non-null" (handle /= nullPtr)
    action handle `finally` (destroy slot >>= (@?= DuckDBV2ErrorNone))

withConnection :: (DuckDBV2ConnectionHandle -> IO a) -> IO a
withConnection action =
    withHandle c_duckdb_v2_environment_create c_duckdb_v2_environment_destroy $ \env ->
        withHandle (c_duckdb_v2_instance_create env) c_duckdb_v2_instance_destroy $ \instanceHandle -> do
            withView "threads" $ \name -> withView "2" $ \setting ->
                checked (c_duckdb_v2_instance_set_option instanceHandle name setting)
            withView ":memory:" $ \path ->
                checked (c_duckdb_v2_instance_attach instanceHandle path nullPtr nullPtr (CBool 1))
            withHandle (c_duckdb_v2_connection_create instanceHandle) c_duckdb_v2_connection_destroy action

withView :: String -> (Ptr DuckDBV2Str -> IO a) -> IO a
withView text action = withCStringLen text $ \(ptr, len) -> with (DuckDBV2Str ptr (fromIntegral len)) action

readView :: DuckDBV2Str -> IO String
readView (DuckDBV2Str ptr len)
    | len == 0 = pure ""
    | otherwise = peekCStringLen (ptr, fromIntegral len)

withStatement :: DuckDBV2ConnectionHandle -> String -> (DuckDBV2SqlStatementHandle -> IO a) -> IO a
withStatement conn sql action = withCString sql $ \text ->
    withHandle (c_duckdb_v2_parse_sql conn text) c_duckdb_v2_statement_iterator_destroy $ \iterator ->
        withHandle (c_duckdb_v2_statement_iterator_next iterator) c_duckdb_v2_sql_statement_destroy action

withResult :: DuckDBV2ConnectionHandle -> String -> (DuckDBV2ResultHandle -> IO a) -> IO a
withResult conn sql action = withStatement conn sql $ \statement ->
    withHandle (c_duckdb_v2_statement_execute conn statement nullPtr nullPtr 0) c_duckdb_v2_result_destroy action

output :: (HasCallStack, Storable a) => (Ptr a -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO a
output action = alloca $ \slot -> checked (action slot) >> peek slot

readIntegers :: DuckDBV2ResultHandle -> IO [Maybe Int64]
readIntegers result = alloca $ \chunkSlot -> do
    poke chunkSlot nullPtr
    checked (c_duckdb_v2_result_fetch_chunk result chunkSlot)
    chunk <- peek chunkSlot
    if chunk == nullPtr
        then pure []
        else do
            rows <-
                ( do
                    vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                    view <- output (c_duckdb_v2_vector_get_view vector)
                    forM [0 .. fromIntegral (duckdbV2VectorViewCount view) - 1] $ \i -> do
                        index <-
                            if duckdbV2VectorViewSel view == nullPtr
                                then pure i
                                else fromIntegral <$> peekElemOff (duckdbV2VectorViewSel view) i
                        valid <-
                            if duckdbV2VectorViewValidity view == nullPtr
                                then pure True
                                else do
                                    word <- peekElemOff (duckdbV2VectorViewValidity view) (index `div` 64)
                                    pure (testBit word (index `mod` 64))
                        if valid
                            then Just <$> peekElemOff (castPtr (duckdbV2VectorViewData view)) index
                            else pure Nothing
                )
                    `finally` (c_duckdb_v2_data_chunk_destroy chunkSlot >>= (@?= DuckDBV2ErrorNone))
            rest <- readIntegers result
            pure (rows <> rest)

testEnvironment :: IO ()
testEnvironment = alloca $ \envSlot -> do
    poke envSlot nullPtr
    checked (c_duckdb_v2_environment_create envSlot)
    env <- peek envSlot
    withHandle (c_duckdb_v2_instance_create env) c_duckdb_v2_instance_destroy $ \_ -> do
        output (c_duckdb_v2_environment_get_instance_count env) >>= (@?= 1)
        c_duckdb_v2_environment_destroy envSlot >>= (@?= DuckDBV2ErrorResourceInUse)
        peek envSlot >>= (@?= env)
    output (c_duckdb_v2_environment_get_instance_count env) >>= (@?= 0)
    c_duckdb_v2_environment_destroy envSlot >>= (@?= DuckDBV2ErrorNone)
    peek envSlot >>= (@?= nullPtr)

testQuery :: IO ()
testQuery = withConnection $ \conn ->
    withResult conn "SELECT CASE WHEN i % 3 = 0 THEN NULL ELSE i END::BIGINT AS amount FROM range(10) t(i) WHERE i % 2 = 0 ORDER BY i" $ \result -> do
        withHandle (c_duckdb_v2_result_get_schema result) c_duckdb_v2_schema_destroy $ \schema -> do
            output (c_duckdb_v2_schema_get_count schema) >>= (@?= 1)
            alloca $ \name -> do
                ty <- output (c_duckdb_v2_schema_get_field schema 0 name)
                peek name >>= readView >>= (@?= "amount")
                output (c_duckdb_v2_logical_type_get_id ty) >>= (@?= DuckDBV2LogicalTypeIdBigint)
        readIntegers result >>= (@?= [Nothing, Just 2, Just 4, Nothing, Just 8])
        readIntegers result >>= (@?= [])

testBind :: IO ()
testBind = withConnection $ \conn -> withStatement conn "SELECT $amount::BIGINT AS total" $ \statement ->
    alloca $ \parameters -> do
        poke parameters nullPtr
        withHandle (\schema err -> c_duckdb_v2_statement_bind conn statement schema parameters err) c_duckdb_v2_schema_destroy $ \schema -> do
            output (c_duckdb_v2_schema_get_count schema) >>= (@?= 1)
            params <- peek parameters
            assertBool "parameter schema is non-null" (params /= nullPtr)
            (output (c_duckdb_v2_schema_get_count params) >>= (@?= 1))
                `finally` void (c_duckdb_v2_schema_destroy parameters)

testCancellation :: IO ()
testCancellation = withConnection $ \conn -> withResult conn "SELECT i::BIGINT FROM range(100000) t(i)" $ \result -> do
    withStatement conn "SELECT 9::BIGINT" $ \statement -> alloca $ \slot -> do
        poke slot nullPtr
        code <- c_duckdb_v2_statement_execute conn statement nullPtr nullPtr 0 slot nullPtr
        code @?= DuckDBV2ErrorResourceInUse
        peek slot >>= (@?= nullPtr)
    checked (c_duckdb_v2_connection_interrupt conn)
    alloca $ \chunk -> alloca $ \status -> do
        poke chunk nullPtr
        checked (c_duckdb_v2_result_step result chunk status)
        peek status >>= (@?= DuckDBV2ResultStepStatusCancelled)
        peek chunk >>= (@?= nullPtr)
    withResult conn "SELECT 9::BIGINT" $ \second -> readIntegers second >>= (@?= [Just 9])

testPrepared :: IO ()
testPrepared = withConnection $ \conn ->
    withStatement conn "SELECT $amount::BIGINT + 1 AS answer" $ \statement ->
        withHandle (c_duckdb_v2_prepared_statement_create conn statement (CBool 1)) c_duckdb_v2_prepared_statement_destroy $ \prepared -> do
            output (c_duckdb_v2_prepared_statement_reuses_plan prepared) >>= (@?= CBool 1)
            forM_ [40, 41] $ \amount ->
                withHandle (c_duckdb_v2_value_create_bigint_with_connection conn amount) c_duckdb_v2_value_destroy $ \value ->
                    withView "amount" $ \name -> withArray [value] $ \values ->
                        withHandle (c_duckdb_v2_prepared_statement_execute prepared name values 1) c_duckdb_v2_result_destroy $ \result ->
                            readIntegers result >>= (@?= [Just (amount + 1)])

testStrings :: IO ()
testStrings = withConnection $ \conn -> withView "alpha\0omega" $ \input ->
    withHandle (c_duckdb_v2_value_create_varchar_with_connection conn input) c_duckdb_v2_value_destroy $ \value -> do
        output (c_duckdb_v2_value_get_varchar value) >>= readView >>= (@?= "alpha\0omega")
        withStatement conn "SELECT length($text)::BIGINT" $ \statement ->
            withView "text" $ \name -> withArray [value] $ \values ->
                withHandle (c_duckdb_v2_statement_execute conn statement name values 1) c_duckdb_v2_result_destroy $ \result ->
                    readIntegers result >>= (@?= [Just 11])

testNumericStructures :: IO ()
testNumericStructures = withConnection $ \conn -> do
    let huge = DuckDBHugeInt 0xffffffffffffffff (-7)
        unsigned = DuckDBUHugeInt 0xfffffffffffffffd 13
        interval = DuckDBInterval 3 (-2) 1234567
    with huge $ \ptr ->
        withHandle (c_duckdb_v2_value_create_hugeint_with_connection conn ptr) c_duckdb_v2_value_destroy $ \value ->
            output (c_duckdb_v2_value_get_hugeint value) >>= (@?= huge)
    with unsigned $ \ptr ->
        withHandle (c_duckdb_v2_value_create_uhugeint_with_connection conn ptr) c_duckdb_v2_value_destroy $ \value ->
            output (c_duckdb_v2_value_get_uhugeint value) >>= (@?= unsigned)
    with interval $ \ptr ->
        withHandle (c_duckdb_v2_value_create_interval_with_connection conn ptr) c_duckdb_v2_value_destroy $ \value ->
            output (c_duckdb_v2_value_get_interval value) >>= (@?= interval)

testError :: IO ()
testError = withConnection $ \conn -> withStatement conn "SELECT absent_column" $ \statement -> alloca $ \result -> alloca $ \err -> do
    poke result nullPtr
    poke err nullPtr
    code <- c_duckdb_v2_statement_execute conn statement nullPtr nullPtr 0 result err
    code @?= DuckDBV2ErrorQueryBinder
    peek result >>= (@?= nullPtr)
    info <- peek err
    assertBool "failure has an owned error handle" (info /= nullPtr)
    alloca $ \stored -> do
        c_duckdb_v2_error_info_get_code info stored >>= (@?= DuckDBV2ErrorNone)
        peek stored >>= (@?= code)
    alloca $ \text -> do
        c_duckdb_v2_error_info_get_text info text >>= (@?= DuckDBV2ErrorNone)
        message <- peek text >>= readView
        assertBool "error identifies the missing column" ("absent_column" `isInfixOf` message)
    c_duckdb_v2_error_info_destroy err >>= (@?= DuckDBV2ErrorNone)
    peek err >>= (@?= nullPtr)

callbackGuard :: IORef [String] -> Ptr DuckDBV2ErrorInfoHandle -> IO () -> IO ()
callbackGuard failures err action =
    action `catch` \(failure :: SomeException) -> do
        modifyIORef' failures (show failure :)
        info <- peek err
        void (c_duckdb_v2_error_info_set_code info DuckDBV2ErrorApi)
        withView (show failure) $ \message -> void (c_duckdb_v2_error_info_set_text info message)

testScalar :: IO ()
testScalar = do
    failures <- newIORef []
    calls <- newIORef (0 :: Int)
    let callback info _context err = callbackGuard failures err $ do
            modifyIORef' calls (+ 1)
            rows <- output (c_duckdb_v2_scalar_function_exec_get_row_count info)
            vector <- output (c_duckdb_v2_scalar_function_exec_get_result info)
            dataPtr <- output (c_duckdb_v2_vector_get_data_mutable vector)
            forM_ [0 .. fromIntegral rows - 1] $ \i -> pokeElemOff (castPtr dataPtr) i (42 :: Int64)
    bracket (mkDuckDBV2ScalarFunctionExecCallbackFn callback) freeHaskellFunPtr $ \callbackPtr ->
        withConnection $ \conn ->
            withHandle (c_duckdb_v2_connection_create_type_from_id conn DuckDBV2LogicalTypeIdBigint nullPtr nullPtr 0) c_duckdb_v2_logical_type_destroy $ \ty ->
                withHandle (c_duckdb_v2_scalar_function_create_with_connection conn) c_duckdb_v2_scalar_function_destroy $ \function -> do
                    withView "v2_answer" $ \name -> checked (c_duckdb_v2_scalar_function_set_name function name)
                    signature <- output (c_duckdb_v2_scalar_function_get_signature function)
                    checked (c_duckdb_v2_function_signature_set_return_type signature ty)
                    checked (c_duckdb_v2_scalar_function_set_exec_callback function callbackPtr)
                    checked (c_duckdb_v2_scalar_function_register function)
                    withResult conn "SELECT v2_answer() FROM range(3)" $ \result -> readIntegers result >>= (@?= replicate 3 (Just 42))
    readIORef failures >>= (@?= [])
    readIORef calls >>= assertBool "scalar callback was invoked" . (> 0)

testTable :: IO ()
testTable = do
    failures <- newIORef []
    destroyed <- newIORef (0 :: Int)
    let destroyState ptr = free ptr >> modifyIORef' destroyed (+ 1)
        bind info result context err = callbackGuard failures err $ do
            withHandle (c_duckdb_v2_function_bind_get_arg_value info 0) c_duckdb_v2_value_destroy $ \value ->
                output (c_duckdb_v2_value_get_bigint value) >>= (@?= 3)
            withHandle (c_duckdb_v2_context_create_type_from_id context DuckDBV2LogicalTypeIdBigint nullPtr nullPtr 0) c_duckdb_v2_logical_type_destroy $ \ty ->
                withView "amount" $ \name -> checked (c_duckdb_v2_table_function_bind_add_result_column result name ty)
            checked (c_duckdb_v2_table_function_bind_set_cardinality result 3 (CBool 1))
        execute info _context err = callbackGuard failures err $ do
            state <- output (c_duckdb_v2_table_function_exec_get_global_state info)
            emitted <- peek (castPtr state :: Ptr Int64)
            chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
            vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
            if emitted == 0
                then do
                    raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
                    forM_ [0 .. 2] $ \i -> pokeElemOff (castPtr raw) i (42 + fromIntegral i :: Int64)
                    checked (c_duckdb_v2_vector_set_size vector 3)
                    poke (castPtr state) (1 :: Int64)
                else checked (c_duckdb_v2_vector_set_size vector 0)
    bracket (mkDuckDBV2OpaqueDestroyFn destroyState) freeHaskellFunPtr $ \destroyPtr -> do
        let initialize info _context err = callbackGuard failures err $ do
                state <- malloc
                poke state (0 :: Int64)
                with (DuckDBV2Opaque (castPtr state) destroyPtr nullFunPtr) $ \opaque ->
                    checked (c_duckdb_v2_table_function_init_global_set_global_state info opaque)
                checked (c_duckdb_v2_table_function_init_global_set_max_threads info 1)
        bracket (mkDuckDBV2TableFunctionBindCallbackFn bind) freeHaskellFunPtr $ \bindPtr ->
            bracket (mkDuckDBV2TableFunctionInitGlobalCallbackFn initialize) freeHaskellFunPtr $ \initializePtr ->
                bracket (mkDuckDBV2TableFunctionExecCallbackFn execute) freeHaskellFunPtr $ \executePtr ->
                    withConnection $ \conn -> do
                        withHandle (c_duckdb_v2_connection_create_type_from_id conn DuckDBV2LogicalTypeIdBigint nullPtr nullPtr 0) c_duckdb_v2_logical_type_destroy $ \ty ->
                            withHandle (c_duckdb_v2_table_function_create_with_connection conn) c_duckdb_v2_table_function_destroy $ \function -> do
                                withView "v2_scan" $ \name -> checked (c_duckdb_v2_table_function_set_name function name)
                                signature <- output (c_duckdb_v2_table_function_get_signature function)
                                withView "count" $ \name -> checked (c_duckdb_v2_function_signature_add_parameter signature name ty nullPtr DuckDBV2FunctionParameterKindStandard)
                                checked (c_duckdb_v2_table_function_set_bind_callback function bindPtr)
                                checked (c_duckdb_v2_table_function_set_init_global_callback function initializePtr)
                                checked (c_duckdb_v2_table_function_set_exec_callback function executePtr)
                                checked (c_duckdb_v2_table_function_register function)
                        withResult conn "SELECT amount FROM v2_scan(3)" $ \result -> readIntegers result >>= (@?= [Just 42, Just 43, Just 44])
    readIORef failures >>= (@?= [])
    readIORef destroyed >>= (@?= 1)

testAggregate :: IO ()
testAggregate = do
    failures <- newIORef []
    combinations <- newIORef (0 :: Int)
    let stateSize info err =
            callbackGuard failures err $
                checked (c_duckdb_v2_aggregate_function_size_set_state_size info (fromIntegral (sizeOf (0 :: Int64))))
        initialize info err = callbackGuard failures err $ do
            states <- output (c_duckdb_v2_aggregate_function_init_get_states info)
            count <- output (c_duckdb_v2_aggregate_function_init_get_state_count info)
            forM_ [0 .. fromIntegral count - 1] $ \i -> peekElemOff states i >>= \state -> poke (castPtr state) (0 :: Int64)
        update info err = callbackGuard failures err $ do
            states <- output (c_duckdb_v2_aggregate_function_update_get_states info)
            count <- output (c_duckdb_v2_aggregate_function_update_get_row_count info)
            vector <- output (c_duckdb_v2_aggregate_function_update_get_arg info 0)
            checked (c_duckdb_v2_vector_flatten vector)
            view <- output (c_duckdb_v2_vector_get_view vector)
            forM_ [0 .. fromIntegral count - 1] $ \i -> do
                index <- if duckdbV2VectorViewSel view == nullPtr then pure i else fromIntegral <$> peekElemOff (duckdbV2VectorViewSel view) i
                value <- peekElemOff (castPtr (duckdbV2VectorViewData view)) index
                state <- castPtr <$> peekElemOff states i
                total <- peek state
                poke state (total + value :: Int64)
        combine info err = callbackGuard failures err $ do
            modifyIORef' combinations (+ 1)
            sources <- output (c_duckdb_v2_aggregate_function_combine_get_sources info)
            targets <- output (c_duckdb_v2_aggregate_function_combine_get_targets info)
            count <- output (c_duckdb_v2_aggregate_function_combine_get_state_count info)
            forM_ [0 .. fromIntegral count - 1] $ \i -> do
                source <- peekElemOff sources i >>= peek . castPtr
                target <- castPtr <$> peekElemOff targets i
                total <- peek target
                poke target (total + source :: Int64)
        finalize info err = callbackGuard failures err $ do
            states <- output (c_duckdb_v2_aggregate_function_finalize_get_states info)
            count <- output (c_duckdb_v2_aggregate_function_finalize_get_state_count info)
            offset <- output (c_duckdb_v2_aggregate_function_finalize_get_result_offset info)
            vector <- output (c_duckdb_v2_aggregate_function_finalize_get_result info)
            raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
            forM_ [0 .. fromIntegral count - 1] $ \i -> do
                total <- peekElemOff states i >>= peek . castPtr
                pokeElemOff (castPtr raw) (fromIntegral offset + i) (total :: Int64)
    bracket (mkDuckDBV2AggregateFunctionSizeCallbackFn stateSize) freeHaskellFunPtr $ \sizePtr ->
        bracket (mkDuckDBV2AggregateFunctionInitCallbackFn initialize) freeHaskellFunPtr $ \initializePtr ->
            bracket (mkDuckDBV2AggregateFunctionUpdateCallbackFn update) freeHaskellFunPtr $ \updatePtr ->
                bracket (mkDuckDBV2AggregateFunctionCombineCallbackFn combine) freeHaskellFunPtr $ \combinePtr ->
                    bracket (mkDuckDBV2AggregateFunctionFinalizeCallbackFn finalize) freeHaskellFunPtr $ \finalizePtr ->
                        withConnection $ \conn -> do
                            withHandle (c_duckdb_v2_connection_create_type_from_id conn DuckDBV2LogicalTypeIdBigint nullPtr nullPtr 0) c_duckdb_v2_logical_type_destroy $ \ty ->
                                withHandle (c_duckdb_v2_aggregate_function_create_with_connection conn) c_duckdb_v2_aggregate_function_destroy $ \function -> do
                                    withView "v2_sum" $ \name -> checked (c_duckdb_v2_aggregate_function_set_name function name)
                                    signature <- output (c_duckdb_v2_aggregate_function_get_signature function)
                                    withView "value" $ \name -> checked (c_duckdb_v2_function_signature_add_parameter signature name ty nullPtr DuckDBV2FunctionParameterKindStandard)
                                    checked (c_duckdb_v2_function_signature_set_return_type signature ty)
                                    checked (c_duckdb_v2_aggregate_function_set_size_callback function sizePtr)
                                    checked (c_duckdb_v2_aggregate_function_set_init_callback function initializePtr)
                                    checked (c_duckdb_v2_aggregate_function_set_update_callback function updatePtr)
                                    checked (c_duckdb_v2_aggregate_function_set_combine_callback function combinePtr)
                                    checked (c_duckdb_v2_aggregate_function_set_finalize_callback function finalizePtr)
                                    checked (c_duckdb_v2_aggregate_function_register function)
                            withResult conn "SELECT v2_sum(i::BIGINT) FROM range(12) t(i) GROUP BY i % 3 ORDER BY i % 3" $ \result ->
                                readIntegers result >>= (@?= [Just 18, Just 22, Just 26])
                            withResult conn "SELECT v2_sum(i::BIGINT) OVER () FROM range(4096) t(i) LIMIT 1" $ \result ->
                                readIntegers result >>= (@?= [Just 8386560])
    readIORef failures >>= (@?= [])
    readIORef combinations >>= assertBool "window execution combines aggregate states" . (> 0)

testCast :: IO ()
testCast = do
    failures <- newIORef []
    modes <- newIORef []
    let execute info context err = callbackGuard failures err $ do
            assertBool "cast context is borrowed and non-null" (context /= nullPtr)
            mode <- output (c_duckdb_v2_cast_function_exec_get_mode info)
            modifyIORef' modes (mode :)
            count <- output (c_duckdb_v2_cast_function_exec_get_row_count info)
            input <- output (c_duckdb_v2_cast_function_exec_get_input info)
            checked (c_duckdb_v2_vector_flatten input)
            view <- output (c_duckdb_v2_vector_get_view input)
            result <- output (c_duckdb_v2_cast_function_exec_get_output info)
            raw <- output (c_duckdb_v2_vector_get_data_mutable result)
            forM_ [0 .. fromIntegral count - 1] $ \i -> do
                index <- if duckdbV2VectorViewSel view == nullPtr then pure i else fromIntegral <$> peekElemOff (duckdbV2VectorViewSel view) i
                value <- peekElemOff (castPtr (duckdbV2VectorViewData view)) index
                pokeElemOff (castPtr raw) i (value + 100 :: Int64)
    bracket (mkDuckDBV2CastFunctionExecCallbackFn execute) freeHaskellFunPtr $ \executePtr ->
        withConnection $ \conn ->
            withHandle (c_duckdb_v2_connection_create_type_from_id conn DuckDBV2LogicalTypeIdBigint nullPtr nullPtr 0) c_duckdb_v2_logical_type_destroy $ \ty ->
                withView "v2_shifted" $ \name -> do
                    withHandle (c_duckdb_v2_custom_type_create_with_connection conn) c_duckdb_v2_custom_type_destroy $ \custom -> do
                        checked (c_duckdb_v2_custom_type_set_name custom name)
                        checked (c_duckdb_v2_custom_type_set_base_type custom ty)
                        checked (c_duckdb_v2_custom_type_register custom)
                    withHandle (c_duckdb_v2_connection_create_type_with_alias conn ty name) c_duckdb_v2_logical_type_destroy $ \alias ->
                        withHandle (c_duckdb_v2_cast_function_create_with_connection conn) c_duckdb_v2_cast_function_destroy $ \function -> do
                            checked (c_duckdb_v2_cast_function_set_source_type function ty)
                            checked (c_duckdb_v2_cast_function_set_target_type function alias)
                            checked (c_duckdb_v2_cast_function_set_exec_callback function executePtr)
                            checked (c_duckdb_v2_cast_function_register function)
                    withResult conn "SELECT CAST(i::BIGINT AS v2_shifted)::BIGINT FROM range(3) t(i)" $ \result -> readIntegers result >>= (@?= [Just 100, Just 101, Just 102])
                    withResult conn "SELECT TRY_CAST(i::BIGINT AS v2_shifted)::BIGINT FROM range(3) t(i)" $ \result -> readIntegers result >>= (@?= [Just 100, Just 101, Just 102])
    readIORef failures >>= (@?= [])
    readIORef modes >>= \seen -> assertBool "normal and try cast modes reach the callback" (DuckDBV2CastModeNormal `elem` seen && DuckDBV2CastModeTry `elem` seen)

testVectorMutation :: IO ()
testVectorMutation = withConnection $ \conn ->
    withHandle (c_duckdb_v2_connection_create_type_from_id conn DuckDBV2LogicalTypeIdBigint nullPtr nullPtr 0) c_duckdb_v2_logical_type_destroy $ \integer ->
        withHandle (c_duckdb_v2_connection_create_type_from_id conn DuckDBV2LogicalTypeIdVarchar nullPtr nullPtr 0) c_duckdb_v2_logical_type_destroy $ \text ->
            withArray [integer, text] $ \types ->
                withHandle (c_duckdb_v2_data_chunk_create_with_connection conn types 2) c_duckdb_v2_data_chunk_destroy $ \chunk -> do
                    vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                    withHandle (c_duckdb_v2_value_create_bigint_with_connection conn 7) c_duckdb_v2_value_destroy $ \value ->
                        checked (c_duckdb_v2_vector_make_constant vector value 4)
                    constant <- output (c_duckdb_v2_vector_get_view vector)
                    duckdbV2VectorViewCount constant @?= 4
                    forM_ [0 .. 3] $ \i -> peekElemOff (duckdbV2VectorViewSel constant) i >>= (@?= 0)
                    peek (castPtr (duckdbV2VectorViewData constant)) >>= (@?= (7 :: Int64))
                    checked (c_duckdb_v2_vector_make_sequence vector 10 3 4)
                    alloca $ \view -> do
                        c_duckdb_v2_vector_get_view vector view nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
                        invalid <- peek view
                        duckdbV2VectorViewData invalid @?= nullPtr
                        duckdbV2VectorViewCount invalid @?= 0
                    checked (c_duckdb_v2_vector_flatten vector)
                    flat <- output (c_duckdb_v2_vector_get_view vector)
                    forM [0 .. 3] (peekElemOff (castPtr (duckdbV2VectorViewData flat))) >>= (@?= ([10, 13, 16, 19] :: [Int64]))
                    checked (c_duckdb_v2_vector_set_null vector 1)
                    validity <- output (c_duckdb_v2_vector_flat_get_validity_mutable vector)
                    peek validity >>= \word -> assertBool "only the chosen row is null" (testBit word 0 && not (testBit word 1) && testBit word 2)
                    checked (c_duckdb_v2_vector_set_size vector 2)
                    strings <- output (c_duckdb_v2_data_chunk_get_vector chunk 1)
                    let values = ["", "a\0b", replicate 12 'x', replicate 13 'y', replicate 100 'z']
                    checked (c_duckdb_v2_vector_set_size strings (fromIntegral (length values)))
                    forM_ (zip [0 ..] values) $ \(i, value) -> withView value $ \input ->
                        withHandle (c_duckdb_v2_value_create_varchar_with_connection conn input) c_duckdb_v2_value_destroy $ \owned ->
                            checked (c_duckdb_v2_vector_set_value strings i owned)
                    stringView <- output (c_duckdb_v2_vector_get_view strings)
                    forM
                        (zip [0 ..] values)
                        ( \(i, value) -> do
                            storage <- peekElemOff (castPtr (duckdbV2VectorViewData stringView)) i
                            with storage $ \ptr -> do
                                len <- Helpers.c_duckdb_string_t_length ptr
                                len @?= duckDBStringTLength storage
                                len @?= fromIntegral (length value)
                                inlineFlag <- Helpers.c_duckdb_string_is_inlined ptr
                                inlineFlag @?= if len <= duckdbV2BytesInlineLength then CBool 1 else CBool 0
                                bytes <- if len <= duckdbV2BytesInlineLength then pure (duckdbV2BytesInlinePointer ptr) else duckdbV2BytesPointer ptr
                                Helpers.c_duckdb_string_t_data ptr >>= (@?= bytes)
                                peekCStringLen (bytes, fromIntegral len)
                        )
                        >>= (@?= values)
                    arena <- output (c_duckdb_v2_vector_get_arena strings)
                    allocated <- output (c_duckdb_v2_arena_allocate arena 100)
                    fillBytes allocated 120 100
                    peekCStringLen (castPtr allocated, 100) >>= (@?= replicate 100 'x')

testOptions :: IO ()
testOptions =
    withHandle c_duckdb_v2_environment_create c_duckdb_v2_environment_destroy $ \env ->
        withHandle (c_duckdb_v2_instance_create env) c_duckdb_v2_instance_destroy $ \instanceHandle -> do
            withView "threads" $ \name -> withView "2" $ \setting ->
                checked (c_duckdb_v2_instance_set_option instanceHandle name setting)
            withView ":memory:" $ \path -> checked (c_duckdb_v2_instance_attach instanceHandle path nullPtr nullPtr (CBool 1))
            withHandle (c_duckdb_v2_connection_create instanceHandle) c_duckdb_v2_connection_destroy $ \first ->
                withHandle (c_duckdb_v2_connection_create instanceHandle) c_duckdb_v2_connection_destroy $ \second ->
                    withView "TimeZone" $ \name -> do
                        withView "Europe/Berlin" $ \setting -> checked (c_duckdb_v2_connection_set_option first name setting DuckDBV2SettingScopeGlobal)
                        withView "UTC" $ \setting -> checked (c_duckdb_v2_connection_set_option first name setting DuckDBV2SettingScopeLocal)
                        withHandle (c_duckdb_v2_connection_get_option_by_name first name) c_duckdb_v2_option_destroy $ \option ->
                            output (c_duckdb_v2_option_get_setting option) >>= readView >>= (@?= "UTC")
                        withHandle (c_duckdb_v2_connection_get_option_by_name second name) c_duckdb_v2_option_destroy $ \option ->
                            output (c_duckdb_v2_option_get_setting option) >>= readView >>= (@?= "Europe/Berlin")
                        withResult first "SELECT EXTRACT(hour FROM TIMESTAMPTZ '2000-01-01 00:00:00+00')::BIGINT" $ \result -> readIntegers result >>= (@?= [Just 0])
                        withResult second "SELECT EXTRACT(hour FROM TIMESTAMPTZ '2000-01-01 00:00:00+00')::BIGINT" $ \result -> readIntegers result >>= (@?= [Just 1])

testCopy :: IO ()
testCopy = do
    (path, handle) <- openBinaryTempFile "." "duckdb-v2-copy"
    hClose handle
    withCString path $ \name -> removeTestFile name >>= (@?= 0)
    failures <- newIORef []
    destroyed <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
    prepared <- newIORef (0 :: Int)
    flushed <- newIORef (0 :: Int64)
    finalized <- newIORef (0 :: Int)
    let destroyState ptr = do
            marker <- peek (castPtr ptr :: Ptr Int64)
            atomicModifyIORef' destroyed $ \(bound, initialized, batches) ->
                ( if marker == -1 then (bound + 1, initialized, batches) else if marker == -2 then (bound, initialized + 1, batches) else (bound, initialized, batches + 1)
                , ()
                )
            free ptr
    bracket (mkDuckDBV2OpaqueDestroyFn destroyState) freeHaskellFunPtr $ \destroyPtr -> do
        let setState setter marker = do
                state <- malloc
                poke state (marker :: Int64)
                with (DuckDBV2Opaque (castPtr state) destroyPtr nullFunPtr) $ \opaque -> checked (setter opaque)
            bind info _context err = callbackGuard failures err $ do
                output (c_duckdb_v2_copy_to_bind_get_column_count info) >>= (@?= 1)
                withHandle (c_duckdb_v2_copy_to_bind_get_column_type info 0) c_duckdb_v2_logical_type_destroy $ \ty ->
                    output (c_duckdb_v2_logical_type_get_id ty) >>= (@?= DuckDBV2LogicalTypeIdBigint)
                setState (c_duckdb_v2_copy_to_bind_set_bind_data info) (-1)
            initialize info _context err = callbackGuard failures err $ do
                bound <- output (c_duckdb_v2_copy_to_init_get_bind_data info)
                peek (castPtr bound) >>= (@?= (-1 :: Int64))
                setState (c_duckdb_v2_copy_to_init_set_init_data info) (-2)
            batch info _context err = callbackGuard failures err $
                withHandle (c_duckdb_v2_copy_to_batch_take_input info) c_duckdb_v2_column_data_collection_destroy $ \input -> do
                    atomicModifyIORef' prepared $ \count -> (count + 1, ())
                    count <- output (c_duckdb_v2_column_data_collection_row_count input)
                    alloca $ \second -> do
                        poke second nullPtr
                        c_duckdb_v2_copy_to_batch_take_input info second nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
                        peek second >>= (@?= nullPtr)
                    setState (c_duckdb_v2_copy_to_batch_set_batch_data info) (fromIntegral count)
            flush info _context err = callbackGuard failures err $ do
                initialized <- output (c_duckdb_v2_copy_to_flush_get_init_data info)
                peek (castPtr initialized) >>= (@?= (-2 :: Int64))
                state <- output (c_duckdb_v2_copy_to_flush_get_batch_data info)
                count <- peek (castPtr state)
                atomicModifyIORef' flushed $ \total -> (total + count, ())
            finalize info _context err = callbackGuard failures err $ do
                initialized <- output (c_duckdb_v2_copy_to_finalize_get_init_data info)
                peek (castPtr initialized) >>= (@?= (-2 :: Int64))
                readIORef flushed >>= (@?= 5)
                modifyIORef' finalized (+ 1)
        bracket (mkDuckDBV2CopyToBindCallbackFn bind) freeHaskellFunPtr $ \bindPtr ->
            bracket (mkDuckDBV2CopyToInitCallbackFn initialize) freeHaskellFunPtr $ \initializePtr ->
                bracket (mkDuckDBV2CopyToBatchCallbackFn batch) freeHaskellFunPtr $ \batchPtr ->
                    bracket (mkDuckDBV2CopyToFlushCallbackFn flush) freeHaskellFunPtr $ \flushPtr ->
                        bracket (mkDuckDBV2CopyToFinalizeCallbackFn finalize) freeHaskellFunPtr $ \finalizePtr ->
                            withConnection $ \conn -> do
                                withHandle (c_duckdb_v2_copy_function_create_with_connection conn) c_duckdb_v2_copy_function_destroy $ \function -> do
                                    withView "v2_count" $ \name -> checked (c_duckdb_v2_copy_function_set_name function name)
                                    checked (c_duckdb_v2_copy_to_set_bind_callback function bindPtr)
                                    checked (c_duckdb_v2_copy_to_set_init_callback function initializePtr)
                                    checked (c_duckdb_v2_copy_to_set_batch_callback function batchPtr)
                                    checked (c_duckdb_v2_copy_to_set_flush_callback function flushPtr)
                                    checked (c_duckdb_v2_copy_to_set_finalize_callback function finalizePtr)
                                    checked (c_duckdb_v2_copy_function_register function)
                                withResult conn ("COPY (SELECT i::BIGINT FROM range(5) t(i)) TO '" <> path <> "' (FORMAT v2_count, USE_TMP_FILE FALSE)") $ \result ->
                                    readIntegers result >>= (@?= [Just 5])
    readIORef failures >>= (@?= [])
    batches <- readIORef prepared
    assertBool "COPY prepared a batch" (batches > 0)
    readIORef destroyed >>= (@?= (1, 1, batches))
    readIORef finalized >>= (@?= 1)

-- | Remove the temporary file through the C standard library.
foreign import ccall unsafe "remove"
    removeTestFile :: CString -> IO CInt

testFileSystem :: IO ()
testFileSystem =
    bracket
        ( do
            (path, handle) <- openBinaryTempFile "." "duckdb-v2-file"
            hClose handle
            pure path
        )
        (\path -> withCString path $ \name -> removeTestFile name >>= (@?= 0))
        ( \path -> withConnection $ \conn -> do
            fileSystem <- output (c_duckdb_v2_file_system_get_from_connection conn)
            withHandle (c_duckdb_v2_file_open_options_create fileSystem) c_duckdb_v2_file_open_options_destroy $ \options -> do
                checked (c_duckdb_v2_file_open_options_set_flag options DuckDBV2FileFlagRead)
                checked (c_duckdb_v2_file_open_options_set_flag options DuckDBV2FileFlagWrite)
                withView path $ \name ->
                    withHandle (c_duckdb_v2_file_system_open fileSystem name options) c_duckdb_v2_file_destroy $ \file ->
                        withCStringLen "alpha\0omega" $ \(bytes, len) -> allocaBytes (len + 1) $ \buffer -> do
                            checked (c_duckdb_v2_file_write_at file (castPtr bytes) (fromIntegral len) 0)
                            output (c_duckdb_v2_file_tell file) >>= (@?= 0)
                            output (c_duckdb_v2_file_size file) >>= (@?= fromIntegral len)
                            checked (c_duckdb_v2_file_read_at file buffer (fromIntegral len) 0)
                            peekCStringLen (castPtr buffer, len) >>= (@?= "alpha\0omega")
                            output (c_duckdb_v2_file_tell file) >>= (@?= 0)
                            output (c_duckdb_v2_file_read file buffer (fromIntegral len)) >>= (@?= fromIntegral len)
                            withCString "!" $ \suffix -> output (c_duckdb_v2_file_write file (castPtr suffix) 1) >>= (@?= 1)
                            output (c_duckdb_v2_file_tell file) >>= (@?= fromIntegral (len + 1))
                            checked (c_duckdb_v2_file_sync file)
                            checked (c_duckdb_v2_file_seek file 0)
                            output (c_duckdb_v2_file_read file buffer (fromIntegral (len + 1))) >>= (@?= fromIntegral (len + 1))
                            peekCStringLen (castPtr buffer, len + 1) >>= (@?= "alpha\0omega!")
                            output (c_duckdb_v2_file_read file buffer 1) >>= (@?= 0)
                            checked (c_duckdb_v2_file_close file)
        )

testLogging :: IO ()
testLogging = do
    failures <- newIORef []
    let consume result = alloca $ \chunk -> do
            poke chunk nullPtr
            checked (c_duckdb_v2_result_fetch_chunk result chunk)
            next <- peek chunk
            unless (next == nullPtr) $ do
                c_duckdb_v2_data_chunk_destroy chunk >>= (@?= DuckDBV2ErrorNone)
                consume result
        execute info context err = callbackGuard failures err $ do
            withView "" $ \logType -> withView "v2 callback message" $ \message ->
                checked (c_duckdb_v2_context_log context DuckDBV2LogLevelInfo logType message)
            vector <- output (c_duckdb_v2_scalar_function_exec_get_result info)
            raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
            poke (castPtr raw) (1 :: Int64)
    bracket (mkDuckDBV2ScalarFunctionExecCallbackFn execute) freeHaskellFunPtr $ \executePtr ->
        withConnection $ \conn -> do
            withHandle (c_duckdb_v2_connection_create_type_from_id conn DuckDBV2LogicalTypeIdBigint nullPtr nullPtr 0) c_duckdb_v2_logical_type_destroy $ \ty ->
                withHandle (c_duckdb_v2_scalar_function_create_with_connection conn) c_duckdb_v2_scalar_function_destroy $ \function -> do
                    withView "v2_log" $ \name -> checked (c_duckdb_v2_scalar_function_set_name function name)
                    signature <- output (c_duckdb_v2_scalar_function_get_signature function)
                    checked (c_duckdb_v2_function_signature_set_return_type signature ty)
                    checked (c_duckdb_v2_scalar_function_set_exec_callback function executePtr)
                    checked (c_duckdb_v2_scalar_function_register function)
            withResult conn "CALL enable_logging()" consume
            withResult conn "SET logging_level='info'" consume
            withResult conn "SELECT v2_log()" $ \result -> readIntegers result >>= (@?= [Just 1])
            withResult conn "SELECT count(*)::BIGINT FROM duckdb_logs WHERE message='v2 callback message'" $ \result -> readIntegers result >>= (@?= [Just 1])
            withResult conn "CALL disable_logging()" consume
            withResult conn "SELECT v2_log()" $ \result -> readIntegers result >>= (@?= [Just 1])
            withResult conn "SELECT count(*)::BIGINT FROM duckdb_logs WHERE message='v2 callback message'" $ \result -> readIntegers result >>= (@?= [Just 1])
    readIORef failures >>= (@?= [])

testTextSink :: IO ()
testTextSink = do
    received <- newIORef []
    failures <- newIORef []
    let callback text _user err = callbackGuard failures err $ do
            rendered <- peek text >>= readView
            modifyIORef' received (rendered :)
    bracket (mkDuckDBV2TextSinkFn callback) freeHaskellFunPtr $ \sink -> withConnection $ \conn ->
        withStatement conn "SELECT 42::BIGINT AS answer" $ \statement -> alloca $ \result -> do
            poke result nullPtr
            checked (c_duckdb_v2_statement_execute conn statement nullPtr nullPtr 0 result)
            original <- peek result
            assertBool "execution returns a live result" (original /= nullPtr)
            flip finally (c_duckdb_v2_result_destroy result >>= (@?= DuckDBV2ErrorNone)) $
                withView "" $ \nullText -> do
                    alloca $ \err -> do
                        poke err nullPtr
                        flip finally (c_duckdb_v2_error_info_destroy err >>= (@?= DuckDBV2ErrorNone)) $ do
                            code <- c_duckdb_v2_result_render_box result 0 80 20 nullText 2 0 sink nullPtr err
                            code @?= DuckDBV2ErrorInputInvalid
                            peek result >>= (@?= original)
                            readIORef received >>= (@?= [])
                    checked (c_duckdb_v2_result_render_box result 0 80 20 nullText 0 0 sink nullPtr)
                    peek result >>= (@?= nullPtr)
    readIORef failures >>= (@?= [])
    rendered <- readIORef received
    length rendered @?= 1
    assertBool "rendered box contains its column and value" (all (\text -> "answer" `isInfixOf` text && "42" `isInfixOf` text) rendered)

testArrow :: IO ()
testArrow = withConnection $ \conn -> withStatement conn "SELECT i::BIGINT AS amount FROM range(5) t(i)" $ \statement -> alloca $ \result -> alloca $ \streamPtr -> do
    poke result nullPtr
    checked (c_duckdb_v2_statement_execute_arrow conn statement nullPtr nullPtr 0 0 result)
    fillBytes streamPtr 0 (sizeOf (undefined :: DuckDBV2ArrowArrayStream))
    checked (c_duckdb_v2_arrow_result_to_arrow_c_stream result streamPtr)
    peek result >>= (@?= nullPtr)
    stream <- peek streamPtr
    assertBool "Arrow stream owns a release callback" (arrowStreamRelease stream /= nullFunPtr)
    flip finally (callDuckDBV2ArrowArrayStreamReleaseFn (arrowStreamRelease stream) streamPtr) $ do
        alloca $ \schemaPtr -> do
            fillBytes schemaPtr 0 (sizeOf (undefined :: DuckDBV2ArrowSchema))
            callDuckDBV2ArrowArrayStreamGetSchemaFn (arrowStreamGetSchema stream) streamPtr schemaPtr >>= (@?= 0)
            schema <- peek schemaPtr
            arrowSchemaChildCount schema @?= 1
            callDuckDBV2ArrowSchemaReleaseFn (arrowSchemaRelease schema) schemaPtr
        let fetch total = alloca $ \arrayPtr -> do
                fillBytes arrayPtr 0 (sizeOf (undefined :: DuckDBV2ArrowArray))
                callDuckDBV2ArrowArrayStreamGetNextFn (arrowStreamGetNext stream) streamPtr arrayPtr >>= (@?= 0)
                array <- peek arrayPtr
                if arrowArrayRelease array == nullFunPtr
                    then pure total
                    else do
                        let count = arrowArrayLength array
                        callDuckDBV2ArrowArrayReleaseFn (arrowArrayRelease array) arrayPtr
                        fetch (total + count)
        fetch 0 >>= (@?= 5)
