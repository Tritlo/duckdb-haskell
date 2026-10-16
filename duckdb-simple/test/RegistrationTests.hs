{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Native integration checks for managed V2 registration.
module RegistrationTests (registrationTests) where

import Control.Exception (bracket, throwIO, try)
import Control.Monad (forM, forM_, void, when)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import qualified Data.Text as Text
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple hiding (bind, execute)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..))
import qualified Database.DuckDB.Simple.Registration as R
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

-- | Check registration, callback errors, typed signatures, and resource release.
registrationTests :: TestTree
registrationTests =
    testGroup
        "V2 registration"
        [ testCase "scalar named defaults and all parameter kinds" testScalar
        , testCase "aggregate grouped and window execution" testAggregate
        , testCase "table state and typed default parameter" testTable
        , testCase "custom type and normal and try cast" testCast
        , testCase "connection scans precede instance scans" testReplacement
        , testCase "callback failures become SQL errors" testErrors
        , testCase "callback closures and query states are released" testRelease
        ]

bigint :: R.TypeDeclaration
bigint = R.StructuredType (LogicalTypeScalar DuckDBV2LogicalTypeIdBigInt)

scalarSignature :: [R.FunctionParameter] -> R.FunctionSignature
scalarSignature parameters = R.FunctionSignature parameters (Just bigint)

scalarSum :: R.ContextCallback DuckDBV2ScalarFunctionExecInfoHandle
scalarSum info _ = do
    count <- R.output (c_duckdb_v2_scalar_function_exec_get_row_count info)
    arguments <- allocaArray 4 \counts -> do
        R.checked (c_duckdb_v2_scalar_function_exec_get_arg_count info counts (advancePtr counts 1) (advancePtr counts 2) (advancePtr counts 3))
        fromIntegral . sum <$> peekArray 4 counts
    columns <- forM [0 .. arguments - 1 :: Int] \index ->
        R.output (c_duckdb_v2_scalar_function_exec_get_arg info (fromIntegral index)) >>= readIntegers (fromIntegral count)
    vector <- R.output (c_duckdb_v2_scalar_function_exec_get_result info)
    writeIntegers vector [sum [column !! row | column <- columns] | row <- [0 .. fromIntegral count - 1]]

testScalar :: Assertion
testScalar = withConnection ":memory:" \connection -> do
    let left = (R.parameter "left" bigint){R.parameterDefault = Just (R.defaultValue (7 :: Int64))}
        right = (R.parameter "right" bigint){R.parameterDefault = Just (R.defaultValue (35 :: Int64)), R.parameterKind = DuckDBV2FunctionParameterKindNamedOnly}
    R.registerScalarFunction connection "managed_default" (scalarSignature [left, right]) [] (R.scalarCallbacks scalarSum)
    query_ connection "SELECT managed_default()" >>= (@?= [Only (42 :: Int64)])
    query_ connection "SELECT managed_default(right := 5, left := 3)" >>= (@?= [Only (8 :: Int64)])
    let positional = (R.parameter "first" bigint){R.parameterKind = DuckDBV2FunctionParameterKindPositionalOnly}
        standard = R.parameter "second" bigint
        rest = (R.parameter "rest" bigint){R.parameterKind = DuckDBV2FunctionParameterKindPositionalVariadic}
        named = (R.parameter "named" bigint){R.parameterKind = DuckDBV2FunctionParameterKindNamedOnly}
        kwargs = (R.parameter "kwargs" bigint){R.parameterKind = DuckDBV2FunctionParameterKindNamedVariadic}
    R.registerScalarFunction connection "managed_variadic" (scalarSignature [positional, standard, rest, named, kwargs]) [] (R.scalarCallbacks scalarSum)
    query_ connection "SELECT managed_variadic(1, 2, 3, 4, named := 5, extra := 6)" >>= (@?= [Only (21 :: Int64)])

sumCallbacks :: R.AggregateCallbacks
sumCallbacks = R.aggregateCallbacks stateSize initialize update combine finalize
  where
    stateSize info = R.checked (c_duckdb_v2_aggregate_function_size_set_state_size info (fromIntegral (sizeOf (0 :: Int64))))
    initialize info = do
        count <- R.output (c_duckdb_v2_aggregate_function_init_get_state_count info)
        states <- R.output (c_duckdb_v2_aggregate_function_init_get_states info)
        forM_ [0 .. fromIntegral count - 1] \index -> peekElemOff states index >>= \state -> poke (castPtr state) (0 :: Int64)
    update info = do
        count <- R.output (c_duckdb_v2_aggregate_function_update_get_row_count info)
        states <- R.output (c_duckdb_v2_aggregate_function_update_get_states info)
        values <- R.output (c_duckdb_v2_aggregate_function_update_get_arg info 0) >>= readIntegers (fromIntegral count)
        forM_ (zip [0 ..] values) \(index, value) -> do
            state <- castPtr <$> peekElemOff states index
            total <- peek state
            poke state (total + value :: Int64)
    combine info = do
        count <- R.output (c_duckdb_v2_aggregate_function_combine_get_state_count info)
        sources <- R.output (c_duckdb_v2_aggregate_function_combine_get_sources info)
        targets <- R.output (c_duckdb_v2_aggregate_function_combine_get_targets info)
        forM_ [0 .. fromIntegral count - 1] \index -> do
            source <- peekElemOff sources index >>= peek . castPtr
            target <- castPtr <$> peekElemOff targets index
            total <- peek target
            poke target (total + source :: Int64)
    finalize info = do
        count <- R.output (c_duckdb_v2_aggregate_function_finalize_get_state_count info)
        states <- R.output (c_duckdb_v2_aggregate_function_finalize_get_states info)
        offset <- R.output (c_duckdb_v2_aggregate_function_finalize_get_result_offset info)
        result <- R.output (c_duckdb_v2_aggregate_function_finalize_get_result info)
        raw <- R.output (c_duckdb_v2_vector_get_data_mutable result)
        forM_ [0 .. fromIntegral count - 1] \index -> do
            total <- peekElemOff states index >>= peek . castPtr
            pokeElemOff (castPtr raw) (fromIntegral offset + index) (total :: Int64)

testAggregate :: Assertion
testAggregate = withConnection ":memory:" \connection -> do
    destroyed <- newIORef (0 :: Int)
    let callbacks = sumCallbacks{R.aggregateDestroyCallback = Just (\_ -> modifyIORef' destroyed (+ 1))}
    R.registerAggregateFunction connection "managed_sum" (scalarSignature [R.parameter "value" bigint]) [] callbacks
    query_ connection "SELECT managed_sum(i::BIGINT) FROM range(12) t(i) GROUP BY i % 3 ORDER BY i % 3" >>= (@?= map Only ([18, 22, 26] :: [Int64]))
    query_ connection "SELECT managed_sum(i::BIGINT) OVER () FROM range(4096) t(i) LIMIT 1" >>= (@?= [Only (8386560 :: Int64)])
    readIORef destroyed >>= assertBool "aggregate state destruction ran" . (> 0)

tableDefinition :: R.TableCallbacks
tableDefinition = (R.tableCallbacks bind execute){R.tableInitGlobalCallback = Just initialize}
  where
    bind input result context = do
        amount <- bracket (R.output (c_duckdb_v2_function_bind_get_arg_value input 0)) (R.destroy c_duckdb_v2_value_destroy) \value ->
            R.output (c_duckdb_v2_value_get_bigint value)
        R.setCallbackState (c_duckdb_v2_function_bind_set_bind_data input) (amount :: Int64)
        bracket
            (R.output (c_duckdb_v2_context_create_type_from_id context DuckDBV2LogicalTypeIdBigInt (ConstPtr nullPtr) (ConstPtr nullPtr) 0))
            (R.destroy c_duckdb_v2_logical_type_destroy)
            \dtype -> R.withIdentifier "amount" \name -> R.checked (c_duckdb_v2_table_function_bind_add_result_column result name dtype)
    initialize info _ = do
        amount <- R.getCallbackState (c_duckdb_v2_table_function_init_global_get_bind_data info)
        state <- newIORef (Just (amount :: Int64))
        R.setCallbackState (c_duckdb_v2_table_function_init_global_set_global_state info) state
    execute info _ = do
        state <- R.getCallbackState (c_duckdb_v2_table_function_exec_get_global_state info) :: IO (IORef (Maybe Int64))
        amount <- atomicModifyIORef' state (\value -> (Nothing, value))
        chunk <- R.output (c_duckdb_v2_table_function_exec_get_output_chunk info)
        vector <- R.output (c_duckdb_v2_data_chunk_get_vector chunk 0)
        writeIntegers vector (maybe [] (\count -> [42 .. 41 + count]) amount)

testTable :: Assertion
testTable = withConnection ":memory:" \connection -> do
    let count = (R.parameter "count" bigint){R.parameterDefault = Just (R.defaultValue (3 :: Int64))}
    R.registerTableFunction connection "managed_scan" [count] tableDefinition
    query_ connection "SELECT amount FROM managed_scan()" >>= (@?= map Only ([42, 43, 44] :: [Int64]))
    query_ connection "SELECT amount FROM managed_scan(count := 1)" >>= (@?= [Only (42 :: Int64)])

testCast :: Assertion
testCast = withConnection ":memory:" \connection -> do
    R.registerCustomType connection "managed_shifted" bigint
    modes <- newIORef []
    R.registerCast connection bigint (R.TypeExpression "managed_shifted") Nothing \info _ -> do
        mode <- R.output (c_duckdb_v2_cast_function_exec_get_mode info)
        modifyIORef' modes (mode :)
        count <- R.output (c_duckdb_v2_cast_function_exec_get_row_count info)
        values <- R.output (c_duckdb_v2_cast_function_exec_get_input info) >>= readIntegers (fromIntegral count)
        result <- R.output (c_duckdb_v2_cast_function_exec_get_output info)
        writeIntegers result (map (+ 100) values)
    query_ connection "SELECT CAST(i::BIGINT AS managed_shifted)::BIGINT FROM range(3) t(i)" >>= (@?= map Only ([100, 101, 102] :: [Int64]))
    query_ connection "SELECT TRY_CAST(7::BIGINT AS managed_shifted)::BIGINT" >>= (@?= [Only (107 :: Int64)])
    seen <- readIORef modes
    assertBool "normal cast" (DuckDBV2CastModeNormal `elem` seen)
    assertBool "try cast" (DuckDBV2CastModeTry `elem` seen)

replacement :: Int64 -> R.ContextCallback DuckDBV2ReplacementScanInfoHandle
replacement number info _ =
    bracket (R.output (c_duckdb_v2_replacement_scan_get_name info)) (R.destroy c_duckdb_v2_qname_destroy) \name -> do
        count <- R.output (c_duckdb_v2_qname_get_part_count name)
        when (count == 1) do
            part <- R.output (c_duckdb_v2_qname_get_part name 0) >>= R.readIdentifier
            when (part == "managed_missing")
                $ R.withText ("SELECT " <> Text.pack (show number) <> "::BIGINT AS amount")
                $ R.checked . c_duckdb_v2_replacement_scan_set_subquery info

testReplacement :: Assertion
testReplacement = withEnvironment \environment -> withInstance environment [] \instance' -> do
    R.registerInstanceReplacementScan instance' (replacement 7)
    withConnectionOn instance' \first -> withConnectionOn instance' \second -> do
        R.registerReplacementScan first (replacement 42)
        query_ first "SELECT amount FROM managed_missing" >>= (@?= [Only (42 :: Int64)])
        query_ second "SELECT amount FROM managed_missing" >>= (@?= [Only (7 :: Int64)])

testErrors :: Assertion
testErrors = withConnection ":memory:" \connection -> do
    let failure = throwIO (userError "managed callback failure")
    R.registerScalarFunction connection "managed_error" (scalarSignature []) [] (R.scalarCallbacks (\_ _ -> failure))
    assertSqlError (query_ connection "SELECT managed_error()" :: IO [Only Int64])
    R.registerAggregateFunction connection "managed_error_sum" (scalarSignature [R.parameter "value" bigint]) [] sumCallbacks{R.aggregateUpdateCallback = \_ -> failure}
    assertSqlError (query_ connection "SELECT managed_error_sum(i::BIGINT) FROM range(3) t(i)" :: IO [Only Int64])
    R.registerTableFunction connection "managed_error_table" [] (R.tableCallbacks (\_ _ _ -> failure) (\_ _ -> pure ()))
    assertSqlError (query_ connection "SELECT * FROM managed_error_table()" :: IO [Only Int64])
    R.registerReplacementScan connection (\_ _ -> failure)
    assertSqlError (query_ connection "SELECT * FROM managed_unknown" :: IO [Only Int64])

testRelease :: Assertion
testRelease = do
    references <- withConnection ":memory:" \connection -> do
        closure <- newIORef (42 :: Int64)
        closureWeak <- mkWeakIORef closure (pure ())
        states <- newIORef []
        let initialize info _ = do
                state <- newIORef (1 :: Int64)
                weak <- mkWeakIORef state (pure ())
                modifyIORef' states (weak :)
                R.setCallbackState (c_duckdb_v2_scalar_function_init_set_init_data info) state
            execute info _ = do
                state <- R.getCallbackState (c_duckdb_v2_scalar_function_exec_get_init_data info) :: IO (IORef Int64)
                increment <- readIORef state
                value <- (+ increment) <$> readIORef closure
                result <- R.output (c_duckdb_v2_scalar_function_exec_get_result info)
                writeIntegers result [value]
        R.registerScalarFunction connection "managed_release" (scalarSignature []) [] (R.scalarCallbacks execute){R.scalarInitCallback = Just initialize}
        query_ connection "SELECT managed_release()" >>= (@?= [Only (43 :: Int64)])
        stateRefs <- readIORef states
        assertBool "state initialized" (not (null stateRefs))
        failed <- failedRegistration connection
        pure (closureWeak : failed : stateRefs)
    performMajorGC
    performMajorGC
    forM_ references \reference -> deRefWeak reference >>= assertBool "native callback state or closure remains live" . isNothing

failedRegistration :: Connection -> IO (Weak (IORef Int64))
failedRegistration connection = do
    retained <- newIORef (1 :: Int64)
    reference <- mkWeakIORef retained (pure ())
    let execute _ _ = void (readIORef retained)
        duplicate = R.parameter "duplicate" bigint
    result <- try (R.registerScalarFunction connection "managed_invalid" (scalarSignature [duplicate, duplicate]) [] (R.scalarCallbacks execute))
    case result of
        Left (_ :: SQLError) -> pure ()
        Right () -> assertFailure "duplicate parameter names must fail"
    pure reference

assertSqlError :: IO a -> Assertion
assertSqlError action = do
    outcome <- try (void action)
    case outcome of
        Left (err :: SQLError) -> assertBool "callback diagnostic" ("managed callback failure" `Text.isInfixOf` sqlErrorMessage err)
        Right () -> assertFailure "expected SQL error"

readIntegers :: Int -> DuckDBV2VectorHandle -> IO [Int64]
readIntegers count vector = do
    R.checked (c_duckdb_v2_vector_flatten vector)
    raw <- R.output (c_duckdb_v2_vector_get_data_mutable vector)
    mapM (peekElemOff (castPtr raw)) [0 .. count - 1]

writeIntegers :: DuckDBV2VectorHandle -> [Int64] -> IO ()
writeIntegers vector values = do
    R.checked (c_duckdb_v2_vector_set_size vector (fromIntegral (length values)))
    R.checked (c_duckdb_v2_vector_flatten vector)
    raw <- R.output (c_duckdb_v2_vector_get_data_mutable vector)
    forM_ (zip [0 ..] values) \(index, value) -> pokeElemOff (castPtr raw) index value
