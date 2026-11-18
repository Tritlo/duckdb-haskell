{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : RegistrationTests
-- Description : Native integration checks for managed V2 registration.
module RegistrationTests (registrationTests) where

import Control.Exception (IOException, bracket, evaluate, throwIO, try)
import Control.Monad (forM, forM_, void, when)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple hiding (bind, execute)
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Generic (DuckValue (..), ViaDuckDB (..))
import Database.DuckDB.Simple.Internal (withBytes, withConnectionFactory, withConnectionHandle)
import qualified Database.DuckDB.Simple.Logging as Logging
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructValue, UnionValue)
import qualified Database.DuckDB.Simple.QueryMetrics as Metrics
import qualified Database.DuckDB.Simple.V2.FunctionDocs as Docs
import qualified Database.DuckDB.Simple.V2.Logging as V2Logging
import qualified Database.DuckDB.Simple.V2.Pragma as Pragma
import qualified Database.DuckDB.Simple.V2.Registration as R
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray)
import Foreign.Ptr (castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff)
import GHC.Generics (Generic)
import HsBindgen.Runtime.Marshal (staticSizeOf)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, (@?=))
import TestUtils (assertSqlErrorContaining, writeIntegers, writeNullRows)

-- | Check registration, callback errors, typed signatures, and resource release.
registrationTests :: TestTree
registrationTests =
  testGroup
    "V2 registration"
    [ testCase "scalar named defaults and all parameter kinds" testScalar
    , testCase "scalar type resolution bind state and documentation" testScalarResolution
    , testCase "pragma query execution errors and documentation" testPragma
    , testCase "pragma names must be nonempty and must not contain NUL" testPragmaNames
    , testCase "query metric snapshots copy values and survive later queries" testMetrics
    , testCase "aggregate grouped and window execution" testAggregate
    , testCase "aggregate registration cleanup follows callback ownership" testAggregateCleanup
    , testCase "table state and typed default parameter" testTable
    , testCase "custom type and normal and try cast" testCast
    , testCase "a composite value with a member of a custom type binds with no registered cast" testCastAliasMembers
    , testCase "a second cast for the same pair of types has no effect" testCastSecond
    , testGroup "a failed cast gives a Mismatch Type error and keeps the database usable" $
        [testCase (castPairName castPair) (testCastFailureCode castPair) | castPair <- castFailurePairs]
          <> [ testCase "a query on another connection fails in the callback" testCastFailureQuery
             , testCase "an SQLError of the type Conversion, Invalid Input, or Out of Range in the callback" testCastTypedErrors
             ]
    , testCase "a COMMIT after a failed cast stores no row" testCastFailureCommit
    , testCase "connection scans precede instance scans" testReplacement
    , testCase "callback contexts write log entries" testContextLog
    , testCase "callback failures become SQL errors" testErrors
    , testCase "a native error text that is not valid UTF-8 gives a readable SQL error" testInvalidErrorText
    , testCase "callback closures and query states are released" testRelease
    ]

bigint :: R.TypeDeclaration
bigint = R.StructuredType (LogicalTypeScalar LogicalTypeIdBigInt)

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
      right =
        (R.parameter "right" bigint)
          { R.parameterDefault = Just (R.DefaultValue (\factory -> R.output (c_duckdb_v2_value_create_bigint factory 35)))
          , R.parameterKind = DuckDBV2FunctionParameterKindNamedOnly
          }
  R.registerScalarFunction connection "managed_default" (scalarSignature [left, right]) [] (R.scalarCallbacks scalarSum)
  query_ connection "SELECT managed_default()" >>= (@?= [Only (42 :: Int64)])
  query_ connection "SELECT managed_default(right := 5, left := 3)" >>= (@?= [Only (8 :: Int64)])
  let target = R.ConnectionTarget connection
  R.withRegistrationFactory target \factory ->
    R.registerScalarFunctionWith
      target
      ( \function -> do
          R.withIdentifier "managed_target_default" (R.checked . c_duckdb_v2_scalar_function_set_name function)
          R.output (c_duckdb_v2_scalar_function_get_signature function) >>= R.configureFunctionSignature factory (scalarSignature [left])
      )
      (R.scalarCallbacks scalarSum)
  query_ connection "SELECT managed_target_default()" >>= (@?= [Only (7 :: Int64)])
  let positional = (R.parameter "first" bigint){R.parameterKind = DuckDBV2FunctionParameterKindPositionalOnly}
      standard = R.parameter "second" bigint
      rest = (R.parameter "rest" bigint){R.parameterKind = DuckDBV2FunctionParameterKindPositionalVariadic}
      named = (R.parameter "named" bigint){R.parameterKind = DuckDBV2FunctionParameterKindNamedOnly}
      kwargs = (R.parameter "kwargs" bigint){R.parameterKind = DuckDBV2FunctionParameterKindNamedVariadic}
  R.registerScalarFunction
    connection
    "managed_variadic"
    (scalarSignature [positional, standard, rest, named, kwargs])
    []
    (R.scalarCallbacks scalarSum)
  query_ connection "SELECT managed_variadic(1, 2, 3, 4, named := 5, extra := 6)" >>= (@?= [Only (21 :: Int64)])

testScalarResolution :: Assertion
testScalarResolution = withConnection ":memory:" \connection -> do
  phases <- newIORef ([] :: [Text.Text])
  let mark phase = modifyIORef' phases (phase :)
      resolve input result _ = do
        mark "resolve"
        bracket (R.output (c_duckdb_v2_function_bind_get_arg_type input 0)) (R.destroy c_duckdb_v2_logical_type_destroy) $ \dtype ->
          R.checked (c_duckdb_v2_scalar_function_resolve_types_set_return_type result dtype)
      bindState _ result _ = do
        mark "bind"
        R.setCallbackState (c_duckdb_v2_scalar_function_bind_set_bind_data result) (42 :: Int)
      executeRows info _ = do
        state <- R.getCallbackState (c_duckdb_v2_scalar_function_exec_get_bind_data info)
        (state :: Int) @?= 42
        mismatch <- try (R.getCallbackState (c_duckdb_v2_scalar_function_exec_get_bind_data info) :: IO Bool)
        either (\(_ :: SQLError) -> pure ()) (\_ -> assertFailure "state read with another type") mismatch
        count <- R.output (c_duckdb_v2_scalar_function_exec_get_row_count info)
        argument <- R.output (c_duckdb_v2_scalar_function_exec_get_arg info 0)
        result <- R.output (c_duckdb_v2_scalar_function_exec_get_result info)
        R.checked (c_duckdb_v2_vector_set_size result count)
        forM_ (takeWhile (< count) [0 ..]) $ \index ->
          bracket (R.output (c_duckdb_v2_vector_get_value argument index)) (R.destroy c_duckdb_v2_value_destroy) $ \value ->
            R.checked (c_duckdb_v2_vector_set_value result index value)
      anyType = R.StructuredType (LogicalTypeScalar LogicalTypeIdAny)
      signature = R.FunctionSignature [R.parameter "value" anyType] (Just anyType)
      target = R.ConnectionTarget connection
  R.withRegistrationFactory target $ \factory ->
    R.registerScalarFunctionWith
      target
      ( \function -> do
          R.withIdentifier "managed_resolved" (R.checked . c_duckdb_v2_scalar_function_set_name function)
          R.output (c_duckdb_v2_scalar_function_get_signature function) >>= R.configureFunctionSignature factory signature
          docs <- R.output (c_duckdb_v2_scalar_function_get_docs function)
          Docs.setFunctionDocs docs (Docs.FunctionDocs (Just "Preserve the input type.") [("Example", "SELECT managed_resolved(42)")] ["test"])
      )
      (R.scalarCallbacks executeRows){R.scalarResolveTypesCallback = Just resolve, R.scalarBindCallback = Just bindState}
  query_ connection "SELECT typeof(managed_resolved(i::SMALLINT)), managed_resolved(i::SMALLINT) FROM range(2) t(i) ORDER BY i"
    >>= (@?= [("SMALLINT" :: Text.Text, 0 :: Int64), ("SMALLINT", 1)])
  seen <- readIORef phases
  assertBool "resolve-types callback" ("resolve" `elem` seen)
  assertBool "bind callback" ("bind" `elem` seen)
  query_ connection "SELECT description FROM duckdb_functions() WHERE function_name='managed_resolved'"
    >>= (@?= [Only ("Preserve the input type." :: Text.Text)])

testPragma :: Assertion
testPragma = withConnection ":memory:" \connection -> do
  let amount = (R.parameter "amount" bigint){R.parameterDefault = Just (R.defaultValue (42 :: Int64))}
  Pragma.registerPragmaFunctionOn (R.ConnectionTarget connection) "managed_query" [amount] $ Pragma.QueryPragma $ \info _ -> do
    value <-
      bracket
        (R.output (c_duckdb_v2_function_bind_get_arg_value info 0))
        (R.destroy c_duckdb_v2_value_destroy)
        (R.output . c_duckdb_v2_value_get_bigint)
    pure ("SELECT " <> Text.pack (show value) <> "::BIGINT")
  query_ connection "PRAGMA managed_query" >>= (@?= [Only (42 :: Int64)])
  query_ connection "PRAGMA managed_query(7)" >>= (@?= [Only (7 :: Int64)])
  calls <- newIORef (0 :: Int)
  Pragma.registerPragmaFunctionWith
    (R.ConnectionTarget connection)
    ( \function -> do
        R.withIdentifier "managed_exec" (R.checked . c_duckdb_v2_pragma_function_set_name function)
        docs <- R.output (c_duckdb_v2_pragma_function_get_docs function)
        Docs.setFunctionDocs docs Docs.defaultFunctionDocs{Docs.functionDescription = Just "Count calls."}
    )
    (Pragma.ExecPragma (\_ _ -> modifyIORef' calls (+ 1)))
  void (execute_ connection "PRAGMA managed_exec")
  readIORef calls >>= (@?= 1)
  Pragma.registerPragmaFunctionOn
    (R.ConnectionTarget connection)
    "managed_pragma_error"
    []
    (Pragma.QueryPragma (\_ _ -> throwIO (userError "managed callback failure")))
  assertSqlError (query_ connection "PRAGMA managed_pragma_error" :: IO [Only Int64])
  query_ connection "SELECT 1::BIGINT" >>= (@?= [Only (1 :: Int64)])

testPragmaNames :: Assertion
testPragmaNames = withConnection ":memory:" \connection -> do
  forM_ ["", "managed\0pragma"] \name -> do
    outcome <- try (Pragma.registerPragmaFunctionOn (R.ConnectionTarget connection) name [] (Pragma.ExecPragma (\_ _ -> pure ())))
    case outcome of
      Left (err :: SQLError) -> assertBool "name diagnostic" ("pragma function name" `Text.isInfixOf` sqlErrorMessage err)
      Right () -> assertFailure "expected SQL error"
  query_ connection "SELECT 1::BIGINT" >>= (@?= [Only (1 :: Int64)])

testMetrics :: Assertion
testMetrics = withConnection ":memory:" \connection -> do
  Metrics.enableProfiling connection True
  query_ connection "SELECT sum(i)::BIGINT FROM range(10) t(i)" >>= (@?= [Only (45 :: Int64)])
  snapshot <- Metrics.queryMetrics connection
  Metrics.metricsNodeName snapshot @?= "QUERY"
  assertBool "root metrics" (not (null (Metrics.metricsValues snapshot)))
  assertBool "operator metrics" (not (null (Metrics.metricsChildren snapshot)))
  let scanRows =
        [ value
        | node <- metricNodes snapshot
        , Metrics.metricsNodeName node == "TABLE_SCAN"
        , Just value <- [lookup "operator.intermediate_rows" (Metrics.metricsValues node)]
        ]
  scanRows @?= [FieldWord64 10]
  Metrics.enableProfiling connection False
  query_ connection "SELECT 2::BIGINT" >>= (@?= [Only (2 :: Int64)])
  empty <- Metrics.queryMetrics connection
  Metrics.metricsValues empty @?= []
  Metrics.metricsChildren empty @?= []
  where
    metricNodes node = node : concatMap metricNodes (Metrics.metricsChildren node)

sumCallbacks :: R.AggregateCallbacks
sumCallbacks = R.aggregateCallbacks stateSize initialize update combine finalize
  where
    stateSize info = R.checked (c_duckdb_v2_aggregate_function_size_set_state_size info (fromIntegral (staticSizeOf (Proxy @Int64))))
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
  query_ connection "SELECT managed_sum(i::BIGINT) FROM range(12) t(i) GROUP BY i % 3 ORDER BY i % 3"
    >>= (@?= map Only ([18, 22, 26] :: [Int64]))
  query_ connection "SELECT managed_sum(i::BIGINT) OVER () FROM range(4096) t(i) LIMIT 1" >>= (@?= [Only (8386560 :: Int64)])
  readIORef destroyed >>= assertBool "aggregate state destruction ran" . (> 0)

-- | Keep registration resources across queries and prepared statements. Release
-- them once when the database closes, or when configuration fails.
testAggregateCleanup :: Assertion
testAggregateCleanup = do
  released <- newIORef ([] :: [Text])
  let release name = atomicModifyIORef' released \names -> (name : names, ())
  withConnection ":memory:" \connection -> do
    let target = R.ConnectionTarget connection
    R.withRegistrationFactory target \factory -> do
      let configure name function = do
            R.withIdentifier name (R.checked . c_duckdb_v2_aggregate_function_set_name function)
            signature <- R.output (c_duckdb_v2_aggregate_function_get_signature function)
            R.configureFunctionSignature factory (scalarSignature [R.parameter "value" bigint]) signature
      R.registerAggregateFunctionWithCleanup target (release "used") (configure "managed_owned_sum") sumCallbacks
      R.registerAggregateFunctionWithCleanup target (release "unused") (configure "managed_unused_sum") sumCallbacks
      failed <-
        try
          ( R.registerAggregateFunctionWithCleanup
              target
              (release "failed")
              (\_ -> throwIO (userError "aggregate configuration failed"))
              sumCallbacks
          )
      case failed of
        Left (_ :: IOException) -> pure ()
        Right () -> assertFailure "aggregate configuration must fail"
    readIORef released >>= (@?= ["failed"])
    forM_ [1 .. 3 :: Int] \_ ->
      query_ connection "SELECT managed_owned_sum(i) FROM range(10) t(i)" >>= (@?= [Only (45 :: Int64)])
    withStatement connection "SELECT managed_owned_sum(i) FROM range(10) t(i)" \statement -> do
      nextRow statement >>= (@?= Just (Only (45 :: Int64)))
      void (executeStatement statement)
      nextRow statement >>= (@?= Just (Only (45 :: Int64)))
    readIORef released >>= (@?= ["failed"])
  names <- readIORef released
  forM_ ["used", "unused", "failed"] \name ->
    assertEqual ("cleanup count for " <> Text.unpack name) 1 (length (filter (== name) names))

tableDefinition :: R.TableCallbacks
tableDefinition = (R.tableCallbacks bind execute){R.tableInitGlobalCallback = Just initialize}
  where
    bind input result context = do
      amount <- bracket (R.output (c_duckdb_v2_function_bind_get_arg_value input 0)) (R.destroy c_duckdb_v2_value_destroy) \value ->
        R.output (c_duckdb_v2_value_get_bigint value)
      R.setCallbackState (c_duckdb_v2_function_bind_set_bind_data input) (amount :: Int64)
      bracket
        ( R.output (c_duckdb_v2_context_get_factory context) >>= \factory -> R.output (c_duckdb_v2_factory_create_type_from_id factory DuckDBV2LogicalTypeIdBigInt (ConstPtr nullPtr) (ConstPtr nullPtr) 0)
        )
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

-- | A BIGINT of the custom type member_shifted, as a record field.
newtype Shifted = Shifted Int64

instance DuckValue Shifted where
  duckToField (Shifted value) = FieldInt64 value
  duckFromField = fmap Shifted . duckFromField
  duckLogicalType _ = LogicalTypeAlias "member_shifted" (LogicalTypeScalar LogicalTypeIdBigInt)

-- | A record with a field of the custom type member_shifted.
newtype Holder = Holder {held :: Shifted}
  deriving (Generic)
  deriving (ToField) via (ViaDuckDB Holder)

-- | A STRUCT value and a UNION value with a member of a registered custom type
-- bind again with no run of the registered cast from the base type, and so
-- does a record whose field declares that type. The library writes these
-- values with the default cast of DuckDB. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
testCastAliasMembers :: Assertion
testCastAliasMembers = withConnection ":memory:" \connection -> do
  R.registerCustomType connection "member_shifted" bigint
  calls <- newIORef (0 :: Int)
  R.registerCast connection bigint (R.TypeExpression "member_shifted") Nothing \info _ -> do
    modifyIORef' calls (+ 1)
    count <- R.output (c_duckdb_v2_cast_function_exec_get_row_count info)
    values <- R.output (c_duckdb_v2_cast_function_exec_get_input info) >>= readIntegers (fromIntegral count)
    result <- R.output (c_duckdb_v2_cast_function_exec_get_output info)
    writeIntegers result (map (+ 100) values)
  [(struct, union)] <-
    query_ connection "SELECT {'v': 7::BIGINT::member_shifted}, union_value(v := 7::BIGINT::member_shifted)"
      :: IO [(StructValue FieldValue, UnionValue FieldValue)]
  readIORef calls >>= assertBool "the query runs the registered cast" . (> 0)
  writeIORef calls 0
  query connection "SELECT (?).v::BIGINT, union_extract(?, 'v')::BIGINT, (?).held::BIGINT" (struct, union, Holder (Shifted 7))
    >>= (@?= [(107 :: Int64, 107 :: Int64, 7 :: Int64)])
  query connection "SELECT typeof(?), typeof(?), typeof(?)" (struct, union, Holder (Shifted 7))
    >>= (@?= [("STRUCT(v member_shifted)" :: Text, "UNION(v member_shifted)" :: Text, "STRUCT(held member_shifted)" :: Text)])
  readIORef calls >>= (@?= 0)

-- | A pair of types for 'testCastFailureCode': the setup, the source type, the
-- target type, a constant of the source type, and a column of the source type
-- over @i@. DuckDB plans a cast of the first two pairs as a cast that cannot
-- fail.
data CastPair = CastPair
  { castPairName :: String
  , castPairSetup :: Connection -> IO ()
  , castPairSource :: R.TypeDeclaration
  , castPairTarget :: Text
  , castPairConstant :: Text
  , castPairColumn :: Text
  }

-- | The pairs of 'testCastFailureCode' and 'testCastFailureCommit'.
castFailurePairs :: [CastPair]
castFailurePairs =
  [ CastPair "BIGINT to VARCHAR" (\_ -> pure ()) bigint "VARCHAR" "1::BIGINT" "i::BIGINT"
  , CastPair "INTEGER to BIGINT" (\_ -> pure ()) (R.TypeExpression "INTEGER") "BIGINT" "1::INTEGER" "i::INTEGER"
  , CastPair "BIGINT to a custom type" (\conn -> R.registerCustomType conn "cast_target" bigint) bigint "cast_target" "1::BIGINT" "i::BIGINT"
  ]

-- | Register a cast of the pair that writes NULL to each row and then raises
-- an exception with the text 'castFailure'.
registerFailingCast :: Connection -> CastPair -> IO ()
registerFailingCast conn castPair =
  R.registerCast conn (castPairSource castPair) (R.TypeExpression (castPairTarget castPair)) Nothing \info _ ->
    writeNullRows info >> throwIO (userError castFailure)

-- | The text of the exception of 'registerFailingCast'.
castFailure :: String
castFailure = "the cast refused the value"

-- | Fail a cast of a pair in a constant, in a column of 3000 rows, in an INSERT
-- with the assignment cast, and in an INSERT with CAST. Each failure gives
-- 'ErrorTypeMismatch' with the text of the exception. Each connection of the
-- database runs a query after the failure. TRY_CAST gives the NULL that the
-- callback wrote, and TRY(CAST) raises the error. See
-- Note [The error code of a failed cast] in "Database.DuckDB.Simple.V2.Registration".
testCastFailureCode :: CastPair -> Assertion
testCastFailureCode castPair = withEnvironment \environment ->
  forM_ ["constant", "column", "INSERT", "INSERT with CAST"] \form -> withCastDatabase environment castPair \instance' conn -> do
    let target = castPairTarget castPair
        constant = castPairConstant castPair
        label = castPairName castPair <> ", " <> form
        statement = case form of
          "constant" -> "SELECT CAST(" <> constant <> " AS " <> target <> ")"
          "column" -> "SELECT CAST(c AS " <> target <> ") FROM src"
          "INSERT" -> "INSERT INTO dst SELECT c FROM src"
          _ -> "INSERT INTO dst SELECT CAST(c AS " <> target <> ") FROM src"
    assertMismatch label (Text.pack castFailure) (void (query_ conn (Query statement) :: IO [Only FieldValue]))
    query_ conn "SELECT 42" >>= assertEqual label [Only (42 :: Int64)]
    withConnectionOn instance' \other -> query_ other "SELECT 42" >>= assertEqual label [Only (42 :: Int64)]
    when (form == "constant") do
      query_ conn (Query ("SELECT TRY_CAST(" <> constant <> " AS " <> target <> ") IS NULL")) >>= assertEqual label [Only True]
      assertMismatch
        label
        (Text.pack castFailure)
        (void (query_ conn (Query ("SELECT TRY(CAST(" <> constant <> " AS " <> target <> "))")) :: IO [Only FieldValue]))

-- | A query on another connection fails in the callback of a cast with the
-- type Conversion. The cast error has the type of each cast error.
testCastFailureQuery :: Assertion
testCastFailureQuery = withEnvironment \environment ->
  withCastInstance environment \instance' -> withConnectionOn instance' \conn -> withConnectionOn instance' \other -> do
    R.registerCast conn bigint (R.TypeExpression "VARCHAR") Nothing \_ _ ->
      void (query_ other "SELECT 'abc'::INTEGER" :: IO [Only Int64])
    assertMismatch
      "query"
      "Could not convert string 'abc' to INT32"
      (void (query_ conn "SELECT CAST(i AS VARCHAR) FROM range(10) t(i)" :: IO [Only Text]))
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    query_ other "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | An 'SQLError' of the type Conversion, Invalid Input, or Out of Range from
-- the callback of a cast from BIGINT to VARCHAR gives 'ErrorTypeMismatch', and
-- the database stays usable. DuckDB plans this cast as a cast that cannot fail.
-- With the code of the type of the error, DuckDB would raise an INTERNAL error
-- and invalidate the database. See Note [The error code of a failed cast] in
-- "Database.DuckDB.Simple.V2.Registration".
testCastTypedErrors :: Assertion
testCastTypedErrors = withEnvironment \environment -> forM_ [ErrorTypeConversion, ErrorInputInvalid, ErrorInputOutOfRange] \errorType ->
  withCastInstance environment \instance' -> withConnectionOn instance' \conn -> do
    let label = show errorType
        text = "the cast raised " <> Text.pack label
    void (execute_ conn "CREATE TABLE src AS SELECT i::BIGINT AS c FROM range(3000) t(i)")
    R.registerCast conn bigint (R.TypeExpression "VARCHAR") Nothing \_ _ ->
      throwIO SQLError{sqlErrorMessage = text, sqlErrorType = Just errorType, sqlErrorQuery = Nothing, sqlErrorBody = Just text}
    assertMismatch label text (void (query_ conn "SELECT CAST(c AS VARCHAR) FROM src" :: IO [Only Text]))
    query_ conn "SELECT 42" >>= assertEqual label [Only (42 :: Int64)]
    withConnectionOn instance' \other -> query_ other "SELECT 42" >>= assertEqual label [Only (42 :: Int64)]

-- | A COMMIT after a failed cast in an explicit transaction stores no row,
-- under each transaction invalidation policy. The last cast fails only in its
-- fifth call, the last chunk of 10000 rows, so that the INSERT writes rows
-- before the failure.
testCastFailureCommit :: Assertion
testCastFailureCommit = withEnvironment \environment -> forM_ [Nothing, Just "SYNTACTIC_ERRORS_DO_NOT_INVALIDATE"] \policy -> do
  let commitAfter label instance' conn failing = do
        forM_ (policy :: Maybe Text) \name -> forM_ ["default", "current"] \scope ->
          execute_ conn (Query ("SET " <> scope <> "_transaction_invalidation_policy = '" <> name <> "'"))
        void (execute_ conn "CREATE TABLE marker (m INTEGER)")
        void (execute_ conn "BEGIN TRANSACTION")
        void (execute_ conn "INSERT INTO marker VALUES (1)")
        assertMismatch label (Text.pack castFailure) (void (execute_ conn failing))
        void (try (execute_ conn "COMMIT") :: IO (Either SQLError Int))
        withConnectionOn instance' \other ->
          query_ other "SELECT (SELECT count(*) FROM marker) + (SELECT count(*) FROM dst)"
            >>= assertEqual (label <> ", " <> show policy) [Only (0 :: Int64)]
  forM_ castFailurePairs \castPair -> withCastDatabase environment castPair \instance' conn ->
    commitAfter (castPairName castPair) instance' conn (Query ("INSERT INTO dst SELECT CAST(c AS " <> castPairTarget castPair <> ") FROM src"))
  withCastInstance environment \instance' -> withConnectionOn instance' \conn -> do
    void (execute_ conn "CREATE TABLE src AS SELECT i::BIGINT AS c FROM range(10000) t(i)")
    void (execute_ conn "CREATE TABLE dst (x VARCHAR)")
    calls <- newIORef (0 :: Int)
    R.registerCast conn bigint (R.TypeExpression "VARCHAR") Nothing \info _ -> do
      writeNullRows info
      call <- atomicModifyIORef' calls \count -> (count + 1, count + 1)
      when (call >= 5) (throwIO (userError castFailure))
    commitAfter "BIGINT to VARCHAR over 10000 rows" instance' conn "INSERT INTO dst SELECT CAST(c AS VARCHAR) FROM src"

-- | Require an 'SQLError' of the type 'ErrorTypeMismatch' whose message
-- contains the text.
assertMismatch :: String -> Text -> IO () -> Assertion
assertMismatch label text action =
  try action >>= \case
    Left (err :: SQLError) -> do
      assertEqual label (Just ErrorTypeMismatch) (sqlErrorType err)
      assertBool (label <> ": " <> Text.unpack (sqlErrorMessage err)) (text `Text.isInfixOf` sqlErrorMessage err)
    Right () -> assertFailure (label <> ": expected an SQL error")

-- | Run an action with a new instance that has an in-memory database.
withCastInstance :: Environment -> (Instance -> IO a) -> IO a
withCastInstance environment action = withInstance environment [] \instance' -> do
  attachDatabase instance' ":memory:" "cast_failure" []
  setDefaultDatabase instance' "cast_failure"
  action instance'

-- | Run an action with a new instance, the tables src and dst of a pair, and
-- the failing cast of the pair.
withCastDatabase :: Environment -> CastPair -> (Instance -> Connection -> IO a) -> IO a
withCastDatabase environment castPair action = withCastInstance environment \instance' -> withConnectionOn instance' \conn -> do
  castPairSetup castPair conn
  void (execute_ conn (Query ("CREATE TABLE src AS SELECT " <> castPairColumn castPair <> " AS c FROM range(3000) t(i)")))
  void (execute_ conn (Query ("CREATE TABLE dst (x " <> castPairTarget castPair <> ")")))
  registerFailingCast conn castPair
  action instance' conn

-- | A second registration of a cast for the same pair of types returns with no
-- error, and CAST still runs the callback of the first registration. DuckDB
-- 2.0.0 keeps the first cast for a pair.
testCastSecond :: Assertion
testCastSecond = withConnection ":memory:" \connection -> do
  R.registerCustomType connection "managed_kept" bigint
  let shift amount info _ = do
        count <- R.output (c_duckdb_v2_cast_function_exec_get_row_count info)
        values <- R.output (c_duckdb_v2_cast_function_exec_get_input info) >>= readIntegers (fromIntegral count)
        result <- R.output (c_duckdb_v2_cast_function_exec_get_output info)
        writeIntegers result (map (+ amount) values)
  R.registerCast connection bigint (R.TypeExpression "managed_kept") Nothing (shift 100)
  R.registerCast connection bigint (R.TypeExpression "managed_kept") Nothing (shift 200)
  query_ connection "SELECT CAST(i::BIGINT AS managed_kept)::BIGINT FROM range(3) t(i)" >>= (@?= map Only ([100, 101, 102] :: [Int64]))

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

testContextLog :: Assertion
testContextLog = do
  entries <- newIORef []
  withConnection ":memory:" \connection -> do
    Logging.registerLogSink connection "context_log" (\entry -> atomicModifyIORef' entries (\values -> (entry : values, ()))) (pure ())
    let logged info context = do
          V2Logging.logMessage context Logging.LogLevelWarning "HaskellContext" "from a callback"
          count <- R.output (c_duckdb_v2_scalar_function_exec_get_row_count info)
          vector <- R.output (c_duckdb_v2_scalar_function_exec_get_result info)
          writeIntegers vector (replicate (fromIntegral count) 1)
    R.registerScalarFunction connection "managed_log" (scalarSignature []) [] (R.scalarCallbacks logged)
    void $ execute_ connection "CALL enable_logging(); SET logging_storage = 'context_log'"
    query_ connection "SELECT managed_log()" >>= (@?= [Only (1 :: Int64)])
    void $ execute_ connection "SET logging_storage = 'memory'"
  received <- filter ((== "HaskellContext") . Logging.logEntryType) <$> readIORef entries
  map Logging.logEntryMessage received @?= ["from a callback"]
  map Logging.logEntryScope received @?= [Logging.LogScopeConnection]

testErrors :: Assertion
testErrors = withConnection ":memory:" \connection -> do
  let failure = throwIO (userError "managed callback failure")
  R.registerScalarFunction connection "managed_error" (scalarSignature []) [] (R.scalarCallbacks (\_ _ -> failure))
  assertSqlError (query_ connection "SELECT managed_error()" :: IO [Only Int64])
  R.registerAggregateFunction
    connection
    "managed_error_sum"
    (scalarSignature [R.parameter "value" bigint])
    []
    sumCallbacks{R.aggregateUpdateCallback = \_ -> failure}
  assertSqlError (query_ connection "SELECT managed_error_sum(i::BIGINT) FROM range(3) t(i)" :: IO [Only Int64])
  R.registerTableFunction connection "managed_error_table" [] (R.tableCallbacks (\_ _ _ -> failure) (\_ _ -> pure ()))
  assertSqlError (query_ connection "SELECT * FROM managed_error_table()" :: IO [Only Int64])
  R.registerReplacementScan connection (\_ _ -> failure)
  assertSqlError (query_ connection "SELECT * FROM managed_unknown" :: IO [Only Int64])

-- | A raw exec callback writes an error text with the byte 0xff, which is not
-- valid UTF-8. The query must fail with an SQL error that can be shown.
testInvalidErrorText :: Assertion
testInvalidErrorText =
  bracket (toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux invalidText)) freeHaskellFunPtr \exec ->
    withConnection ":memory:" \connection -> do
      withConnectionHandle connection \native -> withConnectionFactory native \factory ->
        bracket (R.output (c_duckdb_v2_scalar_function_create factory)) (R.destroy c_duckdb_v2_scalar_function_destroy) \function -> do
          R.withIdentifier "invalid_error_text" (R.checked . c_duckdb_v2_scalar_function_set_name function)
          R.output (c_duckdb_v2_scalar_function_get_signature function) >>= R.configureFunctionSignature factory (scalarSignature [])
          R.checked (c_duckdb_v2_scalar_function_set_exec_callback function (DuckDBV2ScalarFunctionExecCallbackFn exec))
          R.checked (c_duckdb_v2_connection_register_scalar_function native function)
      outcome <- try (query_ connection "SELECT invalid_error_text()" :: IO [Only Int64])
      case outcome of
        Left (err :: SQLError) -> do
          assertBool "replacement character" ("bad \xFFFD message" `Text.isInfixOf` sqlErrorMessage err)
          sqlErrorQuery err @?= Just "SELECT invalid_error_text()"
          void (evaluate (length (show err)))
        Right _ -> assertFailure "expected SQL error"
  where
    invalidText _ _ err = do
      info <- peek err
      void (c_duckdb_v2_error_info_set_code info DuckDBV2ErrorInputInvalid)
      withBytes "bad \xff message" (void . c_duckdb_v2_error_info_set_text info)

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
    R.registerScalarFunction
      connection
      "managed_release"
      (scalarSignature [])
      []
      (R.scalarCallbacks execute){R.scalarInitCallback = Just initialize}
    query_ connection "SELECT managed_release()" >>= (@?= [Only (43 :: Int64)])
    stateRefs <- readIORef states
    assertBool "state initialized" (not (null stateRefs))
    pragmaClosure <- newIORef (7 :: Int64)
    pragmaWeak <- mkWeakIORef pragmaClosure (pure ())
    Pragma.registerPragmaFunctionOn
      (R.ConnectionTarget connection)
      "managed_pragma_release"
      []
      (Pragma.ExecPragma (\_ _ -> void (readIORef pragmaClosure)))
    void (execute_ connection "PRAGMA managed_pragma_release")
    failed <- failedRegistration connection
    pure (closureWeak : pragmaWeak : failed : stateRefs)
  performMajorGC
  performMajorGC
  forM_ references \reference -> deRefWeak reference >>= assertBool "native callback state or closure remains live" . isNothing

failedRegistration :: Connection -> IO (Weak (IORef Int64))
failedRegistration connection = do
  retained <- newIORef (1 :: Int64)
  reference <- mkWeakIORef retained (pure ())
  let execute _ _ = void (readIORef retained)
      duplicate = R.parameter "duplicate" bigint
  result <-
    try (R.registerScalarFunction connection "managed_invalid" (scalarSignature [duplicate, duplicate]) [] (R.scalarCallbacks execute))
  case result of
    Left (_ :: SQLError) -> pure ()
    Right () -> assertFailure "duplicate parameter names must fail"
  pure reference

-- | Require an 'SQLError' with the message of a failed managed callback.
assertSqlError :: IO a -> Assertion
assertSqlError = assertSqlErrorContaining "managed callback failure"

readIntegers :: Int -> DuckDBV2VectorHandle -> IO [Int64]
readIntegers count vector = do
  R.checked (c_duckdb_v2_vector_flatten vector)
  raw <- R.output (c_duckdb_v2_vector_get_data_mutable vector)
  mapM (peekElemOff (castPtr raw)) [0 .. count - 1]
