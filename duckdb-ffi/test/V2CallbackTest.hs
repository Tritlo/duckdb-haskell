{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module V2CallbackTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, unless, when, zipWithM_)
import Data.Bits (clearBit)
import Data.Coerce (coerce)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes, free, malloc)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import GHC.Records (getField)
import HsBindgen.Runtime.Prelude (toFunPtr)
import System.Environment (getExecutablePath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (
  argCounts,
  borrowedOpaque,
  callbackGuard,
  checked,
  consume,
  countCall,
  eachPartRequired,
  expectCallbackFailure,
  failOnPurpose,
  output,
  readArgument,
  readIntegers,
  readView,
  withConnection,
  withFactory,
  withHandle,
  withInstance,
  withResult,
  withStatement,
  withType,
  withView,
 )

tests :: TestTree
tests =
  testGroup
    "DuckDB V2 callbacks"
    [ testCase "scalar callbacks read the user data, the bind data and the arguments of the call" testScalarCallbacks
    , testCase "only a cast with a non-negative cost applies implicitly, and the cast reads its user data" testCastCallbacks
    , testCase "custom option updates report the scope and the reset, and can replace the value" testCustomOption
    , testCase "a scalar bind, init or exec callback fails its statement and leaves the connection usable" testScalarFailures
    , testCase "a failing cast callback fails CAST, and TRY_CAST keeps the output of the callback" testCastFailure
    , testCase "a failing option update callback fails the SET and keeps the old value" testCustomOptionFailure
    , testCase "registration rejects a scalar function, a cast, a custom type or a custom option without a required part" testRegistrationFailures
    , testCase "config writes reject an unknown option, an unsupported scope and a startup option" testConfigFailures
    ]

-- | Read the name of an argument.
argName :: DuckDBV2FunctionBindInfoHandle -> DuckDBIdx -> IO String
argName info index = alloca $ \name -> do
  checked (c_duckdb_v2_function_bind_get_arg_name info index name)
  peek name >>= readView . coerce

-- | Look up an argument by name. The index starts as 99, so a lookup that does not find the name returns 99.
argIndex :: DuckDBV2FunctionBindInfoHandle -> String -> IO (CBool, DuckDBIdx)
argIndex info text = withView text $ \name -> alloca $ \index -> alloca $ \found -> do
  poke index 99
  checked (c_duckdb_v2_function_bind_get_arg_index info (coerce name) index found)
  (,) <$> peek found <*> peek index

-- | Read an option as text, with the scope that the value comes from.
readOption :: DuckDBV2ConfigHandle -> String -> IO (String, DuckDBV2SettingScope)
readOption config text = withView text $ \name -> alloca $ \scope ->
  withHandle (\value -> c_duckdb_v2_config_get_option_value config (coerce name) value scope) c_duckdb_v2_value_destroy $ \value -> do
    setting <- output (c_duckdb_v2_value_get_varchar value) >>= readView
    (,) setting <$> peek scope

-- | Ask a file system whether a file exists at a path.
pathExists :: DuckDBV2FileSystemHandle -> String -> IO CBool
pathExists fileSystem path = withView path $ \view -> output (c_duckdb_v2_file_system_path_exists fileSystem view)

testScalarCallbacks :: IO ()
testScalarCallbacks = do
  executable <- getExecutablePath
  failures <- newIORef []
  binds <- newIORef 0
  inits <- newIORef 0
  executions <- newIORef 0
  orders <- newIORef []
  -- The user data and the bind data point to two different cells, so an accessor that returns the other one fails.
  allocaBytes 1 $ \user -> allocaBytes 1 $ \bound -> do
    let bind info result context err = callbackGuard failures err $ do
          countCall binds
          output (c_duckdb_v2_function_bind_get_user_data info) >>= (@?= user)
          -- The query calls v2_probe(i, 1, 2, 3, extra := 5, p := 6): one positional-only, one standard, two *args,
          -- one omitted named-only and two **kwargs arguments. The name p goes to **kwargs, because p is positional-only.
          argCounts (c_duckdb_v2_function_bind_get_arg_count info) >>= (@?= (2, 2, 1, 2))
          checked (c_duckdb_v2_function_bind_get_arg_count info nullPtr nullPtr nullPtr nullPtr)
          mapM (argName info) [0 .. 6] >>= (@?= ["p", "x", "", "", "n", "extra", "p"])
          alloca $ \name -> do
            code <- c_duckdb_v2_function_bind_get_arg_name info 7 name nullPtr
            assertBool "an index after the last argument fails" (code /= DuckDBV2ErrorNone)
          argIndex info "X" >>= (@?= (CBool 1, 1))
          argIndex info "N" >>= (@?= (CBool 1, 4))
          argIndex info "Extra" >>= (@?= (CBool 1, 5))
          argIndex info "P" >>= (@?= (CBool 1, 6))
          argIndex info "args" >>= (@?= (CBool 0, 99))
          argIndex info "absent" >>= (@?= (CBool 0, 99))
          with (borrowedOpaque bound) $ \opaque -> checked (c_duckdb_v2_scalar_function_bind_set_bind_data result opaque)
          config <- output (c_duckdb_v2_context_get_config context)
          readOption config "default_order" >>= \order -> atomicModifyIORef' orders (\seen -> (order : seen, ()))
          fileSystem <- output (c_duckdb_v2_context_get_file_system context)
          pathExists fileSystem executable >>= (@?= CBool 1)
          pathExists fileSystem (executable <> ".absent") >>= (@?= CBool 0)
        initialize info _context err = callbackGuard failures err $ do
          countCall inits
          output (c_duckdb_v2_scalar_function_init_get_user_data info) >>= (@?= user)
          output (c_duckdb_v2_scalar_function_init_get_bind_data info) >>= (@?= bound)
        execute info _context err = callbackGuard failures err $ do
          countCall executions
          output (c_duckdb_v2_scalar_function_exec_get_user_data info) >>= (@?= user)
          rows <- output (c_duckdb_v2_scalar_function_exec_get_row_count info)
          vector <- output (c_duckdb_v2_scalar_function_exec_get_result info)
          raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
          forM_ [0 .. fromIntegral rows - 1] $ \i -> pokeElemOff (coerce raw) i (42 :: Int64)
    bracket
      (DuckDBV2ScalarFunctionBindCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionBindCallbackFn_Aux bind))
      (freeHaskellFunPtr . getField @"unwrap")
      $ \bindPtr ->
        bracket
          (DuckDBV2ScalarFunctionInitCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionInitCallbackFn_Aux initialize))
          (freeHaskellFunPtr . getField @"unwrap")
          $ \initializePtr ->
            bracket
              (DuckDBV2ScalarFunctionExecCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux execute))
              (freeHaskellFunPtr . getField @"unwrap")
              $ \executePtr ->
                withConnection $ \conn -> withFactory conn $ \factory -> do
                  withType factory "BIGINT" $ \ty ->
                    withHandle (c_duckdb_v2_scalar_function_create factory) c_duckdb_v2_scalar_function_destroy $ \function -> do
                      withView "v2_probe" $ \name -> checked (c_duckdb_v2_scalar_function_set_name function (coerce name))
                      signature <- output (c_duckdb_v2_scalar_function_get_signature function)
                      let parameter name value kind = withView name $ \view ->
                            checked (c_duckdb_v2_function_signature_add_parameter signature (coerce view) ty value kind)
                      parameter "p" (coerce nullPtr) DuckDBV2FunctionParameterKindPositionalOnly
                      parameter "x" (coerce nullPtr) DuckDBV2FunctionParameterKindStandard
                      parameter "args" (coerce nullPtr) DuckDBV2FunctionParameterKindPositionalVariadic
                      withHandle (c_duckdb_v2_value_create_bigint factory 10) c_duckdb_v2_value_destroy $ \value ->
                        parameter "n" value DuckDBV2FunctionParameterKindNamedOnly
                      parameter "kwargs" (coerce nullPtr) DuckDBV2FunctionParameterKindNamedVariadic
                      checked (c_duckdb_v2_function_signature_set_return_type signature ty)
                      with (borrowedOpaque user) $ \opaque -> checked (c_duckdb_v2_scalar_function_set_user_data function opaque)
                      checked (c_duckdb_v2_scalar_function_set_bind_callback function bindPtr)
                      checked (c_duckdb_v2_scalar_function_set_init_callback function initializePtr)
                      checked (c_duckdb_v2_scalar_function_set_exec_callback function executePtr)
                      checked (c_duckdb_v2_connection_register_scalar_function conn function)
                  -- The context gives the config of its connection, so the callback sees the session value.
                  config <- output (c_duckdb_v2_connection_get_config conn)
                  withView "default_order" $ \name -> withView "desc" $ \setting ->
                    checked (c_duckdb_v2_config_set_option_text config (coerce name) setting DuckDBV2SettingScopeSession)
                  -- The argument i is a column, so the engine cannot fold the call into a constant.
                  withResult conn "SELECT v2_probe(i, 1, 2, 3, extra := 5, p := 6) FROM range(3) t(i)" $ \result ->
                    readIntegers result >>= (@?= replicate 3 (Just 42))
                  session <- readOption config "default_order"
                  snd session @?= DuckDBV2SettingScopeSession
                  readIORef orders >>= \seen -> assertBool ("the context config gives the session value: " <> show seen) (not (null seen) && all (== session) seen)
  readIORef failures >>= (@?= [])
  readIORef binds >>= assertBool "the bind callback was invoked" . (> 0)
  readIORef inits >>= assertBool "the init callback was invoked" . (> 0)
  readIORef executions >>= assertBool "the exec callback was invoked" . (> 0)

-- | Write each BIGINT of the input vector, plus an offset, to the output vector.
shiftBigints :: DuckDBIdx -> DuckDBV2VectorHandle -> DuckDBV2VectorHandle -> Int64 -> IO ()
shiftBigints count input result offset = do
  checked (c_duckdb_v2_vector_flatten input)
  view <- output (c_duckdb_v2_vector_get_view input)
  raw <- output (c_duckdb_v2_vector_get_data_mutable result)
  forM_ [0 .. fromIntegral count - 1] $ \i -> do
    index <-
      if getField @"sel" view == coerce nullPtr
        then pure i
        else fromIntegral <$> peekElemOff (coerce (getField @"sel" view) :: Ptr DuckDBV2SelT) i
    value <- peekElemOff (coerce (getField @"data'" view)) index
    pokeElemOff (coerce raw) i (value + offset :: Int64)

testCastCallbacks :: IO ()
testCastCallbacks = do
  failures <- newIORef []
  casts <- newIORef 0
  allocaBytes 1 $ \user -> do
    let cast info _context err = callbackGuard failures err $ do
          countCall casts
          output (c_duckdb_v2_cast_function_exec_get_user_data info) >>= (@?= user)
          count <- output (c_duckdb_v2_cast_function_exec_get_row_count info)
          input <- output (c_duckdb_v2_cast_function_exec_get_input info)
          result <- output (c_duckdb_v2_cast_function_exec_get_output info)
          shiftBigints count input result 100
        unwrap info _context err = callbackGuard failures err $ do
          count <- output (c_duckdb_v2_scalar_function_exec_get_row_count info)
          input <- output (c_duckdb_v2_scalar_function_exec_get_arg info 0)
          result <- output (c_duckdb_v2_scalar_function_exec_get_result info)
          shiftBigints count input result 0
    bracket
      (DuckDBV2CastFunctionExecCallbackFn <$> toFunPtr (DuckDBV2CastFunctionExecCallbackFn_Aux cast))
      (freeHaskellFunPtr . getField @"unwrap")
      $ \castPtr ->
        bracket
          (DuckDBV2ScalarFunctionExecCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux unwrap))
          (freeHaskellFunPtr . getField @"unwrap")
          $ \unwrapPtr ->
            withConnection $ \conn -> withFactory conn $ \factory -> withType factory "BIGINT" $ \ty -> do
              -- Register an alias of BIGINT, a function that takes the alias, and a cast from BIGINT to the alias.
              let register typeName functionName cost = withView typeName $ \name -> do
                    withHandle (c_duckdb_v2_custom_type_create factory) c_duckdb_v2_custom_type_destroy $ \custom -> do
                      checked (c_duckdb_v2_custom_type_set_name custom (coerce name))
                      checked (c_duckdb_v2_custom_type_set_base_type custom ty)
                      checked (c_duckdb_v2_connection_register_custom_type conn custom)
                    withHandle (c_duckdb_v2_factory_create_type_with_alias factory ty (coerce name)) c_duckdb_v2_logical_type_destroy $ \alias -> do
                      withHandle (c_duckdb_v2_scalar_function_create factory) c_duckdb_v2_scalar_function_destroy $ \function -> do
                        withView functionName $ \view -> checked (c_duckdb_v2_scalar_function_set_name function (coerce view))
                        signature <- output (c_duckdb_v2_scalar_function_get_signature function)
                        withView "v" $ \view ->
                          checked (c_duckdb_v2_function_signature_add_parameter signature (coerce view) alias (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
                        checked (c_duckdb_v2_function_signature_set_return_type signature ty)
                        checked (c_duckdb_v2_scalar_function_set_exec_callback function unwrapPtr)
                        checked (c_duckdb_v2_connection_register_scalar_function conn function)
                      withHandle (c_duckdb_v2_cast_function_create factory) c_duckdb_v2_cast_function_destroy $ \function -> do
                        checked (c_duckdb_v2_cast_function_set_source_type function ty)
                        checked (c_duckdb_v2_cast_function_set_target_type function alias)
                        checked (c_duckdb_v2_cast_function_set_exec_callback function castPtr)
                        with (borrowedOpaque user) $ \opaque -> checked (c_duckdb_v2_cast_function_set_user_data function opaque)
                        checked (c_duckdb_v2_cast_function_set_implicit_cast_cost function cost)
                        checked (c_duckdb_v2_connection_register_cast_function conn function)
              register "v2_loose" "v2_take_loose" 0
              register "v2_strict" "v2_take_strict" (-1)
              withResult conn "SELECT v2_take_loose(i) FROM range(3) t(i)" $ \result ->
                readIntegers result >>= (@?= [Just 100, Just 101, Just 102])
              withStatement conn "SELECT v2_take_strict(i) FROM range(3) t(i)" $ \statement -> alloca $ \slot -> do
                poke slot (coerce nullPtr)
                flip finally (c_duckdb_v2_result_destroy slot) $ do
                  code <- c_duckdb_v2_statement_execute conn statement (coerce nullPtr) (coerce nullPtr) 0 slot nullPtr
                  assertBool "a negative cost does not apply the cast implicitly" (code /= DuckDBV2ErrorNone)
              withResult conn "SELECT v2_take_strict(CAST(i AS v2_strict)) FROM range(3) t(i)" $ \result ->
                readIntegers result >>= (@?= [Just 100, Just 101, Just 102])
  readIORef failures >>= (@?= [])
  readIORef casts >>= assertBool "the cast callback was invoked" . (> 0)

testCustomOption :: IO ()
testCustomOption = do
  failures <- newIORef []
  updates <- newIORef []
  allocaBytes 1 $ \user -> do
    let update info context err = callbackGuard failures err $ do
          output (c_duckdb_v2_custom_option_update_get_user_data info) >>= (@?= user)
          scope <- output (c_duckdb_v2_custom_option_update_get_scope info)
          reset <- output (c_duckdb_v2_custom_option_update_is_reset info)
          value <- withHandle (c_duckdb_v2_custom_option_update_get_value info) c_duckdb_v2_value_destroy (output . c_duckdb_v2_value_get_bigint)
          atomicModifyIORef' updates (\seen -> ((scope, reset, value) : seen, ()))
          -- The callback stores ten times the value. A reset keeps the default value.
          unless (reset == CBool 1) $ do
            factory <- output (c_duckdb_v2_context_get_factory context)
            withHandle (c_duckdb_v2_value_create_bigint factory (value * 10)) c_duckdb_v2_value_destroy $ \replaced ->
              checked (c_duckdb_v2_custom_option_update_set_value info replaced)
    bracket
      (DuckDBV2CustomOptionUpdateCallbackFn <$> toFunPtr (DuckDBV2CustomOptionUpdateCallbackFn_Aux update))
      (freeHaskellFunPtr . getField @"unwrap")
      $ \updatePtr ->
        withConnection $ \conn -> withFactory conn $ \factory -> withView "v2_probe_option" $ \name -> do
          withHandle (c_duckdb_v2_custom_option_create factory) c_duckdb_v2_custom_option_destroy $ \option -> do
            checked (c_duckdb_v2_custom_option_set_name option (coerce name))
            withHandle (c_duckdb_v2_value_create_bigint factory 1) c_duckdb_v2_value_destroy $ \value ->
              checked (c_duckdb_v2_custom_option_set_default_value option value)
            with (borrowedOpaque user) $ \opaque -> checked (c_duckdb_v2_custom_option_set_user_data option opaque)
            checked (c_duckdb_v2_custom_option_set_update_callback option updatePtr)
            checked (c_duckdb_v2_connection_register_custom_option conn option)
          -- Compare the latest update and the stored value.
          let expect latest stored = do
                readIORef updates >>= \seen -> take 1 seen @?= [latest]
                withResult conn "SELECT current_setting('v2_probe_option')::BIGINT" $ \result -> readIntegers result >>= (@?= [Just stored])
          withResult conn "SET GLOBAL v2_probe_option = 3" consume
          expect (DuckDBV2SettingScopeGlobal, CBool 0, 3) 30
          -- A write at the default scope goes to SESSION, the default scope of the option.
          withResult conn "SET v2_probe_option = 2" consume
          expect (DuckDBV2SettingScopeSession, CBool 0, 2) 20
          withResult conn "RESET v2_probe_option" consume
          expect (DuckDBV2SettingScopeSession, CBool 1, 1) 1
          config <- output (c_duckdb_v2_connection_get_config conn)
          withHandle (c_duckdb_v2_value_create_bigint factory 4) c_duckdb_v2_value_destroy $ \value ->
            checked (c_duckdb_v2_config_set_option_value config (coerce name) value DuckDBV2SettingScopeDefault)
          expect (DuckDBV2SettingScopeSession, CBool 0, 4) 40
  readIORef failures >>= (@?= [])

-- | The function v2_fail(stage, x) returns x. The constant stage selects the callback that fails on purpose:
-- 1 fails the bind, 2 fails the init, and 3 fails the exec for the input 2. The bind data holds the stage.
testScalarFailures :: IO ()
testScalarFailures = do
  failures <- newIORef []
  binds <- newIORef 0
  released <- newIORef 0
  let release ptr = free ptr >> countCall released
  bracket (DuckDBV2OpaqueDestroyFn <$> toFunPtr (DuckDBV2OpaqueDestroyFn_Aux release)) (freeHaskellFunPtr . getField @"unwrap") $ \releasePtr -> do
    let bind info result _context err = callbackGuard failures err $ do
          countCall binds
          stage <- withHandle (c_duckdb_v2_function_bind_get_arg_value info 0) c_duckdb_v2_value_destroy (output . c_duckdb_v2_value_get_bigint)
          -- The argument x is a column, so it does not fold to a constant.
          alloca $ \value -> do
            poke value (coerce nullPtr)
            code <- c_duckdb_v2_function_bind_get_arg_value info 1 value nullPtr
            c_duckdb_v2_value_destroy value >>= (@?= DuckDBV2ErrorNone)
            assertBool "a column argument does not fold to a constant" (code /= DuckDBV2ErrorNone)
            outside <- c_duckdb_v2_function_bind_get_arg_value info 2 value nullPtr
            c_duckdb_v2_value_destroy value >>= (@?= DuckDBV2ErrorNone)
            assertBool "an index after the last argument fails" (outside /= DuckDBV2ErrorNone)
          -- The bind sets the bind data first, so DuckDB must release it also when the bind fails.
          state <- malloc
          poke state (stage :: Int64)
          -- A scalar function sets its bind data through its own setter only.
          with (borrowedOpaque nullPtr) $ \opaque ->
            c_duckdb_v2_function_bind_set_bind_data info opaque nullPtr
              >>= assertBool "the generic bind data setter fails for a scalar function" . (/= DuckDBV2ErrorNone)
          with (DuckDBV2Opaque (coerce state) releasePtr (DuckDBV2OpaqueEqualsFn nullFunPtr)) $ \opaque ->
            checked (c_duckdb_v2_scalar_function_bind_set_bind_data result opaque)
          when (stage == 1) (failOnPurpose "bind")
        initialize info _context err = callbackGuard failures err $ do
          stage <- output (c_duckdb_v2_scalar_function_init_get_bind_data info) >>= peek . coerce
          when (stage == (2 :: Int64)) (failOnPurpose "init")
        execute info _context err = callbackGuard failures err $ do
          stage <- output (c_duckdb_v2_scalar_function_exec_get_bind_data info) >>= peek . coerce
          count <- output (c_duckdb_v2_scalar_function_exec_get_row_count info)
          values <- output (c_duckdb_v2_scalar_function_exec_get_arg info 1) >>= readArgument count
          alloca $ \vector ->
            c_duckdb_v2_scalar_function_exec_get_arg info 2 vector nullPtr
              >>= assertBool "an index after the last argument fails" . (/= DuckDBV2ErrorNone)
          when (stage == (3 :: Int64) && 2 `elem` values) (failOnPurpose "exec")
          result <- output (c_duckdb_v2_scalar_function_exec_get_result info)
          raw <- output (c_duckdb_v2_vector_get_data_mutable result)
          zipWithM_ (pokeElemOff (coerce raw)) [0 ..] values
    bracket
      (DuckDBV2ScalarFunctionBindCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionBindCallbackFn_Aux bind))
      (freeHaskellFunPtr . getField @"unwrap")
      $ \bindPtr ->
        bracket
          (DuckDBV2ScalarFunctionInitCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionInitCallbackFn_Aux initialize))
          (freeHaskellFunPtr . getField @"unwrap")
          $ \initializePtr ->
            bracket
              (DuckDBV2ScalarFunctionExecCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux execute))
              (freeHaskellFunPtr . getField @"unwrap")
              $ \executePtr ->
                withConnection $ \conn -> withFactory conn $ \factory -> do
                  withType factory "BIGINT" $ \ty ->
                    withHandle (c_duckdb_v2_scalar_function_create factory) c_duckdb_v2_scalar_function_destroy $ \function -> do
                      withView "v2_fail" $ \name -> checked (c_duckdb_v2_scalar_function_set_name function (coerce name))
                      signature <- output (c_duckdb_v2_scalar_function_get_signature function)
                      forM_ ["stage", "x"] $ \name -> withView name $ \view ->
                        checked (c_duckdb_v2_function_signature_add_parameter signature (coerce view) ty (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
                      checked (c_duckdb_v2_function_signature_set_return_type signature ty)
                      checked (c_duckdb_v2_scalar_function_set_bind_callback function bindPtr)
                      checked (c_duckdb_v2_scalar_function_set_init_callback function initializePtr)
                      checked (c_duckdb_v2_scalar_function_set_exec_callback function executePtr)
                      checked (c_duckdb_v2_connection_register_scalar_function conn function)
                  let passes = withResult conn "SELECT v2_fail(0, i) FROM range(3) t(i)" $ \result -> readIntegers result >>= (@?= [Just 0, Just 1, Just 2])
                  passes
                  expectCallbackFailure failures conn "SELECT v2_fail(1, i) FROM range(3) t(i)" "bind"
                  passes
                  expectCallbackFailure failures conn "SELECT v2_fail(2, i) FROM range(3) t(i)" "init"
                  passes
                  withResult conn "SELECT v2_fail(3, i) FROM range(2) t(i)" $ \result -> readIntegers result >>= (@?= [Just 0, Just 1])
                  expectCallbackFailure failures conn "SELECT v2_fail(3, i) FROM range(3) t(i)" "exec"
                  passes
  readIORef failures >>= (@?= [])
  bound <- readIORef binds
  assertBool "the bind callback was invoked" (bound > 0)
  readIORef released >>= (@?= bound)

-- | A cast from BIGINT to the alias v2_guarded fails on purpose when the input holds 2.
-- The callback writes NULL for the input 2 before it fails.
testCastFailure :: IO ()
testCastFailure = do
  failures <- newIORef []
  let cast info _context err = callbackGuard failures err $ do
        count <- output (c_duckdb_v2_cast_function_exec_get_row_count info)
        values <- output (c_duckdb_v2_cast_function_exec_get_input info) >>= readArgument count
        result <- output (c_duckdb_v2_cast_function_exec_get_output info)
        raw <- output (c_duckdb_v2_vector_get_data_mutable result)
        zipWithM_ (pokeElemOff (coerce raw)) [0 ..] values
        validity <- output (c_duckdb_v2_vector_flat_get_validity_mutable result)
        forM_ (zip [0 ..] values) $ \(row, value) ->
          when (value == 2) (peekElemOff validity (row `div` 64) >>= pokeElemOff validity (row `div` 64) . (`clearBit` (row `mod` 64)))
        when (2 `elem` values) (failOnPurpose "cast")
  bracket
    (DuckDBV2CastFunctionExecCallbackFn <$> toFunPtr (DuckDBV2CastFunctionExecCallbackFn_Aux cast))
    (freeHaskellFunPtr . getField @"unwrap")
    $ \castPtr ->
      withConnection $ \conn -> withFactory conn $ \factory -> withType factory "BIGINT" $ \ty -> withView "v2_guarded" $ \name -> do
        withHandle (c_duckdb_v2_custom_type_create factory) c_duckdb_v2_custom_type_destroy $ \custom -> do
          checked (c_duckdb_v2_custom_type_set_name custom (coerce name))
          checked (c_duckdb_v2_custom_type_set_base_type custom ty)
          checked (c_duckdb_v2_connection_register_custom_type conn custom)
        withHandle (c_duckdb_v2_factory_create_type_with_alias factory ty (coerce name)) c_duckdb_v2_logical_type_destroy $ \alias ->
          withHandle (c_duckdb_v2_cast_function_create factory) c_duckdb_v2_cast_function_destroy $ \function -> do
            checked (c_duckdb_v2_cast_function_set_source_type function ty)
            checked (c_duckdb_v2_cast_function_set_target_type function alias)
            checked (c_duckdb_v2_cast_function_set_exec_callback function castPtr)
            checked (c_duckdb_v2_connection_register_cast_function conn function)
        let casts sql = withResult conn sql readIntegers
        casts "SELECT CAST(i AS v2_guarded)::BIGINT FROM range(2) t(i)" >>= (@?= [Just 0, Just 1])
        expectCallbackFailure failures conn "SELECT CAST(i AS v2_guarded)::BIGINT FROM range(3) t(i)" "cast"
        casts "SELECT CAST(i AS v2_guarded)::BIGINT FROM range(2) t(i)" >>= (@?= [Just 0, Just 1])
        -- TRY_CAST ignores the error of the callback.
        casts "SELECT TRY_CAST(i AS v2_guarded)::BIGINT FROM range(3) t(i)" >>= (@?= [Just 0, Just 1, Nothing])
        atomicModifyIORef' failures (\seen -> ([], seen)) >>= (@?= [show (userError "v2 deliberate cast failure")])
  readIORef failures >>= (@?= [])

-- | The update callback of the option v2_guarded_option refuses the value 13.
-- Each update also replaces the value with its own text, after a replacement that does not cast to BIGINT fails.
testCustomOptionFailure :: IO ()
testCustomOptionFailure = do
  failures <- newIORef []
  let update info context err = callbackGuard failures err $ do
        value <- withHandle (c_duckdb_v2_custom_option_update_get_value info) c_duckdb_v2_value_destroy (output . c_duckdb_v2_value_get_bigint)
        factory <- output (c_duckdb_v2_context_get_factory context)
        let replace text = withView text $ \view -> withHandle (c_duckdb_v2_value_create_varchar factory view) c_duckdb_v2_value_destroy $ \replacement ->
              c_duckdb_v2_custom_option_update_set_value info replacement nullPtr
        replace "not a number" >>= assertBool "a replacement that does not cast to BIGINT fails" . (/= DuckDBV2ErrorNone)
        replace (show value) >>= (@?= DuckDBV2ErrorNone)
        when (value == 13) (failOnPurpose "update")
  bracket
    (DuckDBV2CustomOptionUpdateCallbackFn <$> toFunPtr (DuckDBV2CustomOptionUpdateCallbackFn_Aux update))
    (freeHaskellFunPtr . getField @"unwrap")
    $ \updatePtr ->
      withConnection $ \conn -> withFactory conn $ \factory -> do
        withHandle (c_duckdb_v2_custom_option_create factory) c_duckdb_v2_custom_option_destroy $ \option -> do
          withView "v2_guarded_option" $ \name -> checked (c_duckdb_v2_custom_option_set_name option (coerce name))
          withHandle (c_duckdb_v2_value_create_bigint factory 1) c_duckdb_v2_value_destroy $ \value ->
            checked (c_duckdb_v2_custom_option_set_default_value option value)
          checked (c_duckdb_v2_custom_option_set_update_callback option updatePtr)
          checked (c_duckdb_v2_connection_register_custom_option conn option)
        let current = withResult conn "SELECT current_setting('v2_guarded_option')::BIGINT" readIntegers
        withResult conn "SET v2_guarded_option = 12" consume
        current >>= (@?= [Just 12])
        expectCallbackFailure failures conn "SET v2_guarded_option = 13" "update"
        current >>= (@?= [Just 12])
  readIORef failures >>= (@?= [])

-- | The signature rules that a registration checks. 'parametersWithout' breaks one rule for each of them.
signatureRules :: [String]
signatureRules = ["unique parameter names", "single *args", "single **kwargs", "defaults last", "*args without default", "**kwargs without default"]

-- | The parameters of the scalar function in the registration test: a name, a kind, and whether the parameter has a default.
parametersWithout :: String -> [(String, DuckDBV2FunctionParameterKind, Bool)]
parametersWithout omitted = case omitted of
  "unique parameter names" -> [("x", standard, False), ("x", standard, False)]
  "single *args" -> [("x", standard, False), ("rest", variadic, False), ("more", variadic, False)]
  "defaults last" -> [("x", standard, True), ("y", standard, False)]
  "single **kwargs" -> [("x", standard, False), ("options", named, False), ("more", named, False)]
  "*args without default" -> [("rest", variadic, True)]
  "**kwargs without default" -> [("options", named, True)]
  _ -> [("x", standard, False), ("y", standard, True), ("rest", variadic, False)]
  where
    standard = DuckDBV2FunctionParameterKindStandard
    variadic = DuckDBV2FunctionParameterKindPositionalVariadic
    named = DuckDBV2FunctionParameterKindNamedVariadic

-- | Each registration leaves out one part that the header requires, or uses ANY where the header requires a concrete
-- type. The last registration of each kind has every part. The test never calls the functions.
testRegistrationFailures :: IO ()
testRegistrationFailures = do
  let execute _info _context _err = pure ()
      cast _info _context _err = pure ()
  bracket
    (DuckDBV2ScalarFunctionExecCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux execute))
    (freeHaskellFunPtr . getField @"unwrap")
    $ \executePtr ->
      bracket
        (DuckDBV2CastFunctionExecCallbackFn <$> toFunPtr (DuckDBV2CastFunctionExecCallbackFn_Aux cast))
        (freeHaskellFunPtr . getField @"unwrap")
        $ \castPtr ->
          withInstance $ \other -> withConnection $ \conn -> withFactory conn $ \factory -> withType factory "BIGINT" $ \ty ->
            withHandle
              (c_duckdb_v2_factory_create_type_from_id factory DuckDBV2LogicalTypeIdAny (coerce nullPtr) (coerce nullPtr) 0)
              c_duckdb_v2_logical_type_destroy
              $ \anyType -> do
                -- The part "concrete type" replaces BIGINT with ANY. The part "own factory" creates the object through
                -- the factory of another instance.
                otherFactory <- output (c_duckdb_v2_instance_get_factory other)
                let concrete omitted = if omitted == "concrete type" then anyType else ty
                    creator omitted = if omitted == "own factory" then otherFactory else factory
                eachPartRequired (["name", "exec callback", "return type", "concrete type", "own factory"] <> signatureRules) $ \omitted ->
                  withHandle (c_duckdb_v2_value_create_bigint factory 1) c_duckdb_v2_value_destroy $ \one ->
                    withHandle (c_duckdb_v2_scalar_function_create (creator omitted)) c_duckdb_v2_scalar_function_destroy $ \function -> do
                      unless (omitted == "name") $ withView "v2_registered" $ \name -> checked (c_duckdb_v2_scalar_function_set_name function (coerce name))
                      unless (omitted == "exec callback") $ checked (c_duckdb_v2_scalar_function_set_exec_callback function executePtr)
                      signature <- output (c_duckdb_v2_scalar_function_get_signature function)
                      unless (omitted == "return type") $ checked (c_duckdb_v2_function_signature_set_return_type signature (concrete omitted))
                      forM_ (parametersWithout omitted) $ \(name, kind, defaulted) -> withView name $ \view ->
                        checked (c_duckdb_v2_function_signature_add_parameter signature (coerce view) ty (if defaulted then one else coerce nullPtr) kind)
                      c_duckdb_v2_connection_register_scalar_function conn function nullPtr
                withView "v2_registered_type" $ \name -> do
                  withHandle (c_duckdb_v2_factory_create_type_with_alias factory ty (coerce name)) c_duckdb_v2_logical_type_destroy $ \alias ->
                    eachPartRequired ["source type", "target type", "exec callback", "concrete type", "own factory"] $ \omitted ->
                      withHandle (c_duckdb_v2_cast_function_create (creator omitted)) c_duckdb_v2_cast_function_destroy $ \function -> do
                        unless (omitted == "source type") $ checked (c_duckdb_v2_cast_function_set_source_type function (concrete omitted))
                        unless (omitted == "target type") $ checked (c_duckdb_v2_cast_function_set_target_type function alias)
                        unless (omitted == "exec callback") $ checked (c_duckdb_v2_cast_function_set_exec_callback function castPtr)
                        c_duckdb_v2_connection_register_cast_function conn function nullPtr
                  eachPartRequired ["name", "base type", "concrete type", "own factory"] $ \omitted ->
                    withHandle (c_duckdb_v2_custom_type_create (creator omitted)) c_duckdb_v2_custom_type_destroy $ \custom -> do
                      unless (omitted == "name") $ checked (c_duckdb_v2_custom_type_set_name custom (coerce name))
                      unless (omitted == "base type") $ checked (c_duckdb_v2_custom_type_set_base_type custom (concrete omitted))
                      c_duckdb_v2_connection_register_custom_type conn custom nullPtr
                -- Registration casts the text of the default value to BIGINT. The part "default value that casts" gives a text that does not cast.
                let option omitted = withHandle (c_duckdb_v2_custom_option_create (creator omitted)) c_duckdb_v2_custom_option_destroy $ \custom -> do
                      unless (omitted == "name") $ withView "v2_registered_option" $ \name -> checked (c_duckdb_v2_custom_option_set_name custom (coerce name))
                      unless (omitted == "type or default value") $ do
                        checked (c_duckdb_v2_custom_option_set_type custom ty)
                        withView (if omitted == "default value that casts" then "five" else "5") $ \text ->
                          withHandle (c_duckdb_v2_value_create_varchar factory text) c_duckdb_v2_value_destroy $ \value ->
                            checked (c_duckdb_v2_custom_option_set_default_value custom value)
                      c_duckdb_v2_connection_register_custom_option conn custom nullPtr
                eachPartRequired ["name", "type or default value", "default value that casts", "own factory"] option
                option "" >>= assertBool "a second option with the same name fails" . (/= DuckDBV2ErrorNone)
                withResult conn "SELECT current_setting('v2_registered_option')::BIGINT" $ \result -> readIntegers result >>= (@?= [Just 5])

-- | A connection config rejects an unknown option, a SESSION write of the GLOBAL option threads and a write of the
-- startup option allow_unsigned_extensions. An instance config has no SESSION scope.
testConfigFailures :: IO ()
testConfigFailures = withInstance $ \instanceHandle ->
  withHandle (c_duckdb_v2_connection_create instanceHandle) c_duckdb_v2_connection_destroy $ \conn -> do
    instanceConfig <- output (c_duckdb_v2_instance_get_config instanceHandle)
    config <- output (c_duckdb_v2_connection_get_config conn)
    let write target name setting scope = withView name $ \view -> withView setting $ \text ->
          c_duckdb_v2_config_set_option_text target (coerce view) text scope nullPtr
        rejected code = assertBool ("the write fails: " <> show code) (code /= DuckDBV2ErrorNone)
    write config "threads" "2" DuckDBV2SettingScopeGlobal >>= (@?= DuckDBV2ErrorNone)
    -- The header names ERROR_INPUT_INVALID for a scope that the option does not support. DuckDB reports a catalog
    -- error, so the test checks only that the write fails.
    write config "threads" "2" DuckDBV2SettingScopeSession >>= rejected
    write instanceConfig "TimeZone" "UTC" DuckDBV2SettingScopeGlobal >>= (@?= DuckDBV2ErrorNone)
    write instanceConfig "TimeZone" "UTC" DuckDBV2SettingScopeSession >>= (@?= DuckDBV2ErrorInputInvalid)
    write config "v2_no_such_option" "2" DuckDBV2SettingScopeGlobal >>= rejected
    write config "preserve_insertion_order" "true" DuckDBV2SettingScopeGlobal >>= (@?= DuckDBV2ErrorNone)
    write config "allow_unsigned_extensions" "true" DuckDBV2SettingScopeGlobal >>= rejected
    -- The write casts the value to the type of the option.
    factory <- output (c_duckdb_v2_connection_get_factory conn)
    let writeValue setting = withView "threads" $ \name -> withView setting $ \text ->
          withHandle (c_duckdb_v2_value_create_varchar factory text) c_duckdb_v2_value_destroy $ \value ->
            c_duckdb_v2_config_set_option_value config (coerce name) value DuckDBV2SettingScopeGlobal nullPtr
    writeValue "3" >>= (@?= DuckDBV2ErrorNone)
    writeValue "three" >>= rejected
    readOption config "TimeZone" >>= (@?= ("UTC", DuckDBV2SettingScopeGlobal))
    withView "v2_no_such_option" $ \name -> alloca $ \value -> alloca $ \scope -> do
      poke value (coerce nullPtr)
      code <- c_duckdb_v2_config_get_option_value config (coerce name) value scope nullPtr
      c_duckdb_v2_value_destroy value >>= (@?= DuckDBV2ErrorNone)
      code @?= DuckDBV2ErrorInputInvalid
