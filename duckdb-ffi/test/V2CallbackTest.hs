{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module V2CallbackTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, unless)
import Data.Coerce (coerce)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import GHC.Records (getField)
import HsBindgen.Runtime.Prelude (toFunPtr)
import System.Environment (getExecutablePath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (borrowedOpaque, callbackGuard, checked, consume, countCall, output, readIntegers, readView, withConnection, withFactory, withHandle, withResult, withStatement, withType, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 callbacks"
        [ testCase "scalar callbacks read the user data, the bind data and the arguments of the call" testScalarCallbacks
        , testCase "only a cast with a non-negative cost applies implicitly, and the cast reads its user data" testCastCallbacks
        , testCase "custom option updates report the scope and the reset, and can replace the value" testCustomOption
        ]

-- | Read the sizes of the four parts of the argument list.
argCounts :: DuckDBV2FunctionBindInfoHandle -> IO (DuckDBIdx, DuckDBIdx, DuckDBIdx, DuckDBIdx)
argCounts info = alloca $ \fixed -> alloca $ \variadic -> alloca $ \named -> alloca $ \kwargs -> do
    checked (c_duckdb_v2_function_bind_get_arg_count info fixed variadic named kwargs)
    (,,,) <$> peek fixed <*> peek variadic <*> peek named <*> peek kwargs

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
                -- The query calls v2_probe(i, 2, 3, extra := 5): one standard, two *args, one omitted named-only and one **kwargs argument.
                argCounts info >>= (@?= (1, 2, 1, 1))
                checked (c_duckdb_v2_function_bind_get_arg_count info nullPtr nullPtr nullPtr nullPtr)
                mapM (argName info) [0 .. 4] >>= (@?= ["x", "", "", "n", "extra"])
                alloca $ \name -> do
                    code <- c_duckdb_v2_function_bind_get_arg_name info 5 name nullPtr
                    assertBool "an index after the last argument fails" (code /= DuckDBV2ErrorNone)
                argIndex info "X" >>= (@?= (CBool 1, 0))
                argIndex info "N" >>= (@?= (CBool 1, 3))
                argIndex info "Extra" >>= (@?= (CBool 1, 4))
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
        bracket (DuckDBV2ScalarFunctionBindCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionBindCallbackFn_Aux bind)) (freeHaskellFunPtr . getField @"unwrap") $ \bindPtr ->
            bracket (DuckDBV2ScalarFunctionInitCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionInitCallbackFn_Aux initialize)) (freeHaskellFunPtr . getField @"unwrap") $ \initializePtr ->
                bracket (DuckDBV2ScalarFunctionExecCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux execute)) (freeHaskellFunPtr . getField @"unwrap") $ \executePtr ->
                    withConnection $ \conn -> withFactory conn $ \factory -> do
                        withType factory "BIGINT" $ \ty ->
                            withHandle (c_duckdb_v2_scalar_function_create factory) c_duckdb_v2_scalar_function_destroy $ \function -> do
                                withView "v2_probe" $ \name -> checked (c_duckdb_v2_scalar_function_set_name function (coerce name))
                                signature <- output (c_duckdb_v2_scalar_function_get_signature function)
                                let parameter name value kind = withView name $ \view ->
                                        checked (c_duckdb_v2_function_signature_add_parameter signature (coerce view) ty value kind)
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
                        withResult conn "SELECT v2_probe(i, 2, 3, extra := 5) FROM range(3) t(i)" $ \result ->
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
        index <- if getField @"sel" view == coerce nullPtr then pure i else fromIntegral <$> peekElemOff (coerce (getField @"sel" view) :: Ptr DuckDBV2SelT) i
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
        bracket (DuckDBV2CastFunctionExecCallbackFn <$> toFunPtr (DuckDBV2CastFunctionExecCallbackFn_Aux cast)) (freeHaskellFunPtr . getField @"unwrap") $ \castPtr ->
            bracket (DuckDBV2ScalarFunctionExecCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux unwrap)) (freeHaskellFunPtr . getField @"unwrap") $ \unwrapPtr ->
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
                                    withView "v" $ \view -> checked (c_duckdb_v2_function_signature_add_parameter signature (coerce view) alias (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
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
        bracket (DuckDBV2CustomOptionUpdateCallbackFn <$> toFunPtr (DuckDBV2CustomOptionUpdateCallbackFn_Aux update)) (freeHaskellFunPtr . getField @"unwrap") $ \updatePtr ->
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
