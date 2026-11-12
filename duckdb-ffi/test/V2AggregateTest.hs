{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module V2AggregateTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Void (Void)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import GHC.Records (getField)
import HsBindgen.Runtime.Prelude (toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (argCounts, borrowedOpaque, callbackGuard, checked, fillDocs, output, readIntegers, withConnection, withFactory, withHandle, withResult, withType, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 aggregate functions"
        [ testCase "each aggregate callback reads the user data and the bind data, and the catalog shows the documentation" testAggregateCallbacks
        ]

{- | A state holds a tag at index 0 and a total at index 1.
The init callback sets the tag to 'initialized', the finalize callback sets it to 'finalized',
and the destroy callback sets it to 'destroyed'.
-}
initialized, finalized, destroyed :: Int64
initialized = 1
finalized = 2
destroyed = 3

-- | Get the state at an index of a state array.
stateAt :: Ptr (Ptr Void) -> Int -> IO (Ptr Int64)
stateAt states i = castPtr <$> peekElemOff states i

-- | Add a value to the total of a state.
addTo :: Ptr Int64 -> Int64 -> IO ()
addTo state value = peekElemOff state 1 >>= pokeElemOff state 1 . (+ value)

testAggregateCallbacks :: IO ()
testAggregateCallbacks = do
    failures <- newIORef []
    invoked <- newIORef []
    destroyedStates <- newIORef 0
    -- The user data and the bind data point to two different cells, so an accessor that returns the other one fails.
    allocaBytes 1 $ \user -> allocaBytes 1 $ \bound -> do
        let note name = atomicModifyIORef' invoked (\names -> (name : names, ()))
            expectData name getUser getBind = do
                note name
                output getUser >>= (@?= user)
                output getBind >>= (@?= bound)
            -- The query calls v2_total(i, i, i): one standard argument and two *args.
            bind info result context err = callbackGuard failures err $ do
                note "bind"
                output (c_duckdb_v2_function_bind_get_user_data info) >>= (@?= user)
                argCounts (c_duckdb_v2_function_bind_get_arg_count info) >>= (@?= (1, 2, 0, 0))
                with (borrowedOpaque bound) $ \opaque -> checked (c_duckdb_v2_function_bind_set_bind_data info opaque)
                factory <- output (c_duckdb_v2_context_get_factory context)
                withType factory "BIGINT" $ \ty -> checked (c_duckdb_v2_aggregate_function_bind_set_return_type result ty)
            size info err = callbackGuard failures err $ do
                expectData "size" (c_duckdb_v2_aggregate_function_size_get_user_data info) (c_duckdb_v2_aggregate_function_size_get_bind_data info)
                checked (c_duckdb_v2_aggregate_function_size_set_state_size info (fromIntegral (2 * sizeOf (0 :: Int64))))
            initialize info err = callbackGuard failures err $ do
                expectData "init" (c_duckdb_v2_aggregate_function_init_get_user_data info) (c_duckdb_v2_aggregate_function_init_get_bind_data info)
                states <- output (c_duckdb_v2_aggregate_function_init_get_states info)
                count <- output (c_duckdb_v2_aggregate_function_init_get_state_count info)
                forM_ [0 .. fromIntegral count - 1] $ \i -> do
                    state <- stateAt states i
                    pokeElemOff state 0 initialized
                    pokeElemOff state 1 0
            update info err = callbackGuard failures err $ do
                expectData "update" (c_duckdb_v2_aggregate_function_update_get_user_data info) (c_duckdb_v2_aggregate_function_update_get_bind_data info)
                argCounts (c_duckdb_v2_aggregate_function_update_get_arg_count info) >>= (@?= (1, 2, 0, 0))
                checked (c_duckdb_v2_aggregate_function_update_get_arg_count info nullPtr nullPtr nullPtr nullPtr)
                states <- output (c_duckdb_v2_aggregate_function_update_get_states info)
                count <- output (c_duckdb_v2_aggregate_function_update_get_row_count info)
                vector <- output (c_duckdb_v2_aggregate_function_update_get_arg info 0)
                checked (c_duckdb_v2_vector_flatten vector)
                view <- output (c_duckdb_v2_vector_get_view vector)
                forM_ [0 .. fromIntegral count - 1] $ \i -> do
                    index <- if getField @"sel" view == coerce nullPtr then pure i else fromIntegral <$> peekElemOff (coerce (getField @"sel" view) :: Ptr DuckDBV2SelT) i
                    value <- peekElemOff (coerce (getField @"data'" view)) index
                    state <- stateAt states i
                    addTo state value
            combine info err = callbackGuard failures err $ do
                expectData "combine" (c_duckdb_v2_aggregate_function_combine_get_user_data info) (c_duckdb_v2_aggregate_function_combine_get_bind_data info)
                sources <- output (c_duckdb_v2_aggregate_function_combine_get_sources info)
                targets <- output (c_duckdb_v2_aggregate_function_combine_get_targets info)
                count <- output (c_duckdb_v2_aggregate_function_combine_get_state_count info)
                forM_ [0 .. fromIntegral count - 1] $ \i -> do
                    total <- stateAt sources i >>= \source -> peekElemOff source 1
                    stateAt targets i >>= \target -> addTo target total
            finalize info err = callbackGuard failures err $ do
                expectData "finalize" (c_duckdb_v2_aggregate_function_finalize_get_user_data info) (c_duckdb_v2_aggregate_function_finalize_get_bind_data info)
                states <- output (c_duckdb_v2_aggregate_function_finalize_get_states info)
                count <- output (c_duckdb_v2_aggregate_function_finalize_get_state_count info)
                offset <- output (c_duckdb_v2_aggregate_function_finalize_get_result_offset info)
                vector <- output (c_duckdb_v2_aggregate_function_finalize_get_result info)
                raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
                forM_ [0 .. fromIntegral count - 1] $ \i -> do
                    state <- stateAt states i
                    peekElemOff state 1 >>= pokeElemOff (coerce raw) (fromIntegral offset + i)
                    pokeElemOff state 0 finalized
            -- The engine ignores an error from the destroy callback. The test reads the failures after the query.
            destroy info err = callbackGuard failures err $ do
                expectData "destroy" (c_duckdb_v2_aggregate_function_destroy_get_user_data info) (c_duckdb_v2_aggregate_function_destroy_get_bind_data info)
                states <- output (c_duckdb_v2_aggregate_function_destroy_get_states info)
                count <- output (c_duckdb_v2_aggregate_function_destroy_get_state_count info)
                forM_ [0 .. fromIntegral count - 1] $ \i -> do
                    state <- stateAt states i
                    tag <- peekElemOff state 0
                    assertBool ("each state to destroy is live, but a tag is " <> show tag) (tag == initialized || tag == finalized)
                    pokeElemOff state 0 destroyed
                atomicModifyIORef' destroyedStates (\total -> (total + count, ()))
        bracket (DuckDBV2AggregateFunctionBindCallbackFn <$> toFunPtr (DuckDBV2AggregateFunctionBindCallbackFn_Aux bind)) (freeHaskellFunPtr . getField @"unwrap") $ \bindPtr ->
            bracket (DuckDBV2AggregateFunctionSizeCallbackFn <$> toFunPtr (DuckDBV2AggregateFunctionSizeCallbackFn_Aux size)) (freeHaskellFunPtr . getField @"unwrap") $ \sizePtr ->
                bracket (DuckDBV2AggregateFunctionInitCallbackFn <$> toFunPtr (DuckDBV2AggregateFunctionInitCallbackFn_Aux initialize)) (freeHaskellFunPtr . getField @"unwrap") $ \initializePtr ->
                    bracket (DuckDBV2AggregateFunctionUpdateCallbackFn <$> toFunPtr (DuckDBV2AggregateFunctionUpdateCallbackFn_Aux update)) (freeHaskellFunPtr . getField @"unwrap") $ \updatePtr ->
                        bracket (DuckDBV2AggregateFunctionCombineCallbackFn <$> toFunPtr (DuckDBV2AggregateFunctionCombineCallbackFn_Aux combine)) (freeHaskellFunPtr . getField @"unwrap") $ \combinePtr ->
                            bracket (DuckDBV2AggregateFunctionFinalizeCallbackFn <$> toFunPtr (DuckDBV2AggregateFunctionFinalizeCallbackFn_Aux finalize)) (freeHaskellFunPtr . getField @"unwrap") $ \finalizePtr ->
                                bracket (DuckDBV2AggregateFunctionDestroyCallbackFn <$> toFunPtr (DuckDBV2AggregateFunctionDestroyCallbackFn_Aux destroy)) (freeHaskellFunPtr . getField @"unwrap") $ \destroyPtr ->
                                    withConnection $ \conn -> withFactory conn $ \factory -> do
                                        withType factory "BIGINT" $ \ty ->
                                            withHandle (c_duckdb_v2_factory_create_type_from_id factory DuckDBV2LogicalTypeIdAny (coerce nullPtr) (coerce nullPtr) 0) c_duckdb_v2_logical_type_destroy $ \anyType ->
                                                withHandle (c_duckdb_v2_aggregate_function_create factory) c_duckdb_v2_aggregate_function_destroy $ \function -> do
                                                    withView "v2_total" $ \name -> checked (c_duckdb_v2_aggregate_function_set_name function (coerce name))
                                                    signature <- output (c_duckdb_v2_aggregate_function_get_signature function)
                                                    withView "x" $ \name -> checked (c_duckdb_v2_function_signature_add_parameter signature (coerce name) ty (coerce nullPtr) DuckDBV2FunctionParameterKindStandard)
                                                    withView "rest" $ \name -> checked (c_duckdb_v2_function_signature_add_parameter signature (coerce name) ty (coerce nullPtr) DuckDBV2FunctionParameterKindPositionalVariadic)
                                                    -- The bind callback sets the concrete return type.
                                                    checked (c_duckdb_v2_function_signature_set_return_type signature anyType)
                                                    with (borrowedOpaque user) $ \opaque -> checked (c_duckdb_v2_aggregate_function_set_user_data function opaque)
                                                    checked (c_duckdb_v2_aggregate_function_set_bind_callback function bindPtr)
                                                    checked (c_duckdb_v2_aggregate_function_set_size_callback function sizePtr)
                                                    checked (c_duckdb_v2_aggregate_function_set_init_callback function initializePtr)
                                                    checked (c_duckdb_v2_aggregate_function_set_update_callback function updatePtr)
                                                    checked (c_duckdb_v2_aggregate_function_set_combine_callback function combinePtr)
                                                    checked (c_duckdb_v2_aggregate_function_set_finalize_callback function finalizePtr)
                                                    checked (c_duckdb_v2_aggregate_function_set_destroy_callback function destroyPtr)
                                                    output (c_duckdb_v2_aggregate_function_get_docs function) >>= fillDocs
                                                    checked (c_duckdb_v2_connection_register_aggregate_function conn function)
                                        withResult conn "SELECT v2_total(i, i, i) FROM range(12) t(i) GROUP BY i % 3 ORDER BY i % 3" $ \result -> do
                                            withHandle (c_duckdb_v2_result_get_schema result) c_duckdb_v2_schema_destroy $ \schema -> alloca $ \name -> do
                                                ty <- output (c_duckdb_v2_schema_get_field schema 0 name)
                                                output (c_duckdb_v2_logical_type_get_id ty) >>= (@?= DuckDBV2LogicalTypeIdBigInt)
                                            readIntegers result >>= (@?= [Just 18, Just 22, Just 26])
                                        -- A window over all rows combines the states of a segment tree.
                                        withResult conn "SELECT v2_total(i, i, i) OVER () FROM range(4096) t(i) LIMIT 1" $ \result ->
                                            readIntegers result >>= (@?= [Just 8386560])
                                        withResult conn "SELECT count(*)::BIGINT FROM duckdb_functions() WHERE function_name = 'v2_total' AND description = 'Replaces the description.' AND examples = ['SELECT v2_plus_one(1)', 'SELECT v2_plus_one(2)'] AND categories = ['numeric']" $ \result ->
                                            readIntegers result >>= (@?= [Just 1])
    readIORef failures >>= (@?= [])
    names <- readIORef invoked
    forM_ ["bind", "size", "init", "update", "combine", "finalize", "destroy"] $ \name ->
        assertBool ("the " <> name <> " callback was invoked") (name `elem` names)
    readIORef destroyedStates >>= assertBool "the destroy callback received states" . (> 0)
