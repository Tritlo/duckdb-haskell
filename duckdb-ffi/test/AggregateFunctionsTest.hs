{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module AggregateFunctionsTest (tests) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Void (Void)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..), CChar)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase, withLogicalType, withResultCString)

tests :: TestTree
tests =
    testGroup
        "Aggregate Functions"
        [ sumAggregate
        , extraInfoAggregate
        , aggregateFunctionSet
        , aggregateErrorPropagation
        , specialHandlingNulls
        ]

-- Test cases ----------------------------------------------------------------

sumAggregate :: TestTree
sumAggregate =
    testCase "register custom aggregate and execute" $
        runInBoundThread do
            withDatabase \db ->
                withConnection db \conn ->
                    withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) \intType ->
                        withAggregateFunction \aggFun ->
                            withCallbacks \cbs -> do
                                setupAggregateFunction aggFun cbs intType "haskell_sum"
                                duckdb_register_aggregate_function conn aggFun >>= (@?= DuckDBSuccess)
                                withConstCString "SELECT haskell_sum(v) FROM (VALUES (1), (2), (3)) t(v)" \sql ->
                                    withResultCString conn sql \resPtr ->
                                        duckdb_value_int32 resPtr 0 0 >>= (@?= 6)

extraInfoAggregate :: TestTree
extraInfoAggregate =
    testCase "extra info is visible inside callbacks" $
        runInBoundThread do
            let config = defaultAggregateConfig{cfgBonus = 2}
            withDatabase \db ->
                withConnection db \conn ->
                    withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) \intType ->
                        withAggregateConfig config \configPtr ->
                            withAggregateFunction \aggFun ->
                                withCallbacks \cbs -> do
                                    setupAggregateFunction aggFun cbs intType "haskell_bonus_sum"
                                    duckdb_aggregate_function_set_extra_info aggFun configPtr (coerce (nullFunPtr :: FunPtr Void))

                                    duckdb_register_aggregate_function conn aggFun >>= (@?= DuckDBSuccess)

                                    withConstCString "SELECT haskell_bonus_sum(v) FROM (VALUES (1), (2), (3)) t(v)" \sql ->
                                        withResultCString conn sql \resPtr ->
                                            duckdb_value_int32 resPtr 0 0 >>= (@?= 12)

aggregateFunctionSet :: TestTree
aggregateFunctionSet =
    testCase "aggregate function set registers overload" $
        runInBoundThread do
            withDatabase \db ->
                withConnection db \conn ->
                    withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) \intType ->
                        withAggregateFunction \aggFun ->
                            withCallbacks \cbs -> do
                                setupAggregateFunction aggFun cbs intType "haskell_sum_set"
                                withConstCString "haskell_sum_set" \setName ->
                                    withAggregateFunctionSet setName \set -> do
                                        duckdb_add_aggregate_function_to_set set aggFun >>= (@?= DuckDBSuccess)
                                        duckdb_register_aggregate_function_set conn set >>= (@?= DuckDBSuccess)

                                        withConstCString "SELECT haskell_sum_set(v) FROM (VALUES (4), (5), (6)) t(v)" \sql ->
                                            withResultCString conn sql \resPtr ->
                                                duckdb_value_int32 resPtr 0 0 >>= (@?= 15)

aggregateErrorPropagation :: TestTree
aggregateErrorPropagation =
    testCase "callbacks can signal errors" $
        runInBoundThread do
            let config = defaultAggregateConfig{cfgFailOnNegative = 1}
            withDatabase \db ->
                withConnection db \conn ->
                    withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) \intType ->
                        withAggregateConfig config \configPtr ->
                            withAggregateFunction \aggFun ->
                                withCallbacks \cbs -> do
                                    setupAggregateFunction aggFun cbs intType "no_negatives"
                                    duckdb_aggregate_function_set_extra_info aggFun configPtr (coerce (nullFunPtr :: FunPtr Void))
                                    duckdb_register_aggregate_function conn aggFun >>= (@?= DuckDBSuccess)

                                    withConstCString "SELECT no_negatives(v) FROM (VALUES (1), (-5)) t(v)" \sql ->
                                        alloca \resPtr -> do
                                            errState <- duckdb_query conn sql resPtr
                                            errState @?= DuckDBError

                                            msgPtr <- duckdb_result_error resPtr
                                            errMsg <- (peekCString . coerce) msgPtr
                                            assertBool "expected negative error message" ("negatives not allowed" `isInfixOf` errMsg)

                                            duckdb_destroy_result resPtr

specialHandlingNulls :: TestTree
specialHandlingNulls =
    testCase "special handling allows returning NULL when all inputs are NULL" $
        runInBoundThread do
            let config = defaultAggregateConfig{cfgNullIfAllInvalid = 1}
            withDatabase \db ->
                withConnection db \conn ->
                    withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) \intType ->
                        withAggregateConfig config \configPtr ->
                            withAggregateFunction \aggFun ->
                                withCallbacks \cbs -> do
                                    setupAggregateFunction aggFun cbs intType "nullable_sum"
                                    duckdb_aggregate_function_set_extra_info aggFun configPtr (coerce (nullFunPtr :: FunPtr Void))
                                    duckdb_aggregate_function_set_special_handling aggFun
                                    duckdb_register_aggregate_function conn aggFun >>= (@?= DuckDBSuccess)

                                    withConstCString "SELECT nullable_sum(v) FROM (VALUES (CAST(NULL AS INTEGER))) t(v)" \sql ->
                                        withResultCString conn sql \resPtr -> do
                                            isNull <- duckdb_value_is_null resPtr 0 0
                                            cbToBool isNull @?= True

-- Aggregate state -----------------------------------------------------------

data SumState = SumState
    { ssTotal :: Int32
    , ssSeen :: Int32
    , ssNulls :: Int32
    }

initialSumState :: SumState
initialSumState = SumState 0 0 0

sumStateSize :: Int
sumStateSize = 3 * sizeOf (undefined :: Int32)

type SumStatePtr = Ptr Int32

readSumState :: SumStatePtr -> IO SumState
readSumState ptr = do
    total <- peekElemOff ptr 0
    seen <- peekElemOff ptr 1
    nulls <- peekElemOff ptr 2
    pure SumState{ssTotal = total, ssSeen = seen, ssNulls = nulls}

writeSumState :: SumStatePtr -> SumState -> IO ()
writeSumState ptr SumState{ssTotal = total, ssSeen = seen, ssNulls = nulls} = do
    pokeElemOff ptr 0 total
    pokeElemOff ptr 1 seen
    pokeElemOff ptr 2 nulls

-- Aggregate configuration ---------------------------------------------------

data AggregateConfig = AggregateConfig
    { cfgBonus :: Int32
    , cfgFailOnNegative :: Int32
    , cfgReturnNullCount :: Int32
    , cfgNullIfAllInvalid :: Int32
    }

defaultAggregateConfig :: AggregateConfig
defaultAggregateConfig = AggregateConfig 0 0 0 0

type AggregateConfigPtr = Ptr Int32

aggregateConfigSize :: Int
aggregateConfigSize = 4 * sizeOf (undefined :: Int32)

writeAggregateConfig :: AggregateConfigPtr -> AggregateConfig -> IO ()
writeAggregateConfig ptr AggregateConfig{cfgBonus = bonus, cfgFailOnNegative = failNeg, cfgReturnNullCount = returnNulls, cfgNullIfAllInvalid = nullAll} = do
    pokeElemOff ptr 0 bonus
    pokeElemOff ptr 1 failNeg
    pokeElemOff ptr 2 returnNulls
    pokeElemOff ptr 3 nullAll

peekAggregateConfig :: AggregateConfigPtr -> IO AggregateConfig
peekAggregateConfig ptr = do
    bonus <- peekElemOff ptr 0
    failNeg <- peekElemOff ptr 1
    returnNulls <- peekElemOff ptr 2
    nullAll <- peekElemOff ptr 3
    pure
        AggregateConfig
            { cfgBonus = bonus
            , cfgFailOnNegative = failNeg
            , cfgReturnNullCount = returnNulls
            , cfgNullIfAllInvalid = nullAll
            }

configFromInfo :: Duckdb_function_info -> IO AggregateConfig
configFromInfo info = do
    raw <- duckdb_aggregate_function_get_extra_info info
    if raw == (coerce (nullPtr :: Ptr Void))
        then pure defaultAggregateConfig
        else peekAggregateConfig (coerce raw)

withAggregateConfig :: AggregateConfig -> (Ptr Void -> IO a) -> IO a
withAggregateConfig cfg action =
    bracket acquire freeConfig (action . coerce)
  where
    acquire = do
        ptr <- mallocBytes aggregateConfigSize :: IO AggregateConfigPtr
        writeAggregateConfig ptr cfg
        pure ptr
    freeConfig = free

shouldFailOnNegative :: AggregateConfig -> Bool
shouldFailOnNegative AggregateConfig{cfgFailOnNegative = flag} = flag /= 0

shouldReturnNullCount :: AggregateConfig -> Bool
shouldReturnNullCount AggregateConfig{cfgReturnNullCount = flag} = flag /= 0

shouldMarkNullWhenAllInvalid :: AggregateConfig -> Bool
shouldMarkNullWhenAllInvalid AggregateConfig{cfgNullIfAllInvalid = flag} = flag /= 0

-- Callback implementations --------------------------------------------------

data Callbacks = Callbacks
    { cbStateSize :: Duckdb_aggregate_state_size
    , cbInit :: Duckdb_aggregate_init_t
    , cbUpdate :: Duckdb_aggregate_update_t
    , cbCombine :: Duckdb_aggregate_combine_t
    , cbFinalize :: Duckdb_aggregate_finalize_t
    , cbDestroy :: Duckdb_aggregate_destroy_t
    }

withCallbacks :: (Callbacks -> IO a) -> IO a
withCallbacks = bracket acquire release
  where
    acquire = do
        sizeFun <- mkStateSizeFun stateSizeFun
        initFun <- mkInitFun initCallback
        updateFun <- mkUpdateFun updateCallback
        combineFun <- mkCombineFun combineCallback
        finalizeFun <- mkFinalizeFun finalizeCallback
        destroyFun <- mkDestroyFun destroyCallback
        pure
            Callbacks
                { cbStateSize = sizeFun
                , cbInit = initFun
                , cbUpdate = updateFun
                , cbCombine = combineFun
                , cbFinalize = finalizeFun
                , cbDestroy = destroyFun
                }
    release Callbacks{cbStateSize = sizeFun, cbInit = initFun, cbUpdate = updateFun, cbCombine = combineFun, cbFinalize = finalizeFun, cbDestroy = destroyFun} = do
        (freeHaskellFunPtr . coerce) sizeFun
        (freeHaskellFunPtr . coerce) initFun
        (freeHaskellFunPtr . coerce) updateFun
        (freeHaskellFunPtr . coerce) combineFun
        (freeHaskellFunPtr . coerce) finalizeFun
        (freeHaskellFunPtr . coerce) destroyFun

setupAggregateFunction :: Duckdb_aggregate_function -> Callbacks -> Duckdb_logical_type -> String -> IO ()
setupAggregateFunction aggFun Callbacks{cbStateSize = sizeFun, cbInit = initFun, cbUpdate = updateFun, cbCombine = combineFun, cbFinalize = finalizeFun, cbDestroy = destroyFun} intType name = do
    withConstCString name $ \cname -> duckdb_aggregate_function_set_name aggFun cname
    duckdb_aggregate_function_add_parameter aggFun intType
    duckdb_aggregate_function_set_return_type aggFun intType
    duckdb_aggregate_function_set_functions aggFun sizeFun initFun updateFun combineFun finalizeFun
    duckdb_aggregate_function_set_destructor aggFun destroyFun

stateSizeFun :: Duckdb_function_info -> IO Idx_t
stateSizeFun _ = pure (fromIntegral (sizeOf ((coerce (nullPtr :: Ptr Void)) :: Ptr Void)))

initCallback :: Duckdb_function_info -> Duckdb_aggregate_state -> IO ()
initCallback _ state = do
    raw <- duckdb_malloc (fromIntegral sumStateSize)
    let storage = coerce raw :: SumStatePtr
    writeSumState storage initialSumState
    writeStateValuePtr state storage

updateCallback :: Duckdb_function_info -> Duckdb_data_chunk -> Ptr Duckdb_aggregate_state -> IO ()
updateCallback info chunk stateArrayPtr = do
    rowCount <- duckdb_data_chunk_get_size chunk
    vec <- duckdb_data_chunk_get_vector chunk 0
    dataPtr <- vectorDataPtr vec
    validity <- duckdb_vector_get_validity vec
    config <- configFromInfo info

    let rowCountInt = fromIntegral rowCount
    forM_ [0 .. rowCountInt - 1] \i -> do
        statePtr <- peekElemOff stateArrayPtr i
        storage <- readStateValuePtr statePtr
        current <- readSumState storage
        let seen' = ssSeen current + 1
            idx = fromIntegral i

        isValid <- rowIsValid validity idx
        if not isValid
            then writeSumState storage current{ssSeen = seen', ssNulls = ssNulls current + 1}
            else do
                value <- peekElemOff dataPtr i
                if shouldFailOnNegative config && value < 0
                    then do
                        withConstCString "negatives not allowed" $ \errMsg ->
                            duckdb_aggregate_function_set_error info errMsg
                        writeSumState storage current{ssSeen = seen'}
                    else
                        let total' = ssTotal current + value + cfgBonus config
                         in writeSumState storage SumState{ssTotal = total', ssSeen = seen', ssNulls = ssNulls current}

combineCallback :: Duckdb_function_info -> Ptr Duckdb_aggregate_state -> Ptr Duckdb_aggregate_state -> Idx_t -> IO ()
combineCallback _ sourceStates targetStates count =
    forM_ [0 .. fromIntegral count - 1] \i -> do
        sourceState <- peekElemOff sourceStates i
        targetState <- peekElemOff targetStates i
        sourcePtr <- readStateValuePtr sourceState
        targetPtr <- readStateValuePtr targetState
        source <- readSumState sourcePtr
        target <- readSumState targetPtr

        let combined =
                SumState
                    { ssTotal = ssTotal source + ssTotal target
                    , ssSeen = ssSeen source + ssSeen target
                    , ssNulls = ssNulls source + ssNulls target
                    }
        writeSumState targetPtr combined

finalizeCallback :: Duckdb_function_info -> Ptr Duckdb_aggregate_state -> Duckdb_vector -> Idx_t -> Idx_t -> IO ()
finalizeCallback info stateArrayPtr outVec _ offset = do
    state <- peekElemOff stateArrayPtr (fromIntegral offset)
    storage <- readStateValuePtr state
    SumState{ssTotal = total, ssSeen = seen, ssNulls = nulls} <- readSumState storage
    config <- configFromInfo info

    outPtr <- vectorDataPtr outVec
    let resultValue = if shouldReturnNullCount config then nulls else total
    pokeElemOff outPtr (fromIntegral offset) resultValue

    let allInvalid = seen > 0 && seen == nulls
    when (shouldMarkNullWhenAllInvalid config && allInvalid) do
        duckdb_vector_ensure_validity_writable outVec
        validity <- duckdb_vector_get_validity outVec
        duckdb_validity_set_row_invalid validity offset

destroyCallback :: Ptr Duckdb_aggregate_state -> Idx_t -> IO ()
destroyCallback states count =
    forM_ [0 .. fromIntegral count - 1] \i -> do
        state <- peekElemOff states i
        storage <- readStateValuePtr state
        when (storage /= (coerce (nullPtr :: Ptr Void))) $
            duckdb_free (coerce storage)

-- Helper pointer accessors --------------------------------------------------

writeStateValuePtr :: Duckdb_aggregate_state -> SumStatePtr -> IO ()
writeStateValuePtr state ptr =
    poke (coerce state :: Ptr (Ptr Void)) (coerce ptr)

readStateValuePtr :: Duckdb_aggregate_state -> IO SumStatePtr
readStateValuePtr state = do
    raw <- peek (coerce state :: Ptr (Ptr Void))
    pure (coerce raw)

vectorDataPtr :: Duckdb_vector -> IO (Ptr Int32)
vectorDataPtr vec = coerce <$> duckdb_vector_get_data vec

rowIsValid :: Ptr Word64 -> Idx_t -> IO Bool
rowIsValid validity idx
    | validity == (coerce (nullPtr :: Ptr Void)) = pure True
    | otherwise = cbToBool <$> duckdb_validity_row_is_valid validity idx

cbToBool :: CBool -> Bool
cbToBool (CBool v) = v /= 0

-- Wrapper builders ----------------------------------------------------------

mkStateSizeFun :: (Duckdb_function_info -> IO Idx_t) -> IO Duckdb_aggregate_state_size
mkStateSizeFun = fmap Duckdb_aggregate_state_size . toFunPtr . Duckdb_aggregate_state_size_Aux

mkInitFun :: (Duckdb_function_info -> Duckdb_aggregate_state -> IO ()) -> IO Duckdb_aggregate_init_t
mkInitFun = fmap Duckdb_aggregate_init_t . toFunPtr . Duckdb_aggregate_init_t_Aux

mkUpdateFun :: (Duckdb_function_info -> Duckdb_data_chunk -> Ptr Duckdb_aggregate_state -> IO ()) -> IO Duckdb_aggregate_update_t
mkUpdateFun = fmap Duckdb_aggregate_update_t . toFunPtr . Duckdb_aggregate_update_t_Aux

mkCombineFun :: (Duckdb_function_info -> Ptr Duckdb_aggregate_state -> Ptr Duckdb_aggregate_state -> Idx_t -> IO ()) -> IO Duckdb_aggregate_combine_t
mkCombineFun = fmap Duckdb_aggregate_combine_t . toFunPtr . Duckdb_aggregate_combine_t_Aux

mkFinalizeFun :: (Duckdb_function_info -> Ptr Duckdb_aggregate_state -> Duckdb_vector -> Idx_t -> Idx_t -> IO ()) -> IO Duckdb_aggregate_finalize_t
mkFinalizeFun = fmap Duckdb_aggregate_finalize_t . toFunPtr . Duckdb_aggregate_finalize_t_Aux

mkDestroyFun :: (Ptr Duckdb_aggregate_state -> Idx_t -> IO ()) -> IO Duckdb_aggregate_destroy_t
mkDestroyFun = fmap Duckdb_aggregate_destroy_t . toFunPtr . Duckdb_aggregate_destroy_t_Aux

-- Resource helpers ----------------------------------------------------------

withAggregateFunction :: (Duckdb_aggregate_function -> IO a) -> IO a
withAggregateFunction = bracket duckdb_create_aggregate_function destroy
  where
    destroy fun = alloca \ptr -> poke ptr fun >> duckdb_destroy_aggregate_function ptr

withAggregateFunctionSet :: (ConstPtr CChar) -> (Duckdb_aggregate_function_set -> IO a) -> IO a
withAggregateFunctionSet name = bracket acquire release
  where
    acquire = duckdb_create_aggregate_function_set name
    release set = alloca \ptr -> poke ptr set >> duckdb_destroy_aggregate_function_set ptr
