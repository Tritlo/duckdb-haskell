{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module AggregateFunctionsTest (tests) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withDatabase, withLogicalType, withResultCString)

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
                    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
                        withAggregateFunction \aggFun ->
                            withCallbacks \cbs -> do
                                setupAggregateFunction aggFun cbs intType "haskell_sum"
                                c_duckdb_register_aggregate_function conn aggFun >>= (@?= DuckDBSuccess)
                                withCString "SELECT haskell_sum(v) FROM (VALUES (1), (2), (3)) t(v)" \sql ->
                                    withResultCString conn sql \resPtr ->
                                        c_duckdb_value_int32 resPtr 0 0 >>= (@?= 6)

extraInfoAggregate :: TestTree
extraInfoAggregate =
    testCase "extra info is visible inside callbacks" $
        runInBoundThread do
            let config = defaultAggregateConfig{cfgBonus = 2}
            withDatabase \db ->
                withConnection db \conn ->
                    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
                        withAggregateConfig config \configPtr ->
                            withAggregateFunction \aggFun ->
                                withCallbacks \cbs -> do
                                    setupAggregateFunction aggFun cbs intType "haskell_bonus_sum"
                                    c_duckdb_aggregate_function_set_extra_info aggFun configPtr nullFunPtr

                                    c_duckdb_register_aggregate_function conn aggFun >>= (@?= DuckDBSuccess)

                                    withCString "SELECT haskell_bonus_sum(v) FROM (VALUES (1), (2), (3)) t(v)" \sql ->
                                        withResultCString conn sql \resPtr ->
                                            c_duckdb_value_int32 resPtr 0 0 >>= (@?= 12)

aggregateFunctionSet :: TestTree
aggregateFunctionSet =
    testCase "aggregate function set registers overload" $
        runInBoundThread do
            withDatabase \db ->
                withConnection db \conn ->
                    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
                        withAggregateFunction \aggFun ->
                            withCallbacks \cbs -> do
                                setupAggregateFunction aggFun cbs intType "haskell_sum_set"
                                withCString "haskell_sum_set" \setName ->
                                    withAggregateFunctionSet setName \set -> do
                                        c_duckdb_add_aggregate_function_to_set set aggFun >>= (@?= DuckDBSuccess)
                                        c_duckdb_register_aggregate_function_set conn set >>= (@?= DuckDBSuccess)

                                        withCString "SELECT haskell_sum_set(v) FROM (VALUES (4), (5), (6)) t(v)" \sql ->
                                            withResultCString conn sql \resPtr ->
                                                c_duckdb_value_int32 resPtr 0 0 >>= (@?= 15)

aggregateErrorPropagation :: TestTree
aggregateErrorPropagation =
    testCase "callbacks can signal errors" $
        runInBoundThread do
            let config = defaultAggregateConfig{cfgFailOnNegative = 1}
            withDatabase \db ->
                withConnection db \conn ->
                    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
                        withAggregateConfig config \configPtr ->
                            withAggregateFunction \aggFun ->
                                withCallbacks \cbs -> do
                                    setupAggregateFunction aggFun cbs intType "no_negatives"
                                    c_duckdb_aggregate_function_set_extra_info aggFun configPtr nullFunPtr
                                    c_duckdb_register_aggregate_function conn aggFun >>= (@?= DuckDBSuccess)

                                    withCString "SELECT no_negatives(v) FROM (VALUES (1), (-5)) t(v)" \sql ->
                                        alloca \resPtr -> do
                                            errState <- c_duckdb_query conn sql resPtr
                                            errState @?= DuckDBError

                                            msgPtr <- c_duckdb_result_error resPtr
                                            errMsg <- peekCString msgPtr
                                            assertBool "expected negative error message" ("negatives not allowed" `isInfixOf` errMsg)

                                            c_duckdb_destroy_result resPtr

specialHandlingNulls :: TestTree
specialHandlingNulls =
    testCase "special handling allows returning NULL when all inputs are NULL" $
        runInBoundThread do
            let config = defaultAggregateConfig{cfgNullIfAllInvalid = 1}
            withDatabase \db ->
                withConnection db \conn ->
                    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
                        withAggregateConfig config \configPtr ->
                            withAggregateFunction \aggFun ->
                                withCallbacks \cbs -> do
                                    setupAggregateFunction aggFun cbs intType "nullable_sum"
                                    c_duckdb_aggregate_function_set_extra_info aggFun configPtr nullFunPtr
                                    c_duckdb_aggregate_function_set_special_handling aggFun
                                    c_duckdb_register_aggregate_function conn aggFun >>= (@?= DuckDBSuccess)

                                    withCString "SELECT nullable_sum(v) FROM (VALUES (CAST(NULL AS INTEGER))) t(v)" \sql ->
                                        withResultCString conn sql \resPtr -> do
                                            isNull <- c_duckdb_value_is_null resPtr 0 0
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

configFromInfo :: DuckDBFunctionInfo -> IO AggregateConfig
configFromInfo info = do
    raw <- c_duckdb_aggregate_function_get_extra_info info
    if raw == nullPtr
        then pure defaultAggregateConfig
        else peekAggregateConfig (castPtr raw)

withAggregateConfig :: AggregateConfig -> (Ptr () -> IO a) -> IO a
withAggregateConfig cfg action =
    bracket acquire freeConfig (action . castPtr)
  where
    acquire = do
        ptr <- mallocBytes aggregateConfigSize :: IO AggregateConfigPtr
        writeAggregateConfig ptr cfg
        pure ptr
    freeConfig ptr = free ptr

shouldFailOnNegative :: AggregateConfig -> Bool
shouldFailOnNegative AggregateConfig{cfgFailOnNegative = flag} = flag /= 0

shouldReturnNullCount :: AggregateConfig -> Bool
shouldReturnNullCount AggregateConfig{cfgReturnNullCount = flag} = flag /= 0

shouldMarkNullWhenAllInvalid :: AggregateConfig -> Bool
shouldMarkNullWhenAllInvalid AggregateConfig{cfgNullIfAllInvalid = flag} = flag /= 0

-- Callback implementations --------------------------------------------------

data Callbacks = Callbacks
    { cbStateSize :: DuckDBAggregateStateSizeFun
    , cbInit :: DuckDBAggregateInitFun
    , cbUpdate :: DuckDBAggregateUpdateFun
    , cbCombine :: DuckDBAggregateCombineFun
    , cbFinalize :: DuckDBAggregateFinalizeFun
    , cbDestroy :: DuckDBAggregateDestroyFun
    }

withCallbacks :: (Callbacks -> IO a) -> IO a
withCallbacks action =
    bracket acquire release action
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
        freeHaskellFunPtr sizeFun
        freeHaskellFunPtr initFun
        freeHaskellFunPtr updateFun
        freeHaskellFunPtr combineFun
        freeHaskellFunPtr finalizeFun
        freeHaskellFunPtr destroyFun

setupAggregateFunction :: DuckDBAggregateFunction -> Callbacks -> DuckDBLogicalType -> String -> IO ()
setupAggregateFunction aggFun Callbacks{cbStateSize = sizeFun, cbInit = initFun, cbUpdate = updateFun, cbCombine = combineFun, cbFinalize = finalizeFun, cbDestroy = destroyFun} intType name = do
    withCString name \cname -> c_duckdb_aggregate_function_set_name aggFun cname
    c_duckdb_aggregate_function_add_parameter aggFun intType
    c_duckdb_aggregate_function_set_return_type aggFun intType
    c_duckdb_aggregate_function_set_functions aggFun sizeFun initFun updateFun combineFun finalizeFun
    c_duckdb_aggregate_function_set_destructor aggFun destroyFun

stateSizeFun :: DuckDBFunctionInfo -> IO DuckDBIdx
stateSizeFun _ = pure (fromIntegral (sizeOf (nullPtr :: Ptr ())))

initCallback :: DuckDBFunctionInfo -> DuckDBAggregateState -> IO ()
initCallback _ state = do
    raw <- c_duckdb_malloc (fromIntegral sumStateSize)
    let storage = castPtr raw :: SumStatePtr
    writeSumState storage initialSumState
    writeStateValuePtr state storage

updateCallback :: DuckDBFunctionInfo -> DuckDBDataChunk -> Ptr DuckDBAggregateState -> IO ()
updateCallback info chunk stateArrayPtr = do
    rowCount <- c_duckdb_data_chunk_get_size chunk
    vec <- c_duckdb_data_chunk_get_vector chunk 0
    dataPtr <- vectorDataPtr vec
    validity <- c_duckdb_vector_get_validity vec
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
                        withCString "negatives not allowed" \errMsg ->
                            c_duckdb_aggregate_function_set_error info errMsg
                        writeSumState storage current{ssSeen = seen'}
                    else
                        let total' = ssTotal current + value + cfgBonus config
                         in writeSumState storage SumState{ssTotal = total', ssSeen = seen', ssNulls = ssNulls current}

combineCallback :: DuckDBFunctionInfo -> Ptr DuckDBAggregateState -> Ptr DuckDBAggregateState -> DuckDBIdx -> IO ()
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

finalizeCallback :: DuckDBFunctionInfo -> Ptr DuckDBAggregateState -> DuckDBVector -> DuckDBIdx -> DuckDBIdx -> IO ()
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
        c_duckdb_vector_ensure_validity_writable outVec
        validity <- c_duckdb_vector_get_validity outVec
        c_duckdb_validity_set_row_invalid validity offset

destroyCallback :: Ptr DuckDBAggregateState -> DuckDBIdx -> IO ()
destroyCallback states count =
    forM_ [0 .. fromIntegral count - 1] \i -> do
        state <- peekElemOff states i
        storage <- readStateValuePtr state
        when (storage /= nullPtr) $
            c_duckdb_free (castPtr storage)

-- Helper pointer accessors --------------------------------------------------

writeStateValuePtr :: DuckDBAggregateState -> SumStatePtr -> IO ()
writeStateValuePtr state ptr =
    poke (castPtr state :: Ptr (Ptr ())) (castPtr ptr)

readStateValuePtr :: DuckDBAggregateState -> IO SumStatePtr
readStateValuePtr state = do
    raw <- peek (castPtr state :: Ptr (Ptr ()))
    pure (castPtr raw)

vectorDataPtr :: DuckDBVector -> IO (Ptr Int32)
vectorDataPtr vec = castPtr <$> c_duckdb_vector_get_data vec

rowIsValid :: Ptr Word64 -> DuckDBIdx -> IO Bool
rowIsValid validity idx
    | validity == nullPtr = pure True
    | otherwise = cbToBool <$> c_duckdb_validity_row_is_valid validity idx

cbToBool :: CBool -> Bool
cbToBool (CBool v) = v /= 0

-- Wrapper builders ----------------------------------------------------------

foreign import ccall safe "wrapper"
    mkStateSizeFun :: (DuckDBFunctionInfo -> IO DuckDBIdx) -> IO DuckDBAggregateStateSizeFun

foreign import ccall safe "wrapper"
    mkInitFun :: (DuckDBFunctionInfo -> DuckDBAggregateState -> IO ()) -> IO DuckDBAggregateInitFun

foreign import ccall safe "wrapper"
    mkUpdateFun :: (DuckDBFunctionInfo -> DuckDBDataChunk -> Ptr DuckDBAggregateState -> IO ()) -> IO DuckDBAggregateUpdateFun

foreign import ccall safe "wrapper"
    mkCombineFun :: (DuckDBFunctionInfo -> Ptr DuckDBAggregateState -> Ptr DuckDBAggregateState -> DuckDBIdx -> IO ()) -> IO DuckDBAggregateCombineFun

foreign import ccall safe "wrapper"
    mkFinalizeFun :: (DuckDBFunctionInfo -> Ptr DuckDBAggregateState -> DuckDBVector -> DuckDBIdx -> DuckDBIdx -> IO ()) -> IO DuckDBAggregateFinalizeFun

foreign import ccall safe "wrapper"
    mkDestroyFun :: (Ptr DuckDBAggregateState -> DuckDBIdx -> IO ()) -> IO DuckDBAggregateDestroyFun

-- Resource helpers ----------------------------------------------------------

withAggregateFunction :: (DuckDBAggregateFunction -> IO a) -> IO a
withAggregateFunction action = bracket c_duckdb_create_aggregate_function destroy action
  where
    destroy fun = alloca \ptr -> poke ptr fun >> c_duckdb_destroy_aggregate_function ptr

withAggregateFunctionSet :: CString -> (DuckDBAggregateFunctionSet -> IO a) -> IO a
withAggregateFunctionSet name action = bracket acquire release action
  where
    acquire = c_duckdb_create_aggregate_function_set name
    release set = alloca \ptr -> poke ptr set >> c_duckdb_destroy_aggregate_function_set ptr
