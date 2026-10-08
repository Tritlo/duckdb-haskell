{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module TableFunctionsTest (tests) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Char (isAsciiUpper)
import Data.Coerce (coerce)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..), CChar)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase, withLogicalType, withResultCString, withValue)

tests :: TestTree
tests =
    testGroup
        "Table Functions"
        [tableFunctionLifecycle]

tableFunctionLifecycle :: TestTree
tableFunctionLifecycle =
    testCase "table function lifecycle covers core callbacks" $
        runInBoundThread do
            withDatabase \db ->
                withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_BIGINT)) \bigint ->
                    withHarness bigint \harness@TableHarness{} -> do
                        (bindError, execError, initError) <-
                            withConnection db \conn ->
                                withTableFunction \tableFun -> do
                                    configureTableFunction tableFun harness
                                    duckdb_register_table_function conn tableFun >>= (@?= DuckDBSuccess)

                                    withConstCString "SELECT value, double_value FROM haskell_numbers(5, start => 2)" \sql ->
                                        withResultCString conn sql \resPtr -> do
                                            duckdb_column_count resPtr >>= (@?= 2)
                                            duckdb_row_count resPtr >>= (@?= 5)
                                            forM_ [0 .. 4] \idx -> do
                                                duckdb_value_int64 resPtr 0 idx >>= (@?= 2 + fromIntegral idx)
                                                duckdb_value_int64 resPtr 1 idx >>= (@?= 2 * (2 + fromIntegral idx))

                                    withConstCString "SELECT value FROM haskell_numbers(3)" \sql ->
                                        withResultCString conn sql \resPtr -> do
                                            duckdb_column_count resPtr >>= (@?= 1)
                                            forM_ (zip [0 ..] [0 .. 2]) \(idx, expected) ->
                                                duckdb_value_int64 resPtr 0 idx >>= (@?= expected)

                                    bindError <-
                                        withConstCString "SELECT * FROM haskell_numbers(0)" \sql ->
                                            alloca \resPtr -> do
                                                state <- duckdb_query conn sql resPtr
                                                errPtr <- duckdb_result_error resPtr
                                                msg <- peekCStringMaybe errPtr
                                                duckdb_destroy_result resPtr
                                                pure (state, msg)

                                    execError <-
                                        withConstCString "SELECT * FROM haskell_numbers(5, fail_at => 3)" \sql ->
                                            alloca \resPtr -> do
                                                state <- duckdb_query conn sql resPtr
                                                errPtr <- duckdb_result_error resPtr
                                                msg <- peekCStringMaybe errPtr
                                                duckdb_destroy_result resPtr
                                                pure (state, msg)

                                    initError <-
                                        withConstCString "SELECT * FROM haskell_numbers(2, force_init_error => 1)" \sql ->
                                            alloca \resPtr -> do
                                                state <- duckdb_query conn sql resPtr
                                                errPtr <- duckdb_result_error resPtr
                                                msg <- peekCStringMaybe errPtr
                                                duckdb_destroy_result resPtr
                                                pure (state, msg)

                                    pure (bindError, execError, initError)

                        stats <- snapshotStats harness
                        hsBindFreed stats @?= 3
                        hsInitFreed stats @?= 3
                        hsLocalFreed stats @?= 3
                        assertBool "client context seen" (hsClientContextSeen stats)
                        hsCounts stats @?= [5, 3, 0, 5, 2]
                        hsProjectionHistory stats
                            @?= [ ProjectionSnapshot 2 [0, 1]
                                , ProjectionSnapshot 1 [0]
                                , ProjectionSnapshot 2 [0, 1]
                                ]
                        fst bindError @?= DuckDBError
                        assertBool "bind error mentions positivity" ("positive" `contains` snd bindError)
                        fst execError @?= DuckDBError
                        assertBool "execution error mentions table function" ("execution" `contains` snd execError)
                        fst initError @?= DuckDBError
                        assertBool "init error mentions initialization" ("init" `contains` snd initError)

-- Harness --------------------------------------------------------------------

data ProjectionSnapshot = ProjectionSnapshot
    { psCount :: !Int
    , psIndices :: ![Int]
    }
    deriving (Eq, Show)

data ExecutionMode
    = ModeUnset
    | ModeNormal
    | ModeInitError
    | ModeRejected
    deriving (Eq, Show)

data TableHarness = TableHarness
    { thColumnType :: !Duckdb_logical_type
    , thExtraBuffer :: !(Ptr Void)
    , thExtraCount :: !(IORef Int)
    , thBindCount :: !(IORef Int)
    , thInitCount :: !(IORef Int)
    , thLocalCount :: !(IORef Int)
    , thBindPtr :: !(IORef (Maybe (Ptr Void)))
    , thInitPtr :: !(IORef (Maybe (Ptr Void)))
    , thLocalPtr :: !(IORef (Maybe (Ptr Void)))
    , thProjectionCurrent :: !(IORef (Maybe ProjectionSnapshot))
    , thProjectionHistory :: !(IORef [ProjectionSnapshot])
    , thClientContextSeen :: !(IORef Bool)
    , thModeRef :: !(IORef ExecutionMode)
    , thCountHistory :: !(IORef [Int64])
    , thCallbacks :: !TableCallbacks
    , thDeletes :: !DeleteCallbacks
    }

data TableCallbacks = TableCallbacks
    { tcBind :: !Duckdb_table_function_bind_t
    , tcInit :: !Duckdb_table_function_init_t
    , tcLocalInit :: !Duckdb_table_function_init_t
    , tcExecute :: !Duckdb_table_function_t
    }

data DeleteCallbacks = DeleteCallbacks
    { dcExtra :: !Duckdb_delete_callback_t
    , dcBind :: !Duckdb_delete_callback_t
    , dcInit :: !Duckdb_delete_callback_t
    , dcLocal :: !Duckdb_delete_callback_t
    }

data HarnessStats = HarnessStats
    { hsExtraFreed :: !Int
    , hsBindFreed :: !Int
    , hsInitFreed :: !Int
    , hsLocalFreed :: !Int
    , hsClientContextSeen :: !Bool
    , hsProjectionHistory :: ![ProjectionSnapshot]
    , hsCounts :: ![Int64]
    }
    deriving (Eq, Show)

withHarness :: Duckdb_logical_type -> (TableHarness -> IO a) -> IO a
withHarness columnType action = do
    rawExtraBuffer <- mallocBytes sizeOfInt64
    let extraBuffer = coerce rawExtraBuffer :: Ptr Void
    poke (coerce extraBuffer :: Ptr Int64) extraInfoSentinel
    extraCount <- newIORef 0
    bindCount <- newIORef 0
    initCount <- newIORef 0
    localCount <- newIORef 0
    bindPtrRef <- newIORef Nothing
    initPtrRef <- newIORef Nothing
    localPtrRef <- newIORef Nothing
    projectionCurrent <- newIORef Nothing
    projectionHistory <- newIORef []
    clientContextSeen <- newIORef False
    modeRef <- newIORef ModeUnset
    countHistory <- newIORef []

    let takeInt64 ptr = peekElemOff (coerce ptr :: Ptr Int64)
        putInt64 ptr = pokeElemOff (coerce ptr :: Ptr Int64)

    extraDelete <-
        mkDeleteCallback \ptr -> do
            modifyIORef' extraCount (+ 1)
            when (ptr /= (coerce (nullPtr :: Ptr Void))) (free ptr)

    bindDelete <-
        mkDeleteCallback \ptr -> do
            modifyIORef' bindCount (+ 1)
            writeIORef bindPtrRef Nothing
            when (ptr /= (coerce (nullPtr :: Ptr Void))) (free ptr)

    initDelete <-
        mkDeleteCallback \ptr -> do
            modifyIORef' initCount (+ 1)
            writeIORef initPtrRef Nothing
            when (ptr /= (coerce (nullPtr :: Ptr Void))) (free ptr)

    localDelete <-
        mkDeleteCallback \ptr -> do
            modifyIORef' localCount (+ 1)
            writeIORef localPtrRef Nothing
            when (ptr /= (coerce (nullPtr :: Ptr Void))) (free ptr)

    let resetLifecycle = do
            writeIORef projectionCurrent Nothing
            writeIORef modeRef ModeUnset
            writeIORef bindPtrRef Nothing
            writeIORef initPtrRef Nothing
            writeIORef localPtrRef Nothing

        recordProjection snapshot = do
            writeIORef projectionCurrent (Just snapshot)
            modifyIORef' projectionHistory (<> [snapshot])

        bindCallback info = do
            resetLifecycle
            writeIORef modeRef ModeRejected
            extraPtr <- duckdb_bind_get_extra_info info
            extraPtr @?= extraBuffer
            sentinel <- peek (coerce extraPtr :: Ptr Int64)
            sentinel @?= extraInfoSentinel

            alloca \ctxPtr -> do
                poke ctxPtr (coerce (nullPtr :: Ptr Void))
                duckdb_table_function_get_client_context info ctxPtr
                ctx <- peek ctxPtr
                assertBool "client context should not be null" (ctx /= (coerce (nullPtr :: Ptr Void)))
                cid <- duckdb_client_context_get_connection_id ctx
                assertBool "connection id should be non-negative" (cid >= 0)
                duckdb_destroy_client_context ctxPtr
                writeIORef clientContextSeen True

            paramCount <- duckdb_bind_get_parameter_count info
            paramCount @?= 1

            withValue (duckdb_bind_get_parameter info 0) \value -> do
                countValue <- duckdb_get_int64 value
                modifyIORef' countHistory (<> [countValue])
                if countValue <= 0
                    then withConstCString "count must be positive" $ \msg -> duckdb_bind_set_error info msg
                    else do
                        startValue <-
                            withConstCString "start" \name ->
                                fmap (fromMaybe 0) (withOptionalValue (duckdb_bind_get_named_parameter info name) duckdb_get_int64)
                        failAtValue <-
                            withConstCString "fail_at" \name ->
                                fmap (fromMaybe (-1)) (withOptionalValue (duckdb_bind_get_named_parameter info name) duckdb_get_int64)
                        initErrValue <-
                            withConstCString "force_init_error" \name ->
                                fmap (fromMaybe 0) (withOptionalValue (duckdb_bind_get_named_parameter info name) duckdb_get_int64)
                        absentValue <-
                            withConstCString "missing" \name ->
                                withOptionalValue (duckdb_bind_get_named_parameter info name) duckdb_get_int64
                        absentValue @?= Nothing

                        withConstCString "value" \col -> duckdb_bind_add_result_column info col columnType
                        withConstCString "double_value" \col -> duckdb_bind_add_result_column info col columnType
                        duckdb_bind_set_cardinality info (fromIntegral countValue) (toCBool True)

                        if initErrValue /= 0
                            then do
                                writeIORef modeRef ModeInitError
                                writeIORef bindPtrRef Nothing
                            else do
                                cfgPtr <- mallocBytes (3 * sizeOfInt64)
                                putInt64 cfgPtr 0 startValue
                                putInt64 cfgPtr 1 countValue
                                putInt64 cfgPtr 2 failAtValue
                                duckdb_bind_set_bind_data info (coerce cfgPtr) bindDelete
                                writeIORef bindPtrRef (Just (coerce cfgPtr))
                                writeIORef modeRef ModeNormal

        initCallback info = do
            extraPtr <- duckdb_init_get_extra_info info
            extraPtr @?= extraBuffer
            mode <- readIORef modeRef
            case mode of
                ModeInitError -> do
                    bindRaw <- duckdb_init_get_bind_data info
                    bindRaw @?= (coerce (nullPtr :: Ptr Void))
                    withConstCString "init rejected missing bind data" $ \msg -> duckdb_init_set_error info msg
                ModeNormal -> do
                    bindRaw <- duckdb_init_get_bind_data info
                    stored <- readIORef bindPtrRef
                    case stored of
                        Nothing -> assertFailure "bind data expected during init"
                        Just expected -> bindRaw @?= expected

                    startValue <- takeInt64 bindRaw 0
                    countValue <- takeInt64 bindRaw 1
                    failAtValue <- takeInt64 bindRaw 2

                    columnCount <- fromIntegral <$> duckdb_init_get_column_count info
                    indices <-
                        mapM
                            (fmap (fromIntegral :: Idx_t -> Int) . duckdb_init_get_column_index info . fromIntegral)
                            [0 .. columnCount - 1]
                    recordProjection ProjectionSnapshot{psCount = columnCount, psIndices = indices}

                    statePtr <- mallocBytes (3 * sizeOfInt64)
                    putInt64 statePtr 0 startValue
                    putInt64 statePtr 1 (startValue + countValue)
                    putInt64 statePtr 2 failAtValue
                    duckdb_init_set_init_data info (coerce statePtr) initDelete
                    writeIORef initPtrRef (Just (coerce statePtr))
                    duckdb_init_set_max_threads info 1
                _ -> pure ()

        localInitCallback info = do
            extraPtr <- duckdb_init_get_extra_info info
            extraPtr @?= extraBuffer
            mode <- readIORef modeRef
            case mode of
                ModeNormal -> do
                    bindRaw <- duckdb_init_get_bind_data info
                    stored <- readIORef bindPtrRef
                    case stored of
                        Nothing -> assertFailure "bind data should be present during local init"
                        Just expected -> bindRaw @?= expected
                    localPtr <- mallocBytes sizeOfInt64
                    poke (coerce localPtr :: Ptr Int64) localInitSentinel
                    duckdb_init_set_init_data info (coerce localPtr) localDelete
                    writeIORef localPtrRef (Just (coerce localPtr))
                ModeInitError -> withConstCString "local init skipped after init error" $ \msg -> duckdb_init_set_error info msg
                _ -> pure ()

        executeCallback info chunk = do
            extraPtr <- duckdb_function_get_extra_info info
            extraPtr @?= extraBuffer

            mode <- readIORef modeRef
            case mode of
                ModeNormal -> do
                    bindRaw <- duckdb_function_get_bind_data info
                    storedBind <- readIORef bindPtrRef
                    case storedBind of
                        Nothing -> assertFailure "bind data should be recorded before execution"
                        Just expected -> bindRaw @?= expected

                    initRaw <- duckdb_function_get_init_data info
                    storedInit <- readIORef initPtrRef
                    case storedInit of
                        Nothing -> assertFailure "init data should exist during execution"
                        Just expected -> initRaw @?= expected

                    localRaw <- duckdb_function_get_local_init_data info
                    storedLocal <- readIORef localPtrRef
                    case storedLocal of
                        Nothing -> assertFailure "local init data should exist during execution"
                        Just expected -> localRaw @?= expected
                    sentinel <- peek (coerce localRaw :: Ptr Int64)
                    sentinel @?= localInitSentinel

                    snapshot <- readIORef projectionCurrent
                    ProjectionSnapshot{..} <-
                        case snapshot of
                            Nothing -> do
                                _ <- assertFailure "projection snapshot missing during execution"
                                pure (ProjectionSnapshot 0 [])
                            Just snap -> pure snap
                    assertBool "projection slots match indices" (length psIndices == psCount)

                    nextVal <- takeInt64 initRaw 0
                    endVal <- takeInt64 initRaw 1
                    failAtValue <- takeInt64 initRaw 2
                    if nextVal >= endVal
                        then duckdb_data_chunk_set_size chunk 0
                        else do
                            let remaining = endVal - nextVal
                                batch = min remaining 1024
                                chunkEnd = nextVal + batch
                            if failAtValue >= nextVal && failAtValue < chunkEnd && failAtValue >= 0
                                then do
                                    withConstCString "execution aborted by table function" $ \msg -> duckdb_function_set_error info msg
                                    duckdb_data_chunk_set_size chunk 0
                                    putInt64 initRaw 0 chunkEnd
                                else do
                                    forM_ (zip [0 :: Integer ..] psIndices) \(slot, schemaIdx) -> do
                                        vec <- duckdb_data_chunk_get_vector chunk (fromIntegral slot)
                                        dataPtr <- vectorDataPtr vec
                                        forM_ [0 :: Integer .. fromIntegral batch - 1] \offset -> do
                                            let base = nextVal + fromIntegral offset
                                                val =
                                                    case schemaIdx of
                                                        0 -> base
                                                        1 -> 2 * base
                                                        _ -> 0
                                            pokeElemOff dataPtr (fromIntegral offset) val
                                    duckdb_data_chunk_set_size chunk (fromIntegral batch)
                                    putInt64 initRaw 0 chunkEnd
                ModeInitError -> do
                    withConstCString "execution blocked by init error" $ \msg -> duckdb_function_set_error info msg
                    duckdb_data_chunk_set_size chunk 0
                ModeRejected -> do
                    withConstCString "execution not expected after bind error" $ \msg -> duckdb_function_set_error info msg
                    duckdb_data_chunk_set_size chunk 0
                ModeUnset -> do
                    withConstCString "execution invoked without prior bind" $ \msg -> duckdb_function_set_error info msg
                    duckdb_data_chunk_set_size chunk 0

    bindFun <- mkBindFun bindCallback
    initFun <- mkInitFun initCallback
    localInitFun <- mkInitFun localInitCallback
    execFun <- mkFunctionFun executeCallback

    let callbacks =
            TableCallbacks
                { tcBind = bindFun
                , tcInit = initFun
                , tcLocalInit = localInitFun
                , tcExecute = execFun
                }
        deletes =
            DeleteCallbacks
                { dcExtra = extraDelete
                , dcBind = bindDelete
                , dcInit = initDelete
                , dcLocal = localDelete
                }

    let harness =
            TableHarness
                { thColumnType = columnType
                , thExtraBuffer = extraBuffer
                , thExtraCount = extraCount
                , thBindCount = bindCount
                , thInitCount = initCount
                , thLocalCount = localCount
                , thBindPtr = bindPtrRef
                , thInitPtr = initPtrRef
                , thLocalPtr = localPtrRef
                , thProjectionCurrent = projectionCurrent
                , thProjectionHistory = projectionHistory
                , thClientContextSeen = clientContextSeen
                , thModeRef = modeRef
                , thCountHistory = countHistory
                , thCallbacks = callbacks
                , thDeletes = deletes
                }

    result <- action harness

    (freeHaskellFunPtr . coerce) bindFun
    (freeHaskellFunPtr . coerce) initFun
    (freeHaskellFunPtr . coerce) localInitFun
    (freeHaskellFunPtr . coerce) execFun
    -- DuckDB may still invoke delete callbacks while the table function tears down.
    -- Rely on process teardown to reclaim them to avoid racing with finalizers.

    pure result

withTableFunction :: (Duckdb_table_function -> IO a) -> IO a
withTableFunction = bracket duckdb_create_table_function destroy
  where
    destroy fun = alloca \ptr -> poke ptr fun >> duckdb_destroy_table_function ptr

configureTableFunction :: Duckdb_table_function -> TableHarness -> IO ()
configureTableFunction fun TableHarness{..} = do
    withConstCString "haskell_numbers" $ \name -> duckdb_table_function_set_name fun name
    duckdb_table_function_add_parameter fun thColumnType
    withConstCString "start" \n -> duckdb_table_function_add_named_parameter fun n thColumnType
    withConstCString "fail_at" \n -> duckdb_table_function_add_named_parameter fun n thColumnType
    withConstCString "force_init_error" \n -> duckdb_table_function_add_named_parameter fun n thColumnType
    -- NOTE: DuckDB expects the extra-info pointer to be populated alongside the other callbacks;
    -- leaving it unset would skip the destructor and break the lifetime assertions below.
    duckdb_table_function_set_extra_info fun thExtraBuffer (dcExtra thDeletes)
    duckdb_table_function_set_bind fun (tcBind thCallbacks)
    duckdb_table_function_set_init fun (tcInit thCallbacks)
    duckdb_table_function_set_local_init fun (tcLocalInit thCallbacks)
    duckdb_table_function_set_function fun (tcExecute thCallbacks)
    duckdb_table_function_supports_projection_pushdown fun (toCBool True)

snapshotStats :: TableHarness -> IO HarnessStats
snapshotStats TableHarness{..} = do
    hsExtraFreed <- readIORef thExtraCount
    hsBindFreed <- readIORef thBindCount
    hsInitFreed <- readIORef thInitCount
    hsLocalFreed <- readIORef thLocalCount
    hsClientContextSeen <- readIORef thClientContextSeen
    history <- readIORef thProjectionHistory
    hsCounts <- readIORef thCountHistory
    pure
        HarnessStats
            { hsProjectionHistory = history
            , ..
            }

-- Helpers --------------------------------------------------------------------

extraInfoSentinel :: Int64
extraInfoSentinel = 0xdeadbeef42

localInitSentinel :: Int64
localInitSentinel = 0x105105105

sizeOfInt64 :: Int
sizeOfInt64 = sizeOf (undefined :: Int64)

withOptionalValue :: IO Duckdb_value -> (Duckdb_value -> IO a) -> IO (Maybe a)
withOptionalValue acquire action = do
    value <- acquire
    if value == (coerce (nullPtr :: Ptr Void))
        then pure Nothing
        else do
            result <- action value
            destroy value
            pure (Just result)
  where
    destroy val = alloca \ptr -> poke ptr val >> duckdb_destroy_value ptr

vectorDataPtr :: Duckdb_vector -> IO (Ptr Int64)
vectorDataPtr vec = coerce <$> duckdb_vector_get_data vec

contains :: String -> String -> Bool
contains needle haystack = mapLower needle `isInfixOf` mapLower haystack
  where
    mapLower = map toLowerAscii
    toLowerAscii c
        | isAsciiUpper c = toEnum (fromEnum c + 32)
        | otherwise = c

toCBool :: Bool -> CBool
toCBool False = CBool 0
toCBool True = CBool 1

peekCStringMaybe :: (ConstPtr CChar) -> IO String
peekCStringMaybe ptr
    | ptr == (coerce (nullPtr :: Ptr Void)) = pure ""
    | otherwise = (peekCString . coerce) ptr

-- Wrappers -------------------------------------------------------------------

mkBindFun :: (Duckdb_bind_info -> IO ()) -> IO Duckdb_table_function_bind_t
mkBindFun = fmap Duckdb_table_function_bind_t . toFunPtr . Duckdb_table_function_bind_t_Aux

mkInitFun :: (Duckdb_init_info -> IO ()) -> IO Duckdb_table_function_init_t
mkInitFun = fmap Duckdb_table_function_init_t . toFunPtr . Duckdb_table_function_init_t_Aux

mkFunctionFun :: (Duckdb_function_info -> Duckdb_data_chunk -> IO ()) -> IO Duckdb_table_function_t
mkFunctionFun = fmap Duckdb_table_function_t . toFunPtr . Duckdb_table_function_t_Aux

mkDeleteCallback :: (Ptr Void -> IO ()) -> IO Duckdb_delete_callback_t
mkDeleteCallback = fmap Duckdb_delete_callback_t . toFunPtr . Duckdb_delete_callback_t_Aux
