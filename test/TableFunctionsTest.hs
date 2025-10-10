{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module TableFunctionsTest (tests) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, pokeElemOff, poke, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase, withLogicalType, withResultCString, withValue)

tests :: TestTree
tests =
  testGroup
    "Table Functions"
    [
    --    tableFunctionRoundtrip
    -- ,  tableFunctionBindErrors
    -- ,  tableFunctionExecutionErrors
    ]

tableFunctionRoundtrip :: TestTree
tableFunctionRoundtrip =
  testCase "register custom table function and execute" $
    runInBoundThread do
      env <-
        withDatabase \db ->
          withConnection db \conn ->
            withLogicalType (c_duckdb_create_logical_type DuckDBTypeBigInt) \bigint ->
              withCallbacks bigint \harness -> do
                let env = thEnv harness
                withTableFunction \tableFun -> do
                  setupTableFunction tableFun harness
                  c_duckdb_register_table_function conn tableFun >>= (@?= DuckDBSuccess)

                  withCString "SELECT value, double_value FROM haskell_numbers(4)" \sql ->
                    withResultCString conn sql \resPtr -> do
                      c_duckdb_row_count resPtr >>= (@?= 4)
                      forM_ [0 .. 3] \idx -> do
                        c_duckdb_value_int64 resPtr 0 idx >>= (@?= fromIntegral idx)
                        c_duckdb_value_int64 resPtr 1 idx >>= (@?= 2 * fromIntegral idx)

                  withCString "SELECT value FROM haskell_numbers(4)" \sql ->
                    withResultCString conn sql \resPtr -> do
                      c_duckdb_row_count resPtr >>= (@?= 4)
                      forM_ [0 .. 3] \idx ->
                        c_duckdb_value_int64 resPtr 0 idx >>= (@?= fromIntegral idx)

                  withCString "SELECT value FROM haskell_numbers(3, start => 5)" \sql ->
                    withResultCString conn sql \resPtr -> do
                      expected <- pure [5, 6, 7]
                      forM_ (zip [0 ..] expected) \(idx, val) ->
                        c_duckdb_value_int64 resPtr 0 idx >>= (@?= val)

                history <- readIORef (teProjectionHistory env)
                history
                  @?=
                    [ ProjectionInfo 2 [0, 1]
                    , ProjectionInfo 1 [0]
                    , ProjectionInfo 1 [0]
                    ]

                contextSeen <- readIORef (teClientContextSeen env)
                assertBool "expected client context retrieval during bind" contextSeen

                pure env

      stats <- collectStats env
      assertBool "extra info destructor should run at least once" (tsExtraDestroyed stats >= 1)
      assertBool "bind data destructor should run per query" (tsBindFreed stats >= 3)
      assertBool "init data destructor should run per query" (tsInitFreed stats >= 3)
      assertBool "local init data destructor should run per query" (tsLocalFreed stats >= 3)
      assertBool "client context retrieval should flip the flag" (tsClientContextSeen stats)

tableFunctionBindErrors :: TestTree
tableFunctionBindErrors =
  testCase "bind callback can signal errors" $
    runInBoundThread do
      env <-
        withDatabase \db ->
          withConnection db \conn ->
            withLogicalType (c_duckdb_create_logical_type DuckDBTypeBigInt) \bigint ->
              withCallbacks bigint \harness -> do
                let env = thEnv harness
                withTableFunction \tableFun -> do
                  setupTableFunction tableFun harness
                  c_duckdb_register_table_function conn tableFun >>= (@?= DuckDBSuccess)

                  alloca \resPtr ->
                    withCString "SELECT * FROM haskell_numbers(0)" \sql -> do
                      state <- c_duckdb_query conn sql resPtr
                      state @?= DuckDBError
                      errPtr <- c_duckdb_result_error resPtr
                      errMsg <- peekCString errPtr
                      assertBool "expected positive count message" ("positive" `contains` errMsg)
                      c_duckdb_destroy_result resPtr

                pure env

      stats <- collectStats env
      assertBool "client context should be observed even on bind error" (tsClientContextSeen stats)
      tsBindFreed stats @?= 0
      tsInitFreed stats @?= 0
      tsLocalFreed stats @?= 0
      assertBool "extra info destructor should run at least once" (tsExtraDestroyed stats >= 1)

tableFunctionExecutionErrors :: TestTree
tableFunctionExecutionErrors =
  testCase "execution callback can report errors" $
    runInBoundThread do
      env <-
        withDatabase \db ->
          withConnection db \conn ->
            withLogicalType (c_duckdb_create_logical_type DuckDBTypeBigInt) \bigint ->
              withCallbacks bigint \harness -> do
                let env = thEnv harness
                withTableFunction \tableFun -> do
                  setupTableFunction tableFun harness
                  c_duckdb_register_table_function conn tableFun >>= (@?= DuckDBSuccess)

                  alloca \resPtr ->
                    withCString "SELECT * FROM haskell_numbers(5, fail_at => 3)" \sql -> do
                      state <- c_duckdb_query conn sql resPtr
                      state @?= DuckDBError
                      errPtr <- c_duckdb_result_error resPtr
                      errMsg <- peekCString errPtr
                      assertBool "expected execution error message" ("execution aborted" `contains` errMsg)
                      c_duckdb_destroy_result resPtr

                pure env

      stats <- collectStats env
      assertBool "bind data should be freed even when execution errors" (tsBindFreed stats >= 1)
      assertBool "init data should be freed even when execution errors" (tsInitFreed stats >= 1)
      assertBool "local init data should be freed even when execution errors" (tsLocalFreed stats >= 1)
      assertBool "extra info destructor should run at least once" (tsExtraDestroyed stats >= 1)

-- Table function plumbing ---------------------------------------------------

data BindConfig = BindConfig
  { bcStart :: !Int64
  , bcCount :: !Int64
  , bcFailAt :: !Int64
  }

bindConfigSize :: Int
bindConfigSize = 3 * sizeOfInt64

data RunState = RunState
  { rsNext :: !Int64
  , rsEnd :: !Int64
  , rsFailAt :: !Int64
  }

runStateSize :: Int
runStateSize = 3 * sizeOfInt64

sizeOfInt64 :: Int
sizeOfInt64 = sizeOf (undefined :: Int64)

extraInfoSentinel :: Int64
extraInfoSentinel = 424242

localInitSentinel :: Int64
localInitSentinel = 9001

data ProjectionInfo = ProjectionInfo
  { piCount :: !Int
  , piIndices :: ![Int]
  }
  deriving (Eq, Show)

data TableEnv = TableEnv
  { teColumnType :: !DuckDBLogicalType
  , teExtraInfoPtr :: !(Ptr ())
  , teExtraInfoDestroyCount :: !(IORef Int)
  , teBindDataPtr :: !(IORef (Maybe (Ptr ())))
  , teBindDataFreeCount :: !(IORef Int)
  , teInitDataPtr :: !(IORef (Maybe (Ptr ())))
  , teInitDataFreeCount :: !(IORef Int)
  , teLocalDataPtr :: !(IORef (Maybe (Ptr ())))
  , teLocalDataFreeCount :: !(IORef Int)
  , teProjectionCurrent :: !(IORef (Maybe ProjectionInfo))
  , teProjectionHistory :: !(IORef [ProjectionInfo])
  , teClientContextSeen :: !(IORef Bool)
  , teExtraDelete :: !DuckDBDeleteCallback
  , teBindDelete :: !DuckDBDeleteCallback
  , teInitDelete :: !DuckDBDeleteCallback
  , teLocalDelete :: !DuckDBDeleteCallback
  }

data TableCallbacks = TableCallbacks
  { tcBind :: DuckDBTableFunctionBindFun
  , tcInit :: DuckDBTableFunctionInitFun
  , tcLocalInit :: DuckDBTableFunctionInitFun
  , tcExecute :: DuckDBTableFunctionFun
  }

data TableHarness = TableHarness
  { thEnv :: !TableEnv
  , thCallbacks :: !TableCallbacks
  }

data TableStats = TableStats
  { tsExtraDestroyed :: !Int
  , tsBindFreed :: !Int
  , tsInitFreed :: !Int
  , tsLocalFreed :: !Int
  , tsClientContextSeen :: !Bool
  }
  deriving (Eq, Show)

collectStats :: TableEnv -> IO TableStats
collectStats env = do
  extraDestroyed <- readIORef (teExtraInfoDestroyCount env)
  bindFreed <- readIORef (teBindDataFreeCount env)
  initFreed <- readIORef (teInitDataFreeCount env)
  localFreed <- readIORef (teLocalDataFreeCount env)
  clientSeen <- readIORef (teClientContextSeen env)
  pure
    TableStats
      { tsExtraDestroyed = extraDestroyed
      , tsBindFreed = bindFreed
      , tsInitFreed = initFreed
      , tsLocalFreed = localFreed
      , tsClientContextSeen = clientSeen
      }

withCallbacks :: DuckDBLogicalType -> (TableHarness -> IO a) -> IO a
withCallbacks columnType action =
  bracket acquire release action
  where
    acquire = do
      extraStorage <- mallocBytes sizeOfInt64
      poke (castPtr extraStorage :: Ptr Int64) extraInfoSentinel

      extraDestroyCount <- newIORef 0
      bindDataFreeCount <- newIORef 0
      initDataFreeCount <- newIORef 0
      localDataFreeCount <- newIORef 0

      bindDataPtr <- newIORef Nothing
      initDataPtr <- newIORef Nothing
      localDataPtr <- newIORef Nothing
      projectionCurrent <- newIORef Nothing
      projectionHistory <- newIORef []
      clientContextSeen <- newIORef False

      let trackingFree counter ptr = do
            modifyIORef' counter (+ 1)
            when (ptr /= nullPtr) (free ptr)

      extraDelete <- mkDeleteCallback (trackingFree extraDestroyCount)
      bindDelete <- mkDeleteCallback (trackingFree bindDataFreeCount)
      initDelete <- mkDeleteCallback (trackingFree initDataFreeCount)
      localDelete <- mkDeleteCallback (trackingFree localDataFreeCount)

      let env =
            TableEnv
              { teColumnType = columnType
              , teExtraInfoPtr = castPtr extraStorage
              , teExtraInfoDestroyCount = extraDestroyCount
              , teBindDataPtr = bindDataPtr
              , teBindDataFreeCount = bindDataFreeCount
              , teInitDataPtr = initDataPtr
              , teInitDataFreeCount = initDataFreeCount
              , teLocalDataPtr = localDataPtr
              , teLocalDataFreeCount = localDataFreeCount
              , teProjectionCurrent = projectionCurrent
              , teProjectionHistory = projectionHistory
              , teClientContextSeen = clientContextSeen
              , teExtraDelete = extraDelete
              , teBindDelete = bindDelete
              , teInitDelete = initDelete
              , teLocalDelete = localDelete
              }

      bindFun <- mkBindFun (numbersBind env)
      initFun <- mkInitFun (numbersInit env)
      localInitFun <- mkInitFun (numbersLocalInit env)
      execFun <- mkFunctionFun (numbersExecute env)

      pure
        TableHarness
          { thEnv = env
          , thCallbacks =
              TableCallbacks
                { tcBind = bindFun
                , tcInit = initFun
                , tcLocalInit = localInitFun
                , tcExecute = execFun
                }
        }
    release TableHarness{thEnv = env, thCallbacks = TableCallbacks{tcBind = bindFun, tcInit = initFun, tcLocalInit = localInitFun, tcExecute = execFun}} = do
      freeHaskellFunPtr bindFun
      freeHaskellFunPtr initFun
      freeHaskellFunPtr localInitFun
      freeHaskellFunPtr execFun
      freeHaskellFunPtr (teBindDelete env)
      freeHaskellFunPtr (teInitDelete env)
      freeHaskellFunPtr (teLocalDelete env)
      freeHaskellFunPtr (teExtraDelete env)

setupTableFunction :: DuckDBTableFunction -> TableHarness -> IO ()
setupTableFunction fun TableHarness{thEnv = env, thCallbacks = TableCallbacks{tcBind = bindFun, tcInit = initFun, tcLocalInit = localInitFun, tcExecute = execFun}} = do
  withCString "haskell_numbers" \name -> c_duckdb_table_function_set_name fun name
  c_duckdb_table_function_add_parameter fun (teColumnType env)
  withCString "start" \n -> c_duckdb_table_function_add_named_parameter fun n (teColumnType env)
  withCString "fail_at" \n -> c_duckdb_table_function_add_named_parameter fun n (teColumnType env)
  c_duckdb_table_function_set_extra_info fun (teExtraInfoPtr env) (teExtraDelete env)
  c_duckdb_table_function_set_bind fun bindFun
  c_duckdb_table_function_set_init fun initFun
  c_duckdb_table_function_set_local_init fun localInitFun
  c_duckdb_table_function_set_function fun execFun
  c_duckdb_table_function_supports_projection_pushdown fun (toCBool True)

numbersBind :: TableEnv -> DuckDBBindInfo -> IO ()
numbersBind env info = do
  extraPtr <- c_duckdb_bind_get_extra_info info
  extraPtr @?= teExtraInfoPtr env
  sentinel <- peek (castPtr extraPtr :: Ptr Int64)
  sentinel @?= extraInfoSentinel

  alloca \ctxPtr -> do
    poke ctxPtr nullPtr
    c_duckdb_table_function_get_client_context info ctxPtr
    ctx <- peek ctxPtr
    assertBool "client context should not be null" (ctx /= nullPtr)
    cid <- c_duckdb_client_context_get_connection_id ctx
    assertBool "connection id should be non-negative" (cid >= 0)
    c_duckdb_destroy_client_context ctxPtr
    writeIORef (teClientContextSeen env) True

  paramCount <- c_duckdb_bind_get_parameter_count info
  if paramCount /= 1
    then withCString "exactly one parameter required" \msg -> c_duckdb_bind_set_error info msg
    else do
      countValue <- withValue (c_duckdb_bind_get_parameter info 0) c_duckdb_get_int64
      if countValue <= 0
        then withCString "count must be positive" \msg -> c_duckdb_bind_set_error info msg
        else do
          startValue <-
            withCString "start" \name ->
              withOptionalValue (c_duckdb_bind_get_named_parameter info name) c_duckdb_get_int64
          failAtValue <-
            withCString "fail_at" \name ->
              withOptionalValue (c_duckdb_bind_get_named_parameter info name) c_duckdb_get_int64
          bindPtr <- mallocBytes bindConfigSize
          pokeElemOff bindPtr 0 (fromMaybe 0 startValue)
          pokeElemOff bindPtr 1 countValue
          pokeElemOff bindPtr 2 (fromMaybe (-1) failAtValue)
          c_duckdb_bind_set_bind_data info (castPtr bindPtr) (teBindDelete env)
          writeIORef (teBindDataPtr env) (Just (castPtr bindPtr))
          withCString "value" \col -> c_duckdb_bind_add_result_column info col (teColumnType env)
          withCString "double_value" \col -> c_duckdb_bind_add_result_column info col (teColumnType env)
          c_duckdb_bind_set_cardinality info (fromIntegral countValue) (toCBool True)

numbersInit :: TableEnv -> DuckDBInitInfo -> IO ()
numbersInit env info = do
  extraPtr <- c_duckdb_init_get_extra_info info
  extraPtr @?= teExtraInfoPtr env
  sentinel <- peek (castPtr extraPtr :: Ptr Int64)
  sentinel @?= extraInfoSentinel

  bindRaw <- c_duckdb_init_get_bind_data info
  if bindRaw == nullPtr
    then withCString "missing bind data" \msg -> c_duckdb_init_set_error info msg
    else do
      recordedBind <- readIORef (teBindDataPtr env)
      case recordedBind of
        Just expected -> bindRaw @?= expected
        Nothing -> assertFailure "bind data pointer should have been recorded"
      let bindPtr = castPtr bindRaw :: Ptr Int64
      startValue <- peekElemOff bindPtr 0
      countValue <- peekElemOff bindPtr 1
      failAtValue <- peekElemOff bindPtr 2

      columnCount <- fromIntegral <$> c_duckdb_init_get_column_count info
      indices <-
        mapM
          (fmap fromIntegral . c_duckdb_init_get_column_index info . fromIntegral)
          [0 .. columnCount - 1]
      let projection = ProjectionInfo {piCount = columnCount, piIndices = indices}
      writeIORef (teProjectionCurrent env) (Just projection)
      modifyIORef' (teProjectionHistory env) (<> [projection])

      statePtr <- mallocBytes runStateSize
      pokeElemOff statePtr 0 startValue
      pokeElemOff statePtr 1 (startValue + countValue)
      pokeElemOff statePtr 2 failAtValue
      c_duckdb_init_set_init_data info (castPtr statePtr) (teInitDelete env)
      writeIORef (teInitDataPtr env) (Just (castPtr statePtr))
      c_duckdb_init_set_max_threads info 1

numbersLocalInit :: TableEnv -> DuckDBInitInfo -> IO ()
numbersLocalInit env info = do
  extraPtr <- c_duckdb_init_get_extra_info info
  extraPtr @?= teExtraInfoPtr env

  bindRaw <- c_duckdb_init_get_bind_data info
  recordedBind <- readIORef (teBindDataPtr env)
  case recordedBind of
    Just expected -> bindRaw @?= expected
    Nothing -> assertFailure "bind data pointer should have been recorded before local init"

  localPtr <- mallocBytes sizeOfInt64
  poke (castPtr localPtr :: Ptr Int64) localInitSentinel
  c_duckdb_init_set_init_data info (castPtr localPtr) (teLocalDelete env)
  writeIORef (teLocalDataPtr env) (Just (castPtr localPtr))

numbersExecute :: TableEnv -> DuckDBFunctionInfo -> DuckDBDataChunk -> IO ()
numbersExecute env info chunk = do
  extraPtr <- c_duckdb_function_get_extra_info info
  extraPtr @?= teExtraInfoPtr env

  bindPtr <- c_duckdb_function_get_bind_data info
  recordedBind <- readIORef (teBindDataPtr env)
  case recordedBind of
    Just expected -> bindPtr @?= expected
    Nothing -> assertFailure "bind data pointer should have been recorded before execution"

  stateRaw <- c_duckdb_function_get_init_data info
  if stateRaw == nullPtr
    then do
      withCString "missing init data" \msg -> c_duckdb_function_set_error info msg
      c_duckdb_data_chunk_set_size chunk 0
    else do
      recordedInit <- readIORef (teInitDataPtr env)
      case recordedInit of
        Just expected -> stateRaw @?= expected
        Nothing -> assertFailure "init data pointer should have been recorded"
      localRaw <- c_duckdb_function_get_local_init_data info
      recordedLocal <- readIORef (teLocalDataPtr env)
      case recordedLocal of
        Just expected -> localRaw @?= expected
        Nothing -> assertFailure "local init data pointer should have been recorded"
      localSentinel <- peek (castPtr localRaw :: Ptr Int64)
      localSentinel @?= localInitSentinel

      let statePtr = castPtr stateRaw :: Ptr Int64
      nextValue <- peekElemOff statePtr 0
      endValue <- peekElemOff statePtr 1
      failAtValue <- peekElemOff statePtr 2
      if nextValue >= endValue
        then c_duckdb_data_chunk_set_size chunk 0
        else do
          let remaining = endValue - nextValue
              rowsThisChunk = min remaining 1024
              chunkEnd = nextValue + rowsThisChunk
          if failAtValue >= nextValue && failAtValue < chunkEnd && failAtValue >= 0
            then do
              withCString "execution aborted by table function" \msg -> c_duckdb_function_set_error info msg
              c_duckdb_data_chunk_set_size chunk 0
              pokeElemOff statePtr 0 chunkEnd
            else do
              projectionInfo <- readIORef (teProjectionCurrent env)
              ProjectionInfo{piCount = colCount, piIndices = indices} <-
                case projectionInfo of
                  Nothing -> do
                    assertFailure "expected projection info to be recorded during init"
                    pure (ProjectionInfo 0 [])
                  Just infoRecord -> pure infoRecord
              assertBool "projection count matches indices" (length indices == colCount)

              let rowCount = fromIntegral rowsThisChunk :: Int
                  baseValue offset = nextValue + fromIntegral offset
                  fillColumn slot schemaIdx = do
                    vec <- c_duckdb_data_chunk_get_vector chunk (fromIntegral slot)
                    dataPtr <- vectorDataPtr vec
                    forM_ [0 .. rowCount - 1] \offset -> do
                      let val = case schemaIdx of
                            0 -> baseValue offset
                            1 -> 2 * baseValue offset
                            _ -> 0
                      pokeElemOff dataPtr offset val

              forM_ (zip [0 ..] indices) \(slot, schemaIdx) ->
                fillColumn slot schemaIdx

              c_duckdb_data_chunk_set_size chunk (fromIntegral rowsThisChunk)
              pokeElemOff statePtr 0 chunkEnd

-- Helpers ------------------------------------------------------------------

withTableFunction :: (DuckDBTableFunction -> IO a) -> IO a
withTableFunction action = bracket c_duckdb_create_table_function destroy action
  where
    destroy fun = alloca \ptr -> poke ptr fun >> c_duckdb_destroy_table_function ptr

withOptionalValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO (Maybe a)
withOptionalValue acquire action = do
  value <- acquire
  if value == nullPtr
    then pure Nothing
    else do
      result <- action value
      alloca \ptr -> poke ptr value >> c_duckdb_destroy_value ptr
      pure (Just result)

vectorDataPtr :: DuckDBVector -> IO (Ptr Int64)
vectorDataPtr vec = castPtr <$> c_duckdb_vector_get_data vec

contains :: String -> String -> Bool
contains needle haystack = needle `isInfixOf` haystack

toCBool :: Bool -> CBool
toCBool False = CBool 0
toCBool True = CBool 1

-- Wrapper builders ----------------------------------------------------------

foreign import ccall safe "wrapper"
  mkBindFun :: (DuckDBBindInfo -> IO ()) -> IO DuckDBTableFunctionBindFun

foreign import ccall safe "wrapper"
  mkInitFun :: (DuckDBInitInfo -> IO ()) -> IO DuckDBTableFunctionInitFun

foreign import ccall safe "wrapper"
  mkFunctionFun :: (DuckDBFunctionInfo -> DuckDBDataChunk -> IO ()) -> IO DuckDBTableFunctionFun

foreign import ccall safe "wrapper"
  mkDeleteCallback :: (Ptr () -> IO ()) -> IO DuckDBDeleteCallback
