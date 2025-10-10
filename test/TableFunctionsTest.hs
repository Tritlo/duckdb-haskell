{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module TableFunctionsTest (tests) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Database.DuckDB.FFI
import Debug.Trace (traceM)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase, withLogicalType, withResultCString, withValue)

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
        withLogicalType (c_duckdb_create_logical_type DuckDBTypeBigInt) \bigint ->
          withHarness bigint \harness -> do
            (bindError, execError, initError) <-
              withConnection db \conn ->
                withTableFunction \tableFun -> do
                  configureTableFunction tableFun harness
                  c_duckdb_register_table_function conn tableFun >>= (@?= DuckDBSuccess)

                  withCString "SELECT value, double_value FROM haskell_numbers(5, start => 2)" \sql ->
                    withResultCString conn sql \resPtr -> do
                      c_duckdb_column_count resPtr >>= (@?= 2)
                      c_duckdb_row_count resPtr >>= (@?= 5)
                      forM_ [0 .. 4] \idx -> do
                        c_duckdb_value_int64 resPtr 0 idx >>= (@?= 2 + fromIntegral idx)
                        c_duckdb_value_int64 resPtr 1 idx >>= (@?= 2 * (2 + fromIntegral idx))

                  withCString "SELECT value FROM haskell_numbers(3)" \sql ->
                    withResultCString conn sql \resPtr -> do
                      c_duckdb_column_count resPtr >>= (@?= 1)
                      forM_ (zip [0 ..] [0 .. 2]) \(idx, expected) ->
                        c_duckdb_value_int64 resPtr 0 idx >>= (@?= expected)

                  bindError <-
                    withCString "SELECT * FROM haskell_numbers(0)" \sql ->
                      alloca \resPtr -> do
                        state <- c_duckdb_query conn sql resPtr
                        errPtr <- c_duckdb_result_error resPtr
                        msg <- peekCStringMaybe errPtr
                        c_duckdb_destroy_result resPtr
                        pure (state, msg)

                  execError <-
                    withCString "SELECT * FROM haskell_numbers(5, fail_at => 3)" \sql ->
                      alloca \resPtr -> do
                        state <- c_duckdb_query conn sql resPtr
                        errPtr <- c_duckdb_result_error resPtr
                        msg <- peekCStringMaybe errPtr
                        c_duckdb_destroy_result resPtr
                        pure (state, msg)

                  initError <-
                    withCString "SELECT * FROM haskell_numbers(2, force_init_error => 1)" \sql ->
                      alloca \resPtr -> do
                        state <- c_duckdb_query conn sql resPtr
                        errPtr <- c_duckdb_result_error resPtr
                        msg <- peekCStringMaybe errPtr
                        c_duckdb_destroy_result resPtr
                        pure (state, msg)
                  c_duckdb_table_function_set_extra_info tableFun nullPtr (dcExtra (thDeletes harness))
                  pure (bindError, execError, initError)

            stats <- snapshotStats harness
            traceM ("stats=" <> show stats)
            let showState (st, msg) = (unDuckDBState st, msg)
            traceM ("errors=" <> show (map showState [bindError, execError, initError]))
            hsCounts stats @?= [5, 3, 0, 5, 2]
            hsBindFreed stats @?= 3
            hsInitFreed stats @?= 3
            hsLocalFreed stats @?= 3
            assertBool "client context seen" (hsClientContextSeen stats)
            hsProjectionHistory stats
              @?=
                [ ProjectionSnapshot 2 [0, 1]
                , ProjectionSnapshot 1 [0]
                , ProjectionSnapshot 2 [0, 1]
                ]
            fst bindError @?= DuckDBError
            assertBool "bind error mentions positivity" ("positive" `contains` snd bindError)
            fst execError @?= DuckDBError
            assertBool "execution error mentions table function" ("execution" `contains` snd execError)
            fst initError @?= DuckDBError
            assertBool "init error mentions initialization" ("init" `contains` snd initError)

-- Test harness ----------------------------------------------------------------

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
  { thColumnType :: !DuckDBLogicalType
  , thExtraBuffer :: !(Ptr ())
  , thExtraCount :: !(IORef Int)
  , thBindCount :: !(IORef Int)
  , thInitCount :: !(IORef Int)
  , thLocalCount :: !(IORef Int)
  , thBindPtr :: !(IORef (Maybe (Ptr ())))
  , thInitPtr :: !(IORef (Maybe (Ptr ())))
  , thLocalPtr :: !(IORef (Maybe (Ptr ())))
  , thProjectionCurrent :: !(IORef (Maybe ProjectionSnapshot))
  , thProjectionHistory :: !(IORef [ProjectionSnapshot])
  , thClientContextSeen :: !(IORef Bool)
  , thModeRef :: !(IORef ExecutionMode)
  , thCountHistory :: !(IORef [Int64])
  , thCallbacks :: !TableCallbacks
  , thDeletes :: !DeleteCallbacks
  }

data TableCallbacks = TableCallbacks
  { tcBind :: !DuckDBTableFunctionBindFun
  , tcInit :: !DuckDBTableFunctionInitFun
  , tcLocalInit :: !DuckDBTableFunctionInitFun
  , tcExecute :: !DuckDBTableFunctionFun
  }

data DeleteCallbacks = DeleteCallbacks
  { dcExtra :: !DuckDBDeleteCallback
  , dcBind :: !DuckDBDeleteCallback
  , dcInit :: !DuckDBDeleteCallback
  , dcLocal :: !DuckDBDeleteCallback
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
  deriving (Show)

withHarness :: DuckDBLogicalType -> (TableHarness -> IO a) -> IO a
withHarness columnType action = do
  extraBuffer <- mallocBytes sizeOfInt64
  poke (castPtr extraBuffer :: Ptr Int64) extraInfoSentinel
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

  let takeInt64 ptr idx = peekElemOff (castPtr ptr :: Ptr Int64) idx
      putInt64 ptr idx val = pokeElemOff (castPtr ptr :: Ptr Int64) idx val

  extraDelete <-
    mkDeleteCallback \ptr -> do
      modifyIORef' extraCount (+1)
      when (ptr /= nullPtr) (free ptr)

  bindDelete <-
    mkDeleteCallback \ptr -> do
      modifyIORef' bindCount (+1)
      writeIORef bindPtrRef Nothing
      when (ptr /= nullPtr) (free ptr)

  initDelete <-
    mkDeleteCallback \ptr -> do
      modifyIORef' initCount (+1)
      writeIORef initPtrRef Nothing
      when (ptr /= nullPtr) (free ptr)

  localDelete <-
    mkDeleteCallback \ptr -> do
      modifyIORef' localCount (+1)
      writeIORef localPtrRef Nothing
      when (ptr /= nullPtr) (free ptr)

  let resetForBind = do
        writeIORef projectionCurrent Nothing
        writeIORef modeRef ModeRejected
        writeIORef initPtrRef Nothing
        writeIORef localPtrRef Nothing

      recordProjection snapshot = do
        writeIORef projectionCurrent (Just snapshot)
        modifyIORef' projectionHistory (<> [snapshot])

      bindCallback info = do
        resetForBind
        extraPtr <- c_duckdb_bind_get_extra_info info
        extraPtr @?= extraBuffer
        sentinel <- peek (castPtr extraPtr :: Ptr Int64)
        sentinel @?= extraInfoSentinel

        alloca \ctxPtr -> do
          poke ctxPtr nullPtr
          c_duckdb_table_function_get_client_context info ctxPtr
          ctx <- peek ctxPtr
          assertBool "client context is not null" (ctx /= nullPtr)
          cid <- c_duckdb_client_context_get_connection_id ctx
          assertBool "connection id non-negative" (cid >= 0)
          c_duckdb_destroy_client_context ctxPtr
          writeIORef clientContextSeen True

        paramCount <- c_duckdb_bind_get_parameter_count info
        paramCount @?= 1

        withValue (c_duckdb_bind_get_parameter info 0) \value -> do
          countValue <- c_duckdb_get_int64 value
          modifyIORef' countHistory (<> [countValue])
          if countValue <= 0
            then do
              withCString "count must be positive" \msg -> c_duckdb_bind_set_error info msg
              writeIORef modeRef ModeRejected
              writeIORef bindPtrRef Nothing
            else do
              startValue <-
                withCString "start" \name ->
                  fmap (fromMaybe 0) (withOptionalValue (c_duckdb_bind_get_named_parameter info name) c_duckdb_get_int64)
              failAtValue <-
                withCString "fail_at" \name ->
                  fmap
                    (fromMaybe (-1))
                    (withOptionalValue (c_duckdb_bind_get_named_parameter info name) c_duckdb_get_int64)
              absentCheck <-
                withCString "missing" \name ->
                  withOptionalValue (c_duckdb_bind_get_named_parameter info name) c_duckdb_get_int64
              absentCheck @?= Nothing
              initErrorFlag <-
                withCString "force_init_error" \name ->
                  fmap
                    (fromMaybe 0)
                    (withOptionalValue (c_duckdb_bind_get_named_parameter info name) c_duckdb_get_int64)

              withCString "value" \col -> c_duckdb_bind_add_result_column info col columnType
              withCString "double_value" \col -> c_duckdb_bind_add_result_column info col columnType
              c_duckdb_bind_set_cardinality info (fromIntegral countValue) (toCBool True)

              if initErrorFlag /= 0
                then do
                  writeIORef modeRef ModeInitError
                  writeIORef bindPtrRef Nothing
                else do
                  configPtr <- mallocBytes (3 * sizeOfInt64)
                  putInt64 configPtr 0 startValue
                  putInt64 configPtr 1 countValue
                  putInt64 configPtr 2 failAtValue
                  c_duckdb_bind_set_bind_data info (castPtr configPtr) bindDelete
                  writeIORef bindPtrRef (Just (castPtr configPtr))
                  writeIORef modeRef ModeNormal

      initCallback info = do
        extraPtr <- c_duckdb_init_get_extra_info info
        extraPtr @?= extraBuffer
        sentinel <- peek (castPtr extraPtr :: Ptr Int64)
        sentinel @?= extraInfoSentinel

        mode <- readIORef modeRef
        case mode of
          ModeInitError -> do
            bindRaw <- c_duckdb_init_get_bind_data info
            bindRaw @?= nullPtr
            withCString "init rejected missing bind data" \msg -> c_duckdb_init_set_error info msg
          ModeNormal -> do
            bindRaw <- c_duckdb_init_get_bind_data info
            stored <- readIORef bindPtrRef
            case stored of
              Nothing -> assertFailure "bind data pointer expected during init"
              Just expected -> bindRaw @?= expected

            startValue <- takeInt64 bindRaw 0
            countValue <- takeInt64 bindRaw 1
            failAtValue <- takeInt64 bindRaw 2

            columnCount <- fromIntegral <$> c_duckdb_init_get_column_count info
            indices <-
              mapM
                (fmap (fromIntegral :: DuckDBIdx -> Int) . c_duckdb_init_get_column_index info . fromIntegral)
                [0 .. columnCount - 1]
            recordProjection ProjectionSnapshot {psCount = columnCount, psIndices = indices}

            statePtr <- mallocBytes (3 * sizeOfInt64)
            putInt64 statePtr 0 startValue
            putInt64 statePtr 1 (startValue + countValue)
            putInt64 statePtr 2 failAtValue
            c_duckdb_init_set_init_data info (castPtr statePtr) initDelete
            writeIORef initPtrRef (Just (castPtr statePtr))
            c_duckdb_init_set_max_threads info 1
          _ -> pure ()

      localInitCallback info = do
        extraPtr <- c_duckdb_init_get_extra_info info
        extraPtr @?= extraBuffer
        mode <- readIORef modeRef
        case mode of
          ModeNormal -> do
            bindRaw <- c_duckdb_init_get_bind_data info
            stored <- readIORef bindPtrRef
            case stored of
              Nothing -> assertFailure "bind data should be present during local init"
              Just expected -> bindRaw @?= expected
            localPtr <- mallocBytes sizeOfInt64
            poke (castPtr localPtr :: Ptr Int64) localInitSentinel
            c_duckdb_init_set_init_data info (castPtr localPtr) localDelete
            writeIORef localPtrRef (Just (castPtr localPtr))
          ModeInitError -> do
            withCString "local init should not run after init error" \msg -> c_duckdb_init_set_error info msg
          _ -> pure ()

      executeCallback info chunk = do
        extraPtr <- c_duckdb_function_get_extra_info info
        extraPtr @?= extraBuffer
        mode <- readIORef modeRef
        case mode of
          ModeNormal -> do
            bindRaw <- c_duckdb_function_get_bind_data info
            storedBind <- readIORef bindPtrRef
            case storedBind of
              Nothing -> assertFailure "bind data should be recorded before execution"
              Just expected -> bindRaw @?= expected

            initRaw <- c_duckdb_function_get_init_data info
            storedInit <- readIORef initPtrRef
            case storedInit of
              Nothing -> assertFailure "init data should be present during execution"
              Just expected -> initRaw @?= expected

            localRaw <- c_duckdb_function_get_local_init_data info
            storedLocal <- readIORef localPtrRef
            case storedLocal of
              Nothing -> assertFailure "local init data should be present during execution"
              Just expected -> localRaw @?= expected
            localSentinel <- peek (castPtr localRaw :: Ptr Int64)
            localSentinel @?= localInitSentinel

            snapshot <- readIORef projectionCurrent
            ProjectionSnapshot {..} <-
              case snapshot of
                Nothing -> assertFailure "projection snapshot missing during execution"
                Just snap -> pure snap
            assertBool "projection slots match indices" (length psIndices == psCount)

            nextValue <- takeInt64 initRaw 0
            endValue <- takeInt64 initRaw 1
            failAtValue <- takeInt64 initRaw 2
            if nextValue >= endValue
              then c_duckdb_data_chunk_set_size chunk 0
              else do
                let remaining = endValue - nextValue
                    batch = min remaining 1024
                    chunkEnd = nextValue + batch
                if failAtValue >= nextValue && failAtValue < chunkEnd && failAtValue >= 0
                  then do
                    withCString "execution aborted by table function" \msg -> c_duckdb_function_set_error info msg
                    c_duckdb_data_chunk_set_size chunk 0
                    putInt64 initRaw 0 chunkEnd
                  else do
                    forM_ (zip [0 ..] psIndices) \(slot, schemaIdx) -> do
                      vec <- c_duckdb_data_chunk_get_vector chunk (fromIntegral slot)
                      dataPtr <- vectorDataPtr vec
                      forM_ [0 .. fromIntegral batch - 1] \offset -> do
                        let base = nextValue + fromIntegral offset
                            val =
                              case schemaIdx of
                                0 -> base
                                1 -> 2 * base
                                _ -> 0
                        pokeElemOff dataPtr (fromIntegral offset) val
                    c_duckdb_data_chunk_set_size chunk (fromIntegral batch)
                    putInt64 initRaw 0 chunkEnd
          ModeInitError -> do
            withCString "execution blocked by init failure" \msg -> c_duckdb_function_set_error info msg
            c_duckdb_data_chunk_set_size chunk 0
          ModeRejected -> do
            withCString "execution not expected after bind error" \msg -> c_duckdb_function_set_error info msg
            c_duckdb_data_chunk_set_size chunk 0
          ModeUnset -> do
            withCString "execution invoked without prior bind" \msg -> c_duckdb_function_set_error info msg
            c_duckdb_data_chunk_set_size chunk 0

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
      harness =
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

  freeHaskellFunPtr (tcBind callbacks)
  freeHaskellFunPtr (tcInit callbacks)
  freeHaskellFunPtr (tcLocalInit callbacks)
  freeHaskellFunPtr (tcExecute callbacks)
  freeHaskellFunPtr (dcExtra deletes)
  freeHaskellFunPtr (dcBind deletes)
  freeHaskellFunPtr (dcInit deletes)
  freeHaskellFunPtr (dcLocal deletes)

  pure result

withTableFunction :: (DuckDBTableFunction -> IO a) -> IO a
withTableFunction action =
  bracket c_duckdb_create_table_function destroy action
  where
    destroy fun = alloca \ptr -> poke ptr fun >> c_duckdb_destroy_table_function ptr

configureTableFunction :: DuckDBTableFunction -> TableHarness -> IO ()
configureTableFunction fun TableHarness {..} = do
  withCString "haskell_numbers" \name -> c_duckdb_table_function_set_name fun name
  c_duckdb_table_function_add_parameter fun thColumnType
  withCString "start" \n -> c_duckdb_table_function_add_named_parameter fun n thColumnType
  withCString "fail_at" \n -> c_duckdb_table_function_add_named_parameter fun n thColumnType
  withCString "force_init_error" \n -> c_duckdb_table_function_add_named_parameter fun n thColumnType
  c_duckdb_table_function_set_extra_info fun thExtraBuffer (dcExtra thDeletes)
  c_duckdb_table_function_set_bind fun (tcBind thCallbacks)
  c_duckdb_table_function_set_init fun (tcInit thCallbacks)
  c_duckdb_table_function_set_local_init fun (tcLocalInit thCallbacks)
  c_duckdb_table_function_set_function fun (tcExecute thCallbacks)
  c_duckdb_table_function_supports_projection_pushdown fun (toCBool True)

snapshotStats :: TableHarness -> IO HarnessStats
snapshotStats TableHarness {..} = do
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

-- Helpers ---------------------------------------------------------------------

extraInfoSentinel :: Int64
extraInfoSentinel = 0xdeadbeef42

localInitSentinel :: Int64
localInitSentinel = 0x105105105

sizeOfInt64 :: Int
sizeOfInt64 = sizeOf (undefined :: Int64)

withOptionalValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO (Maybe a)
withOptionalValue acquire action = do
  value <- acquire
  if value == nullPtr
    then pure Nothing
    else do
      result <- action value
      destroy value
      pure (Just result)
  where
    destroy val = alloca \ptr -> poke ptr val >> c_duckdb_destroy_value ptr

vectorDataPtr :: DuckDBVector -> IO (Ptr Int64)
vectorDataPtr vec = castPtr <$> c_duckdb_vector_get_data vec

contains :: String -> String -> Bool
contains needle haystack = mapLower needle `isInfixOf` mapLower haystack
  where
    mapLower = map toLowerAscii
    toLowerAscii c
      | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

toCBool :: Bool -> CBool
toCBool False = CBool 0
toCBool True = CBool 1

peekCStringMaybe :: CString -> IO String
peekCStringMaybe ptr
  | ptr == nullPtr = pure ""
  | otherwise = peekCString ptr

-- Wrapper builders -----------------------------------------------------------

foreign import ccall safe "wrapper"
  mkBindFun :: (DuckDBBindInfo -> IO ()) -> IO DuckDBTableFunctionBindFun

foreign import ccall safe "wrapper"
  mkInitFun :: (DuckDBInitInfo -> IO ()) -> IO DuckDBTableFunctionInitFun

foreign import ccall safe "wrapper"
  mkFunctionFun :: (DuckDBFunctionInfo -> DuckDBDataChunk -> IO ()) -> IO DuckDBTableFunctionFun

foreign import ccall safe "wrapper"
  mkDeleteCallback :: (Ptr () -> IO ()) -> IO DuckDBDeleteCallback
