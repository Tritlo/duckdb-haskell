{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module TableFunctionsTest (tests) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, pokeElemOff, poke, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Table Functions"
    [ tableFunctionRoundtrip
    , tableFunctionBindErrors
    , tableFunctionExecutionErrors
    ]

tableFunctionRoundtrip :: TestTree
tableFunctionRoundtrip =
  testCase "register custom table function and execute" $
    runInBoundThread do
      withDatabase \db ->
        withConnection db \conn ->
          withLogicalType (c_duckdb_create_logical_type DuckDBTypeBigInt) \bigint ->
            withCallbacks bigint \cbs ->
              withTableFunction \tableFun -> do
                setupTableFunction tableFun cbs bigint
                c_duckdb_register_table_function conn tableFun >>= (@?= DuckDBSuccess)

                withCString "SELECT value FROM haskell_numbers(4)" \sql ->
                  withResult conn sql \resPtr -> do
                    c_duckdb_row_count resPtr >>= (@?= 4)
                    forM_ [0 .. 3] \idx ->
                      c_duckdb_value_int64 resPtr 0 idx >>= (@?= fromIntegral idx)

                withCString "SELECT value FROM haskell_numbers(3, start => 5)" \sql ->
                  withResult conn sql \resPtr -> do
                    expected <- pure [5, 6, 7]
                    forM_ (zip [0 ..] expected) \(idx, val) ->
                      c_duckdb_value_int64 resPtr 0 idx >>= (@?= val)

tableFunctionBindErrors :: TestTree
tableFunctionBindErrors =
  testCase "bind callback can signal errors" $
    runInBoundThread do
      withDatabase \db ->
        withConnection db \conn ->
          withLogicalType (c_duckdb_create_logical_type DuckDBTypeBigInt) \bigint ->
            withCallbacks bigint \cbs ->
              withTableFunction \tableFun -> do
                setupTableFunction tableFun cbs bigint
                c_duckdb_register_table_function conn tableFun >>= (@?= DuckDBSuccess)

                alloca \resPtr ->
                  withCString "SELECT * FROM haskell_numbers(0)" \sql -> do
                    state <- c_duckdb_query conn sql resPtr
                    state @?= DuckDBError
                    errPtr <- c_duckdb_result_error resPtr
                    errMsg <- peekCString errPtr
                    assertBool "expected positive count message" ("positive" `contains` errMsg)
                    c_duckdb_destroy_result resPtr

tableFunctionExecutionErrors :: TestTree
tableFunctionExecutionErrors =
  testCase "execution callback can report errors" $
    runInBoundThread do
      withDatabase \db ->
        withConnection db \conn ->
          withLogicalType (c_duckdb_create_logical_type DuckDBTypeBigInt) \bigint ->
            withCallbacks bigint \cbs ->
              withTableFunction \tableFun -> do
                setupTableFunction tableFun cbs bigint
                c_duckdb_register_table_function conn tableFun >>= (@?= DuckDBSuccess)

                alloca \resPtr ->
                  withCString "SELECT * FROM haskell_numbers(5, fail_at => 3)" \sql -> do
                    state <- c_duckdb_query conn sql resPtr
                    state @?= DuckDBError
                    errPtr <- c_duckdb_result_error resPtr
                    errMsg <- peekCString errPtr
                    assertBool "expected execution error message" ("execution aborted" `contains` errMsg)
                    c_duckdb_destroy_result resPtr

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

data TableCallbacks = TableCallbacks
  { tcBind :: DuckDBTableFunctionBindFun
  , tcInit :: DuckDBTableFunctionInitFun
  , tcExecute :: DuckDBTableFunctionFun
  , tcFreeBuffer :: DuckDBDeleteCallback
  }

withCallbacks :: DuckDBLogicalType -> (TableCallbacks -> IO a) -> IO a
withCallbacks columnType action =
  bracket acquire release action
  where
    acquire = do
      freeCallback <- mkDeleteCallback free
      bindFun <- mkBindFun (numbersBind freeCallback columnType)
      initFun <- mkInitFun (numbersInit freeCallback)
      execFun <- mkFunctionFun numbersExecute
      pure TableCallbacks
        { tcBind = bindFun
        , tcInit = initFun
        , tcExecute = execFun
        , tcFreeBuffer = freeCallback
        }
    release TableCallbacks{tcBind = bindFun, tcInit = initFun, tcExecute = execFun, tcFreeBuffer = freeCallback} = do
      freeHaskellFunPtr bindFun
      freeHaskellFunPtr initFun
      freeHaskellFunPtr execFun
      freeHaskellFunPtr freeCallback

setupTableFunction :: DuckDBTableFunction -> TableCallbacks -> DuckDBLogicalType -> IO ()
setupTableFunction fun TableCallbacks{tcBind = bindFun, tcInit = initFun, tcExecute = execFun, tcFreeBuffer = freeCallback} columnType = do
  withCString "haskell_numbers" \name -> c_duckdb_table_function_set_name fun name
  c_duckdb_table_function_add_parameter fun columnType
  withCString "start" \n -> c_duckdb_table_function_add_named_parameter fun n columnType
  withCString "fail_at" \n -> c_duckdb_table_function_add_named_parameter fun n columnType
  c_duckdb_table_function_set_bind fun bindFun
  c_duckdb_table_function_set_init fun initFun
  c_duckdb_table_function_set_function fun execFun
  let _ = freeCallback
  pure ()

numbersBind :: DuckDBDeleteCallback -> DuckDBLogicalType -> DuckDBBindInfo -> IO ()
numbersBind freeCallback columnType info = do
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
          pokeElemOff bindPtr 0 (maybe 0 id startValue)
          pokeElemOff bindPtr 1 countValue
          pokeElemOff bindPtr 2 (maybe (-1) id failAtValue)
          c_duckdb_bind_set_bind_data info (castPtr bindPtr) freeCallback
          withCString "value" \col -> c_duckdb_bind_add_result_column info col columnType
          c_duckdb_bind_set_cardinality info (fromIntegral countValue) (toCBool True)

numbersInit :: DuckDBDeleteCallback -> DuckDBInitInfo -> IO ()
numbersInit freeCallback info = do
  bindRaw <- c_duckdb_init_get_bind_data info
  if bindRaw == nullPtr
    then withCString "missing bind data" \msg -> c_duckdb_init_set_error info msg
    else do
      let bindPtr = castPtr bindRaw :: Ptr Int64
      startValue <- peekElemOff bindPtr 0
      countValue <- peekElemOff bindPtr 1
      failAtValue <- peekElemOff bindPtr 2
      statePtr <- mallocBytes runStateSize
      pokeElemOff statePtr 0 startValue
      pokeElemOff statePtr 1 (startValue + countValue)
      pokeElemOff statePtr 2 failAtValue
      c_duckdb_init_set_init_data info (castPtr statePtr) freeCallback
      c_duckdb_init_set_max_threads info 1

numbersExecute :: DuckDBFunctionInfo -> DuckDBDataChunk -> IO ()
numbersExecute info chunk = do
  stateRaw <- c_duckdb_function_get_init_data info
  if stateRaw == nullPtr
    then do
      withCString "missing init data" \msg -> c_duckdb_function_set_error info msg
      c_duckdb_data_chunk_set_size chunk 0
    else do
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
              vec <- c_duckdb_data_chunk_get_vector chunk 0
              dataPtr <- vectorDataPtr vec
              forM_ [0 .. rowsThisChunk - 1] \offset -> do
                let value = nextValue + offset
                pokeElemOff dataPtr (fromIntegral offset) value
              c_duckdb_data_chunk_set_size chunk (fromIntegral rowsThisChunk)
              pokeElemOff statePtr 0 chunkEnd

-- Helpers ------------------------------------------------------------------

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase action =
  withCString ":memory:" \path ->
    alloca \dbPtr -> do
      c_duckdb_open path dbPtr >>= (@?= DuckDBSuccess)
      db <- peek dbPtr
      result <- action db
      c_duckdb_close dbPtr
      pure result

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db action = bracket acquire release action
  where
    acquire = alloca \ptr -> c_duckdb_connect db ptr >>= (@?= DuckDBSuccess) >> peek ptr
    release conn = alloca \ptr -> poke ptr conn >> c_duckdb_disconnect ptr

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType acquire action = bracket acquire destroy action
  where
    destroy lt = alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

withTableFunction :: (DuckDBTableFunction -> IO a) -> IO a
withTableFunction action = bracket c_duckdb_create_table_function destroy action
  where
    destroy fun = alloca \ptr -> poke ptr fun >> c_duckdb_destroy_table_function ptr

withResult :: DuckDBConnection -> CString -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn sql action =
  alloca \resPtr -> do
    c_duckdb_query conn sql resPtr >>= (@?= DuckDBSuccess)
    result <- action resPtr
    c_duckdb_destroy_result resPtr
    pure result

withValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO a
withValue acquire action = bracket acquire destroy action
  where
    destroy value = alloca \ptr -> poke ptr value >> c_duckdb_destroy_value ptr

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
