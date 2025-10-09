{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}

module CastFunctionsTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString, withCStringLen)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peek, peekElemOff, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

data CastHarness = CastHarness
  { chLastMode :: IO DuckDBCastMode
  , chExtraSeen :: IO Bool
  }

tests :: TestTree
tests =
  testGroup
    "Cast Functions"
    [ castFunctionOverridesBuiltin
    , castFunctionReportsErrors
    , castFunctionTryMode
    ]

castFunctionOverridesBuiltin :: TestTree
castFunctionOverridesBuiltin =
  testCase "custom cast overrides builtin conversion" $
    withDatabase \db ->
      withConnection db \conn ->
        withTestCast conn \_ -> do
          withResult conn "SELECT CAST(v AS VARCHAR) FROM (VALUES (1), (5)) AS t(v)" \resPtr -> do
            fetchString resPtr 0 0 >>= (@?= "value: 1")
            fetchString resPtr 0 1 >>= (@?= "value: 5")
            errPtr <- c_duckdb_result_error resPtr
            errPtr @?= nullPtr

castFunctionReportsErrors :: TestTree
castFunctionReportsErrors =
  testCase "cast failure surfaces error message" $
    withDatabase \db ->
      withConnection db \conn ->
        withTestCast conn \_ ->
          withCString "SELECT CAST(v AS VARCHAR) FROM (VALUES (-7)) AS t(v)" \sql ->
            alloca \resPtr -> do
              state <- c_duckdb_query conn sql resPtr
              state @?= DuckDBError
              errPtr <- c_duckdb_result_error resPtr
              assertBool "expected error pointer" (errPtr /= nullPtr)
              errMsg <- peekCString errPtr
              assertBool "error message should mention negative" ("negative" `isInfixOf` errMsg)
              c_duckdb_destroy_result resPtr

castFunctionTryMode :: TestTree
castFunctionTryMode =
  testCase "try_cast produces null rows and reports try mode" $
    withDatabase \db ->
      withConnection db \conn ->
        withTestCast conn \CastHarness{chLastMode, chExtraSeen} -> do
          withResult conn "SELECT TRY_CAST(v AS VARCHAR) FROM (VALUES (2), (-3)) AS t(v)" \resPtr -> do
            fetchString resPtr 0 0 >>= (@?= "value: 2")
            c_duckdb_value_is_null_safe resPtr 0 1 >>= (@?= CBool 1)
            errPtr <- c_duckdb_result_error resPtr
            errPtr @?= nullPtr
          chLastMode >>= (@?= DuckDBCastTry)
          chExtraSeen >>= (@?= True)

-- Harness ------------------------------------------------------------------

withTestCast :: DuckDBConnection -> (CastHarness -> IO a) -> IO a
withTestCast conn action = do
  modeRef <- newIORef DuckDBCastNormal
  extraSeenRef <- newIORef False
  prefixStable <- newStablePtr ("value: " :: String)
  castFunPtr <- mkCastFun (castCallback modeRef extraSeenRef prefixStable)
  result <-
    withCastFunction \castFun ->
      withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \sourceType ->
        withLogicalType (c_duckdb_create_logical_type DuckDBTypeVarchar) \targetType -> do
          c_duckdb_cast_function_set_source_type castFun sourceType
          c_duckdb_cast_function_set_target_type castFun targetType
          c_duckdb_cast_function_set_implicit_cast_cost castFun 0
          c_duckdb_cast_function_set_extra_info castFun (castStablePtrToPtr prefixStable) nullFunPtr
          c_duckdb_cast_function_set_function castFun castFunPtr
          c_duckdb_register_cast_function conn castFun >>= (@?= DuckDBSuccess)
          action CastHarness
            { chLastMode = readIORef modeRef
            , chExtraSeen = readIORef extraSeenRef
            }
  freeHaskellFunPtr castFunPtr
  freeStablePtr prefixStable
  pure result

castCallback :: IORef DuckDBCastMode -> IORef Bool -> StablePtr String -> DuckDBFunctionInfo -> DuckDBIdx -> DuckDBVector -> DuckDBVector -> IO CBool
castCallback modeRef extraSeenRef prefixStable info count inputVec outputVec = do
  actualPtr <- c_duckdb_cast_function_get_extra_info info
  when (actualPtr == castStablePtrToPtr prefixStable) $ writeIORef extraSeenRef True
  prefix <-
    if actualPtr == nullPtr
      then pure "value: "
      else deRefStablePtr (castPtrToStablePtr actualPtr)
  mode <- c_duckdb_cast_function_get_cast_mode info
  writeIORef modeRef mode
  inputData <- c_duckdb_vector_get_data inputVec
  c_duckdb_vector_ensure_validity_writable outputVec
  let inPtr = castPtr inputData :: Ptr Int32
      rowCount = fromIntegral count :: Int
  success <- processRows prefix mode inPtr outputVec 0 rowCount
  pure (if success then CBool 1 else CBool 0)
  where
    processRows :: String -> DuckDBCastMode -> Ptr Int32 -> DuckDBVector -> Int -> Int -> IO Bool
    processRows prefix mode inPtr outVec idx total
      | idx >= total = pure True
      | otherwise = do
          val <- peekElemOff inPtr idx
          if val < 0
            then
              withCString "negative values not allowed" \errMsg ->
                if mode == DuckDBCastTry
                  then do
                    c_duckdb_cast_function_set_row_error info errMsg (fromIntegral idx) outVec
                    processRows prefix mode inPtr outVec (idx + 1) total
                  else do
                    c_duckdb_cast_function_set_error info errMsg
                    pure False
            else do
              let rendered = prefix ++ show val
              withCStringLen rendered \(cStr, len) ->
                c_duckdb_vector_assign_string_element_len outVec (fromIntegral idx) cStr (fromIntegral len)
              processRows prefix mode inPtr outVec (idx + 1) total

-- Helpers ------------------------------------------------------------------

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase action =
  withCString ":memory:" \path ->
    alloca \dbPtr -> do
      state <- c_duckdb_open path dbPtr
      state @?= DuckDBSuccess
      db <- peek dbPtr
      result <- action db
      c_duckdb_close dbPtr
      pure result

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db action =
  alloca \connPtr -> do
    state <- c_duckdb_connect db connPtr
    state @?= DuckDBSuccess
    conn <- peek connPtr
    result <- action conn
    c_duckdb_disconnect connPtr
    pure result

withCastFunction :: (DuckDBCastFunction -> IO a) -> IO a
withCastFunction action = bracket c_duckdb_create_cast_function destroy action
  where
    destroy cf = alloca \ptr -> poke ptr cf >> c_duckdb_destroy_cast_function ptr

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType create action = bracket create destroy action
  where
    destroy lt = alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

withResult :: DuckDBConnection -> String -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn sql action =
  withCString sql \sqlPtr ->
    alloca \resPtr -> do
      state <- c_duckdb_query_safe conn sqlPtr resPtr
      state @?= DuckDBSuccess
      result <- action resPtr
      c_duckdb_destroy_result resPtr
      pure result

withErrorResult :: DuckDBConnection -> String -> (Ptr DuckDBResult -> IO a) -> IO a
withErrorResult conn sql action =
  withCString sql \sqlPtr ->
    alloca \resPtr -> do
      state <- c_duckdb_query_safe conn sqlPtr resPtr
      state @?= DuckDBError
      result <- action resPtr
      c_duckdb_destroy_result resPtr
      pure result

fetchString :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO String
fetchString resPtr col row = do
  cStr <- c_duckdb_value_varchar_safe resPtr col row
  value <- peekCString cStr
  c_duckdb_free_safe (castPtr cStr)
  pure value

-- Wrapper constructors ------------------------------------------------------

foreign import ccall "wrapper"
  mkCastFun :: (DuckDBFunctionInfo -> DuckDBIdx -> DuckDBVector -> DuckDBVector -> IO CBool) -> IO DuckDBCastFunctionFun

foreign import ccall "wrapper"
  mkDeleteCallback :: (Ptr () -> IO ()) -> IO DuckDBDeleteCallback

-- Safe wrappers -------------------------------------------------------------

foreign import ccall safe "duckdb_query"
  c_duckdb_query_safe :: DuckDBConnection -> CString -> Ptr DuckDBResult -> IO DuckDBState

foreign import ccall safe "duckdb_value_varchar"
  c_duckdb_value_varchar_safe :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO CString

foreign import ccall safe "duckdb_value_is_null"
  c_duckdb_value_is_null_safe :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO CBool

foreign import ccall safe "duckdb_free"
  c_duckdb_free_safe :: Ptr () -> IO ()
