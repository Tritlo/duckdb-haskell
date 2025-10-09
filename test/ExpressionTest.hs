{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module ExpressionTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (unless, when)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString, withCStringLen)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

data ExpressionHarness = ExpressionHarness
  { ehFoldable :: IO (Maybe Bool)
  , ehReturnType :: IO (Maybe DuckDBType)
  , ehFoldedValue :: IO (Maybe String)
  , ehFoldError :: IO (Maybe String)
  }

data ExpressionState = ExpressionState
  { esFoldable :: IORef (Maybe Bool)
  , esReturnType :: IORef (Maybe DuckDBType)
  , esFoldValue :: IORef (Maybe String)
  , esFoldError :: IORef (Maybe String)
  }

tests :: TestTree
tests =
  testGroup
    "Expression Interface"
    [ expressionFoldLiteral
    , expressionNonFoldable
    ]

expressionFoldLiteral :: TestTree
expressionFoldLiteral =
  testCase "folds literal expressions to constant values" $
    withDatabase \db ->
      withConnection db \conn ->
        withExpressionFunction conn "expr_literal" \ExpressionHarness{ehFoldable, ehReturnType, ehFoldedValue, ehFoldError} -> do
          withResult conn "SELECT expr_literal(42)" \_ -> pure ()
          ehFoldable >>= (@?= Just True)
          ehReturnType >>= (@?= Just DuckDBTypeInteger)
          ehFoldedValue >>= (@?= Just "42")
          ehFoldError >>= (@?= Nothing)

expressionNonFoldable :: TestTree
expressionNonFoldable =
  testCase "detects non-foldable column references" $
    withDatabase \db ->
      withConnection db \conn ->
        withExpressionFunction conn "expr_non_foldable" \ExpressionHarness{ehFoldable, ehReturnType, ehFoldedValue, ehFoldError} -> do
          withResult conn "SELECT expr_non_foldable(v) FROM (VALUES (7)) t(v)" \_ -> pure ()
          ehFoldable >>= (@?= Just False)
          ehReturnType >>= (@?= Just DuckDBTypeInteger)
          ehFoldedValue >>= (@?= Nothing)
          errMsg <- ehFoldError
          assertBool "expected fold error message" $
            maybe False (isInfixOf "fold" . map toLower) errMsg

withExpressionFunction :: DuckDBConnection -> String -> (ExpressionHarness -> IO a) -> IO a
withExpressionFunction conn funcName action = do
  foldRef <- newIORef Nothing
  typeRef <- newIORef Nothing
  valueRef <- newIORef Nothing
  errorRef <- newIORef Nothing
  let state = ExpressionState foldRef typeRef valueRef errorRef
  bindPtr <- mkScalarBindFun (expressionBind state)
  execPtr <- mkScalarExecFun expressionExec
  result <-
    withScalarFunction \scalarFun -> do
      withCString funcName \name -> c_duckdb_scalar_function_set_name scalarFun name
      withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType -> do
        c_duckdb_scalar_function_add_parameter scalarFun intType
        c_duckdb_scalar_function_set_return_type scalarFun intType
        c_duckdb_scalar_function_set_bind scalarFun bindPtr
        c_duckdb_scalar_function_set_function scalarFun execPtr
        c_duckdb_scalar_function_set_extra_info scalarFun nullPtr nullFunPtr
        c_duckdb_register_scalar_function conn scalarFun >>= (@?= DuckDBSuccess)
        action
          ExpressionHarness
            { ehFoldable = readIORef foldRef
            , ehReturnType = readIORef typeRef
            , ehFoldedValue = readIORef valueRef
            , ehFoldError = readIORef errorRef
            }
  freeHaskellFunPtr bindPtr
  freeHaskellFunPtr execPtr
  pure result

expressionBind :: ExpressionState -> DuckDBBindInfo -> IO ()
expressionBind ExpressionState{esFoldable, esReturnType, esFoldValue, esFoldError} info = do
  argCount <- c_duckdb_scalar_function_bind_get_argument_count info
  argCount @?= 1
  exprHandle <- c_duckdb_scalar_function_bind_get_argument info 0
  foldableFlag <- c_duckdb_expression_is_foldable exprHandle
  let isFoldable = foldableFlag /= 0
  writeIORef esFoldable (Just isFoldable)
  retType <- c_duckdb_expression_return_type exprHandle
  typeId <- c_duckdb_get_type_id retType
  destroyLogicalType retType
  writeIORef esReturnType (Just typeId)
  alloca \ctxPtr -> do
    c_duckdb_scalar_function_get_client_context info ctxPtr
    ctx <- peek ctxPtr
    alloca \valuePtr -> do
      poke valuePtr nullPtr
      errData <- c_duckdb_expression_fold ctx exprHandle valuePtr
      if errData == nullPtr
        then do
          valueHandle <- peek valuePtr
          if valueHandle == nullPtr
            then do
              writeIORef esFoldValue Nothing
              writeIORef esFoldError (Just "fold produced null value")
            else do
              rendered <- duckValueToString valueHandle
              writeIORef esFoldValue (Just rendered)
              writeIORef esFoldError Nothing
              destroyDuckValue valueHandle
        else do
          msgPtr <- c_duckdb_error_data_message errData
          msg <- peekCString msgPtr
          writeIORef esFoldValue Nothing
          writeIORef esFoldError (Just msg)
          destroyErrorData errData

expressionExec :: DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()
expressionExec _ chunk outVec = do
  inVec <- c_duckdb_data_chunk_get_vector chunk 0
  c_duckdb_vector_reference_vector outVec inVec

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

withScalarFunction :: (DuckDBScalarFunction -> IO a) -> IO a
withScalarFunction action = bracket c_duckdb_create_scalar_function destroy action
  where
    destroy fun = alloca \ptr -> poke ptr fun >> c_duckdb_destroy_scalar_function ptr

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType acquire action = bracket acquire destroy action
  where
    destroy lt = alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

duckValueToString :: DuckDBValue -> IO String
duckValueToString value = do
  strPtr <- c_duckdb_value_to_string value
  text <- peekCString strPtr
  c_duckdb_free (castPtr strPtr)
  pure text

destroyDuckValue :: DuckDBValue -> IO ()
destroyDuckValue value =
  alloca \ptr -> poke ptr value >> c_duckdb_destroy_value ptr

destroyErrorData :: DuckDBErrorData -> IO ()
destroyErrorData errData =
  alloca \ptr -> poke ptr errData >> c_duckdb_destroy_error_data ptr

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt =
  alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

withResult :: DuckDBConnection -> String -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn sql action =
  withCString sql \sqlPtr ->
    alloca \resPtr -> do
      state <- c_duckdb_query conn sqlPtr resPtr
      state @?= DuckDBSuccess
      result <- action resPtr
      c_duckdb_destroy_result resPtr
      pure result

-- Wrapper constructors -----------------------------------------------------

foreign import ccall "wrapper"
  mkScalarBindFun :: (DuckDBBindInfo -> IO ()) -> IO DuckDBScalarFunctionBindFun

foreign import ccall "wrapper"
  mkScalarExecFun :: (DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()) -> IO DuckDBScalarFunctionFun
