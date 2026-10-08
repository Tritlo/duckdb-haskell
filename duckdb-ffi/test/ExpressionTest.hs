{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module ExpressionTest (tests) where

import Control.Exception (finally)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, poke)
import GHC.Records (getField)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (
    destroyDuckValue,
    destroyErrorData,
    destroyLogicalType,
    withConnection,
    withConstCString,
    withDatabase,
    withLogicalType,
    withResult,
    withScalarFunction,
 )

data ExpressionHarness = ExpressionHarness
    { ehFoldable :: IO (Maybe Bool)
    , ehReturnType :: IO (Maybe DUCKDB_TYPE)
    , ehFoldedValue :: IO (Maybe String)
    , ehFoldError :: IO (Maybe String)
    }

data ExpressionState = ExpressionState
    { esFoldable :: IORef (Maybe Bool)
    , esReturnType :: IORef (Maybe DUCKDB_TYPE)
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
                    ehReturnType >>= (@?= Just DUCKDB_TYPE_INTEGER)
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
                    ehReturnType >>= (@?= Just DUCKDB_TYPE_INTEGER)
                    ehFoldedValue >>= (@?= Nothing)
                    errMsg <- ehFoldError
                    assertBool "expected fold error message" $
                        maybe False (isInfixOf "fold" . map toLower) errMsg

withExpressionFunction :: Duckdb_connection -> String -> (ExpressionHarness -> IO a) -> IO a
withExpressionFunction conn funcName action = do
    foldRef <- newIORef Nothing
    typeRef <- newIORef Nothing
    valueRef <- newIORef Nothing
    errorRef <- newIORef Nothing
    let state = ExpressionState foldRef typeRef valueRef errorRef
    bindPtr <- mkScalarBindFun (expressionBind state)
    execPtr <- mkScalarExecFun expressionExec
    result <-
        withScalarFunction $ \scalarFun -> do
            withConstCString funcName $ \name -> duckdb_scalar_function_set_name scalarFun name
            withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) $ \intType -> do
                duckdb_scalar_function_add_parameter scalarFun intType
                duckdb_scalar_function_set_return_type scalarFun intType
                duckdb_scalar_function_set_bind scalarFun bindPtr
                duckdb_scalar_function_set_function scalarFun execPtr
                duckdb_scalar_function_set_extra_info scalarFun (coerce (nullPtr :: Ptr Void)) (coerce (nullFunPtr :: FunPtr Void))
                duckdb_register_scalar_function conn scalarFun >>= (@?= DuckDBSuccess)
                action
                    ExpressionHarness
                        { ehFoldable = readIORef foldRef
                        , ehReturnType = readIORef typeRef
                        , ehFoldedValue = readIORef valueRef
                        , ehFoldError = readIORef errorRef
                        }
    (freeHaskellFunPtr . coerce) bindPtr
    (freeHaskellFunPtr . coerce) execPtr
    pure result

expressionBind :: ExpressionState -> Duckdb_bind_info -> IO ()
expressionBind ExpressionState{esFoldable, esReturnType, esFoldValue, esFoldError} info = do
    argCount <- duckdb_scalar_function_bind_get_argument_count info
    argCount @?= 1
    exprHandle <- duckdb_scalar_function_bind_get_argument info 0
    finally
        ( do
            foldableFlag <- duckdb_expression_is_foldable exprHandle
            let isFoldable = foldableFlag /= 0
            writeIORef esFoldable (Just isFoldable)
            retType <- duckdb_expression_return_type exprHandle
            typeId <- (fmap (getField @"unwrap") . duckdb_get_type_id) retType
            destroyLogicalType retType
            writeIORef esReturnType (Just typeId)
            alloca \ctxPtr -> do
                duckdb_scalar_function_get_client_context info ctxPtr
                ctx <- peek ctxPtr
                alloca \valuePtr -> do
                    poke valuePtr (coerce (nullPtr :: Ptr Void))
                    errData <- duckdb_expression_fold ctx exprHandle valuePtr
                    if errData == (coerce (nullPtr :: Ptr Void))
                        then do
                            valueHandle <- peek valuePtr
                            if valueHandle == (coerce (nullPtr :: Ptr Void))
                                then do
                                    writeIORef esFoldValue Nothing
                                    writeIORef esFoldError (Just "fold produced null value")
                                else do
                                    rendered <- duckValueToString valueHandle
                                    writeIORef esFoldValue (Just rendered)
                                    writeIORef esFoldError Nothing
                                    destroyDuckValue valueHandle
                        else do
                            msgPtr <- duckdb_error_data_message errData
                            msg <- (peekCString . coerce) msgPtr
                            writeIORef esFoldValue Nothing
                            writeIORef esFoldError (Just msg)
                            destroyErrorData errData
        )
        (alloca \exprPtr -> poke exprPtr exprHandle >> duckdb_destroy_expression exprPtr)

expressionExec :: Duckdb_function_info -> Duckdb_data_chunk -> Duckdb_vector -> IO ()
expressionExec _ chunk outVec = do
    inVec <- duckdb_data_chunk_get_vector chunk 0
    duckdb_vector_reference_vector outVec inVec

-- Helpers ------------------------------------------------------------------

duckValueToString :: Duckdb_value -> IO String
duckValueToString value = do
    strPtr <- duckdb_value_to_string value
    text <- (peekCString . coerce) strPtr
    duckdb_free (coerce strPtr)
    pure text

-- Wrapper constructors -----------------------------------------------------

mkScalarBindFun :: (Duckdb_bind_info -> IO ()) -> IO Duckdb_scalar_function_bind_t
mkScalarBindFun = fmap Duckdb_scalar_function_bind_t . toFunPtr . Duckdb_scalar_function_bind_t_Aux

mkScalarExecFun :: (Duckdb_function_info -> Duckdb_data_chunk -> Duckdb_vector -> IO ()) -> IO Duckdb_scalar_function_t
mkScalarExecFun = fmap Duckdb_scalar_function_t . toFunPtr . Duckdb_scalar_function_t_Aux
