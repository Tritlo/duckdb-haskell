{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module CastFunctionsTest (tests) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString, withCStringLen)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peekElemOff, poke)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase, withLogicalType, withResult)

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
                        errPtr @?= (coerce (nullPtr :: Ptr Void))

castFunctionReportsErrors :: TestTree
castFunctionReportsErrors =
    testCase "cast failure surfaces error message" $
        withDatabase \db ->
            withConnection db \conn ->
                withTestCast conn \_ ->
                    withConstCString "SELECT CAST(v AS VARCHAR) FROM (VALUES (-7)) AS t(v)" \sql ->
                        alloca \resPtr -> do
                            state <- c_duckdb_query conn sql resPtr
                            state @?= DuckDBError
                            errPtr <- c_duckdb_result_error resPtr
                            assertBool "expected error pointer" (errPtr /= (coerce (nullPtr :: Ptr Void)))
                            errMsg <- (peekCString . coerce) errPtr
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
                        c_duckdb_value_is_null resPtr 0 1 >>= (@?= CBool 1)
                        errPtr <- c_duckdb_result_error resPtr
                        errPtr @?= (coerce (nullPtr :: Ptr Void))
                    chLastMode >>= (@?= DUCKDB_CAST_TRY)
                    chExtraSeen >>= (@?= True)

-- Harness ------------------------------------------------------------------

withTestCast :: DuckDBConnection -> (CastHarness -> IO a) -> IO a
withTestCast conn action = do
    modeRef <- newIORef DUCKDB_CAST_NORMAL
    extraSeenRef <- newIORef False
    prefixStable <- newStablePtr ("value: " :: String)
    castFunPtr <- mkCastFun (castCallback modeRef extraSeenRef prefixStable)
    result <-
        withCastFunction \castFun ->
            withLogicalType (c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_INTEGER)) \sourceType ->
                withLogicalType (c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_VARCHAR)) \targetType -> do
                    c_duckdb_cast_function_set_source_type castFun sourceType
                    c_duckdb_cast_function_set_target_type castFun targetType
                    c_duckdb_cast_function_set_implicit_cast_cost castFun 0
                    c_duckdb_cast_function_set_extra_info castFun ((coerce . castStablePtrToPtr) prefixStable) (coerce (nullFunPtr :: FunPtr Void))
                    c_duckdb_cast_function_set_function castFun castFunPtr
                    c_duckdb_register_cast_function conn castFun >>= (@?= DuckDBSuccess)
                    action
                        CastHarness
                            { chLastMode = readIORef modeRef
                            , chExtraSeen = readIORef extraSeenRef
                            }
    (freeHaskellFunPtr . coerce) castFunPtr
    freeStablePtr prefixStable
    pure result

castCallback :: IORef DuckDBCastMode -> IORef Bool -> StablePtr String -> DuckDBFunctionInfo -> DuckDBIdx -> DuckDBVector -> DuckDBVector -> IO CBool
castCallback modeRef extraSeenRef prefixStable info count inputVec outputVec = do
    actualPtr <- c_duckdb_cast_function_get_extra_info info
    when (actualPtr == (coerce . castStablePtrToPtr) prefixStable) $ writeIORef extraSeenRef True
    prefix <-
        if actualPtr == (coerce (nullPtr :: Ptr Void))
            then pure "value: "
            else deRefStablePtr ((castPtrToStablePtr . coerce) actualPtr)
    mode <- c_duckdb_cast_function_get_cast_mode info
    writeIORef modeRef mode
    inputData <- c_duckdb_vector_get_data inputVec
    c_duckdb_vector_ensure_validity_writable outputVec
    let inPtr = coerce inputData :: Ptr Int32
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
                then withConstCString "negative values not allowed" \errMsg ->
                    if mode == DUCKDB_CAST_TRY
                        then do
                            c_duckdb_cast_function_set_row_error info errMsg (fromIntegral idx) outVec
                            processRows prefix mode inPtr outVec (idx + 1) total
                        else do
                            c_duckdb_cast_function_set_error info errMsg
                            pure False
                else do
                    let rendered = prefix ++ show val
                    withCStringLen rendered \(cStr, len) ->
                        c_duckdb_vector_assign_string_element_len outVec (fromIntegral idx) (coerce cStr) (fromIntegral len)
                    processRows prefix mode inPtr outVec (idx + 1) total

-- Helpers ------------------------------------------------------------------

withCastFunction :: (DuckDBCastFunction -> IO a) -> IO a
withCastFunction = bracket c_duckdb_create_cast_function destroy
  where
    destroy cf = alloca \ptr -> poke ptr cf >> c_duckdb_destroy_cast_function ptr

fetchString :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO String
fetchString resPtr col row = do
    cStr <- c_duckdb_value_varchar resPtr col row
    value <- (peekCString . coerce) cStr
    c_duckdb_free (coerce cStr)
    pure value

-- Wrapper constructors ------------------------------------------------------

mkCastFun :: (DuckDBFunctionInfo -> DuckDBIdx -> DuckDBVector -> DuckDBVector -> IO CBool) -> IO DuckDBCastFunctionFun
mkCastFun = fmap DuckDBCastFunctionFun . toFunPtr . DuckDBCastFunctionFun_Aux
