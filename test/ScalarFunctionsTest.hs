{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module ScalarFunctionsTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withDatabase, withResultCString)

-- | Coverage of scalar function registration and bind/exec helpers.
tests :: TestTree
tests =
    testGroup
        "Scalar Functions"
        [ scalarFunctionRoundtrip
        , scalarFunctionSetFeatures
        ]

scalarFunctionRoundtrip :: TestTree
scalarFunctionRoundtrip =
    testCase "register custom scalar function and execute" $ do
        withDatabase \db ->
            withConnection db \conn -> do
                withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType -> do
                    funPtr <- mkScalarFun negateCallback
                    withScalarFunction \fun -> do
                        withCString "negate_int" $ \name -> c_duckdb_scalar_function_set_name fun name
                        c_duckdb_scalar_function_add_parameter fun intType
                        c_duckdb_scalar_function_set_return_type fun intType
                        c_duckdb_scalar_function_set_function fun funPtr

                        c_duckdb_register_scalar_function conn fun >>= (@?= DuckDBSuccess)

                        withCString "SELECT negate_int(5)" $ \sql ->
                            withResultCString conn sql $ \resPtr ->
                                c_duckdb_value_int32 resPtr 0 0 >>= (@?= (-5))

-- Callback ------------------------------------------------------------------

negateCallback :: DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()
negateCallback _ chunk outVec = do
    rows <- c_duckdb_data_chunk_get_size chunk
    inVec <- c_duckdb_data_chunk_get_vector chunk 0
    inPtr <- castToInt32Ptr <$> c_duckdb_vector_get_data inVec
    outPtr <- castToInt32Ptr <$> c_duckdb_vector_get_data outVec

    forM_ [0 .. fromIntegral rows - 1] \idx -> do
        val <- peekElemOff inPtr idx
        pokeElemOff outPtr idx (negate val)

    c_duckdb_data_chunk_set_size chunk rows

varargBind :: Ptr () -> DuckDBDeleteCallback -> DuckDBCopyCallback -> Int -> DuckDBBindInfo -> IO ()
varargBind extraPtr deleteBindCb copyCb bindDataSize info = do
    extra <- c_duckdb_scalar_function_bind_get_extra_info info
    assertBool "extra info propagated to bind phase" (extra == extraPtr)

    argCount <- c_duckdb_scalar_function_bind_get_argument_count info

    alloca \ctxPtr -> do
        c_duckdb_scalar_function_get_client_context info ctxPtr
        ctx <- peek ctxPtr
        unless (ctx == nullPtr) $
            c_duckdb_destroy_client_context ctxPtr

    if argCount == 0
        then withCString "at least one argument required" $ \msg ->
            c_duckdb_scalar_function_bind_set_error info msg
        else do
            expr <- c_duckdb_scalar_function_bind_get_argument info 0
            alloca \exprPtr -> poke exprPtr expr >> c_duckdb_destroy_expression exprPtr

            bindStorage <- mallocBytes bindDataSize
            poke (castPtr bindStorage :: Ptr Int32) (fromIntegral argCount)

            c_duckdb_scalar_function_set_bind_data info bindStorage deleteBindCb
            c_duckdb_scalar_function_set_bind_data_copy info copyCb

varargExec :: Ptr () -> Int -> DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()
varargExec extraPtr _ info chunk outVec = do
    bindData <- c_duckdb_scalar_function_get_bind_data info
    argCount <-
        if bindData == nullPtr
            then pure (0 :: Integer)
            else fromIntegral <$> peek (castPtr bindData :: Ptr Int32)

    extra <- c_duckdb_scalar_function_get_extra_info info
    assertBool "extra info visible during execution" (extra == extraPtr)

    rowCount <- fromIntegral <$> c_duckdb_data_chunk_get_size chunk
    vectors <- mapM (c_duckdb_data_chunk_get_vector chunk . fromIntegral) [0 .. argCount - 1]
    dataPtrs <- mapM (fmap castToInt32Ptr . c_duckdb_vector_get_data) vectors
    outPtr <- castToInt32Ptr <$> c_duckdb_vector_get_data outVec

    let loop idx
            | idx >= rowCount = pure False
            | otherwise = do
                values <- mapM (`peekElemOff` idx) dataPtrs
                let total = sum values
                if total < 0
                    then do
                        withCString "negative sum not allowed" $ \msg ->
                            c_duckdb_scalar_function_set_error info msg
                        c_duckdb_data_chunk_set_size chunk 0
                        pure True
                    else do
                        pokeElemOff outPtr idx total
                        loop (idx + 1)

    errored <- loop 0
    unless errored $
        c_duckdb_data_chunk_set_size chunk (fromIntegral rowCount)

scalarFunctionSetFeatures :: TestTree
scalarFunctionSetFeatures =
    testCase "scalar function bind helpers and function sets" $
        withDatabase \db ->
            withConnection db \conn ->
                withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType -> do
                    let bindDataSize = sizeOf (undefined :: Int32)

                    extraStorage <- mallocBytes bindDataSize
                    poke (castPtr extraStorage) (123 :: Int32)
                    deleteExtraCb <- mkDeleteCallback \ptr ->
                        when (ptr /= nullPtr) (free ptr)

                    deleteBindCb <- mkDeleteCallback \ptr ->
                        when (ptr /= nullPtr) (free ptr)

                    let copyBindData src
                            | src == nullPtr = pure nullPtr
                            | otherwise = do
                                newPtr <- mallocBytes bindDataSize
                                value <- peek (castPtr src :: Ptr Int32)
                                poke (castPtr newPtr) value
                                pure newPtr
                    copyCb <- mkCopyCallback copyBindData

                    bindFun <- mkScalarBindFun (varargBind (castPtr extraStorage) deleteBindCb copyCb bindDataSize)
                    execFun <- mkScalarFun (varargExec (castPtr extraStorage) bindDataSize)

                    let functionName = "haskell_vararg"

                    withScalarFunction \fun -> do
                        withCString functionName $ \cName ->
                            c_duckdb_scalar_function_set_name fun cName
                        c_duckdb_scalar_function_set_return_type fun intType
                        c_duckdb_scalar_function_set_varargs fun intType
                        c_duckdb_scalar_function_set_special_handling fun
                        c_duckdb_scalar_function_set_volatile fun
                        c_duckdb_scalar_function_set_extra_info fun (castPtr extraStorage) deleteExtraCb
                        c_duckdb_scalar_function_set_bind fun bindFun
                        c_duckdb_scalar_function_set_function fun execFun

                        withScalarFunctionSet functionName \funSet -> do
                            c_duckdb_add_scalar_function_to_set funSet fun >>= (@?= DuckDBSuccess)
                            c_duckdb_register_scalar_function_set conn funSet >>= (@?= DuckDBSuccess)

                            withCString "SELECT haskell_vararg(1, 2, 3)" \sql ->
                                withResultCString conn sql \resPtr ->
                                    c_duckdb_value_int32 resPtr 0 0 >>= (@?= 6)

                            expectQueryError conn "SELECT haskell_vararg()" "at least one argument"
                            expectQueryError conn "SELECT haskell_vararg(-5, 2)" "negative sum"

                    pure ()

-- Wrapper builder -----------------------------------------------------------

foreign import ccall "wrapper"
    mkScalarFun :: (DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()) -> IO DuckDBScalarFunctionFun

foreign import ccall "wrapper"
    mkScalarBindFun :: (DuckDBBindInfo -> IO ()) -> IO DuckDBScalarFunctionBindFun

foreign import ccall "wrapper"
    mkDeleteCallback :: (Ptr () -> IO ()) -> IO DuckDBDeleteCallback

foreign import ccall "wrapper"
    mkCopyCallback :: (Ptr () -> IO (Ptr ())) -> IO DuckDBCopyCallback

-- Resource helpers ----------------------------------------------------------

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType acquire = bracket acquire destroyLogicalType

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt = alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

withScalarFunction :: (DuckDBScalarFunction -> IO a) -> IO a
withScalarFunction = bracket c_duckdb_create_scalar_function destroy
  where
    destroy fun = alloca \ptr -> poke ptr fun >> c_duckdb_destroy_scalar_function ptr

withScalarFunctionSet :: String -> (DuckDBScalarFunctionSet -> IO a) -> IO a
withScalarFunctionSet name action =
    withCString name \cName ->
        bracket (c_duckdb_create_scalar_function_set cName) destroy action
  where
    destroy set = alloca \ptr -> poke ptr set >> c_duckdb_destroy_scalar_function_set ptr

expectQueryError :: DuckDBConnection -> String -> String -> IO ()
expectQueryError conn sql expectedFragment =
    withCString sql \sqlPtr ->
        alloca \resPtr -> do
            state <- c_duckdb_query conn sqlPtr resPtr
            state @?= DuckDBError
            errPtr <- c_duckdb_result_error resPtr
            errMsg <- peekCString errPtr
            let needle = map toLower expectedFragment
                haystack = map toLower errMsg
            assertBool
                ("expected fragment \"" ++ expectedFragment ++ "\" in error message:\n" ++ errMsg)
                (needle `isInfixOf` haystack)
            c_duckdb_destroy_result resPtr

castToInt32Ptr :: Ptr () -> Ptr Int32
castToInt32Ptr = castPtr
