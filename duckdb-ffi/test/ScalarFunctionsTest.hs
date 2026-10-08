{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module ScalarFunctionsTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase, withResultCString)

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
                withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) \intType -> do
                    funPtr <- mkScalarFun negateCallback
                    withScalarFunction \fun -> do
                        withConstCString "negate_int" $ \name -> duckdb_scalar_function_set_name fun name
                        duckdb_scalar_function_add_parameter fun intType
                        duckdb_scalar_function_set_return_type fun intType
                        duckdb_scalar_function_set_function fun funPtr

                        duckdb_register_scalar_function conn fun >>= (@?= DuckDBSuccess)

                        withConstCString "SELECT negate_int(5)" $ \sql ->
                            withResultCString conn sql $ \resPtr ->
                                duckdb_value_int32 resPtr 0 0 >>= (@?= (-5))

-- Callback ------------------------------------------------------------------

negateCallback :: Duckdb_function_info -> Duckdb_data_chunk -> Duckdb_vector -> IO ()
negateCallback _ chunk outVec = do
    rows <- duckdb_data_chunk_get_size chunk
    inVec <- duckdb_data_chunk_get_vector chunk 0
    inPtr <- castToInt32Ptr <$> duckdb_vector_get_data inVec
    outPtr <- castToInt32Ptr <$> duckdb_vector_get_data outVec

    forM_ [0 .. fromIntegral rows - 1] \idx -> do
        val <- peekElemOff inPtr idx
        pokeElemOff outPtr idx (negate val)

    duckdb_data_chunk_set_size chunk rows

varargBind :: Ptr Void -> Duckdb_delete_callback_t -> Duckdb_copy_callback_t -> Int -> Duckdb_bind_info -> IO ()
varargBind extraPtr deleteBindCb copyCb bindDataSize info = do
    extra <- duckdb_scalar_function_bind_get_extra_info info
    assertBool "extra info propagated to bind phase" (extra == extraPtr)

    argCount <- duckdb_scalar_function_bind_get_argument_count info

    alloca \ctxPtr -> do
        duckdb_scalar_function_get_client_context info ctxPtr
        ctx <- peek ctxPtr
        unless (ctx == (coerce (nullPtr :: Ptr Void))) $
            duckdb_destroy_client_context ctxPtr

    if argCount == 0
        then withConstCString "at least one argument required" $ \msg ->
            duckdb_scalar_function_bind_set_error info msg
        else do
            expr <- duckdb_scalar_function_bind_get_argument info 0
            alloca \exprPtr -> poke exprPtr expr >> duckdb_destroy_expression exprPtr

            bindStorage <- mallocBytes bindDataSize
            poke (coerce bindStorage :: Ptr Int32) (fromIntegral argCount)

            duckdb_scalar_function_set_bind_data info bindStorage deleteBindCb
            duckdb_scalar_function_set_bind_data_copy info copyCb

varargExec :: Ptr Void -> Int -> Duckdb_function_info -> Duckdb_data_chunk -> Duckdb_vector -> IO ()
varargExec extraPtr _ info chunk outVec = do
    bindData <- duckdb_scalar_function_get_bind_data info
    argCount <-
        if bindData == (coerce (nullPtr :: Ptr Void))
            then pure (0 :: Integer)
            else fromIntegral <$> peek (coerce bindData :: Ptr Int32)

    extra <- duckdb_scalar_function_get_extra_info info
    assertBool "extra info visible during execution" (extra == extraPtr)

    rowCount <- fromIntegral <$> duckdb_data_chunk_get_size chunk
    vectors <- mapM (duckdb_data_chunk_get_vector chunk . fromIntegral) [0 .. argCount - 1]
    dataPtrs <- mapM (fmap castToInt32Ptr . duckdb_vector_get_data) vectors
    outPtr <- castToInt32Ptr <$> duckdb_vector_get_data outVec

    let loop idx
            | idx >= rowCount = pure False
            | otherwise = do
                values <- mapM (`peekElemOff` idx) dataPtrs
                let total = sum values
                if total < 0
                    then do
                        withConstCString "negative sum not allowed" $ \msg ->
                            duckdb_scalar_function_set_error info msg
                        duckdb_data_chunk_set_size chunk 0
                        pure True
                    else do
                        pokeElemOff outPtr idx total
                        loop (idx + 1)

    errored <- loop 0
    unless errored $
        duckdb_data_chunk_set_size chunk (fromIntegral rowCount)

scalarFunctionSetFeatures :: TestTree
scalarFunctionSetFeatures =
    testCase "scalar function bind helpers and function sets" $
        withDatabase \db ->
            withConnection db \conn ->
                withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) \intType -> do
                    let bindDataSize = sizeOf (undefined :: Int32)

                    extraStorage <- mallocBytes bindDataSize
                    poke (coerce extraStorage) (123 :: Int32)
                    deleteExtraCb <- mkDeleteCallback \ptr ->
                        when (ptr /= (coerce (nullPtr :: Ptr Void))) (free ptr)

                    deleteBindCb <- mkDeleteCallback \ptr ->
                        when (ptr /= (coerce (nullPtr :: Ptr Void))) (free ptr)

                    let copyBindData src
                            | src == (coerce (nullPtr :: Ptr Void)) = pure (coerce (nullPtr :: Ptr Void))
                            | otherwise = do
                                newPtr <- mallocBytes bindDataSize
                                value <- peek (coerce src :: Ptr Int32)
                                poke (coerce newPtr) value
                                pure newPtr
                    copyCb <- mkCopyCallback copyBindData

                    bindFun <- mkScalarBindFun (varargBind (coerce extraStorage) deleteBindCb copyCb bindDataSize)
                    execFun <- mkScalarFun (varargExec (coerce extraStorage) bindDataSize)

                    let functionName = "haskell_vararg"

                    withScalarFunction \fun -> do
                        withConstCString functionName $ \cName ->
                            duckdb_scalar_function_set_name fun cName
                        duckdb_scalar_function_set_return_type fun intType
                        duckdb_scalar_function_set_varargs fun intType
                        duckdb_scalar_function_set_special_handling fun
                        duckdb_scalar_function_set_volatile fun
                        duckdb_scalar_function_set_extra_info fun (coerce extraStorage) deleteExtraCb
                        duckdb_scalar_function_set_bind fun bindFun
                        duckdb_scalar_function_set_function fun execFun

                        withScalarFunctionSet functionName \funSet -> do
                            duckdb_add_scalar_function_to_set funSet fun >>= (@?= DuckDBSuccess)
                            duckdb_register_scalar_function_set conn funSet >>= (@?= DuckDBSuccess)

                            withConstCString "SELECT haskell_vararg(1, 2, 3)" \sql ->
                                withResultCString conn sql \resPtr ->
                                    duckdb_value_int32 resPtr 0 0 >>= (@?= 6)

                            expectQueryError conn "SELECT haskell_vararg()" "at least one argument"
                            expectQueryError conn "SELECT haskell_vararg(-5, 2)" "negative sum"

                    pure ()

-- Wrapper builder -----------------------------------------------------------

mkScalarFun :: (Duckdb_function_info -> Duckdb_data_chunk -> Duckdb_vector -> IO ()) -> IO Duckdb_scalar_function_t
mkScalarFun = fmap Duckdb_scalar_function_t . toFunPtr . Duckdb_scalar_function_t_Aux

mkScalarBindFun :: (Duckdb_bind_info -> IO ()) -> IO Duckdb_scalar_function_bind_t
mkScalarBindFun = fmap Duckdb_scalar_function_bind_t . toFunPtr . Duckdb_scalar_function_bind_t_Aux

mkDeleteCallback :: (Ptr Void -> IO ()) -> IO Duckdb_delete_callback_t
mkDeleteCallback = fmap Duckdb_delete_callback_t . toFunPtr . Duckdb_delete_callback_t_Aux

mkCopyCallback :: (Ptr Void -> IO (Ptr Void)) -> IO Duckdb_copy_callback_t
mkCopyCallback = fmap Duckdb_copy_callback_t . toFunPtr . Duckdb_copy_callback_t_Aux

-- Resource helpers ----------------------------------------------------------

withLogicalType :: IO Duckdb_logical_type -> (Duckdb_logical_type -> IO a) -> IO a
withLogicalType acquire = bracket acquire destroyLogicalType

destroyLogicalType :: Duckdb_logical_type -> IO ()
destroyLogicalType lt = alloca \ptr -> poke ptr lt >> duckdb_destroy_logical_type ptr

withScalarFunction :: (Duckdb_scalar_function -> IO a) -> IO a
withScalarFunction = bracket duckdb_create_scalar_function destroy
  where
    destroy fun = alloca \ptr -> poke ptr fun >> duckdb_destroy_scalar_function ptr

withScalarFunctionSet :: String -> (Duckdb_scalar_function_set -> IO a) -> IO a
withScalarFunctionSet name action =
    withConstCString name \cName ->
        bracket (duckdb_create_scalar_function_set cName) destroy action
  where
    destroy set = alloca \ptr -> poke ptr set >> duckdb_destroy_scalar_function_set ptr

expectQueryError :: Duckdb_connection -> String -> String -> IO ()
expectQueryError conn sql expectedFragment =
    withConstCString sql \sqlPtr ->
        alloca \resPtr -> do
            state <- duckdb_query conn sqlPtr resPtr
            state @?= DuckDBError
            errPtr <- duckdb_result_error resPtr
            errMsg <- (peekCString . coerce) errPtr
            let needle = map toLower expectedFragment
                haystack = map toLower errMsg
            assertBool
                ("expected fragment \"" ++ expectedFragment ++ "\" in error message:\n" ++ errMsg)
                (needle `isInfixOf` haystack)
            duckdb_destroy_result resPtr

castToInt32Ptr :: Ptr Void -> Ptr Int32
castToInt32Ptr = coerce
