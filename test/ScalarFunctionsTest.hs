{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module ScalarFunctionsTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Int (Int32)
import Database.DuckDB.FFI
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- | Minimal exercise of scalar function registration APIs.
tests :: TestTree
tests = testGroup "Scalar Functions" [scalarFunctionRoundtrip]

scalarFunctionRoundtrip :: TestTree
scalarFunctionRoundtrip =
  testCase "register custom scalar function and execute" $ do
    withDatabase \db ->
      withConnection db \conn -> do
        withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType -> do
          funPtr <- mkScalarFun negateCallback
          withScalarFunction \fun -> do
            withCString "negate_int" \name -> c_duckdb_scalar_function_set_name fun name
            c_duckdb_scalar_function_add_parameter fun intType
            c_duckdb_scalar_function_set_return_type fun intType
            c_duckdb_scalar_function_set_function fun funPtr

            c_duckdb_register_scalar_function conn fun >>= (@?= DuckDBSuccess)

            withCString "SELECT negate_int(5)" \sql ->
              withResult conn sql \resPtr ->
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

-- Wrapper builder -----------------------------------------------------------

foreign import ccall "wrapper"
  mkScalarFun :: (DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()) -> IO DuckDBScalarFunctionFun

-- Resource helpers ----------------------------------------------------------

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
withLogicalType acquire action = bracket acquire destroyLogicalType action

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt = alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

withScalarFunction :: (DuckDBScalarFunction -> IO a) -> IO a
withScalarFunction action = bracket c_duckdb_create_scalar_function destroy action
  where
    destroy fun = alloca \ptr -> poke ptr fun >> c_duckdb_destroy_scalar_function ptr

withResult :: DuckDBConnection -> CString -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn sql action =
  alloca \resPtr -> do
    c_duckdb_query conn sql resPtr >>= (@?= DuckDBSuccess)
    result <- action resPtr
    c_duckdb_destroy_result resPtr
    pure result

castToInt32Ptr :: Ptr () -> Ptr Int32
castToInt32Ptr = castPtr
