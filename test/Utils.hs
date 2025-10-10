{-# LANGUAGE BlockArguments #-}

module Utils
  ( withDatabase
  , withConnection
  , withResult
  , withResultCString
  , withValue
  , withDuckValue
  , destroyDuckValue
  , withLogicalType
  , destroyLogicalType
  , withSelectionVector
  , withScalarFunction
  , withVector
  , withVectorOfType
  , setAllValid
  , clearValidityBit
  , plusWord
  , destroyErrorData
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Bits (clearBit, setBit)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import Test.Tasty.HUnit ((@?=))

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
withConnection db action =
  bracket acquire release action
  where
    acquire =
      alloca \connPtr -> do
        c_duckdb_connect db connPtr >>= (@?= DuckDBSuccess)
        peek connPtr
    release conn =
      alloca \connPtr -> do
        poke connPtr conn
        c_duckdb_disconnect connPtr

withResult :: DuckDBConnection -> String -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn sql action =
  withCString sql \sqlPtr -> withResultCString conn sqlPtr action

withResultCString :: DuckDBConnection -> CString -> (Ptr DuckDBResult -> IO a) -> IO a
withResultCString conn sql action =
  alloca \resPtr -> do
    c_duckdb_query conn sql resPtr >>= (@?= DuckDBSuccess)
    result <- action resPtr
    c_duckdb_destroy_result resPtr
    pure result

withValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO a
withValue acquire = bracket acquire destroyDuckValue

withDuckValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO a
withDuckValue = withValue

destroyDuckValue :: DuckDBValue -> IO ()
destroyDuckValue value =
  alloca \ptr -> poke ptr value >> c_duckdb_destroy_value ptr

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType acquire action = bracket acquire destroyLogicalType action

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt =
  alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

withSelectionVector :: DuckDBIdx -> (DuckDBSelectionVector -> IO a) -> IO a
withSelectionVector n action =
  bracket (c_duckdb_create_selection_vector n) c_duckdb_destroy_selection_vector action

withScalarFunction :: (DuckDBScalarFunction -> IO a) -> IO a
withScalarFunction action =
  bracket c_duckdb_create_scalar_function destroy action
  where
    destroy fun =
      alloca \ptr -> poke ptr fun >> c_duckdb_destroy_scalar_function ptr

withVector :: IO DuckDBVector -> (DuckDBVector -> IO a) -> IO a
withVector acquire action =
  bracket acquire destroyVector action
  where
    destroyVector vec =
      alloca \ptr -> poke ptr vec >> c_duckdb_destroy_vector ptr

withVectorOfType :: DuckDBLogicalType -> DuckDBIdx -> (DuckDBVector -> IO a) -> IO a
withVectorOfType lt capacity action =
  withVector (c_duckdb_create_vector lt capacity) action

setAllValid :: Ptr Word64 -> Int -> IO ()
setAllValid mask count =
  let totalWords = max 1 ((count + 63) `div` 64)
   in forM_ [0 .. totalWords - 1] \wordIdx -> do
        let start = wordIdx * 64
            end = min count (start + 64)
            bits = foldl setBit 0 [0 .. end - start - 1]
        poke (mask `plusWord` wordIdx) bits

clearValidityBit :: Ptr Word64 -> Int -> IO ()
clearValidityBit mask idx = do
  let wordIdx = idx `div` 64
      bitIdx = idx `mod` 64
      entryPtr = mask `plusWord` wordIdx
  current <- peek entryPtr
  poke entryPtr (clearBit current bitIdx)

plusWord :: Ptr Word64 -> Int -> Ptr Word64
plusWord base idx = base `plusPtr` (idx * sizeOf (undefined :: Word64))

destroyErrorData :: DuckDBErrorData -> IO ()
destroyErrorData errData =
  alloca \ptr -> poke ptr errData >> c_duckdb_destroy_error_data ptr
