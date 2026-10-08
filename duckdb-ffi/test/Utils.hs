{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Utils (
    withDatabase,
    withConnection,
    withResult,
    withResultCString,
    withValue,
    withDuckValue,
    destroyDuckValue,
    withLogicalType,
    destroyLogicalType,
    withSelectionVector,
    withScalarFunction,
    withVector,
    withVectorOfType,
    setAllValid,
    clearValidityBit,
    plusWord,
    destroyErrorData,
    withConstCString,
    releaseArrowSchema,
    releaseArrowArray,
    releaseArrowStream,
) where

import Control.Exception (bracket, bracket_, mask_)
import Control.Monad (forM_, when)
import Data.Bits (clearBit, setBit)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import HsBindgen.Runtime.Struct qualified as Struct
import HsBindgen.Runtime.Support.FunPtr (fromFunPtr)
import Test.Tasty.HUnit ((@?=))

-- | Supply a constant C string for one native call.
withConstCString :: String -> (ConstPtr CChar -> IO a) -> IO a
withConstCString text action = withCString text (action . ConstPtr)

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase action =
    alloca \dbPtr -> do
        poke dbPtr (DuckDBDatabase nullPtr)
        bracket_ (pure ()) (c_duckdb_close dbPtr) $
            withConstCString ":memory:" \path -> do
                c_duckdb_open path dbPtr >>= (@?= DuckDBSuccess)
                peek dbPtr >>= action

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db = bracket acquire release
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
    withConstCString sql \sqlPtr -> withResultCString conn sqlPtr action

withResultCString :: DuckDBConnection -> (ConstPtr CChar) -> (Ptr DuckDBResult -> IO a) -> IO a
withResultCString conn sql action =
    alloca \resPtr -> do
        poke resPtr Struct.zero
        bracket_ (pure ()) (c_duckdb_destroy_result resPtr) $ do
            c_duckdb_query conn sql resPtr >>= (@?= DuckDBSuccess)
            action resPtr

withValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO a
withValue acquire = bracket acquire destroyDuckValue

withDuckValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO a
withDuckValue = withValue

destroyDuckValue :: DuckDBValue -> IO ()
destroyDuckValue value =
    alloca \ptr -> poke ptr value >> c_duckdb_destroy_value ptr

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType acquire = bracket acquire destroyLogicalType

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt =
    alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

withSelectionVector :: DuckDBIdx -> (DuckDBSelectionVector -> IO a) -> IO a
withSelectionVector n = bracket (c_duckdb_create_selection_vector n) c_duckdb_destroy_selection_vector

withScalarFunction :: (DuckDBScalarFunction -> IO a) -> IO a
withScalarFunction = bracket c_duckdb_create_scalar_function destroy
  where
    destroy fun =
        alloca \ptr -> poke ptr fun >> c_duckdb_destroy_scalar_function ptr

withVector :: IO DuckDBVector -> (DuckDBVector -> IO a) -> IO a
withVector acquire = bracket acquire destroyVector
  where
    destroyVector vec =
        alloca \ptr -> poke ptr vec >> c_duckdb_destroy_vector ptr

withVectorOfType :: DuckDBLogicalType -> DuckDBIdx -> (DuckDBVector -> IO a) -> IO a
withVectorOfType lt capacity = withVector (c_duckdb_create_vector lt capacity)

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

-- | Release the buffers of an initialized Arrow schema.
releaseArrowSchema :: Ptr ArrowSchema -> IO ()
releaseArrowSchema ptr = mask_ do
    value <- peek ptr
    when (value.release /= nullFunPtr) (fromFunPtr value.release ptr)

-- | Release the buffers of an initialized Arrow array.
releaseArrowArray :: Ptr ArrowArray -> IO ()
releaseArrowArray ptr = mask_ do
    value <- peek ptr
    when (value.release /= nullFunPtr) (fromFunPtr value.release ptr)

-- | Release the state of an initialized Arrow stream.
releaseArrowStream :: Ptr ArrowArrayStream -> IO ()
releaseArrowStream ptr = mask_ do
    value <- peek ptr
    when (value.release /= nullFunPtr) (fromFunPtr value.release ptr)
