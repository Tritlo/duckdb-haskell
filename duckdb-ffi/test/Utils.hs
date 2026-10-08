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

withDatabase :: (Duckdb_database -> IO a) -> IO a
withDatabase action =
    alloca \dbPtr -> do
        poke dbPtr (Duckdb_database nullPtr)
        bracket_ (pure ()) (duckdb_close dbPtr) $
            withConstCString ":memory:" \path -> do
                duckdb_open path dbPtr >>= (@?= DuckDBSuccess)
                peek dbPtr >>= action

withConnection :: Duckdb_database -> (Duckdb_connection -> IO a) -> IO a
withConnection db = bracket acquire release
  where
    acquire =
        alloca \connPtr -> do
            duckdb_connect db connPtr >>= (@?= DuckDBSuccess)
            peek connPtr
    release conn =
        alloca \connPtr -> do
            poke connPtr conn
            duckdb_disconnect connPtr

withResult :: Duckdb_connection -> String -> (Ptr Duckdb_result -> IO a) -> IO a
withResult conn sql action =
    withConstCString sql \sqlPtr -> withResultCString conn sqlPtr action

withResultCString :: Duckdb_connection -> (ConstPtr CChar) -> (Ptr Duckdb_result -> IO a) -> IO a
withResultCString conn sql action =
    alloca \resPtr -> do
        poke resPtr Struct.zero
        bracket_ (pure ()) (duckdb_destroy_result resPtr) $ do
            duckdb_query conn sql resPtr >>= (@?= DuckDBSuccess)
            action resPtr

withValue :: IO Duckdb_value -> (Duckdb_value -> IO a) -> IO a
withValue acquire = bracket acquire destroyDuckValue

withDuckValue :: IO Duckdb_value -> (Duckdb_value -> IO a) -> IO a
withDuckValue = withValue

destroyDuckValue :: Duckdb_value -> IO ()
destroyDuckValue value =
    alloca \ptr -> poke ptr value >> duckdb_destroy_value ptr

withLogicalType :: IO Duckdb_logical_type -> (Duckdb_logical_type -> IO a) -> IO a
withLogicalType acquire = bracket acquire destroyLogicalType

destroyLogicalType :: Duckdb_logical_type -> IO ()
destroyLogicalType lt =
    alloca \ptr -> poke ptr lt >> duckdb_destroy_logical_type ptr

withSelectionVector :: Idx_t -> (Duckdb_selection_vector -> IO a) -> IO a
withSelectionVector n = bracket (duckdb_create_selection_vector n) duckdb_destroy_selection_vector

withScalarFunction :: (Duckdb_scalar_function -> IO a) -> IO a
withScalarFunction = bracket duckdb_create_scalar_function destroy
  where
    destroy fun =
        alloca \ptr -> poke ptr fun >> duckdb_destroy_scalar_function ptr

withVector :: IO Duckdb_vector -> (Duckdb_vector -> IO a) -> IO a
withVector acquire = bracket acquire destroyVector
  where
    destroyVector vec =
        alloca \ptr -> poke ptr vec >> duckdb_destroy_vector ptr

withVectorOfType :: Duckdb_logical_type -> Idx_t -> (Duckdb_vector -> IO a) -> IO a
withVectorOfType lt capacity = withVector (duckdb_create_vector lt capacity)

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

destroyErrorData :: Duckdb_error_data -> IO ()
destroyErrorData errData =
    alloca \ptr -> poke ptr errData >> duckdb_destroy_error_data ptr

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
