{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module VectorTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Bits (clearBit, complement, setBit)
import Data.Int (Int32)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, plusPtr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- | Entry point for vector-centric tests.
tests :: TestTree
tests =
  testGroup
    "Vector Interface"
    [ vectorDataAccess
    , vectorValidityMask
    ]

-- | Create a numeric vector, write elements, and observe the stored values.
vectorDataAccess :: TestTree
vectorDataAccess =
  testCase "write and read integer vector data" $ do
    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
      withVector intType 4 \vec -> do
        colType <- c_duckdb_vector_get_column_type vec
        withLogicalType (pure colType) \lt -> c_duckdb_get_type_id lt >>= (@?= DuckDBTypeInteger)

        rawPtr <- c_duckdb_vector_get_data vec
        let dataPtr = castPtr rawPtr :: Ptr Int32
        forM_ (zip [0 ..] [10, 20, 30, 40]) \(idx, val) -> poke (dataPtr `plusElem` idx) val

        forM_ (zip [0 ..] [10, 20, 30, 40]) \(idx, val) -> peek (dataPtr `plusElem` idx) >>= (@?= val)

-- | Ensure validity mask allocation and manipulate individual bits.
vectorValidityMask :: TestTree
vectorValidityMask =
  testCase "ensure validity mask and clear single entry" $ do
    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
      withVector intType 4 \vec -> do
        void (c_duckdb_vector_ensure_validity_writable vec)
        validity <- c_duckdb_vector_get_validity vec
        assertBool "validity mask should exist" (validity /= nullPtr)

        setAllValid validity 4
        clearValidityBit validity 2

        current <- peek validity
        let expected = foldl setBit (0 :: Word64) [0, 1, 3]
        current @?= expected

-- helpers -------------------------------------------------------------------

withVector :: DuckDBLogicalType -> DuckDBIdx -> (DuckDBVector -> IO a) -> IO a
withVector lt capacity action = bracket (c_duckdb_create_vector lt capacity) destroy action
  where
    destroy vec = alloca \ptr -> poke ptr vec >> c_duckdb_destroy_vector ptr

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType acquire action = bracket acquire destroyLogicalType action

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt = alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

plusElem :: Ptr Int32 -> Int -> Ptr Int32
plusElem base idx = base `plusPtr` (idx * sizeOf (undefined :: Int32))

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
