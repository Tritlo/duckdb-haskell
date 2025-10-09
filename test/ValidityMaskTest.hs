{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module ValidityMaskTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Bits (setBit)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (poke, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- | Entry point for validity-mask focused tests.
tests :: TestTree
tests =
  testGroup
    "Validity Mask Functions"
    [ validityRowHelpers
    , validitySetOperations
    ]

validityRowHelpers :: TestTree
validityRowHelpers =
  testCase "row validity helpers reflect changes" $ do
    withIntegerVector 4 \vec -> do
      void (c_duckdb_vector_ensure_validity_writable vec)
      mask <- c_duckdb_vector_get_validity vec
      assertBool "validity pointer should not be null" (mask /= nullPtr)
      setAllValid mask 4

      toBool (c_duckdb_validity_row_is_valid mask 2) >>= (@?= True)
      c_duckdb_validity_set_row_invalid mask 2
      toBool (c_duckdb_validity_row_is_valid mask 2) >>= (@?= False)
      c_duckdb_validity_set_row_valid mask 2
      toBool (c_duckdb_validity_row_is_valid mask 2) >>= (@?= True)

validitySetOperations :: TestTree
validitySetOperations =
  testCase "set_row_validity toggles state based on CBool" $ do
    withIntegerVector 3 \vec -> do
      void (c_duckdb_vector_ensure_validity_writable vec)
      mask <- c_duckdb_vector_get_validity vec
      setAllValid mask 3

      c_duckdb_validity_set_row_validity mask 1 (CBool 0)
      toBool (c_duckdb_validity_row_is_valid mask 1) >>= (@?= False)

      c_duckdb_validity_set_row_validity mask 1 (CBool 1)
      toBool (c_duckdb_validity_row_is_valid mask 1) >>= (@?= True)

-- Helpers -------------------------------------------------------------------

withIntegerVector :: DuckDBIdx -> (DuckDBVector -> IO a) -> IO a
withIntegerVector capacity action =
  withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
    withVector (c_duckdb_create_vector intType capacity) action

withVector :: IO DuckDBVector -> (DuckDBVector -> IO a) -> IO a
withVector acquire action = bracket acquire destroyVector action
  where
    destroyVector vec = alloca \ptr -> poke ptr vec >> c_duckdb_destroy_vector ptr

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType acquire action = bracket acquire destroyLogicalType action

setAllValid :: Ptr Word64 -> Int -> IO ()
setAllValid mask count =
  let totalWords = max 1 ((count + 63) `div` 64)
   in forM_ [0 .. totalWords - 1] \wordIdx -> do
        let start = wordIdx * 64
            end = min count (start + 64)
            bits = foldl setBit (0 :: Word64) [0 .. end - start - 1]
        poke (mask `plusWord` wordIdx) bits

plusWord :: Ptr Word64 -> Int -> Ptr Word64
plusWord base idx = base `plusPtr` (idx * sizeOf (undefined :: Word64))

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt = alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

toBool :: IO CBool -> IO Bool
toBool action = do
  CBool v <- action
  pure (v /= 0)
