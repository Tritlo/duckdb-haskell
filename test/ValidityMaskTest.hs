{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module ValidityMaskTest (tests) where

import Control.Monad (void)
import Database.DuckDB.FFI
import Foreign.C.Types (CBool (..))
import Foreign.Ptr (nullPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (setAllValid, withLogicalType, withVector)

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

toBool :: IO CBool -> IO Bool
toBool action = do
  CBool v <- action
  pure (v /= 0)
