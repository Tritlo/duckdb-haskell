{-# LANGUAGE BlockArguments #-}

module ValidityMaskTest (tests) where

import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.Types (CBool (..))
import Foreign.Ptr (Ptr, nullPtr)
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
            void (duckdb_vector_ensure_validity_writable vec)
            mask <- duckdb_vector_get_validity vec
            assertBool "validity pointer should not be null" (mask /= (coerce (nullPtr :: Ptr Void)))
            setAllValid mask 4

            toBool (duckdb_validity_row_is_valid mask 2) >>= (@?= True)
            duckdb_validity_set_row_invalid mask 2
            toBool (duckdb_validity_row_is_valid mask 2) >>= (@?= False)
            duckdb_validity_set_row_valid mask 2
            toBool (duckdb_validity_row_is_valid mask 2) >>= (@?= True)

validitySetOperations :: TestTree
validitySetOperations =
    testCase "set_row_validity toggles state based on CBool" $ do
        withIntegerVector 3 \vec -> do
            void (duckdb_vector_ensure_validity_writable vec)
            mask <- duckdb_vector_get_validity vec
            setAllValid mask 3

            duckdb_validity_set_row_validity mask 1 (CBool 0)
            toBool (duckdb_validity_row_is_valid mask 1) >>= (@?= False)

            duckdb_validity_set_row_validity mask 1 (CBool 1)
            toBool (duckdb_validity_row_is_valid mask 1) >>= (@?= True)

-- Helpers -------------------------------------------------------------------

withIntegerVector :: Idx_t -> (Duckdb_vector -> IO a) -> IO a
withIntegerVector capacity action =
    withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)) \intType ->
        withVector (duckdb_create_vector intType capacity) action

toBool :: IO CBool -> IO Bool
toBool action = do
    CBool v <- action
    pure (v /= 0)
