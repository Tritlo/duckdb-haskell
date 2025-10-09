{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module ErrorDataTest (tests) where

import Database.DuckDB.FFI
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

tests :: TestTree
tests =
  testGroup
    "Error Data"
    [createInspectDestroy]

createInspectDestroy :: TestTree
createInspectDestroy =
  testCase "create error data, inspect properties, destroy" $
    withCString "synthetic failure" \message -> do
      errData <- c_duckdb_create_error_data DuckDBErrorInvalid message
      assertBool "error data pointer should not be null" (errData /= nullPtr)

      errType <- c_duckdb_error_data_error_type errData
      errType @?= DuckDBErrorInvalid

      retrievedMessagePtr <- c_duckdb_error_data_message errData
      retrievedMessage <- peekCString retrievedMessagePtr
      retrievedMessage @?= "synthetic failure"

      hasErr <- c_duckdb_error_data_has_error errData
      hasErr @?= CBool 1

      alloca \ptr -> do
        poke ptr errData
        c_duckdb_destroy_error_data ptr
