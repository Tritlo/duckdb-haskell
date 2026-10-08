{-# LANGUAGE BlockArguments #-}

module ErrorDataTest (tests) where

import Data.Coerce (coerce)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConstCString)

tests :: TestTree
tests =
    testGroup
        "Error Data"
        [createInspectDestroy]

createInspectDestroy :: TestTree
createInspectDestroy =
    testCase "create error data, inspect properties, destroy" $
        withConstCString "synthetic failure" \message -> do
            errData <- c_duckdb_create_error_data DUCKDB_ERROR_INVALID message
            assertBool "error data pointer should not be null" (errData /= (coerce (nullPtr :: Ptr Void)))

            errType <- c_duckdb_error_data_error_type errData
            errType @?= DUCKDB_ERROR_INVALID

            retrievedMessagePtr <- c_duckdb_error_data_message errData
            retrievedMessage <- (peekCString . coerce) retrievedMessagePtr
            retrievedMessage @?= "synthetic failure"

            hasErr <- c_duckdb_error_data_has_error errData
            hasErr @?= CBool 1

            alloca \ptr -> do
                poke ptr errData
                c_duckdb_destroy_error_data ptr
