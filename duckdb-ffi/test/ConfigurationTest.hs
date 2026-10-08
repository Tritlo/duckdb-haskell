{-# LANGUAGE BlockArguments #-}

module ConfigurationTest (tests) where

import Data.Coerce (coerce)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConstCString)

tests :: TestTree
tests =
    testGroup
        "Configuration"
        [configurationLifecycle]

configurationLifecycle :: TestTree
configurationLifecycle =
    testCase "create, inspect, mutate, and destroy configuration" $
        alloca \configPtr -> do
            state <- c_duckdb_create_config configPtr
            state @?= DuckDBSuccess

            config <- peek configPtr
            assertBool "config pointer should not be null" (config /= (coerce (nullPtr :: Ptr Void)))

            count <- c_duckdb_config_count
            assertBool "config flag count should be > 0" (count > 0)

            alloca \namePtr -> alloca \descPtr -> do
                flagState <- c_duckdb_get_config_flag 0 namePtr descPtr
                flagState @?= DuckDBSuccess
                flagName <- peek namePtr >>= (peekCString . coerce)
                flagDesc <- peek descPtr >>= (peekCString . coerce)
                assertBool "flag name should not be empty" (not (null flagName))
                assertBool "flag description should not be empty" (not (null flagDesc))

            setState <-
                withConstCString "access_mode" \flag ->
                    withConstCString "READ_WRITE" $ \value ->
                        c_duckdb_set_config config flag value
            setState @?= DuckDBSuccess

            c_duckdb_destroy_config configPtr
