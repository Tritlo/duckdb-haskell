{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module OpenConnectTest (tests) where

import Control.Monad (forM_, when)
import Data.Coerce (coerce)
import Data.Version (Version (..), parseVersion)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import GHC.Records (getField)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Text.ParserCombinators.ReadP (char, eof, readP_to_S)
import Utils (withConstCString)

tests :: TestTree
tests =
    testGroup
        "Open Connect"
        [ testLibraryVersion
        , testOpenAndConnect
        , testOpenExtWithConfig
        , testInstanceCache
        ]

testLibraryVersion :: TestTree
testLibraryVersion =
    testCase "loads a supported DuckDB runtime" $ do
        versionPtr <- duckdb_library_version
        version <- (peekCString . coerce) versionPtr
        let unsupported = "Expected DuckDB >= 1.5.3 and < 1.6; loaded " <> version
        case readP_to_S (char 'v' *> parseVersion <* eof) version of
            [(Version [1, 5, patch] [], "")] -> assertBool unsupported (patch >= 3)
            _ -> assertFailure unsupported
        expected <- lookupEnv "DUCKDB_TEST_VERSION"
        forM_ expected $ \wanted -> version @?= ('v' : wanted)

testOpenAndConnect :: TestTree
testOpenAndConnect =
    testCase "open, connect, and gather connection details" $
        withConstCString ":memory:" $ \path ->
            alloca $ \dbPtr -> do
                state <- duckdb_open path dbPtr
                state @?= DuckDBSuccess
                db <- peek dbPtr

                alloca $ \connPtr -> do
                    connState <- duckdb_connect db connPtr
                    connState @?= DuckDBSuccess
                    conn <- peek connPtr

                    -- Query progress information (should succeed even without a running query)
                    alloca $ \progressPtr -> do
                        poke progressPtr (Duckdb_query_progress_type 0 0 0)
                        (duckdb_query_progress conn >>= poke progressPtr)
                        progress <- peek progressPtr
                        assertBool "rows processed should be non-negative" (getField @"rows_processed" progress >= 0 && getField @"total_rows_to_process" progress >= 0)

                    -- Interrupt is a no-op but should succeed
                    duckdb_interrupt conn

                    -- Retrieve client context and confirm it points back to the same connection
                    alloca $ \ctxPtr -> do
                        poke ctxPtr (coerce (nullPtr :: Ptr Void))
                        duckdb_connection_get_client_context conn ctxPtr
                        ctx <- peek ctxPtr
                        assertBool "client context should not be null" (ctx /= (coerce (nullPtr :: Ptr Void)))
                        cid <- duckdb_client_context_get_connection_id ctx
                        assertBool "connection id should be non-negative" (cid >= 0)
                        duckdb_destroy_client_context ctxPtr

                    -- Retrieve arrow options object
                    alloca $ \arrowPtr -> do
                        poke arrowPtr (coerce (nullPtr :: Ptr Void))
                        duckdb_connection_get_arrow_options conn arrowPtr
                        arrowOpts <- peek arrowPtr
                        assertBool "arrow options should not be null" (arrowOpts /= (coerce (nullPtr :: Ptr Void)))
                        duckdb_destroy_arrow_options arrowPtr

                    -- Fetch table names (there should be none, but the call should succeed)
                    alloca $ \valuePtr -> do
                        tablesValue <- withConstCString "%" $ \filterStr -> duckdb_get_table_names conn filterStr (CBool 0)
                        poke valuePtr tablesValue
                        duckdb_destroy_value valuePtr

                    -- Disconnect and close database
                    duckdb_disconnect connPtr

                duckdb_close dbPtr

testOpenExtWithConfig :: TestTree
testOpenExtWithConfig =
    testCase "open_ext with configuration" $
        withConstCString ":memory:" $ \path ->
            alloca $ \configPtr -> do
                cfgState <- duckdb_create_config configPtr
                cfgState @?= DuckDBSuccess
                config <- peek configPtr

                alloca $ \errorPtr -> do
                    poke errorPtr (coerce (nullPtr :: Ptr Void))
                    alloca $ \dbPtr -> do
                        state <- duckdb_open_ext path dbPtr config errorPtr
                        state @?= DuckDBSuccess

                        errMsgPtr <- peek errorPtr
                        when (errMsgPtr /= (coerce (nullPtr :: Ptr Void))) $
                            duckdb_free (coerce errMsgPtr)

                        duckdb_close dbPtr

                duckdb_destroy_config configPtr

testInstanceCache :: TestTree
testInstanceCache =
    testCase "create instance cache and reuse database handle" $
        withConstCString ":memory:" $ \path ->
            alloca $ \configPtr -> do
                cfgState <- duckdb_create_config configPtr
                cfgState @?= DuckDBSuccess
                config <- peek configPtr

                cache <- duckdb_create_instance_cache
                assertBool "instance cache should not be null" (cache /= (coerce (nullPtr :: Ptr Void)))

                alloca $ \dbPtr -> do
                    alloca $ \errorPtr -> do
                        poke errorPtr (coerce (nullPtr :: Ptr Void))
                        state <- duckdb_get_or_create_from_cache cache path dbPtr config errorPtr
                        state @?= DuckDBSuccess
                        err <- peek errorPtr
                        when (err /= (coerce (nullPtr :: Ptr Void))) $
                            duckdb_free (coerce err)

                        -- the database returned from the cache should be usable (we close it immediately)
                        duckdb_close dbPtr

                alloca $ \cachePtr -> do
                    poke cachePtr cache
                    duckdb_destroy_instance_cache cachePtr

                duckdb_destroy_config configPtr
