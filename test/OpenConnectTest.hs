{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module OpenConnectTest (tests) where

import Control.Monad (when)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

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
    testCase "duckdb_library_version returns a non-empty string" $ do
        versionPtr <- c_duckdb_library_version
        version <- peekCString versionPtr
        assertBool "library version should not be empty" (not (null version))

testOpenAndConnect :: TestTree
testOpenAndConnect =
    testCase "open, connect, and gather connection details" $
        withCString ":memory:" $ \path ->
            alloca $ \dbPtr -> do
                state <- c_duckdb_open path dbPtr
                state @?= DuckDBSuccess
                db <- peek dbPtr

                alloca $ \connPtr -> do
                    connState <- c_duckdb_connect db connPtr
                    connState @?= DuckDBSuccess
                    conn <- peek connPtr

                    -- Query progress information (should succeed even without a running query)
                    alloca $ \progressPtr -> do
                        poke progressPtr (DuckDBQueryProgress 0 0 0)
                        c_duckdb_query_progress conn progressPtr
                        DuckDBQueryProgress{..} <- peek progressPtr
                        assertBool "rows processed should be non-negative" (duckDBQueryProgressRowsProcessed >= 0 && duckDBQueryProgressTotalRows >= 0)

                    -- Interrupt is a no-op but should succeed
                    c_duckdb_interrupt conn

                    -- Retrieve client context and confirm it points back to the same connection
                    alloca $ \ctxPtr -> do
                        poke ctxPtr nullPtr
                        c_duckdb_connection_get_client_context conn ctxPtr
                        ctx <- peek ctxPtr
                        assertBool "client context should not be null" (ctx /= nullPtr)
                        cid <- c_duckdb_client_context_get_connection_id ctx
                        assertBool "connection id should be non-negative" (cid >= 0)
                        c_duckdb_destroy_client_context ctxPtr

                    -- Retrieve arrow options object
                    alloca $ \arrowPtr -> do
                        poke arrowPtr nullPtr
                        c_duckdb_connection_get_arrow_options conn arrowPtr
                        arrowOpts <- peek arrowPtr
                        assertBool "arrow options should not be null" (arrowOpts /= nullPtr)
                        c_duckdb_destroy_arrow_options arrowPtr

                    -- Fetch table names (there should be none, but the call should succeed)
                    alloca $ \valuePtr -> do
                        tablesValue <- withCString "%" $ \filterStr -> c_duckdb_get_table_names conn filterStr (CBool 0)
                        poke valuePtr tablesValue
                        c_duckdb_destroy_value valuePtr

                    -- Disconnect and close database
                    c_duckdb_disconnect connPtr

                c_duckdb_close dbPtr

testOpenExtWithConfig :: TestTree
testOpenExtWithConfig =
    testCase "open_ext with configuration" $
        withCString ":memory:" $ \path ->
            alloca $ \configPtr -> do
                cfgState <- c_duckdb_create_config configPtr
                cfgState @?= DuckDBSuccess
                config <- peek configPtr

                alloca $ \errorPtr -> do
                    poke errorPtr nullPtr
                    alloca $ \dbPtr -> do
                        state <- c_duckdb_open_ext path dbPtr config errorPtr
                        state @?= DuckDBSuccess

                        errMsgPtr <- peek errorPtr
                        when (errMsgPtr /= nullPtr) $
                            c_duckdb_free (castPtr errMsgPtr)

                        c_duckdb_close dbPtr

                c_duckdb_destroy_config configPtr

testInstanceCache :: TestTree
testInstanceCache =
    testCase "create instance cache and reuse database handle" $
        withCString ":memory:" $ \path ->
            alloca $ \configPtr -> do
                cfgState <- c_duckdb_create_config configPtr
                cfgState @?= DuckDBSuccess
                config <- peek configPtr

                cache <- c_duckdb_create_instance_cache
                assertBool "instance cache should not be null" (cache /= nullPtr)

                alloca $ \dbPtr -> do
                    alloca $ \errorPtr -> do
                        poke errorPtr nullPtr
                        state <- c_duckdb_get_or_create_from_cache cache path dbPtr config errorPtr
                        state @?= DuckDBSuccess
                        err <- peek errorPtr
                        when (err /= nullPtr) $
                            c_duckdb_free (castPtr err)

                        -- the database returned from the cache should be usable (we close it immediately)
                        c_duckdb_close dbPtr

                alloca $ \cachePtr -> do
                    poke cachePtr cache
                    c_duckdb_destroy_instance_cache cachePtr

                c_duckdb_destroy_config configPtr
