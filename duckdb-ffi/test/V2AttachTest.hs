module V2AttachTest (tests) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Coerce (coerce)
import Database.DuckDB.FFI.V2
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Ptr (nullPtr)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (checked, readIntegers, removeTestFile, withHandle, withInstance, withResult, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 attach options"
        [ testCase "attach options pass READ_ONLY to the attach, and an unknown option fails it" testAttachOptions
        ]

-- | Set one attach option from text.
setOption :: DuckDBV2AttachOptionsHandle -> String -> String -> IO ()
setOption options key setting = withView key $ \name -> withView setting $ \value ->
    checked (c_duckdb_v2_attach_options_set options (coerce name) value)

testAttachOptions :: IO ()
testAttachOptions =
    bracket
        ( do
            (path, handle) <- openBinaryTempFile "." "duckdb-v2-attach"
            hClose handle
            -- The first attach creates the database file.
            withCString path $ \name -> removeTestFile name >>= (@?= 0)
            pure path
        )
        (\path -> withCString path (void . removeTestFile))
        ( \path -> withInstance $ \instanceHandle ->
            withHandle (c_duckdb_v2_connection_create instanceHandle) c_duckdb_v2_connection_destroy $ \conn -> withView path $ \file -> do
                let attach name options = withView name $ \view ->
                        checked (c_duckdb_v2_instance_attach instanceHandle (coerce view) file options (CBool 0))
                    detach name = withView name $ \view -> checked (c_duckdb_v2_instance_detach instanceHandle (coerce view))
                    readOnly name = withResult conn ("SELECT readonly::BIGINT FROM duckdb_databases() WHERE database_name = '" <> name <> "'") readIntegers
                attach "v2_created" (coerce nullPtr)
                readOnly "v2_created" >>= (@?= [Just 0])
                detach "v2_created"
                withHandle (c_duckdb_v2_attach_options_create instanceHandle) c_duckdb_v2_attach_options_destroy $ \options -> do
                    -- The key is case-insensitive, and the second value for the key replaces the first.
                    setOption options "read_only" "false"
                    setOption options "READ_ONLY" "true"
                    attach "v2_read_only" options
                    readOnly "v2_read_only" >>= (@?= [Just 1])
                    detach "v2_read_only"
                withHandle (c_duckdb_v2_attach_options_create instanceHandle) c_duckdb_v2_attach_options_destroy $ \options -> do
                    setOption options "v2_no_such_option" "1"
                    withView "v2_unknown" $ \name -> do
                        code <- c_duckdb_v2_instance_attach instanceHandle (coerce name) file options (CBool 0) nullPtr
                        assertBool "an unknown option fails the attach" (code /= DuckDBV2ErrorNone)
                    readOnly "v2_unknown" >>= (@?= [])
        )
