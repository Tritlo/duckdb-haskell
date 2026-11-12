module V2FileSystemTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Coerce (coerce)
import Database.DuckDB.FFI.V2
import Foreign.C.String (peekCStringLen, withCString)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import System.IO (hClose, hPutStr, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (checked, output, removeTestFile, withHandle, withInstance, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 file system"
        [ testCase "the instance file system opens files with options that carry values" testInstanceFileSystem
        ]

testInstanceFileSystem :: IO ()
testInstanceFileSystem =
    bracket
        ( do
            (path, handle) <- openBinaryTempFile "." "duckdb-v2-instance"
            hPutStr handle contents
            hClose handle
            pure path
        )
        (\path -> withCString path $ \name -> removeTestFile name >>= (@?= 0))
        ( \path -> withInstance $ \instanceHandle -> do
            fileSystem <- output (c_duckdb_v2_instance_get_file_system instanceHandle)
            assertBool "the file system is non-null" (fileSystem /= coerce nullPtr)
            factory <- output (c_duckdb_v2_instance_get_factory instanceHandle)
            withHandle (c_duckdb_v2_file_open_options_create fileSystem) c_duckdb_v2_file_open_options_destroy $ \options -> do
                checked (c_duckdb_v2_file_open_options_set_flag options DuckDBV2FileFlagRead)
                -- The options copy each value, so the test destroys the value at once. The test sets the same name twice.
                forM_ [1, 2] $ \hint -> withView "v2_unknown_hint" $ \name ->
                    withHandle (c_duckdb_v2_value_create_bigint factory hint) c_duckdb_v2_value_destroy $ \value ->
                        checked (c_duckdb_v2_file_open_options_set_value options name value)
                -- A file system ignores a value that it does not recognise.
                withView path $ \name ->
                    withHandle (c_duckdb_v2_file_system_open fileSystem name options) c_duckdb_v2_file_destroy $ \file -> do
                        output (c_duckdb_v2_file_size file) >>= (@?= fromIntegral (length contents))
                        allocaBytes (length contents) $ \buffer -> do
                            checked (c_duckdb_v2_file_read_at file buffer (fromIntegral (length contents)) 0)
                            peekCStringLen (coerce buffer, length contents) >>= (@?= contents)
        )
  where
    contents = "read through the instance"
