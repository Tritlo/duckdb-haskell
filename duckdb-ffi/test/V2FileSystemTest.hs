module V2FileSystemTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_)
import Data.Coerce (coerce)
import Database.DuckDB.FFI.V2
import Foreign.C.String (peekCStringLen, withCString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (poke)
import System.IO (hClose, hPutStr, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (checked, output, removeTestFile, withHandle, withInstance, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 file system"
        [ testCase "the instance file system opens files with options that carry values" testInstanceFileSystem
        , testCase "a file does not open without flags, when it is missing or when exclusive create finds it, and a short read fails" testFileFailures
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

-- | Check that a call fails. The header names no error code for it.
failed :: DuckDBV2Error -> IO ()
failed code = assertBool ("the call fails: " <> show code) (code /= DuckDBV2ErrorNone)

testFileFailures :: IO ()
testFileFailures =
    bracket
        ( do
            (path, handle) <- openBinaryTempFile "." "duckdb-v2-failures"
            hPutStr handle contents
            hClose handle
            pure path
        )
        (\path -> withCString path $ \name -> removeTestFile name >>= (@?= 0))
        ( \path -> withInstance $ \instanceHandle -> do
            fileSystem <- output (c_duckdb_v2_instance_get_file_system instanceHandle)
            let withOptions flags action = withHandle (c_duckdb_v2_file_open_options_create fileSystem) c_duckdb_v2_file_open_options_destroy $ \options -> do
                    forM_ flags $ \flag -> checked (c_duckdb_v2_file_open_options_set_flag options flag)
                    action options
                open name options = withView name $ \view -> alloca $ \out -> do
                    poke out (coerce nullPtr)
                    code <- c_duckdb_v2_file_system_open fileSystem view options out nullPtr
                    c_duckdb_v2_file_destroy out >>= (@?= DuckDBV2ErrorNone)
                    pure code
            withOptions [] $ \options -> do
                c_duckdb_v2_file_open_options_set_flag options DuckDBV2FileFlagInvalid nullPtr >>= failed
                c_duckdb_v2_file_open_options_set_flag options (DuckDBV2FileFlag 99) nullPtr >>= failed
                -- The options refused both values, so they still carry no flag.
                open path options >>= failed
                checked (c_duckdb_v2_file_open_options_set_flag options DuckDBV2FileFlagRead)
                open path options >>= (@?= DuckDBV2ErrorNone)
                open (path <> ".absent") options >>= failed
            withOptions [DuckDBV2FileFlagWrite, DuckDBV2FileFlagCreate, DuckDBV2FileFlagExclusiveCreate] $ \options -> do
                open path options >>= failed
                let created = path <> ".created"
                (open created options >>= (@?= DuckDBV2ErrorNone)) `finally` withCString created removeTestFile
            withOptions [DuckDBV2FileFlagRead] $ \options -> withView path $ \name ->
                withHandle (c_duckdb_v2_file_system_open fileSystem name options) c_duckdb_v2_file_destroy $ \file ->
                    allocaBytes (length contents + 1) $ \buffer -> do
                        checked (c_duckdb_v2_file_read_at file buffer (fromIntegral (length contents)) 0)
                        c_duckdb_v2_file_read_at file buffer (fromIntegral (length contents + 1)) 0 nullPtr >>= failed
        )
  where
    contents = "a short file"
