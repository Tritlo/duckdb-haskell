{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module ExtractStatementsTest (tests) where

import Control.Monad (forM_, when)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Extract Statements"
        [ extractPrepareAndExecuteSequence
        , extractFailureYieldsErrorMessage
        ]

extractPrepareAndExecuteSequence :: TestTree
extractPrepareAndExecuteSequence =
    testCase "extract, prepare, and execute a multi-statement script" $
        withDatabase \db ->
            withConnection db \conn -> do
                let script =
                        "CREATE TABLE extracted_values(val INTEGER);"
                            <> "INSERT INTO extracted_values VALUES (1), (2);"
                            <> "SELECT SUM(val) FROM extracted_values;"

                withConstCString script \scriptPtr ->
                    alloca \exPtr -> do
                        stmtCount <- c_duckdb_extract_statements conn scriptPtr exPtr
                        stmtCount @?= 3

                        extracted <- peek exPtr
                        assertBool "extracted handle should not be null" (extracted /= (coerce (nullPtr :: Ptr Void)))

                        let indices = [0 :: Integer .. fromIntegral stmtCount - 1]
                        forM_ indices \idx ->
                            alloca \stmtPtr -> do
                                let duckIdx = fromIntegral idx
                                prepState <- c_duckdb_prepare_extracted_statement conn extracted duckIdx stmtPtr
                                prepState @?= DuckDBSuccess

                                stmt <- peek stmtPtr
                                assertBool "prepared statement should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                                alloca \resPtr -> do
                                    execState <- c_duckdb_execute_prepared stmt resPtr
                                    execState @?= DuckDBSuccess

                                    case idx of
                                        0 -> do
                                            resultType <- (peek resPtr >>= \rawValue -> c_duckdb_result_return_type rawValue)
                                            resultType @?= DUCKDB_RESULT_TYPE_NOTHING
                                        1 -> do
                                            resultType <- (peek resPtr >>= \rawValue -> c_duckdb_result_return_type rawValue)
                                            resultType @?= DUCKDB_RESULT_TYPE_CHANGED_ROWS
                                        2 -> do
                                            rowCount <- c_duckdb_row_count resPtr
                                            rowCount @?= 1
                                            total <- c_duckdb_value_int64 resPtr 0 0
                                            (total :: Int64) @?= 3
                                        _ -> pure ()

                                    c_duckdb_destroy_result resPtr

                                c_duckdb_destroy_prepare stmtPtr

                        -- Preparing out-of-range should fail with an informative error
                        alloca \stmtPtr -> do
                            let invalidIdx = stmtCount
                            stInvalid <- c_duckdb_prepare_extracted_statement conn extracted invalidIdx stmtPtr
                            stInvalid @?= DuckDBError
                            stmt <- peek stmtPtr
                            assertBool "invalid index should not yield a statement handle" (stmt == (coerce (nullPtr :: Ptr Void)))

                        c_duckdb_destroy_extracted exPtr

extractFailureYieldsErrorMessage :: TestTree
extractFailureYieldsErrorMessage =
    testCase "failed extract surfaces parser error" $
        withDatabase \db ->
            withConnection db \conn -> do
                withConstCString "SELECT * FROM invalid_table WHERE" \badSql ->
                    alloca \exPtr -> do
                        stmtCount <- c_duckdb_extract_statements conn badSql exPtr
                        stmtCount @?= 0

                        extracted <- peek exPtr
                        assertBool "extracted handle should be available for errors" (extracted /= (coerce (nullPtr :: Ptr Void)))

                        errPtr <- c_duckdb_extract_statements_error extracted
                        when (errPtr == (coerce (nullPtr :: Ptr Void))) $
                            assertFailure "expected error message pointer from failed extract"
                        errMsg <- (peekCString . coerce) errPtr
                        assertBool "error message should not be empty" (not (null errMsg))

                        c_duckdb_destroy_extracted exPtr
