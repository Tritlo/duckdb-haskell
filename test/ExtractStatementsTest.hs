{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module ExtractStatementsTest (tests) where

import Control.Monad (forM_, when)
import Data.Int (Int64)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Extract Statements"
    [ extractPrepareAndExecuteSequence
    , extractFailureYieldsErrorMessage
    ]

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase action =
  withCString ":memory:" \path ->
    alloca \dbPtr -> do
      st <- c_duckdb_open path dbPtr
      st @?= DuckDBSuccess
      db <- peek dbPtr
      result <- action db
      c_duckdb_close dbPtr
      pure result

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db action =
  alloca \connPtr -> do
    st <- c_duckdb_connect db connPtr
    st @?= DuckDBSuccess
    conn <- peek connPtr
    result <- action conn
    c_duckdb_disconnect connPtr
    pure result

extractPrepareAndExecuteSequence :: TestTree
extractPrepareAndExecuteSequence =
  testCase "extract, prepare, and execute a multi-statement script" $
    withDatabase \db ->
      withConnection db \conn -> do
        let script =
              "CREATE TABLE extracted_values(val INTEGER);"
                <> "INSERT INTO extracted_values VALUES (1), (2);"
                <> "SELECT SUM(val) FROM extracted_values;"

        withCString script \scriptPtr ->
          alloca \exPtr -> do
            stmtCount <- c_duckdb_extract_statements conn scriptPtr exPtr
            stmtCount @?= 3

            extracted <- peek exPtr
            assertBool "extracted handle should not be null" (extracted /= nullPtr)

            let indices = [0 .. fromIntegral stmtCount - 1]
            forM_ indices \idx ->
              alloca \stmtPtr -> do
                let duckIdx = fromIntegral idx
                prepState <- c_duckdb_prepare_extracted_statement conn extracted duckIdx stmtPtr
                prepState @?= DuckDBSuccess

                stmt <- peek stmtPtr
                assertBool "prepared statement should not be null" (stmt /= nullPtr)

                alloca \resPtr -> do
                  execState <- c_duckdb_execute_prepared stmt resPtr
                  execState @?= DuckDBSuccess

                  case idx of
                    0 -> do
                      resultType <- c_duckdb_result_return_type resPtr
                      resultType @?= DuckDBResultTypeNothing
                    1 -> do
                      resultType <- c_duckdb_result_return_type resPtr
                      resultType @?= DuckDBResultTypeChangedRows
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
              assertBool "invalid index should not yield a statement handle" (stmt == nullPtr)

            c_duckdb_destroy_extracted exPtr

extractFailureYieldsErrorMessage :: TestTree
extractFailureYieldsErrorMessage =
  testCase "failed extract surfaces parser error" $
    withDatabase \db ->
      withConnection db \conn -> do
        withCString "SELECT * FROM invalid_table WHERE" \badSql ->
          alloca \exPtr -> do
            stmtCount <- c_duckdb_extract_statements conn badSql exPtr
            stmtCount @?= 0

            extracted <- peek exPtr
            assertBool "extracted handle should be available for errors" (extracted /= nullPtr)

            errPtr <- c_duckdb_extract_statements_error extracted
            when (errPtr == nullPtr) $
              assertFailure "expected error message pointer from failed extract"
            errMsg <- peekCString errPtr
            assertBool "error message should not be empty" (not (null errMsg))

            c_duckdb_destroy_extracted exPtr
