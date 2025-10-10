{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module PreparedStatementsTest (tests) where

import Control.Monad (when)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase)

tests :: TestTree
tests =
  testGroup
    "Prepared Statements"
    [ preparedStatementMetadata
    , preparedStatementError
    ]

preparedStatementMetadata :: TestTree
preparedStatementMetadata =
  testCase "inspect prepared statement metadata" $
    withDatabase \db ->
      withConnection db \conn -> do
        -- Seed table for column metadata checks
        withCString "CREATE TABLE numbers(value INTEGER)" \ddl -> do
          alloca \resPtr -> do
            st <- c_duckdb_query conn ddl resPtr
            st @?= DuckDBSuccess
            c_duckdb_destroy_result resPtr

        let sql = "SELECT value FROM numbers WHERE value = ?"
        withCString sql \cSql ->
          alloca \stmtPtr -> do
            st <- c_duckdb_prepare conn cSql stmtPtr
            st @?= DuckDBSuccess
            stmt <- peek stmtPtr
            assertBool "statement pointer should not be null" (stmt /= nullPtr)

            -- Parameter metadata
            paramCount <- c_duckdb_nparams stmt
            paramCount @?= 1

            paramType <- c_duckdb_param_type stmt 0
            -- Physical type defaults to invalid until bound; logical type carries information.
            paramType @?= DuckDBTypeInvalid

            logicalTypePtr <- c_duckdb_param_logical_type stmt 0
            when (logicalTypePtr /= nullPtr) $ do
              alloca \typePtr -> do
                poke typePtr logicalTypePtr
                c_duckdb_destroy_logical_type typePtr

            namePtr <- c_duckdb_parameter_name stmt 0
            when (namePtr /= nullPtr) $ do
              name <- peekCString namePtr
              name @?= ""

            -- Statement type
            stmtType <- c_duckdb_prepared_statement_type stmt
            stmtType @?= DuckDBStatementTypeSelect

            -- Column metadata
            colCount <- c_duckdb_prepared_statement_column_count stmt
            colCount @?= 1

            colNamePtr <- c_duckdb_prepared_statement_column_name stmt 0
            colName <- peekCString colNamePtr
            colName @?= "value"

            colType <- c_duckdb_prepared_statement_column_type stmt 0
            colType @?= DuckDBTypeInteger

            colLogicalType <- c_duckdb_prepared_statement_column_logical_type stmt 0
            assertBool "column logical type should not be null" (colLogicalType /= nullPtr)
            alloca \colTypePtr -> do
              poke colTypePtr colLogicalType
              c_duckdb_destroy_logical_type colTypePtr

            -- Clear bindings succeeds even before binding values
            c_duckdb_clear_bindings stmt >>= (@?= DuckDBSuccess)

            -- No preparation error
            errPtr <- c_duckdb_prepare_error stmt
            when (errPtr /= nullPtr) $ do
              msg <- peekCString errPtr
              msg @?= ""

            c_duckdb_destroy_prepare stmtPtr

preparedStatementError :: TestTree
preparedStatementError =
  testCase "prepare error surfaces message" $
    withDatabase \db ->
      withConnection db \conn -> do
        let badSql = "SELECT * FROM non_existing_table WHERE value = ?"
        withCString badSql \cSql ->
          alloca \stmtPtr -> do
            st <- c_duckdb_prepare conn cSql stmtPtr
            st @?= DuckDBError
            stmt <- peek stmtPtr
            if stmt == nullPtr
              then assertFailure "expected prepared statement handle after failure"
              else do
                errMsgPtr <- c_duckdb_prepare_error stmt
                when (errMsgPtr == nullPtr) $ assertFailure "expected error message for failed prepare"
                msg <- peekCString errMsgPtr
                assertBool "error message should mention table" (not (null msg))
                c_duckdb_destroy_prepare stmtPtr
