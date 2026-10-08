{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module PreparedStatementsTest (tests) where

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import GHC.Records (getField)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase)

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
                withConstCString "CREATE TABLE numbers(value INTEGER)" \ddl -> do
                    alloca \resPtr -> do
                        st <- duckdb_query conn ddl resPtr
                        st @?= DuckDBSuccess
                        duckdb_destroy_result resPtr

                let sql = "SELECT value FROM numbers WHERE value = ?"
                withConstCString sql \cSql ->
                    alloca \stmtPtr -> do
                        st <- duckdb_prepare conn cSql stmtPtr
                        st @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "statement pointer should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        -- Parameter metadata
                        paramCount <- duckdb_nparams stmt
                        paramCount @?= 1

                        paramType <- (getField @"unwrap" <$> duckdb_param_type stmt 0)
                        -- Physical type defaults to invalid until bound; logical type carries information.
                        paramType @?= DUCKDB_TYPE_INVALID

                        logicalTypePtr <- duckdb_param_logical_type stmt 0
                        when (logicalTypePtr /= (coerce (nullPtr :: Ptr Void))) $ do
                            alloca \typePtr -> do
                                poke typePtr logicalTypePtr
                                duckdb_destroy_logical_type typePtr

                        namePtr <- duckdb_parameter_name stmt 0
                        when (namePtr /= (coerce (nullPtr :: Ptr Void))) $ do
                            name <- (peekCString . coerce) namePtr
                            name @?= ""

                        -- Statement type
                        stmtType <- duckdb_prepared_statement_type stmt
                        stmtType @?= DUCKDB_STATEMENT_TYPE_SELECT

                        -- Column metadata
                        colCount <- duckdb_prepared_statement_column_count stmt
                        colCount @?= 1

                        colNamePtr <- duckdb_prepared_statement_column_name stmt 0
                        colName <- (peekCString . coerce) colNamePtr
                        colName @?= "value"

                        colType <- duckdb_prepared_statement_column_type stmt 0
                        colType @?= Duckdb_type DUCKDB_TYPE_INTEGER

                        colLogicalType <- duckdb_prepared_statement_column_logical_type stmt 0
                        assertBool "column logical type should not be null" (colLogicalType /= (coerce (nullPtr :: Ptr Void)))
                        alloca \colTypePtr -> do
                            poke colTypePtr colLogicalType
                            duckdb_destroy_logical_type colTypePtr

                        -- Clear bindings succeeds even before binding values
                        duckdb_clear_bindings stmt >>= (@?= DuckDBSuccess)

                        -- No preparation error
                        errPtr <- duckdb_prepare_error stmt
                        when (errPtr /= (coerce (nullPtr :: Ptr Void))) $ do
                            msg <- (peekCString . coerce) errPtr
                            msg @?= ""

                        duckdb_destroy_prepare stmtPtr

preparedStatementError :: TestTree
preparedStatementError =
    testCase "prepare error surfaces message" $
        withDatabase \db ->
            withConnection db \conn -> do
                let badSql = "SELECT * FROM non_existing_table WHERE value = ?"
                withConstCString badSql \cSql ->
                    alloca \stmtPtr -> do
                        st <- duckdb_prepare conn cSql stmtPtr
                        st @?= DuckDBError
                        stmt <- peek stmtPtr
                        if stmt == (coerce (nullPtr :: Ptr Void))
                            then assertFailure "expected prepared statement handle after failure"
                            else do
                                errMsgPtr <- duckdb_prepare_error stmt
                                when (errMsgPtr == (coerce (nullPtr :: Ptr Void))) $ assertFailure "expected error message for failed prepare"
                                msg <- (peekCString . coerce) errMsgPtr
                                assertBool "error message should mention table" (not (null msg))
                                duckdb_destroy_prepare stmtPtr
