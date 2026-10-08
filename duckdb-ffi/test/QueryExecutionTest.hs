{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module QueryExecutionTest (tests) where

import Control.Monad (forM_, when)
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
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Query Execution"
        [ queryLifecycle
        ]

queryLifecycle :: TestTree
queryLifecycle =
    testCase "run a query and inspect metadata" $
        withDatabase \db ->
            withConnection db \conn -> do
                -- Seed table
                let createSQL = "CREATE TABLE items(id INTEGER, name VARCHAR)"
                    insertSQL = "INSERT INTO items VALUES (1, 'apple'), (2, 'banana')"
                forM_ [createSQL, insertSQL] \sql ->
                    withConstCString sql \cSql ->
                        alloca \resPtr -> do
                            st <- c_duckdb_query conn cSql resPtr
                            st @?= DuckDBSuccess
                            c_duckdb_destroy_result resPtr

                -- Query data
                withConstCString "SELECT id, name FROM items ORDER BY id" \selectSql ->
                    alloca \resPtr -> do
                        st <- c_duckdb_query conn selectSql resPtr
                        st @?= DuckDBSuccess

                        -- Column metadata
                        columnCount <- c_duckdb_column_count resPtr
                        columnCount @?= 2

                        rowCount <- c_duckdb_row_count resPtr
                        rowCount @?= 2

                        rowsChanged <- c_duckdb_rows_changed resPtr
                        rowsChanged @?= 0

                        stmtType <- (peek resPtr >>= \rawValue -> c_duckdb_result_statement_type rawValue)
                        stmtType @?= DUCKDB_STATEMENT_TYPE_SELECT

                        arrowOpts <- c_duckdb_result_get_arrow_options resPtr
                        assertBool "arrow options should not be null" (arrowOpts /= (coerce (nullPtr :: Ptr Void)))
                        alloca \arrowPtr -> do
                            poke arrowPtr arrowOpts
                            c_duckdb_destroy_arrow_options arrowPtr

                        forM_ [0 .. columnCount - 1] \idx -> do
                            namePtr <- c_duckdb_column_name resPtr idx
                            colName <- (peekCString . coerce) namePtr
                            let expected = if idx == 0 then "id" else "name"
                            colName @?= expected

                            colType <- (getField @"unwrap" <$> c_duckdb_column_type resPtr idx)
                            let expectedType = if idx == 0 then DUCKDB_TYPE_INTEGER else DUCKDB_TYPE_VARCHAR
                            colType @?= expectedType

                            logicalType <- c_duckdb_column_logical_type resPtr idx
                            assertBool "logical type pointer should not be null" (logicalType /= (coerce (nullPtr :: Ptr Void)))
                            alloca \typePtr -> do
                                poke typePtr logicalType
                                c_duckdb_destroy_logical_type typePtr

                            dataPtr <- c_duckdb_column_data resPtr idx
                            assertBool "column data pointer should not be null" (dataPtr /= (coerce (nullPtr :: Ptr Void)))

                            nullmaskPtr <- c_duckdb_nullmask_data resPtr idx
                            assertBool "nullmask pointer should not be null" (nullmaskPtr /= (coerce (nullPtr :: Ptr Void)))

                        errPtr <- c_duckdb_result_error resPtr
                        when (errPtr /= (coerce (nullPtr :: Ptr Void))) $ do
                            errMsg <- (peekCString . coerce) errPtr
                            assertBool ("unexpected error: " <> errMsg) False

                        errType <- c_duckdb_result_error_type resPtr
                        errType @?= DUCKDB_ERROR_INVALID

                        c_duckdb_destroy_result resPtr
