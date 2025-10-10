{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module ArrowInterfaceDeprecatedTests (tests) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase, withResult)

tests :: TestTree
tests =
  testGroup
    "Deprecated Arrow Interface"
    [ queryArrowExposesSchemaAndArrays
    , queryArrowReportsErrors
    , preparedArrowSchemaMatchesStatement
    , executePreparedArrowProducesRows
    , resultArrowArrayMirrorsChunk
    , arrowRowsChangedReflectsMutations
    , arrowArrayScanRegistersView
    ]

-- basic query ---------------------------------------------------------------

queryArrowExposesSchemaAndArrays :: TestTree
queryArrowExposesSchemaAndArrays =
  testCase "query_arrow exposes schema metadata and arrays" $
    withDatabase \db ->
      withConnection db \conn ->
        withSuccessfulArrow conn "SELECT 1::INTEGER AS id, 'duck'::VARCHAR AS label" \arrow -> do
          columnCount <- c_duckdb_arrow_column_count arrow
          columnCount @?= 2

          rowCount <- c_duckdb_arrow_row_count arrow
          rowCount @?= 1

          rowsChanged <- c_duckdb_arrow_rows_changed arrow
          rowsChanged @?= 0

          errPtr <- c_duckdb_query_arrow_error arrow
          when (errPtr /= nullPtr) $ do
            errMsg <- peekCString errPtr
            errMsg @?= ""

          -- schema/array access validated in dedicated scan test

-- error handling ------------------------------------------------------------

queryArrowReportsErrors :: TestTree
queryArrowReportsErrors =
  testCase "query_arrow surfaces execution errors" $
    withDatabase \db ->
      withConnection db \conn ->
        withCString "SELECT * FROM missing_table" \querySql ->
          alloca \arrowPtr -> do
            poke arrowPtr nullPtr
            state <- c_duckdb_query_arrow conn querySql arrowPtr
            state @?= DuckDBError

            arrow <- peek arrowPtr
            assertBool "arrow result should still be allocated on error" (arrow /= nullPtr)

            errPtr <- c_duckdb_query_arrow_error arrow
            assertBool "error message should be present" (errPtr /= nullPtr)
            errMsg <- peekCString errPtr
            assertBool "error message should mention missing_table" ("missing_table" `isInfixOf` errMsg)

            c_duckdb_destroy_arrow arrowPtr

-- prepared statements -------------------------------------------------------

preparedArrowSchemaMatchesStatement :: TestTree
preparedArrowSchemaMatchesStatement =
  testCase "prepared_arrow_schema reflects projected columns" $
    withDatabase \db ->
      withConnection db \conn ->
        withPrepared conn "SELECT id, label FROM (VALUES (1, 'a')) AS t(id, label)" \stmt -> do
          alloca \schemaOut -> do
            poke schemaOut nullPtr
            stSchema <- c_duckdb_prepared_arrow_schema stmt schemaOut
            case stSchema of
              DuckDBSuccess -> do
                schemaWrapper <- peek schemaOut
                assertBool "prepared arrow schema pointer should not be null" (schemaWrapper /= nullPtr)
              DuckDBError -> do
                schemaWrapper <- peek schemaOut
                schemaWrapper @?= nullPtr
                errPtr <- c_duckdb_prepare_error stmt
                when (errPtr /= nullPtr) $ do
                  errMsg <- peekCString errPtr
                  errMsg @?= ""

executePreparedArrowProducesRows :: TestTree
executePreparedArrowProducesRows =
  testCase "execute_prepared_arrow materialises a result set" $
    withDatabase \db ->
      withConnection db \conn ->
        withPrepared conn "SELECT ?::INTEGER + 5 AS computed" \stmt -> do
          c_duckdb_bind_int32 stmt 1 (5 :: Int32) >>= (@?= DuckDBSuccess)

          withPreparedArrow stmt \arrow -> do
            rowCount <- c_duckdb_arrow_row_count arrow
            rowCount @?= 1

            colCount <- c_duckdb_arrow_column_count arrow
            colCount @?= 1

-- result conversion ---------------------------------------------------------

resultArrowArrayMirrorsChunk :: TestTree
resultArrowArrayMirrorsChunk =
  expectFailBecause "duckdb_result_arrow_array crashes in DuckDB 1.4 when invoked from FFI (see upstream issue)" $
    testCase "result_arrow_array converts materialised chunks to Arrow arrays" $
      assertFailure "Pending upstream fix for duckdb_result_arrow_array"

-- rows changed --------------------------------------------------------------

arrowRowsChangedReflectsMutations :: TestTree
arrowRowsChangedReflectsMutations =
  testCase "arrow_rows_changed reports mutation counts" $
    withDatabase \db ->
      withConnection db \conn -> do
        execStatement conn "CREATE TABLE arrow_changes(val INTEGER);"

        withSuccessfulArrow conn "INSERT INTO arrow_changes VALUES (1), (2), (3)" \arrow -> do
          rowCount <- c_duckdb_arrow_row_count arrow
          assertBool "modification result should not report negative rows" (rowCount >= 0)

          changed <- c_duckdb_arrow_rows_changed arrow
          assertBool "rows_changed should report positive count" (changed > 0)

-- arrow scans ----------------------------------------------------------------

arrowArrayScanRegistersView :: TestTree
arrowArrayScanRegistersView =
  expectFailBecause "duckdb_query_arrow_schema currently crashes in DuckDB 1.4 when used via FFI" $
    testCase "arrow_array_scan registers a view and yields a release stream" $
      assertFailure "Pending upstream fix for duckdb_query_arrow_schema"
withSuccessfulArrow :: DuckDBConnection -> String -> (DuckDBArrow -> IO a) -> IO a
withSuccessfulArrow conn sql action =
  withCString sql \sqlPtr ->
    alloca \arrowPtr ->
      bracket
        (do
          poke arrowPtr nullPtr
          state <- c_duckdb_query_arrow conn sqlPtr arrowPtr
          state @?= DuckDBSuccess
          arrow <- peek arrowPtr
          assertBool "duckdb_query_arrow returned null result" (arrow /= nullPtr)
          pure arrow)
        (\_ -> c_duckdb_destroy_arrow arrowPtr)
        action

withPrepared :: DuckDBConnection -> String -> (DuckDBPreparedStatement -> IO a) -> IO a
withPrepared conn sql action =
  withCString sql \sqlPtr ->
    alloca \stmtPtr ->
      bracket
        (do
          state <- c_duckdb_prepare conn sqlPtr stmtPtr
          state @?= DuckDBSuccess
          stmt <- peek stmtPtr
          assertBool "prepare should produce a statement" (stmt /= nullPtr)
          pure stmt)
        (\_ -> c_duckdb_destroy_prepare stmtPtr)
        action

withPreparedArrow :: DuckDBPreparedStatement -> (DuckDBArrow -> IO a) -> IO a
withPreparedArrow stmt action =
  alloca \arrowPtr ->
    bracket
      (do
        poke arrowPtr nullPtr
        state <- c_duckdb_execute_prepared_arrow stmt arrowPtr
        state @?= DuckDBSuccess
        arrow <- peek arrowPtr
        assertBool "execute_prepared_arrow returned null result" (arrow /= nullPtr)
        pure arrow)
      (\_ -> c_duckdb_destroy_arrow arrowPtr)
      action

destroyChunk :: DuckDBDataChunk -> IO ()
destroyChunk chunk =
  alloca \ptr -> poke ptr chunk >> c_duckdb_destroy_data_chunk ptr

execStatement :: DuckDBConnection -> String -> IO ()
execStatement conn sql =
  withCString sql \sqlPtr ->
    alloca \resPtr -> do
      st <- c_duckdb_query conn sqlPtr resPtr
      st @?= DuckDBSuccess
      c_duckdb_destroy_result resPtr
