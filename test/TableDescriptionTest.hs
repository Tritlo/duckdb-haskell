{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module TableDescriptionTest (tests) where

import Control.Exception (finally)
import Control.Monad (when)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase)

tests :: TestTree
tests =
  testGroup
    "Table Description"
    [ tableDescriptionLifecycle
    , tableDescriptionExtended
    , tableDescriptionErrorHandling
    ]

tableDescriptionLifecycle :: TestTree
tableDescriptionLifecycle =
  testCase "inspect column names and defaults for main schema tables" $
    withDatabase \db ->
      withConnection db \conn -> do
        runStatement
          conn
          "CREATE TABLE description_demo(id INTEGER, name VARCHAR DEFAULT 'guest', active BOOLEAN DEFAULT TRUE)"

        withTableDescription conn Nothing "description_demo" \desc -> do
          checkColumn desc 0 "id" False
          checkColumn desc 1 "name" True
          checkColumn desc 2 "active" True
          c_duckdb_table_description_error desc >>= (@?= nullPtr)

tableDescriptionExtended :: TestTree
tableDescriptionExtended =
  testCase "describe table in custom schema via extended constructor" $
    withDatabase \db ->
      withConnection db \conn -> do
        runStatement conn "CREATE SCHEMA custom_schema"
        runStatement conn "CREATE TABLE custom_schema.ext_demo(id INTEGER DEFAULT 7, note VARCHAR)"

        withTableDescriptionExt conn Nothing (Just "custom_schema") "ext_demo" \desc -> do
          checkColumn desc 0 "id" True
          checkColumn desc 1 "note" False
          c_duckdb_table_description_error desc >>= (@?= nullPtr)

tableDescriptionErrorHandling :: TestTree
tableDescriptionErrorHandling =
  testCase "surface error information when describing missing tables" $
    withDatabase \db ->
      withConnection db \conn ->
        withCString "missing_table" \tablePtr ->
          alloca \descPtr -> do
            state <- c_duckdb_table_description_create conn nullPtr tablePtr descPtr
            state @?= DuckDBError
            desc <- peek descPtr
            let cleanup = c_duckdb_table_description_destroy descPtr
            let action =
                  if desc == nullPtr
                    then assertFailure "table description handle should be populated on error"
                    else do
                      errPtr <- c_duckdb_table_description_error desc
                      assertBool "error pointer should not be null" (errPtr /= nullPtr)
                      errMsg <- peekCString errPtr
                      assertBool "error message should not be empty" (not (null errMsg))
            action `finally` cleanup

-- Helpers -------------------------------------------------------------------

runStatement :: DuckDBConnection -> String -> IO ()
runStatement conn sql =
  withCString sql \sqlPtr ->
    alloca \resPtr -> do
      state <- c_duckdb_query conn sqlPtr resPtr
      if state == DuckDBSuccess
        then c_duckdb_destroy_result resPtr
        else do
          errPtr <- c_duckdb_result_error resPtr
          errMsg <-
            if errPtr == nullPtr
              then pure "unknown error"
              else peekCString errPtr
          c_duckdb_destroy_result resPtr
          assertFailure ("duckdb_query failed: " <> errMsg)

withTableDescription :: DuckDBConnection -> Maybe String -> String -> (DuckDBTableDescription -> IO a) -> IO a
withTableDescription conn schema table action =
  withMaybeCString schema \schemaPtr ->
    withCString table \tablePtr ->
      withDescriptionHandle (c_duckdb_table_description_create conn schemaPtr tablePtr) action

withTableDescriptionExt :: DuckDBConnection -> Maybe String -> Maybe String -> String -> (DuckDBTableDescription -> IO a) -> IO a
withTableDescriptionExt conn catalog schema table action =
  withMaybeCString catalog \catalogPtr ->
    withMaybeCString schema \schemaPtr ->
      withCString table \tablePtr ->
        withDescriptionHandle (c_duckdb_table_description_create_ext conn catalogPtr schemaPtr tablePtr) action

withDescriptionHandle :: (Ptr DuckDBTableDescription -> IO DuckDBState) -> (DuckDBTableDescription -> IO a) -> IO a
withDescriptionHandle acquire action =
  alloca \descPtr -> do
    state <- acquire descPtr
    state @?= DuckDBSuccess
    desc <- peek descPtr
    when (desc == nullPtr) $
      assertFailure "table description handle should not be null"
    let cleanup = c_duckdb_table_description_destroy descPtr
    action desc `finally` cleanup

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString Nothing action = action nullPtr
withMaybeCString (Just txt) action = withCString txt action

checkColumn :: DuckDBTableDescription -> DuckDBIdx -> String -> Bool -> IO ()
checkColumn desc idx expectedName expectedDefault = do
  hasDef <- columnHasDefault desc idx
  hasDef @?= expectedDefault
  name <- getColumnName desc idx
  name @?= expectedName

getColumnName :: DuckDBTableDescription -> DuckDBIdx -> IO String
getColumnName desc idx = do
  namePtr <- c_duckdb_table_description_get_column_name desc idx
  assertBool "column name pointer should not be null" (namePtr /= nullPtr)
  name <- peekCString namePtr
  c_duckdb_free (castPtr namePtr)
  pure name

columnHasDefault :: DuckDBTableDescription -> DuckDBIdx -> IO Bool
columnHasDefault desc idx =
  alloca \outPtr -> do
    state <- c_duckdb_column_has_default desc idx outPtr
    state @?= DuckDBSuccess
    CBool val <- peek outPtr
    pure (val /= 0)
