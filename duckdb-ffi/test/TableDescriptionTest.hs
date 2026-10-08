{-# LANGUAGE BlockArguments #-}

module TableDescriptionTest (tests) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..), CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase)

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
                    duckdb_table_description_error desc >>= (@?= (coerce (nullPtr :: Ptr Void)))

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
                    duckdb_table_description_error desc >>= (@?= (coerce (nullPtr :: Ptr Void)))

tableDescriptionErrorHandling :: TestTree
tableDescriptionErrorHandling =
    testCase "surface error information when describing missing tables" $
        withDatabase \db ->
            withConnection db \conn ->
                withConstCString "missing_table" \tablePtr ->
                    alloca \descPtr -> do
                        state <- duckdb_table_description_create conn (coerce (nullPtr :: Ptr Void)) tablePtr descPtr
                        state @?= DuckDBError
                        desc <- peek descPtr
                        let cleanup = duckdb_table_description_destroy descPtr
                        let action =
                                if desc == (coerce (nullPtr :: Ptr Void))
                                    then assertFailure "table description handle should be populated on error"
                                    else do
                                        errPtr <- duckdb_table_description_error desc
                                        assertBool "error pointer should not be null" (errPtr /= (coerce (nullPtr :: Ptr Void)))
                                        errMsg <- (peekCString . coerce) errPtr
                                        assertBool "error message should not be empty" (not (null errMsg))
                        action `finally` cleanup

-- Helpers -------------------------------------------------------------------

runStatement :: Duckdb_connection -> String -> IO ()
runStatement conn sql =
    withConstCString sql \sqlPtr ->
        alloca \resPtr -> do
            state <- duckdb_query conn sqlPtr resPtr
            if state == DuckDBSuccess
                then duckdb_destroy_result resPtr
                else do
                    errPtr <- duckdb_result_error resPtr
                    errMsg <-
                        if errPtr == (coerce (nullPtr :: Ptr Void))
                            then pure "unknown error"
                            else (peekCString . coerce) errPtr
                    duckdb_destroy_result resPtr
                    assertFailure ("duckdb_query failed: " <> errMsg)

withTableDescription :: Duckdb_connection -> Maybe String -> String -> (Duckdb_table_description -> IO a) -> IO a
withTableDescription conn schema table action =
    withMaybeCString schema \schemaPtr ->
        withConstCString table \tablePtr ->
            withDescriptionHandle (duckdb_table_description_create conn schemaPtr tablePtr) action

withTableDescriptionExt :: Duckdb_connection -> Maybe String -> Maybe String -> String -> (Duckdb_table_description -> IO a) -> IO a
withTableDescriptionExt conn catalog schema table action =
    withMaybeCString catalog \catalogPtr ->
        withMaybeCString schema \schemaPtr ->
            withConstCString table \tablePtr ->
                withDescriptionHandle (duckdb_table_description_create_ext conn catalogPtr schemaPtr tablePtr) action

withDescriptionHandle :: (Ptr Duckdb_table_description -> IO Duckdb_state) -> (Duckdb_table_description -> IO a) -> IO a
withDescriptionHandle acquire action =
    alloca \descPtr -> do
        state <- acquire descPtr
        state @?= DuckDBSuccess
        desc <- peek descPtr
        when (desc == (coerce (nullPtr :: Ptr Void))) $
            assertFailure "table description handle should not be null"
        let cleanup = duckdb_table_description_destroy descPtr
        action desc `finally` cleanup

withMaybeCString :: Maybe String -> ((ConstPtr CChar) -> IO a) -> IO a
withMaybeCString Nothing action = action (coerce (nullPtr :: Ptr Void))
withMaybeCString (Just txt) action = withConstCString txt action

checkColumn :: Duckdb_table_description -> Idx_t -> String -> Bool -> IO ()
checkColumn desc idx expectedName expectedDefault = do
    hasDef <- columnHasDefault desc idx
    hasDef @?= expectedDefault
    name <- getColumnName desc idx
    name @?= expectedName

getColumnName :: Duckdb_table_description -> Idx_t -> IO String
getColumnName desc idx = do
    namePtr <- duckdb_table_description_get_column_name desc idx
    assertBool "column name pointer should not be null" (namePtr /= (coerce (nullPtr :: Ptr Void)))
    name <- (peekCString . coerce) namePtr
    duckdb_free (coerce namePtr)
    pure name

columnHasDefault :: Duckdb_table_description -> Idx_t -> IO Bool
columnHasDefault desc idx =
    alloca \outPtr -> do
        state <- duckdb_column_has_default desc idx outPtr
        state @?= DuckDBSuccess
        CBool val <- peek outPtr
        pure (val /= 0)
