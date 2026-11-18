{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : Database.DuckDB.Simple.V2.FunctionDocs
-- Description : Documentation attached to scalar, aggregate, table, macro, and pragma functions.
--
-- The examples use the imports of the module header of "Database.DuckDB.Simple.V2.Registration".
module Database.DuckDB.Simple.V2.FunctionDocs (
  FunctionDocs (..),
  defaultFunctionDocs,
  setFunctionDocs,
) where

import Control.Monad (forM_)
import Data.Text (Text)
import Database.DuckDB.Simple.Native

-- | A description, titled SQL examples, and category names.
-- Each example is a pair of a title and the SQL text:
--
-- > Docs.FunctionDocs (Just "Preserve the input type.") [("Example", "SELECT managed_resolved(42)")] ["test"]
data FunctionDocs = FunctionDocs
  { functionDescription :: Maybe Text
  , functionExamples :: [(Text, Text)]
  , functionCategories :: [Text]
  }
  deriving (Eq, Show, Read)

-- | Leave the description unset and add no examples or categories.
defaultFunctionDocs :: FunctionDocs
defaultFunctionDocs = FunctionDocs Nothing [] []

-- | Apply documentation to a borrowed native documentation handle.
-- Get the handle from the function builder in its configuration action.
-- The action replaces the description when present and appends examples and categories.
-- DuckDB copies all text before the action returns.
-- For example, in the configuration action of
-- 'Database.DuckDB.Simple.V2.Registration.registerScalarFunctionWith':
--
-- > docs <- R.output (c_duckdb_v2_scalar_function_get_docs function)
-- > Docs.setFunctionDocs docs Docs.defaultFunctionDocs{Docs.functionDescription = Just "Add two BIGINT values."}
setFunctionDocs :: DuckDBV2FunctionDocsHandle -> FunctionDocs -> IO ()
setFunctionDocs docs FunctionDocs{functionDescription, functionExamples, functionCategories} = do
  forM_ functionDescription \description ->
    withText description (checked . c_duckdb_v2_function_docs_set_description docs)
  forM_ functionExamples \(title, sql) ->
    withText title \nativeTitle ->
      withText sql \nativeSQL -> checked (c_duckdb_v2_function_docs_add_example docs nativeTitle nativeSQL)
  forM_ functionCategories \category ->
    withText category (checked . c_duckdb_v2_function_docs_add_category docs)
