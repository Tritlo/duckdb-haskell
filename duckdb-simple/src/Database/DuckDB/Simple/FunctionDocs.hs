{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Documentation attached to scalar, aggregate, table, macro, and pragma functions.
module Database.DuckDB.Simple.FunctionDocs (
    FunctionDocs (..),
    defaultFunctionDocs,
    setFunctionDocs,
) where

import Control.Monad (forM_)
import Data.Text (Text)
import Database.DuckDB.Simple.Native

-- | A description, titled SQL examples, and category names.
data FunctionDocs = FunctionDocs
    { functionDescription :: Maybe Text
    , functionExamples :: [(Text, Text)]
    , functionCategories :: [Text]
    }
    deriving (Eq, Show, Read)

-- | Leave the description unset and add no examples or categories.
defaultFunctionDocs :: FunctionDocs
defaultFunctionDocs = FunctionDocs Nothing [] []

{- | Apply documentation to a borrowed native documentation handle.
Get the handle from the function builder in its configuration action.
The action replaces the description when present and appends examples and categories.
DuckDB copies all text before the action returns.
-}
setFunctionDocs :: DuckDBV2FunctionDocsHandle -> FunctionDocs -> IO ()
setFunctionDocs docs FunctionDocs{functionDescription, functionExamples, functionCategories} = do
    forM_ functionDescription \description ->
        withText description (checked . c_duckdb_v2_function_docs_set_description docs)
    forM_ functionExamples \(title, sql) ->
        withText title \nativeTitle ->
            withText sql \nativeSQL -> checked (c_duckdb_v2_function_docs_add_example docs nativeTitle nativeSQL)
    forM_ functionCategories \category ->
        withText category (checked . c_duckdb_v2_function_docs_add_category docs)
