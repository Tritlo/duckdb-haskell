module V2FunctionDocsTest (tests) where

import Database.DuckDB.FFI.V2
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import V2Test (fillDocs, output, withConnection, withFactory, withHandle)

tests :: TestTree
tests =
  testGroup
    "DuckDB V2 function documentation"
    [ testCase "aggregate, table and macro functions lend documentation to fill in" testDocs
    ]

-- | Only an extension can register a macro, and a registered table function needs callbacks.
-- Thus this test does not read the documentation back from the catalog.
-- V2AggregateTest reads the documentation of a registered aggregate function back from the catalog.
testDocs :: IO ()
testDocs = withConnection $ \conn -> withFactory conn $ \factory -> do
  withHandle (c_duckdb_v2_aggregate_function_create factory) c_duckdb_v2_aggregate_function_destroy $ \function ->
    output (c_duckdb_v2_aggregate_function_get_docs function) >>= fillDocs
  withHandle (c_duckdb_v2_table_function_create factory) c_duckdb_v2_table_function_destroy $ \function ->
    output (c_duckdb_v2_table_function_get_docs function) >>= fillDocs
  withHandle (c_duckdb_v2_macro_function_create factory DuckDBV2MacroFunctionTypeScalar) c_duckdb_v2_macro_function_destroy $ \function ->
    output (c_duckdb_v2_macro_function_get_docs function) >>= fillDocs
