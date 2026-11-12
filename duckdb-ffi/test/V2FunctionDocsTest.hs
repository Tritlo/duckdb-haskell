module V2FunctionDocsTest (tests) where

import Data.Coerce (coerce)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Ptr (nullPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import V2Test (checked, output, withConnection, withFactory, withHandle, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 function documentation"
        [ testCase "aggregate, table and macro functions lend documentation to fill in" testDocs
        ]

{- | Fill in borrowed documentation with a description, an example and a category.
Only an extension can register a macro, and a registered aggregate or table function needs callbacks.
Thus this test does not read the documentation back from the catalog.
-}
fillDocs :: DuckDBV2FunctionDocsHandle -> IO ()
fillDocs docs = do
    assertBool "the documentation is non-null" (docs /= coerce nullPtr)
    withView "Adds one." $ \description -> checked (c_duckdb_v2_function_docs_set_description docs description)
    withView "Replaces the description." $ \description -> checked (c_duckdb_v2_function_docs_set_description docs description)
    withView "basic" $ \title -> withView "SELECT v2_plus_one(1)" $ \example ->
        checked (c_duckdb_v2_function_docs_add_example docs title example)
    withView "SELECT v2_plus_one(2)" $ \example -> checked (c_duckdb_v2_function_docs_add_example docs (ConstPtr nullPtr) example)
    withView "numeric" $ \category -> checked (c_duckdb_v2_function_docs_add_category docs category)

testDocs :: IO ()
testDocs = withConnection $ \conn -> withFactory conn $ \factory -> do
    withHandle (c_duckdb_v2_aggregate_function_create factory) c_duckdb_v2_aggregate_function_destroy $ \function ->
        output (c_duckdb_v2_aggregate_function_get_docs function) >>= fillDocs
    withHandle (c_duckdb_v2_table_function_create factory) c_duckdb_v2_table_function_destroy $ \function ->
        output (c_duckdb_v2_table_function_get_docs function) >>= fillDocs
    withHandle (c_duckdb_v2_macro_function_create factory DuckDBV2MacroFunctionTypeScalar) c_duckdb_v2_macro_function_destroy $ \function ->
        output (c_duckdb_v2_macro_function_get_docs function) >>= fillDocs
