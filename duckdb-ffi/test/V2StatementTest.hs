module V2StatementTest (tests) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.List (isInfixOf, isSuffixOf)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (checked, consume, output, readView, withConnection, withHandle, withResult, withStatement, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 statements and results"
        [ testCase "statements and results report their statement types" testStatementType
        , testCase "raw error text is the error text without its category prefix" testRawText
        , testCase "query metrics are found by name" testMetric
        ]

testStatementType :: IO ()
testStatementType = withConnection $ \conn -> do
    let types sql = withStatement conn sql $ \statement -> do
            parsed <- output (c_duckdb_v2_sql_statement_get_type statement)
            executed <-
                withHandle (c_duckdb_v2_statement_execute conn statement (coerce nullPtr) (coerce nullPtr) 0) c_duckdb_v2_result_destroy $ \result -> do
                    statementType <- output (c_duckdb_v2_result_get_statement_type result)
                    consume result
                    pure statementType
            pure (parsed, executed)
    types "CREATE TABLE v2_items (id INTEGER)" >>= (@?= (DuckDBV2StatementTypeCreate, DuckDBV2StatementTypeCreate))
    types "INSERT INTO v2_items VALUES (1)" >>= (@?= (DuckDBV2StatementTypeInsert, DuckDBV2StatementTypeInsert))
    types "SELECT id FROM v2_items" >>= (@?= (DuckDBV2StatementTypeSelect, DuckDBV2StatementTypeSelect))
    -- The parser reports PRAGMA. The result reports the statement that execution rewrites the PRAGMA into.
    (parsed, executed) <- types "PRAGMA table_info('v2_items')"
    parsed @?= DuckDBV2StatementTypePragma
    assertBool ("the PRAGMA executes as a SELECT or a CALL, not as " <> show executed) (executed `elem` [DuckDBV2StatementTypeSelect, DuckDBV2StatementTypeCall])

testRawText :: IO ()
testRawText = withConnection $ \conn -> withStatement conn "SELECT absent_column" $ \statement -> alloca $ \result -> alloca $ \err -> do
    poke result (coerce nullPtr)
    poke err (coerce nullPtr)
    code <- c_duckdb_v2_statement_execute conn statement (coerce nullPtr) (coerce nullPtr) 0 result err
    flip finally (c_duckdb_v2_error_info_destroy err >>= (@?= DuckDBV2ErrorNone)) $ do
        code @?= DuckDBV2ErrorQueryBinder
        info <- peek err
        assertBool "failure has an owned error handle" (info /= coerce nullPtr)
        let readText getter = alloca $ \out -> do
                getter info out >>= (@?= DuckDBV2ErrorNone)
                peek out >>= readView
        text <- readText c_duckdb_v2_error_info_get_text
        raw <- readText c_duckdb_v2_error_info_get_raw_text
        assertBool "the raw text keeps the diagnostic" ("absent_column" `isInfixOf` raw)
        assertBool ("the text ends with the raw text: " <> text) (raw `isSuffixOf` text)
        let prefix = take (length text - length raw) text
        assertBool ("the prefix has the form <Type> Error: " <> prefix) (length prefix > length " Error: " && " Error: " `isSuffixOf` prefix)

testMetric :: IO ()
testMetric = withConnection $ \conn -> do
    config <- output (c_duckdb_v2_connection_get_config conn)
    withView "enable_profiling" $ \name -> withView "no_output" $ \setting ->
        checked (c_duckdb_v2_config_set_option_text config (coerce name) setting DuckDBV2SettingScopeDefault)
    checked (c_duckdb_v2_connection_enable_profiling conn (CBool 1))
    withResult conn "SELECT sum(i) FROM range(1000) t(i)" consume
    withHandle (c_duckdb_v2_connection_get_query_metrics conn) c_duckdb_v2_query_metrics_destroy $ \metrics -> do
        root <- output (c_duckdb_v2_query_metrics_get_root metrics)
        count <- output (c_duckdb_v2_query_metrics_node_get_metric_count root)
        assertBool "profiling gives the root node metrics" (count > 0)
        let asDouble value = output (c_duckdb_v2_value_get_double value)
        forM_ [0 .. count - 1] $ \index -> do
            name <- output (c_duckdb_v2_query_metrics_node_get_metric_name root index) >>= readView
            byIndex <- withHandle (c_duckdb_v2_query_metrics_node_get_metric_value root index) c_duckdb_v2_value_destroy asDouble
            withView name $ \view -> alloca $ \present ->
                withHandle (\value -> c_duckdb_v2_query_metrics_node_get_metric root view value present) c_duckdb_v2_value_destroy $ \value -> do
                    peek present >>= (@?= CBool 1)
                    asDouble value >>= (@?= byIndex)
        withView "v2.no_such_metric" $ \view -> alloca $ \present -> alloca $ \value -> do
            poke value (coerce nullPtr)
            checked (c_duckdb_v2_query_metrics_node_get_metric root view value present)
            peek present >>= (@?= CBool 0)
            peek value >>= (@?= coerce nullPtr)
