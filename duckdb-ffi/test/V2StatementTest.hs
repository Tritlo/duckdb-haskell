module V2StatementTest (tests) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.List (isInfixOf, isSuffixOf)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (checked, consume, output, readIntegers, readView, withConnection, withFactory, withHandle, withResult, withStatement, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 statements and results"
        [ testCase "statements and results report their statement types" testStatementType
        , testCase "raw error text is the error text without its category prefix" testRawText
        , testCase "query metrics are found by name" testMetric
        , testCase "execution rejects parameters that do not match the statement" testParameterRejection
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
        -- An empty text set through the API leaves an error without text, and without a raw body.
        withView "" $ \empty -> c_duckdb_v2_error_info_set_text info empty >>= (@?= DuckDBV2ErrorNone)
        readText c_duckdb_v2_error_info_get_text >>= (@?= "")
        alloca $ \out -> do
            c_duckdb_v2_error_info_get_raw_text info out >>= (@?= DuckDBV2ErrorNone)
            DuckDBV2Str ptr len <- peek out
            (ptr, len) @?= (ConstPtr nullPtr, 0)

testMetric :: IO ()
testMetric = withConnection $ \conn -> do
    config <- output (c_duckdb_v2_connection_get_config conn)
    withView "enable_profiling" $ \name -> withView "no_output" $ \setting ->
        checked (c_duckdb_v2_config_set_option_text config (coerce name) setting DuckDBV2SettingScopeDefault)
    checked (c_duckdb_v2_connection_enable_profiling conn (CBool 1))
    -- A live result is a running query, so the connection has no snapshot to give.
    withResult conn "SELECT sum(i) FROM range(1000) t(i)" $ \_ -> alloca $ \out -> do
        poke out (coerce nullPtr)
        code <- c_duckdb_v2_connection_get_query_metrics conn out nullPtr
        c_duckdb_v2_query_metrics_destroy out >>= (@?= DuckDBV2ErrorNone)
        code @?= DuckDBV2ErrorResourceInUse
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
        alloca $ \name -> c_duckdb_v2_query_metrics_node_get_metric_name root count name nullPtr >>= (@?= DuckDBV2ErrorInputOutOfRange)
        alloca $ \value -> do
            poke value (coerce nullPtr)
            code <- c_duckdb_v2_query_metrics_node_get_metric_value root count value nullPtr
            c_duckdb_v2_value_destroy value >>= (@?= DuckDBV2ErrorNone)
            code @?= DuckDBV2ErrorInputOutOfRange
        withView "v2.no_such_metric" $ \view -> alloca $ \present -> alloca $ \value -> do
            poke value (coerce nullPtr)
            checked (c_duckdb_v2_query_metrics_node_get_metric root view value present)
            peek present >>= (@?= CBool 0)
            peek value >>= (@?= coerce nullPtr)

{- | Execute a statement with the value 7 for one parameter, by name or by position, and read the result.
A failing execution gives its error code and no rows.
-}
executeWith :: DuckDBV2ConnectionHandle -> DuckDBV2FactoryHandle -> String -> Maybe String -> Int -> IO (DuckDBV2Error, [Maybe Int64])
executeWith conn factory sql name count = withStatement conn sql $ \statement ->
    withHandle (c_duckdb_v2_value_create_bigint factory 7) c_duckdb_v2_value_destroy $ \value -> withArray [value] $ \values -> alloca $ \result -> do
        poke result (coerce nullPtr)
        flip finally (c_duckdb_v2_result_destroy result) $ do
            let execute keys = c_duckdb_v2_statement_execute conn statement keys (ConstPtr values) (fromIntegral count) result nullPtr
            code <- maybe (execute (coerce nullPtr)) (\text -> withView text (execute . coerce)) name
            rows <- if code == DuckDBV2ErrorNone then peek result >>= readIntegers else pure []
            pure (code, rows)

testParameterRejection :: IO ()
testParameterRejection = withConnection $ \conn -> withFactory conn $ \factory -> do
    let execute = executeWith conn factory
    execute "SELECT $1::BIGINT" Nothing 1 >>= (@?= (DuckDBV2ErrorNone, [Just 7]))
    execute "SELECT $1::BIGINT" (Just "amount") 1 >>= (@?= (DuckDBV2ErrorInputInvalid, []))
    execute "SELECT $amount::BIGINT" (Just "amount") 1 >>= (@?= (DuckDBV2ErrorNone, [Just 7]))
    execute "SELECT $amount::BIGINT" Nothing 1 >>= (@?= (DuckDBV2ErrorInputInvalid, []))
    -- A PIVOT without an IN list expands into a group of statements. A value for a statement without parameters is a
    -- binder error, so the code of the PIVOT comes from the expansion.
    let pivot = "PIVOT (SELECT 1::BIGINT AS k, 2::BIGINT AS v) ON k USING max(v)"
    execute "SELECT 2::BIGINT" Nothing 1 >>= (@?= (DuckDBV2ErrorQueryBinder, []))
    execute pivot Nothing 1 >>= (@?= (DuckDBV2ErrorInputInvalid, []))
    withResult conn pivot $ \result -> do
        alloca $ \schema -> do
            poke schema (coerce nullPtr)
            c_duckdb_v2_result_get_schema result schema nullPtr >>= (@?= DuckDBV2ErrorInputMetadataDeferred)
            peek schema >>= (@?= coerce nullPtr)
        readIntegers result >>= (@?= [Just 2])
        withHandle (c_duckdb_v2_result_get_schema result) c_duckdb_v2_schema_destroy $ \schema -> alloca $ \name -> do
            output (c_duckdb_v2_schema_get_count schema) >>= (@?= 1)
            _ <- output (c_duckdb_v2_schema_get_field schema 0 name)
            alloca $ \ty -> c_duckdb_v2_schema_get_field schema 1 name ty nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
