{-# LANGUAGE BlockArguments #-}

module ProfilingInfoTest (tests) where

import Control.Monad (forM)
import Data.Coerce (coerce)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (destroyDuckValue, withConnection, withConstCString, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Profiling Info"
        [ profilingDisabledByDefault
        , profilingMetricsRoundtrip
        ]

profilingDisabledByDefault :: TestTree
profilingDisabledByDefault =
    testCase "profiling info requires enabling profiling" $
        withDatabase \db ->
            withConnection db \conn -> do
                infoPtr <- duckdb_get_profiling_info conn
                infoPtr @?= (coerce (nullPtr :: Ptr Void))

profilingMetricsRoundtrip :: TestTree
profilingMetricsRoundtrip =
    testCase "collect metrics and traverse profiling tree" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement conn "PRAGMA enable_profiling='no_output'"
                runStatement conn "CREATE TABLE profiling_numbers(value INTEGER)"
                runStatement conn "INSERT INTO profiling_numbers VALUES (1), (2), (3)"
                runStatement conn "SELECT sum(value) FROM profiling_numbers"

                infoPtr <- duckdb_get_profiling_info conn
                assertBool "profiling info pointer should be non-null" (infoPtr /= (coerce (nullPtr :: Ptr Void)))

                metricsVal <- duckdb_profiling_info_get_metrics infoPtr
                entryCountIdx <- duckdb_get_map_size metricsVal
                assertBool "expected at least one metric entry" (entryCountIdx > 0)
                entries <- collectMetrics metricsVal entryCountIdx
                destroyDuckValue metricsVal

                assertBool "metrics map should contain entries" (not (null entries))
                let (firstKey, firstValue) = case entries of
                        (e : _) -> e
                        [] -> error "unreachable: entries is non-empty"

                withConstCString firstKey \keyPtr -> do
                    valueHandle <- duckdb_profiling_info_get_value infoPtr keyPtr
                    assertBool ("metric " <> firstKey <> " should be present") (valueHandle /= (coerce (nullPtr :: Ptr Void)))
                    fetchedValue <- duckValueToString valueHandle
                    destroyDuckValue valueHandle
                    fetchedValue @?= firstValue

                childCount <- duckdb_profiling_info_get_child_count infoPtr
                assertBool "expected at least one child node" (childCount > 0)
                let firstChildIdx = 0
                childPtr <- duckdb_profiling_info_get_child infoPtr firstChildIdx
                assertBool "child pointer should be non-null" (childPtr /= (coerce (nullPtr :: Ptr Void)))

                childMetrics <- duckdb_profiling_info_get_metrics childPtr
                childCountIdx <- duckdb_get_map_size childMetrics
                assertBool "child node should expose metrics" (childCountIdx > 0)
                destroyDuckValue childMetrics

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

collectMetrics :: Duckdb_value -> Idx_t -> IO [(String, String)]
collectMetrics metricsVal entryCountIdx = do
    let entryCount = fromIntegral entryCountIdx :: Int
    forM [0 .. entryCount - 1] \i -> do
        let idx = fromIntegral i :: Idx_t
        keyHandle <- duckdb_get_map_key metricsVal idx
        keyName <- duckValueToText keyHandle
        destroyDuckValue keyHandle

        valHandle <- duckdb_get_map_value metricsVal idx
        valText <- duckValueToString valHandle
        destroyDuckValue valHandle

        pure (keyName, valText)

duckValueToText :: Duckdb_value -> IO String
duckValueToText valHandle = do
    strPtr <- duckdb_get_varchar valHandle
    text <- (peekCString . coerce) strPtr
    duckdb_free (coerce strPtr)
    pure text

duckValueToString :: Duckdb_value -> IO String
duckValueToString valHandle = do
    strPtr <- duckdb_value_to_string valHandle
    text <- (peekCString . coerce) strPtr
    duckdb_free (coerce strPtr)
    pure text
