{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module ProfilingInfoTest (tests) where

import Control.Monad (forM)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase)

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
        infoPtr <- c_duckdb_get_profiling_info conn
        infoPtr @?= nullPtr

profilingMetricsRoundtrip :: TestTree
profilingMetricsRoundtrip =
  testCase "collect metrics and traverse profiling tree" $
    withDatabase \db ->
      withConnection db \conn -> do
        runStatement conn "PRAGMA enable_profiling='no_output'"
        runStatement conn "CREATE TABLE profiling_numbers(value INTEGER)"
        runStatement conn "INSERT INTO profiling_numbers VALUES (1), (2), (3)"
        runStatement conn "SELECT sum(value) FROM profiling_numbers"

        infoPtr <- c_duckdb_get_profiling_info conn
        assertBool "profiling info pointer should be non-null" (infoPtr /= nullPtr)

        metricsVal <- c_duckdb_profiling_info_get_metrics infoPtr
        entryCountIdx <- c_duckdb_get_map_size metricsVal
        assertBool "expected at least one metric entry" (entryCountIdx > 0)
        entries <- collectMetrics metricsVal entryCountIdx
        destroyDuckValue metricsVal

        assertBool "metrics map should contain entries" (not (null entries))
        let (firstKey, firstValue) = head entries

        withCString firstKey \keyPtr -> do
          valueHandle <- c_duckdb_profiling_info_get_value infoPtr keyPtr
          assertBool ("metric " <> firstKey <> " should be present") (valueHandle /= nullPtr)
          fetchedValue <- duckValueToString valueHandle
          destroyDuckValue valueHandle
          fetchedValue @?= firstValue

        childCount <- c_duckdb_profiling_info_get_child_count infoPtr
        assertBool "expected at least one child node" (childCount > 0)
        let firstChildIdx = 0
        childPtr <- c_duckdb_profiling_info_get_child infoPtr firstChildIdx
        assertBool "child pointer should be non-null" (childPtr /= nullPtr)

        childMetrics <- c_duckdb_profiling_info_get_metrics childPtr
        childCountIdx <- c_duckdb_get_map_size childMetrics
        assertBool "child node should expose metrics" (childCountIdx > 0)
        destroyDuckValue childMetrics

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

collectMetrics :: DuckDBValue -> DuckDBIdx -> IO [(String, String)]
collectMetrics metricsVal entryCountIdx = do
  let entryCount = fromIntegral entryCountIdx :: Int
  forM [0 .. entryCount - 1] \i -> do
    let idx = fromIntegral i :: DuckDBIdx
    keyHandle <- c_duckdb_get_map_key metricsVal idx
    keyName <- duckValueToText keyHandle
    destroyDuckValue keyHandle

    valHandle <- c_duckdb_get_map_value metricsVal idx
    valText <- duckValueToString valHandle
    destroyDuckValue valHandle

    pure (keyName, valText)

duckValueToText :: DuckDBValue -> IO String
duckValueToText valHandle = do
  strPtr <- c_duckdb_get_varchar valHandle
  text <- peekCString strPtr
  c_duckdb_free (castPtr strPtr)
  pure text

duckValueToString :: DuckDBValue -> IO String
duckValueToString valHandle = do
  strPtr <- c_duckdb_value_to_string valHandle
  text <- peekCString strPtr
  c_duckdb_free (castPtr strPtr)
  pure text

destroyDuckValue :: DuckDBValue -> IO ()
destroyDuckValue val =
  alloca \ptr -> poke ptr val >> c_duckdb_destroy_value ptr
