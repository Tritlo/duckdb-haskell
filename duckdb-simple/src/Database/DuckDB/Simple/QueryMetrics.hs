{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Copied query profiles. Returned values do not retain native snapshot handles.
module Database.DuckDB.Simple.QueryMetrics (
    QueryMetrics (..),
    enableProfiling,
    queryMetrics,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM)
import Data.Text (Text)
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.Internal (Connection, withConnectionHandle)
import Database.DuckDB.Simple.Materialize (materializeValue)
import Database.DuckDB.Simple.Native

{- | One query or operator node. Metric order is unspecified. Any metric or
operator can be absent. Native V2 reports numeric metrics only.
Values and child nodes use Haskell-owned storage.
-}
data QueryMetrics = QueryMetrics
    { metricsNodeName :: Text
    , metricsValues :: [(Text, FieldValue)]
    , metricsChildren :: [QueryMetrics]
    }
    deriving (Eq, Show)

{- | Enable or disable profiling for later queries on this connection.
DuckDB also prints the configured profile by default. Set the native
@enable_profiling@ option to @no_output@ to collect metrics without printing.
-}
enableProfiling :: Connection -> Bool -> IO ()
enableProfiling connection enabled = withConnectionHandle connection \native ->
    checked (c_duckdb_v2_connection_enable_profiling native (if enabled then 1 else 0))

{- | Copy the profile of the last completed query. Drain or close any active
result first. The returned snapshot does not change when later queries run.
Without profiling, the root has no metrics and no children. The function
releases all native snapshot and value handles before it returns.
-}
queryMetrics :: Connection -> IO QueryMetrics
queryMetrics connection = withConnectionHandle connection \native ->
    bracket (output (c_duckdb_v2_connection_get_query_metrics native)) (destroy c_duckdb_v2_query_metrics_destroy) \snapshot ->
        withConnectionFactory native \factory -> do
            root <- output (c_duckdb_v2_query_metrics_get_root snapshot)
            copyNode (materializeValue unexpectedVariant factory) root
  where
    unexpectedVariant _ = throwIO (SQLError "duckdb-simple: query metrics must be numeric" Nothing Nothing)
    copyNode readValue node = do
        name <- output (c_duckdb_v2_query_metrics_node_get_name node) >>= readText
        metricCount <- output (c_duckdb_v2_query_metrics_node_get_metric_count node)
        metrics <- forM (indices metricCount) \index -> do
            key <- output (c_duckdb_v2_query_metrics_node_get_metric_name node index) >>= readText
            value <- bracket (output (c_duckdb_v2_query_metrics_node_get_metric_value node index)) (destroy c_duckdb_v2_value_destroy) readValue
            pure (key, value)
        childCount <- output (c_duckdb_v2_query_metrics_node_get_child_count node)
        children <- forM (indices childCount) \index ->
            output (c_duckdb_v2_query_metrics_node_get_child node index) >>= copyNode readValue
        pure (QueryMetrics name metrics children)
    indices count = takeWhile (< count) [0 ..]
