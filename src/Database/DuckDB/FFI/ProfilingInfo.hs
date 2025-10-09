module Database.DuckDB.FFI.ProfilingInfo (
  c_duckdb_get_profiling_info,
  c_duckdb_profiling_info_get_value,
  c_duckdb_profiling_info_get_metrics,
  c_duckdb_profiling_info_get_child_count,
  c_duckdb_profiling_info_get_child
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)

-- | Returns the root node of the profiling information. Returns nullptr, if
-- profiling is not enabled.
--
-- Parameters:
-- * @connection@: A connection object.
--
-- Returns A profiling information object.
foreign import ccall unsafe "duckdb_get_profiling_info"
  c_duckdb_get_profiling_info :: DuckDBConnection -> IO DuckDBProfilingInfo

-- | Returns the value of the metric of the current profiling info node. Returns
-- nullptr, if the metric does not exist or is not enabled. Currently, the value
-- holds a string, and you can retrieve the string by calling the corresponding
-- function: char *duckdb_get_varchar(duckdb_value value).
--
-- Parameters:
-- * @info@: A profiling information object.
-- * @key@: The name of the requested metric.
--
-- Returns The value of the metric. Must be freed with @duckdb_destroy_value@
foreign import ccall unsafe "duckdb_profiling_info_get_value"
  c_duckdb_profiling_info_get_value :: DuckDBProfilingInfo -> CString -> IO DuckDBValue

-- | Returns the key-value metric map of this profiling node as a MAP duckdb_value.
-- The individual elements are accessible via the duckdb_value MAP functions.
--
-- Parameters:
-- * @info@: A profiling information object.
--
-- Returns The key-value metric map as a MAP duckdb_value.
foreign import ccall unsafe "duckdb_profiling_info_get_metrics"
  c_duckdb_profiling_info_get_metrics :: DuckDBProfilingInfo -> IO DuckDBValue

-- | Returns the number of children in the current profiling info node.
--
-- Parameters:
-- * @info@: A profiling information object.
--
-- Returns The number of children in the current node.
foreign import ccall unsafe "duckdb_profiling_info_get_child_count"
  c_duckdb_profiling_info_get_child_count :: DuckDBProfilingInfo -> IO DuckDBIdx

-- | Returns the child node at the specified index.
--
-- Parameters:
-- * @info@: A profiling information object.
-- * @index@: The index of the child node.
--
-- Returns The child node at the specified index.
foreign import ccall unsafe "duckdb_profiling_info_get_child"
  c_duckdb_profiling_info_get_child :: DuckDBProfilingInfo -> DuckDBIdx -> IO DuckDBProfilingInfo
