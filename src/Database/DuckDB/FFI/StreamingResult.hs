module Database.DuckDB.FFI.StreamingResult (
  c_duckdb_stream_fetch_chunk,
  c_duckdb_fetch_chunk
) where

import Database.DuckDB.FFI.Types
import Foreign.Ptr (Ptr)

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Fetches a data chunk from the (streaming) duckdb_result. This function should
-- be called repeatedly until the result is exhausted.
--
-- The result must be destroyed with @duckdb_destroy_data_chunk@.
--
-- This function can only be used on duckdb_results created with
-- @duckdb_pending_prepared_streaming@
--
-- If this function is used, none of the other result functions can be used and
-- vice versa (i.e., this function cannot be mixed with the legacy result
-- functions or the materialized result functions).
--
-- It is not known beforehand how many chunks will be returned by this result.
--
-- Parameters:
-- * @result@: The result object to fetch the data chunk from.
--
-- Returns The resulting data chunk. Returns @NULL@ if the result has an error.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_stream_fetch_chunk@ but mirror the DuckDB C API semantics of
-- @duckdb_stream_fetch_chunk@.
foreign import ccall safe "wrapped_duckdb_stream_fetch_chunk"
  c_duckdb_stream_fetch_chunk :: Ptr DuckDBResult -> IO DuckDBDataChunk

-- | Fetches a data chunk from a duckdb_result. This function should be called
-- repeatedly until the result is exhausted.
--
-- The result must be destroyed with @duckdb_destroy_data_chunk@.
--
-- It is not known beforehand how many chunks will be returned by this result.
--
-- Parameters:
-- * @result@: The result object to fetch the data chunk from.
--
-- Returns The resulting data chunk. Returns @NULL@ if the result has an error.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_fetch_chunk@ but
-- mirror the DuckDB C API semantics of @duckdb_fetch_chunk@.
foreign import ccall safe "wrapped_duckdb_fetch_chunk"
  c_duckdb_fetch_chunk :: Ptr DuckDBResult -> IO DuckDBDataChunk
