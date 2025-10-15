module Database.DuckDB.FFI.StreamingResult (
    c_duckdb_fetch_chunk,
) where

import Database.DuckDB.FFI.Types
import Foreign.Ptr (Ptr)

{- | Fetches a data chunk from a duckdb_result. This function should be called
repeatedly until the result is exhausted.

The result must be destroyed with @duckdb_destroy_data_chunk@.

It is not known beforehand how many chunks will be returned by this result.

Parameters:
* @result@: The result object to fetch the data chunk from.

Returns The resulting data chunk. Returns @NULL@ if the result has an error.

These bindings call the wrapper symbol @wrapped_duckdb_fetch_chunk@ but
mirror the DuckDB C API semantics of @duckdb_fetch_chunk@.
-}
foreign import ccall safe "wrapped_duckdb_fetch_chunk"
    c_duckdb_fetch_chunk :: Ptr DuckDBResult -> IO DuckDBDataChunk
