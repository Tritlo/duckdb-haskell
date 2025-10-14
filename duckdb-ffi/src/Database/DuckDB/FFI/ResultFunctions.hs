module Database.DuckDB.FFI.ResultFunctions (
    c_duckdb_result_get_chunk,
    c_duckdb_result_is_streaming,
    c_duckdb_result_chunk_count,
    c_duckdb_result_return_type,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)

{- | > Warning Deprecation notice. This method is scheduled for removal in a future
release.

Fetches a data chunk from the duckdb_result. This function should be called
repeatedly until the result is exhausted.

The result must be destroyed with @duckdb_destroy_data_chunk@.

This function supersedes all @duckdb_value@ functions, as well as the
@duckdb_column_data@ and @duckdb_nullmask_data@ functions. It results in
significantly better performance, and should be preferred in newer code-bases.

If this function is used, none of the other result functions can be used and
vice versa (i.e., this function cannot be mixed with the legacy result
functions).

Use @duckdb_result_chunk_count@ to figure out how many chunks there are in the
result.

Parameters:
* @result@: The result object to fetch the data chunk from.
* @chunk_index@: The chunk index to fetch from.

Returns The resulting data chunk. Returns @NULL@ if the chunk index is out of
bounds.

These bindings call the wrapper symbol
@wrapped_duckdb_result_get_chunk@ but mirror the DuckDB C API semantics of
@duckdb_result_get_chunk@.
-}
foreign import ccall safe "wrapped_duckdb_result_get_chunk"
    c_duckdb_result_get_chunk :: Ptr DuckDBResult -> DuckDBIdx -> IO DuckDBDataChunk

{- | > Warning Deprecation notice. This method is scheduled for removal in a future
release.

Checks if the type of the internal result is StreamQueryResult.

Parameters:
* @result@: The result object to check.

Returns Whether or not the result object is of the type StreamQueryResult

These bindings call the wrapper symbol
@wrapped_duckdb_result_is_streaming@ but mirror the DuckDB C API semantics of
@duckdb_result_is_streaming@.
-}
foreign import ccall safe "wrapped_duckdb_result_is_streaming"
    c_duckdb_result_is_streaming :: Ptr DuckDBResult -> IO CBool

{- | > Warning Deprecation notice. This method is scheduled for removal in a future
release.

Returns the number of data chunks present in the result.

Parameters:
* @result@: The result object

Returns Number of data chunks present in the result.

These bindings call the wrapper symbol
@wrapped_duckdb_result_chunk_count@ but mirror the DuckDB C API semantics of
@duckdb_result_chunk_count@.
-}
foreign import ccall safe "wrapped_duckdb_result_chunk_count"
    c_duckdb_result_chunk_count :: Ptr DuckDBResult -> IO DuckDBIdx

{- | Returns the return_type of the given result, or DUCKDB_RETURN_TYPE_INVALID on
error

Parameters:
* @result@: The result object

Returns The return_type

These bindings call the wrapper symbol
@wrapped_duckdb_result_return_type@ but mirror the DuckDB C API semantics of
@duckdb_result_return_type@.
-}
foreign import ccall safe "wrapped_duckdb_result_return_type"
    c_duckdb_result_return_type :: Ptr DuckDBResult -> IO DuckDBResultType
