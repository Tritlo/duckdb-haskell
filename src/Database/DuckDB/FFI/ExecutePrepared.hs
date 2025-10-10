module Database.DuckDB.FFI.ExecutePrepared (
    c_duckdb_execute_prepared,
    c_duckdb_execute_prepared_streaming,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

{- | Executes the prepared statement with the given bound parameters, and returns a
materialized query result.

This method can be called multiple times for each prepared statement, and the
parameters can be modified between calls to this function.

Note that the result must be freed with @duckdb_destroy_result@.

Parameters:
* @prepared_statement@: The prepared statement to execute.
* @out_result@: The query result.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall safe "duckdb_execute_prepared"
    c_duckdb_execute_prepared :: DuckDBPreparedStatement -> Ptr DuckDBResult -> IO DuckDBState

{- | > Warning Deprecation notice. This method is scheduled for removal in a future
release.

Executes the prepared statement with the given bound parameters, and returns
an optionally-streaming query result. To determine if the resulting query was
in fact streamed, use @duckdb_result_is_streaming@

This method can be called multiple times for each prepared statement, and the
parameters can be modified between calls to this function.

Note that the result must be freed with @duckdb_destroy_result@.

Parameters:
* @prepared_statement@: The prepared statement to execute.
* @out_result@: The query result.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall safe "duckdb_execute_prepared_streaming"
    c_duckdb_execute_prepared_streaming :: DuckDBPreparedStatement -> Ptr DuckDBResult -> IO DuckDBState
