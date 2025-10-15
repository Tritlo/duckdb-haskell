module Database.DuckDB.FFI.Deprecated.PendingResult (
    c_duckdb_pending_prepared_streaming,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

{- | > Warning Deprecation notice. This method is scheduled for removal in a future
release.

Executes the prepared statement with the given bound parameters, and returns a
pending result. This pending result will create a streaming duckdb_result when
executed. The pending result represents an intermediate structure for a query
that is not yet fully executed.

Note that after calling @duckdb_pending_prepared_streaming@, the pending
result should always be destroyed using @duckdb_destroy_pending@, even if this
function returns DuckDBError.

Parameters:
* @prepared_statement@: The prepared statement to execute.
* @out_result@: The pending query result.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall "duckdb_pending_prepared_streaming"
    c_duckdb_pending_prepared_streaming :: DuckDBPreparedStatement -> Ptr DuckDBPendingResult -> IO DuckDBState
