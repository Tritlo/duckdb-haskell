module Database.DuckDB.FFI.PendingResult (
    c_duckdb_pending_prepared,
    c_duckdb_pending_prepared_streaming,
    c_duckdb_destroy_pending,
    c_duckdb_pending_error,
    c_duckdb_pending_execute_task,
    c_duckdb_pending_execute_check_state,
    c_duckdb_execute_pending,
    c_duckdb_pending_execution_is_finished,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)

{- | Executes the prepared statement with the given bound parameters, and returns a
pending result. The pending result represents an intermediate structure for a
query that is not yet fully executed. The pending result can be used to
incrementally execute a query, returning control to the client between tasks.

Note that after calling @duckdb_pending_prepared@, the pending result should
always be destroyed using @duckdb_destroy_pending@, even if this function
returns DuckDBError.

Parameters:
* @prepared_statement@: The prepared statement to execute.
* @out_result@: The pending query result.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall safe "duckdb_pending_prepared"
    c_duckdb_pending_prepared :: DuckDBPreparedStatement -> Ptr DuckDBPendingResult -> IO DuckDBState

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
foreign import ccall safe "duckdb_pending_prepared_streaming"
    c_duckdb_pending_prepared_streaming :: DuckDBPreparedStatement -> Ptr DuckDBPendingResult -> IO DuckDBState

{- | Closes the pending result and de-allocates all memory allocated for the
result.

Parameters:
* @pending_result@: The pending result to destroy.
-}
foreign import ccall unsafe "duckdb_destroy_pending"
    c_duckdb_destroy_pending :: Ptr DuckDBPendingResult -> IO ()

{- | Returns the error message contained within the pending result.

The result of this function must not be freed. It will be cleaned up when
@duckdb_destroy_pending@ is called.

Parameters:
* @pending_result@: The pending result to fetch the error from.

Returns The error of the pending result.
-}
foreign import ccall unsafe "duckdb_pending_error"
    c_duckdb_pending_error :: DuckDBPendingResult -> IO CString

{- | Executes a single task within the query, returning whether or not the query is
ready.

If this returns DUCKDB_PENDING_RESULT_READY, the duckdb_execute_pending
function can be called to obtain the result. If this returns
DUCKDB_PENDING_RESULT_NOT_READY, the duckdb_pending_execute_task function
should be called again. If this returns DUCKDB_PENDING_ERROR, an error
occurred during execution.

The error message can be obtained by calling duckdb_pending_error on the
pending_result.

Parameters:
* @pending_result@: The pending result to execute a task within.

Returns The state of the pending result after the execution.
-}
foreign import ccall safe "duckdb_pending_execute_task"
    c_duckdb_pending_execute_task :: DuckDBPendingResult -> IO DuckDBPendingState

{- | If this returns DUCKDB_PENDING_RESULT_READY, the duckdb_execute_pending
function can be called to obtain the result. If this returns
DUCKDB_PENDING_RESULT_NOT_READY, the duckdb_pending_execute_check_state
function should be called again. If this returns DUCKDB_PENDING_ERROR, an
error occurred during execution.

The error message can be obtained by calling duckdb_pending_error on the
pending_result.

Parameters:
* @pending_result@: The pending result.

Returns The state of the pending result.
-}
foreign import ccall unsafe "duckdb_pending_execute_check_state"
    c_duckdb_pending_execute_check_state :: DuckDBPendingResult -> IO DuckDBPendingState

{- | Fully execute a pending query result, returning the final query result.

If duckdb_pending_execute_task has been called until
DUCKDB_PENDING_RESULT_READY was returned, this will return fast. Otherwise,
all remaining tasks must be executed first.

Note that the result must be freed with @duckdb_destroy_result@.

Parameters:
* @pending_result@: The pending result to execute.
* @out_result@: The result object.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall safe "duckdb_execute_pending"
    c_duckdb_execute_pending :: DuckDBPendingResult -> Ptr DuckDBResult -> IO DuckDBState

{- | Returns whether a duckdb_pending_state is finished executing. For example if
@pending_state@ is DUCKDB_PENDING_RESULT_READY, this function will return
true.

Parameters:
* @pending_state@: The pending state on which to decide whether to finish
  execution.

Returns Boolean indicating pending execution should be considered finished.
-}
foreign import ccall unsafe "duckdb_pending_execution_is_finished"
    c_duckdb_pending_execution_is_finished :: DuckDBPendingState -> IO CBool
