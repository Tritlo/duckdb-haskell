module Database.DuckDB.FFI.PreparedStatements (
    c_duckdb_prepare,
    c_duckdb_destroy_prepare,
    c_duckdb_prepare_error,
    c_duckdb_nparams,
    c_duckdb_parameter_name,
    c_duckdb_param_type,
    c_duckdb_param_logical_type,
    c_duckdb_clear_bindings,
    c_duckdb_prepared_statement_type,
    c_duckdb_prepared_statement_column_count,
    c_duckdb_prepared_statement_column_name,
    c_duckdb_prepared_statement_column_logical_type,
    c_duckdb_prepared_statement_column_type,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

{- | Create a prepared statement object from a query.

Note that after calling @duckdb_prepare@, the prepared statement should always
be destroyed using @duckdb_destroy_prepare@, even if the prepare fails.

If the prepare fails, @duckdb_prepare_error@ can be called to obtain the
reason why the prepare failed.

Parameters:
* @connection@: The connection object
* @query@: The SQL query to prepare
* @out_prepared_statement@: The resulting prepared statement object

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall "duckdb_prepare"
    c_duckdb_prepare :: DuckDBConnection -> CString -> Ptr DuckDBPreparedStatement -> IO DuckDBState

{- | Closes the prepared statement and de-allocates all memory allocated for the
statement.

Parameters:
* @prepared_statement@: The prepared statement to destroy.
-}
foreign import ccall safe "duckdb_destroy_prepare"
    c_duckdb_destroy_prepare :: Ptr DuckDBPreparedStatement -> IO ()

{- | Returns the error message associated with the given prepared statement. If the
prepared statement has no error message, this returns @nullptr@ instead.

The error message should not be freed. It will be de-allocated when
@duckdb_destroy_prepare@ is called.

Parameters:
* @prepared_statement@: The prepared statement to obtain the error from.

Returns The error message, or @nullptr@ if there is none.
-}
foreign import ccall safe "duckdb_prepare_error"
    c_duckdb_prepare_error :: DuckDBPreparedStatement -> IO CString

{- | Returns the number of parameters that can be provided to the given prepared
statement.

Returns 0 if the query was not successfully prepared.

Parameters:
* @prepared_statement@: The prepared statement to obtain the number of
  parameters for.
-}
foreign import ccall safe "duckdb_nparams"
    c_duckdb_nparams :: DuckDBPreparedStatement -> IO DuckDBIdx

{- | Returns the name used to identify the parameter The returned string should be
freed using @duckdb_free@.

Returns NULL if the index is out of range for the provided prepared statement.

Parameters:
* @prepared_statement@: The prepared statement for which to get the parameter
  name from.
-}
foreign import ccall safe "duckdb_parameter_name"
    c_duckdb_parameter_name :: DuckDBPreparedStatement -> DuckDBIdx -> IO CString

{- | Returns the parameter type for the parameter at the given index.

Returns @DUCKDB_TYPE_INVALID@ if the parameter index is out of range or the
statement was not successfully prepared.

Parameters:
* @prepared_statement@: The prepared statement.
* @param_idx@: The parameter index.

Returns The parameter type
-}
foreign import ccall safe "duckdb_param_type"
    c_duckdb_param_type :: DuckDBPreparedStatement -> DuckDBIdx -> IO DuckDBType

{- | Returns the logical type for the parameter at the given index.

Returns @nullptr@ if the parameter index is out of range or the statement was
not successfully prepared.

The return type of this call should be destroyed with
@duckdb_destroy_logical_type@.

Parameters:
* @prepared_statement@: The prepared statement.
* @param_idx@: The parameter index.

Returns The logical type of the parameter
-}
foreign import ccall safe "duckdb_param_logical_type"
    c_duckdb_param_logical_type :: DuckDBPreparedStatement -> DuckDBIdx -> IO DuckDBLogicalType

-- | Clear the params bind to the prepared statement.
foreign import ccall safe "duckdb_clear_bindings"
    c_duckdb_clear_bindings :: DuckDBPreparedStatement -> IO DuckDBState

{- | Returns the statement type of the statement to be executed

Parameters:
* @statement@: The prepared statement.

Returns duckdb_statement_type value or DUCKDB_STATEMENT_TYPE_INVALID
-}
foreign import ccall safe "duckdb_prepared_statement_type"
    c_duckdb_prepared_statement_type :: DuckDBPreparedStatement -> IO DuckDBStatementType

{- | Returns the number of columns present in a the result of the prepared
statement. If any of the column types are invalid, the result will be 1.

Parameters:
* @prepared_statement@: The prepared statement.

Returns The number of columns present in the result of the prepared statement.
-}
foreign import ccall safe "duckdb_prepared_statement_column_count"
    c_duckdb_prepared_statement_column_count :: DuckDBPreparedStatement -> IO DuckDBIdx

{- | Returns the name of the specified column of the result of the
prepared_statement. The returned string should be freed using @duckdb_free@.

Returns @nullptr@ if the column is out of range.

Parameters:
* @prepared_statement@: The prepared statement.
* @col_idx@: The column index.

Returns The column name of the specified column.
-}
foreign import ccall safe "duckdb_prepared_statement_column_name"
    c_duckdb_prepared_statement_column_name :: DuckDBPreparedStatement -> DuckDBIdx -> IO CString

{- | Returns the column type of the specified column of the result of the
prepared_statement.

Returns @DUCKDB_TYPE_INVALID@ if the column is out of range. The return type
of this call should be destroyed with @duckdb_destroy_logical_type@.

Parameters:
* @prepared_statement@: The prepared statement to fetch the column type from.
* @col_idx@: The column index.

Returns The logical type of the specified column.
-}
foreign import ccall safe "duckdb_prepared_statement_column_logical_type"
    c_duckdb_prepared_statement_column_logical_type :: DuckDBPreparedStatement -> DuckDBIdx -> IO DuckDBLogicalType

{- | Returns the column type of the specified column of the result of the
prepared_statement.

Returns @DUCKDB_TYPE_INVALID@ if the column is out of range.

Parameters:
* @prepared_statement@: The prepared statement to fetch the column type from.
* @col_idx@: The column index.

Returns The type of the specified column.
-}
foreign import ccall safe "duckdb_prepared_statement_column_type"
    c_duckdb_prepared_statement_column_type :: DuckDBPreparedStatement -> DuckDBIdx -> IO DuckDBType
