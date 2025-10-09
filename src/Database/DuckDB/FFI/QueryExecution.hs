module Database.DuckDB.FFI.QueryExecution (
  c_duckdb_query,
  c_duckdb_destroy_result,
  c_duckdb_column_name,
  c_duckdb_column_type,
  c_duckdb_result_statement_type,
  c_duckdb_column_logical_type,
  c_duckdb_result_get_arrow_options,
  c_duckdb_column_count,
  c_duckdb_row_count,
  c_duckdb_rows_changed,
  c_duckdb_column_data,
  c_duckdb_nullmask_data,
  c_duckdb_result_error,
  c_duckdb_result_error_type
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)

-- | Executes a SQL query within a connection and stores the full (materialized)
-- result in the out_result pointer. If the query fails to execute, DuckDBError
-- is returned and the error message can be retrieved by calling
-- @duckdb_result_error@.
--
-- Note that after running @duckdb_query@, @duckdb_destroy_result@ must be called
-- on the result object even if the query fails, otherwise the error stored
-- within the result will not be freed correctly.
--
-- Parameters:
-- * @connection@: The connection to perform the query in.
-- * @query@: The SQL query to run.
-- * @out_result@: The query result.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall safe "duckdb_query"
  c_duckdb_query :: DuckDBConnection -> CString -> Ptr DuckDBResult -> IO DuckDBState

-- | Closes the result and de-allocates all memory allocated for that result.
--
-- Parameters:
-- * @result@: The result to destroy.
foreign import ccall unsafe "duckdb_destroy_result"
  c_duckdb_destroy_result :: Ptr DuckDBResult -> IO ()

-- | Returns the column name of the specified column. The result should not need to
-- be freed; the column names will automatically be destroyed when the result is
-- destroyed.
--
-- Returns @NULL@ if the column is out of range.
--
-- Parameters:
-- * @result@: The result object to fetch the column name from.
-- * @col@: The column index.
--
-- Returns The column name of the specified column.
foreign import ccall unsafe "duckdb_column_name"
  c_duckdb_column_name :: Ptr DuckDBResult -> DuckDBIdx -> IO CString

-- | Returns the column type of the specified column.
--
-- Returns @DUCKDB_TYPE_INVALID@ if the column is out of range.
--
-- Parameters:
-- * @result@: The result object to fetch the column type from.
-- * @col@: The column index.
--
-- Returns The column type of the specified column.
foreign import ccall unsafe "duckdb_column_type"
  c_duckdb_column_type :: Ptr DuckDBResult -> DuckDBIdx -> IO DuckDBType

-- | Returns the statement type of the statement that was executed
--
-- Parameters:
-- * @result@: The result object to fetch the statement type from.
--
-- Returns duckdb_statement_type value or DUCKDB_STATEMENT_TYPE_INVALID
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_result_statement_type@ but mirror the DuckDB C API semantics of
-- @duckdb_result_statement_type@.
foreign import ccall unsafe "wrapped_duckdb_result_statement_type"
  c_duckdb_result_statement_type :: Ptr DuckDBResult -> IO DuckDBStatementType

-- | Returns the logical column type of the specified column.
--
-- The return type of this call should be destroyed with
-- @duckdb_destroy_logical_type@.
--
-- Returns @NULL@ if the column is out of range.
--
-- Parameters:
-- * @result@: The result object to fetch the column type from.
-- * @col@: The column index.
--
-- Returns The logical column type of the specified column.
foreign import ccall unsafe "duckdb_column_logical_type"
  c_duckdb_column_logical_type :: Ptr DuckDBResult -> DuckDBIdx -> IO DuckDBLogicalType

-- | Returns the arrow options associated with the given result. These options are
-- definitions of how the arrow arrays/schema should be produced.
--
-- Parameters:
-- * @result@: The result object to fetch arrow options from.
--
-- Returns The arrow options associated with the given result. This must be
-- destroyed with @duckdb_destroy_arrow_options@.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_result_get_arrow_options@ but mirror the DuckDB C API semantics
-- of @duckdb_result_get_arrow_options@.
foreign import ccall unsafe "wrapped_duckdb_result_get_arrow_options"
  c_duckdb_result_get_arrow_options :: Ptr DuckDBResult -> IO DuckDBArrowOptions

-- | Returns the number of columns present in a the result object.
--
-- Parameters:
-- * @result@: The result object.
--
-- Returns The number of columns present in the result object.
foreign import ccall unsafe "duckdb_column_count"
  c_duckdb_column_count :: Ptr DuckDBResult -> IO DuckDBIdx

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns the number of rows present in the result object.
--
-- Parameters:
-- * @result@: The result object.
--
-- Returns The number of rows present in the result object.
foreign import ccall safe "duckdb_row_count"
  c_duckdb_row_count :: Ptr DuckDBResult -> IO DuckDBIdx

-- | Returns the number of rows changed by the query stored in the result. This is
-- relevant only for INSERT/UPDATE/DELETE queries. For other queries the
-- rows_changed will be 0.
--
-- Parameters:
-- * @result@: The result object.
--
-- Returns The number of rows changed.
foreign import ccall unsafe "duckdb_rows_changed"
  c_duckdb_rows_changed :: Ptr DuckDBResult -> IO DuckDBIdx

-- | > Deprecated This method has been deprecated. Prefer using
-- @duckdb_result_get_chunk@ instead.
--
-- Returns the data of a specific column of a result in columnar format.
--
-- The function returns a dense array which contains the result data. The exact
-- type stored in the array depends on the corresponding duckdb_type (as provided
-- by @duckdb_column_type@). For the exact type by which the data should be
-- accessed, see the comments in [the types section](types) or the @DUCKDB_TYPE@
-- enum.
--
-- For example, for a column of type @DUCKDB_TYPE_INTEGER@, rows can be accessed
-- in the following manner: ``@c int32_t *data = (int32_t *)
-- duckdb_column_data(&result, 0); printf("Data for row %d: %d\n", row,
-- data[row]); @``
--
-- Parameters:
-- * @result@: The result object to fetch the column data from.
-- * @col@: The column index.
--
-- Returns The column data of the specified column.
foreign import ccall unsafe "duckdb_column_data"
  c_duckdb_column_data :: Ptr DuckDBResult -> DuckDBIdx -> IO (Ptr ())

-- | > Deprecated This method has been deprecated. Prefer using
-- @duckdb_result_get_chunk@ instead.
--
-- Returns the nullmask of a specific column of a result in columnar format. The
-- nullmask indicates for every row whether or not the corresponding row is
-- @NULL@. If a row is @NULL@, the values present in the array provided by
-- @duckdb_column_data@ are undefined.
--
-- ``@c int32_t *data = (int32_t *) duckdb_column_data(&result, 0); bool
-- *nullmask = duckdb_nullmask_data(&result, 0); if (nullmask[row]) {
-- printf("Data for row %d: NULL\n", row); } else { printf("Data for row %d:
-- %d\n", row, data[row]); } @``
--
-- Parameters:
-- * @result@: The result object to fetch the nullmask from.
-- * @col@: The column index.
--
-- Returns The nullmask of the specified column.
foreign import ccall unsafe "duckdb_nullmask_data"
  c_duckdb_nullmask_data :: Ptr DuckDBResult -> DuckDBIdx -> IO (Ptr CBool)

-- | Returns the error message contained within the result. The error is only set
-- if @duckdb_query@ returns @DuckDBError@.
--
-- The result of this function must not be freed. It will be cleaned up when
-- @duckdb_destroy_result@ is called.
--
-- Parameters:
-- * @result@: The result object to fetch the error from.
--
-- Returns The error of the result.
foreign import ccall unsafe "duckdb_result_error"
  c_duckdb_result_error :: Ptr DuckDBResult -> IO CString

-- | Returns the result error type contained within the result. The error is only
-- set if @duckdb_query@ returns @DuckDBError@.
--
-- Parameters:
-- * @result@: The result object to fetch the error from.
--
-- Returns The error type of the result.
foreign import ccall unsafe "duckdb_result_error_type"
  c_duckdb_result_error_type :: Ptr DuckDBResult -> IO DuckDBErrorType
