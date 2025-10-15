module Database.DuckDB.FFI.Deprecated.QueryExecution (
    c_duckdb_column_data,
    c_duckdb_nullmask_data,
    c_duckdb_row_count,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.Types (CBool (..))
import Foreign.Ptr (Ptr)

{- | > Deprecated This method has been deprecated. Prefer using
@duckdb_result_get_chunk@ instead.

Returns the data of a specific column of a result in columnar format.

The function returns a dense array which contains the result data. The exact
type stored in the array depends on the corresponding duckdb_type (as provided
by @duckdb_column_type@). For the exact type by which the data should be
accessed, see the comments in [the types section](types) or the @DUCKDB_TYPE@
enum.

For example, for a column of type @DUCKDB_TYPE_INTEGER@, rows can be accessed
in the following manner: ``@c int32_t *data = (int32_t *)
duckdb_column_data(&result, 0); printf("Data for row %d: %d\n", row,
data[row]); @``

Parameters:
* @result@: The result object to fetch the column data from.
* @col@: The column index.

Returns The column data of the specified column.
-}
foreign import ccall safe "duckdb_column_data"
    c_duckdb_column_data :: Ptr DuckDBResult -> DuckDBIdx -> IO (Ptr ())

{- | > Deprecated This method has been deprecated. Prefer using
@duckdb_result_get_chunk@ instead.

Returns the nullmask of a specific column of a result in columnar format. The
nullmask indicates for every row whether or not the corresponding row is
@NULL@. If a row is @NULL@, the values present in the array provided by
@duckdb_column_data@ are undefined.

``@c int32_t *data = (int32_t *) duckdb_column_data(&result, 0); bool
*nullmask = duckdb_nullmask_data(&result, 0); if (nullmask[row]) {
printf("Data for row %d: NULL\n", row); } else { printf("Data for row %d:
%d\n", row, data[row]); } @``

Parameters:
* @result@: The result object to fetch the nullmask from.
* @col@: The column index.

Returns The nullmask of the specified column.
-}
foreign import ccall safe "duckdb_nullmask_data"
    c_duckdb_nullmask_data :: Ptr DuckDBResult -> DuckDBIdx -> IO (Ptr CBool)

{- | > Warning Deprecation notice. This method is scheduled for removal in a future
release.

Returns the number of rows present in the result object.

Parameters:
* @result@: The result object.

Returns The number of rows present in the result object.
-}
foreign import ccall safe "duckdb_row_count"
    c_duckdb_row_count :: Ptr DuckDBResult -> IO DuckDBIdx
