module Database.DuckDB.FFI.TableDescription (
    c_duckdb_table_description_create,
    c_duckdb_table_description_create_ext,
    c_duckdb_table_description_destroy,
    c_duckdb_table_description_error,
    c_duckdb_column_has_default,
    c_duckdb_table_description_get_column_name,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)

{- | Creates a table description object. Note that
@duckdb_table_description_destroy@ should always be called on the resulting
table_description, even if the function returns @DuckDBError@.

Parameters:
* @connection@: The connection context.
* @schema@: The schema of the table, or @nullptr@ for the default schema.
* @table@: The table name.
* @out@: The resulting table description object.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall safe "duckdb_table_description_create"
    c_duckdb_table_description_create :: DuckDBConnection -> CString -> CString -> Ptr DuckDBTableDescription -> IO DuckDBState

{- | Creates a table description object. Note that
@duckdb_table_description_destroy@ must be called on the resulting
table_description, even if the function returns @DuckDBError@.

Parameters:
* @connection@: The connection context.
* @catalog@: The catalog (database) name of the table, or @nullptr@ for the
  default catalog.
* @schema@: The schema of the table, or @nullptr@ for the default schema.
* @table@: The table name.
* @out@: The resulting table description object.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall safe "duckdb_table_description_create_ext"
    c_duckdb_table_description_create_ext :: DuckDBConnection -> CString -> CString -> CString -> Ptr DuckDBTableDescription -> IO DuckDBState

{- | Destroy the TableDescription object.

Parameters:
* @table_description@: The table_description to destroy.
-}
foreign import ccall safe "duckdb_table_description_destroy"
    c_duckdb_table_description_destroy :: Ptr DuckDBTableDescription -> IO ()

{- | Returns the error message associated with the given table_description. If the
table_description has no error message, this returns @nullptr@ instead. The
error message should not be freed. It will be de-allocated when
@duckdb_table_description_destroy@ is called.

Parameters:
* @table_description@: The table_description to get the error from.

Returns The error message, or @nullptr@ if there is none.
-}
foreign import ccall safe "duckdb_table_description_error"
    c_duckdb_table_description_error :: DuckDBTableDescription -> IO CString

{- | Check if the column at @index@ index of the table has a DEFAULT expression.

Parameters:
* @table_description@: The table_description to query.
* @index@: The index of the column to query.
* @out@: The out-parameter used to store the result.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall safe "duckdb_column_has_default"
    c_duckdb_column_has_default :: DuckDBTableDescription -> DuckDBIdx -> Ptr CBool -> IO DuckDBState

{- | Obtain the column name at @index@. The out result must be destroyed with
@duckdb_free@.

Parameters:
* @table_description@: The table_description to query.
* @index@: The index of the column to query.

Returns The column name.
-}
foreign import ccall safe "duckdb_table_description_get_column_name"
    c_duckdb_table_description_get_column_name :: DuckDBTableDescription -> DuckDBIdx -> IO CString
