module Database.DuckDB.FFI.Deprecated.Appender (
    c_duckdb_appender_error,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)

{- | > Warning Deprecation notice. This method is scheduled for removal in a future
release. Use duckdb_appender_error_data instead.

Returns the error message associated with the appender. If the appender has no
error message, this returns @nullptr@ instead.

The error message should not be freed. It will be de-allocated when
@duckdb_appender_destroy@ is called.

Parameters:
* @appender@: The appender to get the error from.

Returns The error message, or @nullptr@ if there is none.
-}
foreign import ccall "duckdb_appender_error"
    c_duckdb_appender_error :: DuckDBAppender -> IO CString
