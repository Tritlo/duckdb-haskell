module Database.DuckDB.FFI.ErrorData (
    c_duckdb_create_error_data,
    c_duckdb_destroy_error_data,
    c_duckdb_error_data_error_type,
    c_duckdb_error_data_message,
    c_duckdb_error_data_has_error,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)

{- | Creates duckdb_error_data. Must be destroyed with @duckdb_destroy_error_data@.

Parameters:
* @type@: The error type.
* @message@: The error message.

Returns The error data.
-}
foreign import ccall safe "duckdb_create_error_data"
    c_duckdb_create_error_data :: DuckDBErrorType -> CString -> IO DuckDBErrorData

{- | Destroys the error data and deallocates its memory.

Parameters:
* @error_data@: The error data to destroy.
-}
foreign import ccall safe "duckdb_destroy_error_data"
    c_duckdb_destroy_error_data :: Ptr DuckDBErrorData -> IO ()

{- | Returns the duckdb_error_type of the error data.

Parameters:
* @error_data@: The error data.

Returns The error type.
-}
foreign import ccall safe "duckdb_error_data_error_type"
    c_duckdb_error_data_error_type :: DuckDBErrorData -> IO DuckDBErrorType

{- | Returns the error message of the error data. Must not be freed.

Parameters:
* @error_data@: The error data.

Returns The error message.
-}
foreign import ccall safe "duckdb_error_data_message"
    c_duckdb_error_data_message :: DuckDBErrorData -> IO CString

{- | Returns whether the error data contains an error or not.

Parameters:
* @error_data@: The error data.

Returns True, if the error data contains an exception, else false.
-}
foreign import ccall safe "duckdb_error_data_has_error"
    c_duckdb_error_data_has_error :: DuckDBErrorData -> IO CBool
