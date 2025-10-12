module Database.DuckDB.FFI.CastFunctions (
    c_duckdb_create_cast_function,
    c_duckdb_cast_function_set_source_type,
    c_duckdb_cast_function_set_target_type,
    c_duckdb_cast_function_set_implicit_cast_cost,
    c_duckdb_cast_function_set_function,
    c_duckdb_cast_function_set_extra_info,
    c_duckdb_cast_function_get_extra_info,
    c_duckdb_cast_function_get_cast_mode,
    c_duckdb_cast_function_set_error,
    c_duckdb_cast_function_set_row_error,
    c_duckdb_register_cast_function,
    c_duckdb_destroy_cast_function,
) where

import Data.Int (Int64)
import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

{- | Creates a new cast function object.

Returns The cast function object.
-}
foreign import ccall safe "duckdb_create_cast_function"
    c_duckdb_create_cast_function :: IO DuckDBCastFunction

{- | Sets the source type of the cast function.

Parameters:
* @cast_function@: The cast function object.
* @source_type@: The source type to set.
-}
foreign import ccall safe "duckdb_cast_function_set_source_type"
    c_duckdb_cast_function_set_source_type :: DuckDBCastFunction -> DuckDBLogicalType -> IO ()

{- | Sets the target type of the cast function.

Parameters:
* @cast_function@: The cast function object.
* @target_type@: The target type to set.
-}
foreign import ccall safe "duckdb_cast_function_set_target_type"
    c_duckdb_cast_function_set_target_type :: DuckDBCastFunction -> DuckDBLogicalType -> IO ()

{- | Sets the "cost" of implicitly casting the source type to the target type using
this function.

Parameters:
* @cast_function@: The cast function object.
* @cost@: The cost to set.
-}
foreign import ccall safe "duckdb_cast_function_set_implicit_cast_cost"
    c_duckdb_cast_function_set_implicit_cast_cost :: DuckDBCastFunction -> Int64 -> IO ()

{- | Sets the actual cast function to use.

Parameters:
* @cast_function@: The cast function object.
* @function@: The function to set.
-}
foreign import ccall safe "duckdb_cast_function_set_function"
    c_duckdb_cast_function_set_function :: DuckDBCastFunction -> DuckDBCastFunctionFun -> IO ()

{- | Assigns extra information to the cast function that can be fetched during
execution, etc.

Parameters:
* @extra_info@: The extra information
* @destroy@: The callback that will be called to destroy the extra information
  (if any)
-}
foreign import ccall safe "duckdb_cast_function_set_extra_info"
    c_duckdb_cast_function_set_extra_info :: DuckDBCastFunction -> Ptr () -> DuckDBDeleteCallback -> IO ()

{- | Retrieves the extra info of the function as set in
@duckdb_cast_function_set_extra_info@.

Parameters:
* @info@: The info object.

Returns The extra info.
-}
foreign import ccall safe "duckdb_cast_function_get_extra_info"
    c_duckdb_cast_function_get_extra_info :: DuckDBFunctionInfo -> IO (Ptr ())

{- | Get the cast execution mode from the given function info.

Parameters:
* @info@: The info object.

Returns The cast mode.
-}
foreign import ccall safe "duckdb_cast_function_get_cast_mode"
    c_duckdb_cast_function_get_cast_mode :: DuckDBFunctionInfo -> IO DuckDBCastMode

{- | Report that an error has occurred while executing the cast function.

Parameters:
* @info@: The info object.
* @error@: The error message.
-}
foreign import ccall safe "duckdb_cast_function_set_error"
    c_duckdb_cast_function_set_error :: DuckDBFunctionInfo -> CString -> IO ()

{- | Report that an error has occurred while executing the cast function, setting
the corresponding output row to NULL.

Parameters:
* @info@: The info object.
* @error@: The error message.
* @row@: The index of the row within the output vector to set to NULL.
* @output@: The output vector.
-}
foreign import ccall safe "duckdb_cast_function_set_row_error"
    c_duckdb_cast_function_set_row_error :: DuckDBFunctionInfo -> CString -> DuckDBIdx -> DuckDBVector -> IO ()

{- | Registers a cast function within the given connection.

Parameters:
* @con@: The connection to use.
* @cast_function@: The cast function to register.

Returns Whether or not the registration was successful.
-}
foreign import ccall safe "duckdb_register_cast_function"
    c_duckdb_register_cast_function :: DuckDBConnection -> DuckDBCastFunction -> IO DuckDBState

{- | Destroys the cast function object.

Parameters:
* @cast_function@: The cast function object.
-}
foreign import ccall safe "duckdb_destroy_cast_function"
    c_duckdb_destroy_cast_function :: Ptr DuckDBCastFunction -> IO ()
