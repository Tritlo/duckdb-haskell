module Database.DuckDB.FFI.ScalarFunctions (
  c_duckdb_create_scalar_function,
  c_duckdb_destroy_scalar_function,
  c_duckdb_scalar_function_set_name,
  c_duckdb_scalar_function_set_varargs,
  c_duckdb_scalar_function_set_special_handling,
  c_duckdb_scalar_function_set_volatile,
  c_duckdb_scalar_function_add_parameter,
  c_duckdb_scalar_function_set_return_type,
  c_duckdb_scalar_function_set_extra_info,
  c_duckdb_scalar_function_set_bind,
  c_duckdb_scalar_function_set_bind_data,
  c_duckdb_scalar_function_set_bind_data_copy,
  c_duckdb_scalar_function_bind_set_error,
  c_duckdb_scalar_function_set_function,
  c_duckdb_register_scalar_function,
  c_duckdb_scalar_function_get_extra_info,
  c_duckdb_scalar_function_bind_get_extra_info,
  c_duckdb_scalar_function_get_bind_data,
  c_duckdb_scalar_function_get_client_context,
  c_duckdb_scalar_function_set_error,
  c_duckdb_create_scalar_function_set,
  c_duckdb_destroy_scalar_function_set,
  c_duckdb_add_scalar_function_to_set,
  c_duckdb_register_scalar_function_set,
  c_duckdb_scalar_function_bind_get_argument_count,
  c_duckdb_scalar_function_bind_get_argument
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

-- | Creates a new empty scalar function.
--
-- The return value must be destroyed with @duckdb_destroy_scalar_function@.
--
-- Returns The scalar function object.
foreign import ccall unsafe "duckdb_create_scalar_function"
  c_duckdb_create_scalar_function :: IO DuckDBScalarFunction

-- | Destroys the given scalar function object.
--
-- Parameters:
-- * @scalar_function@: The scalar function to destroy
foreign import ccall unsafe "duckdb_destroy_scalar_function"
  c_duckdb_destroy_scalar_function :: Ptr DuckDBScalarFunction -> IO ()

-- | Sets the name of the given scalar function.
--
-- Parameters:
-- * @scalar_function@: The scalar function
-- * @name@: The name of the scalar function
foreign import ccall unsafe "duckdb_scalar_function_set_name"
  c_duckdb_scalar_function_set_name :: DuckDBScalarFunction -> CString -> IO ()

-- | Sets the parameters of the given scalar function to varargs. Does not require
-- adding parameters with duckdb_scalar_function_add_parameter.
--
-- Parameters:
-- * @scalar_function@: The scalar function.
-- * @type@: The type of the arguments.
--
-- Returns The parameter type. Cannot contain INVALID.
foreign import ccall unsafe "duckdb_scalar_function_set_varargs"
  c_duckdb_scalar_function_set_varargs :: DuckDBScalarFunction -> DuckDBLogicalType -> IO ()

-- | Sets the scalar function's null-handling behavior to special.
--
-- Parameters:
-- * @scalar_function@: The scalar function.
foreign import ccall unsafe "duckdb_scalar_function_set_special_handling"
  c_duckdb_scalar_function_set_special_handling :: DuckDBScalarFunction -> IO ()

-- | Sets the Function Stability of the scalar function to VOLATILE, indicating the
-- function should be re-run for every row. This limits optimization that can be
-- performed for the function.
--
-- Parameters:
-- * @scalar_function@: The scalar function.
foreign import ccall unsafe "duckdb_scalar_function_set_volatile"
  c_duckdb_scalar_function_set_volatile :: DuckDBScalarFunction -> IO ()

-- | Adds a parameter to the scalar function.
--
-- Parameters:
-- * @scalar_function@: The scalar function.
-- * @type@: The parameter type. Cannot contain INVALID.
foreign import ccall unsafe "duckdb_scalar_function_add_parameter"
  c_duckdb_scalar_function_add_parameter :: DuckDBScalarFunction -> DuckDBLogicalType -> IO ()

-- | Sets the return type of the scalar function.
--
-- Parameters:
-- * @scalar_function@: The scalar function
-- * @type@: Cannot contain INVALID or ANY.
foreign import ccall unsafe "duckdb_scalar_function_set_return_type"
  c_duckdb_scalar_function_set_return_type :: DuckDBScalarFunction -> DuckDBLogicalType -> IO ()

-- | Assigns extra information to the scalar function that can be fetched during
-- binding, etc.
--
-- Parameters:
-- * @scalar_function@: The scalar function
-- * @extra_info@: The extra information
-- * @destroy@: The callback that will be called to destroy the extra information
--   (if any)
foreign import ccall unsafe "duckdb_scalar_function_set_extra_info"
  c_duckdb_scalar_function_set_extra_info :: DuckDBScalarFunction -> Ptr () -> DuckDBDeleteCallback -> IO ()

-- | Sets the (optional) bind function of the scalar function.
--
-- Parameters:
-- * @scalar_function@: The scalar function.
-- * @bind@: The bind function.
foreign import ccall unsafe "duckdb_scalar_function_set_bind"
  c_duckdb_scalar_function_set_bind :: DuckDBScalarFunction -> DuckDBScalarFunctionBindFun -> IO ()

-- | Sets the user-provided bind data in the bind object of the scalar function.
-- The bind data object can be retrieved again during execution. In most case,
-- you also need to set the copy-callback of your bind data via
-- duckdb_scalar_function_set_bind_data_copy.
--
-- Parameters:
-- * @info@: The bind info of the scalar function.
-- * @bind_data@: The bind data object.
-- * @destroy@: The callback to destroy the bind data (if any).
foreign import ccall unsafe "duckdb_scalar_function_set_bind_data"
  c_duckdb_scalar_function_set_bind_data :: DuckDBBindInfo -> Ptr () -> DuckDBDeleteCallback -> IO ()

-- | Sets the copy-callback for the user-provided bind data in the bind object of
-- the scalar function.
--
-- Parameters:
-- * @info@: The bind info of the scalar function.
-- * @copy@: The callback to copy the bind data (if any).
foreign import ccall unsafe "duckdb_scalar_function_set_bind_data_copy"
  c_duckdb_scalar_function_set_bind_data_copy :: DuckDBBindInfo -> DuckDBCopyCallback -> IO ()

-- | Report that an error has occurred while calling bind on a scalar function.
--
-- Parameters:
-- * @info@: The bind info object.
-- * @error@: The error message.
foreign import ccall unsafe "duckdb_scalar_function_bind_set_error"
  c_duckdb_scalar_function_bind_set_error :: DuckDBBindInfo -> CString -> IO ()

-- | Sets the main function of the scalar function.
--
-- Parameters:
-- * @scalar_function@: The scalar function
-- * @function@: The function
foreign import ccall unsafe "duckdb_scalar_function_set_function"
  c_duckdb_scalar_function_set_function :: DuckDBScalarFunction -> DuckDBScalarFunctionFun -> IO ()

-- | Register the scalar function object within the given connection.
--
-- The function requires at least a name, a function and a return type.
--
-- If the function is incomplete or a function with this name already exists
-- DuckDBError is returned.
--
-- Parameters:
-- * @con@: The connection to register it in.
-- * @scalar_function@: The function pointer
--
-- Returns Whether or not the registration was successful.
foreign import ccall unsafe "duckdb_register_scalar_function"
  c_duckdb_register_scalar_function :: DuckDBConnection -> DuckDBScalarFunction -> IO DuckDBState

-- | Retrieves the extra info of the function as set in
-- @duckdb_scalar_function_set_extra_info@.
--
-- Parameters:
-- * @info@: The info object.
--
-- Returns The extra info.
foreign import ccall unsafe "duckdb_scalar_function_get_extra_info"
  c_duckdb_scalar_function_get_extra_info :: DuckDBFunctionInfo -> IO (Ptr ())

-- | Retrieves the extra info of the function as set in the bind info.
--
-- Parameters:
-- * @info@: The info object.
--
-- Returns The extra info.
foreign import ccall unsafe "duckdb_scalar_function_bind_get_extra_info"
  c_duckdb_scalar_function_bind_get_extra_info :: DuckDBBindInfo -> IO (Ptr ())

-- | Gets the scalar function's bind data set by
-- @duckdb_scalar_function_set_bind_data@. Note that the bind data is read-only.
--
-- Parameters:
-- * @info@: The function info.
--
-- Returns The bind data object.
foreign import ccall unsafe "duckdb_scalar_function_get_bind_data"
  c_duckdb_scalar_function_get_bind_data :: DuckDBFunctionInfo -> IO (Ptr ())

-- | Retrieves the client context of the bind info of a scalar function.
--
-- Parameters:
-- * @info@: The bind info object of the scalar function.
-- * @out_context@: The client context of the bind info. Must be destroyed with
--   @duckdb_destroy_client_context@.
foreign import ccall unsafe "duckdb_scalar_function_get_client_context"
  c_duckdb_scalar_function_get_client_context :: DuckDBBindInfo -> Ptr DuckDBClientContext -> IO ()

-- | Report that an error has occurred while executing the scalar function.
--
-- Parameters:
-- * @info@: The info object.
-- * @error@: The error message
foreign import ccall unsafe "duckdb_scalar_function_set_error"
  c_duckdb_scalar_function_set_error :: DuckDBFunctionInfo -> CString -> IO ()

-- | Creates a new empty scalar function set.
--
-- The return value must be destroyed with @duckdb_destroy_scalar_function_set@.
--
-- Returns The scalar function set object.
foreign import ccall unsafe "duckdb_create_scalar_function_set"
  c_duckdb_create_scalar_function_set :: CString -> IO DuckDBScalarFunctionSet

-- | Destroys the given scalar function set object.
foreign import ccall unsafe "duckdb_destroy_scalar_function_set"
  c_duckdb_destroy_scalar_function_set :: Ptr DuckDBScalarFunctionSet -> IO ()

-- | Adds the scalar function as a new overload to the scalar function set.
--
-- Returns DuckDBError if the function could not be added, for example if the
-- overload already exists.
--
-- Parameters:
-- * @set@: The scalar function set
-- * @function@: The function to add
foreign import ccall unsafe "duckdb_add_scalar_function_to_set"
  c_duckdb_add_scalar_function_to_set :: DuckDBScalarFunctionSet -> DuckDBScalarFunction -> IO DuckDBState

-- | Register the scalar function set within the given connection.
--
-- The set requires at least a single valid overload.
--
-- If the set is incomplete or a function with this name already exists
-- DuckDBError is returned.
--
-- Parameters:
-- * @con@: The connection to register it in.
-- * @set@: The function set to register
--
-- Returns Whether or not the registration was successful.
foreign import ccall unsafe "duckdb_register_scalar_function_set"
  c_duckdb_register_scalar_function_set :: DuckDBConnection -> DuckDBScalarFunctionSet -> IO DuckDBState

-- | Returns the number of input arguments of the scalar function.
--
-- Parameters:
-- * @info@: The bind info.
--
-- Returns The number of input arguments.
foreign import ccall unsafe "duckdb_scalar_function_bind_get_argument_count"
  c_duckdb_scalar_function_bind_get_argument_count :: DuckDBBindInfo -> IO DuckDBIdx

-- | Returns the input argument at index of the scalar function.
--
-- Parameters:
-- * @info@: The bind info.
-- * @index@: The argument index.
--
-- Returns The input argument at index. Must be destroyed with
-- @duckdb_destroy_expression@.
foreign import ccall unsafe "duckdb_scalar_function_bind_get_argument"
  c_duckdb_scalar_function_bind_get_argument :: DuckDBBindInfo -> DuckDBIdx -> IO DuckDBExpression
