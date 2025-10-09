module Database.DuckDB.FFI.TableFunctions (
  -- Table function definition
  c_duckdb_create_table_function,
  c_duckdb_destroy_table_function,
  c_duckdb_table_function_set_name,
  c_duckdb_table_function_add_parameter,
  c_duckdb_table_function_add_named_parameter,
  c_duckdb_table_function_set_extra_info,
  c_duckdb_table_function_set_bind,
  c_duckdb_table_function_set_init,
  c_duckdb_table_function_set_local_init,
  c_duckdb_table_function_set_function,
  c_duckdb_table_function_supports_projection_pushdown,
  c_duckdb_register_table_function,
  -- Bind helpers
  c_duckdb_bind_get_extra_info,
  c_duckdb_table_function_get_client_context,
  c_duckdb_bind_add_result_column,
  c_duckdb_bind_get_parameter_count,
  c_duckdb_bind_get_parameter,
  c_duckdb_bind_get_named_parameter,
  c_duckdb_bind_set_bind_data,
  c_duckdb_bind_set_cardinality,
  c_duckdb_bind_set_error,
  -- Init helpers
  c_duckdb_init_get_extra_info,
  c_duckdb_init_get_bind_data,
  c_duckdb_init_set_init_data,
  c_duckdb_init_get_column_count,
  c_duckdb_init_get_column_index,
  c_duckdb_init_set_max_threads,
  c_duckdb_init_set_error,
  -- Execution helpers
  c_duckdb_function_get_extra_info,
  c_duckdb_function_get_bind_data,
  c_duckdb_function_get_init_data,
  c_duckdb_function_get_local_init_data,
  c_duckdb_function_set_error
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)

-- | Creates a new empty table function.
--
-- The return value should be destroyed with @duckdb_destroy_table_function@.
--
-- Returns The table function object.
foreign import ccall unsafe "duckdb_create_table_function"
  c_duckdb_create_table_function :: IO DuckDBTableFunction

-- | Destroys the given table function object.
--
-- Parameters:
-- * @table_function@: The table function to destroy
foreign import ccall unsafe "duckdb_destroy_table_function"
  c_duckdb_destroy_table_function :: Ptr DuckDBTableFunction -> IO ()

-- | Sets the name of the given table function.
--
-- Parameters:
-- * @table_function@: The table function
-- * @name@: The name of the table function
foreign import ccall unsafe "duckdb_table_function_set_name"
  c_duckdb_table_function_set_name :: DuckDBTableFunction -> CString -> IO ()

-- | Adds a parameter to the table function.
--
-- Parameters:
-- * @table_function@: The table function.
-- * @type@: The parameter type. Cannot contain INVALID.
foreign import ccall unsafe "duckdb_table_function_add_parameter"
  c_duckdb_table_function_add_parameter :: DuckDBTableFunction -> DuckDBLogicalType -> IO ()

-- | Adds a named parameter to the table function.
--
-- Parameters:
-- * @table_function@: The table function.
-- * @name@: The parameter name.
-- * @type@: The parameter type. Cannot contain INVALID.
foreign import ccall unsafe "duckdb_table_function_add_named_parameter"
  c_duckdb_table_function_add_named_parameter :: DuckDBTableFunction -> CString -> DuckDBLogicalType -> IO ()

-- | Assigns extra information to the table function that can be fetched during
-- binding, etc.
--
-- Parameters:
-- * @table_function@: The table function
-- * @extra_info@: The extra information
-- * @destroy@: The callback that will be called to destroy the extra information
--   (if any)
foreign import ccall unsafe "duckdb_table_function_set_extra_info"
  c_duckdb_table_function_set_extra_info :: DuckDBTableFunction -> Ptr () -> DuckDBDeleteCallback -> IO ()

-- | Sets the bind function of the table function.
--
-- Parameters:
-- * @table_function@: The table function
-- * @bind@: The bind function
foreign import ccall unsafe "duckdb_table_function_set_bind"
  c_duckdb_table_function_set_bind :: DuckDBTableFunction -> DuckDBTableFunctionBindFun -> IO ()

-- | Sets the init function of the table function.
--
-- Parameters:
-- * @table_function@: The table function
-- * @init@: The init function
foreign import ccall unsafe "duckdb_table_function_set_init"
  c_duckdb_table_function_set_init :: DuckDBTableFunction -> DuckDBTableFunctionInitFun -> IO ()

-- | Sets the thread-local init function of the table function.
--
-- Parameters:
-- * @table_function@: The table function
-- * @init@: The init function
foreign import ccall unsafe "duckdb_table_function_set_local_init"
  c_duckdb_table_function_set_local_init :: DuckDBTableFunction -> DuckDBTableFunctionInitFun -> IO ()

-- | Sets the main function of the table function.
--
-- Parameters:
-- * @table_function@: The table function
-- * @function@: The function
foreign import ccall unsafe "duckdb_table_function_set_function"
  c_duckdb_table_function_set_function :: DuckDBTableFunction -> DuckDBTableFunctionFun -> IO ()

-- | Sets whether or not the given table function supports projection pushdown.
--
-- If this is set to true, the system will provide a list of all required columns
-- in the @init@ stage through the @duckdb_init_get_column_count@ and
-- @duckdb_init_get_column_index@ functions. If this is set to false (the
-- default), the system will expect all columns to be projected.
--
-- Parameters:
-- * @table_function@: The table function
-- * @pushdown@: True if the table function supports projection pushdown, false
--   otherwise.
foreign import ccall unsafe "duckdb_table_function_supports_projection_pushdown"
  c_duckdb_table_function_supports_projection_pushdown :: DuckDBTableFunction -> CBool -> IO ()

-- | Register the table function object within the given connection.
--
-- The function requires at least a name, a bind function, an init function and a
-- main function.
--
-- If the function is incomplete or a function with this name already exists
-- DuckDBError is returned.
--
-- Parameters:
-- * @con@: The connection to register it in.
-- * @function@: The function pointer
--
-- Returns Whether or not the registration was successful.
foreign import ccall unsafe "duckdb_register_table_function"
  c_duckdb_register_table_function :: DuckDBConnection -> DuckDBTableFunction -> IO DuckDBState

-- | Retrieves the extra info of the function as set in
-- @duckdb_table_function_set_extra_info@.
--
-- Parameters:
-- * @info@: The info object
--
-- Returns The extra info
foreign import ccall unsafe "duckdb_bind_get_extra_info"
  c_duckdb_bind_get_extra_info :: DuckDBBindInfo -> IO (Ptr ())

-- | Retrieves the client context of the bind info of a table function.
--
-- Parameters:
-- * @info@: The bind info object of the table function.
-- * @out_context@: The client context of the bind info. Must be destroyed with
--   @duckdb_destroy_client_context@.
foreign import ccall unsafe "duckdb_table_function_get_client_context"
  c_duckdb_table_function_get_client_context :: DuckDBBindInfo -> Ptr DuckDBClientContext -> IO ()

-- | Adds a result column to the output of the table function.
--
-- Parameters:
-- * @info@: The table function's bind info.
-- * @name@: The column name.
-- * @type@: The logical column type.
foreign import ccall safe "duckdb_bind_add_result_column"
  c_duckdb_bind_add_result_column :: DuckDBBindInfo -> CString -> DuckDBLogicalType -> IO ()

-- | Retrieves the number of regular (non-named) parameters to the function.
--
-- Parameters:
-- * @info@: The info object
--
-- Returns The number of parameters
foreign import ccall safe "duckdb_bind_get_parameter_count"
  c_duckdb_bind_get_parameter_count :: DuckDBBindInfo -> IO DuckDBIdx

-- | Retrieves the parameter at the given index.
--
-- The result must be destroyed with @duckdb_destroy_value@.
--
-- Parameters:
-- * @info@: The info object
-- * @index@: The index of the parameter to get
--
-- Returns The value of the parameter. Must be destroyed with
-- @duckdb_destroy_value@.
foreign import ccall safe "duckdb_bind_get_parameter"
  c_duckdb_bind_get_parameter :: DuckDBBindInfo -> DuckDBIdx -> IO DuckDBValue

-- | Retrieves a named parameter with the given name.
--
-- The result must be destroyed with @duckdb_destroy_value@.
--
-- Parameters:
-- * @info@: The info object
-- * @name@: The name of the parameter
--
-- Returns The value of the parameter. Must be destroyed with
-- @duckdb_destroy_value@.
foreign import ccall safe "duckdb_bind_get_named_parameter"
  c_duckdb_bind_get_named_parameter :: DuckDBBindInfo -> CString -> IO DuckDBValue

-- | Sets the user-provided bind data in the bind object of the table function.
-- This object can be retrieved again during execution.
--
-- Parameters:
-- * @info@: The bind info of the table function.
-- * @bind_data@: The bind data object.
-- * @destroy@: The callback to destroy the bind data (if any).
foreign import ccall safe "duckdb_bind_set_bind_data"
  c_duckdb_bind_set_bind_data :: DuckDBBindInfo -> Ptr () -> DuckDBDeleteCallback -> IO ()

-- | Sets the cardinality estimate for the table function, used for optimization.
--
-- Parameters:
-- * @info@: The bind data object.
-- * @is_exact@: Whether or not the cardinality estimate is exact, or an
--   approximation
foreign import ccall safe "duckdb_bind_set_cardinality"
  c_duckdb_bind_set_cardinality :: DuckDBBindInfo -> DuckDBIdx -> CBool -> IO ()

-- | Report that an error has occurred while calling bind on a table function.
--
-- Parameters:
-- * @info@: The info object
-- * @error@: The error message
foreign import ccall safe "duckdb_bind_set_error"
  c_duckdb_bind_set_error :: DuckDBBindInfo -> CString -> IO ()

-- | Retrieves the extra info of the function as set in
-- @duckdb_table_function_set_extra_info@.
--
-- Parameters:
-- * @info@: The info object
--
-- Returns The extra info
foreign import ccall unsafe "duckdb_init_get_extra_info"
  c_duckdb_init_get_extra_info :: DuckDBInitInfo -> IO (Ptr ())

-- | Gets the bind data set by @duckdb_bind_set_bind_data@ during the bind.
--
-- Note that the bind data should be considered as read-only. For tracking state,
-- use the init data instead.
--
-- Parameters:
-- * @info@: The info object
--
-- Returns The bind data object
foreign import ccall safe "duckdb_init_get_bind_data"
  c_duckdb_init_get_bind_data :: DuckDBInitInfo -> IO (Ptr ())

-- | Sets the user-provided init data in the init object. This object can be
-- retrieved again during execution.
--
-- Parameters:
-- * @info@: The info object
-- * @init_data@: The init data object.
-- * @destroy@: The callback that will be called to destroy the init data (if
--   any)
foreign import ccall safe "duckdb_init_set_init_data"
  c_duckdb_init_set_init_data :: DuckDBInitInfo -> Ptr () -> DuckDBDeleteCallback -> IO ()

-- | Returns the number of projected columns.
--
-- This function must be used if projection pushdown is enabled to figure out
-- which columns to emit.
--
-- Parameters:
-- * @info@: The info object
--
-- Returns The number of projected columns.
foreign import ccall unsafe "duckdb_init_get_column_count"
  c_duckdb_init_get_column_count :: DuckDBInitInfo -> IO DuckDBIdx

-- | Returns the column index of the projected column at the specified position.
--
-- This function must be used if projection pushdown is enabled to figure out
-- which columns to emit.
--
-- Parameters:
-- * @info@: The info object
-- * @column_index@: The index at which to get the projected column index, from
--   0..duckdb_init_get_column_count(info)
--
-- Returns The column index of the projected column.
foreign import ccall unsafe "duckdb_init_get_column_index"
  c_duckdb_init_get_column_index :: DuckDBInitInfo -> DuckDBIdx -> IO DuckDBIdx

-- | Sets how many threads can process this table function in parallel (default: 1)
--
-- Parameters:
-- * @info@: The info object
-- * @max_threads@: The maximum amount of threads that can process this table
--   function
foreign import ccall safe "duckdb_init_set_max_threads"
  c_duckdb_init_set_max_threads :: DuckDBInitInfo -> DuckDBIdx -> IO ()

-- | Report that an error has occurred while calling init.
--
-- Parameters:
-- * @info@: The info object
-- * @error@: The error message
foreign import ccall safe "duckdb_init_set_error"
  c_duckdb_init_set_error :: DuckDBInitInfo -> CString -> IO ()

-- | Retrieves the extra info of the function as set in
-- @duckdb_table_function_set_extra_info@.
--
-- Parameters:
-- * @info@: The info object
--
-- Returns The extra info
foreign import ccall unsafe "duckdb_function_get_extra_info"
  c_duckdb_function_get_extra_info :: DuckDBFunctionInfo -> IO (Ptr ())

-- | Gets the table function's bind data set by @duckdb_bind_set_bind_data@.
--
-- Note that the bind data is read-only. For tracking state, use the init data
-- instead.
--
-- Parameters:
-- * @info@: The function info object.
--
-- Returns The bind data object.
foreign import ccall unsafe "duckdb_function_get_bind_data"
  c_duckdb_function_get_bind_data :: DuckDBFunctionInfo -> IO (Ptr ())

-- | Gets the init data set by @duckdb_init_set_init_data@ during the init.
--
-- Parameters:
-- * @info@: The info object
--
-- Returns The init data object
foreign import ccall safe "duckdb_function_get_init_data"
  c_duckdb_function_get_init_data :: DuckDBFunctionInfo -> IO (Ptr ())

-- | Gets the thread-local init data set by @duckdb_init_set_init_data@ during the
-- local_init.
--
-- Parameters:
-- * @info@: The info object
--
-- Returns The init data object
foreign import ccall unsafe "duckdb_function_get_local_init_data"
  c_duckdb_function_get_local_init_data :: DuckDBFunctionInfo -> IO (Ptr ())

-- | Report that an error has occurred while executing the function.
--
-- Parameters:
-- * @info@: The info object
-- * @error@: The error message
foreign import ccall safe "duckdb_function_set_error"
  c_duckdb_function_set_error :: DuckDBFunctionInfo -> CString -> IO ()
