module Database.DuckDB.FFI.AggregateFunctions (
    c_duckdb_create_aggregate_function,
    c_duckdb_destroy_aggregate_function,
    c_duckdb_aggregate_function_set_name,
    c_duckdb_aggregate_function_add_parameter,
    c_duckdb_aggregate_function_set_return_type,
    c_duckdb_aggregate_function_set_functions,
    c_duckdb_aggregate_function_set_destructor,
    c_duckdb_register_aggregate_function,
    c_duckdb_aggregate_function_set_special_handling,
    c_duckdb_aggregate_function_set_extra_info,
    c_duckdb_aggregate_function_get_extra_info,
    c_duckdb_aggregate_function_set_error,
    c_duckdb_create_aggregate_function_set,
    c_duckdb_destroy_aggregate_function_set,
    c_duckdb_add_aggregate_function_to_set,
    c_duckdb_register_aggregate_function_set,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

{- | Creates a new empty aggregate function.

The return value should be destroyed with @duckdb_destroy_aggregate_function@.

Returns The aggregate function object.
-}
foreign import ccall safe "duckdb_create_aggregate_function"
    c_duckdb_create_aggregate_function :: IO DuckDBAggregateFunction

-- | Destroys the given aggregate function object.
foreign import ccall safe "duckdb_destroy_aggregate_function"
    c_duckdb_destroy_aggregate_function :: Ptr DuckDBAggregateFunction -> IO ()

{- | Sets the name of the given aggregate function.

Parameters:
* @aggregate_function@: The aggregate function
* @name@: The name of the aggregate function
-}
foreign import ccall safe "duckdb_aggregate_function_set_name"
    c_duckdb_aggregate_function_set_name :: DuckDBAggregateFunction -> CString -> IO ()

{- | Adds a parameter to the aggregate function.

Parameters:
* @aggregate_function@: The aggregate function.
* @type@: The parameter type. Cannot contain INVALID.
-}
foreign import ccall safe "duckdb_aggregate_function_add_parameter"
    c_duckdb_aggregate_function_add_parameter :: DuckDBAggregateFunction -> DuckDBLogicalType -> IO ()

{- | Sets the return type of the aggregate function.

Parameters:
* @aggregate_function@: The aggregate function.
* @type@: The return type. Cannot contain INVALID or ANY.
-}
foreign import ccall safe "duckdb_aggregate_function_set_return_type"
    c_duckdb_aggregate_function_set_return_type :: DuckDBAggregateFunction -> DuckDBLogicalType -> IO ()

{- | Sets the main functions of the aggregate function.

Parameters:
* @aggregate_function@: The aggregate function
* @state_size@: state size
* @state_init@: state init function
* @update@: update states
* @combine@: combine states
* @finalize@: finalize states
-}
foreign import ccall safe "duckdb_aggregate_function_set_functions"
    c_duckdb_aggregate_function_set_functions :: DuckDBAggregateFunction -> DuckDBAggregateStateSizeFun -> DuckDBAggregateInitFun -> DuckDBAggregateUpdateFun -> DuckDBAggregateCombineFun -> DuckDBAggregateFinalizeFun -> IO ()

{- | Sets the state destructor callback of the aggregate function (optional)

Parameters:
* @aggregate_function@: The aggregate function
* @destroy@: state destroy callback
-}
foreign import ccall safe "duckdb_aggregate_function_set_destructor"
    c_duckdb_aggregate_function_set_destructor :: DuckDBAggregateFunction -> DuckDBAggregateDestroyFun -> IO ()

{- | Register the aggregate function object within the given connection.

The function requires at least a name, functions and a return type.

If the function is incomplete or a function with this name already exists
DuckDBError is returned.

Parameters:
* @con@: The connection to register it in.

Returns Whether or not the registration was successful.
-}
foreign import ccall safe "duckdb_register_aggregate_function"
    c_duckdb_register_aggregate_function :: DuckDBConnection -> DuckDBAggregateFunction -> IO DuckDBState

{- | Sets the NULL handling of the aggregate function to SPECIAL_HANDLING.

Parameters:
* @aggregate_function@: The aggregate function
-}
foreign import ccall safe "duckdb_aggregate_function_set_special_handling"
    c_duckdb_aggregate_function_set_special_handling :: DuckDBAggregateFunction -> IO ()

{- | Assigns extra information to the scalar function that can be fetched during
binding, etc.

Parameters:
* @aggregate_function@: The aggregate function
* @extra_info@: The extra information
* @destroy@: The callback that will be called to destroy the extra information
  (if any)
-}
foreign import ccall safe "duckdb_aggregate_function_set_extra_info"
    c_duckdb_aggregate_function_set_extra_info :: DuckDBAggregateFunction -> Ptr () -> DuckDBDeleteCallback -> IO ()

{- | Retrieves the extra info of the function as set in
@duckdb_aggregate_function_set_extra_info@.

Parameters:
* @info@: The info object

Returns The extra info
-}
foreign import ccall safe "duckdb_aggregate_function_get_extra_info"
    c_duckdb_aggregate_function_get_extra_info :: DuckDBFunctionInfo -> IO (Ptr ())

{- | Report that an error has occurred while executing the aggregate function.

Parameters:
* @info@: The info object
* @error@: The error message
-}
foreign import ccall safe "duckdb_aggregate_function_set_error"
    c_duckdb_aggregate_function_set_error :: DuckDBFunctionInfo -> CString -> IO ()

{- | Creates a new empty aggregate function set.

The return value should be destroyed with
@duckdb_destroy_aggregate_function_set@.

Returns The aggregate function set object.
-}
foreign import ccall safe "duckdb_create_aggregate_function_set"
    c_duckdb_create_aggregate_function_set :: CString -> IO DuckDBAggregateFunctionSet

-- | Destroys the given aggregate function set object.
foreign import ccall safe "duckdb_destroy_aggregate_function_set"
    c_duckdb_destroy_aggregate_function_set :: Ptr DuckDBAggregateFunctionSet -> IO ()

{- | Adds the aggregate function as a new overload to the aggregate function set.

Returns DuckDBError if the function could not be added, for example if the
overload already exists.

Parameters:
* @set@: The aggregate function set
* @function@: The function to add
-}
foreign import ccall safe "duckdb_add_aggregate_function_to_set"
    c_duckdb_add_aggregate_function_to_set :: DuckDBAggregateFunctionSet -> DuckDBAggregateFunction -> IO DuckDBState

{- | Register the aggregate function set within the given connection.

The set requires at least a single valid overload.

If the set is incomplete or a function with this name already exists
DuckDBError is returned.

Parameters:
* @con@: The connection to register it in.
* @set@: The function set to register

Returns Whether or not the registration was successful.
-}
foreign import ccall safe "duckdb_register_aggregate_function_set"
    c_duckdb_register_aggregate_function_set :: DuckDBConnection -> DuckDBAggregateFunctionSet -> IO DuckDBState
