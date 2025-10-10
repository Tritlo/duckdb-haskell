module Database.DuckDB.FFI.Configuration (
    c_duckdb_create_config,
    c_duckdb_config_count,
    c_duckdb_get_config_flag,
    c_duckdb_set_config,
    c_duckdb_destroy_config,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)

{- | Initializes an empty configuration object that can be used to provide start-up
options for the DuckDB instance through @duckdb_open_ext@. The duckdb_config
must be destroyed using @duckdb_destroy_config@

This will always succeed unless there is a malloc failure.

Note that @duckdb_destroy_config@ should always be called on the resulting
config, even if the function returns @DuckDBError@.

Parameters:
* @out_config@: The result configuration object.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall unsafe "duckdb_create_config"
    c_duckdb_create_config :: Ptr DuckDBConfig -> IO DuckDBState

{- | This returns the total amount of configuration options available for usage
with @duckdb_get_config_flag@.

This should not be called in a loop as it internally loops over all the
options.

Returns The amount of config options available.
-}
foreign import ccall unsafe "duckdb_config_count"
    c_duckdb_config_count :: IO CSize

{- | Obtains a human-readable name and description of a specific configuration
option. This can be used to e.g. display configuration options. This will
succeed unless @index@ is out of range (i.e., @>= duckdb_config_count@).

The result name or description MUST NOT be freed.

Parameters:
* @index@: The index of the configuration option (between 0 and
  @duckdb_config_count@)
* @out_name@: A name of the configuration flag.
* @out_description@: A description of the configuration flag.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall unsafe "duckdb_get_config_flag"
    c_duckdb_get_config_flag :: CSize -> Ptr CString -> Ptr CString -> IO DuckDBState

{- | Sets the specified option for the specified configuration. The configuration
option is indicated by name. To obtain a list of config options, see
@duckdb_get_config_flag@.

In the source code, configuration options are defined in @config.cpp@.

This can fail if either the name is invalid, or if the value provided for the
option is invalid.

Parameters:
* @config@: The configuration object to set the option on.
* @name@: The name of the configuration flag to set.
* @option@: The value to set the configuration flag to.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall unsafe "duckdb_set_config"
    c_duckdb_set_config :: DuckDBConfig -> CString -> CString -> IO DuckDBState

{- | Destroys the specified configuration object and de-allocates all memory
allocated for the object.

Parameters:
* @config@: The configuration object to destroy.
-}
foreign import ccall unsafe "duckdb_destroy_config"
    c_duckdb_destroy_config :: Ptr DuckDBConfig -> IO ()
