module Database.DuckDB.FFI.OpenConnect (
    c_duckdb_create_instance_cache,
    c_duckdb_get_or_create_from_cache,
    c_duckdb_destroy_instance_cache,
    c_duckdb_open,
    c_duckdb_open_ext,
    c_duckdb_close,
    c_duckdb_connect,
    c_duckdb_interrupt,
    c_duckdb_query_progress,
    c_duckdb_disconnect,
    c_duckdb_connection_get_client_context,
    c_duckdb_connection_get_arrow_options,
    c_duckdb_client_context_get_connection_id,
    c_duckdb_destroy_client_context,
    c_duckdb_destroy_arrow_options,
    c_duckdb_library_version,
    c_duckdb_get_table_names,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)

{- | Creates a new database instance cache. The instance cache is necessary if a
client/program (re)opens multiple databases to the same file within the same
process. Must be destroyed with @duckdb_destroy_instance_cache@.

Returns The database instance cache.
-}
foreign import ccall safe "duckdb_create_instance_cache"
    c_duckdb_create_instance_cache :: IO DuckDBInstanceCache

{- | Creates a new database instance in the instance cache, or retrieves an
existing database instance. Must be closed with @duckdb_close@.

Parameters:
* @instance_cache@: The instance cache in which to create the database, or
  from which to take the database.
* @path@: Path to the database file on disk. Both @nullptr@ and @:memory:@
  open or retrieve an in-memory database.
* @out_database@: The resulting cached database.
* @config@: (Optional) configuration used to create the database.
* @out_error@: If set and the function returns @DuckDBError@, this contains
  the error message. Note that the error message must be freed using
  @duckdb_free@.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall "duckdb_get_or_create_from_cache"
    c_duckdb_get_or_create_from_cache :: DuckDBInstanceCache -> CString -> Ptr DuckDBDatabase -> DuckDBConfig -> Ptr CString -> IO DuckDBState

{- | Destroys an existing database instance cache and de-allocates its memory.

Parameters:
* @instance_cache@: The instance cache to destroy.
-}
foreign import ccall safe "duckdb_destroy_instance_cache"
    c_duckdb_destroy_instance_cache :: Ptr DuckDBInstanceCache -> IO ()

{- | Creates a new database or opens an existing database file stored at the given
path. If no path is given a new in-memory database is created instead. The
database must be closed with @duckdb_close@.

Parameters:
* @path@: Path to the database file on disk. Both @nullptr@ and @:memory:@
  open an in-memory database.
* @out_database@: The result database object.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall "duckdb_open"
    c_duckdb_open :: CString -> Ptr DuckDBDatabase -> IO DuckDBState

{- | Extended version of duckdb_open. Creates a new database or opens an existing
database file stored at the given path. The database must be closed with
@duckdb_close@.

Parameters:
* @path@: Path to the database file on disk. Both @nullptr@ and @:memory:@
  open an in-memory database.
* @out_database@: The result database object.
* @config@: (Optional) configuration used to start up the database.
* @out_error@: If set and the function returns @DuckDBError@, this contains
  the error message. Note that the error message must be freed using
  @duckdb_free@.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall "duckdb_open_ext"
    c_duckdb_open_ext :: CString -> Ptr DuckDBDatabase -> DuckDBConfig -> Ptr CString -> IO DuckDBState

{- | Closes the specified database and de-allocates all memory allocated for that
database. This should be called after you are done with any database allocated
through @duckdb_open@ or @duckdb_open_ext@. Note that failing to call
@duckdb_close@ (in case of e.g., a program crash) will not cause data
corruption. Still, it is recommended to always correctly close a database
object after you are done with it.

Parameters:
* @database@: The database object to shut down.
-}
foreign import ccall "duckdb_close"
    c_duckdb_close :: Ptr DuckDBDatabase -> IO ()

{- | Opens a connection to a database. Connections are required to query the
database, and store transactional state associated with the connection. The
instantiated connection should be closed using @duckdb_disconnect@.

Parameters:
* @database@: The database file to connect to.
* @out_connection@: The result connection object.

Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
-}
foreign import ccall "duckdb_connect"
    c_duckdb_connect :: DuckDBDatabase -> Ptr DuckDBConnection -> IO DuckDBState

{- | Interrupt running query

Parameters:
* @connection@: The connection to interrupt
-}
foreign import ccall safe "duckdb_interrupt"
    c_duckdb_interrupt :: DuckDBConnection -> IO ()

{- | Get progress of the running query

Parameters:
* @connection@: The working connection

Returns -1 if no progress or a percentage of the progress

These bindings call the wrapper symbol @wrapped_duckdb_query_progress@
but mirror the DuckDB C API semantics of @duckdb_query_progress@.
-}
foreign import ccall safe "wrapped_duckdb_query_progress"
    c_duckdb_query_progress :: DuckDBConnection -> Ptr DuckDBQueryProgress -> IO ()

{- | Closes the specified connection and de-allocates all memory allocated for that
connection.

Parameters:
* @connection@: The connection to close.
-}
foreign import ccall "duckdb_disconnect"
    c_duckdb_disconnect :: Ptr DuckDBConnection -> IO ()

{- | Retrieves the client context of the connection.

Parameters:
* @connection@: The connection.
* @out_context@: The client context of the connection. Must be destroyed with
  @duckdb_destroy_client_context@.
-}
foreign import ccall safe "duckdb_connection_get_client_context"
    c_duckdb_connection_get_client_context :: DuckDBConnection -> Ptr DuckDBClientContext -> IO ()

{- | Retrieves the arrow options of the connection.

Parameters:
* @connection@: The connection.
-}
foreign import ccall safe "duckdb_connection_get_arrow_options"
    c_duckdb_connection_get_arrow_options :: DuckDBConnection -> Ptr DuckDBArrowOptions -> IO ()

{- | Returns the connection id of the client context.

Parameters:
* @context@: The client context.

Returns The connection id of the client context.
-}
foreign import ccall safe "duckdb_client_context_get_connection_id"
    c_duckdb_client_context_get_connection_id :: DuckDBClientContext -> IO DuckDBIdx

{- | Destroys the client context and deallocates its memory.

Parameters:
* @context@: The client context to destroy.
-}
foreign import ccall safe "duckdb_destroy_client_context"
    c_duckdb_destroy_client_context :: Ptr DuckDBClientContext -> IO ()

{- | Destroys the arrow options and deallocates its memory.

Parameters:
* @arrow_options@: The arrow options to destroy.
-}
foreign import ccall safe "duckdb_destroy_arrow_options"
    c_duckdb_destroy_arrow_options :: Ptr DuckDBArrowOptions -> IO ()

{- | Returns the version of the linked DuckDB, with a version postfix for dev
versions

Usually used for developing C extensions that must return this for a
compatibility check.
-}
foreign import ccall safe "duckdb_library_version"
    c_duckdb_library_version :: IO CString

{- | Get the list of (fully qualified) table names of the query.

Parameters:
* @connection@: The connection for which to get the table names.
* @query@: The query for which to get the table names.
* @qualified@: Returns fully qualified table names (catalog.schema.table), if
  set to true, else only the (not escaped) table names.

Returns A duckdb_value of type VARCHAR[] containing the (fully qualified)
table names of the query. Must be destroyed with duckdb_destroy_value.
-}
foreign import ccall "duckdb_get_table_names"
    c_duckdb_get_table_names :: DuckDBConnection -> CString -> CBool -> IO DuckDBValue
