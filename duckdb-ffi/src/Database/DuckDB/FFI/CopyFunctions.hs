{- |
Module      : Database.DuckDB.FFI.CopyFunctions
Description : Raw bindings for DuckDB's custom COPY-function API.
-}
module Database.DuckDB.FFI.CopyFunctions (
    c_duckdb_create_copy_function,
    c_duckdb_copy_function_set_name,
    c_duckdb_copy_function_set_extra_info,
    c_duckdb_register_copy_function,
    c_duckdb_destroy_copy_function,
    c_duckdb_copy_function_set_bind,
    c_duckdb_copy_function_bind_set_error,
    c_duckdb_copy_function_bind_get_extra_info,
    c_duckdb_copy_function_bind_get_client_context,
    c_duckdb_copy_function_bind_get_column_count,
    c_duckdb_copy_function_bind_get_column_type,
    c_duckdb_copy_function_bind_get_options,
    c_duckdb_copy_function_bind_set_bind_data,
    c_duckdb_copy_function_set_global_init,
    c_duckdb_copy_function_global_init_set_error,
    c_duckdb_copy_function_global_init_get_extra_info,
    c_duckdb_copy_function_global_init_get_client_context,
    c_duckdb_copy_function_global_init_get_bind_data,
    c_duckdb_copy_function_global_init_get_file_path,
    c_duckdb_copy_function_global_init_set_global_state,
    c_duckdb_copy_function_set_sink,
    c_duckdb_copy_function_sink_set_error,
    c_duckdb_copy_function_sink_get_extra_info,
    c_duckdb_copy_function_sink_get_client_context,
    c_duckdb_copy_function_sink_get_bind_data,
    c_duckdb_copy_function_sink_get_global_state,
    c_duckdb_copy_function_set_finalize,
    c_duckdb_copy_function_finalize_set_error,
    c_duckdb_copy_function_finalize_get_extra_info,
    c_duckdb_copy_function_finalize_get_client_context,
    c_duckdb_copy_function_finalize_get_bind_data,
    c_duckdb_copy_function_finalize_get_global_state,
    c_duckdb_copy_function_set_copy_from_function,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

-- | Allocate a new copy-function object.
foreign import ccall safe "duckdb_create_copy_function"
    c_duckdb_create_copy_function :: IO DuckDBCopyFunction

-- | Set the registration name of a copy function.
foreign import ccall safe "duckdb_copy_function_set_name"
    c_duckdb_copy_function_set_name :: DuckDBCopyFunction -> CString -> IO ()

-- | Attach user-managed extra data to a copy function.
foreign import ccall safe "duckdb_copy_function_set_extra_info"
    c_duckdb_copy_function_set_extra_info :: DuckDBCopyFunction -> Ptr () -> DuckDBDeleteCallback -> IO ()

-- | Register a copy function on a connection.
foreign import ccall safe "duckdb_register_copy_function"
    c_duckdb_register_copy_function :: DuckDBConnection -> DuckDBCopyFunction -> IO DuckDBState

-- | Destroy a copy-function object.
foreign import ccall safe "duckdb_destroy_copy_function"
    c_duckdb_destroy_copy_function :: Ptr DuckDBCopyFunction -> IO ()

-- | Set the bind callback for `COPY ... TO`.
foreign import ccall safe "duckdb_copy_function_set_bind"
    c_duckdb_copy_function_set_bind :: DuckDBCopyFunction -> DuckDBCopyFunctionBindFun -> IO ()

-- | Report a bind-phase error from a copy callback.
foreign import ccall safe "duckdb_copy_function_bind_set_error"
    c_duckdb_copy_function_bind_set_error :: DuckDBCopyFunctionBindInfo -> CString -> IO ()

-- | Fetch the copy function's extra info during bind.
foreign import ccall safe "duckdb_copy_function_bind_get_extra_info"
    c_duckdb_copy_function_bind_get_extra_info :: DuckDBCopyFunctionBindInfo -> IO (Ptr ())

-- | Fetch the current client context during bind.
foreign import ccall safe "duckdb_copy_function_bind_get_client_context"
    c_duckdb_copy_function_bind_get_client_context :: DuckDBCopyFunctionBindInfo -> IO DuckDBClientContext

-- | Fetch the number of columns supplied to `COPY ... TO`.
foreign import ccall safe "duckdb_copy_function_bind_get_column_count"
    c_duckdb_copy_function_bind_get_column_count :: DuckDBCopyFunctionBindInfo -> IO DuckDBIdx

-- | Fetch the logical type of a `COPY ... TO` output column.
foreign import ccall safe "duckdb_copy_function_bind_get_column_type"
    c_duckdb_copy_function_bind_get_column_type :: DuckDBCopyFunctionBindInfo -> DuckDBIdx -> IO DuckDBLogicalType

-- | Fetch the options struct supplied to `COPY ... TO`.
foreign import ccall safe "duckdb_copy_function_bind_get_options"
    c_duckdb_copy_function_bind_get_options :: DuckDBCopyFunctionBindInfo -> IO DuckDBValue

-- | Store bind-phase state for later copy callbacks.
foreign import ccall safe "duckdb_copy_function_bind_set_bind_data"
    c_duckdb_copy_function_bind_set_bind_data :: DuckDBCopyFunctionBindInfo -> Ptr () -> DuckDBDeleteCallback -> IO ()

-- | Set the global-init callback for `COPY ... TO`.
foreign import ccall safe "duckdb_copy_function_set_global_init"
    c_duckdb_copy_function_set_global_init :: DuckDBCopyFunction -> DuckDBCopyFunctionGlobalInitFun -> IO ()

-- | Report an init-phase error from a copy callback.
foreign import ccall safe "duckdb_copy_function_global_init_set_error"
    c_duckdb_copy_function_global_init_set_error :: DuckDBCopyFunctionGlobalInitInfo -> CString -> IO ()

-- | Fetch the copy function's extra info during global init.
foreign import ccall safe "duckdb_copy_function_global_init_get_extra_info"
    c_duckdb_copy_function_global_init_get_extra_info :: DuckDBCopyFunctionGlobalInitInfo -> IO (Ptr ())

-- | Fetch the current client context during global init.
foreign import ccall safe "duckdb_copy_function_global_init_get_client_context"
    c_duckdb_copy_function_global_init_get_client_context :: DuckDBCopyFunctionGlobalInitInfo -> IO DuckDBClientContext

-- | Fetch the bind-phase state during global init.
foreign import ccall safe "duckdb_copy_function_global_init_get_bind_data"
    c_duckdb_copy_function_global_init_get_bind_data :: DuckDBCopyFunctionGlobalInitInfo -> IO (Ptr ())

-- | Fetch the file path supplied to `COPY ... TO`.
foreign import ccall safe "duckdb_copy_function_global_init_get_file_path"
    c_duckdb_copy_function_global_init_get_file_path :: DuckDBCopyFunctionGlobalInitInfo -> IO CString

-- | Store global state for sink/finalize callbacks.
foreign import ccall safe "duckdb_copy_function_global_init_set_global_state"
    c_duckdb_copy_function_global_init_set_global_state :: DuckDBCopyFunctionGlobalInitInfo -> Ptr () -> DuckDBDeleteCallback -> IO ()

-- | Set the sink callback for `COPY ... TO`.
foreign import ccall safe "duckdb_copy_function_set_sink"
    c_duckdb_copy_function_set_sink :: DuckDBCopyFunction -> DuckDBCopyFunctionSinkFun -> IO ()

-- | Report a sink-phase error from a copy callback.
foreign import ccall safe "duckdb_copy_function_sink_set_error"
    c_duckdb_copy_function_sink_set_error :: DuckDBCopyFunctionSinkInfo -> CString -> IO ()

-- | Fetch the copy function's extra info during sink execution.
foreign import ccall safe "duckdb_copy_function_sink_get_extra_info"
    c_duckdb_copy_function_sink_get_extra_info :: DuckDBCopyFunctionSinkInfo -> IO (Ptr ())

-- | Fetch the current client context during sink execution.
foreign import ccall safe "duckdb_copy_function_sink_get_client_context"
    c_duckdb_copy_function_sink_get_client_context :: DuckDBCopyFunctionSinkInfo -> IO DuckDBClientContext

-- | Fetch bind-phase state during sink execution.
foreign import ccall safe "duckdb_copy_function_sink_get_bind_data"
    c_duckdb_copy_function_sink_get_bind_data :: DuckDBCopyFunctionSinkInfo -> IO (Ptr ())

-- | Fetch global-init state during sink execution.
foreign import ccall safe "duckdb_copy_function_sink_get_global_state"
    c_duckdb_copy_function_sink_get_global_state :: DuckDBCopyFunctionSinkInfo -> IO (Ptr ())

-- | Set the finalize callback for `COPY ... TO`.
foreign import ccall safe "duckdb_copy_function_set_finalize"
    c_duckdb_copy_function_set_finalize :: DuckDBCopyFunction -> DuckDBCopyFunctionFinalizeFun -> IO ()

-- | Report a finalize-phase error from a copy callback.
foreign import ccall safe "duckdb_copy_function_finalize_set_error"
    c_duckdb_copy_function_finalize_set_error :: DuckDBCopyFunctionFinalizeInfo -> CString -> IO ()

-- | Fetch the copy function's extra info during finalize.
foreign import ccall safe "duckdb_copy_function_finalize_get_extra_info"
    c_duckdb_copy_function_finalize_get_extra_info :: DuckDBCopyFunctionFinalizeInfo -> IO (Ptr ())

-- | Fetch the current client context during finalize.
foreign import ccall safe "duckdb_copy_function_finalize_get_client_context"
    c_duckdb_copy_function_finalize_get_client_context :: DuckDBCopyFunctionFinalizeInfo -> IO DuckDBClientContext

-- | Fetch bind-phase state during finalize.
foreign import ccall safe "duckdb_copy_function_finalize_get_bind_data"
    c_duckdb_copy_function_finalize_get_bind_data :: DuckDBCopyFunctionFinalizeInfo -> IO (Ptr ())

-- | Fetch global-init state during finalize.
foreign import ccall safe "duckdb_copy_function_finalize_get_global_state"
    c_duckdb_copy_function_finalize_get_global_state :: DuckDBCopyFunctionFinalizeInfo -> IO (Ptr ())

-- | Attach a table function used for `COPY ... FROM` with this format.
foreign import ccall safe "duckdb_copy_function_set_copy_from_function"
    c_duckdb_copy_function_set_copy_from_function :: DuckDBCopyFunction -> DuckDBTableFunction -> IO ()
