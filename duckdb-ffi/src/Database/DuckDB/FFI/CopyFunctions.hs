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

foreign import ccall safe "duckdb_create_copy_function"
    c_duckdb_create_copy_function :: IO DuckDBCopyFunction

foreign import ccall safe "duckdb_copy_function_set_name"
    c_duckdb_copy_function_set_name :: DuckDBCopyFunction -> CString -> IO ()

foreign import ccall safe "duckdb_copy_function_set_extra_info"
    c_duckdb_copy_function_set_extra_info :: DuckDBCopyFunction -> Ptr () -> DuckDBDeleteCallback -> IO ()

foreign import ccall safe "duckdb_register_copy_function"
    c_duckdb_register_copy_function :: DuckDBConnection -> DuckDBCopyFunction -> IO DuckDBState

foreign import ccall safe "duckdb_destroy_copy_function"
    c_duckdb_destroy_copy_function :: Ptr DuckDBCopyFunction -> IO ()

foreign import ccall safe "duckdb_copy_function_set_bind"
    c_duckdb_copy_function_set_bind :: DuckDBCopyFunction -> DuckDBCopyFunctionBindFun -> IO ()

foreign import ccall safe "duckdb_copy_function_bind_set_error"
    c_duckdb_copy_function_bind_set_error :: DuckDBCopyFunctionBindInfo -> CString -> IO ()

foreign import ccall safe "duckdb_copy_function_bind_get_extra_info"
    c_duckdb_copy_function_bind_get_extra_info :: DuckDBCopyFunctionBindInfo -> IO (Ptr ())

foreign import ccall safe "duckdb_copy_function_bind_get_client_context"
    c_duckdb_copy_function_bind_get_client_context :: DuckDBCopyFunctionBindInfo -> IO DuckDBClientContext

foreign import ccall safe "duckdb_copy_function_bind_get_column_count"
    c_duckdb_copy_function_bind_get_column_count :: DuckDBCopyFunctionBindInfo -> IO DuckDBIdx

foreign import ccall safe "duckdb_copy_function_bind_get_column_type"
    c_duckdb_copy_function_bind_get_column_type :: DuckDBCopyFunctionBindInfo -> DuckDBIdx -> IO DuckDBLogicalType

foreign import ccall safe "duckdb_copy_function_bind_get_options"
    c_duckdb_copy_function_bind_get_options :: DuckDBCopyFunctionBindInfo -> IO DuckDBValue

foreign import ccall safe "duckdb_copy_function_bind_set_bind_data"
    c_duckdb_copy_function_bind_set_bind_data :: DuckDBCopyFunctionBindInfo -> Ptr () -> DuckDBDeleteCallback -> IO ()

foreign import ccall safe "duckdb_copy_function_set_global_init"
    c_duckdb_copy_function_set_global_init :: DuckDBCopyFunction -> DuckDBCopyFunctionGlobalInitFun -> IO ()

foreign import ccall safe "duckdb_copy_function_global_init_set_error"
    c_duckdb_copy_function_global_init_set_error :: DuckDBCopyFunctionGlobalInitInfo -> CString -> IO ()

foreign import ccall safe "duckdb_copy_function_global_init_get_extra_info"
    c_duckdb_copy_function_global_init_get_extra_info :: DuckDBCopyFunctionGlobalInitInfo -> IO (Ptr ())

foreign import ccall safe "duckdb_copy_function_global_init_get_client_context"
    c_duckdb_copy_function_global_init_get_client_context :: DuckDBCopyFunctionGlobalInitInfo -> IO DuckDBClientContext

foreign import ccall safe "duckdb_copy_function_global_init_get_bind_data"
    c_duckdb_copy_function_global_init_get_bind_data :: DuckDBCopyFunctionGlobalInitInfo -> IO (Ptr ())

foreign import ccall safe "duckdb_copy_function_global_init_get_file_path"
    c_duckdb_copy_function_global_init_get_file_path :: DuckDBCopyFunctionGlobalInitInfo -> IO CString

foreign import ccall safe "duckdb_copy_function_global_init_set_global_state"
    c_duckdb_copy_function_global_init_set_global_state :: DuckDBCopyFunctionGlobalInitInfo -> Ptr () -> DuckDBDeleteCallback -> IO ()

foreign import ccall safe "duckdb_copy_function_set_sink"
    c_duckdb_copy_function_set_sink :: DuckDBCopyFunction -> DuckDBCopyFunctionSinkFun -> IO ()

foreign import ccall safe "duckdb_copy_function_sink_set_error"
    c_duckdb_copy_function_sink_set_error :: DuckDBCopyFunctionSinkInfo -> CString -> IO ()

foreign import ccall safe "duckdb_copy_function_sink_get_extra_info"
    c_duckdb_copy_function_sink_get_extra_info :: DuckDBCopyFunctionSinkInfo -> IO (Ptr ())

foreign import ccall safe "duckdb_copy_function_sink_get_client_context"
    c_duckdb_copy_function_sink_get_client_context :: DuckDBCopyFunctionSinkInfo -> IO DuckDBClientContext

foreign import ccall safe "duckdb_copy_function_sink_get_bind_data"
    c_duckdb_copy_function_sink_get_bind_data :: DuckDBCopyFunctionSinkInfo -> IO (Ptr ())

foreign import ccall safe "duckdb_copy_function_sink_get_global_state"
    c_duckdb_copy_function_sink_get_global_state :: DuckDBCopyFunctionSinkInfo -> IO (Ptr ())

foreign import ccall safe "duckdb_copy_function_set_finalize"
    c_duckdb_copy_function_set_finalize :: DuckDBCopyFunction -> DuckDBCopyFunctionFinalizeFun -> IO ()

foreign import ccall safe "duckdb_copy_function_finalize_set_error"
    c_duckdb_copy_function_finalize_set_error :: DuckDBCopyFunctionFinalizeInfo -> CString -> IO ()

foreign import ccall safe "duckdb_copy_function_finalize_get_extra_info"
    c_duckdb_copy_function_finalize_get_extra_info :: DuckDBCopyFunctionFinalizeInfo -> IO (Ptr ())

foreign import ccall safe "duckdb_copy_function_finalize_get_client_context"
    c_duckdb_copy_function_finalize_get_client_context :: DuckDBCopyFunctionFinalizeInfo -> IO DuckDBClientContext

foreign import ccall safe "duckdb_copy_function_finalize_get_bind_data"
    c_duckdb_copy_function_finalize_get_bind_data :: DuckDBCopyFunctionFinalizeInfo -> IO (Ptr ())

foreign import ccall safe "duckdb_copy_function_finalize_get_global_state"
    c_duckdb_copy_function_finalize_get_global_state :: DuckDBCopyFunctionFinalizeInfo -> IO (Ptr ())

foreign import ccall safe "duckdb_copy_function_set_copy_from_function"
    c_duckdb_copy_function_set_copy_from_function :: DuckDBCopyFunction -> DuckDBTableFunction -> IO ()
