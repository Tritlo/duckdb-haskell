module Database.DuckDB.FFI.FileSystem (
    c_duckdb_client_context_get_file_system,
    c_duckdb_destroy_file_system,
    c_duckdb_file_system_error_data,
    c_duckdb_file_system_open,
    c_duckdb_create_file_open_options,
    c_duckdb_file_open_options_set_flag,
    c_duckdb_destroy_file_open_options,
    c_duckdb_destroy_file_handle,
    c_duckdb_file_handle_error_data,
    c_duckdb_file_handle_read,
    c_duckdb_file_handle_write,
    c_duckdb_file_handle_tell,
    c_duckdb_file_handle_size,
    c_duckdb_file_handle_seek,
    c_duckdb_file_handle_sync,
    c_duckdb_file_handle_close,
) where

import Data.Int (Int64)
import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr)

foreign import ccall safe "duckdb_client_context_get_file_system"
    c_duckdb_client_context_get_file_system :: DuckDBClientContext -> IO DuckDBFileSystem

foreign import ccall safe "duckdb_destroy_file_system"
    c_duckdb_destroy_file_system :: Ptr DuckDBFileSystem -> IO ()

foreign import ccall safe "duckdb_file_system_error_data"
    c_duckdb_file_system_error_data :: DuckDBFileSystem -> IO DuckDBErrorData

foreign import ccall safe "duckdb_file_system_open"
    c_duckdb_file_system_open :: DuckDBFileSystem -> CString -> DuckDBFileOpenOptions -> Ptr DuckDBFileHandle -> IO DuckDBState

foreign import ccall safe "duckdb_create_file_open_options"
    c_duckdb_create_file_open_options :: IO DuckDBFileOpenOptions

foreign import ccall safe "duckdb_file_open_options_set_flag"
    c_duckdb_file_open_options_set_flag :: DuckDBFileOpenOptions -> DuckDBFileFlag -> CBool -> IO DuckDBState

foreign import ccall safe "duckdb_destroy_file_open_options"
    c_duckdb_destroy_file_open_options :: Ptr DuckDBFileOpenOptions -> IO ()

foreign import ccall safe "duckdb_destroy_file_handle"
    c_duckdb_destroy_file_handle :: Ptr DuckDBFileHandle -> IO ()

foreign import ccall safe "duckdb_file_handle_error_data"
    c_duckdb_file_handle_error_data :: DuckDBFileHandle -> IO DuckDBErrorData

foreign import ccall safe "duckdb_file_handle_read"
    c_duckdb_file_handle_read :: DuckDBFileHandle -> Ptr () -> Int64 -> IO Int64

foreign import ccall safe "duckdb_file_handle_write"
    c_duckdb_file_handle_write :: DuckDBFileHandle -> Ptr () -> Int64 -> IO Int64

foreign import ccall safe "duckdb_file_handle_tell"
    c_duckdb_file_handle_tell :: DuckDBFileHandle -> IO Int64

foreign import ccall safe "duckdb_file_handle_size"
    c_duckdb_file_handle_size :: DuckDBFileHandle -> IO Int64

foreign import ccall safe "duckdb_file_handle_seek"
    c_duckdb_file_handle_seek :: DuckDBFileHandle -> Int64 -> IO DuckDBState

foreign import ccall safe "duckdb_file_handle_sync"
    c_duckdb_file_handle_sync :: DuckDBFileHandle -> IO DuckDBState

foreign import ccall safe "duckdb_file_handle_close"
    c_duckdb_file_handle_close :: DuckDBFileHandle -> IO DuckDBState
