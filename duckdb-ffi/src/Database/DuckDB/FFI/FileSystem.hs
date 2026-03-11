{- |
Module      : Database.DuckDB.FFI.FileSystem
Description : Raw bindings for DuckDB's VFS and file-handle APIs.
-}
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

-- | Fetch the file-system handle associated with a client context.
foreign import ccall safe "duckdb_client_context_get_file_system"
    c_duckdb_client_context_get_file_system :: DuckDBClientContext -> IO DuckDBFileSystem

-- | Destroy a file-system handle obtained from DuckDB.
foreign import ccall safe "duckdb_destroy_file_system"
    c_duckdb_destroy_file_system :: Ptr DuckDBFileSystem -> IO ()

-- | Fetch the last structured file-system error.
foreign import ccall safe "duckdb_file_system_error_data"
    c_duckdb_file_system_error_data :: DuckDBFileSystem -> IO DuckDBErrorData

-- | Open a file via DuckDB's file-system abstraction.
foreign import ccall safe "duckdb_file_system_open"
    c_duckdb_file_system_open :: DuckDBFileSystem -> CString -> DuckDBFileOpenOptions -> Ptr DuckDBFileHandle -> IO DuckDBState

-- | Allocate a mutable file-open options object.
foreign import ccall safe "duckdb_create_file_open_options"
    c_duckdb_create_file_open_options :: IO DuckDBFileOpenOptions

-- | Enable or disable a file-open flag on an options object.
foreign import ccall safe "duckdb_file_open_options_set_flag"
    c_duckdb_file_open_options_set_flag :: DuckDBFileOpenOptions -> DuckDBFileFlag -> CBool -> IO DuckDBState

-- | Destroy a file-open options object.
foreign import ccall safe "duckdb_destroy_file_open_options"
    c_duckdb_destroy_file_open_options :: Ptr DuckDBFileOpenOptions -> IO ()

-- | Destroy a file handle obtained from DuckDB.
foreign import ccall safe "duckdb_destroy_file_handle"
    c_duckdb_destroy_file_handle :: Ptr DuckDBFileHandle -> IO ()

-- | Fetch the last structured file-handle error.
foreign import ccall safe "duckdb_file_handle_error_data"
    c_duckdb_file_handle_error_data :: DuckDBFileHandle -> IO DuckDBErrorData

-- | Read bytes from a file handle into caller-provided memory.
foreign import ccall safe "duckdb_file_handle_read"
    c_duckdb_file_handle_read :: DuckDBFileHandle -> Ptr () -> Int64 -> IO Int64

-- | Write bytes to a file handle from caller-provided memory.
foreign import ccall safe "duckdb_file_handle_write"
    c_duckdb_file_handle_write :: DuckDBFileHandle -> Ptr () -> Int64 -> IO Int64

-- | Query the current file position.
foreign import ccall safe "duckdb_file_handle_tell"
    c_duckdb_file_handle_tell :: DuckDBFileHandle -> IO Int64

-- | Query the size of a file handle.
foreign import ccall safe "duckdb_file_handle_size"
    c_duckdb_file_handle_size :: DuckDBFileHandle -> IO Int64

-- | Seek a file handle to an absolute byte offset.
foreign import ccall safe "duckdb_file_handle_seek"
    c_duckdb_file_handle_seek :: DuckDBFileHandle -> Int64 -> IO DuckDBState

-- | Flush a file handle's buffered writes to stable storage.
foreign import ccall safe "duckdb_file_handle_sync"
    c_duckdb_file_handle_sync :: DuckDBFileHandle -> IO DuckDBState

-- | Close a file handle without destroying the surrounding handle object.
foreign import ccall safe "duckdb_file_handle_close"
    c_duckdb_file_handle_close :: DuckDBFileHandle -> IO DuckDBState
