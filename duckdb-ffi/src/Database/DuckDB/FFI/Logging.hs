{- |
Module      : Database.DuckDB.FFI.Logging
Description : Raw bindings for DuckDB's custom log-storage API.
-}
module Database.DuckDB.FFI.Logging (
    c_duckdb_create_log_storage,
    c_duckdb_destroy_log_storage,
    c_duckdb_log_storage_set_write_log_entry,
    c_duckdb_log_storage_set_extra_data,
    c_duckdb_log_storage_set_name,
    c_duckdb_register_log_storage,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

-- | Allocate a new log storage object.
foreign import ccall safe "duckdb_create_log_storage"
    c_duckdb_create_log_storage :: IO DuckDBLogStorage

-- | Destroy a log storage object.
foreign import ccall safe "duckdb_destroy_log_storage"
    c_duckdb_destroy_log_storage :: Ptr DuckDBLogStorage -> IO ()

-- | Set the callback used when DuckDB writes a log entry.
foreign import ccall safe "duckdb_log_storage_set_write_log_entry"
    c_duckdb_log_storage_set_write_log_entry :: DuckDBLogStorage -> DuckDBLoggerWriteLogEntryFun -> IO ()

-- | Attach user-managed extra data to a log storage object.
foreign import ccall safe "duckdb_log_storage_set_extra_data"
    c_duckdb_log_storage_set_extra_data :: DuckDBLogStorage -> Ptr () -> DuckDBDeleteCallback -> IO ()

-- | Set the registration name of a log storage object.
foreign import ccall safe "duckdb_log_storage_set_name"
    c_duckdb_log_storage_set_name :: DuckDBLogStorage -> CString -> IO ()

-- | Register a custom log storage on a database handle.
foreign import ccall safe "duckdb_register_log_storage"
    c_duckdb_register_log_storage :: DuckDBDatabase -> DuckDBLogStorage -> IO DuckDBState
