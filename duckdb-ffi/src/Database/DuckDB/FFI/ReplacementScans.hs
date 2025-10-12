module Database.DuckDB.FFI.ReplacementScans (
    c_duckdb_add_replacement_scan,
    c_duckdb_replacement_scan_set_function_name,
    c_duckdb_replacement_scan_add_parameter,
    c_duckdb_replacement_scan_set_error,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

{- | Add a replacement scan definition to the specified database.

Parameters:
* @db@: The database object to add the replacement scan to
* @replacement@: The replacement scan callback
* @extra_data@: Extra data that is passed back into the specified callback
* @delete_callback@: The delete callback to call on the extra data, if any
-}
foreign import ccall safe "duckdb_add_replacement_scan"
    c_duckdb_add_replacement_scan :: DuckDBDatabase -> DuckDBReplacementCallback -> Ptr () -> DuckDBDeleteCallback -> IO ()

{- | Sets the replacement function name. If this function is called in the
replacement callback, the replacement scan is performed. If it is not called,
the replacement callback is not performed.

Parameters:
* @info@: The info object
* @function_name@: The function name to substitute.
-}
foreign import ccall safe "duckdb_replacement_scan_set_function_name"
    c_duckdb_replacement_scan_set_function_name :: DuckDBReplacementScanInfo -> CString -> IO ()

{- | Adds a parameter to the replacement scan function.

Parameters:
* @info@: The info object
* @parameter@: The parameter to add.
-}
foreign import ccall safe "duckdb_replacement_scan_add_parameter"
    c_duckdb_replacement_scan_add_parameter :: DuckDBReplacementScanInfo -> DuckDBValue -> IO ()

{- | Report that an error has occurred while executing the replacement scan.

Parameters:
* @info@: The info object
* @error@: The error message
-}
foreign import ccall safe "duckdb_replacement_scan_set_error"
    c_duckdb_replacement_scan_set_error :: DuckDBReplacementScanInfo -> CString -> IO ()
