module Database.DuckDB.FFI.Catalog (
    c_duckdb_client_context_get_catalog,
    c_duckdb_catalog_get_type_name,
    c_duckdb_catalog_get_entry,
    c_duckdb_destroy_catalog,
    c_duckdb_catalog_entry_get_type,
    c_duckdb_catalog_entry_get_name,
    c_duckdb_destroy_catalog_entry,
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

foreign import ccall safe "duckdb_client_context_get_catalog"
    c_duckdb_client_context_get_catalog :: DuckDBClientContext -> CString -> IO DuckDBCatalog

foreign import ccall safe "duckdb_catalog_get_type_name"
    c_duckdb_catalog_get_type_name :: DuckDBCatalog -> IO CString

foreign import ccall safe "duckdb_catalog_get_entry"
    c_duckdb_catalog_get_entry :: DuckDBCatalog -> DuckDBClientContext -> DuckDBCatalogEntryType -> CString -> CString -> IO DuckDBCatalogEntry

foreign import ccall safe "duckdb_destroy_catalog"
    c_duckdb_destroy_catalog :: Ptr DuckDBCatalog -> IO ()

foreign import ccall safe "duckdb_catalog_entry_get_type"
    c_duckdb_catalog_entry_get_type :: DuckDBCatalogEntry -> IO DuckDBCatalogEntryType

foreign import ccall safe "duckdb_catalog_entry_get_name"
    c_duckdb_catalog_entry_get_name :: DuckDBCatalogEntry -> IO CString

foreign import ccall safe "duckdb_destroy_catalog_entry"
    c_duckdb_destroy_catalog_entry :: Ptr DuckDBCatalogEntry -> IO ()
