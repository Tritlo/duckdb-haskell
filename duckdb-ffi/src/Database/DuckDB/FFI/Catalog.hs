{- |
Module      : Database.DuckDB.FFI.Catalog
Description : Raw bindings for DuckDB's catalog inspection API.
-}
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

-- | Look up a named catalog from a client context.
foreign import ccall safe "duckdb_client_context_get_catalog"
    c_duckdb_client_context_get_catalog :: DuckDBClientContext -> CString -> IO DuckDBCatalog

-- | Fetch the backend type name of a catalog.
foreign import ccall safe "duckdb_catalog_get_type_name"
    c_duckdb_catalog_get_type_name :: DuckDBCatalog -> IO CString

-- | Look up a catalog entry by schema, name, and entry kind.
foreign import ccall safe "duckdb_catalog_get_entry"
    c_duckdb_catalog_get_entry :: DuckDBCatalog -> DuckDBClientContext -> DuckDBCatalogEntryType -> CString -> CString -> IO DuckDBCatalogEntry

-- | Destroy a catalog handle obtained from DuckDB.
foreign import ccall safe "duckdb_destroy_catalog"
    c_duckdb_destroy_catalog :: Ptr DuckDBCatalog -> IO ()

-- | Read the entry kind of a catalog entry handle.
foreign import ccall safe "duckdb_catalog_entry_get_type"
    c_duckdb_catalog_entry_get_type :: DuckDBCatalogEntry -> IO DuckDBCatalogEntryType

-- | Read the name of a catalog entry handle.
foreign import ccall safe "duckdb_catalog_entry_get_name"
    c_duckdb_catalog_entry_get_name :: DuckDBCatalogEntry -> IO CString

-- | Destroy a catalog entry handle obtained from DuckDB.
foreign import ccall safe "duckdb_destroy_catalog_entry"
    c_duckdb_destroy_catalog_entry :: Ptr DuckDBCatalogEntry -> IO ()
