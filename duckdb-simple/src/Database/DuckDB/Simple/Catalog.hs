{-# LANGUAGE BlockArguments #-}

{- |
Module      : Database.DuckDB.Simple.Catalog
Description : High-level helpers for DuckDB catalog inspection.
-}
module Database.DuckDB.Simple.Catalog (
    CatalogEntry (..),
    catalogTypeName,
    lookupCatalogEntry,
) where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Internal (Connection, withClientContext)
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (poke)

-- | A simplified view of a catalog entry returned by DuckDB.
data CatalogEntry = CatalogEntry
    { catalogEntryName :: !Text
    , catalogEntryType :: !DuckDBCatalogEntryType
    }
    deriving (Eq, Show)

-- | Look up the backend type name of a named catalog.
catalogTypeName :: Connection -> Text -> IO (Maybe Text)
catalogTypeName conn catalogName =
    withClientContext conn \ctx ->
        TextForeign.withCString catalogName \cName ->
            withMaybeCatalog ctx cName \catalog -> do
                namePtr <- c_duckdb_catalog_get_type_name catalog
                if namePtr == nullPtr
                    then pure Nothing
                    else Just . Text.pack <$> peekCString namePtr

-- | Look up a catalog entry by catalog, schema, name, and expected entry kind.
lookupCatalogEntry :: Connection -> Text -> Text -> Text -> DuckDBCatalogEntryType -> IO (Maybe CatalogEntry)
lookupCatalogEntry conn catalogName schemaName entryName entryType =
    withClientContext conn \ctx ->
        TextForeign.withCString catalogName \cCatalog ->
            TextForeign.withCString schemaName \cSchema ->
                TextForeign.withCString entryName \cEntry ->
                    withMaybeCatalog ctx cCatalog \catalog ->
                        withMaybeCatalogEntry catalog ctx entryType cSchema cEntry \entry -> do
                            typ <- c_duckdb_catalog_entry_get_type entry
                            namePtr <- c_duckdb_catalog_entry_get_name entry
                            if namePtr == nullPtr
                                then pure Nothing
                                else do
                                    name <- Text.pack <$> peekCString namePtr
                                    pure (Just CatalogEntry{catalogEntryName = name, catalogEntryType = typ})

destroyCatalog :: DuckDBCatalog -> IO ()
destroyCatalog catalog =
    alloca \ptr -> poke ptr catalog >> c_duckdb_destroy_catalog ptr

destroyCatalogEntry :: DuckDBCatalogEntry -> IO ()
destroyCatalogEntry entry =
    alloca \ptr -> poke ptr entry >> c_duckdb_destroy_catalog_entry ptr

withMaybeCatalog :: DuckDBClientContext -> CString -> (DuckDBCatalog -> IO (Maybe a)) -> IO (Maybe a)
withMaybeCatalog ctx name action = do
    catalog <- c_duckdb_client_context_get_catalog ctx name
    if catalog == nullPtr
        then pure Nothing
        else bracket (pure catalog) destroyCatalog action

withMaybeCatalogEntry ::
    DuckDBCatalog ->
    DuckDBClientContext ->
    DuckDBCatalogEntryType ->
    CString ->
    CString ->
    (DuckDBCatalogEntry -> IO (Maybe a)) ->
    IO (Maybe a)
withMaybeCatalogEntry catalog ctx entryType schemaName entryName action = do
    entry <- c_duckdb_catalog_get_entry catalog ctx entryType schemaName entryName
    if entry == nullPtr
        then pure Nothing
        else bracket (pure entry) destroyCatalogEntry action
