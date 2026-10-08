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
import Database.DuckDB.Simple.Internal (Connection, peekUtf8CString, throwRegistrationError, withClientContext)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (poke)

-- | A simplified view of a catalog entry returned by DuckDB.
data CatalogEntry = CatalogEntry
    { catalogEntryName :: !Text
    , catalogEntryType :: !Duckdb_catalog_entry_type
    }
    deriving (Eq, Show)

-- | Look up the backend type name of a named catalog.
catalogTypeName :: Connection -> Text -> IO (Maybe Text)
catalogTypeName conn catalogName
    | Text.any (== '\0') catalogName = throwRegistrationError "catalog name contains NUL"
    | otherwise =
        withClientContext conn \ctx ->
            TextForeign.withCString catalogName \cName ->
                withMaybeCatalog ctx (ConstPtr cName) \catalog -> do
                    namePtr <- duckdb_catalog_get_type_name catalog
                    if namePtr == ConstPtr nullPtr
                        then pure Nothing
                        else Just <$> peekUtf8CString namePtr

-- | Look up a catalog entry by catalog, schema, name, and expected entry kind.
lookupCatalogEntry :: Connection -> Text -> Text -> Text -> Duckdb_catalog_entry_type -> IO (Maybe CatalogEntry)
lookupCatalogEntry conn catalogName schemaName entryName entryType
    | any (Text.any (== '\0')) [catalogName, schemaName, entryName] = throwRegistrationError "catalog lookup name contains NUL"
    | entryType `notElem` [DUCKDB_CATALOG_ENTRY_TYPE_TABLE, DUCKDB_CATALOG_ENTRY_TYPE_VIEW, DUCKDB_CATALOG_ENTRY_TYPE_INDEX, DUCKDB_CATALOG_ENTRY_TYPE_SEQUENCE, DUCKDB_CATALOG_ENTRY_TYPE_COLLATION, DUCKDB_CATALOG_ENTRY_TYPE_TYPE] =
        throwRegistrationError "unsupported catalog entry type"
    | otherwise =
        withClientContext conn \ctx ->
            TextForeign.withCString catalogName \cCatalog ->
                TextForeign.withCString schemaName \cSchema ->
                    TextForeign.withCString entryName \cEntry ->
                        withMaybeCatalog ctx (ConstPtr cCatalog) \catalog ->
                            withMaybeCatalogEntry catalog ctx entryType (ConstPtr cSchema) (ConstPtr cEntry) \entry -> do
                                typ <- duckdb_catalog_entry_get_type entry
                                namePtr <- duckdb_catalog_entry_get_name entry
                                if namePtr == ConstPtr nullPtr
                                    then pure Nothing
                                    else do
                                        name <- peekUtf8CString namePtr
                                        pure (Just CatalogEntry{catalogEntryName = name, catalogEntryType = typ})

destroyCatalog :: Duckdb_catalog -> IO ()
destroyCatalog catalog =
    alloca \ptr -> poke ptr catalog >> duckdb_destroy_catalog ptr

destroyCatalogEntry :: Duckdb_catalog_entry -> IO ()
destroyCatalogEntry entry =
    alloca \ptr -> poke ptr entry >> duckdb_destroy_catalog_entry ptr

withMaybeCatalog :: Duckdb_client_context -> ConstPtr CChar -> (Duckdb_catalog -> IO (Maybe a)) -> IO (Maybe a)
withMaybeCatalog ctx name action =
    bracket (duckdb_client_context_get_catalog ctx name) destroyCatalog \catalog ->
        if catalog == Duckdb_catalog nullPtr then pure Nothing else action catalog

withMaybeCatalogEntry ::
    Duckdb_catalog ->
    Duckdb_client_context ->
    Duckdb_catalog_entry_type ->
    ConstPtr CChar ->
    ConstPtr CChar ->
    (Duckdb_catalog_entry -> IO (Maybe a)) ->
    IO (Maybe a)
withMaybeCatalogEntry catalog ctx entryType schemaName entryName action =
    bracket (duckdb_catalog_get_entry catalog ctx entryType schemaName entryName) destroyCatalogEntry \entry ->
        if entry == Duckdb_catalog_entry nullPtr then pure Nothing else action entry
