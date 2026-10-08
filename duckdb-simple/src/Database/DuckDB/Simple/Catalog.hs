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
import Database.DuckDB.FFI.Compat
import Database.DuckDB.Simple.Internal (Connection, peekUtf8CString, throwRegistrationError, withClientContext)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CChar)
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
catalogTypeName conn catalogName
    | Text.any (== '\0') catalogName = throwRegistrationError "catalog name contains NUL"
    | otherwise =
        withClientContext conn \ctx ->
            TextForeign.withCString catalogName \cName ->
                withMaybeCatalog ctx (ConstPtr cName) \catalog -> do
                    namePtr <- c_duckdb_catalog_get_type_name catalog
                    if namePtr == ConstPtr nullPtr
                        then pure Nothing
                        else Just <$> peekUtf8CString namePtr

-- | Look up a catalog entry by catalog, schema, name, and expected entry kind.
lookupCatalogEntry :: Connection -> Text -> Text -> Text -> DuckDBCatalogEntryType -> IO (Maybe CatalogEntry)
lookupCatalogEntry conn catalogName schemaName entryName entryType
    | any (Text.any (== '\0')) [catalogName, schemaName, entryName] = throwRegistrationError "catalog lookup name contains NUL"
    | entryType `notElem` [DuckDBCatalogEntryTypeTable, DuckDBCatalogEntryTypeView, DuckDBCatalogEntryTypeIndex, DuckDBCatalogEntryTypeSequence, DuckDBCatalogEntryTypeCollation, DuckDBCatalogEntryTypeType] =
        throwRegistrationError "unsupported catalog entry type"
    | otherwise =
        withClientContext conn \ctx ->
            TextForeign.withCString catalogName \cCatalog ->
                TextForeign.withCString schemaName \cSchema ->
                    TextForeign.withCString entryName \cEntry ->
                        withMaybeCatalog ctx (ConstPtr cCatalog) \catalog ->
                            withMaybeCatalogEntry catalog ctx entryType (ConstPtr cSchema) (ConstPtr cEntry) \entry -> do
                                typ <- c_duckdb_catalog_entry_get_type entry
                                namePtr <- c_duckdb_catalog_entry_get_name entry
                                if namePtr == ConstPtr nullPtr
                                    then pure Nothing
                                    else do
                                        name <- peekUtf8CString namePtr
                                        pure (Just CatalogEntry{catalogEntryName = name, catalogEntryType = typ})

destroyCatalog :: DuckDBCatalog -> IO ()
destroyCatalog catalog =
    alloca \ptr -> poke ptr catalog >> c_duckdb_destroy_catalog ptr

destroyCatalogEntry :: DuckDBCatalogEntry -> IO ()
destroyCatalogEntry entry =
    alloca \ptr -> poke ptr entry >> c_duckdb_destroy_catalog_entry ptr

withMaybeCatalog :: DuckDBClientContext -> ConstPtr CChar -> (DuckDBCatalog -> IO (Maybe a)) -> IO (Maybe a)
withMaybeCatalog ctx name action =
    bracket (c_duckdb_client_context_get_catalog ctx name) destroyCatalog \catalog ->
        if catalog == DuckDBCatalog nullPtr then pure Nothing else action catalog

withMaybeCatalogEntry ::
    DuckDBCatalog ->
    DuckDBClientContext ->
    DuckDBCatalogEntryType ->
    ConstPtr CChar ->
    ConstPtr CChar ->
    (DuckDBCatalogEntry -> IO (Maybe a)) ->
    IO (Maybe a)
withMaybeCatalogEntry catalog ctx entryType schemaName entryName action =
    bracket (c_duckdb_catalog_get_entry catalog ctx entryType schemaName entryName) destroyCatalogEntry \entry ->
        if entry == DuckDBCatalogEntry nullPtr then pure Nothing else action entry
