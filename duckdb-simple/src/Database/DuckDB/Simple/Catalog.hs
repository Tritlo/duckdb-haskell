{-# LANGUAGE OverloadedStrings #-}

-- | Catalog inspection through V2 queries and native table descriptions.
module Database.DuckDB.Simple.Catalog (
    CatalogEntryType (..),
    CatalogEntry (..),
    catalogTypeName,
    lookupCatalogEntry,
    TableDescription (..),
    ColumnDescription (..),
    describeTable,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, when)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple (Only (..), query)
import Database.DuckDB.Simple.Internal (Connection, withConnectionHandle)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep, logicalTypeToRep)
import Database.DuckDB.Simple.Native

-- | Entry kinds that DuckDB exposes through catalog metadata.
data CatalogEntryType = CatalogTable | CatalogView | CatalogIndex | CatalogSequence | CatalogCollation | CatalogType
    deriving (Eq, Ord, Show, Read)

-- | A copied catalog entry name and kind.
data CatalogEntry = CatalogEntry
    { catalogEntryName :: !Text
    , catalogEntryType :: !CatalogEntryType
    }
    deriving (Eq, Show)

-- | A table's qualified name, access mode, and columns.
data TableDescription = TableDescription
    { tableDescriptionName :: ![Text]
    , tableDescriptionReadonly :: !Bool
    , tableDescriptionColumns :: ![ColumnDescription]
    }
    deriving (Eq, Show)

-- | A copied column description. Native handles remain inside the call.
data ColumnDescription = ColumnDescription
    { columnDescriptionName :: !Text
    , columnDescriptionType :: !LogicalTypeRep
    , columnDescriptionHasDefault :: !Bool
    , columnDescriptionGenerated :: !Bool
    }
    deriving (Eq, Show)

-- | Read the backend type of an attached database.
catalogTypeName :: Connection -> Text -> IO (Maybe Text)
catalogTypeName conn name = do
    validateNames [name]
    rows <- query conn (Query ("SELECT type::VARCHAR FROM system.main.duckdb_databases() WHERE " <> identifierMatches "database_name")) (Only name)
    pure (case rows of Only value : _ -> Just value; [] -> Nothing)

-- | Look up an entry in a top-level schema. Names use DuckDB's identifier rules.
lookupCatalogEntry :: Connection -> Text -> Text -> Text -> CatalogEntryType -> IO (Maybe CatalogEntry)
lookupCatalogEntry conn catalog schema name kind = do
    validateNames [catalog, schema, name]
    (functionName, columnName) <- case kind of
        CatalogTable -> pure ("duckdb_tables()", "table_name")
        CatalogView -> pure ("duckdb_views()", "view_name")
        CatalogIndex -> pure ("duckdb_indexes()", "index_name")
        CatalogSequence -> pure ("duckdb_sequences()", "sequence_name")
        CatalogType -> pure ("duckdb_types()", "type_name")
        CatalogCollation -> throwIO (SQLError "duckdb-simple: scoped collation lookup is unavailable in DuckDB V2 metadata" (Just DuckDBV2ErrorApi) Nothing)
    let sql =
            Query
                ( "SELECT e."
                    <> columnName
                    <> "::VARCHAR FROM system.main."
                    <> functionName
                    <> " e"
                    <> " JOIN system.main.duckdb_schemas() s ON e.database_oid = s.database_oid AND e.schema_oid = s.oid"
                    <> " WHERE s.parent_schema_oid IS NULL AND "
                    <> identifierMatches "e.database_name"
                    <> " AND "
                    <> identifierMatches "e.schema_name"
                    <> " AND "
                    <> identifierMatches ("e." <> columnName)
                    <> " LIMIT 1"
                )
    rows <- query conn sql (catalog, schema, name)
    pure (case rows of Only value : _ -> Just (CatalogEntry value kind); [] -> Nothing)

{- | Compare a trusted column with a parameter using ASCII case folding.
Binary comparison keeps the result independent of the SQL collation.
-}
identifierMatches :: Text -> Text
identifierMatches column = normalize column <> " = " <> normalize "?"
  where
    normalize expression = "system.main.encode(system.main.translate(" <> expression <> ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'))"

-- | Copy a native base-table description using a parsed qualified name.
describeTable :: Connection -> Text -> IO TableDescription
describeTable conn name = withConnectionHandle conn $ \handle ->
    withText name $ \text -> bracket (output (c_duckdb_v2_qname_parse text)) (destroy c_duckdb_v2_qname_destroy) $ \qname ->
        bracket (output (c_duckdb_v2_connection_describe_table handle qname)) (destroy c_duckdb_v2_table_description_destroy) $ \table -> do
            copiedName <- bracket (output (c_duckdb_v2_table_description_get_qname table)) (destroy c_duckdb_v2_qname_destroy) $ \resolved -> do
                parts <- output (c_duckdb_v2_qname_get_part_count resolved)
                forM (indices parts) $ \index -> output (c_duckdb_v2_qname_get_part resolved index) >>= readIdentifier
            readonly <- output (c_duckdb_v2_table_description_is_readonly table)
            count <- output (c_duckdb_v2_table_description_get_column_count table)
            columns <- forM (indices count) $ \index ->
                bracket (output (c_duckdb_v2_table_description_get_column table index)) (destroy c_duckdb_v2_column_description_destroy) $ \column -> do
                    columnName <- output (c_duckdb_v2_column_description_get_name column) >>= readIdentifier
                    columnType <- output (c_duckdb_v2_column_description_get_type column) >>= logicalTypeToRep
                    defaultValue <- output (c_duckdb_v2_column_description_has_default column)
                    generated <- output (c_duckdb_v2_column_description_has_generated column)
                    pure (ColumnDescription columnName columnType (defaultValue /= 0) (generated /= 0))
            pure (TableDescription copiedName (readonly /= 0) columns)

-- | Reject names that contain NUL.
validateNames :: [Text] -> IO ()
validateNames names = when (any (Text.any (== '\0')) names) $ throwInvalidArgument "catalog lookup name contains NUL"
