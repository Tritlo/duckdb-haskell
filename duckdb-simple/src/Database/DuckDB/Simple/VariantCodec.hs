{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lossless VARIANT decoding through public V2 values and SQL type metadata.
module Database.DuckDB.Simple.VariantCodec (
    decodeVariant,
    prepareVariantDecoder,
) where

import Control.Exception (bracket, evaluate, finally, mask_, onException, throwIO)
import Control.Monad (forM, unless)
import Data.Array (listArray, (!))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Variant (variantObject)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (withCString)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)

-- | Decode one row. Preparing the reader copies every row in the vector.
decodeVariant :: (DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue) -> DuckDBV2ConnectionHandle -> DuckDBV2VectorHandle -> Int -> IO FieldValue
decodeVariant scalar connection vector row = prepareVariantDecoder scalar connection vector >>= ($ row)

{- | Copy a vector before returning its row reader. The connection is a dedicated
metadata connection. It must remain exclusively borrowed until this function
returns. The scalar callback decodes a value after its exact native cast.
Each nesting level uses one type query for all values at that level.
-}
prepareVariantDecoder :: (DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue) -> DuckDBV2ConnectionHandle -> DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)
prepareVariantDecoder scalar connection vector = do
    count <- output (c_duckdb_v2_vector_get_size vector)
    bracket (acquireItems releaseValue [output (c_duckdb_v2_vector_get_value vector i) | i <- indices count]) (releaseItems releaseValue) \values ->
        withTypeStatement connection \statement -> do
            decoded <- decodeValues scalar connection statement values
            let rows = listArray (0, length decoded - 1) decoded
            pure (evaluate . (rows !))

-- | A decoded node owns the child values needed at the next nesting level.
data VariantNode
    = ScalarNode FieldValue
    | ListNode [DuckDBV2ValueHandle]
    | ObjectNode [(Text, DuckDBV2ValueHandle)]

-- | Decode all siblings together before reconstructing their parent nodes.
decodeValues :: (DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue) -> DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> [DuckDBV2ValueHandle] -> IO [FieldValue]
decodeValues _ _ _ [] = pure []
decodeValues scalar connection statement values = do
    tags <- queryTypes connection statement values
    unless (length tags == length values) (codecError "type query returned an unexpected number of tags")
    bracket (acquireItems releaseNode (zipWith decodeNode tags values)) (releaseItems releaseNode) \nodes -> do
        children <- decodeValues scalar connection statement (concatMap nodeChildren nodes)
        assemble nodes children
  where
    decodeNode tag value
        | tag == "VARIANT_NULL" = pure (ScalarNode FieldNull)
        | "ARRAY(" `Text.isPrefixOf` tag =
            withCast connection value "VARIANT[]" \list -> do
                count <- output (c_duckdb_v2_value_get_child_count list)
                ListNode <$> acquireItems releaseValue [output (c_duckdb_v2_value_get_child list i) | i <- indices count]
        | "OBJECT(" `Text.isPrefixOf` tag =
            withCast connection value "MAP(VARCHAR,VARIANT)" \object -> do
                count <- output (c_duckdb_v2_value_get_child_count object)
                unless (even count) (codecError "object cast returned an incomplete key/value pair")
                ObjectNode <$> acquireItems (releaseValue . snd) [readEntry object i | i <- indices (count `div` 2)]
        | otherwise = do
            target <- scalarType tag
            ScalarNode <$> withCast connection value target (scalar connection)
    readEntry object index = do
        key <- bracket (output (c_duckdb_v2_value_get_child object (2 * index))) releaseValue \value ->
            output (c_duckdb_v2_value_get_varchar value) >>= readText
        child <- output (c_duckdb_v2_value_get_child object (2 * index + 1))
        pure (key, child)

-- | Rebuild a level from its copied children in their native order.
assemble :: [VariantNode] -> [FieldValue] -> IO [FieldValue]
assemble [] [] = pure []
assemble [] _ = codecError "decoded child count exceeds its parent count"
assemble (node : rest) children = do
    let (count, build) = case node of
            ScalarNode value -> (0, const value)
            ListNode values -> (length values, FieldList)
            ObjectNode entries -> (length entries, variantObject . zip (map fst entries))
        (ownChildren, remaining) = splitAt count children
    unless (length ownChildren == count) (codecError "decoded child count is smaller than its parent count")
    value <- evaluate (build ownChildren)
    (value :) <$> assemble rest remaining

-- | Borrow the native cast result while copying its payload.
withCast :: DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> Text -> (DuckDBV2ValueHandle -> IO a) -> IO a
withCast connection value target action =
    bracket (withText target (output . c_duckdb_v2_connection_create_type_from_text connection)) (destroy c_duckdb_v2_logical_type_destroy) \logical ->
        bracket (output (c_duckdb_v2_value_cast_with_connection connection value logical)) releaseValue action

-- | Parse one metadata statement and reuse it for each nesting level.
withTypeStatement :: DuckDBV2ConnectionHandle -> (DuckDBV2SqlStatementHandle -> IO a) -> IO a
withTypeStatement connection action =
    withCString "SELECT system.main.list_transform(?::VARIANT[], lambda value: system.main.variant_typeof(value))" \sql ->
        bracket (output (c_duckdb_v2_parse_sql connection (ConstPtr sql))) (destroy c_duckdb_v2_statement_iterator_destroy) \iterator ->
            bracket (output (c_duckdb_v2_statement_iterator_next iterator)) (destroy c_duckdb_v2_sql_statement_destroy) action

-- | Read exact scalar tags without converting payloads to text or JSON.
queryTypes :: DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> [DuckDBV2ValueHandle] -> IO [Text]
queryTypes connection statement values =
    bracket (withText "VARIANT" (output . c_duckdb_v2_connection_create_type_from_text connection)) (destroy c_duckdb_v2_logical_type_destroy) \logical ->
        withArray values \items ->
            bracket (output (c_duckdb_v2_value_create_list_with_connection connection logical (ConstPtr items) (fromIntegral (length values)))) releaseValue \list ->
                withArray [list] \parameter ->
                    bracket (output (c_duckdb_v2_statement_execute connection statement (ConstPtr nullPtr) (ConstPtr parameter) 1)) (destroy c_duckdb_v2_result_destroy) \result ->
                        bracket (output (c_duckdb_v2_result_fetch_chunk result)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
                            unless (chunk /= nullHandle) (codecError "type query returned no rows")
                            vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                            bracket (output (c_duckdb_v2_vector_get_value vector 0)) releaseValue \tags -> do
                                count <- output (c_duckdb_v2_value_get_child_count tags)
                                forM (indices count) \i ->
                                    bracket (output (c_duckdb_v2_value_get_child tags i)) releaseValue \tag ->
                                        output (c_duckdb_v2_value_get_varchar tag) >>= readText

-- | Map every native VARIANT scalar tag to its exact cast target.
scalarType :: Text -> IO Text
scalarType tag = case tag of
    "BOOL_TRUE" -> pure "BOOLEAN"
    "BOOL_FALSE" -> pure "BOOLEAN"
    "INT8" -> pure "TINYINT"
    "INT16" -> pure "SMALLINT"
    "INT32" -> pure "INTEGER"
    "INT64" -> pure "BIGINT"
    "INT128" -> pure "HUGEINT"
    "UINT8" -> pure "UTINYINT"
    "UINT16" -> pure "USMALLINT"
    "UINT32" -> pure "UINTEGER"
    "UINT64" -> pure "UBIGINT"
    "UINT128" -> pure "UHUGEINT"
    "TIME_MICROS" -> pure "TIME"
    "TIME_NANOS" -> pure "TIME_NS"
    "TIME_MICROS_TZ" -> pure "TIMETZ"
    "TIMESTAMP_SEC" -> pure "TIMESTAMP_S"
    "TIMESTAMP_MILIS" -> pure "TIMESTAMP_MS"
    "TIMESTAMP_MICROS" -> pure "TIMESTAMP"
    "TIMESTAMP_NANOS" -> pure "TIMESTAMP_NS"
    "TIMESTAMP_MICROS_TZ" -> pure "TIMESTAMPTZ"
    "TIMESTAMP_NANOS_TZ" -> pure "TIMESTAMPTZ_NS"
    "BITSTRING" -> pure "BIT"
    _
        | tag `elem` ["FLOAT", "DOUBLE", "VARCHAR", "BLOB", "UUID", "DATE", "INTERVAL", "BIGNUM", "GEOMETRY"] -> pure tag
        | "DECIMAL(" `Text.isPrefixOf` tag && ")" `Text.isSuffixOf` tag -> pure tag
        | otherwise -> codecError ("unsupported scalar tag " <> tag)

-- | List valid native indices without unsigned subtraction at zero.
indices :: DuckDBIdx -> [DuckDBIdx]
indices count = if count == 0 then [] else [0 .. count - 1]

-- | Acquire owned items without an asynchronous exception between transfers.
acquireItems :: (a -> IO ()) -> [IO a] -> IO [a]
acquireItems release = mask_ . go []
  where
    go owned [] = pure (reverse owned)
    go owned (acquire : rest) = do
        item <- acquire `onException` releaseItems release owned
        go (item : owned) rest

-- | Release every owned item even if one destructor reports an error.
releaseItems :: (a -> IO ()) -> [a] -> IO ()
releaseItems _ [] = pure ()
releaseItems release (item : rest) = release item `finally` releaseItems release rest

-- | List a node's owned children in their native order.
nodeChildren :: VariantNode -> [DuckDBV2ValueHandle]
nodeChildren (ScalarNode _) = []
nodeChildren (ListNode values) = values
nodeChildren (ObjectNode entries) = map snd entries

-- | Release the children owned by a decoded node.
releaseNode :: VariantNode -> IO ()
releaseNode = releaseItems releaseValue . nodeChildren

-- | Destroy an owned native value.
releaseValue :: DuckDBV2ValueHandle -> IO ()
releaseValue = destroy c_duckdb_v2_value_destroy

-- | Report a metadata or payload mismatch before decoding a native value.
codecError :: Text -> IO a
codecError message = throwIO (SQLError ("duckdb-simple: VARIANT: " <> message) Nothing Nothing)
