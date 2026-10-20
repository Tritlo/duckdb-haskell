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
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
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
Each level batches type and key metadata. Objects with the same keys share
one extraction query. Keys remain bound values, including empty and NUL keys.
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
    metadata <- queryMetadata connection statement values
    unless (length metadata == length values) (codecError "metadata query returned an unexpected number of nodes")
    let indexed = zip3 [0 ..] metadata values
        groups = Map.fromListWith (++) [(keys, [(index, value)]) | (index, (tag, keys), value) <- indexed, isObject tag]
        releaseIndexed = releaseNode . snd
    bracket
        (acquireItems releaseIndexed [(,) index <$> decodeNode tag value | (index, (tag, _), value) <- indexed, not (isObject tag)])
        (releaseItems releaseIndexed)
        \otherNodes ->
            bracket
                (acquireItems (releaseItems releaseIndexed) [queryObjectGroup connection keys rows | (keys, rows) <- Map.toList groups])
                (releaseItems (releaseItems releaseIndexed))
                \objectGroups -> do
                    let indexedNodes = IntMap.fromList (otherNodes <> concat objectGroups)
                    nodes <- forM [0 .. length values - 1] \index ->
                        maybe (codecError "object extraction omitted a node") pure (IntMap.lookup index indexedNodes)
                    children <- decodeValues scalar connection statement (concatMap nodeChildren nodes)
                    assemble nodes children
  where
    isObject = Text.isPrefixOf "OBJECT("
    decodeNode tag value
        | tag == "VARIANT_NULL" = pure (ScalarNode FieldNull)
        | "ARRAY(" `Text.isPrefixOf` tag =
            withCast connection value "VARIANT[]" \list -> do
                count <- output (c_duckdb_v2_value_get_child_count list)
                ListNode <$> acquireItems releaseValue [output (c_duckdb_v2_value_get_child list i) | i <- indices count]
        | otherwise = do
            target <- scalarType tag
            ScalarNode <$> withCast connection value target (scalar connection)

{- | Extract objects with one key layout as a native VARIANT[][] value.
The binder needs constant extraction keys. Bind each key once and apply the
extractions to all objects with that layout. This avoids per-row queries and
preserves keys that cannot be SQL identifiers.
-}
queryObjectGroup :: DuckDBV2ConnectionHandle -> [Text] -> [(Int, DuckDBV2ValueHandle)] -> IO [(Int, VariantNode)]
queryObjectGroup _ [] rows = pure [(index, ObjectNode []) | (index, _) <- rows]
queryObjectGroup connection keys rows =
    withVariantList connection (map snd rows) \list ->
        withConnectionFactory connection \factory ->
            bracket
                (acquireItems releaseValue [withText key (output . c_duckdb_v2_value_create_varchar factory) | key <- keys])
                (releaseItems releaseValue)
                \keyValues ->
                    withSQLStatement connection sql \statement ->
                        withStatementValue connection statement (list : keyValues) \objects -> do
                            count <- output (c_duckdb_v2_value_get_child_count objects)
                            unless (toInteger count == toInteger (length rows)) (codecError "object extraction returned an unexpected row count")
                            acquireItems
                                (releaseNode . snd)
                                [ bracket (output (c_duckdb_v2_value_get_child objects i)) releaseValue \children -> do
                                    childCount <- output (c_duckdb_v2_value_get_child_count children)
                                    unless (toInteger childCount == toInteger (length keys)) (codecError "object extraction returned an unexpected child count")
                                    owned <- acquireItems releaseValue [output (c_duckdb_v2_value_get_child children j) | j <- indices childCount]
                                    pure (index, ObjectNode (zip keys owned))
                                | (i, (index, _)) <- zip [0 ..] rows
                                ]
  where
    sql = "SELECT system.main.list_transform($1::VARIANT[], lambda value: system.main.list_value(" <> Text.intercalate "," extracts <> "))"
    extracts = ["system.main.variant_extract(value,$" <> Text.pack (show index) <> "::VARCHAR)" | index <- [2 .. length keys + 1]]

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
    bracket (withText target (\nativeInput -> withConnectionFactory connection (\nativeFactory -> output (c_duckdb_v2_factory_create_type_from_text nativeFactory nativeInput)))) (destroy c_duckdb_v2_logical_type_destroy) \logical ->
        bracket (withConnectionFactory connection (\nativeFactory -> output (c_duckdb_v2_value_cast nativeFactory value logical))) releaseValue action

-- | Parse one metadata statement and reuse it for each nesting level.
withTypeStatement :: DuckDBV2ConnectionHandle -> (DuckDBV2SqlStatementHandle -> IO a) -> IO a
withTypeStatement connection =
    withSQLStatement
        connection
        "SELECT system.main.list_transform(?::VARIANT[], lambda value: system.main.struct_pack(tag := system.main.variant_typeof(value), keys := system.main.variant_keys(value)))"

-- | Borrow a parsed SQL statement. Payloads and object keys are not SQL text.
withSQLStatement :: DuckDBV2ConnectionHandle -> Text -> (DuckDBV2SqlStatementHandle -> IO a) -> IO a
withSQLStatement connection sql action =
    withCString (Text.unpack sql) \source ->
        bracket (output (c_duckdb_v2_parse_sql connection (ConstPtr source))) (destroy c_duckdb_v2_statement_iterator_destroy) \iterator ->
            bracket (output (c_duckdb_v2_statement_iterator_next iterator)) (destroy c_duckdb_v2_sql_statement_destroy) action

-- | Read exact scalar tags and object keys without serializing payloads.
queryMetadata :: DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> [DuckDBV2ValueHandle] -> IO [(Text, [Text])]
queryMetadata connection statement values = withVariantList connection values \list ->
    withStatementValue connection statement [list] \metadata -> do
        count <- output (c_duckdb_v2_value_get_child_count metadata)
        forM (indices count) \index ->
            bracket (output (c_duckdb_v2_value_get_child metadata index)) releaseValue \node -> do
                tag <- bracket (output (c_duckdb_v2_value_get_child node 0)) releaseValue \value ->
                    output (c_duckdb_v2_value_get_varchar value) >>= readText
                keys <- bracket (output (c_duckdb_v2_value_get_child node 1)) releaseValue \value -> do
                    isNull <- output (c_duckdb_v2_value_is_null value)
                    if isNull /= 0
                        then pure []
                        else do
                            keyCount <- output (c_duckdb_v2_value_get_child_count value)
                            forM (indices keyCount) \i ->
                                bracket (output (c_duckdb_v2_value_get_child value i)) releaseValue \key ->
                                    output (c_duckdb_v2_value_get_varchar key) >>= readText
                pure (tag, keys)

-- | Borrow an owned VARIANT list that copies all supplied values.
withVariantList :: DuckDBV2ConnectionHandle -> [DuckDBV2ValueHandle] -> (DuckDBV2ValueHandle -> IO a) -> IO a
withVariantList connection values action =
    withConnectionFactory connection \factory ->
        bracket (withText "VARIANT" (output . c_duckdb_v2_factory_create_type_from_text factory)) (destroy c_duckdb_v2_logical_type_destroy) \logical ->
            withArray values \items ->
                bracket (output (c_duckdb_v2_value_create_list factory logical (ConstPtr items) (fromIntegral (length values)))) releaseValue action

-- | Borrow the first result value while its statement result stays alive.
withStatementValue :: DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> [DuckDBV2ValueHandle] -> (DuckDBV2ValueHandle -> IO a) -> IO a
withStatementValue connection statement parameters action =
    withArray parameters \items ->
        bracket (output (c_duckdb_v2_statement_execute connection statement (ConstPtr nullPtr) (ConstPtr items) (fromIntegral (length parameters)))) (destroy c_duckdb_v2_result_destroy) \result ->
            bracket (output (c_duckdb_v2_result_fetch_chunk result)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
                unless (chunk /= nullHandle) (codecError "metadata query returned no rows")
                vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                bracket (output (c_duckdb_v2_vector_get_value vector 0)) releaseValue action

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
