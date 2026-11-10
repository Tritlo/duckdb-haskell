{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Lossless VARIANT decoding through public V2 values and SQL type metadata.
module Database.DuckDB.Simple.VariantCodec (
    prepareVariantDecoder,
) where

import Control.Exception (SomeException, bracket, evaluate, mask_, onException, throwIO, try)
import Control.Monad (forM, unless)
import Data.Array (elems, listArray, (!))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.LogicalRep (StructField (..), StructValue (..))
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Variant (variantObject)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)

{- | Copy a vector before returning its row reader. The connection is a dedicated
metadata connection. It must remain exclusively borrowed until this function
returns. The callback decodes a value that contains no VARIANT.
Each level batches type and key metadata. Scalars with the same tag share
one native cast. Objects with the same keys share one extraction query.
Keys remain bound values, including empty and NUL keys.
-}
prepareVariantDecoder :: (DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue) -> DuckDBV2ConnectionHandle -> DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)
prepareVariantDecoder plain connection vector = do
    count <- output (c_duckdb_v2_vector_get_size vector)
    bracket (acquireItems destroyValue [output (c_duckdb_v2_vector_get_value vector i) | i <- indices count]) (releaseItems destroyValue) \values ->
        withTypeStatement connection \statement -> withCastTypes \castTypes -> do
            decoded <- decodeValues plain connection statement castTypes values
            let rows = listArray (0, length decoded - 1) decoded
            pure (evaluate . (rows !))

-- | A decoded node owns the child values needed at the next nesting level.
data VariantNode
    = ScalarNode FieldValue
    | ListNode [DuckDBV2ValueHandle]
    | ObjectNode [(Text, DuckDBV2ValueHandle)]

-- | Decode all siblings together before reconstructing their parent nodes.
decodeValues :: (DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue) -> DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> CastTypes -> [DuckDBV2ValueHandle] -> IO [FieldValue]
decodeValues _ _ _ _ [] = pure []
decodeValues plain connection statement castTypes values = do
    metadata <- queryMetadata plain connection statement values
    unless (length metadata == length values) (codecError "metadata query returned an unexpected number of nodes")
    let indexed = zip3 [0 ..] metadata values
        grouped key select = Map.toList (Map.fromListWith (++) [(key node, [(index, value)]) | (index, node@(tag, _), value) <- indexed, select tag])
        nulls = [(index, ScalarNode FieldNull) | (index, (tag, _), _) <- indexed, isNull tag]
        releaseIndexed = releaseNode . snd
    scalars <- mapFlat (uncurry decodeScalars) (grouped fst (\tag -> not (isNull tag || isArray tag || isObject tag)))
    bracket
        (acquireItems releaseIndexed [(,) index <$> decodeArray value | (index, (tag, _), value) <- indexed, isArray tag])
        (releaseItems releaseIndexed)
        \arrays ->
            bracket
                (acquireItems (releaseItems releaseIndexed) [queryObjectGroup connection keys rows | (keys, rows) <- grouped snd isObject])
                (releaseItems (releaseItems releaseIndexed))
                \objectGroups -> do
                    let indexedNodes = IntMap.fromList (nulls <> concat scalars <> arrays <> concat objectGroups)
                    nodes <- forM [0 .. length values - 1] \index ->
                        maybe (codecError "object extraction omitted a node") pure (IntMap.lookup index indexedNodes)
                    children <- decodeValues plain connection statement castTypes (concatMap nodeChildren nodes)
                    assemble nodes children
  where
    isNull = (== "VARIANT_NULL")
    isArray = Text.isPrefixOf "ARRAY("
    isObject = Text.isPrefixOf "OBJECT("
    decodeArray value =
        withCast connection castTypes value "VARIANT[]" \list -> do
            count <- output (c_duckdb_v2_value_get_child_count list)
            ListNode <$> acquireItems destroyValue [output (c_duckdb_v2_value_get_child list i) | i <- indices count]
    -- Cast the scalars of one tag as one list. The cast keeps the exact type.
    decodeScalars tag rows = do
        target <- scalarType tag
        withVariantList connection (map snd rows) \list ->
            withCast connection castTypes list (target <> "[]") \cast ->
                plain connection cast >>= \case
                    FieldList decoded | length decoded == length rows -> pure (zip (map fst rows) (map ScalarNode decoded))
                    _ -> codecError "scalar cast returned an unexpected value"

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
                (acquireItems destroyValue [withText key (output . c_duckdb_v2_value_create_varchar factory) | key <- keys])
                (releaseItems destroyValue)
                \keyValues ->
                    withSQLStatement connection sql \statement ->
                        withStatementValue connection statement (list : keyValues) \objects -> do
                            count <- output (c_duckdb_v2_value_get_child_count objects)
                            unless (toInteger count == toInteger (length rows)) (codecError "object extraction returned an unexpected row count")
                            acquireItems
                                (releaseNode . snd)
                                [ bracket (output (c_duckdb_v2_value_get_child objects i)) destroyValue \children -> do
                                    childCount <- output (c_duckdb_v2_value_get_child_count children)
                                    unless (toInteger childCount == toInteger (length keys)) (codecError "object extraction returned an unexpected child count")
                                    owned <- acquireItems destroyValue [output (c_duckdb_v2_value_get_child children j) | j <- indices childCount]
                                    pure (index, ObjectNode (zip keys owned))
                                | (i, (index, _)) <- zip [0 ..] rows
                                ]
  where
    sql = "SELECT system.main.list_transform($1::VARIANT[], lambda value: system.main.list_value(" <> Text.intercalate "," extracts <> ")) AS payload"
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

-- | Cast target types by type text. One decode parses each type once.
type CastTypes = IORef (Map.Map Text DuckDBV2LogicalTypeHandle)

-- | Destroy the cached cast types after the action.
withCastTypes :: (CastTypes -> IO a) -> IO a
withCastTypes = bracket (newIORef Map.empty) (\ref -> readIORef ref >>= mapM_ destroyLogicalType)

-- | Borrow the native cast result while copying its payload.
withCast :: DuckDBV2ConnectionHandle -> CastTypes -> DuckDBV2ValueHandle -> Text -> (DuckDBV2ValueHandle -> IO a) -> IO a
withCast connection castTypes value target action = do
    logical <-
        Map.lookup target <$> readIORef castTypes >>= \case
            Just cached -> pure cached
            Nothing -> mask_ do
                created <- withText target (\text -> factoryOutput connection (\factory -> c_duckdb_v2_factory_create_type_from_text factory text))
                modifyIORef' castTypes (Map.insert target created)
                pure created
    bracket (factoryOutput connection (\factory -> c_duckdb_v2_value_cast factory value logical)) destroyValue action

-- | Parse one metadata statement and reuse it for each nesting level.
withTypeStatement :: DuckDBV2ConnectionHandle -> (DuckDBV2SqlStatementHandle -> IO a) -> IO a
withTypeStatement connection =
    withSQLStatement
        connection
        "SELECT system.main.list_transform(?::VARIANT[], lambda value: system.main.struct_pack(tag := system.main.variant_typeof(value), keys := system.main.variant_keys(value))) AS metadata"

-- | Read exact scalar tags and object keys without serializing payloads.
queryMetadata :: (DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue) -> DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> [DuckDBV2ValueHandle] -> IO [(Text, [Text])]
queryMetadata plain connection statement values = withVariantList connection values \list ->
    withStatementValue connection statement [list] \metadata ->
        plain connection metadata >>= maybe (codecError "metadata query returned an unexpected value") pure . metadataNodes

{- | Read the tag and the object keys of each metadata node. A NULL key list
gives no keys. Return 'Nothing' for an unexpected shape.
-}
metadataNodes :: FieldValue -> Maybe [(Text, [Text])]
metadataNodes = \case
    FieldList nodes -> traverse node nodes
    _ -> Nothing
  where
    node = \case
        FieldStruct struct -> case elems (structValueFields struct) of
            [StructField "tag" (FieldText tag), StructField "keys" keys] -> (,) tag <$> keyList keys
            _ -> Nothing
        _ -> Nothing
    keyList = \case
        FieldNull -> Just []
        FieldList keys -> traverse keyText keys
        _ -> Nothing
    keyText = \case
        FieldText key -> Just key
        _ -> Nothing

-- | Borrow an owned VARIANT list that copies all supplied values.
withVariantList :: DuckDBV2ConnectionHandle -> [DuckDBV2ValueHandle] -> (DuckDBV2ValueHandle -> IO a) -> IO a
withVariantList connection values action =
    withConnectionFactory connection \factory ->
        bracket (withText "VARIANT" (output . c_duckdb_v2_factory_create_type_from_text factory)) (destroy c_duckdb_v2_logical_type_destroy) \logical ->
            withArray values \items ->
                bracket (output (c_duckdb_v2_value_create_list factory logical (ConstPtr items) (fromIntegral (length values)))) destroyValue action

-- | Borrow the first result value while its statement result stays alive.
withStatementValue :: DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> [DuckDBV2ValueHandle] -> (DuckDBV2ValueHandle -> IO a) -> IO a
withStatementValue connection statement parameters action =
    withArray parameters \items ->
        bracket (output (c_duckdb_v2_statement_execute connection statement (ConstPtr nullPtr) (ConstPtr items) (fromIntegral (length parameters)))) (destroy c_duckdb_v2_result_destroy) \result ->
            bracket (output (c_duckdb_v2_result_fetch_chunk result)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
                unless (chunk /= nullHandle) (codecError "metadata query returned no rows")
                vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                bracket (output (c_duckdb_v2_vector_get_value vector 0)) destroyValue action

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

-- | Acquire owned items without an asynchronous exception between transfers.
acquireItems :: (a -> IO ()) -> [IO a] -> IO [a]
acquireItems release = mask_ . go []
  where
    go owned [] = pure (reverse owned)
    go owned (acquire : rest) = do
        item <- acquire `onException` releaseItems release owned
        go (item : owned) rest

{- | Release every owned item even if one destructor reports an error. Raise
the first error after the last release. The loop keeps the stack flat.
-}
releaseItems :: forall a. (a -> IO ()) -> [a] -> IO ()
releaseItems release = go Nothing
  where
    go :: Maybe SomeException -> [a] -> IO ()
    go failure [] = mapM_ throwIO failure
    go failure (item : rest) = do
        outcome <- try (release item)
        go (either (\err -> Just (maybe err id failure)) (const failure) outcome) rest

-- | List a node's owned children in their native order.
nodeChildren :: VariantNode -> [DuckDBV2ValueHandle]
nodeChildren (ScalarNode _) = []
nodeChildren (ListNode values) = values
nodeChildren (ObjectNode entries) = map snd entries

-- | Release the children owned by a decoded node.
releaseNode :: VariantNode -> IO ()
releaseNode = releaseItems destroyValue . nodeChildren

-- | Report a metadata or payload mismatch before decoding a native value.
codecError :: Text -> IO a
codecError message = throwIO (SQLError ("duckdb-simple: VARIANT: " <> message) Nothing Nothing)
