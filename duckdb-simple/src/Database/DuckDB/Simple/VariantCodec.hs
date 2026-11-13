{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.VariantCodec
Description : Lossless VARIANT decoding through public V2 values and SQL type metadata.
-}
module Database.DuckDB.Simple.VariantCodec (
    VariantState,
    newVariantState,
    destroyVariantState,
    prepareVariantDecoder,
) where

import Control.Exception (bracket, evaluate, mask_, throwIO)
import Control.Monad (forM, unless)
import Data.Array (elems, listArray, (!))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), StructValue (..))
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeFromRep)
import Database.DuckDB.Simple.Variant (variantObject)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)

{- | Native objects that the VARIANT decoder keeps for one helper connection:
the parsed metadata statement and the types for casts and VARIANT lists. The
owner destroys them with 'destroyVariantState' before it destroys the
connection. Use the state from one thread at a time.
-}
data VariantState = VariantState
    { variantStatement :: !(IORef (Maybe DuckDBV2SqlStatementHandle))
    , variantTypes :: !CastTypes
    }

-- | Make an empty state. This allocates no native object.
newVariantState :: IO VariantState
newVariantState = VariantState <$> newIORef Nothing <*> newIORef Map.empty

{- | Destroy the statement and every cached type. Release every item even if
one destructor fails, and raise the first error after the last release. The
state is empty afterwards, so a second call does nothing.
-}
destroyVariantState :: VariantState -> IO ()
destroyVariantState VariantState{variantStatement, variantTypes} = mask_ do
    statement <- readIORef variantStatement
    types <- readIORef variantTypes
    writeIORef variantStatement Nothing
    writeIORef variantTypes Map.empty
    releaseItems id (map (destroy c_duckdb_v2_sql_statement_destroy) (maybe [] pure statement) <> map destroyLogicalType (Map.elems types))

{- | Copy a vector before returning its row reader. The connection is a dedicated
metadata connection. It must remain exclusively borrowed until this function
returns. The state belongs to that connection. The callback decodes a value
that contains no VARIANT.
Each level batches type and key metadata. Scalars with the same tag share
one native cast. Objects with the same keys share one native STRUCT cast. If a
key cannot be a STRUCT field name, the objects share one extraction query. That
query binds the keys as values, so empty and NUL keys stay exact.
-}
prepareVariantDecoder :: (DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue) -> DuckDBV2ConnectionHandle -> VariantState -> DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)
prepareVariantDecoder plain connection state vector = do
    count <- output (c_duckdb_v2_vector_get_size vector)
    bracket (acquireItems destroyValue [output (c_duckdb_v2_vector_get_value vector i) | i <- indices count]) (releaseItems destroyValue) \values -> do
        decoded <- decodeValues plain connection state values
        let rows = listArray (0, length decoded - 1) decoded
        pure (evaluate . (rows !))

-- | A decoded node owns the child values needed at the next nesting level.
data VariantNode
    = ScalarNode FieldValue
    | ListNode [DuckDBV2ValueHandle]
    | ObjectNode [(Text, DuckDBV2ValueHandle)]

-- | Decode all siblings together before reconstructing their parent nodes.
decodeValues :: (DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue) -> DuckDBV2ConnectionHandle -> VariantState -> [DuckDBV2ValueHandle] -> IO [FieldValue]
decodeValues _ _ _ [] = pure []
decodeValues plain connection state values = do
    metadata <- queryMetadata plain connection state values
    unless (length metadata == length values) (codecError "metadata query returned an unexpected number of nodes")
    let indexed = zip3 [0 ..] metadata values
        grouped key select = Map.toList (Map.fromListWith (++) [(key node, [(index, value)]) | (index, node@(tag, _), value) <- indexed, select tag])
        nulls = [(index, ScalarNode FieldNull) | (index, (tag, _), _) <- indexed, isNull tag]
        releaseIndexed = releaseNode . snd
    scalars <- mapM (uncurry decodeScalars) (grouped fst (\tag -> not (isNull tag || isArray tag || isObject tag)))
    bracket
        (acquireItems releaseIndexed [(,) index <$> decodeArray value | (index, (tag, _), value) <- indexed, isArray tag])
        (releaseItems releaseIndexed)
        \arrays ->
            bracket
                (acquireItems (releaseItems releaseIndexed) [queryObjectGroup connection castTypes keys rows | (keys, rows) <- grouped snd isObject])
                (releaseItems (releaseItems releaseIndexed))
                \objectGroups -> do
                    let indexedNodes = IntMap.fromList (nulls <> concat scalars <> arrays <> concat objectGroups)
                    nodes <- forM [0 .. length values - 1] \index ->
                        maybe (codecError "object extraction omitted a node") pure (IntMap.lookup index indexedNodes)
                    children <- decodeValues plain connection state (concatMap nodeChildren nodes)
                    assemble nodes children
  where
    castTypes = variantTypes state
    isNull = (== "VARIANT_NULL")
    isArray = Text.isPrefixOf "ARRAY("
    isObject = Text.isPrefixOf "OBJECT("
    decodeArray value =
        withCast connection castTypes value "VARIANT[]" \list -> do
            count <- output (c_duckdb_v2_value_get_child_count list)
            ListNode <$> acquireItems destroyValue [output (c_duckdb_v2_value_get_child list i) | i <- indices count]
    -- Cast the scalars of one tag as one list. The cast keeps the exact type.
    -- Cast a single scalar directly, because a list needs more native calls.
    decodeScalars tag rows = do
        target <- scalarType tag
        case rows of
            [(index, value)] -> withCast connection castTypes value target \cast -> do
                decoded <- plain connection cast
                pure [(index, ScalarNode decoded)]
            _ ->
                withVariantList connection castTypes (map snd rows) \list ->
                    withCast connection castTypes list (target <> "[]") \cast ->
                        plain connection cast >>= \case
                            FieldList decoded | length decoded == length rows -> pure (zip (map fst rows) (map ScalarNode decoded))
                            _ -> codecError "scalar cast returned an unexpected value"

{- | Extract the children of objects with one key layout. One native cast or
one extraction query extracts the children of all objects. Each object gets one
child for each key, in key order. Check the object count and the child count of
each object.
-}
queryObjectGroup :: DuckDBV2ConnectionHandle -> CastTypes -> [Text] -> [(Int, DuckDBV2ValueHandle)] -> IO [(Int, VariantNode)]
queryObjectGroup _ _ [] rows = pure [(index, ObjectNode []) | (index, _) <- rows]
queryObjectGroup connection castTypes keys rows =
    withVariantList connection castTypes (map snd rows) \list ->
        withObjectChildren connection keys list \objects -> do
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

{- | Borrow one value for each object in a VARIANT list. Each value has one
child for each key, in key order. If the keys are valid STRUCT field names, a
native cast to a STRUCT list makes the values. Otherwise an extraction query
makes them. The binder needs constant extraction keys. The query binds each
key once and applies the extractions to all objects. The bound keys can be
empty or contain NUL.
-}
withObjectChildren :: DuckDBV2ConnectionHandle -> [Text] -> DuckDBV2ValueHandle -> (DuckDBV2ValueHandle -> IO a) -> IO a
withObjectChildren connection keys list action
    | validStructFields keys =
        -- The cast is lenient. It matches a key and a field name without
        -- regard to ASCII case. It gives NULL for a missing key. It ignores an
        -- extra key. Thus a wrong field list does not cause an error. The
        -- result is exact only because the fields are the keys of each object
        -- and 'validStructFields' accepts them. Do not relax that predicate.
        withConnectionFactory connection \factory ->
            bracket (logicalTypeFromRep factory structList) destroyLogicalType \logical ->
                bracket (output (c_duckdb_v2_value_cast factory list logical)) destroyValue action
    | otherwise =
        withConnectionFactory connection \factory ->
            bracket
                (acquireItems destroyValue [withText key (output . c_duckdb_v2_value_create_varchar factory) | key <- keys])
                (releaseItems destroyValue)
                \keyValues ->
                    withSQLStatement connection sql \statement ->
                        withStatementValue connection statement (list : keyValues) action
  where
    structList = LogicalTypeList (LogicalTypeStruct (listArray (0, length keys - 1) [StructField key (LogicalTypeScalar LogicalTypeIdVariant) | key <- keys]))
    sql = "SELECT system.main.list_transform($1::VARIANT[], lambda value: system.main.list_value(" <> Text.intercalate "," extracts <> ")) AS payload"
    extracts = ["system.main.variant_extract(value,$" <> Text.pack (show index) <> "::VARCHAR)" | index <- [2 .. length keys + 1]]

{- | Tell if the keys can be the field names of one STRUCT type. A name must
not be empty and must not contain NUL. Two names must not be equal after ASCII
case folding.
-}
validStructFields :: [Text] -> Bool
validStructFields keys =
    all (\key -> not (Text.null key || Text.any (== '\0') key)) keys
        && Set.size (Set.fromList (map foldIdentifier keys)) == length keys

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

{- | Native types by type text, for casts and VARIANT lists. The map is small
and bounded. It holds the types of the fixed scalar tags and their list forms,
"VARIANT" and "VARIANT[]", and one DECIMAL type and its list form for each
width and scale. It never holds a STRUCT type.
-}
type CastTypes = IORef (Map.Map Text DuckDBV2LogicalTypeHandle)

-- | Borrow the native cast result while copying its payload.
withCast :: DuckDBV2ConnectionHandle -> CastTypes -> DuckDBV2ValueHandle -> Text -> (DuckDBV2ValueHandle -> IO a) -> IO a
withCast connection castTypes value target action = do
    logical <- castType connection castTypes target
    bracket (factoryOutput connection (\factory -> c_duckdb_v2_value_cast factory value logical)) destroyValue action

-- | Borrow a type from the cache. Parse the type text on its first use.
castType :: DuckDBV2ConnectionHandle -> CastTypes -> Text -> IO DuckDBV2LogicalTypeHandle
castType connection castTypes target =
    Map.lookup target <$> readIORef castTypes >>= \case
        Just cached -> pure cached
        Nothing -> mask_ do
            created <- withText target (\text -> factoryOutput connection (\factory -> c_duckdb_v2_factory_create_type_from_text factory text))
            modifyIORef' castTypes (Map.insert target created)
            pure created

{- | Borrow the metadata statement. Parse it on first use and keep it in the
state. The statement stays valid after its iterator is destroyed, and an
execution does not consume it.
-}
metadataStatement :: DuckDBV2ConnectionHandle -> VariantState -> IO DuckDBV2SqlStatementHandle
metadataStatement connection VariantState{variantStatement} =
    readIORef variantStatement >>= \case
        Just statement -> pure statement
        -- Store the statement before the iterator is destroyed, so that the
        -- state owns it even if that destroy call fails.
        Nothing -> mask_ $ TextForeign.withCString sql \source ->
            bracket (output (c_duckdb_v2_parse_sql connection (ConstPtr source))) (destroy c_duckdb_v2_statement_iterator_destroy) \iterator -> do
                statement <- output (c_duckdb_v2_statement_iterator_next iterator)
                writeIORef variantStatement (Just statement)
                pure statement
  where
    sql = "SELECT system.main.list_transform(?::VARIANT[], lambda value: system.main.struct_pack(tag := system.main.variant_typeof(value), keys := system.main.variant_keys(value))) AS metadata"

-- | Read exact scalar tags and object keys without serializing payloads.
queryMetadata :: (DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue) -> DuckDBV2ConnectionHandle -> VariantState -> [DuckDBV2ValueHandle] -> IO [(Text, [Text])]
queryMetadata plain connection state values = withVariantList connection (variantTypes state) values \list -> do
    statement <- metadataStatement connection state
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
withVariantList :: DuckDBV2ConnectionHandle -> CastTypes -> [DuckDBV2ValueHandle] -> (DuckDBV2ValueHandle -> IO a) -> IO a
withVariantList connection castTypes values action = do
    logical <- castType connection castTypes "VARIANT"
    withConnectionFactory connection \factory ->
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
