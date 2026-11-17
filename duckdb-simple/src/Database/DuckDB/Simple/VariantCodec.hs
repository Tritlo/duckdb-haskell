{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Database.DuckDB.Simple.VariantCodec
-- Description : Lossless VARIANT decoding through public V2 values and SQL type metadata.
module Database.DuckDB.Simple.VariantCodec (
  VariantState,
  newVariantState,
  destroyVariantState,
  prepareVariantDecoder,
) where

import Control.Exception (bracket, evaluate, mask_)
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
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)

{- Note [VARIANT decoding]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The decoder copies each VARIANT row of a vector to a FieldValue with the native
type of the stored value. It uses SQL metadata and native casts on a helper
connection. It does not read the private payload format of DuckDB.

- Why SQL. The header commits to no view layout for VARIANT, so the decoder
  reads each row with duckdb_v2_vector_get_value. V2 has no accessor for the
  children of a VARIANT value. SQL functions and native casts give the tag, the
  keys, and the children of a value.

- The metadata statement (metadataStatement). One statement gives the tag
  (variant_typeof) and the object keys (variant_keys) of each value in a
  VARIANT list. The state keeps one prepared statement, so executions reuse
  its bound plan. Its parameter has the fixed type VARIANT[]. The decoder
  reads the borrowed result vector before its chunk closes. This avoids copying the
  complete metadata list into an owned value and then into another vector.

- The casts. The tag of a scalar is the text of variant_typeof. scalarType
  gives the type text of the exact cast target of each tag. The scalars with
  the same tag share one cast to a list of the target type. A tag with one
  scalar gets a direct cast of that scalar. Each array gets its own cast to a
  VARIANT list. The state keeps each parsed type (castType).

- The objects (withObjectChildren). The objects with the same keys share one
  extraction. If the keys can be STRUCT field names (validStructFields), one
  cast to a STRUCT list extracts the children. Otherwise one extraction query
  binds the keys as values, so empty and NUL keys stay exact. The SQL text of
  the query depends only on the key count. The state keeps the parsed
  extraction statement of the last key count (extractionStatement). A group
  with another key count replaces that statement. The state keeps one
  extraction statement only. Thus the memory of the state does not grow with
  the number of key counts in the data. An object without keys needs no
  extraction (queryObjectGroup).

- The levels (decodeValues). The decoder reads the metadata of all values of
  one nesting level together. Then it decodes the children of all nodes of the
  level together, and then it builds the nodes of the level (assemble).

The helper connection. The queries of the decoder do not run on the caller
connection. An explicit stream keeps its result open on the caller connection
between two fetches, and the decoder reads the rows of each chunk while that
result is open (fetchChunk in "Database.DuckDB.Simple.Result"). DuckDB permits
one open result for each connection, and it rejects a second execution. Thus
the queries of the decoder run on a helper connection, so that the decode does
not interrupt the stream. See Note [Connection lock] in
"Database.DuckDB.Simple.Internal". A materialized result closes before its rows
are decoded, but it uses the same reader. The helper connection has the
settings of the database, not the SESSION settings of the caller. The
metadata query needs an expression depth of 11 when it is prepared. A GLOBAL
max_expression_depth below 11 fails its preparation with a Binder Error.

VariantState holds the helper connection, the prepared metadata statement, the
parsed extraction statement of the last key count, and the cast types. The
owner of the helper connection destroys the state before the connection.

- A caller connection (CacheState and withVariantState). The type cache of the
  connection opens the helper connection and the state at the first VARIANT
  read, and keeps them until the cache closes. The calls run in sequence.

- COPY TO (withVariantReader). The reader opens a helper connection of the
  database for each vector, and keeps the state for one decode. An extension
  target has no instance handle, so its reader raises an error for each
  VARIANT row.

The decoder casts with the factory of the instance (variantCasts). It decodes
each value that contains no VARIANT, such as a cast result, with the same
factory. See Note [User casts and the casts of the library] in
"Database.DuckDB.Simple.Materialize".
-}

-- | The helper connection of the VARIANT decoder, and the native objects that
-- the decoder keeps for it: the prepared metadata statement, the parsed
-- extraction statement of the last key count, and the types for casts and
-- VARIANT lists. The state borrows the connection. The owner destroys
-- the state with 'destroyVariantState' before it destroys the connection. Use
-- the state from one thread at a time. See Note [VARIANT decoding].
data VariantState = VariantState
  { variantConnection :: DuckDBV2ConnectionHandle
  , variantCasts :: DuckDBV2FactoryHandle
  -- ^ The factory of the instance of the helper connection. The decoder casts
  --     with it.
  , variantStatement :: IORef (Maybe DuckDBV2PreparedStatementHandle)
  , variantExtraction :: IORef (Maybe (Int, DuckDBV2SqlStatementHandle))
  , variantTypes :: CastTypes
  }

-- | Make an empty state for a helper connection and the factory of its
-- instance. This allocates no native object.
newVariantState :: DuckDBV2ConnectionHandle -> DuckDBV2FactoryHandle -> IO VariantState
newVariantState connection casts = VariantState connection casts <$> newIORef Nothing <*> newIORef Nothing <*> newIORef Map.empty

-- | Destroy the prepared metadata statement, the extraction statement, and
-- every cached type. Release every item even if one destructor fails, and raise the first
-- error after the last release. The state is empty afterwards, so a second call
-- does nothing.
destroyVariantState :: VariantState -> IO ()
destroyVariantState VariantState{variantStatement, variantExtraction, variantTypes} = mask_ do
  statement <- readIORef variantStatement
  extraction <- readIORef variantExtraction
  types <- readIORef variantTypes
  writeIORef variantStatement Nothing
  writeIORef variantExtraction Nothing
  writeIORef variantTypes Map.empty
  releaseItems
    id
    ( map (destroy c_duckdb_v2_prepared_statement_destroy) (maybe [] pure statement)
        <> map (destroy c_duckdb_v2_sql_statement_destroy . snd) (maybe [] pure extraction)
        <> map destroyLogicalType (Map.elems types)
    )

-- | Copy a vector before returning its row reader. The connection of the state
-- is a dedicated helper connection. It must remain exclusively borrowed until
-- this function returns. The callbacks decode a value, or the first row of a
-- borrowed vector, that contains no VARIANT. They use the factory they get and
-- complete all native reads before returning. Each level batches type and key
-- metadata. See Note [VARIANT decoding].
prepareVariantDecoder
  :: (DuckDBV2FactoryHandle -> DuckDBV2ValueHandle -> IO FieldValue)
  -> (DuckDBV2FactoryHandle -> DuckDBV2VectorHandle -> IO FieldValue)
  -> VariantState
  -> DuckDBV2VectorHandle
  -> IO (Int -> IO FieldValue)
prepareVariantDecoder plain plainVector state vector = do
  count <- output (c_duckdb_v2_vector_get_size vector)
  bracket (acquireItems destroyValue [output (c_duckdb_v2_vector_get_value vector i) | i <- indices count]) (releaseItems destroyValue) \values -> do
    decoded <- decodeValues plain plainVector state values
    let rows = listArray (0, length decoded - 1) decoded
    -- Give the element, not a thunk that keeps the array alive.
    pure (evaluate . (rows !))

-- | A decoded node owns the child values needed at the next nesting level.
data VariantNode
  = ScalarNode FieldValue
  | ListNode [DuckDBV2ValueHandle]
  | ObjectNode [(Text, DuckDBV2ValueHandle)]

-- | Decode all siblings together before reconstructing their parent nodes.
-- See Note [VARIANT decoding].
decodeValues
  :: (DuckDBV2FactoryHandle -> DuckDBV2ValueHandle -> IO FieldValue)
  -> (DuckDBV2FactoryHandle -> DuckDBV2VectorHandle -> IO FieldValue)
  -> VariantState
  -> [DuckDBV2ValueHandle]
  -> IO [FieldValue]
decodeValues _ _ _ [] = pure []
decodeValues plain plainVector state values = do
  metadata <- queryMetadata plainVector state values
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
        (acquireItems (releaseItems releaseIndexed) [queryObjectGroup state keys rows | (keys, rows) <- grouped snd isObject])
        (releaseItems (releaseItems releaseIndexed))
        \objectGroups -> do
          let indexedNodes = IntMap.fromList (nulls <> concat scalars <> arrays <> concat objectGroups)
          nodes <- forM [0 .. length values - 1] \index ->
            maybe (codecError "object extraction omitted a node") pure (IntMap.lookup index indexedNodes)
          children <- decodeValues plain plainVector state (concatMap nodeChildren nodes)
          assemble nodes children
  where
    casts = variantCasts state
    isNull = (== "VARIANT_NULL")
    isArray = Text.isPrefixOf "ARRAY("
    isObject = Text.isPrefixOf "OBJECT("
    decodeArray value =
      withCast state value "VARIANT[]" \list -> do
        count <- output (c_duckdb_v2_value_get_child_count list)
        ListNode <$> acquireItems destroyValue [output (c_duckdb_v2_value_get_child list i) | i <- indices count]
    -- Cast the scalars of one tag as one list. The cast keeps the exact type.
    -- Cast a single scalar directly, because a list needs more native calls.
    decodeScalars tag rows = do
      target <- scalarType tag
      case rows of
        [(index, value)] -> withCast state value target \cast -> do
          -- The cast fixes the type. Read these payloads without an owned vector.
          decoded <- case tag of
            "INT64" -> FieldInt64 <$> output (c_duckdb_v2_value_get_bigint cast)
            "UINT64" -> FieldWord64 <$> output (c_duckdb_v2_value_get_ubigint cast)
            _ -> plain casts cast
          pure [(index, ScalarNode decoded)]
        _ ->
          withVariantList state (map snd rows) \list ->
            withCast state list (target <> "[]") \cast ->
              plain casts cast >>= \case
                FieldList decoded | length decoded == length rows -> pure (zip (map fst rows) (map ScalarNode decoded))
                _ -> codecError "scalar cast returned an unexpected value"

-- | Extract the children of objects with one key layout. One native cast or
-- one extraction query extracts the children of all objects. Each object gets one
-- child for each key, in key order. Check the object count and the child count of
-- each object.
queryObjectGroup :: VariantState -> [Text] -> [(Int, DuckDBV2ValueHandle)] -> IO [(Int, VariantNode)]
queryObjectGroup _ [] rows = pure [(index, ObjectNode []) | (index, _) <- rows]
queryObjectGroup state keys rows =
  withVariantList state (map snd rows) \list ->
    withObjectChildren state keys list \objects -> do
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

-- | Borrow one value for each object in a VARIANT list. Each value has one
-- child for each key, in key order. If the keys are valid STRUCT field names, a
-- native cast to a STRUCT list makes the values. Otherwise an extraction query
-- makes them. The binder needs constant extraction keys. The query binds each
-- key once and applies the extractions to all objects. The bound keys can be
-- empty or contain NUL. The state keeps the parsed extraction statement of the
-- last key count ('extractionStatement'). A group with another key count
-- replaces it. V2 has no accessor for the children of a VARIANT value. The
-- STRUCT cast is the fast path. The query is the only exact path for the other
-- key lists. See Note [VARIANT decoding].
withObjectChildren :: VariantState -> [Text] -> DuckDBV2ValueHandle -> (DuckDBV2ValueHandle -> IO a) -> IO a
withObjectChildren state@VariantState{variantConnection = connection} keys list action
  | validStructFields keys =
      -- The cast is lenient. It matches a key and a field name without
      -- regard to ASCII case. It gives NULL for a missing key. It ignores an
      -- extra key. Thus a wrong field list does not cause an error. The
      -- result is exact only because the fields are the keys of each object
      -- and 'validStructFields' accepts them. Do not relax that predicate.
      withConnectionFactory connection \factory ->
        bracket (logicalTypeFromRep factory structList) destroyLogicalType \logical ->
          bracket (output (c_duckdb_v2_value_cast (variantCasts state) list logical)) destroyValue action
  | otherwise =
      withConnectionFactory connection \factory ->
        bracket
          (acquireItems destroyValue [withText key (output . c_duckdb_v2_value_create_varchar factory) | key <- keys])
          (releaseItems destroyValue)
          \keyValues -> do
            statement <- extractionStatement state (length keys)
            withStatementValue connection statement (list : keyValues) action
  where
    structList =
      LogicalTypeList
        (LogicalTypeStruct (listArray (0, length keys - 1) [StructField key (LogicalTypeScalar LogicalTypeIdVariant) | key <- keys]))

-- | Tell if the keys can be the field names of one STRUCT type. A name must
-- not be empty and must not contain NUL. Two names must not be equal after ASCII
-- case folding.
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
  -- Build the constructor of this node now. This reads no native value and
  -- cannot fail.
  value <- evaluate (build ownChildren)
  (value :) <$> assemble rest remaining

-- | Native types by type text, for casts and VARIANT lists. The map is small
-- and bounded. It holds the types of the fixed scalar tags and their list forms,
-- "VARIANT" and "VARIANT[]", and one DECIMAL type and its list form for each
-- width and scale. It never holds a STRUCT type.
type CastTypes = IORef (Map.Map Text DuckDBV2LogicalTypeHandle)

-- | Borrow the native cast result while copying its payload. The cast uses the
-- factory of the instance. See Note [User casts and the casts of the library]
-- in "Database.DuckDB.Simple.Materialize".
withCast :: VariantState -> DuckDBV2ValueHandle -> Text -> (DuckDBV2ValueHandle -> IO a) -> IO a
withCast state value target action = do
  logical <- castType state target
  bracket (output (c_duckdb_v2_value_cast (variantCasts state) value logical)) destroyValue action

-- | Borrow a type from the cache. Parse the type text on its first use.
castType :: VariantState -> Text -> IO DuckDBV2LogicalTypeHandle
castType VariantState{variantConnection = connection, variantTypes = castTypes} target =
  Map.lookup target <$> readIORef castTypes >>= \case
    Just cached -> pure cached
    Nothing -> mask_ do
      created <- withText target (\text -> factoryOutput connection (\factory -> c_duckdb_v2_factory_create_type_from_text factory text))
      modifyIORef' castTypes (Map.insert target created)
      pure created

-- | Borrow the prepared metadata statement. Prepare it on first use and keep
-- it in the state. An execution does not consume it. See Note [VARIANT decoding].
metadataStatement :: VariantState -> IO DuckDBV2PreparedStatementHandle
metadataStatement VariantState{variantConnection = connection, variantStatement} =
  readIORef variantStatement >>= \case
    Just statement -> pure statement
    Nothing -> mask_ $ parseKept connection sql \parsed ->
      bracket (pure parsed) (destroy c_duckdb_v2_sql_statement_destroy) \statement -> do
        prepared <- output (c_duckdb_v2_prepared_statement_create connection statement 0)
        writeIORef variantStatement (Just prepared)
        pure prepared
  where
    -- See Note [SQL text of the library] in "Database.DuckDB.Simple.Native".
    sql =
      "SELECT system.main.list_transform(?::VARIANT[], lambda value: system.main.struct_pack(tag := system.main.variant_typeof(value), keys := system.main.variant_keys(value))) AS metadata"

-- | Borrow the extraction statement for a key count. The state keeps the
-- statement of the last key count. A call with another key count parses a new
-- statement and destroys the old statement. The statement is valid until the
-- next call of this function or 'destroyVariantState'. See
-- Note [VARIANT decoding].
extractionStatement :: VariantState -> Int -> IO DuckDBV2SqlStatementHandle
extractionStatement VariantState{variantConnection = connection, variantExtraction} count =
  readIORef variantExtraction >>= \case
    Just (kept, statement) | kept == count -> pure statement
    previous ->
      -- Store the new statement before the old statement is destroyed. If the
      -- parse fails, the slot keeps the old statement. If the destroy of the
      -- old statement fails, the error propagates. Then the slot holds the new
      -- statement. Nothing destroys the old statement again, as in
      -- 'destroyVariantState'.
      mask_ $ parseKept connection sql \statement -> do
        writeIORef variantExtraction (Just (count, statement))
        mapM_ (destroy c_duckdb_v2_sql_statement_destroy . snd) previous
        pure statement
  where
    -- See Note [SQL text of the library] in "Database.DuckDB.Simple.Native".
    sql =
      "SELECT system.main.list_transform($1::VARIANT[], lambda value: system.main.list_value(" <> Text.intercalate "," extracts <> ")) AS payload"
    extracts = ["system.main.variant_extract(value,$" <> Text.pack (show index) <> "::VARCHAR)" | index <- [2 .. count + 1]]

-- | Parse trusted SQL and give its first statement to the action before the
-- iterator closes. The action must keep or destroy the statement, also when
-- the iterator destroy fails. Call this function under 'mask_'.
parseKept :: DuckDBV2ConnectionHandle -> Text -> (DuckDBV2SqlStatementHandle -> IO a) -> IO a
parseKept connection sql keep =
  TextForeign.withCString sql \source ->
    bracket (output (c_duckdb_v2_parse_sql connection (ConstPtr source))) (destroy c_duckdb_v2_statement_iterator_destroy) \iterator -> do
      statement <- output (c_duckdb_v2_statement_iterator_next iterator)
      keep statement

-- | Read exact scalar tags and object keys without serializing payloads.
queryMetadata
  :: (DuckDBV2FactoryHandle -> DuckDBV2VectorHandle -> IO FieldValue)
  -> VariantState
  -> [DuckDBV2ValueHandle]
  -> IO [(Text, [Text])]
queryMetadata plainVector state values = withVariantList state values \list -> do
  statement <- metadataStatement state
  with list \items ->
    withResultVector
      (output (c_duckdb_v2_prepared_statement_execute statement (ConstPtr nullPtr) (ConstPtr items) 1))
      \vector ->
        plainVector (variantCasts state) vector >>= maybe (codecError "metadata query returned an unexpected value") pure . metadataNodes

-- | Read the tag and the object keys of each metadata node. A NULL key list
-- gives no keys. Return 'Nothing' for an unexpected shape.
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
withVariantList :: VariantState -> [DuckDBV2ValueHandle] -> (DuckDBV2ValueHandle -> IO a) -> IO a
withVariantList state values action = do
  logical <- castType state "VARIANT"
  withConnectionFactory (variantConnection state) \factory ->
    withArray values \items ->
      bracket (output (c_duckdb_v2_value_create_list factory logical (ConstPtr items) (fromIntegral (length values)))) destroyValue action

-- | Borrow the first result value while its statement result stays alive.
withStatementValue
  :: DuckDBV2ConnectionHandle -> DuckDBV2SqlStatementHandle -> [DuckDBV2ValueHandle] -> (DuckDBV2ValueHandle -> IO a) -> IO a
withStatementValue connection statement parameters action =
  withArray parameters \items ->
    withResultVector
      (output (c_duckdb_v2_statement_execute connection statement (ConstPtr nullPtr) (ConstPtr items) (fromIntegral (length parameters))))
      \vector -> bracket (output (c_duckdb_v2_vector_get_value vector 0)) destroyValue action

-- | Borrow the first result vector while its result and chunk stay alive.
-- Complete native reads before the action returns.
withResultVector :: IO DuckDBV2ResultHandle -> (DuckDBV2VectorHandle -> IO a) -> IO a
withResultVector execute action =
  bracket execute (destroy c_duckdb_v2_result_destroy) \result ->
    bracket (output (c_duckdb_v2_result_fetch_chunk result)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
      unless (chunk /= nullHandle) (codecError "helper query returned no rows")
      vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
      action vector

-- | Map every native VARIANT scalar tag to its exact cast target. The tags are
-- the text of variant_typeof. The result is type text, because a DECIMAL tag
-- holds its width and scale. 'castType' parses each text once for each state.
-- See Note [VARIANT decoding].
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
codecError message = throwInternalError ("VARIANT: " <> message)
