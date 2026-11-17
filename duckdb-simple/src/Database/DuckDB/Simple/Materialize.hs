{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Database.DuckDB.Simple.Materialize
-- Description : Readers that copy native vectors and values to field values.
module Database.DuckDB.Simple.Materialize (
  VectorReader,
  FallbackReaders (..),
  rejectRows,
  prepareVariantReader,
  readCallbackVariant,
  readInstanceVariant,
  createHelperInstance,
  ReadFactory (..),
  callbackReadFactory,
  readFactoryHandle,
  prepareGeometryReader,
  materializeValue,
  prepareValueReader,
  prepareVectorReader,
  TypeTree,
  EnumLabels,
  prepareVectorReaderWith,
) where

import Control.Exception (bracket)
import Control.Monad (foldM, forM, when)
import Data.Array (Array, bounds, elems, listArray, rangeSize, (!))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Element (
  chunkIsRowValid,
  decodeElement,
  duckDBHugeIntToInteger,
  vectorElementType,
  withVectorType,
 )
import Database.DuckDB.Simple.FromField (
  DecimalValue (..),
  FieldValue (..),
  RawGeometry (..),
 )
import Database.DuckDB.Simple.LogicalRep (
  LogicalTypeRep (..),
  StructField (..),
  StructValue (..),
  TupleValue (..),
  UnionMemberType (..),
  UnionValue (..),
  aliasBase,
  logicalTypeSQL,
 )
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeToBaseRep, logicalTypeToRep)
import qualified Database.DuckDB.Simple.VariantCodec as VariantCodec
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..), peekElemOff)
import GHC.Records (getField)

-- | Prepare a reader for the rows of a vector. The reader must not outlive the vector.
type VectorReader = DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)

-- | Readers that need state outside the vector. Type metadata needs a factory,
-- VARIANT needs a helper connection, and GEOMETRY needs a factory for casts.
-- See Note [Type trees]. For VARIANT, see Note [VARIANT decoding] in
-- "Database.DuckDB.Simple.VariantCodec".
data FallbackReaders = FallbackReaders
  { typeReader :: DuckDBV2LogicalTypeHandle -> IO LogicalTypeRep
  , variantReader :: VectorReader
  , geometryReader :: VectorReader
  }

-- | A reader for a type that the caller cannot read. It reads each row when the
-- row is read, not when the vector is prepared. A NULL row gives 'FieldNull',
-- and a row that holds a value raises the error. Thus a value of a nested type
-- with no value of that type, such as an empty VARIANT list or a NULL STRUCT,
-- gives its value. The header commits to no view layout for VARIANT and
-- GEOMETRY, so the reader reads each row with @duckdb_v2_vector_get_value@,
-- whose row is logical. 'prepareVectorReaderWith' flattens the vector, so the
-- row that the reader gets is the logical row. See Note [Vector flatten]. The
-- reader creates and destroys one native value for each row.
rejectRows :: IO FieldValue -> VectorReader
rejectRows raise vector = pure \row ->
  bracket (output (c_duckdb_v2_vector_get_value vector (fromIntegral row))) destroyValue \value -> do
    isNull <- output (c_duckdb_v2_value_is_null value)
    if isNull /= 0 then pure FieldNull else raise

-- | Copy VARIANT rows before releasing the borrowed helper connection. The
-- state belongs to that connection. The decoder reads logical rows. See
-- Note [Vector flatten] and Note [VARIANT decoding] in
-- "Database.DuckDB.Simple.VariantCodec".
prepareVariantReader :: VariantCodec.VariantState -> VectorReader
prepareVariantReader =
  VariantCodec.prepareVariantDecoder
    (materializeValue noVariant . InstanceFactory)
    ( \factory vector ->
        prepareVectorReader (FallbackReaders (logicalTypeToRep factory) noVariant (prepareGeometryReader (InstanceFactory factory))) vector
          >>= ($ 0)
    )
  where
    noVariant = rejectRows (throwInternalError (Text.pack "expected a value without VARIANT"))

{- Note [VARIANT reads inside callbacks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The VARIANT decoder runs SQL. A callback cannot run it on the executing
connection. Even a new connection to the host instance can need a lock that
the caller holds, for example during SET GLOBAL. An extension has no host
instance handle at all.

The decoder uses an instance with only built-in types, casts and system
functions. A scalar or aggregate registration opens its instance on demand.
Each vector uses a separate connection on that instance. Other typed
callbacks, including extension COPY TO, open a private instance for each
VARIANT vector. The decoder copies the rows before it closes the connection.
See Note [Function argument readers] in "Database.DuckDB.Simple.Typed"
and Note [VARIANT decoding] in "Database.DuckDB.Simple.VariantCodec".
-}

-- | Decode a callback's VARIANT vector independently of the invoking database.
readCallbackVariant :: VectorReader
readCallbackVariant vector =
  bracket (output c_duckdb_v2_environment_create) (destroy c_duckdb_v2_environment_destroy) \env ->
    bracket (createHelperInstance env) (destroy c_duckdb_v2_instance_destroy) (`readInstanceVariant` vector)

-- | Copy a VARIANT vector through a temporary connection on the supplied instance.
readInstanceVariant :: DuckDBV2InstanceHandle -> VectorReader
readInstanceVariant instance_ vector =
  bracket (output (c_duckdb_v2_connection_create instance_)) (destroy c_duckdb_v2_connection_destroy) \helper ->
    withInstanceFactory instance_ \factory ->
      bracket (VariantCodec.newVariantState helper factory) VariantCodec.destroyVariantState (`prepareVariantReader` vector)

-- | Create an instance for synchronous metadata work, without background workers.
createHelperInstance :: DuckDBV2EnvironmentHandle -> IO DuckDBV2InstanceHandle
createHelperInstance env =
  withTextArray (map Text.pack ["threads", "async_threads"]) \names ->
    withTextArray (map Text.pack ["1", "0"]) \values ->
      output (c_duckdb_v2_instance_create_with_options env (ConstPtr names) (ConstPtr values) 2)

-- | The factory of a read, and its kind. The kind decides how
-- 'prepareGeometryReader' casts GEOMETRY to BLOB. See
-- Note [User casts and the casts of the library].
data ReadFactory
  = -- | The factory of an instance. Its casts are the built-in casts.
    InstanceFactory DuckDBV2FactoryHandle
  | -- | The factory of a callback context. Its casts include the casts that a
    -- user registers.
    ContextFactory DuckDBV2FactoryHandle

-- | The factory of a read in a callback: the factory of the instance from
-- 'Database.DuckDB.Simple.Callback.targetCastFactory', or the factory of the
-- context for an extension target, which has no instance.
callbackReadFactory :: Maybe DuckDBV2FactoryHandle -> DuckDBV2FactoryHandle -> ReadFactory
callbackReadFactory casts context = maybe (ContextFactory context) InstanceFactory casts

-- | The handle of a read factory.
readFactoryHandle :: ReadFactory -> DuckDBV2FactoryHandle
readFactoryHandle (InstanceFactory factory) = factory
readFactoryHandle (ContextFactory factory) = factory

-- | Copy one value through an owned vector. The caller retains the factory owner.
-- The first argument reads VARIANT. 'prepareGeometryReader' reads GEOMETRY with
-- the factory.
materializeValue :: VectorReader -> ReadFactory -> DuckDBV2ValueHandle -> IO FieldValue
materializeValue readVariant factory value =
  bracket (output (c_duckdb_v2_value_get_logical_type value)) (destroy c_duckdb_v2_logical_type_destroy) \logical ->
    withOwnedVector (readFactoryHandle factory) logical \vector -> do
      checked (c_duckdb_v2_vector_set_size vector 1)
      checked (c_duckdb_v2_vector_set_value vector 0 value)
      prepareVectorReader (FallbackReaders (logicalTypeToRep (readFactoryHandle factory)) readVariant (prepareGeometryReader factory)) vector
        >>= ($ 0)

{- Note [Type trees]
~~~~~~~~~~~~~~~~~~~~
A reader of a vector uses two kinds of type information.

- A layout parameter gives the layout of the bytes, for example the width of a
  DECIMAL, the size of an ARRAY, or the size of an ENUM dictionary. A decoder
  takes each layout parameter from the type of its own vector, because that
  type gives the layout of the bytes. The header of duckdb_v2_vector_get_view
  says that the data has the layout that the logical type of the vector
  implies.

- A description is the field names and field types of a STRUCT or TUPLE, or
  the members of a UNION. A decoder takes a description from a type tree: the
  LogicalTypeRep that an action of type TypeTree gives. A decoder runs the
  action only when it needs a description.

Thus a wrong tree gives an error or a wrong description, but it does not change
how bytes are read. checkChildCount raises an error when a tree gives another
child count than the vector has.

These are the sources of a tree.

- prepareVectorReader converts the type of the vector.
- A caller of prepareVectorReaderWith gives the action. A result converts the
  tree of a column from the vector of the first chunk that needs it, and keeps
  the tree for one execution (prepareChunkReaders).
- A nested decoder gives each child an action that takes the tree of the child
  from its own tree (elementTree, prepareStructDecoder, prepareTupleDecoder, and
  prepareUnionDecoder). The action of a child runs the action of the parent
  only when the child needs its tree.
- The MAP decoder gives its key reader and its value reader no tree, and the
  UNION decoder gives its tag reader no tree. Each of these readers converts
  the type of its own vector when it needs a description.

The readers do not use the result schema. The header of
duckdb_v2_result_get_schema calls the schema the authoritative source of the
column types, and says that vectors do not carry their own type. But the header
of duckdb_v2_vector_get_logical_type says that a vector has the type that was
set on creation, and the layout follows the type of the vector. So the vector
type is the source of each type.
-}

-- | An action that gives the type tree of one vector. A decoder runs it only
-- when it needs a description. See Note [Type trees].
type TypeTree = IO LogicalTypeRep

-- | Read the type, data, and validity of a vector, and prepare its reader. A
-- decoder that needs the type tree converts the type of the vector. See
-- Note [Type trees].
prepareVectorReader :: FallbackReaders -> DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)
prepareVectorReader readers vector = do
  -- The cache of ENUM labels lives as long as this reader. See Note [ENUM labels].
  labels <- newIORef IntMap.empty
  -- See Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
  prepareVectorReaderWith readers (withVectorType vector (typeReader readers)) (Just labels) vector

-- | The labels of an ENUM type that readers read, by index. 'Nothing' means
-- that the type tree holds the labels. See Note [ENUM labels].
type EnumLabels = Maybe (IORef (IntMap.IntMap Text))

{- Note [Vector flatten]
~~~~~~~~~~~~~~~~~~~~~~~~
vector_flatten changes a vector to the FLAT representation in place. A reader
flattens each vector before it reads the view. A writer of a callback output
vector flattens the vector before it writes rows.

Read (prepareVectorReaderWith).

- The header of duckdb_v2_vector_get_view says: "Rejects VECTOR_TYPE_OTHER;
  call vector_flatten first." The flatten also changes a CONSTANT or DICTIONARY
  vector to FLAT.

- In DuckDB 2.0.0 the view of a FLAT vector has no selection
  (duckdb_v2_vector_get_view in src/main/capi/v2/capi_v2_vector.cpp). The header
  does not say this. Then the physical row is the logical row. The readers of
  VARIANT and GEOMETRY need this, because they read logical rows (rejectRows,
  prepareVariantReader, and prepareGeometryReader). The reader still applies
  the selection of a view that has one.

- The flatten changes the buffers of the vector, so it runs before the view is
  read.

- In DuckDB 2.0.0 the flatten is recursive: it also flattens the child of a
  LIST or MAP vector and the fields of a STRUCT vector (VectorListBuffer::Flatten
  and VectorStructBuffer::Flatten). The header does not say this. The MAP
  decoder (prepareMapDecoder) depends on this, because it takes the key and
  value vectors from the entry STRUCT and does not flatten that STRUCT itself.

Write. A writer of a callback output vector sets the size of the vector, and
then flattens the vector (writeRows and writeResults). A writer of an
owned vector from withOwnedVector does not flatten (materializeValue, and the
writers of one-row vectors in "Database.DuckDB.Simple.ToField").

- The writer sets the size before the flatten. In DuckDB 2.0.0,
  duckdb_v2_vector_flatten flattens the rows of the current size of the vector:
  it calls Vector::Flatten without a row count
  (src/main/capi/v2/capi_v2_vector.cpp). duckdb_v2_vector_set_size reserves
  memory only for a vector that is already FLAT. Thus after this order the
  vector is FLAT and has the new number of rows. The header gives no order.

- A new vector of a data chunk is FLAT. The header of
  duckdb_v2_data_chunk_create says: "Allocates one FLAT vector per element of
  the types array". Thus the writer of an owned vector needs no flatten. In
  DuckDB 2.0.0 the children of such a vector are FLAT too: the vector cache
  resets each child of a LIST, ARRAY, or STRUCT vector to FLAT
  (VectorCacheEntry::ResetFromCache in src/common/types/vector_cache.cpp). The
  header does not say this for the children. The writer of a UNION value writes
  the child vectors of a new vector (unionValueDuckValue in
  "Database.DuckDB.Simple.ToField").

- The set operations check the row against the size. The header of
  duckdb_v2_vector_set_value and duckdb_v2_vector_set_null says that each call
  requires a FLAT vector and checks the row against vector_get_size.

- writeResults reads the data pointer and the validity pointer after the
  flatten. The header of duckdb_v2_vector_get_data_mutable says that the storage
  shape of the vector must not change while the pointer is in use.

- prepareResultWriter does not change the size and does not flatten. It raises
  an error for a vector that is not FLAT.
-}

-- | Read the type, data, and validity of a vector, and prepare its reader. The
-- action gives the type tree of the vector. A nested decoder gives each child
-- an action that takes the tree of the child from this tree. The function
-- flattens the vector first. See Note [Vector flatten] and Note [Type trees].
prepareVectorReaderWith :: FallbackReaders -> TypeTree -> EnumLabels -> DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)
prepareVectorReaderWith readers tree labels vector = do
  -- Flatten before the view is read. See Note [Vector flatten].
  checked (c_duckdb_v2_vector_flatten vector)
  dtype <- vectorElementType vector
  view <- output (c_duckdb_v2_vector_get_view vector)
  let ConstPtr dataPtr = getField @"data'" view
      ConstPtr validity = getField @"validity" view
      ConstPtr selection = getField @"sel" view
  -- See Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
  readValue <- prepareValueReader readers (aliasBase <$> tree) labels dtype vector dataPtr validity
  pure \row -> do
    when (row < 0 || fromIntegral row >= getField @"count" view) $
      throwInternalError (Text.pack "vector row index out of range")
    physical <- if selection == nullPtr then pure row else fromIntegral <$> peekElemOff selection row
    readValue physical

{- Note [ENUM labels]
~~~~~~~~~~~~~~~~~~~~
A FieldEnum holds the index and the label of an ENUM value. The label is the
text of the cast of DuckDB to VARCHAR. The reader does not cast the vector, so
a user cast cannot change a label. The reader takes the label from one of two
sources:

- A cache (EnumLabels). The reader reads the label of an index through the
  dictionary of the vector type (duckdb_v2_logical_type_get_param) at the first
  row that has the index, and keeps it. Thus a query reads only the labels of
  the values that it reads.
- The type tree. A STRUCT, TUPLE, or UNION decoder converts the type of the
  value, which reads the whole dictionary of each ENUM type in it (see
  logicalTypeToRep). The reader of a child uses the labels of that tree, and it
  raises an error when the tree has another dictionary size than the vector.

A result column has one cache, which lives for one execution, as the tree of
the column (statementStreamColumnLabels in "Database.DuckDB.Simple.Internal").
The column reader gives the cache to the element reader of a LIST or an ARRAY,
because one column has at most one ENUM type outside a STRUCT, a TUPLE, a
UNION, and a MAP. prepareVectorReader makes a new cache for each reader. These
readers use it: the key and value readers of a MAP, the tag reader of a UNION,
the readers of typed arguments, function arguments, and COPY TO rows, and
materializeValue. There the cache lives for one vector.

Each index is checked against the parameter count of the vector type. An index
out of range raises an error, so the reader cannot give a wrong label.
-}

-- | Prepare metadata once for a vector. The reader must not outlive its chunk.
prepareValueReader
  :: FallbackReaders
  -> TypeTree
  -> EnumLabels
  -> DuckDBV2LogicalTypeId
  -> DuckDBV2VectorHandle
  -> Ptr Void
  -> Ptr Word64
  -> IO (Int -> IO FieldValue)
prepareValueReader readers tree labels dtype vector dataPtr validity = case dtype of
  DuckDBV2LogicalTypeIdVariant -> variantReader readers vector
  DuckDBV2LogicalTypeIdGeometry -> geometryReader readers vector
  DuckDBV2LogicalTypeIdArray -> whenValid FieldArray <$> prepareArrayDecoder readers tree labels vector
  DuckDBV2LogicalTypeIdList -> whenValid FieldList <$> prepareListDecoder readers tree labels vector dataPtr
  DuckDBV2LogicalTypeIdMap -> whenValid FieldMap <$> prepareMapDecoder readers vector dataPtr
  DuckDBV2LogicalTypeIdStruct -> whenValid FieldStruct <$> prepareStructDecoder readers tree vector
  DuckDBV2LogicalTypeIdTuple -> whenValid FieldTuple <$> prepareTupleDecoder readers tree vector
  DuckDBV2LogicalTypeIdUnion -> whenValid FieldUnion <$> prepareUnionDecoder readers tree vector
  DuckDBV2LogicalTypeIdDecimal -> do
    -- See Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
    (width, scale) <-
      withVectorType vector logicalTypeToBaseRep >>= \case
        LogicalTypeDecimal width scale -> pure (width, scale)
        _ -> throwInternalError (Text.pack "invalid DECIMAL type")
    pure (whenValid (FieldDecimal . DecimalValue width scale) (decimalStorage dataPtr width))
  DuckDBV2LogicalTypeIdEnum -> do
    -- See Note [ENUM labels].
    count <- withVectorType vector (output . c_duckdb_v2_logical_type_get_param_count)
    label <- case labels of
      Just cache -> pure (cachedLabel cache)
      Nothing ->
        tree >>= \case
          LogicalTypeEnum entries | toInteger (rangeSize (bounds entries)) == toInteger count -> pure (pure . (entries !) . fromIntegral)
          _ -> throwInternalError (Text.pack "invalid ENUM type")
    let readEnum row = do
          index <- enumStorage dataPtr count row
          when (toInteger index >= toInteger count) $ throwInternalError (Text.pack "ENUM index out of range")
          FieldEnum index <$> label index
    pure (whenValid id readEnum)
  DuckDBV2LogicalTypeIdSQLNull -> pure \_ -> pure FieldNull
  _ -> pure (whenValid id (decodeElement dtype dataPtr))
  where
    -- Read a label through the dictionary of the vector type at the first row
    -- that needs it. See Note [ENUM labels].
    cachedLabel cache index =
      IntMap.lookup (fromIntegral index) <$> readIORef cache >>= \case
        Just text -> pure text
        Nothing -> do
          text <- withVectorType vector \logical -> alloca \nameSlot ->
            bracket (output (c_duckdb_v2_logical_type_get_param logical (fromIntegral index) nameSlot)) destroyValue \value ->
              output (c_duckdb_v2_value_get_varchar value) >>= readText
          modifyIORef' cache (IntMap.insert (fromIntegral index) text)
          pure text
    -- See Note [Vector memory layouts] in "Database.DuckDB.Simple.Element".
    whenValid wrap decode row = do
      valid <- chunkIsRowValid validity (fromIntegral row)
      if valid then wrap <$> decode row else pure FieldNull

{- Note [User casts and the casts of the library]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A cast that a user registers is a cast function of each connection of the
database, also of the helper connection of the VARIANT codec. DuckDB uses a
registered cast in place of the built-in cast for the same pair of types. The
header says: "A connection's or callback's factory casts with that client's
cast functions and settings". It also says that the factory of an instance
"supports only the built-in casts, with default settings".

The library runs casts of DuckDB for its own work. A user cast does not change
their result:

- Reads. prepareGeometryReader casts a GEOMETRY vector to BLOB. A result, the
  value of an option (getConfigValue), and the default value of an option
  (getConfigDefaultValue) read a GEOMETRY value with the factory of the
  instance, and so does queryMetrics. The type cache of the connection keeps
  that factory (typeCacheCasts in "Database.DuckDB.Simple.TypeCache"). The
  callbacks registered on a connection use the same factory.
  A scalar or aggregate function registered by a loadable extension uses
  its private instance factory. See Note [Function argument readers] in
  "Database.DuckDB.Simple.Typed". Both paths cast the whole vector.
  Other extension callbacks have a context factory, which includes user
  casts. For those callbacks, withDefaultCastVector writes each row to a
  BLOB vector with the default cast ("Database.DuckDB.Simple.Native").
  This prevents a user cast from GEOMETRY to BLOB from calling itself when
  its callback reads a GEOMETRY argument.
  TODO: When V2 exposes WKB directly, copy it from the input vector and
  remove the intermediate BLOB cast.
- withObjectChildren and withCast cast VARIANT values to the types of their
  payloads ("Database.DuckDB.Simple.VariantCodec"). The codec also reads a
  GEOMETRY payload. It uses the factory of the instance of its helper
  connection (variantCasts).
- Values. rawGeometryDuckValue casts a BLOB value to GEOMETRY, the ToDuckValue
  instance of Geometry casts a VARCHAR value to GEOMETRY, enumDuckValue casts a
  VARCHAR value to an ENUM type, and variantDuckValue casts a value to VARIANT
  ("Database.DuckDB.Simple.ToField"). Each of them writes the value to a vector
  of the target type with one row, and reads the row (defaultCast). The header
  says that duckdb_v2_vector_set_value casts "through the non-strict default
  cast". Thus the cast does not depend on the factory. The factory still makes
  the target type, and only the factory of a connection or of a callback
  context can make a GEOMETRY type with a CRS. See Note [Raw GEOMETRY
  parameters] in "Database.DuckDB.Simple.ToField".
- The type query of a connection with a custom CRS (readNativeTypes in
  "Database.DuckDB.Simple.TypeCache") binds the empty point as a BLOB value. Its
  statement has no cast.

Why a vector for the values. For GEOMETRY and VARIANT, the header calls
duckdb_v2_vector_set_value "the only way to write a cell". For an ENUM, the
header calls a cast of a VARCHAR to an enum type with duckdb_v2_value_cast "the
sanctioned way to build UNION and ENUM values", and that cast includes
"registered custom casts". enumDuckValue does not use that cast, on purpose.
The library builds an ENUM value from the label of a FieldEnum, and a user
cast from VARCHAR to the ENUM type must not change that value. A UNION value
is built through a vector for another reason. See Note [UNION values] in
"Database.DuckDB.Simple.ToField".

These casts still use the cast functions of the session, so a user cast for
the same pair changes their result:

- setConfigValue and setConfigOption. DuckDB casts the value to the type of the
  option as SQL SET does.
- The casts of the SQL text. DuckDB runs them with the casts of the session.
  The library does not choose them.
-}

-- | Copy the WKB bytes and CRS of every GEOMETRY row in a vector. GEOMETRY has
-- no committed vector layout, so a cast to BLOB reads the rows. The reader takes
-- logical rows. See Note [Vector flatten]. With the factory of an instance, one
-- native cast casts the vector. With the factory of a context, the default cast
-- of 'withDefaultCastVector' casts each row. See
-- Note [User casts and the casts of the library].
prepareGeometryReader :: ReadFactory -> DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)
prepareGeometryReader readFactory vector = do
  -- See Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
  crs <-
    withVectorType vector logicalTypeToBaseRep >>= \case
      LogicalTypeGeometry value -> pure value
      _ -> throwInternalError (Text.pack "invalid GEOMETRY type")
  size <- output (c_duckdb_v2_vector_get_size vector)
  let count = fromIntegral size
      factory = readFactoryHandle readFactory
      readBlobs target = do
        let notBlob = rejectRows (throwInternalError (Text.pack "expected a BLOB vector"))
        readBlob <- prepareVectorReader (FallbackReaders (logicalTypeToRep factory) notBlob notBlob) target
        forM [0 .. count - 1] readBlob
  blobs <-
    bracket
      (output (c_duckdb_v2_factory_create_type_from_id factory DuckDBV2LogicalTypeIdBlob (ConstPtr nullPtr) (ConstPtr nullPtr) 0))
      destroyLogicalType
      \blob -> case readFactory of
        InstanceFactory _ -> withOwnedVector factory blob \target -> do
          checked (c_duckdb_v2_vector_cast factory vector target DuckDBV2CastModeNormal nullPtr nullPtr)
          readBlobs target
        ContextFactory _ ->
          withDefaultCastVector
            factory
            blob
            size
            (\row write -> bracket (output (c_duckdb_v2_vector_get_value vector row)) destroyValue write)
            readBlobs
  let rows = listArray (0, count - 1) blobs :: Array Int FieldValue
  pure \row -> case rows ! row of
    FieldBlob bytes -> pure (FieldGeometry (RawGeometry bytes crs))
    FieldNull -> pure FieldNull
    _ -> throwInternalError (Text.pack "GEOMETRY cast did not produce a BLOB")

-- | Read a DECIMAL. DuckDB stores it in the smallest integer type for its width.
-- See Note [Vector memory layouts] in "Database.DuckDB.Simple.Element".
decimalStorage :: Ptr Void -> Word8 -> Int -> IO Integer
decimalStorage dataPtr width rowIdx
  | width <= 4 = toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int16) rowIdx
  | width <= 9 = toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int32) rowIdx
  | width <= 18 = toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int64) rowIdx
  | otherwise = duckDBHugeIntToInteger <$> peekElemOff (castPtr dataPtr :: Ptr DuckDBV2HugeintT) rowIdx

-- | Read an ENUM index. DuckDB stores it in the smallest unsigned type for the dictionary size.
-- See Note [Vector memory layouts] in "Database.DuckDB.Simple.Element".
enumStorage :: Ptr Void -> DuckDBIdx -> Int -> IO Word32
enumStorage dataPtr count rowIdx
  | count <= 255 = fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
  | count <= 65535 = fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Word16) rowIdx
  | otherwise = peekElemOff (castPtr dataPtr :: Ptr Word32) rowIdx

-- | Read the ARRAY size and prepare the element reader once for a vector. The
-- size gives the layout of the elements, so it comes from the type of this
-- vector. Parameter 1 of an ARRAY type is the size, as 'logicalTypeToRep' reads
-- it. The decoder does not convert the element type. The element reader takes
-- its tree from the tree of the ARRAY. See Note [Type trees].
-- See Note [Constant stack at safe calls] in "Database.DuckDB.Simple.Native".
prepareArrayDecoder :: FallbackReaders -> TypeTree -> EnumLabels -> DuckDBV2VectorHandle -> IO (Int -> IO (Array Int FieldValue))
prepareArrayDecoder readers tree labels vector = do
  size <- withVectorType vector \logical -> alloca \name ->
    bracket (output (c_duckdb_v2_logical_type_get_param logical 1 name)) destroyValue (output . c_duckdb_v2_value_get_bigint)
  arraySize <- ensureWithinIntRange (Text.pack "array size") (fromIntegral size)
  readChild <- vectorChild vector 0 >>= prepareVectorReaderWith readers (elementTree tree) labels
  pure \rowIdx -> do
    let baseIdx = rowIdx * arraySize
    values <- reverse <$> foldM (\values delta -> (: values) <$> readChild (baseIdx + delta)) [] [0 .. arraySize - 1]
    pure (listArray (0, arraySize - 1) values)

-- | Prepare the element reader once for a LIST vector. The element reader
-- takes its tree from the tree of the LIST. See Note [Type trees].
-- See Note [Constant stack at safe calls] in "Database.DuckDB.Simple.Native".
prepareListDecoder :: FallbackReaders -> TypeTree -> EnumLabels -> DuckDBV2VectorHandle -> Ptr Void -> IO (Int -> IO [FieldValue])
prepareListDecoder readers tree labels vector dataPtr = do
  readChild <- vectorChild vector 0 >>= prepareVectorReaderWith readers (elementTree tree) labels
  pure \rowIdx -> do
    entry <- peekElemOff (castPtr dataPtr :: Ptr DuckDBV2ListEntry) rowIdx
    (baseIdx, len) <- listEntryBounds (Text.pack "list") entry
    reverse <$> foldM (\values delta -> (: values) <$> readChild (baseIdx + delta)) [] [0 .. len - 1]

-- | Prepare the key reader and the value reader once for a MAP vector. The
-- decoder gives them no type tree. Each converts the type of its own vector when
-- it needs a description. A tree of the MAP holds the key type and the value
-- type together. A value reader that took its tree from it would thus also
-- convert the key type, for example the dictionary of a large ENUM. The
-- decoder does not flatten the entry STRUCT. See Note [Vector flatten] and
-- Note [Type trees].
-- See Note [Constant stack at safe calls] in "Database.DuckDB.Simple.Native".
prepareMapDecoder :: FallbackReaders -> DuckDBV2VectorHandle -> Ptr Void -> IO (Int -> IO [(FieldValue, FieldValue)])
prepareMapDecoder readers vector dataPtr = do
  entries <- vectorChild vector 0
  readKey <- vectorChild entries 0 >>= prepareVectorReader readers
  readValue <- vectorChild entries 1 >>= prepareVectorReader readers
  pure \rowIdx -> do
    entry <- peekElemOff (castPtr dataPtr :: Ptr DuckDBV2ListEntry) rowIdx
    (baseIdx, len) <- listEntryBounds (Text.pack "map") entry
    reverse
      <$> foldM
        ( \pairs delta -> do
            let childIdx = baseIdx + delta
            keyValue <- readKey childIdx
            valueValue <- readValue childIdx
            pure ((keyValue, valueValue) : pairs)
        )
        []
        [0 .. len - 1]

-- | Read the STRUCT type and prepare a reader for each child once for a vector.
-- The decoder reads a valid row. It must not outlive its chunk. See
-- Note [Type trees].
prepareStructDecoder :: FallbackReaders -> TypeTree -> DuckDBV2VectorHandle -> IO (Int -> IO (StructValue FieldValue))
prepareStructDecoder readers tree vector = do
  structFields <-
    tree >>= \case
      LogicalTypeStruct typeArray -> pure typeArray
      other ->
        throwInternalError (Text.pack "expected STRUCT logical type, but saw " <> logicalTypeSQL other)
  let typeList = elems structFields
      count = length typeList
      indexMap =
        Map.fromList (zip (map structFieldName typeList) [0 ..])
  checkChildCount "STRUCT" vector count
  childReaders <-
    forM (zip [0 ..] typeList) \(childIdx, StructField{structFieldName, structFieldValue}) -> do
      readChild <- vectorChild vector childIdx >>= prepareVectorReaderWith readers (pure structFieldValue) Nothing
      pure (structFieldName, readChild)
  pure \rowIdx -> do
    valueFields <-
      forM childReaders \(name, readChild) -> do
        value <- readChild rowIdx
        pure StructField{structFieldName = name, structFieldValue = value}
    let fieldArray =
          listArray (0, count - 1) valueFields
    pure
      StructValue
        { structValueFields = fieldArray
        , structValueTypes = structFields
        , structValueIndex = indexMap
        }

-- | Read positional field metadata and prepare readers that borrow the tuple.
-- See Note [Type trees].
prepareTupleDecoder :: FallbackReaders -> TypeTree -> DuckDBV2VectorHandle -> IO (Int -> IO (TupleValue FieldValue))
prepareTupleDecoder readers tree vector = do
  types <-
    tree >>= \case
      LogicalTypeTuple fields -> pure fields
      _ -> throwInternalError (Text.pack "invalid TUPLE type")
  let typeList = elems types
      count = length typeList
  checkChildCount "TUPLE" vector count
  fieldReaders <- forM (zip [0 ..] typeList) \(index, fieldType) ->
    vectorChild vector index >>= prepareVectorReaderWith readers (pure fieldType) Nothing
  pure \row -> do
    fields <- mapM ($ row) fieldReaders
    pure (TupleValue (listArray (0, count - 1) fields) types)

-- | Read the UNION type and prepare the tag reader once for a vector. Prepare
-- the reader of a member at the first row that selects the member, so that a
-- member that no row selects makes no native call. The decoder reads a valid
-- row. It must not outlive its chunk. See Note [Type trees].
prepareUnionDecoder :: FallbackReaders -> TypeTree -> DuckDBV2VectorHandle -> IO (Int -> IO (UnionValue FieldValue))
prepareUnionDecoder readers tree vector = do
  membersArray <-
    tree >>= \case
      LogicalTypeUnion members -> pure members
      other ->
        throwInternalError (Text.pack "expected UNION logical type, but saw " <> logicalTypeSQL other)
  let memberCount = rangeSize (bounds membersArray)
  -- Child 0 of a UNION vector is the tag, and member j is child j + 1. The tag
  -- reader needs no type tree.
  checkChildCount "UNION" vector (memberCount + 1)
  readTag <- vectorChild vector 0 >>= prepareVectorReader readers
  readersByTag <- fmap (listArray (0, memberCount - 1)) $
    forM (zip [1 ..] (elems membersArray)) \(childIdx, UnionMemberType{unionMemberName, unionMemberType}) -> do
      memberVec <- vectorChild vector childIdx
      prepared <- newIORef Nothing
      pure \row -> do
        cached <- readIORef prepared
        readMember <- case cached of
          Just reader -> pure reader
          Nothing -> do
            reader <- prepareVectorReaderWith readers (pure unionMemberType) Nothing memberVec
            writeIORef prepared (Just reader)
            pure reader
        payload <- readMember row
        pure (unionMemberName, payload)
  pure \rowIdx -> do
    memberIdx <- readTag rowIdx >>= unionTagIndex
    when (memberIdx < 0 || memberIdx >= memberCount) $
      throwInternalError (Text.pack "union tag out of range")
    (name, payload) <- (readersByTag ! memberIdx) rowIdx
    pure
      UnionValue
        { unionValueIndex = fromIntegral memberIdx
        , unionValueLabel = name
        , unionValuePayload = payload
        , unionValueMembers = membersArray
        }

-- | Convert a decoded UNION tag to a member index.
unionTagIndex :: FieldValue -> IO Int
unionTagIndex = \case
  FieldWord8 tagWord -> pure (fromIntegral tagWord :: Int)
  FieldWord16 tagWord -> pure (fromIntegral tagWord :: Int)
  FieldWord32 tagWord ->
    if tagWord <= fromIntegral (maxBound :: Word16)
      then pure (fromIntegral tagWord)
      else throwInternalError (Text.pack "union tag exceeds Word16 range")
  FieldWord64 tagWord ->
    if tagWord <= fromIntegral (maxBound :: Word16)
      then pure (fromIntegral tagWord)
      else throwInternalError (Text.pack "union tag exceeds Word16 range")
  FieldInt8 tagInt
    | tagInt >= 0 -> pure (fromIntegral tagInt)
    | otherwise -> throwInternalError (Text.pack "union tag negative")
  FieldInt16 tagInt
    | tagInt >= 0 -> pure (fromIntegral tagInt)
    | otherwise -> throwInternalError (Text.pack "union tag negative")
  FieldInt32 tagInt
    | tagInt >= 0 && tagInt <= fromIntegral (maxBound :: Word16) -> pure (fromIntegral tagInt)
    | tagInt < 0 -> throwInternalError (Text.pack "union tag negative")
    | otherwise -> throwInternalError (Text.pack "union tag exceeds Word16 range")
  FieldInt64 tagInt
    | tagInt >= 0 && tagInt <= fromIntegral (maxBound :: Word16) -> pure (fromIntegral tagInt)
    | tagInt < 0 -> throwInternalError (Text.pack "union tag negative")
    | otherwise -> throwInternalError (Text.pack "union tag exceeds Word16 range")
  FieldNull ->
    throwInternalError (Text.pack "encountered NULL union tag")
  other ->
    throwInternalError (Text.pack ("unexpected union tag value " <> show other))

listEntryBounds :: Text -> DuckDBV2ListEntry -> IO (Int, Int)
listEntryBounds context (DuckDBV2ListEntry offset lenRaw) = do
  base <- ensureWithinIntRange (context <> Text.pack " offset") (fromIntegral offset)
  len <- ensureWithinIntRange (context <> Text.pack " length") (fromIntegral lenRaw)
  let maxInt = toInteger (maxBound :: Int)
      upperBound = toInteger base + toInteger len - 1
  when (len > 0 && upperBound > maxInt) $
    throwInternalError (context <> Text.pack " bounds exceed Int range")
  pure (base, len)

ensureWithinIntRange :: Text -> Word64 -> IO Int
ensureWithinIntRange context value =
  let actual = toInteger value
      limit = toInteger (maxBound :: Int)
  in  if actual <= limit
        then pure (fromInteger actual)
        else throwInternalError (context <> Text.pack " exceeds Int range")

-- | Take the element tree from the tree of a LIST or ARRAY. Read the parent
-- tree only when the element reader needs its tree. See
-- Note [Type trees].
elementTree :: TypeTree -> TypeTree
elementTree tree =
  tree >>= \case
    LogicalTypeList element -> pure element
    LogicalTypeArray element _ -> pure element
    _ -> throwInternalError (Text.pack "invalid LIST or ARRAY type")

-- | Raise an error when the type tree of a STRUCT, TUPLE, or UNION vector gives
-- another child count than the vector has. The tree can come from the type of a
-- parent vector, and the decoder must not drop a child. See Note [Type trees].
checkChildCount :: String -> DuckDBV2VectorHandle -> Int -> IO ()
checkChildCount kind vector expected = do
  actual <- output (c_duckdb_v2_vector_get_child_count vector)
  when (toInteger actual /= toInteger expected) $
    throwInternalError
      (Text.pack ("invalid " <> kind <> " type: the type has " <> show expected <> " children, but the vector has " <> show actual))

-- | Borrow a nested vector from its parent chunk.
vectorChild :: DuckDBV2VectorHandle -> DuckDBIdx -> IO DuckDBV2VectorHandle
vectorChild vector index = output (c_duckdb_v2_vector_get_child vector index)
