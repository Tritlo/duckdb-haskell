{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.ToField
Description : Convert Haskell parameters into DuckDB bindable values.

The @ToField@ class mirrors the interface provided by @sqlite-simple@ while
delegating to the DuckDB C API under the hood.
-}
module Database.DuckDB.Simple.ToField (
    FieldBinding,
    ToDuckValue (..),
    ToField (..),
    DuckDBColumnType (..),
    NamedParam (..),
    duckdbColumnType,
    bindFieldBinding,
    renderFieldBinding,
) where

import Control.Exception (bracket, mask_, onException, throwIO)
import Control.Monad (filterM, forM_, unless, when)
import Data.Array (Array, bounds, elems, listArray)
import Data.Bits (complement, shiftL, shiftR, xor, (.|.))
import qualified Data.ByteString as BS
import Data.Fixed (Pico)
import qualified Data.Geometry as G
import qualified Data.Geometry.WKT as WKT
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.List as List
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), TimeZone (..), timeOfDayToTime, timeZoneMinutes, utc, utcToLocalTime)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.FromField (BigNum (..), BitString (..), DecimalValue (..), FieldValue (..), IntervalValue (..), TimeWithZone (..))
import Database.DuckDB.Simple.Internal (
    Statement (..),
    duckDBTypeFromName,
    preparedParameterType,
    storeBoundValue,
    withTypeCache,
 )
import Database.DuckDB.Simple.LogicalRep (
    LogicalTypeRep (..),
    StructField (..),
    StructValue (..),
    TupleValue (..),
    UnionMemberType (..),
    UnionValue (..),
    logicalTypeFromRep,
    logicalTypeToRep,
    structValueTypeRep,
    tupleValueTypeRep,
    unionValueTypeRep,
 )
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import Database.DuckDB.Simple.TypeCache (TypeCache, cachedLogicalType, typeCacheConnection)
import Database.DuckDB.Simple.Types (Null (..))
import Database.DuckDB.Simple.Variant (Variant (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CDouble (..), CFloat (..))
import Foreign.Marshal (fromBool)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import Numeric.Natural (Natural)

-- | Represents a named parameter binding using the @:=@ operator.
data NamedParam where
    (:=) :: (ToField a) => Text -> a -> NamedParam

infixr 3 :=

-- | Encapsulates the action required to bind a single positional parameter, together with a textual description used in diagnostics.
data FieldBinding = FieldBinding
    { fieldBindingValue :: !(TypeCache -> IO (Maybe LogicalTypeRep) -> IO DuckDBV2ValueHandle)
    , fieldBindingDisplay :: !String
    }

-- | Low-level class for values that can be marshalled directly into `DuckDBV2ValueHandle`s.
class (DuckDBColumnType a) => ToDuckValue a where
    {- | Construct an owned value. Keep the connection alive until it is destroyed.
    Use 'ToField' to resolve arbitrary CRS labels from connection options.
    -}
    toDuckValue :: DuckDBV2ConnectionHandle -> a -> IO DuckDBV2ValueHandle

valueBinding :: String -> (DuckDBV2ConnectionHandle -> IO DuckDBV2ValueHandle) -> FieldBinding
valueBinding display make = cacheValueBinding display (make . typeCacheConnection)

-- | Construct a value with the type cache of the statement's connection.
cacheValueBinding :: String -> (TypeCache -> IO DuckDBV2ValueHandle) -> FieldBinding
cacheValueBinding display makeValue =
    targetValueBinding display (\cache _ -> makeValue cache)

-- | Construct a value with the prepared parameter type, when it is available.
targetValueBinding :: String -> (TypeCache -> IO (Maybe LogicalTypeRep) -> IO DuckDBV2ValueHandle) -> FieldBinding
targetValueBinding display makeValue =
    FieldBinding
        { fieldBindingValue = makeValue
        , fieldBindingDisplay = display
        }

-- | Preserve UTC nanoseconds where the prepared parameter requires them.
temporalValueBinding :: String -> LogicalTypeRep -> FieldValue -> (TypeCache -> IO DuckDBV2ValueHandle) -> FieldBinding
temporalValueBinding display source value makeDefault =
    targetValueBinding display \cache readTarget -> do
        target <- readTarget
        let precise = timestampTypeForTarget target source
        if precise == source
            then makeDefault cache
            else fieldValueWithTypeDuckValue (cachedLogicalType cache) (typeCacheConnection cache) precise (valueWithTypeMetadata precise value)

-- | Read the element type of a prepared LIST or ARRAY parameter.
collectionChildType :: Maybe LogicalTypeRep -> Maybe LogicalTypeRep
collectionChildType (Just (LogicalTypeList child)) = Just child
collectionChildType (Just (LogicalTypeArray child _)) = Just child
collectionChildType _ = Nothing

-- | Promote only UTC leaves that correspond to nanosecond parameter leaves.
timestampTypeForTarget :: Maybe LogicalTypeRep -> LogicalTypeRep -> LogicalTypeRep
timestampTypeForTarget target source = case (target, source) of
    (Just (LogicalTypeScalar DuckDBV2LogicalTypeIdTimestampTzNs), LogicalTypeScalar DuckDBV2LogicalTypeIdTimestampTz) ->
        LogicalTypeScalar DuckDBV2LogicalTypeIdTimestampTzNs
    (_, LogicalTypeList child) ->
        maybe source (\expected -> LogicalTypeList (timestampTypeForTarget (Just expected) child)) (collectionChildType target)
    (_, LogicalTypeArray child size) ->
        maybe source (\expected -> LogicalTypeArray (timestampTypeForTarget (Just expected) child) size) (collectionChildType target)
    (Just (LogicalTypeMap targetKey targetValue), LogicalTypeMap key value) ->
        LogicalTypeMap (timestampTypeForTarget (Just targetKey) key) (timestampTypeForTarget (Just targetValue) value)
    (Just (LogicalTypeStruct targetFields), LogicalTypeStruct fields) ->
        let sourceFields = elems fields
            targets = [(identifier name, rep) | StructField name rep <- elems targetFields]
            matching = [lookup (identifier (structFieldName field)) targets | field <- sourceFields]
            updated = zipWith (\field expected -> field{structFieldValue = timestampTypeForTarget expected (structFieldValue field)}) sourceFields matching
         in LogicalTypeStruct (listArray (bounds fields) updated)
    (Just (LogicalTypeTuple targetFields), LogicalTypeStruct fields) ->
        let matching = map Just (elems targetFields) <> repeat Nothing
            updated = zipWith (\field expected -> field{structFieldValue = timestampTypeForTarget expected (structFieldValue field)}) (elems fields) matching
         in LogicalTypeStruct (listArray (bounds fields) updated)
    (Just (LogicalTypeTuple targetFields), LogicalTypeTuple fields) ->
        positionalTuple (elems targetFields) fields
    (Just (LogicalTypeStruct targetFields), LogicalTypeTuple fields) ->
        positionalTuple (map structFieldValue (elems targetFields)) fields
    (Just (LogicalTypeUnion targetMembers), LogicalTypeUnion members) ->
        let targets = [(identifier name, rep) | UnionMemberType name rep <- elems targetMembers]
         in LogicalTypeUnion (fmap (\member -> member{unionMemberType = timestampTypeForTarget (lookup (identifier (unionMemberName member)) targets) (unionMemberType member)}) members)
    _ -> source
  where
    -- DuckDB compares identifiers without ASCII case.
    identifier = foldIdentifier
    positionalTuple targets fields = LogicalTypeTuple (listArray (bounds fields) (zipWith timestampTypeForTarget (map Just targets <> repeat Nothing) (elems fields)))

-- | Copy adapted type metadata into nested values without changing their payloads.
valueWithTypeMetadata :: LogicalTypeRep -> FieldValue -> FieldValue
valueWithTypeMetadata logical value = case (logical, value) of
    (LogicalTypeList child, FieldList values) -> FieldList (map (valueWithTypeMetadata child) values)
    (LogicalTypeArray child _, FieldArray values) -> FieldArray (fmap (valueWithTypeMetadata child) values)
    (LogicalTypeMap keyType valueType, FieldMap pairs) ->
        FieldMap [(valueWithTypeMetadata keyType key, valueWithTypeMetadata valueType item) | (key, item) <- pairs]
    (LogicalTypeStruct types, FieldStruct struct) ->
        let sourceFields = elems (structValueFields struct)
            fields = [(structFieldName field, structFieldValue field) | field <- elems types]
            matching = [lookup (structFieldName field) fields | field <- sourceFields]
            update field expected = case expected of
                Nothing -> field
                Just rep -> field{structFieldValue = valueWithTypeMetadata rep (structFieldValue field)}
         in FieldStruct struct{structValueTypes = types, structValueFields = listArray (bounds (structValueFields struct)) (zipWith update sourceFields matching)}
    (LogicalTypeTuple types, FieldTuple tuple) ->
        let update item expected = maybe item (`valueWithTypeMetadata` item) expected
            fields = zipWith update (elems (tupleValueFields tuple)) (map Just (elems types) <> repeat Nothing)
         in FieldTuple tuple{tupleValueTypes = types, tupleValueFields = listArray (bounds (tupleValueFields tuple)) fields}
    (LogicalTypeUnion members, FieldUnion union) ->
        let payload = case drop (fromIntegral (unionValueIndex union)) (elems members) of
                member : _ -> valueWithTypeMetadata (unionMemberType member) (unionValuePayload union)
                [] -> unionValuePayload union
         in FieldUnion union{unionValueMembers = members, unionValuePayload = payload}
    _ -> value

-- | Types that map to a concrete DuckDB column type when used with @ToField@.
class DuckDBColumnType a where
    duckdbColumnTypeFor :: Proxy a -> Text

-- | Report the DuckDB column type that best matches a given @ToField@ instance.
duckdbColumnType :: forall a. (DuckDBColumnType a) => Proxy a -> Text
duckdbColumnType = duckdbColumnTypeFor

-- | Apply a @FieldBinding@ to the given statement/index.
bindFieldBinding :: Statement -> DuckDBIdx -> FieldBinding -> IO ()
bindFieldBinding stmt idx FieldBinding{fieldBindingValue} =
    withTypeCache (statementConnection stmt) \cache ->
        bindDuckValue stmt idx (fieldBindingValue cache (preparedParameterType stmt idx))

-- | Render a bound parameter for error reporting.
renderFieldBinding :: FieldBinding -> String
renderFieldBinding FieldBinding{fieldBindingDisplay} = fieldBindingDisplay

-- | Types that can be used as positional parameters.
class ToField a where
    toField :: a -> FieldBinding
    default toField :: (Show a, ToDuckValue a) => a -> FieldBinding
    toField value = valueBinding (show value) (\conn -> toDuckValue conn value)

instance ToField Null where
    toField Null = nullBinding "NULL"

instance ToField Bool
instance ToField Int
instance ToField Int8
instance ToField Int16
instance ToField Int32
instance ToField Int64
instance ToField Integer
instance ToField Natural
instance ToField UUID.UUID
instance ToField Word
instance ToField Word8
instance ToField Word16
instance ToField Word32
instance ToField Word64
instance ToField Double
instance ToField Float
instance ToField Text
instance ToField String
instance ToField BitString
instance ToField Day
instance ToField TimeOfDay
instance ToField LocalTime

-- | Bind the shape as @GEOMETRY@ with no CRS.
instance ToField G.Geometry

-- | Bind the payload as a VARIANT with the type cache of the connection.
instance ToField Variant where
    toField value =
        cacheValueBinding (show value) \cache ->
            variantDuckValue (cachedLogicalType cache) (typeCacheConnection cache) (variantPayload value)

instance ToField UTCTime where
    toField value = temporalValueBinding (show value) (LogicalTypeScalar DuckDBV2LogicalTypeIdTimestampTz) (FieldTimestampTZ (Finite value)) (\cache -> utcTimeDuckValue (typeCacheConnection cache) value)
instance ToField (Unbounded Day)
instance ToField (Unbounded LocalTime)
instance ToField (Unbounded UTCTime) where
    toField value = temporalValueBinding (show value) (LogicalTypeScalar DuckDBV2LogicalTypeIdTimestampTz) (FieldTimestampTZ value) (\cache -> utcTimestampDuckValue (typeCacheConnection cache) value)

instance ToField BigNum where
    toField big@(BigNum n) = valueBinding (show n) (\conn -> bigNumDuckValue conn big)

instance ToField (StructValue FieldValue) where
    toField structVal =
        temporalValueBinding "<struct>" (structValueTypeRep structVal) (FieldStruct structVal) \cache ->
            structValueDuckValue (cachedLogicalType cache) (typeCacheConnection cache) structVal

instance ToField (TupleValue FieldValue) where
    toField tuple =
        temporalValueBinding "<tuple>" (tupleValueTypeRep tuple) (FieldTuple tuple) \cache ->
            tupleValueDuckValue (cachedLogicalType cache) (typeCacheConnection cache) tuple

instance ToField (UnionValue FieldValue) where
    toField unionVal =
        let label = Text.unpack (unionValueLabel unionVal)
         in temporalValueBinding ("<union " <> label <> ">") (unionValueTypeRep unionVal) (FieldUnion unionVal) \cache ->
                unionValueDuckValue (cachedLogicalType cache) (typeCacheConnection cache) unionVal

instance DuckDBColumnType BitString where
    duckdbColumnTypeFor _ = "BIT"

instance ToField BS.ByteString where
    toField bs =
        valueBinding
            ("<blob length=" <> show (BS.length bs) <> ">")
            (\conn -> toDuckValue conn bs)

instance (DuckDBColumnType a, ToField a) => ToField (Array Int a) where
    toField arr =
        targetValueBinding
            ("<array length=" <> show (length (elems arr)) <> ">")
            \cache readTarget -> do
                target <- readTarget
                let childTarget = collectionChildType target
                arrayDuckValue (cachedLogicalType cache) (typeCacheConnection cache) childTarget (\value -> fieldBindingValue (toField value) cache (pure childTarget)) arr

instance (ToField a) => ToField (Maybe a) where
    toField Nothing = nullBinding "Nothing"
    toField (Just value) =
        let binding = toField value
         in binding
                { fieldBindingDisplay = "Just " <> renderFieldBinding binding
                }

instance DuckDBColumnType G.Geometry where
    duckdbColumnTypeFor _ = "GEOMETRY"

instance DuckDBColumnType Variant where
    duckdbColumnTypeFor _ = "VARIANT"

instance DuckDBColumnType Null where
    duckdbColumnTypeFor _ = "NULL"

instance DuckDBColumnType Bool where
    duckdbColumnTypeFor _ = "BOOLEAN"

instance DuckDBColumnType Int where
    duckdbColumnTypeFor _ = "BIGINT"

instance DuckDBColumnType Int8 where
    duckdbColumnTypeFor _ = "TINYINT"

instance DuckDBColumnType Int16 where
    duckdbColumnTypeFor _ = "SMALLINT"

instance DuckDBColumnType Int32 where
    duckdbColumnTypeFor _ = "INTEGER"

instance DuckDBColumnType Int64 where
    duckdbColumnTypeFor _ = "BIGINT"

instance DuckDBColumnType BigNum where
    duckdbColumnTypeFor _ = "BIGNUM"

instance DuckDBColumnType UUID.UUID where
    duckdbColumnTypeFor _ = "UUID"

instance DuckDBColumnType Integer where
    duckdbColumnTypeFor _ = "BIGNUM"

instance DuckDBColumnType Natural where
    duckdbColumnTypeFor _ = "BIGNUM"

instance DuckDBColumnType Word where
    duckdbColumnTypeFor _ = "UBIGINT"

instance DuckDBColumnType Word8 where
    duckdbColumnTypeFor _ = "UTINYINT"

instance DuckDBColumnType Word16 where
    duckdbColumnTypeFor _ = "USMALLINT"

instance DuckDBColumnType Word32 where
    duckdbColumnTypeFor _ = "UINTEGER"

instance DuckDBColumnType Word64 where
    duckdbColumnTypeFor _ = "UBIGINT"

instance DuckDBColumnType Double where
    duckdbColumnTypeFor _ = "DOUBLE"

instance DuckDBColumnType Float where
    duckdbColumnTypeFor _ = "FLOAT"

instance DuckDBColumnType Text where
    duckdbColumnTypeFor _ = "TEXT"

instance DuckDBColumnType String where
    duckdbColumnTypeFor _ = "TEXT"

instance DuckDBColumnType BS.ByteString where
    duckdbColumnTypeFor _ = "BLOB"

instance DuckDBColumnType Day where
    duckdbColumnTypeFor _ = "DATE"

instance DuckDBColumnType TimeOfDay where
    duckdbColumnTypeFor _ = "TIME"

instance DuckDBColumnType LocalTime where
    duckdbColumnTypeFor _ = "TIMESTAMP"

instance DuckDBColumnType UTCTime where
    duckdbColumnTypeFor _ = "TIMESTAMPTZ"

instance DuckDBColumnType (Unbounded Day) where
    duckdbColumnTypeFor _ = "DATE"

instance DuckDBColumnType (Unbounded LocalTime) where
    duckdbColumnTypeFor _ = "TIMESTAMP"

instance DuckDBColumnType (Unbounded UTCTime) where
    duckdbColumnTypeFor _ = "TIMESTAMPTZ"

instance DuckDBColumnType (StructValue FieldValue) where
    duckdbColumnTypeFor _ = "STRUCT"

instance DuckDBColumnType (TupleValue FieldValue) where
    duckdbColumnTypeFor _ = "TUPLE"

instance DuckDBColumnType (UnionValue FieldValue) where
    duckdbColumnTypeFor _ = "UNION"

instance (DuckDBColumnType a) => DuckDBColumnType (Maybe a) where
    duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy :: Proxy a)

instance (DuckDBColumnType a) => DuckDBColumnType (Array Int a) where
    duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy :: Proxy a) <> Text.pack "[]"

nullBinding :: String -> FieldBinding
nullBinding repr = valueBinding repr nullDuckValue

nullDuckValue :: DuckDBV2ConnectionHandle -> IO DuckDBV2ValueHandle
nullDuckValue conn = bracket (logicalTypeFromRep conn (LogicalTypeScalar DuckDBV2LogicalTypeIdSQLNull)) destroyLogicalType (\logical -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_null factory logical))

boolDuckValue :: DuckDBV2ConnectionHandle -> Bool -> IO DuckDBV2ValueHandle
boolDuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_bool factory (if value then 1 else 0))

int8DuckValue :: DuckDBV2ConnectionHandle -> Int8 -> IO DuckDBV2ValueHandle
int8DuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_tinyint factory value)

int16DuckValue :: DuckDBV2ConnectionHandle -> Int16 -> IO DuckDBV2ValueHandle
int16DuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_smallint factory value)

int32DuckValue :: DuckDBV2ConnectionHandle -> Int32 -> IO DuckDBV2ValueHandle
int32DuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_int factory value)

int64DuckValue :: DuckDBV2ConnectionHandle -> Int64 -> IO DuckDBV2ValueHandle
int64DuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_bigint factory value)

uint64DuckValue :: DuckDBV2ConnectionHandle -> Word64 -> IO DuckDBV2ValueHandle
uint64DuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_ubigint factory value)

uint32DuckValue :: DuckDBV2ConnectionHandle -> Word32 -> IO DuckDBV2ValueHandle
uint32DuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_uint factory value)

uint16DuckValue :: DuckDBV2ConnectionHandle -> Word16 -> IO DuckDBV2ValueHandle
uint16DuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_usmallint factory value)

uint8DuckValue :: DuckDBV2ConnectionHandle -> Word8 -> IO DuckDBV2ValueHandle
uint8DuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_utinyint factory value)

doubleDuckValue :: DuckDBV2ConnectionHandle -> Double -> IO DuckDBV2ValueHandle
doubleDuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_double factory (CDouble value))

floatDuckValue :: DuckDBV2ConnectionHandle -> Float -> IO DuckDBV2ValueHandle
floatDuckValue conn value = factoryOutput conn (\factory -> c_duckdb_v2_value_create_float factory (CFloat value))

textDuckValue :: DuckDBV2ConnectionHandle -> Text -> IO DuckDBV2ValueHandle
textDuckValue conn value = withText value (\text -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_varchar factory text))

stringDuckValue :: DuckDBV2ConnectionHandle -> String -> IO DuckDBV2ValueHandle
stringDuckValue conn = textDuckValue conn . Text.pack

blobDuckValue :: DuckDBV2ConnectionHandle -> BS.ByteString -> IO DuckDBV2ValueHandle
blobDuckValue conn value = withBytes value (\bytes -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_blob factory bytes))

uuidDuckValue :: DuckDBV2ConnectionHandle -> UUID.UUID -> IO DuckDBV2ValueHandle
uuidDuckValue conn uuid =
    let (upper, lower) = UUID.toWords64 uuid
        value = DuckDBV2HugeintT lower (fromIntegral (upper `xor` 0x8000000000000000))
     in with value (\ptr -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_uuid factory (ConstPtr ptr)))

bitDuckValue :: DuckDBV2ConnectionHandle -> BitString -> IO DuckDBV2ValueHandle
bitDuckValue conn (BitString padding bits) = do
    when (BS.null bits || padding > 7) $
        throwIO (userError "duckdb-simple: BIT requires nonempty data and padding from 0 to 7")
    let nativePadding = complement ((1 `shiftL` (8 - fromIntegral padding)) - 1) :: Word8
        payload = BS.cons padding (BS.cons (BS.head bits .|. nativePadding) (BS.tail bits))
    withBytes payload (\bytes -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_bit factory bytes))

bigNumDuckValue :: DuckDBV2ConnectionHandle -> BigNum -> IO DuckDBV2ValueHandle
bigNumDuckValue conn (BigNum value) = do
    let magnitude = BS.pack (reverse (bytes (abs value)))
        bytes 0 = [0]
        bytes n = fromIntegral n : if n < 256 then [] else bytes (n `shiftR` 8)
    BS.useAsCStringLen magnitude \(raw, len) -> alloca \lengthSlot -> do
        let encode ptr capacity = checked (c_duckdb_v2_bignum_encode (ConstPtr (castPtr raw)) (fromIntegral len) (fromBool (value < 0)) ptr capacity lengthSlot)
        encode nullPtr 0
        size <- peek lengthSlot
        allocaBytes (fromIntegral size) \storage -> do
            encode storage size
            payload <- BS.packCStringLen (castPtr storage, fromIntegral size)
            withBytes payload (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_bignum factory encoded))

dayDuckValue :: DuckDBV2ConnectionHandle -> Day -> IO DuckDBV2ValueHandle
dayDuckValue conn value = encodeDay value >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_date factory encoded))

timeOfDayDuckValue :: DuckDBV2ConnectionHandle -> TimeOfDay -> IO DuckDBV2ValueHandle
timeOfDayDuckValue conn value = encodeTimeOfDay value >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_time factory encoded))

localTimeDuckValue :: DuckDBV2ConnectionHandle -> LocalTime -> IO DuckDBV2ValueHandle
localTimeDuckValue conn value = encodeLocalTime value >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_timestamp factory encoded))

utcTimeDuckValue :: DuckDBV2ConnectionHandle -> UTCTime -> IO DuckDBV2ValueHandle
utcTimeDuckValue conn = utcTimestampDuckValue conn . Finite

dateDuckValue :: DuckDBV2ConnectionHandle -> Date -> IO DuckDBV2ValueHandle
dateDuckValue conn value = encodeUnbounded encodeDay value >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_date factory encoded))

localTimestampDuckValue :: DuckDBV2ConnectionHandle -> LocalTimestamp -> IO DuckDBV2ValueHandle
localTimestampDuckValue conn value = encodeUnbounded (encodeTimestampUnits 1000000) value >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_timestamp factory encoded))

utcTimestampDuckValue :: DuckDBV2ConnectionHandle -> UTCTimestamp -> IO DuckDBV2ValueHandle
utcTimestampDuckValue conn value = encodeUnbounded (encodeTimestampUnits 1000000 . utcToLocalTime utc) value >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_timestamp_tz factory encoded))

-- | Construct an ARRAY value and retain its element type for empty arrays.
arrayDuckValue :: forall a. (DuckDBColumnType a) => (LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle) -> DuckDBV2ConnectionHandle -> Maybe LogicalTypeRep -> (a -> IO DuckDBV2ValueHandle) -> Array Int a -> IO DuckDBV2ValueHandle
arrayDuckValue makeType conn target elementValue array = do
    when (null (elems array)) $
        throwIO (SQLError "duckdb-simple: empty ARRAY parameters are unavailable in DuckDB's V2 API; use an explicit SQL empty LIST" Nothing Nothing)
    withCreatedValues (map elementValue (elems array)) \values ->
        withElementType values \elementType ->
            withArray values \ptr ->
                factoryOutput conn (\factory -> c_duckdb_v2_value_create_array factory elementType (ConstPtr ptr) (fromIntegral (length values)))
  where
    typeName = duckdbColumnType (Proxy :: Proxy a)
    withElementType values action = case duckDBTypeFromName typeName of
        Just dtype -> bracket (makeType (timestampTypeForTarget target (LogicalTypeScalar dtype))) destroyLogicalType action
        Nothing -> do
            present <- filterM (fmap (== 0) . output . c_duckdb_v2_value_is_null) values
            case present of
                value : rest -> bracket (output (c_duckdb_v2_value_get_logical_type value)) destroyLogicalType \logical -> do
                    expected <- logicalTypeToRep logical
                    forM_ rest \element -> do
                        actual <- bracket (output (c_duckdb_v2_value_get_logical_type element)) destroyLogicalType logicalTypeToRep
                        when (actual /= expected) $ throwIO (userError "duckdb-simple: array elements have different logical types")
                    action logical
                [] ->
                    throwIO
                        SQLError
                            { sqlErrorMessage = "duckdb-simple: an empty or all-NULL array of " <> typeName <> " elements has no element type"
                            , sqlErrorType = Nothing
                            , sqlErrorQuery = Nothing
                            }

-- | Construct named fields. The native call copies the field values.
structValueDuckValue :: (LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle) -> DuckDBV2ConnectionHandle -> StructValue FieldValue -> IO DuckDBV2ValueHandle
structValueDuckValue makeType conn StructValue{structValueFields, structValueTypes} = do
    let fields = elems structValueFields
        types = elems structValueTypes
        names = map structFieldName fields
    when (names /= map structFieldName types) $
        throwIO (userError "duckdb-simple: struct value/type fields mismatch")
    withCreatedValues (zipWith (\field logical -> fieldValueWithTypeDuckValue makeType conn (structFieldValue logical) (structFieldValue field)) fields types) \values ->
        withIdentifiers names \namePtr -> withArray values \valuePtr ->
            declaredCompositeValue makeType conn (LogicalTypeStruct structValueTypes) $
                factoryOutput conn (\factory -> c_duckdb_v2_value_create_struct factory (ConstPtr namePtr) (ConstPtr valuePtr) (fromIntegral (length values)))

-- | Construct positional fields without inferring a TUPLE from field names.
tupleValueDuckValue :: (LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle) -> DuckDBV2ConnectionHandle -> TupleValue FieldValue -> IO DuckDBV2ValueHandle
tupleValueDuckValue makeType conn TupleValue{tupleValueFields, tupleValueTypes} = do
    let fields = elems tupleValueFields
        types = elems tupleValueTypes
    when (length fields /= length types) $
        throwIO (userError "duckdb-simple: tuple value/type fields mismatch")
    withCreatedValues (zipWith (fieldValueWithTypeDuckValue makeType conn) types fields) \values ->
        withArray values \valuePtr ->
            declaredCompositeValue makeType conn (LogicalTypeTuple tupleValueTypes) $
                factoryOutput conn (\factory -> c_duckdb_v2_value_create_tuple factory (ConstPtr valuePtr) (fromIntegral (length values)))

{- | Preserve declared field types through an owned vector. Scalar VARCHAR
values do not retain collations, but their enclosing composite type does.
-}
declaredCompositeValue :: (LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle) -> DuckDBV2ConnectionHandle -> LogicalTypeRep -> IO DuckDBV2ValueHandle -> IO DuckDBV2ValueHandle
declaredCompositeValue _ _ rep create | not (hasCollation rep) = create
declaredCompositeValue makeType conn rep create = bracket create destroyValue \value ->
    bracket (makeType rep) destroyLogicalType \logical -> withArray [logical] \types ->
        bracket (factoryOutput conn (\factory -> c_duckdb_v2_data_chunk_create factory (ConstPtr types) 1)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
            vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
            checked (c_duckdb_v2_vector_set_size vector 1)
            checked (c_duckdb_v2_vector_set_value vector 0 value)
            output (c_duckdb_v2_vector_get_value vector 0)

-- | Test whether a type tree contains a VARCHAR collation.
hasCollation :: LogicalTypeRep -> Bool
hasCollation rep = case rep of
    LogicalTypeCollatedVarchar _ -> True
    LogicalTypeList child -> hasCollation child
    LogicalTypeArray child _ -> hasCollation child
    LogicalTypeMap key value -> hasCollation key || hasCollation value
    LogicalTypeStruct fields -> any (hasCollation . structFieldValue) (elems fields)
    LogicalTypeTuple fields -> any hasCollation (elems fields)
    LogicalTypeUnion members -> any (hasCollation . unionMemberType) (elems members)
    _ -> False

-- | Build one UNION row through its public tag and member vectors.
unionValueDuckValue :: (LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle) -> DuckDBV2ConnectionHandle -> UnionValue FieldValue -> IO DuckDBV2ValueHandle
unionValueDuckValue makeType conn UnionValue{unionValueIndex, unionValueLabel, unionValuePayload, unionValueMembers} = do
    let members = elems unionValueMembers
        index = fromIntegral unionValueIndex
    when (index >= length members) $ throwIO (userError "duckdb-simple: union value tag out of range")
    let UnionMemberType name rep = members !! index
    when (name /= unionValueLabel) $ throwIO (userError "duckdb-simple: union tag and member name mismatch")
    bracket (makeType (LogicalTypeUnion unionValueMembers)) destroyLogicalType \logical ->
        withArray [logical] \typePtr ->
            bracket (factoryOutput conn (\factory -> c_duckdb_v2_data_chunk_create factory (ConstPtr typePtr) 1)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
                vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                checked (c_duckdb_v2_vector_set_size vector 1)
                tag <- output (c_duckdb_v2_vector_get_child vector 0)
                checked (c_duckdb_v2_vector_set_size tag 1)
                bracket (uint8DuckValue conn (fromIntegral index)) destroyValue \value ->
                    checked (c_duckdb_v2_vector_set_value tag 0 value)
                forM_ [0 .. length members - 1] \memberIndex -> do
                    member <- output (c_duckdb_v2_vector_get_child vector (fromIntegral memberIndex + 1))
                    checked (c_duckdb_v2_vector_set_size member 1)
                    if memberIndex == index
                        then bracket (fieldValueWithTypeDuckValue makeType conn rep unionValuePayload) destroyValue \value ->
                            checked (c_duckdb_v2_vector_set_value member 0 value)
                        else checked (c_duckdb_v2_vector_set_null member 0)
                output (c_duckdb_v2_vector_get_value vector 0)

-- | Construct a value with explicit logical metadata, including typed NULL.
fieldValueWithTypeDuckValue :: (LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle) -> DuckDBV2ConnectionHandle -> LogicalTypeRep -> FieldValue -> IO DuckDBV2ValueHandle
fieldValueWithTypeDuckValue makeType conn rep FieldNull = bracket (makeType rep) destroyLogicalType \logical ->
    factoryOutput conn (\factory -> c_duckdb_v2_value_create_null factory logical)
fieldValueWithTypeDuckValue makeType conn rep value = case (rep, value) of
    (LogicalTypeScalar DuckDBV2LogicalTypeIdVariant, _) -> variantDuckValue makeType conn value
    (LogicalTypeScalar dtype, _) -> scalarFieldValueDuckValue conn dtype value
    (LogicalTypeCollatedVarchar _, FieldText text) -> textDuckValue conn text
    (LogicalTypeGeometry _, FieldGeometry{}) -> unsupportedRawGeometryBinding
    (LogicalTypeDecimal width scale, FieldDecimal decimal@DecimalValue{decimalWidth, decimalScale})
        | width == decimalWidth && scale == decimalScale -> decimalDuckValue conn decimal
    (LogicalTypeList child, FieldList values) -> sequenceValue c_duckdb_v2_value_create_list child values
    (LogicalTypeArray child size, FieldArray array)
        | fromIntegral (length (elems array)) == size -> sequenceValue c_duckdb_v2_value_create_array child (elems array)
    (LogicalTypeMap keyRep valueRep, FieldMap pairs) ->
        bracket (makeType keyRep) destroyLogicalType \keyType ->
            bracket (makeType valueRep) destroyLogicalType \valueType ->
                withCreatedValues (map (fieldValueWithTypeDuckValue makeType conn keyRep . fst) pairs) \keys ->
                    withCreatedValues (map (fieldValueWithTypeDuckValue makeType conn valueRep . snd) pairs) \values ->
                        withArray keys \keyPtr -> withArray values \valuePtr ->
                            factoryOutput conn (\factory -> c_duckdb_v2_value_create_map factory keyType valueType (ConstPtr keyPtr) (ConstPtr valuePtr) (fromIntegral (length pairs)))
    (LogicalTypeStruct _, FieldStruct struct)
        | rep == structValueTypeRep struct -> structValueDuckValue makeType conn struct
    (LogicalTypeTuple _, FieldTuple tuple)
        | rep == tupleValueTypeRep tuple -> tupleValueDuckValue makeType conn tuple
    (LogicalTypeUnion _, FieldUnion union)
        | rep == unionValueTypeRep union -> unionValueDuckValue makeType conn union
    (LogicalTypeEnum entries, FieldEnum index) -> enumDuckValue conn entries index
    _ -> typeMismatch (show rep) value
  where
    sequenceValue create child values = bracket (makeType child) destroyLogicalType \logical ->
        withCreatedValues (map (fieldValueWithTypeDuckValue makeType conn child) values) \native -> withArray native \ptr ->
            withConnectionFactory conn \factory -> output (create factory logical (ConstPtr ptr) (fromIntegral (length native)))

scalarFieldValueDuckValue :: DuckDBV2ConnectionHandle -> DuckDBV2LogicalTypeId -> FieldValue -> IO DuckDBV2ValueHandle
scalarFieldValueDuckValue conn dtype value = case (dtype, value) of
    (DuckDBV2LogicalTypeIdBoolean, FieldBool b) -> boolDuckValue conn b
    (DuckDBV2LogicalTypeIdTinyInt, FieldInt8 i) -> int8DuckValue conn i
    (DuckDBV2LogicalTypeIdSmallInt, FieldInt16 i) -> int16DuckValue conn i
    (DuckDBV2LogicalTypeIdInteger, FieldInt32 i) -> int32DuckValue conn i
    (DuckDBV2LogicalTypeIdBigInt, FieldInt64 i) -> int64DuckValue conn i
    (DuckDBV2LogicalTypeIdUTinyInt, FieldWord8 w) -> uint8DuckValue conn w
    (DuckDBV2LogicalTypeIdUSmallInt, FieldWord16 w) -> uint16DuckValue conn w
    (DuckDBV2LogicalTypeIdUInteger, FieldWord32 w) -> uint32DuckValue conn w
    (DuckDBV2LogicalTypeIdUBigInt, FieldWord64 w) -> uint64DuckValue conn w
    (DuckDBV2LogicalTypeIdFloat, FieldFloat f) -> floatDuckValue conn f
    (DuckDBV2LogicalTypeIdDouble, FieldDouble d) -> doubleDuckValue conn d
    (DuckDBV2LogicalTypeIdVarchar, FieldText t) -> textDuckValue conn t
    (DuckDBV2LogicalTypeIdBlob, FieldBlob b) -> blobDuckValue conn b
    (DuckDBV2LogicalTypeIdGeometry, FieldGeometry{}) -> unsupportedRawGeometryBinding
    (DuckDBV2LogicalTypeIdUUID, FieldUUID u) -> uuidDuckValue conn u
    (DuckDBV2LogicalTypeIdBit, FieldBit bits) -> bitDuckValue conn bits
    (DuckDBV2LogicalTypeIdDate, FieldDate d) -> dateDuckValue conn d
    (DuckDBV2LogicalTypeIdTime, FieldTime t) -> timeOfDayDuckValue conn t
    (DuckDBV2LogicalTypeIdTimeNs, FieldTime t) ->
        timeOfDayUnits 1000000000 t >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_time_ns factory encoded)) . fromInteger
    (DuckDBV2LogicalTypeIdTimeTz, FieldTimeTZ tz) -> timeWithZoneDuckValue conn tz
    (DuckDBV2LogicalTypeIdTimestamp, FieldTimestamp ts) -> localTimestampDuckValue conn ts
    (DuckDBV2LogicalTypeIdTimestampSec, FieldTimestamp ts) ->
        encodeUnbounded (encodeTimestampUnits 1) ts >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_timestamp_sec factory encoded))
    (DuckDBV2LogicalTypeIdTimestampMs, FieldTimestamp ts) ->
        encodeUnbounded (encodeTimestampUnits 1000) ts >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_timestamp_ms factory encoded))
    (DuckDBV2LogicalTypeIdTimestampNs, FieldTimestamp ts) ->
        encodeUnbounded (encodeTimestampUnits 1000000000) ts >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_timestamp_ns factory encoded))
    (DuckDBV2LogicalTypeIdTimestampTz, FieldTimestampTZ ts) -> utcTimestampDuckValue conn ts
    (DuckDBV2LogicalTypeIdTimestampTzNs, FieldTimestampTZ ts) -> utcTimestampNsDuckValue conn ts
    (DuckDBV2LogicalTypeIdInterval, FieldInterval iv) -> intervalDuckValue conn iv
    (DuckDBV2LogicalTypeIdHugeInt, FieldHugeInt i) -> hugeIntDuckValue conn i
    (DuckDBV2LogicalTypeIdUHugeInt, FieldUHugeInt i) -> uhugeIntDuckValue conn i
    (DuckDBV2LogicalTypeIdBigNum, FieldBigNum big) -> bigNumDuckValue conn big
    (DuckDBV2LogicalTypeIdSQLNull, _) -> nullDuckValue conn
    _ ->
        case value of
            FieldNull -> nullDuckValue conn
            other ->
                throwIO
                    ( userError
                        ( "duckdb-simple: unsupported scalar conversion for "
                            <> show dtype
                            <> " from "
                            <> show other
                        )
                    )

enumDuckValue :: DuckDBV2ConnectionHandle -> Array Int Text -> Word32 -> IO DuckDBV2ValueHandle
enumDuckValue conn entries index = do
    let dictionary = elems entries
    when (toInteger index >= toInteger (length dictionary)) $ throwIO (userError "duckdb-simple: ENUM index out of range")
    bracket (logicalTypeFromRep conn (LogicalTypeEnum entries)) destroyLogicalType \logical ->
        bracket (textDuckValue conn (dictionary !! fromIntegral index)) destroyValue \value ->
            factoryOutput conn (\factory -> c_duckdb_v2_value_cast factory value logical)

hugeIntDuckValue :: DuckDBV2ConnectionHandle -> Integer -> IO DuckDBV2ValueHandle
hugeIntDuckValue conn value = integerToHugeInt value >>= \huge -> with huge (\ptr -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_hugeint factory (ConstPtr ptr)))

uhugeIntDuckValue :: DuckDBV2ConnectionHandle -> Integer -> IO DuckDBV2ValueHandle
uhugeIntDuckValue conn value = integerToUHugeInt value >>= \huge -> with huge (\ptr -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_uhugeint factory (ConstPtr ptr)))

decimalDuckValue :: DuckDBV2ConnectionHandle -> DecimalValue -> IO DuckDBV2ValueHandle
decimalDuckValue conn DecimalValue{decimalWidth, decimalScale, decimalInteger} = do
    when (decimalWidth < 1 || decimalWidth > 38 || decimalScale > decimalWidth) $ throwIO (userError "duckdb-simple: invalid DECIMAL width or scale")
    when (abs decimalInteger >= 10 ^ decimalWidth) $ throwIO (userError "duckdb-simple: DECIMAL value exceeds declared precision")
    huge <- integerToHugeInt decimalInteger
    with huge (\ptr -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_decimal factory (ConstPtr ptr) decimalWidth decimalScale))

intervalDuckValue :: DuckDBV2ConnectionHandle -> IntervalValue -> IO DuckDBV2ValueHandle
intervalDuckValue conn IntervalValue{intervalMonths, intervalDays, intervalMicros} =
    with (DuckDBV2IntervalT intervalMonths intervalDays intervalMicros) (\ptr -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_interval factory (ConstPtr ptr)))

timeWithZoneDuckValue :: DuckDBV2ConnectionHandle -> TimeWithZone -> IO DuckDBV2ValueHandle
timeWithZoneDuckValue conn TimeWithZone{timeWithZoneTime, timeWithZoneZone} = do
    micros <- timeOfDayUnits 1000000 timeWithZoneTime
    let offset = toInteger (timeZoneMinutes timeWithZoneZone) * 60
    when (abs offset > 57599) $ throwIO (userError "duckdb-simple: TIME WITH TIME ZONE offset out of range")
    factoryOutput conn (\factory -> c_duckdb_v2_value_create_time_tz factory ((fromIntegral micros `shiftL` 24) .|. fromIntegral (57599 - offset)))

integerToHugeInt :: Integer -> IO DuckDBV2HugeintT
integerToHugeInt value = do
    when (value < negate (1 `shiftL` 127) || value >= 1 `shiftL` 127) $ throwIO (userError "duckdb-simple: HUGEINT value out of range")
    pure (DuckDBV2HugeintT (fromIntegral value) (fromIntegral (value `shiftR` 64)))

integerToUHugeInt :: Integer -> IO DuckDBV2UhugeintT
integerToUHugeInt value = do
    when (value < 0 || value >= 1 `shiftL` 128) $ throwIO (userError "duckdb-simple: UHUGEINT value out of range")
    pure (DuckDBV2UhugeintT (fromIntegral value) (fromIntegral (value `shiftR` 64)))

-- | Borrow generated identifier views and reject embedded NUL bytes.
withIdentifiers :: [Text] -> (Ptr DuckDBV2IdentifierT -> IO a) -> IO a
withIdentifiers names action = do
    when (any (Text.any (== '\0')) names) $
        throwIO (userError "duckdb-simple: logical type name contains NUL")
    withIdentifierArray names action

typeMismatch :: String -> FieldValue -> IO a
typeMismatch expected actual = throwIO (userError ("duckdb-simple: cannot encode " <> show actual <> " as " <> expected))

-- | Raw WKB binding requires an explicit geometry conversion.
unsupportedRawGeometryBinding :: IO a
unsupportedRawGeometryBinding = throwIO (userError "duckdb-simple: raw GEOMETRY binding requires explicit ST_GeomFromWKB and ST_SetCRS parameters")

-- | Cast the payload to VARIANT with the supplied native connection.
variantDuckValue :: (LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle) -> DuckDBV2ConnectionHandle -> FieldValue -> IO DuckDBV2ValueHandle
variantDuckValue makeType conn value = do
    (rep, payload) <- variantPayloadType value
    bracket (makeType (LogicalTypeScalar DuckDBV2LogicalTypeIdVariant)) destroyLogicalType \logical ->
        bracket (fieldValueWithTypeDuckValue makeType conn rep payload) destroyValue \native ->
            factoryOutput conn (\factory -> c_duckdb_v2_value_cast factory native logical)

-- | Construct a UTC nanosecond value without losing sub-microsecond digits.
utcTimestampNsDuckValue :: DuckDBV2ConnectionHandle -> UTCTimestamp -> IO DuckDBV2ValueHandle
utcTimestampNsDuckValue conn value = encodeUnbounded (encodeTimestampUnits 1000000000 . utcToLocalTime utc) value >>= (\encoded -> factoryOutput conn (\factory -> c_duckdb_v2_value_create_timestamp_tz_ns factory encoded))

instance ToDuckValue G.Geometry where
    toDuckValue conn geometry = do
        wkt <- either (throwIO . userError) pure (WKT.encodeWKT geometry)
        bracket (logicalTypeFromRep conn (LogicalTypeGeometry Nothing)) destroyLogicalType \logical ->
            bracket (textDuckValue conn wkt) destroyValue \value ->
                factoryOutput conn (\factory -> c_duckdb_v2_value_cast factory value logical)

instance ToDuckValue Variant where
    toDuckValue conn = variantDuckValue (logicalTypeFromRep conn) conn . variantPayload

{- | Choose the native type of a VARIANT payload. Containers get VARIANT
elements and fields. Time values with sub-microsecond digits use nanosecond
types. Wide timestamps use milliseconds or seconds when these preserve the
value and fit the native range.
The C API cannot construct a nonempty object whose keys are all empty.
DuckDB 2.0 treats that STRUCT type as a tuple and casts it to a VARIANT array.
-}
variantPayloadType :: FieldValue -> IO (LogicalTypeRep, FieldValue)
variantPayloadType value = case value of
    FieldNull -> pure (variant, value)
    FieldBool{} -> scalar DuckDBV2LogicalTypeIdBoolean
    FieldInt8{} -> scalar DuckDBV2LogicalTypeIdTinyInt
    FieldInt16{} -> scalar DuckDBV2LogicalTypeIdSmallInt
    FieldInt32{} -> scalar DuckDBV2LogicalTypeIdInteger
    FieldInt64{} -> scalar DuckDBV2LogicalTypeIdBigInt
    FieldWord8{} -> scalar DuckDBV2LogicalTypeIdUTinyInt
    FieldWord16{} -> scalar DuckDBV2LogicalTypeIdUSmallInt
    FieldWord32{} -> scalar DuckDBV2LogicalTypeIdUInteger
    FieldWord64{} -> scalar DuckDBV2LogicalTypeIdUBigInt
    FieldHugeInt{} -> scalar DuckDBV2LogicalTypeIdHugeInt
    FieldUHugeInt{} -> scalar DuckDBV2LogicalTypeIdUHugeInt
    FieldFloat{} -> scalar DuckDBV2LogicalTypeIdFloat
    FieldDouble{} -> scalar DuckDBV2LogicalTypeIdDouble
    FieldDecimal DecimalValue{decimalWidth, decimalScale} -> pure (LogicalTypeDecimal decimalWidth decimalScale, value)
    FieldText{} -> scalar DuckDBV2LogicalTypeIdVarchar
    FieldBlob{} -> scalar DuckDBV2LogicalTypeIdBlob
    FieldUUID{} -> scalar DuckDBV2LogicalTypeIdUUID
    FieldDate{} -> scalar DuckDBV2LogicalTypeIdDate
    FieldTime time
        | hasNanos time -> scalar DuckDBV2LogicalTypeIdTimeNs
        | otherwise -> scalar DuckDBV2LogicalTypeIdTime
    FieldTimestamp (Finite LocalTime{localDay, localTimeOfDay})
        | hasNanos localTimeOfDay -> scalar DuckDBV2LogicalTypeIdTimestampNs
        | inFiniteRange (minBound :: Int64) maxBound micros -> scalar DuckDBV2LogicalTypeIdTimestamp
        | micros `rem` 1000 == 0 && inFiniteRange (minBound :: Int64) maxBound (micros `div` 1000) -> scalar DuckDBV2LogicalTypeIdTimestampMs
        | micros `rem` 1000000 == 0 && inFiniteRange (minBound :: Int64) maxBound (micros `div` 1000000) -> scalar DuckDBV2LogicalTypeIdTimestampSec
      where
        micros = diffDays localDay (fromGregorian 1970 1 1) * 86400 * 1000000 + diffTimeToPicoseconds (timeOfDayToTime localTimeOfDay) `div` 1000000
    FieldTimestamp{} -> scalar DuckDBV2LogicalTypeIdTimestamp
    FieldTimestampTZ instant -> scalar (variantUtcTimestampType instant)
    FieldTimeTZ{} -> scalar DuckDBV2LogicalTypeIdTimeTz
    FieldInterval{} -> scalar DuckDBV2LogicalTypeIdInterval
    FieldBigNum{} -> scalar DuckDBV2LogicalTypeIdBigNum
    FieldBit{} -> scalar DuckDBV2LogicalTypeIdBit
    FieldList{} -> pure (LogicalTypeList variant, value)
    FieldArray items -> pure (LogicalTypeList variant, FieldList (elems items))
    FieldTuple TupleValue{tupleValueFields} -> pure (LogicalTypeList variant, FieldList (elems tupleValueFields))
    FieldStruct structValue@StructValue{structValueTypes} -> do
        let names = map structFieldName (elems structValueTypes)
        when (length names /= length (List.nub names)) $
            throwIO (userError "duckdb-simple: VARIANT objects need unique keys")
        when (not (null names) && all Text.null names) $
            throwIO (userError "duckdb-simple: DuckDB's C API cannot construct a VARIANT object with only an empty key")
        let types = fmap (\field -> field{structFieldValue = variant}) structValueTypes
        pure (LogicalTypeStruct types, FieldStruct structValue{structValueTypes = types})
    FieldUnion unionValue -> pure (unionValueTypeRep unionValue, value)
    FieldGeometry{} -> unsupportedRawGeometryBinding
    FieldMap{} -> throwIO (userError "duckdb-simple: VARIANT payloads cannot contain MAP values")
    FieldEnum{} -> throwIO (userError "duckdb-simple: VARIANT payloads cannot contain ENUM values")
  where
    variant = LogicalTypeScalar DuckDBV2LogicalTypeIdVariant
    scalar dtype = pure (LogicalTypeScalar dtype, value)
    hasNanos time = snd (properFraction (todSec time * 1000000) :: (Integer, Pico)) /= 0

-- | Choose nanosecond storage for a precise UTC VARIANT payload.
variantUtcTimestampType :: UTCTimestamp -> DuckDBV2LogicalTypeId
variantUtcTimestampType (Finite instant)
    | snd (properFraction (todSec (localTimeOfDay (utcToLocalTime utc instant)) * 1000000) :: (Integer, Pico)) /= 0 = DuckDBV2LogicalTypeIdTimestampTzNs
variantUtcTimestampType _ = DuckDBV2LogicalTypeIdTimestampTz

instance ToDuckValue Null where
    toDuckValue conn _ = nullDuckValue conn

instance ToDuckValue Bool where
    toDuckValue = boolDuckValue

instance ToDuckValue Int where
    toDuckValue conn = int64DuckValue conn . fromIntegral

instance ToDuckValue Int8 where
    toDuckValue = int8DuckValue

instance ToDuckValue Int16 where
    toDuckValue = int16DuckValue

instance ToDuckValue Int32 where
    toDuckValue = int32DuckValue

instance ToDuckValue Int64 where
    toDuckValue = int64DuckValue

instance ToDuckValue BigNum where
    toDuckValue = bigNumDuckValue

instance ToDuckValue UUID.UUID where
    toDuckValue = uuidDuckValue

instance ToDuckValue Integer where
    toDuckValue conn = bigNumDuckValue conn . BigNum

instance ToDuckValue Natural where
    toDuckValue conn = bigNumDuckValue conn . BigNum . toInteger

instance ToDuckValue Word where
    toDuckValue conn = uint64DuckValue conn . fromIntegral

instance ToDuckValue Word16 where
    toDuckValue = uint16DuckValue

instance ToDuckValue Word32 where
    toDuckValue = uint32DuckValue

instance ToDuckValue Word64 where
    toDuckValue = uint64DuckValue

instance ToDuckValue Word8 where
    toDuckValue = uint8DuckValue

instance ToDuckValue Double where
    toDuckValue = doubleDuckValue

instance ToDuckValue Float where
    toDuckValue = floatDuckValue

instance ToDuckValue Text where
    toDuckValue = textDuckValue

instance ToDuckValue String where
    toDuckValue = stringDuckValue

instance ToDuckValue BS.ByteString where
    toDuckValue = blobDuckValue

instance ToDuckValue BitString where
    toDuckValue = bitDuckValue

instance ToDuckValue Day where
    toDuckValue = dayDuckValue

instance ToDuckValue TimeOfDay where
    toDuckValue = timeOfDayDuckValue

instance ToDuckValue LocalTime where
    toDuckValue = localTimeDuckValue

instance ToDuckValue UTCTime where
    toDuckValue = utcTimeDuckValue

instance ToDuckValue (Unbounded Day) where
    toDuckValue = dateDuckValue

instance ToDuckValue (Unbounded LocalTime) where
    toDuckValue = localTimestampDuckValue

instance ToDuckValue (Unbounded UTCTime) where
    toDuckValue = utcTimestampDuckValue

instance ToDuckValue (StructValue FieldValue) where
    toDuckValue conn = structValueDuckValue (logicalTypeFromRep conn) conn

instance ToDuckValue (TupleValue FieldValue) where
    toDuckValue conn = tupleValueDuckValue (logicalTypeFromRep conn) conn

instance ToDuckValue (UnionValue FieldValue) where
    toDuckValue conn = unionValueDuckValue (logicalTypeFromRep conn) conn

-- | Construct an array with the supplied connection and element type.
instance (DuckDBColumnType a, ToDuckValue a) => ToDuckValue (Array Int a) where
    toDuckValue conn = arrayDuckValue (logicalTypeFromRep conn) conn Nothing (toDuckValue conn)

instance (ToDuckValue a) => ToDuckValue (Maybe a) where
    toDuckValue conn Nothing = nullDuckValue conn
    toDuckValue conn (Just value) = toDuckValue conn value

-- | Preserve infinity sentinels and validate finite values before narrowing.
encodeUnbounded :: (Integral b, Bounded b) => (a -> IO b) -> Unbounded a -> IO b
encodeUnbounded _ NegInfinity = pure (negate maxBound)
encodeUnbounded _ PosInfinity = pure maxBound
encodeUnbounded encode (Finite value) = encode value

-- | Encode finite dates as days from the Unix epoch.
encodeDay :: Day -> IO Int32
encodeDay day = do
    let days = diffDays day (fromGregorian 1970 1 1)
    checkFiniteRange "DATE" (minBound :: Int32) maxBound days
    pure (fromInteger days)

-- | Encode a time of day at DuckDB microsecond precision.
encodeTimeOfDay :: TimeOfDay -> IO Int64
encodeTimeOfDay tod = fromInteger <$> timeOfDayUnits 1000000 tod

-- | Encode finite timestamps without native calendar conversions.
encodeLocalTime :: LocalTime -> IO Int64
encodeLocalTime = encodeTimestampUnits 1000000

-- | Encode finite timestamps in the requested number of units per second.
encodeTimestampUnits :: Integer -> LocalTime -> IO Int64
encodeTimestampUnits units LocalTime{localDay, localTimeOfDay} = do
    time <- timeOfDayUnits units localTimeOfDay
    let total = diffDays localDay (fromGregorian 1970 1 1) * 86400 * units + time
    checkFiniteRange "TIMESTAMP" (minBound :: Int64) maxBound total
    pure (fromInteger total)

-- | Check storage limits and the two DuckDB infinity sentinels.
checkFiniteRange :: (Integral a) => String -> a -> a -> Integer -> IO ()
checkFiniteRange label lower upper value =
    unless (inFiniteRange lower upper value) $
        throwIO (userError ("duckdb-simple: " <> label <> " value out of finite range"))

-- | Check storage limits without accepting the two infinity sentinels.
inFiniteRange :: (Integral a) => a -> a -> Integer -> Bool
inFiniteRange lower upper value =
    value >= toInteger lower && value < toInteger upper && value /= negate (toInteger upper)

-- | Validate time components and convert to the requested units per second.
timeOfDayUnits :: Integer -> TimeOfDay -> IO Integer
timeOfDayUnits units tod@(TimeOfDay hours minutes seconds) = do
    when (hours < 0 || hours > 24 || minutes < 0 || minutes > 59 || seconds < 0 || seconds >= 61 || (hours == 24 && (minutes /= 0 || seconds /= 0))) $
        throwIO (userError "duckdb-simple: invalid time of day")
    let value = diffTimeToPicoseconds (timeOfDayToTime tod) `div` (1000000000000 `div` units)
    when (value > 86400 * units) $
        throwIO (userError "duckdb-simple: time of day out of range")
    pure value

-- | Transfer a newly constructed value to the statement on success.
bindDuckValue :: Statement -> DuckDBIdx -> IO DuckDBV2ValueHandle -> IO ()
bindDuckValue statement index make = mask_ do
    value <- make
    storeBoundValue statement index value `onException` destroyValue value
