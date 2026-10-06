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

import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import Data.Array (Array, elems)
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Fixed (Pico)
import qualified Data.Geometry as G
import qualified Data.Geometry.WKT as WKT
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.List as List
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), TimeZone (..), timeOfDayToTime, timeZoneMinutes, utc, utcToLocalTime)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (BigNum (..), BitString (..), DecimalValue (..), FieldValue (..), IntervalValue (..), TimeWithZone (..), toBigNumBytes)
import Database.DuckDB.Simple.Internal (
    SQLError (..),
    Statement (..),
    withStatementHandle,
    withTypeCache,
 )
import Database.DuckDB.Simple.LogicalRep (
    LogicalTypeRep (..),
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..),
    logicalTypeFromRep,
    logicalTypeFromRepWith,
    structValueTypeRep,
    unionValueTypeRep,
 )
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import Database.DuckDB.Simple.TypeCache (TypeCache, cachedLogicalType)
import Database.DuckDB.Simple.Types (Null (..))
import Database.DuckDB.Simple.Variant (Variant (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CDouble (..), CFloat (..))
import Foreign.Marshal (fromBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (poke)
import Numeric.Natural (Natural)

-- | Represents a named parameter binding using the @:=@ operator.
data NamedParam where
    (:=) :: (ToField a) => Text -> a -> NamedParam

infixr 3 :=

-- | Encapsulates the action required to bind a single positional parameter, together with a textual description used in diagnostics.
data FieldBinding = FieldBinding
    { fieldBindingValue :: !(TypeCache -> IO DuckDBValue)
    , fieldBindingDisplay :: !String
    }

-- | Low-level class for values that can be marshalled directly into `DuckDBValue`s.
class (DuckDBColumnType a) => ToDuckValue a where
    -- | Convert a Haskell value into an owned DuckDB boxed value.
    toDuckValue :: a -> IO DuckDBValue

valueBinding :: String -> IO DuckDBValue -> FieldBinding
valueBinding display = cacheValueBinding display . const

-- | Construct a value with the type cache of the statement's connection.
cacheValueBinding :: String -> (TypeCache -> IO DuckDBValue) -> FieldBinding
cacheValueBinding display makeValue =
    FieldBinding
        { fieldBindingValue = makeValue
        , fieldBindingDisplay = display
        }

-- | Build types with the cached types for VARIANT and GEOMETRY with a CRS.
cachedTypeFromRep :: TypeCache -> LogicalTypeRep -> IO DuckDBLogicalType
cachedTypeFromRep = logicalTypeFromRepWith . cachedLogicalType

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
        bindDuckValue stmt idx (fieldBindingValue cache)

-- | Render a bound parameter for error reporting.
renderFieldBinding :: FieldBinding -> String
renderFieldBinding FieldBinding{fieldBindingDisplay} = fieldBindingDisplay

-- | Types that can be used as positional parameters.
class ToField a where
    toField :: a -> FieldBinding
    default toField :: (Show a, ToDuckValue a) => a -> FieldBinding
    toField value = valueBinding (show value) (toDuckValue value)

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
            variantDuckValue (cachedTypeFromRep cache) (variantPayload value)

instance ToField UTCTime
instance ToField (Unbounded Day)
instance ToField (Unbounded LocalTime)
instance ToField (Unbounded UTCTime)

instance ToField BigNum where
    toField big@(BigNum n) = valueBinding (show n) (bigNumDuckValue big)

instance ToField (StructValue FieldValue) where
    toField structVal =
        cacheValueBinding "<struct>" \cache ->
            structValueDuckValue (cachedTypeFromRep cache) structVal

instance ToField (UnionValue FieldValue) where
    toField unionVal =
        let label = Text.unpack (unionValueLabel unionVal)
         in cacheValueBinding ("<union " <> label <> ">") \cache ->
                unionValueDuckValue (cachedTypeFromRep cache) unionVal

instance DuckDBColumnType BitString where
    duckdbColumnTypeFor _ = "BIT"

instance ToField BS.ByteString where
    toField bs =
        valueBinding
            ("<blob length=" <> show (BS.length bs) <> ">")
            (toDuckValue bs)

instance (DuckDBColumnType a, ToField a) => ToField (Array Int a) where
    toField arr =
        cacheValueBinding
            ("<array length=" <> show (length (elems arr)) <> ">")
            (`arrayDuckValue` arr)

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

instance DuckDBColumnType (UnionValue FieldValue) where
    duckdbColumnTypeFor _ = "UNION"

instance (DuckDBColumnType a) => DuckDBColumnType (Maybe a) where
    duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy :: Proxy a)

instance (DuckDBColumnType a) => DuckDBColumnType (Array Int a) where
    duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy :: Proxy a) <> Text.pack "[]"

nullBinding :: String -> FieldBinding
nullBinding repr = valueBinding repr nullDuckValue

nullDuckValue :: IO DuckDBValue
nullDuckValue = c_duckdb_create_null_value

boolDuckValue :: Bool -> IO DuckDBValue
boolDuckValue value = c_duckdb_create_bool (if value then 1 else 0)

int8DuckValue :: Int8 -> IO DuckDBValue
int8DuckValue = c_duckdb_create_int8

int16DuckValue :: Int16 -> IO DuckDBValue
int16DuckValue = c_duckdb_create_int16

int32DuckValue :: Int32 -> IO DuckDBValue
int32DuckValue = c_duckdb_create_int32

int64DuckValue :: Int64 -> IO DuckDBValue
int64DuckValue = c_duckdb_create_int64

uint64DuckValue :: Word64 -> IO DuckDBValue
uint64DuckValue = c_duckdb_create_uint64

uint32DuckValue :: Word32 -> IO DuckDBValue
uint32DuckValue = c_duckdb_create_uint32

uint16DuckValue :: Word16 -> IO DuckDBValue
uint16DuckValue = c_duckdb_create_uint16

uint8DuckValue :: Word8 -> IO DuckDBValue
uint8DuckValue = c_duckdb_create_uint8

doubleDuckValue :: Double -> IO DuckDBValue
doubleDuckValue = c_duckdb_create_double . CDouble

floatDuckValue :: Float -> IO DuckDBValue
floatDuckValue = c_duckdb_create_float . CFloat

textDuckValue :: Text -> IO DuckDBValue
textDuckValue txt =
    BS.useAsCStringLen (TextEncoding.encodeUtf8 txt) \(ptr, len) ->
        c_duckdb_create_varchar_length ptr (fromIntegral len)

stringDuckValue :: String -> IO DuckDBValue
stringDuckValue = textDuckValue . Text.pack

blobDuckValue :: BS.ByteString -> IO DuckDBValue
blobDuckValue bs =
    BS.useAsCStringLen bs \(ptr, len) ->
        c_duckdb_create_blob (castPtr ptr :: Ptr Word8) (fromIntegral len)

uuidDuckValue :: UUID.UUID -> IO DuckDBValue
uuidDuckValue uuid =
    alloca $ \ptr -> do
        let (upper, lower) = UUID.toWords64 uuid
        poke
            ptr
            DuckDBUHugeInt
                { duckDBUHugeIntLower = lower
                , duckDBUHugeIntUpper = upper
                }
        c_duckdb_create_uuid ptr

bitDuckValue :: BitString -> IO DuckDBValue
bitDuckValue (BitString padding bits) = do
    when (BS.null bits || padding > 7) $
        throwIO (userError "duckdb-simple: BIT requires nonempty data and padding from 0 to 7")
    let nativePadding = complement ((1 `shiftL` (8 - fromIntegral padding)) - 1) :: Word8
        payload = BS.cons padding (BS.cons (BS.head bits .|. nativePadding) (BS.tail bits))
    BS.useAsCStringLen payload \(rawPtr, len) ->
        alloca \ptr -> do
            poke ptr DuckDBBit{duckDBBitData = castPtr rawPtr, duckDBBitSize = fromIntegral len}
            c_duckdb_create_bit ptr

bigNumDuckValue :: BigNum -> IO DuckDBValue
bigNumDuckValue (BigNum big) =
    let neg = fromBool (big < 0)
        payload =
            BS.pack $
                if big < 0
                    then map complement (drop 3 $ toBigNumBytes big)
                    else drop 3 $ toBigNumBytes big
        withPayload action =
            if BS.null payload
                then alloca \ptr -> do
                    poke
                        ptr
                        DuckDBBignum
                            { duckDBBignumData = nullPtr
                            , duckDBBignumSize = 0
                            , duckDBBignumIsNegative = neg
                            }
                    action ptr
                else BS.useAsCStringLen payload \(rawPtr, len) ->
                    alloca \ptr -> do
                        poke
                            ptr
                            DuckDBBignum
                                { duckDBBignumData = castPtr rawPtr
                                , duckDBBignumSize = fromIntegral len
                                , duckDBBignumIsNegative = neg
                                }
                        action ptr
     in withPayload c_duckdb_create_bignum

dayDuckValue :: Day -> IO DuckDBValue
dayDuckValue day = do
    duckDate <- encodeDay day
    c_duckdb_create_date duckDate

timeOfDayDuckValue :: TimeOfDay -> IO DuckDBValue
timeOfDayDuckValue tod = do
    duckTime <- encodeTimeOfDay tod
    c_duckdb_create_time duckTime

localTimeDuckValue :: LocalTime -> IO DuckDBValue
localTimeDuckValue ts = do
    duckTimestamp <- encodeLocalTime ts
    c_duckdb_create_timestamp duckTimestamp

utcTimeDuckValue :: UTCTime -> IO DuckDBValue
utcTimeDuckValue utcTime =
    encodeLocalTime (utcToLocalTime utc utcTime) >>= c_duckdb_create_timestamp_tz

-- | Bind a date, including either infinity sentinel.
dateDuckValue :: Date -> IO DuckDBValue
dateDuckValue value =
    encodeUnbounded (fmap unDuckDBDate . encodeDay) value >>= c_duckdb_create_date . DuckDBDate

-- | Bind a timestamp without a time zone, including infinity.
localTimestampDuckValue :: LocalTimestamp -> IO DuckDBValue
localTimestampDuckValue value =
    encodeUnbounded (encodeTimestampUnits 1000000) value >>= c_duckdb_create_timestamp . DuckDBTimestamp

-- | Bind a timestamp with a time zone, including infinity.
utcTimestampDuckValue :: UTCTimestamp -> IO DuckDBValue
utcTimestampDuckValue value =
    encodeUnbounded (encodeTimestampUnits 1000000 . utcToLocalTime utc) value >>= c_duckdb_create_timestamp_tz . DuckDBTimestamp

arrayDuckValue ::
    forall a.
    (DuckDBColumnType a, ToField a) =>
    TypeCache ->
    Array Int a ->
    IO DuckDBValue
arrayDuckValue cache arr =
    bracket (createElementLogicalType (cachedTypeFromRep cache) (Proxy :: Proxy a)) destroyLogicalType \elementType ->
        withCreatedValues (map (\value -> fieldBindingValue (toField value) cache) (elems arr)) \values ->
            withDuckValues values \ptr ->
                checkedValue (c_duckdb_create_array_value elementType ptr (fromIntegral (length values)))

structValueDuckValue :: (LogicalTypeRep -> IO DuckDBLogicalType) -> StructValue FieldValue -> IO DuckDBValue
structValueDuckValue typeFromRep StructValue{structValueFields, structValueTypes, structValueIndex = _} = do
    let valueFields = elems structValueFields
        typeFields = elems structValueTypes
        typeNames = map structFieldName typeFields
        valueNames = map structFieldName valueFields
    when (length valueFields /= length typeFields) $
        throwIO (userError "duckdb-simple: struct value/type arity mismatch")
    when (typeNames /= valueNames) $
        throwIO (userError "duckdb-simple: struct value/type field names mismatch")
    let actions =
            zipWith
                ( \StructField{structFieldValue = typeRep} StructField{structFieldValue = fieldVal} ->
                    fieldValueWithTypeDuckValue typeFromRep typeRep fieldVal
                )
                typeFields
                valueFields
    bracket (typeFromRep (LogicalTypeStruct structValueTypes)) destroyLogicalType \structLogical ->
        withCreatedValues actions \childValues ->
            withDuckValues childValues $ \ptr ->
                checkedValue (c_duckdb_create_struct_value structLogical ptr)

unionValueDuckValue :: (LogicalTypeRep -> IO DuckDBLogicalType) -> UnionValue FieldValue -> IO DuckDBValue
unionValueDuckValue typeFromRep UnionValue{unionValueIndex, unionValueLabel, unionValuePayload, unionValueMembers} = do
    let membersList = elems unionValueMembers
        idx = fromIntegral unionValueIndex :: Int
        memberCount = length membersList
    when (idx < 0 || idx >= memberCount) $
        throwIO (userError "duckdb-simple: union value tag out of range")
    let UnionMemberType{unionMemberName, unionMemberType = memberType} = membersList !! idx
    when (unionValueLabel /= unionMemberName) $
        throwIO (userError "duckdb-simple: union tag and member name mismatch")
    bracket (typeFromRep (LogicalTypeUnion unionValueMembers)) destroyLogicalType \unionLogical ->
        bracket (checkedValue (fieldValueWithTypeDuckValue typeFromRep memberType unionValuePayload)) destroyValue \payloadValue ->
            checkedValue (c_duckdb_create_union_value unionLogical (fromIntegral unionValueIndex) payloadValue)

fieldValueWithTypeDuckValue :: (LogicalTypeRep -> IO DuckDBLogicalType) -> LogicalTypeRep -> FieldValue -> IO DuckDBValue
fieldValueWithTypeDuckValue typeFromRep typeRep FieldNull =
    bracket (typeFromRep typeRep) destroyLogicalType \logical ->
        withCreatedValues [nullDuckValue] \values ->
            withDuckValues values \ptr ->
                bracket (checkedValue (c_duckdb_create_list_value logical ptr 1)) destroyValue \list ->
                    checkedValue (c_duckdb_get_list_child list 0)
fieldValueWithTypeDuckValue typeFromRep rep value =
    case rep of
        LogicalTypeScalar DuckDBTypeVariant -> variantDuckValue typeFromRep value
        LogicalTypeScalar dtype -> scalarFieldValueDuckValue dtype value
        LogicalTypeGeometry _ ->
            case value of
                FieldGeometry{} -> unsupportedRawGeometryBinding
                other -> typeMismatch "GEOMETRY" other
        LogicalTypeDecimal width scale ->
            case value of
                FieldDecimal decVal@DecimalValue{decimalWidth, decimalScale}
                    | decimalWidth == width && decimalScale == scale -> decimalDuckValue decVal
                    | otherwise -> throwIO (userError "duckdb-simple: decimal value metadata mismatch")
                other -> typeMismatch "DECIMAL" other
        LogicalTypeList elemRep ->
            case value of
                FieldList elemsList ->
                    bracket (typeFromRep elemRep) destroyLogicalType \childLogical ->
                        withCreatedValues (map (fieldValueWithTypeDuckValue typeFromRep elemRep) elemsList) \values ->
                            withDuckValues values \ptr ->
                                checkedValue (c_duckdb_create_list_value childLogical ptr (fromIntegral (length values)))
                other -> typeMismatch "LIST" other
        LogicalTypeArray elemRep size ->
            case value of
                FieldArray arr -> do
                    let elemsList = elems arr
                        actualCount = length elemsList
                    when (fromIntegral actualCount /= size) $
                        throwIO (userError "duckdb-simple: array length mismatch")
                    bracket (typeFromRep elemRep) destroyLogicalType \childLogical ->
                        withCreatedValues (map (fieldValueWithTypeDuckValue typeFromRep elemRep) elemsList) \values ->
                            withDuckValues values \ptr ->
                                checkedValue (c_duckdb_create_array_value childLogical ptr (fromIntegral actualCount))
                other -> typeMismatch "ARRAY" other
        LogicalTypeMap keyRep valueRep ->
            case value of
                FieldMap pairs ->
                    bracket (typeFromRep (LogicalTypeMap keyRep valueRep)) destroyLogicalType \mapLogical ->
                        withCreatedValues (map (fieldValueWithTypeDuckValue typeFromRep keyRep . fst) pairs) \keyValues ->
                            withCreatedValues (map (fieldValueWithTypeDuckValue typeFromRep valueRep . snd) pairs) \valValues ->
                                withDuckValues keyValues \keyPtr ->
                                    withDuckValues valValues \valPtr ->
                                        checkedValue (c_duckdb_create_map_value mapLogical keyPtr valPtr (fromIntegral (length pairs)))
                other -> typeMismatch "MAP" other
        LogicalTypeStruct structRep ->
            case value of
                FieldStruct structVal
                    | structValueTypeRep structVal == LogicalTypeStruct structRep -> structValueDuckValue typeFromRep structVal
                    | otherwise -> throwIO (userError "duckdb-simple: struct value type mismatch")
                other -> typeMismatch "STRUCT" other
        LogicalTypeUnion unionRep ->
            case value of
                FieldUnion unionVal
                    | unionValueTypeRep unionVal == LogicalTypeUnion unionRep -> unionValueDuckValue typeFromRep unionVal
                    | otherwise -> throwIO (userError "duckdb-simple: union value type mismatch")
                other -> typeMismatch "UNION" other
        LogicalTypeEnum dict ->
            case value of
                FieldEnum enumIdx -> enumDuckValue dict enumIdx
                other -> typeMismatch "ENUM" other

scalarFieldValueDuckValue :: DuckDBType -> FieldValue -> IO DuckDBValue
scalarFieldValueDuckValue dtype value =
    case (dtype, value) of
        (DuckDBTypeBoolean, FieldBool b) -> boolDuckValue b
        (DuckDBTypeTinyInt, FieldInt8 i) -> int8DuckValue i
        (DuckDBTypeSmallInt, FieldInt16 i) -> int16DuckValue i
        (DuckDBTypeInteger, FieldInt32 i) -> int32DuckValue i
        (DuckDBTypeBigInt, FieldInt64 i) -> int64DuckValue i
        (DuckDBTypeUTinyInt, FieldWord8 w) -> uint8DuckValue w
        (DuckDBTypeUSmallInt, FieldWord16 w) -> uint16DuckValue w
        (DuckDBTypeUInteger, FieldWord32 w) -> uint32DuckValue w
        (DuckDBTypeUBigInt, FieldWord64 w) -> uint64DuckValue w
        (DuckDBTypeFloat, FieldFloat f) -> floatDuckValue f
        (DuckDBTypeDouble, FieldDouble d) -> doubleDuckValue d
        (DuckDBTypeVarchar, FieldText t) -> textDuckValue t
        (DuckDBTypeBlob, FieldBlob b) -> blobDuckValue b
        (DuckDBTypeGeometry, FieldGeometry{}) -> unsupportedRawGeometryBinding
        (DuckDBTypeUUID, FieldUUID u) -> uuidDuckValue u
        (DuckDBTypeBit, FieldBit bits) -> bitDuckValue bits
        (DuckDBTypeDate, FieldDate d) -> dateDuckValue d
        (DuckDBTypeTime, FieldTime t) -> timeOfDayDuckValue t
        (DuckDBTypeTimeNs, FieldTime t) ->
            timeOfDayUnits 1000000000 t >>= c_duckdb_create_time_ns . DuckDBTimeNs . fromInteger
        (DuckDBTypeTimeTz, FieldTimeTZ tz) -> timeWithZoneDuckValue tz
        (DuckDBTypeTimestamp, FieldTimestamp ts) -> localTimestampDuckValue ts
        (DuckDBTypeTimestampS, FieldTimestamp ts) ->
            encodeUnbounded (encodeTimestampUnits 1) ts >>= c_duckdb_create_timestamp_s . DuckDBTimestampS
        (DuckDBTypeTimestampMs, FieldTimestamp ts) ->
            encodeUnbounded (encodeTimestampUnits 1000) ts >>= c_duckdb_create_timestamp_ms . DuckDBTimestampMs
        (DuckDBTypeTimestampNs, FieldTimestamp ts) ->
            encodeUnbounded (encodeTimestampUnits 1000000000) ts >>= c_duckdb_create_timestamp_ns . DuckDBTimestampNs
        (DuckDBTypeTimestampTz, FieldTimestampTZ ts) -> utcTimestampDuckValue ts
        (DuckDBTypeInterval, FieldInterval iv) -> intervalDuckValue iv
        (DuckDBTypeHugeInt, FieldHugeInt i) -> hugeIntDuckValue i
        (DuckDBTypeUHugeInt, FieldUHugeInt i) -> uhugeIntDuckValue i
        (DuckDBTypeBigNum, FieldBigNum big) -> bigNumDuckValue big
        (DuckDBTypeSQLNull, _) -> nullDuckValue
        _ ->
            case value of
                FieldNull -> nullDuckValue
                other ->
                    throwIO
                        ( userError
                            ( "duckdb-simple: unsupported scalar conversion for "
                                <> show dtype
                                <> " from "
                                <> show other
                            )
                        )

enumDuckValue :: Array Int Text -> Word32 -> IO DuckDBValue
enumDuckValue dict idx = do
    when (toInteger idx >= toInteger (length (elems dict))) $
        throwIO (userError "duckdb-simple: ENUM index out of range")
    bracket (logicalTypeFromRep (LogicalTypeEnum dict)) destroyLogicalType \enumLogical ->
        checkedValue (c_duckdb_create_enum_value enumLogical (fromIntegral idx))

hugeIntDuckValue :: Integer -> IO DuckDBValue
hugeIntDuckValue value =
    integerToHugeInt value >>= \huge ->
        alloca \ptr -> do
            poke ptr huge
            c_duckdb_create_hugeint ptr

uhugeIntDuckValue :: Integer -> IO DuckDBValue
uhugeIntDuckValue value =
    integerToUHugeInt value >>= \uhu ->
        alloca \ptr -> do
            poke ptr uhu
            c_duckdb_create_uhugeint ptr

decimalDuckValue :: DecimalValue -> IO DuckDBValue
decimalDuckValue DecimalValue{decimalWidth, decimalScale, decimalInteger} = do
    when (decimalWidth < 1 || decimalWidth > 38 || decimalScale > decimalWidth) $
        throwIO (userError "duckdb-simple: invalid DECIMAL width or scale")
    when (abs decimalInteger >= 10 ^ decimalWidth) $
        throwIO (userError "duckdb-simple: DECIMAL value exceeds declared precision")
    huge <- integerToHugeInt decimalInteger
    alloca \ptr -> do
        poke
            ptr
            DuckDBDecimal
                { duckDBDecimalWidth = decimalWidth
                , duckDBDecimalScale = decimalScale
                , duckDBDecimalValue = huge
                }
        c_duckdb_create_decimal ptr

intervalDuckValue :: IntervalValue -> IO DuckDBValue
intervalDuckValue IntervalValue{intervalMonths, intervalDays, intervalMicros} =
    alloca \ptr -> do
        poke ptr (DuckDBInterval intervalMonths intervalDays intervalMicros)
        c_duckdb_create_interval ptr

timeWithZoneDuckValue :: TimeWithZone -> IO DuckDBValue
timeWithZoneDuckValue TimeWithZone{timeWithZoneTime, timeWithZoneZone} = do
    totalMicros <- timeOfDayUnits 1000000 timeWithZoneTime
    let offsetSeconds = toInteger (timeZoneMinutes timeWithZoneZone) * 60
    when (abs offsetSeconds > 57599) $
        throwIO (userError "duckdb-simple: TIME WITH TIME ZONE offset out of range")
    tzValue <- c_duckdb_create_time_tz (fromIntegral totalMicros) (fromIntegral offsetSeconds)
    c_duckdb_create_time_tz_value tzValue

integerToHugeInt :: Integer -> IO DuckDBHugeInt
integerToHugeInt value = do
    let minVal = negate (1 `shiftL` 127)
        maxVal = (1 `shiftL` 127) - 1
    when (value < minVal || value > maxVal) $
        throwIO (userError "duckdb-simple: HUGEINT value out of range")
    let lowerMask = (1 `shiftL` 64) - 1
        lower = fromIntegral (value .&. lowerMask)
        upper = fromIntegral (value `shiftR` 64)
    pure DuckDBHugeInt{duckDBHugeIntLower = lower, duckDBHugeIntUpper = upper}

integerToUHugeInt :: Integer -> IO DuckDBUHugeInt
integerToUHugeInt value = do
    let minVal = 0
        maxVal = (1 `shiftL` 128) - 1
    when (value < minVal || value > maxVal) $
        throwIO (userError "duckdb-simple: UHUGEINT value out of range")
    let lowerMask = (1 `shiftL` 64) - 1
        lower = fromIntegral (value .&. lowerMask)
        upper = fromIntegral (value `shiftR` 64)
    pure DuckDBUHugeInt{duckDBUHugeIntLower = lower, duckDBUHugeIntUpper = upper}

-- | Release every child handle when construction fails or completes.
withCreatedValues :: [IO DuckDBValue] -> ([DuckDBValue] -> IO a) -> IO a
withCreatedValues = withMany (\action -> bracket (checkedValue action) destroyValue)

-- | Reject failed native constructors before a handle is used.
checkedValue :: IO DuckDBValue -> IO DuckDBValue
checkedValue action = do
    value <- action
    when (value == nullPtr) $
        throwIO (userError "duckdb-simple: DuckDB value construction failed")
    pure value

withDuckValues :: [DuckDBValue] -> (Ptr DuckDBValue -> IO a) -> IO a
withDuckValues xs action = withArray xs action

typeMismatch :: String -> FieldValue -> IO a
typeMismatch expected actual =
    throwIO
        ( userError
            ( "duckdb-simple: cannot encode "
                <> show actual
                <> " as "
                <> expected
            )
        )

createElementLogicalType :: forall a. (DuckDBColumnType a) => (LogicalTypeRep -> IO DuckDBLogicalType) -> Proxy a -> IO DuckDBLogicalType
createElementLogicalType typeFromRep proxy =
    let typeName = duckdbColumnType proxy
     in case duckDBTypeFromName typeName of
            Just dtype -> typeFromRep (LogicalTypeScalar dtype)
            Nothing ->
                throwIO
                    ( SQLError
                        { sqlErrorMessage =
                            Text.concat
                                [ "duckdb-simple: unsupported array element type "
                                , typeName
                                ]
                        , sqlErrorType = Nothing
                        , sqlErrorQuery = Nothing
                        }
                    )

duckDBTypeFromName :: Text -> Maybe DuckDBType
duckDBTypeFromName name =
    case name of
        "BOOLEAN" -> Just DuckDBTypeBoolean
        "TINYINT" -> Just DuckDBTypeTinyInt
        "SMALLINT" -> Just DuckDBTypeSmallInt
        "INTEGER" -> Just DuckDBTypeInteger
        "BIGINT" -> Just DuckDBTypeBigInt
        "UTINYINT" -> Just DuckDBTypeUTinyInt
        "USMALLINT" -> Just DuckDBTypeUSmallInt
        "UINTEGER" -> Just DuckDBTypeUInteger
        "UBIGINT" -> Just DuckDBTypeUBigInt
        "FLOAT" -> Just DuckDBTypeFloat
        "DOUBLE" -> Just DuckDBTypeDouble
        "DATE" -> Just DuckDBTypeDate
        "TIME" -> Just DuckDBTypeTime
        "TIMESTAMP" -> Just DuckDBTypeTimestamp
        "TIMESTAMPTZ" -> Just DuckDBTypeTimestampTz
        "TEXT" -> Just DuckDBTypeVarchar
        "BLOB" -> Just DuckDBTypeBlob
        "GEOMETRY" -> Just DuckDBTypeGeometry
        "VARIANT" -> Just DuckDBTypeVariant
        "UUID" -> Just DuckDBTypeUUID
        "BIT" -> Just DuckDBTypeBit
        "BIGNUM" -> Just DuckDBTypeBigNum
        -- treat NULL as SQLNULL to provide element type for Maybe values without data
        "NULL" -> Just DuckDBTypeSQLNull
        _ -> Nothing

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType logical =
    alloca $ \ptr -> do
        poke ptr logical
        c_duckdb_destroy_logical_type ptr

-- | Reject raw values that the C API cannot bind without format conversion.
unsupportedRawGeometryBinding :: IO a
unsupportedRawGeometryBinding =
    throwIO (userError "duckdb-simple: raw GEOMETRY binding requires explicit ST_GeomFromWKB and ST_SetCRS parameters")

{- | Construct an owned VARIANT value. A scalar keeps its native type. Lists,
arrays, and STRUCT fields contain VARIANT values. The C API casts the payload
through a one-element VARIANT list.
-}
variantDuckValue :: (LogicalTypeRep -> IO DuckDBLogicalType) -> FieldValue -> IO DuckDBValue
variantDuckValue typeFromRep value = do
    (rep, payload) <- variantPayloadType value
    bracket (typeFromRep (LogicalTypeScalar DuckDBTypeVariant)) destroyLogicalType \variantType ->
        withCreatedValues [fieldValueWithTypeDuckValue typeFromRep rep payload] \values ->
            withDuckValues values \ptr ->
                bracket (checkedValue (c_duckdb_create_list_value variantType ptr 1)) destroyValue \list ->
                    checkedValue (c_duckdb_get_list_child list 0)

{- | Choose the native type of a VARIANT payload. Containers get VARIANT
elements and fields. Time values with sub-microsecond digits use nanosecond
types.
-}
variantPayloadType :: FieldValue -> IO (LogicalTypeRep, FieldValue)
variantPayloadType value = case value of
    FieldNull -> pure (variant, value)
    FieldBool{} -> scalar DuckDBTypeBoolean
    FieldInt8{} -> scalar DuckDBTypeTinyInt
    FieldInt16{} -> scalar DuckDBTypeSmallInt
    FieldInt32{} -> scalar DuckDBTypeInteger
    FieldInt64{} -> scalar DuckDBTypeBigInt
    FieldWord8{} -> scalar DuckDBTypeUTinyInt
    FieldWord16{} -> scalar DuckDBTypeUSmallInt
    FieldWord32{} -> scalar DuckDBTypeUInteger
    FieldWord64{} -> scalar DuckDBTypeUBigInt
    FieldHugeInt{} -> scalar DuckDBTypeHugeInt
    FieldUHugeInt{} -> scalar DuckDBTypeUHugeInt
    FieldFloat{} -> scalar DuckDBTypeFloat
    FieldDouble{} -> scalar DuckDBTypeDouble
    FieldDecimal DecimalValue{decimalWidth, decimalScale} -> pure (LogicalTypeDecimal decimalWidth decimalScale, value)
    FieldText{} -> scalar DuckDBTypeVarchar
    FieldBlob{} -> scalar DuckDBTypeBlob
    FieldUUID{} -> scalar DuckDBTypeUUID
    FieldDate{} -> scalar DuckDBTypeDate
    FieldTime time
        | hasNanos time -> scalar DuckDBTypeTimeNs
        | otherwise -> scalar DuckDBTypeTime
    FieldTimestamp (Finite LocalTime{localTimeOfDay})
        | hasNanos localTimeOfDay -> scalar DuckDBTypeTimestampNs
    FieldTimestamp{} -> scalar DuckDBTypeTimestamp
    FieldTimestampTZ{} -> scalar DuckDBTypeTimestampTz
    FieldTimeTZ{} -> scalar DuckDBTypeTimeTz
    FieldInterval{} -> scalar DuckDBTypeInterval
    FieldBigNum{} -> scalar DuckDBTypeBigNum
    FieldBit{} -> scalar DuckDBTypeBit
    FieldList{} -> pure (LogicalTypeList variant, value)
    FieldArray items -> pure (LogicalTypeList variant, FieldList (elems items))
    FieldStruct structValue@StructValue{structValueTypes} -> do
        let names = map structFieldName (elems structValueTypes)
        when (any Text.null names || length names /= length (List.nub names)) $
            throwIO (userError "duckdb-simple: VARIANT objects need unique, nonempty keys")
        let types = fmap (\field -> field{structFieldValue = variant}) structValueTypes
        pure (LogicalTypeStruct types, FieldStruct structValue{structValueTypes = types})
    FieldUnion unionValue -> pure (unionValueTypeRep unionValue, value)
    FieldGeometry{} -> unsupportedRawGeometryBinding
    FieldMap{} -> throwIO (userError "duckdb-simple: VARIANT payloads cannot contain MAP values")
    FieldEnum{} -> throwIO (userError "duckdb-simple: VARIANT payloads cannot contain ENUM values")
  where
    variant = LogicalTypeScalar DuckDBTypeVariant
    scalar dtype = pure (LogicalTypeScalar dtype, value)
    hasNanos time = snd (properFraction (todSec time * 1000000) :: (Integer, Pico)) /= 0

instance ToDuckValue G.Geometry where
    toDuckValue geometry = do
        wkt <- either (throwIO . userError) pure (WKT.encodeWKT geometry)
        bracket (logicalTypeFromRep (LogicalTypeGeometry Nothing)) destroyLogicalType \logical ->
            withCreatedValues [textDuckValue wkt] \values ->
                withDuckValues values \ptr ->
                    bracket (checkedValue (c_duckdb_create_list_value logical ptr 1)) destroyValue \list ->
                        checkedValue (c_duckdb_get_list_child list 0)

instance ToDuckValue Null where
    toDuckValue _ = nullDuckValue

instance ToDuckValue Bool where
    toDuckValue = boolDuckValue

instance ToDuckValue Int where
    toDuckValue = int64DuckValue . fromIntegral

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
    toDuckValue = bigNumDuckValue . BigNum

instance ToDuckValue Natural where
    toDuckValue = bigNumDuckValue . BigNum . toInteger

instance ToDuckValue Word where
    toDuckValue = uint64DuckValue . fromIntegral

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
    toDuckValue = structValueDuckValue logicalTypeFromRep

instance ToDuckValue (UnionValue FieldValue) where
    toDuckValue = unionValueDuckValue logicalTypeFromRep

instance (ToDuckValue a) => ToDuckValue (Maybe a) where
    toDuckValue Nothing = nullDuckValue
    toDuckValue (Just value) = toDuckValue value

-- | Preserve infinity sentinels and validate finite values before narrowing.
encodeUnbounded :: (Integral b, Bounded b) => (a -> IO b) -> Unbounded a -> IO b
encodeUnbounded _ NegInfinity = pure (negate maxBound)
encodeUnbounded _ PosInfinity = pure maxBound
encodeUnbounded encode (Finite value) = encode value

-- | Encode finite dates as days from the Unix epoch.
encodeDay :: Day -> IO DuckDBDate
encodeDay day = do
    let days = diffDays day (fromGregorian 1970 1 1)
    checkFiniteRange "DATE" (minBound :: Int32) maxBound days
    pure (DuckDBDate (fromInteger days))

-- | Encode a time of day at DuckDB microsecond precision.
encodeTimeOfDay :: TimeOfDay -> IO DuckDBTime
encodeTimeOfDay tod = DuckDBTime . fromInteger <$> timeOfDayUnits 1000000 tod

-- | Encode finite timestamps without native calendar conversions.
encodeLocalTime :: LocalTime -> IO DuckDBTimestamp
encodeLocalTime ts = DuckDBTimestamp <$> encodeTimestampUnits 1000000 ts

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
    when (value < toInteger lower || value >= toInteger upper || value == negate (toInteger upper)) $
        throwIO (userError ("duckdb-simple: " <> label <> " value out of finite range"))

-- | Validate time components and convert to the requested units per second.
timeOfDayUnits :: Integer -> TimeOfDay -> IO Integer
timeOfDayUnits units tod@(TimeOfDay hours minutes seconds) = do
    when (hours < 0 || hours > 24 || minutes < 0 || minutes > 59 || seconds < 0 || seconds >= 61 || (hours == 24 && (minutes /= 0 || seconds /= 0))) $
        throwIO (userError "duckdb-simple: invalid time of day")
    let value = diffTimeToPicoseconds (timeOfDayToTime tod) `div` (1000000000000 `div` units)
    when (value > 86400 * units) $
        throwIO (userError "duckdb-simple: time of day out of range")
    pure value

bindDuckValue :: Statement -> DuckDBIdx -> IO DuckDBValue -> IO ()
bindDuckValue stmt idx makeValue =
    withStatementHandle stmt \handle ->
        bracket (checkedValue makeValue) destroyValue \value -> do
            rc <- c_duckdb_bind_value handle idx value
            when (rc /= DuckDBSuccess) $ do
                err <- fetchPrepareError handle
                throwBindError stmt err

destroyValue :: DuckDBValue -> IO ()
destroyValue value =
    alloca \ptr -> do
        poke ptr value
        c_duckdb_destroy_value ptr

fetchPrepareError :: DuckDBPreparedStatement -> IO Text
fetchPrepareError handle = do
    msgPtr <- c_duckdb_prepare_error handle
    if msgPtr == nullPtr
        then pure (Text.pack "duckdb-simple: parameter binding failed")
        else Text.pack <$> peekCString msgPtr

throwBindError :: Statement -> Text -> IO a
throwBindError Statement{statementQuery} msg =
    throwIO
        SQLError
            { sqlErrorMessage = msg
            , sqlErrorType = Nothing
            , sqlErrorQuery = Just statementQuery
            }
