{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Database.DuckDB.Simple.FromField
-- Description : Conversion from DuckDB column values to Haskell types.
module Database.DuckDB.Simple.FromField (
  Field (..),
  FieldValue (..),
  RawGeometry (..),
  StructField (..),
  StructValue (..),
  TupleValue (..),
  UnionMemberType (..),
  UnionValue (..),
  LogicalTypeRep (..),
  LogicalTypeId (..),
  BitString (..),
  bsFromBool,
  BigNum (..),
  fromBigNumBytes,
  toBigNumBytes,
  DecimalValue (..),
  IntervalValue (..),
  TimeWithZone (..),
  timeWithZone,
  timeWithZoneZone,
  SizedArray,
  sizedArray,
  sizedArrayFromList,
  sizedArrayToArray,
  ResultError (..),
  FieldParser,
  FromField (..),
  returnError,
  fieldTypeName,
) where

import Control.Exception (Exception (..))
import Data.Array (Array, assocs, bounds, elems, listArray, rangeSize)
import Data.Bits (Bits (..), finiteBitSize)
import qualified Data.ByteString as BS
import Data.Data (Typeable, typeRep)
import qualified Data.Geometry as G
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (clamp)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day, addDays, diffDays, fromGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds)
import Data.Time.LocalTime (
  LocalTime (..),
  TimeOfDay (..),
  TimeZone,
  minutesToTimeZone,
  timeOfDayToTime,
  timeZoneMinutes,
 )
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Conversion (Conversion, conversionErrorWith)
import Database.DuckDB.Simple.Geometry (RawGeometry (..), fromRawGeometry)
import Database.DuckDB.Simple.LogicalRep (
  LogicalTypeId (..),
  LogicalTypeRep (..),
  StructField (..),
  StructValue (..),
  TupleValue (..),
  UnionMemberType (..),
  UnionValue (..),
  aliasBase,
  listElementType,
  logicalTypeSQL,
  mapEntryTypes,
 )
import Database.DuckDB.Simple.Native (arraySizeReason)
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import GHC.Float (double2Float, float2Double, int2Float, rationalToDouble, rationalToFloat)
import GHC.Num.Integer (integerFromWordList)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Numeric.Natural (Natural)

-- | Internal representation of a column value.
-- 'FieldType' contains the logical type carried by a SQL @TYPE@ value.
-- 'FieldGeometry' contains raw WKB and CRS metadata. A STRUCT, TUPLE, or UNION
-- parameter binds it as a GEOMETRY with the same CRS. A VARIANT parameter
-- binds it only when it has no CRS. A top-level t'RawGeometry' parameter binds
-- as a GEOMETRY with its own CRS. See Note [Raw GEOMETRY parameters] in
-- "Database.DuckDB.Simple.ToField".
data FieldValue
  = FieldNull
  | FieldInt8 Int8
  | FieldInt16 Int16
  | FieldInt32 Int32
  | FieldInt64 Int64
  | FieldWord8 Word8
  | FieldWord16 Word16
  | FieldWord32 Word32
  | FieldWord64 Word64
  | FieldUUID UUID.UUID
  | FieldFloat Float
  | FieldDouble Double
  | FieldText Text
  | FieldBool Bool
  | FieldBlob BS.ByteString
  | FieldGeometry RawGeometry
  | FieldDate Date
  | FieldTime TimeOfDay
  | FieldTimestamp LocalTimestamp
  | FieldInterval IntervalValue
  | FieldHugeInt Integer
  | FieldUHugeInt Integer
  | FieldDecimal DecimalValue
  | FieldTimestampTZ UTCTimestamp
  | FieldTimeTZ TimeWithZone
  | FieldBit BitString
  | FieldBigNum BigNum
  | FieldType LogicalTypeRep
  | FieldEnum Word32 Text
  | FieldArray (Array Int FieldValue)
  | FieldList [FieldValue]
  | FieldMap [(FieldValue, FieldValue)]
  | FieldStruct (StructValue FieldValue)
  | FieldTuple (TupleValue FieldValue)
  | FieldUnion (UnionValue FieldValue)
  deriving (Eq, Show)

-- | Exact-width decimal payload plus its declared width and scale. A parameter
-- of this type binds as a DECIMAL with this width and scale. A @SizedArray@, a
-- list, or a typed row cannot hold it, because it has no @DuckDBColumnType@
-- instance: one column type cannot give the width and scale of each value.
data DecimalValue = DecimalValue
  { decimalWidth :: Word8
  , decimalScale :: Word8
  , decimalInteger :: Integer
  }
  deriving (Eq, Show)

-- | DuckDB interval payload split into months, days, and microseconds. A
-- parameter of this type binds as an INTERVAL, and DuckDB keeps the three
-- fields.
data IntervalValue = IntervalValue
  { intervalMonths :: Int32
  , intervalDays :: Int32
  , intervalMicros :: Int64
  }
  deriving (Eq, Show)

-- | Arbitrary-precision integer wrapper used for DuckDB's BIGNUM type.
newtype BigNum = BigNum Integer
  deriving stock (Eq, Show)
  deriving (Num) via Integer

-- | Decode a DuckDB BIGNUM blob as a Haskell @Integer@. The blob has a 3-byte
-- header and a big-endian payload. Negative values have a complemented payload.
-- Restore that payload, then split it into machine-word limbs for
-- @integerFromWordList@, with the most significant limb first.
-- This is the BIGNUM storage of DuckDB. The V2 API does not commit to this
-- layout, and this library does not use this function.
fromBigNumBytes :: [Word8] -> Integer
fromBigNumBytes bytes =
  let header = take 3 bytes
      payloadRaw = drop 3 bytes
      isNeg = case header of
        (h : _) -> h .&. 0x80 == 0
        [] -> False
      payload = if isNeg then map complement payloadRaw else payloadRaw
      bytesPerWord = finiteBitSize (0 :: Word) `div` 8 -- 8 on 64-bit, 4 on 32-bit
      len = length payload
      (firstChunk, rest) = splitAt (len `mod` bytesPerWord) payload

      chunkWords [] = []
      chunkWords xs =
        let (chunk, remainder) = splitAt bytesPerWord xs
        in  chunk : chunkWords remainder

      toWord = foldl (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0
      limbs = (if null firstChunk then id else (firstChunk :)) (chunkWords rest)
  in  integerFromWordList isNeg (map toWord limbs)

-- | Encode an @Integer@ as a DuckDB BIGNUM blob. Write the 3-byte header
-- (sign bit and payload length), then the magnitude bytes in big-endian order.
-- Complement the header and the payload for a negative value.
-- This is the BIGNUM storage of DuckDB. The V2 API does not commit to
-- this layout, and this library does not use this function.
toBigNumBytes :: Integer -> [Word8]
toBigNumBytes value =
  let isNeg = value < 0
      magnitude = if isNeg then negate value else value
      payloadBE
        | magnitude == 0 = [0]
        | otherwise = go magnitude []
        where
          go 0 acc = acc
          go n acc =
            let (q, r) = quotRem n 256
            in  go q (fromIntegral r : acc)

      headerBase :: Word32
      headerBase = (fromIntegral (length payloadBE) .|. 0x00800000)
      headerVal :: Word32
      headerVal = if isNeg then complement headerBase else headerBase
      headerMasked = headerVal .&. 0x00FFFFFF
      headerBytes =
        [ fromIntegral ((headerMasked `shiftR` 16) .&. 0xFF)
        , fromIntegral ((headerMasked `shiftR` 8) .&. 0xFF)
        , fromIntegral (headerMasked .&. 0xFF)
        ]
      payloadBytes = if isNeg then map complement payloadBE else payloadBE
  in  headerBytes <> payloadBytes

-- | DuckDB BIT value represented as raw bytes plus left-padding bit count.
data BitString = BitString
  { padding :: Word8
  , bits :: BS.ByteString
  }
  deriving stock (Eq)

instance Show BitString where
  show (BitString padding bits) =
    drop (fromIntegral padding) $ concatMap word8ToString $ BS.unpack bits
    where
      word8ToString :: Word8 -> String
      word8ToString w = map (\n -> if testBit w n then '1' else '0') [7, 6 .. 0]

-- | Construct a @BitString@ from a list of @Bool@ values. The first element is
-- the first bit, and 'True' is a 1 bit. The function puts 0 bits in front of the
-- first element to fill the first byte, and stores their count as the padding.
bsFromBool :: [Bool] -> BitString
bsFromBool bits =
  let totalBits = length bits
      padding = (8 - (totalBits `mod` 8)) `mod` 8
      paddedBits = replicate padding False ++ bits
      byteChunks = chunk 8 paddedBits
      byteValues = map bitsToWord8 byteChunks
  in  BitString (fromIntegral padding) (BS.pack byteValues)
  where
    chunk _ [] = []
    chunk n xs = take n xs : chunk n (drop n xs)

    bitsToWord8 :: [Bool] -> Word8
    bitsToWord8 bs = foldl (\acc (b, i) -> if b then setBit acc i else acc) 0 (zip bs [7, 6 .. 0])

-- | Time of day with its offset from UTC in seconds, as a TIMETZ stores it. A
-- positive offset is east of UTC: @12:00:00+02@ has the offset 7200. DuckDB
-- stores an offset from -57599 to 57599 seconds (-15:59:59 to +15:59:59) and the
-- time in microseconds. A value with such an offset reads back equal to the
-- bound value. A time with digits below one microsecond loses these digits, as
-- in SQL text. 'timeWithZone' makes a value from a "Data.Time" zone, and
-- 'timeWithZoneZone' gives the zone of a value.
data TimeWithZone = TimeWithZone
  { timeWithZoneTime :: TimeOfDay
  , timeWithZoneOffsetSeconds :: Int32
  }
  deriving (Eq, Show)

-- | Make a t'TimeWithZone' from a time of day and a "Data.Time" zone. The offset
-- is the minutes of the zone times 60. If the seconds exceed the 'Int32'
-- range, keep the nearest bound. Binding rejects either bound as an invalid
-- TIMETZ offset. DuckDB does not store the zone name or the summer flag.
timeWithZone :: TimeOfDay -> TimeZone -> TimeWithZone
timeWithZone time zone = TimeWithZone time (fromInteger (clamp bounds32 seconds))
  where
    seconds = toInteger (timeZoneMinutes zone) * 60
    bounds32 = (toInteger (minBound @Int32), toInteger (maxBound @Int32))

-- | Give the "Data.Time" zone of the offset of a t'TimeWithZone'. A 'TimeZone'
-- holds whole minutes, so the seconds of an offset are dropped: the offset
-- 3630 seconds of @12:00:00+01:00:30@ gives the zone @+0100@. The zone has no
-- name.
timeWithZoneZone :: TimeWithZone -> TimeZone
timeWithZoneZone value = minutesToTimeZone (fromIntegral (timeWithZoneOffsetSeconds value `quot` 60))

type role SizedArray nominal representational

-- | An 'Array' of exactly @n@ values, with the bounds @(0, n - 1)@. A
-- parameter of this type binds as the ARRAY type @T[n]@, and a typed
-- registration declares that type. See Note [Sized arrays] in
-- "Database.DuckDB.Simple.ToField". Make a value with 'sizedArray' or
-- 'sizedArrayFromList'.
newtype SizedArray (n :: Nat) a = SizedArray (Array Int a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Make a t'SizedArray' from an 'Array' with any bounds. The result is
-- 'Nothing' when the array does not have exactly @n@ values. The result has
-- the values in their order, with the bounds @(0, n - 1)@.
sizedArray :: forall n a. (KnownNat n) => Array Int a -> Maybe (SizedArray n a)
sizedArray values
  | toInteger count == natVal (Proxy @n) = Just (SizedArray (listArray (0, count - 1) (elems values)))
  | otherwise = Nothing
  where
    count = rangeSize (bounds values)

-- | Make a t'SizedArray' from a list. The result is 'Nothing' when the list
-- does not have exactly @n@ values. The count is checked before the 'Array'
-- is built, and the check reads at most @n + 1@ elements.
-- A size above @maxBound :: Int@ gives 'Nothing' without reading the list.
sizedArrayFromList :: forall n a. (KnownNat n) => [a] -> Maybe (SizedArray n a)
sizedArrayFromList values
  | size > toInteger (maxBound :: Int) = Nothing
  | hasSize size values = Just (SizedArray (listArray (0, fromInteger size - 1) values))
  | otherwise = Nothing
  where
    size = natVal (Proxy @n)
    hasSize 0 [] = True
    hasSize 0 (_ : _) = False
    hasSize _ [] = False
    hasSize remaining (_ : rest) = hasSize (remaining - 1) rest

-- | Give the 'Array' of a t'SizedArray', with the bounds @(0, n - 1)@.
sizedArrayToArray :: SizedArray n a -> Array Int a
sizedArrayToArray (SizedArray values) = values

-- | Match signed values without narrowing to the machine word size.
pattern FieldInt :: Int64 -> FieldValue
pattern FieldInt i <- (fieldValueToInt -> Just i)
  where
    FieldInt i = FieldInt64 i

fieldValueToInt :: FieldValue -> Maybe Int64
fieldValueToInt (FieldInt8 i) = Just (fromIntegral i)
fieldValueToInt (FieldInt16 i) = Just (fromIntegral i)
fieldValueToInt (FieldInt32 i) = Just (fromIntegral i)
fieldValueToInt (FieldInt64 i) = Just i
fieldValueToInt _ = Nothing

-- | Match unsigned values without narrowing to the machine word size.
pattern FieldWord :: Word64 -> FieldValue
pattern FieldWord i <- (fieldValueToWord -> Just i)
  where
    FieldWord i = FieldWord64 i

fieldValueToWord :: FieldValue -> Maybe Word64
fieldValueToWord (FieldWord8 i) = Just (fromIntegral i)
fieldValueToWord (FieldWord16 i) = Just (fromIntegral i)
fieldValueToWord (FieldWord32 i) = Just (fromIntegral i)
fieldValueToWord (FieldWord64 i) = Just i
fieldValueToWord _ = Nothing

-- | Metadata for a single column in a row. The SQL type is an explicit
-- conversion, so a field has no equality or display instance.
data Field = Field
  { fieldName :: Text
  , fieldIndex :: Int
  , fieldValue :: FieldValue
  , fieldType :: Conversion LogicalTypeRep
  -- ^ Read the SQL type, with each alias. VARIANT means that the type of the
  -- stored value decides. See Note [Column types].
  }

{- Note [Column types]
~~~~~~~~~~~~~~~~~~~~~~
fieldType reads the SQL type of a Field, with each alias (see
Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep"). Each place that
makes a Field supplies a conversion:

- A result column. The query result owns its native logical type. A query
  parser runs before this metadata scope closes. A cursor retains its scope
  until exhaustion, reset, or statement closure.
- A callback argument. The callback owns its native logical type. Its scope
  closes when the callback invocation returns.
- A LIST or ARRAY element, or a MAP key or value. The reader maps the type
  projection over the parent's conversion with listElementType or mapEntryTypes.
- A synthetic field made from a FieldValue alone. Its conversion returns VARIANT.

VARIANT means that the stored value determines its type. An element of a
VARIANT column also has type VARIANT. The fieldType action converts no type
tree or ENUM dictionary until requested. Structured value readers can also
build type trees. See Note [Type trees] in
"Database.DuckDB.Simple.Materialize".

The scope caches successful type reads. An interrupted read can retry while
the scope remains open. Closure releases native types and clears the cache.
An escaped native conversion raises SQLError with the message
"duckdb-simple: field type scope is closed", including a cached conversion.
A LogicalTypeRep snapshot already returned to the caller is pure and remains
valid after closure. Completed conversion diagnostics are also pure. The
library resolves them before closing the scope. See Note [Conversion effects]
in "Database.DuckDB.Simple.Conversion".
-}

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError
  = -- | The SQL and Haskell types are not compatible.
    Incompatible
      { errSQLType :: Text
      , errSQLField :: Text
      , errHaskellType :: Text
      , errMessage :: Text
      }
  | -- | A SQL @NULL@ was encountered when the Haskell
    --       type did not permit it.
    UnexpectedNull
      { errSQLType :: Text
      , errSQLField :: Text
      , errHaskellType :: Text
      , errMessage :: Text
      }
  | -- | The SQL value could not be parsed, or could not
    --       be represented as a valid Haskell value, or an
    --       unexpected low-level error occurred (e.g. mismatch
    --       between metadata and actual data in a row).
    ConversionFailed
      { errSQLType :: Text
      , errSQLField :: Text
      , errHaskellType :: Text
      , errMessage :: Text
      }
  deriving (Eq, Show)

-- | The text of the error names the column and the two types.
instance Exception ResultError where
  displayException = Text.unpack . renderResultError

-- | Render a conversion error. An error without a column name, for example one
-- of a field of a generic record, names "a value".
renderResultError :: ResultError -> Text
renderResultError = \case
  Incompatible{errSQLType, errSQLField, errHaskellType, errMessage} ->
    Text.concat ["duckdb-simple: ", subject errSQLField, " has type ", errSQLType, " but expected ", errHaskellType, detail errMessage]
  UnexpectedNull{errSQLField, errHaskellType, errMessage} ->
    Text.concat ["duckdb-simple: ", subject errSQLField, " is NULL but expected ", errHaskellType, detail errMessage]
  ConversionFailed{errMessage} -> errMessage
  where
    subject name = if Text.null name then Text.pack "a value" else Text.pack "column " <> name
    detail message = if Text.null message then Text.empty else Text.pack ": " <> message

-- | Parser used by @FromField@ instances and utilities such as
-- @Database.DuckDB.Simple.FromRow.fieldWith@. The supplied @Field@ contains
-- column metadata and an already-decoded @FieldValue@. Use 'pure' for success
-- or 'returnError' for a conversion failure.
type FieldParser a = Field -> Conversion a

-- | Types that can be constructed from a DuckDB column. Each instance says
-- which SQL types it reads. See Note [Reads of another SQL type].
--
-- An instance that cannot read a value fails with a 'ResultError', for example
-- 'ConversionFailed' or 'Incompatible'. The library does not raise the
-- 'ResultError'. It puts the text of the 'ResultError' into the @SQLError@
-- that the statement raises. The section \"Errors\" of the README gives the
-- type of that @SQLError@.
class FromField a where
  fromField :: FieldParser a

{- Note [Reads of another SQL type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each FromField instance reads the SQL type of its Haskell type, for example
BIGINT for Int64 and TIMESTAMP for LocalTime. An instance also reads another
SQL type only when it gives, for each value, the value of the cast of DuckDB to
the SQL type of the Haskell type, or an error. For example, Int8 reads the
BIGINT value 5 as 5, which is the value of 5::BIGINT::TINYINT, and it raises
ConversionFailed for 300, where the cast raises an error. When the Haskell type
holds more digits than its SQL type, the comparison is with the cast to the
narrowest SQL type that holds the value exactly. Thus LocalTime reads a
TIMESTAMP_NS value with its nanoseconds, as the cast to TIMESTAMP_NS keeps
them.

The instances do not read a pair whose cast needs data that the decoded value
does not have, or whose value the library cannot give exactly. A read of such a
pair raises Incompatible, as for each other pair that an instance does not
read. The error names the SQL type of the column in the text of DuckDB
(fieldTypeName), for example TIMESTAMP_NS or DECIMAL(18,3). For a column of the
type VARIANT, it names the type of the stored value in the same text
(fieldValueTypeName), and the error of a String names Text. Error diagnostics
read type metadata only when the final conversion fails. These pairs are:

- UTCTime from TIMESTAMP, TIMESTAMP_S, TIMESTAMP_MS, TIMESTAMP_NS, and DATE, and
  LocalTime from TIMESTAMPTZ and TIMESTAMPTZ_NS. These casts use the TimeZone
  setting of the session. To read the value as the other type in UTC, write
  c::TIMESTAMP AT TIME ZONE 'UTC' in SQL for a TIMESTAMP, TIMESTAMP_S,
  TIMESTAMP_MS, or DATE value, and c AT TIME ZONE 'UTC' for each other type.
  Without the cast to TIMESTAMP, DuckDB converts a DATE, TIMESTAMP_S, or
  TIMESTAMP_MS value to nanoseconds, which fails outside the years 1677 to
  2262. The cast rounds a TIMESTAMP_NS value to the microsecond.
- Text and String from FLOAT and DOUBLE. The text of the cast differs from the
  text of show, for example 0.01 and 1.0e-2. Read a Double, or write c::VARCHAR.
- ByteString from VARCHAR. The cast to BLOB reads escape sequences such as \x41.
  Read a Text, or write encode(c) for the UTF-8 bytes.
- Each integer type from ENUM. The cast to an integer type parses the label,
  for example '20' gives 20. Write c::INTEGER for that value, or
  enum_code(c)::BIGINT for the index. enum_code gives an unsigned type, which
  the signed integer types do not read.
- Variant from a value that a VARIANT cannot hold (variantShape in
  "Database.DuckDB.Simple.Variant"): a FieldArray, a FieldTuple, a FieldStruct
  that is not in the form of a VARIANT object (a field of a type other than
  VARIANT, or keys out of ascending order), a FieldEnum, a FieldMap, a
  FieldUnion, or a GEOMETRY with a CRS, at any depth. The cast to VARIANT gives
  another value. Write c::VARIANT. A VARIANT column decodes only to values of
  the VARIANT shape. A JSON type at any depth raises Incompatible, because
  DuckDB parses its text when it casts to VARIANT. Write c::VARIANT for the
  parsed value. For a type that contains TYPE, the cast only accepts SQL NULL.
  The bind side
  (variantPayloadType in "Database.DuckDB.Simple.ToField") accepts more: it
  binds a FieldArray and a FieldTuple as a VARIANT list, and the fields of each
  FieldStruct as VARIANT values.

Text and String read the label of an ENUM value, which is the text of the cast
to VARCHAR. See Note [ENUM labels] in "Database.DuckDB.Simple.Materialize".

The rule holds at each depth. A field of a generic record reads the SQL types
that the FromField instance of its type reads: a scalar field uses that
instance, and a list field reads a LIST and an ARRAY (the DuckValue instances
in "Database.DuckDB.Simple.Generic").

See Note [DECIMAL as DOUBLE and FLOAT] for the floating-point value of a DECIMAL.
-}

-- | Reads each SQL type, and does not convert the value.
instance FromField FieldValue where
  fromField Field{fieldValue} = pure fieldValue

-- | Read the logical type carried by a SQL @TYPE@ value.
instance FromField LogicalTypeRep where
  fromField f@Field{fieldValue} = case fieldValue of
    FieldType logical -> pure logical
    FieldNull -> returnError UnexpectedNull f ""
    _ -> returnError Incompatible f "expected TYPE"

-- | Reads GEOMETRY, with its CRS.
instance FromField RawGeometry where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldGeometry value -> pure value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f "expected GEOMETRY"

-- | Reads GEOMETRY. The shape has no CRS metadata. Use t'RawGeometry' to retain
-- the CRS.
instance FromField G.Geometry where
  fromField f@Field{fieldValue} = case fieldValue of
    FieldGeometry raw -> either (returnError ConversionFailed f . Text.pack) pure (fromRawGeometry raw)
    FieldNull -> returnError UnexpectedNull f ""
    _ -> returnError Incompatible f "expected GEOMETRY"

-- | Reads STRUCT.
instance FromField (StructValue FieldValue) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldStruct structVal -> pure structVal
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f "expected STRUCT"

-- | Reads UNION.
instance FromField (UnionValue FieldValue) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldUnion unionVal -> pure unionVal
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f "expected UNION"

-- | Reads UUID, and VARCHAR in the form of 36 characters with dashes. Each
-- other text fails with 'ConversionFailed'.
instance FromField UUID.UUID where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldText t ->
        case UUID.fromText t of
          Just uuid -> pure uuid
          Nothing -> returnError ConversionFailed f "invalid UUID format"
      FieldUUID uuid -> pure uuid
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TUPLE.
instance FromField (TupleValue FieldValue) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTuple tuple -> pure tuple
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f "expected TUPLE"

-- | Reads BOOLEAN, and TINYINT, SMALLINT, INTEGER, and BIGINT. An integer other
-- than 0 is True, as in the cast to BOOLEAN.
instance FromField Bool where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldBool b -> pure b
      FieldInt i -> pure (i /= 0)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TINYINT, SMALLINT, INTEGER, BIGINT, HUGEINT, and UHUGEINT. A value
-- out of range fails with 'ConversionFailed'. It does not read ENUM.
instance FromField Int8 where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i -> boundedIntegral f i
      FieldHugeInt value -> boundedFromInteger f value
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads BIGINT, TINYINT, SMALLINT, INTEGER, HUGEINT, and UHUGEINT. A value
-- out of range fails with 'ConversionFailed'. It does not read ENUM.
instance FromField Int64 where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i -> pure i
      FieldHugeInt value -> boundedFromInteger f value
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads INTEGER, TINYINT, SMALLINT, BIGINT, HUGEINT, and UHUGEINT. A value
-- out of range fails with 'ConversionFailed'. It does not read ENUM.
instance FromField Int32 where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i -> boundedIntegral f i
      FieldHugeInt value -> boundedFromInteger f value
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads SMALLINT, TINYINT, INTEGER, BIGINT, HUGEINT, and UHUGEINT. A value
-- out of range fails with 'ConversionFailed'. It does not read ENUM.
instance FromField Int16 where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i -> boundedIntegral f i
      FieldHugeInt value -> boundedFromInteger f value
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads BIGINT, TINYINT, SMALLINT, INTEGER, HUGEINT, and UHUGEINT. A value
-- out of range fails with 'ConversionFailed'. It does not read ENUM.
instance FromField Int where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i -> boundedIntegral f i
      FieldHugeInt value -> boundedFromInteger f value
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads BIGNUM, TINYINT to BIGINT, UTINYINT to UBIGINT, HUGEINT, and UHUGEINT.
instance FromField Integer where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i -> pure (fromIntegral i)
      FieldWord w -> pure (fromIntegral w)
      FieldBigNum (BigNum big) -> pure big
      FieldHugeInt value -> pure value
      FieldUHugeInt value -> pure value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads BIGNUM, TINYINT to BIGINT, UTINYINT to UBIGINT, HUGEINT, and
-- UHUGEINT. A negative value fails with 'ConversionFailed'. It does not read
-- ENUM.
instance FromField Natural where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i
        | i >= 0 -> pure (fromIntegral i)
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to Natural"
      FieldWord w -> pure (fromIntegral w)
      FieldHugeInt value
        | value >= 0 -> pure (fromIntegral value)
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to Natural"
      FieldUHugeInt value -> pure (fromIntegral value)
      FieldBigNum (BigNum big) ->
        if big >= 0
          then pure $ fromIntegral big
          else returnError ConversionFailed f "negative value cannot be converted to Natural"
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads UTINYINT to UBIGINT, TINYINT to BIGINT, HUGEINT, and UHUGEINT. A
-- negative value or a value out of range fails with 'ConversionFailed'. It does
-- not read ENUM.
instance FromField Word64 where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i
        | i >= 0 -> pure (fromIntegral i)
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldWord w -> pure w
      FieldHugeInt value
        | value >= 0 -> boundedFromInteger f value
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads UTINYINT to UBIGINT, TINYINT to BIGINT, HUGEINT, and UHUGEINT. A
-- negative value or a value out of range fails with 'ConversionFailed'. It does
-- not read ENUM.
instance FromField Word32 where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i
        | i >= 0 -> boundedIntegral f i
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldWord w -> boundedFromInteger f (toInteger w)
      FieldHugeInt value
        | value >= 0 -> boundedFromInteger f value
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads UTINYINT to UBIGINT, TINYINT to BIGINT, HUGEINT, and UHUGEINT. A
-- negative value or a value out of range fails with 'ConversionFailed'. It does
-- not read ENUM.
instance FromField Word16 where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i
        | i >= 0 -> boundedIntegral f i
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldWord w -> boundedFromInteger f (toInteger w)
      FieldHugeInt value
        | value >= 0 -> boundedFromInteger f value
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads UTINYINT to UBIGINT, TINYINT to BIGINT, HUGEINT, and UHUGEINT. A
-- negative value or a value out of range fails with 'ConversionFailed'. It does
-- not read ENUM.
instance FromField Word8 where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i
        | i >= 0 -> boundedIntegral f i
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldWord w -> boundedFromInteger f (toInteger w)
      FieldHugeInt value
        | value >= 0 -> boundedFromInteger f value
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads UTINYINT to UBIGINT, TINYINT to BIGINT, HUGEINT, and UHUGEINT. A
-- negative value or a value out of range fails with 'ConversionFailed'. It does
-- not read ENUM.
instance FromField Word where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i
        | i >= 0 -> boundedFromInteger f (fromIntegral i)
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldWord w -> boundedFromInteger f (toInteger w)
      FieldHugeInt value
        | value >= 0 -> boundedFromInteger f value
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldUHugeInt value -> boundedFromInteger f value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads DOUBLE, FLOAT, TINYINT to BIGINT, and DECIMAL. A BIGINT above 2^53
-- rounds as the cast to DOUBLE. See Note [DECIMAL as DOUBLE and FLOAT].
instance FromField Double where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldDouble d -> pure d
      FieldFloat value -> pure (float2Double value)
      FieldInt i -> pure (fromIntegral i)
      FieldDecimal value -> pure (decimalToDouble value)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads FLOAT, DOUBLE, TINYINT to BIGINT, and DECIMAL. A finite DOUBLE out of
-- the range of FLOAT fails with 'ConversionFailed'. See
-- Note [DECIMAL as DOUBLE and FLOAT].
instance FromField Float where
  fromField field@Field{fieldValue} =
    case fieldValue of
      -- DuckDB casts an integer to FLOAT with static_cast<float>, which
      -- rounds one time to the nearest value, ties to even. int2Float rounds
      -- in the same way. The library gives the nearest value independently
      -- of platform-specific native cast rounding.
      FieldInt i -> pure (int2Float (fromIntegral i))
      FieldDecimal value -> pure (decimalToFloat value)
      _ -> do
        d <- fromField field :: Conversion Double
        if not (isInfinite d || isNaN d) && isInfinite (double2Float d)
          then returnError ConversionFailed field "floating-point value out of bounds"
          else pure (double2Float d)

{- Note [DECIMAL as DOUBLE and FLOAT]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A DECIMAL that a Double or a Float reads gives the value of the cast of DuckDB
to DOUBLE or FLOAT. TryCastDecimalToFloatingPoint in
src/common/operator/cast_operators.cpp uses these paths:

1. Scale 0. DuckDB converts the stored integer directly to the target type.
   For a HUGEINT, CastBigintToFloating in src/common/types/hugeint.cpp converts
   its unsigned magnitude with Uhugeint::TryCast. That conversion folds the
   discarded bits into a sticky bit before rounding. It gives the nearest
   value, with ties to even. The conversion to FLOAT does not pass through DOUBLE. The
   library uses fromInteger for both target types.
2. An integer with magnitude at most 2^53 (2^24 for FLOAT) and scale at most 22
   (10 for FLOAT). DuckDB converts the integer to the target type and divides
   it by the power of ten in that type. Both inputs are exact, so only the
   division rounds. The library uses the same steps.
3. Each other value. DuckDB converts the decimal digits with fast_float,
   which gives the nearest value, with ties to even. The library uses
   rationalToDouble or rationalToFloat with the unscaled integer and 10^scale.
   These functions give the same rounding without reducing the fraction first.

Path 3 is slower than path 2. It applies to a Double with a scale above 0 when
the unscaled integer is above 2^53 or the scale is above 22: at scale 2 for an
absolute value above 9.0e13, and at scale 10 above 9.0e5. It applies to a Float
with a scale above 0 when the unscaled integer is above 2^24 or the scale is
above 10.
-}

-- | The value of the cast of DuckDB from a DECIMAL to DOUBLE.
-- See Note [DECIMAL as DOUBLE and FLOAT].
decimalToDouble :: DecimalValue -> Double
decimalToDouble DecimalValue{decimalScale, decimalInteger}
  | decimalScale == 0 = fromInteger decimalInteger
  | decimalScale <= 22 && abs decimalInteger <= 9007199254740992 = fromInteger decimalInteger / 10 ^ decimalScale
  | otherwise = rationalToDouble decimalInteger (10 ^ decimalScale)

-- | The value of the cast of DuckDB from a DECIMAL to FLOAT.
-- See Note [DECIMAL as DOUBLE and FLOAT].
decimalToFloat :: DecimalValue -> Float
decimalToFloat DecimalValue{decimalScale, decimalInteger}
  | decimalScale == 0 = fromInteger decimalInteger
  | decimalScale <= 10 && abs decimalInteger <= 16777216 = fromInteger decimalInteger / 10 ^ decimalScale
  | otherwise = rationalToFloat decimalInteger (10 ^ decimalScale)

-- | Reads VARCHAR, TINYINT to BIGINT as decimal digits, BOOLEAN as @true@ or
-- @false@, and the label of an ENUM value. It does not read FLOAT or DOUBLE. See
-- Note [Reads of another SQL type].
instance FromField Text where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldText t -> pure t
      FieldInt i -> pure (Text.pack (show i))
      FieldBool b -> pure (if b then Text.pack "true" else Text.pack "false")
      FieldEnum _ label -> pure label
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads the SQL types that 'Text' reads.
instance FromField String where
  fromField field = Text.unpack <$> fromField field

-- | Reads BLOB, the WKB bytes of a GEOMETRY, and the data bytes of a BIT, as the
-- casts to BLOB give them. It does not read VARCHAR. See
-- Note [Reads of another SQL type].
instance FromField BS.ByteString where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldBlob bs -> pure bs
      FieldGeometry RawGeometry{rawGeometryWKB} -> pure rawGeometryWKB
      FieldBit (BitString _ bits) -> pure bits
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads BIT.
instance FromField BitString where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldBit b -> pure b
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads BIGNUM.
instance FromField BigNum where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldBigNum big -> pure big
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads LIST and ARRAY. Each element reads as @a@.
instance {-# OVERLAPPABLE #-} (Typeable a, FromField a) => FromField [a] where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldList entries -> traverse fromField (elementFields f (zip [0 ..] entries))
      FieldArray arr -> traverse fromField (elementFields f (assocs arr))
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Give the fields of the elements of a LIST or an ARRAY field, from the index
-- and the value of each element. An element has the name @name[index]@ and the
-- element type of the field.
elementFields :: Field -> [(Int, FieldValue)] -> [Field]
elementFields f = map element
  where
    element (idx, value) =
      Field
        { fieldName = fieldName f <> Text.pack "[" <> Text.pack (show idx) <> Text.pack "]"
        , fieldIndex = fieldIndex f
        , fieldValue = value
        , fieldType = elementType
        }
    -- The elements share the parent type conversion.
    elementType = listElementType <$> fieldType f

-- | Reads ARRAY and LIST, as a list does. A size @n@ with no DuckDB ARRAY type
-- or a value with another count raises 'Incompatible'. See Note [Sized arrays] in
-- "Database.DuckDB.Simple.ToField".
instance (KnownNat n, Typeable a, FromField a) => FromField (SizedArray n a) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldList entries -> sized (zip [0 ..] entries)
      FieldArray arr -> sized (assocs arr)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""
    where
      size = natVal (Proxy @n)
      sized elements
        | Just reason <- arraySizeReason size = returnError Incompatible f reason
        | toInteger count == size = SizedArray . listArray (0, count - 1) <$> traverse fromField (elementFields f elements)
        | otherwise = returnError Incompatible f (Text.pack ("the value has " <> show count <> " elements"))
        where
          count = length elements

-- | Reads MAP. Each key reads as @k@, and each value reads as @v@.
-- Keys must remain distinct after conversion.
instance (Ord k, Typeable k, Typeable v, FromField k, FromField v) => FromField (Map k v) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldMap pairs -> do
        result <- Map.fromList <$> traverse (uncurry parsePair) (zip [0 :: Int ..] pairs)
        if Map.size result == length pairs
          then pure result
          else returnError ConversionFailed f "duplicate MAP key after conversion"
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""
    where
      parsePair idx (keyValue, valValue) = do
        key <-
          fromField
            Field
              { fieldName = fieldName f <> suffix idx ".key"
              , fieldIndex = fieldIndex f
              , fieldValue = keyValue
              , fieldType = keyType
              }
        value <-
          fromField
            Field
              { fieldName = fieldName f <> suffix idx ".value"
              , fieldIndex = fieldIndex f
              , fieldValue = valValue
              , fieldType = valueType
              }
        pure (key, value)
      entryTypes = mapEntryTypes <$> fieldType f
      keyType = fst <$> entryTypes
      valueType = snd <$> entryTypes
      suffix idx label =
        let idxText = Text.pack (show idx)
        in  Text.pack "[" <> idxText <> Text.pack "]" <> Text.pack label

-- | Reads DECIMAL.
instance FromField DecimalValue where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldDecimal dec -> pure dec
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

{- Note [Temporal casts]
~~~~~~~~~~~~~~~~~~~~~
DuckDB converts TIMESTAMP_S, TIMESTAMP_MS, and TIMESTAMP_NS to microseconds
before a cast to DATE. The S/MS conversion can overflow even when the date
fits DATE. The NS conversion rounds to the nearest microsecond, with ties away
from the epoch. Both Day readers use this intermediate. A DATE-to-timestamp
cast checks midnight in the same microsecond range. Both LocalTime readers
check this range for a DATE input. For a timestamp input, they keep the wider
range and the nanoseconds. Unbounded readers also keep infinity.

A TIMESTAMP_S or TIMESTAMP_MS cast to TIME checks the same microsecond range.
The TimeOfDay reader checks this range for a timestamp input. It reads
TIMESTAMP_NS at nanosecond precision, as a cast to TIME_NS.
-}

-- | Get the checked microsecond intermediate of a temporal cast.
-- Round to the nearest microsecond, with ties away from the epoch. See
-- Note [Temporal casts].
localTimeMicros :: LocalTime -> Maybe Integer
localTimeMicros LocalTime{localDay, localTimeOfDay} =
  let epoch = fromGregorian 1970 1 1
      picoseconds =
        diffDays localDay epoch * 86400000000000000
          + diffTimeToPicoseconds (timeOfDayToTime localTimeOfDay)
      micros
        | picoseconds < 0 = negate ((negate picoseconds + 500000) `div` 1000000)
        | otherwise = (picoseconds + 500000) `div` 1000000
  in  if micros < toInteger (minBound :: Int64) || micros > toInteger (maxBound :: Int64)
        then Nothing
        else Just micros

-- | Convert a timestamp as the native cast to DATE does. See
-- Note [Temporal casts].
timestampDate :: LocalTimestamp -> Maybe Date
timestampDate (Finite timestamp) = do
  micros <- localTimeMicros timestamp
  pure (Finite (addDays (micros `div` 86400000000) (fromGregorian 1970 1 1)))
timestampDate NegInfinity = Just NegInfinity
timestampDate PosInfinity = Just PosInfinity

-- | Convert a date to midnight in the TIMESTAMP range. See
-- Note [Temporal casts].
dateTimestamp :: Date -> Maybe LocalTimestamp
dateTimestamp (Finite day) =
  let timestamp = LocalTime day (TimeOfDay 0 0 0)
  in  Finite timestamp <$ localTimeMicros timestamp
dateTimestamp NegInfinity = Just NegInfinity
dateTimestamp PosInfinity = Just PosInfinity

-- | Reads DATE, and the date of TIMESTAMP, TIMESTAMP_S, TIMESTAMP_MS, and
-- TIMESTAMP_NS. Infinity and a timestamp outside the range of a DATE cast fail
-- with 'ConversionFailed'. See Note [Temporal casts].
instance FromField Day where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldDate day -> finiteField f day
      FieldTimestamp timestamp -> case timestampDate timestamp of
        Just day -> finiteField f day
        Nothing -> returnError ConversionFailed f "timestamp is outside the range of a DATE cast"
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads the SQL types that 'Day' reads, also infinity. See
-- Note [Temporal casts].
instance FromField (Unbounded Day) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldDate day -> pure day
      FieldTimestamp timestamp -> case timestampDate timestamp of
        Just day -> pure day
        Nothing -> returnError ConversionFailed f "timestamp is outside the range of a DATE cast"
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TIME, TIME_NS, and the time of TIMESTAMP, TIMESTAMP_S, TIMESTAMP_MS,
-- and TIMESTAMP_NS. Infinity and a timestamp outside the range of a TIME cast
-- fail with 'ConversionFailed'. See Note [Temporal casts].
instance FromField TimeOfDay where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTime tod -> pure tod
      FieldTimestamp (Finite timestamp)
        | Nothing <- localTimeMicros timestamp ->
            returnError ConversionFailed f "timestamp is outside the range of a TIME cast"
      FieldTimestamp timestamp -> finiteField f (localTimeOfDay <$> timestamp)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TIMETZ.
instance FromField TimeWithZone where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTimeTZ tz -> pure tz
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TIMESTAMP, TIMESTAMP_S, TIMESTAMP_MS, TIMESTAMP_NS, and DATE at
-- midnight. It does not read TIMESTAMPTZ. Infinity and a DATE outside the
-- TIMESTAMP range fail with 'ConversionFailed'. See Note [Temporal casts]
-- and Note [Reads of another SQL type].
instance FromField LocalTime where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTimestamp ts -> finiteField f ts
      FieldDate day -> case dateTimestamp day of
        Just timestamp -> finiteField f timestamp
        Nothing -> returnError ConversionFailed f "date is outside the TIMESTAMP range"
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads the SQL types that t'LocalTime' reads, also infinity. See
-- Note [Temporal casts].
instance FromField (Unbounded LocalTime) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTimestamp ts -> pure ts
      FieldDate day -> case dateTimestamp day of
        Just timestamp -> pure timestamp
        Nothing -> returnError ConversionFailed f "date is outside the TIMESTAMP range"
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads INTERVAL.
instance FromField IntervalValue where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInterval interval -> pure interval
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TIMESTAMPTZ and TIMESTAMPTZ_NS. It does not read TIMESTAMP or DATE.
-- Infinity fails with 'ConversionFailed'. See Note [Reads of another SQL type].
instance FromField UTCTime where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTimestampTZ utcTime -> finiteField f utcTime
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads the SQL types that t'UTCTime' reads, also infinity.
instance FromField (Unbounded UTCTime) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTimestampTZ utcTime -> pure utcTime
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reject infinity when the requested Haskell type holds only finite values.
finiteField :: (Typeable a) => Field -> Unbounded a -> Conversion a
finiteField _ (Finite value) = pure value
finiteField field _ = returnError ConversionFailed field "infinity requires an Unbounded date or timestamp"

-- | Reads NULL as Nothing, and each other value as @a@.
instance (FromField a) => FromField (Maybe a) where
  fromField Field{fieldValue = FieldNull} = pure Nothing
  fromField field = Just <$> fromField field

-- | Helper for bounded integral conversions.
boundedIntegral :: forall a. (Integral a, Bounded a, Typeable a) => Field -> Int64 -> Conversion a
boundedIntegral f = boundedFromInteger f . toInteger

boundedFromInteger :: forall a. (Integral a, Bounded a, Typeable a) => Field -> Integer -> Conversion a
boundedFromInteger f@Field{} value
  | value < toInteger (minBound :: a) =
      returnError ConversionFailed f "integer value out of bounds"
  | value > toInteger (maxBound :: a) =
      returnError ConversionFailed f "integer value out of bounds"
  | otherwise = pure (fromInteger value)

-- | Construct a conversion failure with field context when diagnostics are needed.
-- See Note [Conversion effects] in "Database.DuckDB.Simple.Conversion".
returnError :: forall b. (Typeable b) => (Text -> Text -> Text -> Text -> ResultError) -> Field -> Text -> Conversion b
returnError mkError Field{fieldValue, fieldName, fieldType} msg =
  conversionErrorWith $ do
    logical <- fieldType
    pure $
      mkError
        (fieldTypeName logical fieldValue)
        fieldName
        (Text.pack $ show (typeRep (Proxy @b)))
        msg

-- | The SQL type name of a field in an error: the text of its type, or the type
-- of the stored value when the type is VARIANT. See
-- Note [Reads of another SQL type].
fieldTypeName :: LogicalTypeRep -> FieldValue -> Text
fieldTypeName logical value = case aliasBase logical of
  LogicalTypeScalar LogicalTypeIdVariant -> fieldValueTypeName value
  _ -> logicalTypeSQL logical

-- | The SQL type name of a decoded value, in the type text of DuckDB
-- (logicalTypeSQL), for example TIMESTAMP WITH TIME ZONE or DECIMAL(18,3). A
-- value that does not hold its full type (a LIST, an ARRAY, a MAP, an ENUM, a
-- STRUCT, a TUPLE, or a UNION) gives the name of its kind. See
-- Note [Reads of another SQL type].
fieldValueTypeName :: FieldValue -> Text
fieldValueTypeName = \case
  FieldNull -> scalar LogicalTypeIdSQLNull
  FieldInt8{} -> scalar LogicalTypeIdTinyInt
  FieldInt16{} -> scalar LogicalTypeIdSmallInt
  FieldInt32{} -> scalar LogicalTypeIdInteger
  FieldInt64{} -> scalar LogicalTypeIdBigInt
  FieldWord8{} -> scalar LogicalTypeIdUTinyInt
  FieldWord16{} -> scalar LogicalTypeIdUSmallInt
  FieldWord32{} -> scalar LogicalTypeIdUInteger
  FieldWord64{} -> scalar LogicalTypeIdUBigInt
  FieldFloat{} -> scalar LogicalTypeIdFloat
  FieldDouble{} -> scalar LogicalTypeIdDouble
  FieldText{} -> scalar LogicalTypeIdVarchar
  FieldBool{} -> scalar LogicalTypeIdBoolean
  FieldBlob{} -> scalar LogicalTypeIdBlob
  FieldGeometry RawGeometry{rawGeometryCRS} -> logicalTypeSQL (LogicalTypeGeometry rawGeometryCRS)
  FieldDate{} -> scalar LogicalTypeIdDate
  FieldTime{} -> scalar LogicalTypeIdTime
  FieldTimestamp{} -> scalar LogicalTypeIdTimestamp
  FieldInterval{} -> scalar LogicalTypeIdInterval
  FieldHugeInt{} -> scalar LogicalTypeIdHugeInt
  FieldUHugeInt{} -> scalar LogicalTypeIdUHugeInt
  FieldDecimal DecimalValue{decimalWidth, decimalScale} -> logicalTypeSQL (LogicalTypeDecimal decimalWidth decimalScale)
  FieldTimestampTZ{} -> scalar LogicalTypeIdTimestampTz
  FieldTimeTZ{} -> scalar LogicalTypeIdTimeTz
  FieldBit{} -> scalar LogicalTypeIdBit
  FieldBigNum{} -> scalar LogicalTypeIdBigNum
  FieldType{} -> scalar LogicalTypeIdType
  FieldEnum{} -> "ENUM"
  FieldArray{} -> "ARRAY"
  FieldList{} -> "LIST"
  FieldMap{} -> "MAP"
  FieldUUID{} -> scalar LogicalTypeIdUUID
  FieldStruct{} -> "STRUCT"
  FieldTuple{} -> "TUPLE"
  FieldUnion{} -> "UNION"
  where
    scalar = logicalTypeSQL . LogicalTypeScalar
