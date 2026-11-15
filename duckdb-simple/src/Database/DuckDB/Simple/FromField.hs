{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
  ResultError (..),
  FieldParser,
  FromField (..),
  returnError,
) where

import Control.Exception (Exception (..), SomeException (..))
import Data.Array (Array, assocs, bounds, listArray, range, (!))
import Data.Bits (Bits (..), finiteBitSize)
import qualified Data.ByteString as BS
import Data.Data (Typeable, typeRep)
import qualified Data.Geometry as G
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (
  LocalTime (..),
  TimeOfDay (..),
 )
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Geometry (RawGeometry (..), fromRawGeometry)
import Database.DuckDB.Simple.LogicalRep (
  LogicalTypeId (..),
  LogicalTypeRep (..),
  StructField (..),
  StructValue (..),
  TupleValue (..),
  UnionMemberType (..),
  UnionValue (..),
 )
import Database.DuckDB.Simple.Ok
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import GHC.Float (double2Float, float2Double, int2Double, int2Float, rationalToDouble, rationalToFloat, word2Double)
import GHC.Num.Integer (integerFromWordList)
import Numeric.Natural (Natural)

-- | Internal representation of a column value.
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
  | FieldEnum Word32 Text
  | FieldArray (Array Int FieldValue)
  | FieldList [FieldValue]
  | FieldMap [(FieldValue, FieldValue)]
  | FieldStruct (StructValue FieldValue)
  | FieldTuple (TupleValue FieldValue)
  | FieldUnion (UnionValue FieldValue)
  deriving (Eq, Show)

-- | Exact-width decimal payload plus its declared width and scale. A parameter
-- of this type binds as a DECIMAL with this width and scale. An @Array@, a
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

-- | Decode DuckDB’s BIGNUM blob (3-byte header + big-endian payload where negative magnitudes are
-- bitwise complemented) back into a Haskell @Integer@. We undo the complement when needed, then chunk
-- the remaining bytes into machine-word limbs (MSB chunk first) for @integerFromWordList@.
-- This is the BIGNUM storage of DuckDB 2.0.0. The V2 API does not commit to this
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

-- | Encode an @Integer@ into DuckDB’s BIGNUM blob layout: emit the 3-byte header
--   (sign bit plus payload length) followed by the magnitude bytes in the same
--   big-endian / complemented-on-negative form that DuckDB stores internally.
--   This is the BIGNUM storage of DuckDB 2.0.0. The V2 API does not commit to
--   this layout, and this library does not use this function.
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
-- in SQL text. Use @minutesToTimeZone (fromIntegral (offset \`quot\` 60))@ and
-- @fromIntegral (timeZoneMinutes zone) * 60@ to convert to and from a
-- @Data.Time@ zone. The first conversion drops the seconds of an offset.
data TimeWithZone = TimeWithZone
  { timeWithZoneTime :: TimeOfDay
  , timeWithZoneOffsetSeconds :: Int32
  }
  deriving (Eq, Show)

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

-- | Metadata for a single column in a row.
data Field = Field
  { fieldName :: Text
  , fieldIndex :: Int
  , fieldValue :: FieldValue
  }
  deriving (Eq, Show)

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
-- column metadata and an already-decoded @FieldValue@; callers should return
-- @Ok@ on success or @Errors@ (typically wrapping a @ResultError@) when the
-- conversion fails.
type FieldParser a = Field -> Ok a

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
read. The error names the SQL type of the decoded value, which is not always
the name that typeof gives: a TIMESTAMP_S, TIMESTAMP_MS, or TIMESTAMP_NS value
is TIMESTAMP, a TIMESTAMPTZ_NS value is TIMESTAMPTZ, a VARIANT value has the
type of its payload, and the error of a String names Text. These pairs are:

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
  the VARIANT shape. FromField does not know the SQL type of a column, so the
  one known difference stays: a JSON column reads as its text, as a VARCHAR
  column does, where c::VARIANT gives the parsed value. The bind side
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
  fromField Field{fieldValue} = Ok fieldValue

-- | Reads GEOMETRY, with its CRS.
instance FromField RawGeometry where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldGeometry value -> Ok value
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
      FieldStruct structVal -> Ok structVal
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f "expected STRUCT"

-- | Reads UNION.
instance FromField (UnionValue FieldValue) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldUnion unionVal -> Ok unionVal
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f "expected UNION"

-- | Reads UUID, and VARCHAR in the form of 36 characters with dashes. Each
-- other text fails with 'ConversionFailed'.
instance FromField UUID.UUID where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldText t ->
        case UUID.fromText t of
          Just uuid -> Ok uuid
          Nothing -> returnError ConversionFailed f "invalid UUID format"
      FieldUUID uuid -> Ok uuid
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TUPLE.
instance FromField (TupleValue FieldValue) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTuple tuple -> Ok tuple
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f "expected TUPLE"

-- | Reads BOOLEAN, and TINYINT, SMALLINT, INTEGER, and BIGINT. An integer other
-- than 0 is True, as in the cast to BOOLEAN.
instance FromField Bool where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldBool b -> Ok b
      FieldInt i -> Ok (i /= 0)
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
      FieldInt i -> Ok i
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
      FieldInt i -> Ok (fromIntegral i)
      FieldWord w -> Ok (fromIntegral w)
      FieldBigNum (BigNum big) -> Ok big
      FieldHugeInt value -> Ok value
      FieldUHugeInt value -> Ok value
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads BIGNUM, TINYINT to BIGINT, UTINYINT to UBIGINT, HUGEINT, and
-- UHUGEINT. A negative value fails with 'ConversionFailed'. It does not read
-- ENUM.
instance FromField Natural where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInt i
        | i >= 0 -> Ok (fromIntegral i)
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to Natural"
      FieldWord w -> Ok (fromIntegral w)
      FieldHugeInt value
        | value >= 0 -> Ok (fromIntegral value)
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to Natural"
      FieldUHugeInt value -> Ok (fromIntegral value)
      FieldBigNum (BigNum big) ->
        if big >= 0
          then Ok $ fromIntegral big
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
        | i >= 0 -> Ok (fromIntegral i)
        | otherwise ->
            returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
      FieldWord w -> Ok w
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
      FieldDouble d -> Ok d
      FieldFloat value -> Ok (float2Double value)
      FieldInt i -> Ok (fromIntegral i)
      FieldDecimal value -> Ok (decimalToDouble value)
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
      -- in the same way. As seen in CI, the DuckDB 2.0.0 build for macOS on
      -- Apple Silicon rounds some BIGINT values above 2^53 two times in its
      -- cast to FLOAT, and the library gives the nearest value there.
      FieldInt i -> Ok (int2Float (fromIntegral i))
      FieldDecimal value -> Ok (decimalToFloat value)
      _ -> case (fromField field :: Ok Double) of
        Errors err -> Errors err
        Ok d
          | not (isInfinite d || isNaN d) && isInfinite (double2Float d) ->
              returnError ConversionFailed field "floating-point value out of bounds"
          | otherwise -> Ok (double2Float d)

{- Note [DECIMAL as DOUBLE and FLOAT]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A DECIMAL that a Double or a Float reads gives the value of the cast of DuckDB
2.0.0 to DOUBLE or FLOAT. The cast is TryCastDecimalToFloatingPoint in
src/common/operator/cast_operators.cpp of DuckDB. decimalToDouble and
decimalToFloat use the same three paths:

1. Scale 0 and width 19 to 38. DuckDB stores the value as a HUGEINT and casts
   it with CastBigintToFloating in src/common/types/hugeint.cpp:

     case -1: result = -REAL_T(NumericLimits<uint64_t>::Maximum() - input.lower) - 1;
     default: result = REAL_T(input.lower) + REAL_T(input.upper) * (REAL_T(NumericLimits<uint64_t>::Maximum()) + 1);

   The case is the value of input.upper. A FLOAT result is this DOUBLE value
   cast to float (Hugeint::TryCast for float). hugeIntToDouble copies the
   formula. The formula rounds more than one time, so it does not always give
   the nearest value. DuckDB casts -9007199254740994::DECIMAL(19,0) to
   -9007199254740992.0, and 3356951675583485076575379654993209529::DECIMAL(38,0)
   to 3.356951675583485e36. The nearest values are -9007199254740994.0 and
   3.3569516755834854e36. This path exists only to give the value of the cast.
2. Scale 0 with a width of at most 18, or an integer of at most 2^53 (2^24
   for FLOAT) with a scale of at most 22 (10 for FLOAT). DuckDB converts the
   integer to the floating-point type and divides it by the power of ten in
   that type. Under the limits, the integer and the power of ten are exact in
   the type, so only the division rounds. With scale 0, the division by 1 is
   exact, so only the conversion of the integer rounds. Each step rounds one
   time to the nearest value.
3. Each other value. DuckDB converts the decimal digits with fast_float, which
   gives the nearest value.

Thus paths 2 and 3 give the nearest value, with ties to even. In path 2 the
library does the same steps. With scale 0 and a width of at most 18, the
integer is an Int64 value, and fromInteger converts it with int2Double or
int2Float, which round one time as the C++ conversion of int64_t does. In path
3 the library uses rationalToDouble or rationalToFloat. fromRational uses these
functions too. They give the nearest value with ties to even, and they do not
reduce the fraction first.

Path 3 is slower than path 2. It applies to a Double with a scale above 0 when
the unscaled integer is above 2^53 or the scale is above 22: at scale 2 for an
absolute value above 9.0e13, and at scale 10 above 9.0e5. It applies to a Float
with a scale above 0 when the unscaled integer is above 2^24 or the scale is
above 10.
-}

-- | The value of the cast of DuckDB from a DECIMAL to DOUBLE.
-- See Note [DECIMAL as DOUBLE and FLOAT].
decimalToDouble :: DecimalValue -> Double
decimalToDouble DecimalValue{decimalWidth, decimalScale, decimalInteger}
  | decimalScale == 0 && decimalWidth > 18 = hugeIntToDouble decimalInteger
  | decimalScale == 0 = fromInteger decimalInteger
  | decimalScale <= 22 && abs decimalInteger <= 9007199254740992 = fromInteger decimalInteger / 10 ^ decimalScale
  | otherwise = rationalToDouble decimalInteger (10 ^ decimalScale)

-- | The value of the cast of DuckDB from a DECIMAL to FLOAT.
-- See Note [DECIMAL as DOUBLE and FLOAT].
decimalToFloat :: DecimalValue -> Float
decimalToFloat DecimalValue{decimalWidth, decimalScale, decimalInteger}
  | decimalScale == 0 && decimalWidth > 18 = double2Float (hugeIntToDouble decimalInteger)
  | decimalScale == 0 = fromInteger decimalInteger
  | decimalScale <= 10 && abs decimalInteger <= 16777216 = fromInteger decimalInteger / 10 ^ decimalScale
  | otherwise = rationalToFloat decimalInteger (10 ^ decimalScale)

-- | The cast of DuckDB from a HUGEINT to DOUBLE. It is not always the nearest
-- value. See Note [DECIMAL as DOUBLE and FLOAT].
hugeIntToDouble :: Integer -> Double
hugeIntToDouble value
  | upper == -1 = negate (word2Double (maxBound - lower)) - 1
  | otherwise = word2Double lower + int2Double upper * 18446744073709551616
  where
    -- The two 64-bit halves of the HUGEINT in two's complement. word2Double
    -- and int2Double round one time to the nearest value, ties to even, as
    -- the C++ conversions of uint64_t and int64_t to double do. The C++
    -- factor is 2^64 after its rounding, so the product is exact. The sum or
    -- the difference rounds one time, as in C++.
    upper = fromInteger (value `shiftR` 64) :: Int
    lower = fromInteger value :: Word

-- | Reads VARCHAR, TINYINT to BIGINT as decimal digits, BOOLEAN as @true@ or
-- @false@, and the label of an ENUM value. It does not read FLOAT or DOUBLE. See
-- Note [Reads of another SQL type].
instance FromField Text where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldText t -> Ok t
      FieldInt i -> Ok (Text.pack (show i))
      FieldBool b -> Ok (if b then Text.pack "true" else Text.pack "false")
      FieldEnum _ label -> Ok label
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
      FieldBlob bs -> Ok bs
      FieldGeometry RawGeometry{rawGeometryWKB} -> Ok rawGeometryWKB
      FieldBit (BitString _ bits) -> Ok bits
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads BIT.
instance FromField BitString where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldBit b -> Ok b
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads BIGNUM.
instance FromField BigNum where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldBigNum big -> Ok big
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads LIST and ARRAY. Each element reads as @a@.
instance {-# OVERLAPPABLE #-} (Typeable a, FromField a) => FromField [a] where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldList entries ->
        traverse (uncurry parseElement) (zip [0 :: Int ..] entries)
      FieldArray arr ->
        traverse (uncurry parseElement) (assocs arr)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""
    where
      parseElement idx value =
        fromField
          Field
            { fieldName = fieldNameWithIndex idx
            , fieldIndex = fieldIndex f
            , fieldValue = value
            }
      fieldNameWithIndex idx =
        let base = fieldName f
            idxText = Text.pack (show idx)
        in  base <> Text.pack "[" <> idxText <> Text.pack "]"

-- | Reads ARRAY. Each element reads as @a@.
instance (Typeable a, FromField a) => FromField (Array Int a) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldArray arr -> do
        converted <- traverse (traverseElement arr) (range (bounds arr))
        pure (listArray (bounds arr) converted)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""
    where
      traverseElement arr idx =
        fromField
          Field
            { fieldName = fieldNameWithIndex idx
            , fieldIndex = fieldIndex f
            , fieldValue = arr ! idx
            }
      fieldNameWithIndex idx =
        let idxText = Text.pack (show idx)
        in  fieldName f <> Text.pack "[" <> idxText <> Text.pack "]"

-- | Reads MAP. Each key reads as @k@, and each value reads as @v@.
instance (Ord k, Typeable k, Typeable v, FromField k, FromField v) => FromField (Map k v) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldMap pairs -> Map.fromList <$> traverse (uncurry parsePair) (zip [0 :: Int ..] pairs)
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
              }
        value <-
          fromField
            Field
              { fieldName = fieldName f <> suffix idx ".value"
              , fieldIndex = fieldIndex f
              , fieldValue = valValue
              }
        pure (key, value)
      suffix idx label =
        let idxText = Text.pack (show idx)
        in  Text.pack "[" <> idxText <> Text.pack "]" <> Text.pack label

-- | Reads DECIMAL.
instance FromField DecimalValue where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldDecimal dec -> Ok dec
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads DATE, and the date of TIMESTAMP, TIMESTAMP_S, TIMESTAMP_MS, and
-- TIMESTAMP_NS. Infinity fails with 'ConversionFailed'.
instance FromField Day where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldDate day -> finiteField f day
      FieldTimestamp timestamp -> finiteField f (localDay <$> timestamp)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads the SQL types that 'Day' reads, also infinity.
instance FromField (Unbounded Day) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldDate day -> Ok day
      FieldTimestamp timestamp -> Ok (localDay <$> timestamp)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TIME, TIME_NS, and the time of TIMESTAMP, TIMESTAMP_S, TIMESTAMP_MS,
-- and TIMESTAMP_NS. An infinite TIMESTAMP fails with 'ConversionFailed'.
instance FromField TimeOfDay where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTime tod -> Ok tod
      FieldTimestamp timestamp -> finiteField f (localTimeOfDay <$> timestamp)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TIMETZ.
instance FromField TimeWithZone where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTimeTZ tz -> Ok tz
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reads TIMESTAMP, TIMESTAMP_S, TIMESTAMP_MS, TIMESTAMP_NS, and DATE at
-- midnight. It does not read TIMESTAMPTZ. Infinity fails with
-- 'ConversionFailed'. See Note [Reads of another SQL type].
instance FromField LocalTime where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTimestamp ts -> finiteField f ts
      FieldDate day -> finiteField f ((\value -> LocalTime value midnight) <$> day)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""
    where
      midnight = TimeOfDay 0 0 0

-- | Reads the SQL types that t'LocalTime' reads, also infinity.
instance FromField (Unbounded LocalTime) where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldTimestamp ts -> Ok ts
      FieldDate day -> Ok ((\value -> LocalTime value midnight) <$> day)
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""
    where
      midnight = TimeOfDay 0 0 0

-- | Reads INTERVAL.
instance FromField IntervalValue where
  fromField f@Field{fieldValue} =
    case fieldValue of
      FieldInterval interval -> Ok interval
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
      FieldTimestampTZ utcTime -> Ok utcTime
      FieldNull -> returnError UnexpectedNull f ""
      _ -> returnError Incompatible f ""

-- | Reject infinity when the requested Haskell type holds only finite values.
finiteField :: (Typeable a) => Field -> Unbounded a -> Ok a
finiteField _ (Finite value) = Ok value
finiteField field _ = returnError ConversionFailed field "infinity requires an Unbounded date or timestamp"

-- | Reads NULL as Nothing, and each other value as @a@.
instance (FromField a) => FromField (Maybe a) where
  fromField Field{fieldValue = FieldNull} = Ok Nothing
  fromField field = Just <$> fromField field

-- | Helper for bounded integral conversions.
boundedIntegral :: forall a. (Integral a, Bounded a, Typeable a) => Field -> Int64 -> Ok a
boundedIntegral f = boundedFromInteger f . toInteger

boundedFromInteger :: forall a. (Integral a, Bounded a, Typeable a) => Field -> Integer -> Ok a
boundedFromInteger f@Field{} value
  | value < toInteger (minBound :: a) =
      returnError ConversionFailed f "integer value out of bounds"
  | value > toInteger (maxBound :: a) =
      returnError ConversionFailed f "integer value out of bounds"
  | otherwise = Ok (fromInteger value)

-- | Helper to construct a ResultError with field context.
-- based on postgresql-simple's implementation
returnError :: forall b. (Typeable b) => (Text -> Text -> Text -> Text -> ResultError) -> Field -> Text -> Ok b
returnError mkError Field{fieldValue, fieldName} msg =
  Errors
    [ SomeException $
        mkError
          (fieldValueTypeName fieldValue)
          fieldName
          (Text.pack $ show (typeRep (Proxy @b)))
          msg
    ]

fieldValueTypeName :: FieldValue -> Text
fieldValueTypeName = \case
  FieldNull -> "NULL"
  FieldInt8{} -> "TINYINT"
  FieldInt16{} -> "SMALLINT"
  FieldInt32{} -> "INTEGER"
  FieldInt64{} -> "BIGINT"
  FieldWord8{} -> "UTINYINT"
  FieldWord16{} -> "USMALLINT"
  FieldWord32{} -> "UINTEGER"
  FieldWord64{} -> "UBIGINT"
  FieldFloat{} -> "FLOAT"
  FieldDouble{} -> "DOUBLE"
  FieldText{} -> "VARCHAR"
  FieldBool{} -> "BOOLEAN"
  FieldBlob{} -> "BLOB"
  FieldGeometry{} -> "GEOMETRY"
  FieldDate{} -> "DATE"
  FieldTime{} -> "TIME"
  FieldTimestamp{} -> "TIMESTAMP"
  FieldInterval{} -> "INTERVAL"
  FieldHugeInt{} -> "HUGEINT"
  FieldUHugeInt{} -> "UHUGEINT"
  FieldDecimal{} -> "DECIMAL"
  FieldTimestampTZ{} -> "TIMESTAMPTZ"
  FieldTimeTZ{} -> "TIMETZ"
  FieldBit{} -> "BIT"
  FieldBigNum{} -> "BIGNUM"
  FieldEnum{} -> "ENUM"
  FieldArray{} -> "ARRAY"
  FieldList{} -> "LIST"
  FieldMap{} -> "MAP"
  FieldUUID{} -> "UUID"
  FieldStruct{} -> "STRUCT"
  FieldTuple{} -> "TUPLE"
  FieldUnion{} -> "UNION"
