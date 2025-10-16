{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Database.DuckDB.Simple.FromField
Description : Conversion from DuckDB column values to Haskell types.
-}
module Database.DuckDB.Simple.FromField (
    Field (..),
    FieldValue (..),
    BitString(..),
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

import Control.Exception (Exception, SomeException (..))
import Data.Bits (Bits (..), finiteBitSize)
import qualified Data.ByteString as BS
import Data.Data (Typeable, typeRep)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (
    LocalTime (..),
    TimeOfDay (..),
    TimeZone (..),
    localTimeToUTC,
    utc,
    utcToLocalTime,
 )
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Ok
import Database.DuckDB.Simple.Types (Null (..))
import GHC.Num.Integer (integerFromWordList)
import Numeric.Natural (Natural)
import qualified Data.UUID as UUID

-- | Internal representation of a column value.
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
    | FieldDate Day
    | FieldTime TimeOfDay
    | FieldTimestamp LocalTime
    | FieldInterval IntervalValue
    | FieldHugeInt Integer
    | FieldUHugeInt Integer
    | FieldDecimal DecimalValue
    | FieldTimestampTZ UTCTime
    | FieldTimeTZ TimeWithZone
    | FieldBit BitString
    | FieldBigNum BigNum
    | FieldEnum Word32
    | FieldList [FieldValue]
    | FieldMap [(FieldValue, FieldValue)]
    deriving (Eq, Show)

data DecimalValue = DecimalValue
    { decimalWidth :: !Word8
    , decimalScale :: !Word8
    , decimalInteger :: !Integer
    }
    deriving (Eq, Show)

data IntervalValue = IntervalValue
    { intervalMonths :: !Int32
    , intervalDays :: !Int32
    , intervalMicros :: !Int64
    }
    deriving (Eq, Show)


newtype BigNum = BigNum Integer
    deriving stock (Eq, Show)
    deriving (Num) via Integer

{- | Decode DuckDB’s BIGNUM blob (3-byte header + big-endian payload where negative magnitudes are
bitwise complemented) back into a Haskell 'Integer'. We undo the complement when needed, then chunk
the remaining bytes into machine-word limbs (MSB chunk first) for 'integerFromWordList'.
-}
fromBigNumBytes :: [Word8] -> Integer
fromBigNumBytes bytes =
    let header = take 3 bytes
        payloadRaw = drop 3 bytes
        isNeg = head header .&. 0x80 == 0
        payload = if isNeg then map complement payloadRaw else payloadRaw
        bytesPerWord = finiteBitSize (0 :: Word) `div` 8 -- 8 on 64-bit, 4 on 32-bit
        len = length payload
        (firstChunk, rest) = splitAt (len `mod` bytesPerWord) payload

        chunkWords [] = []
        chunkWords xs =
            let (chunk, remainder) = splitAt bytesPerWord xs
             in chunk : chunkWords remainder

        toWord = foldl (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0
        limbs = (if null firstChunk then id else (firstChunk :)) (chunkWords rest)
     in integerFromWordList isNeg (map toWord limbs)

{- | Encode an 'Integer' into DuckDB’s BIGNUM blob layout: emit the 3-byte header
  (sign bit plus payload length) followed by the magnitude bytes in the same
  big-endian / complemented-on-negative form that DuckDB stores internally.
-}
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
                 in go q (fromIntegral r : acc)

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
     in headerBytes <> payloadBytes

data BitString = BitString { padding :: !Word8
                           , bits :: !BS.ByteString}
    deriving stock (Eq)

instance Show BitString where
    show (BitString padding bits) =
        drop (fromIntegral padding) $ concatMap word8ToString $ BS.unpack bits
      where
        word8ToString :: Word8 -> String
        word8ToString w = map (\n -> if testBit w n then '1' else '0') [7, 6 .. 0]

-- | Construct a 'BitString' from a list of 'Bool's, where the first element
bsFromBool :: [Bool] -> BitString
bsFromBool bits =
    let totalBits = length bits
        padding = (8 - (totalBits `mod` 8)) `mod` 8
        paddedBits = replicate padding False ++ bits
        byteChunks = chunk 8 paddedBits
        byteValues = map bitsToWord8 byteChunks
     in BitString (fromIntegral padding) (BS.pack byteValues)
  where
    chunk _ [] = []
    chunk n xs = take n xs : chunk n (drop n xs)

    bitsToWord8 :: [Bool] -> Word8
    bitsToWord8 bs = foldl (\acc (b, i) -> if b then setBit acc i else acc) 0 (zip bs [7, 6 .. 0])


data TimeWithZone = TimeWithZone
    { timeWithZoneTime :: !TimeOfDay
    , timeWithZoneZone :: !TimeZone
    }
    deriving (Eq, Show)

-- | Pattern synonym to make it easier to match on any integral type.
pattern FieldInt :: Int -> FieldValue
pattern FieldInt i <- (fieldValueToInt -> Just i)
    where
        FieldInt i = FieldInt64 (fromIntegral i)

fieldValueToInt :: FieldValue -> Maybe Int
fieldValueToInt (FieldInt8 i) = Just (fromIntegral i)
fieldValueToInt (FieldInt16 i) = Just (fromIntegral i)
fieldValueToInt (FieldInt32 i) = Just (fromIntegral i)
fieldValueToInt (FieldInt64 i) = Just (fromIntegral i)
fieldValueToInt _ = Nothing

-- | Pattern synonym to make it easier to match on any word size
pattern FieldWord :: Word -> FieldValue
pattern FieldWord i <- (fieldValueToWord -> Just i)
    where
        FieldWord i = FieldWord64 (fromIntegral i)

fieldValueToWord :: FieldValue -> Maybe Word
fieldValueToWord (FieldWord8 i) = Just (fromIntegral i)
fieldValueToWord (FieldWord16 i) = Just (fromIntegral i)
fieldValueToWord (FieldWord32 i) = Just (fromIntegral i)
fieldValueToWord (FieldWord64 i) = Just (fromIntegral i)
fieldValueToWord _ = Nothing

-- | Metadata for a single column in a row.
data Field = Field
    { fieldName :: Text
    , fieldIndex :: Int
    , fieldValue :: FieldValue
    }
    deriving (Eq, Show)

{- | Exception thrown if conversion from a SQL value to a Haskell
value fails.
-}
data ResultError
    = -- | The SQL and Haskell types are not compatible.
      Incompatible
        { errSQLType :: Text
        , errSQLField :: Text
        , errHaskellType :: Text
        , errMessage :: Text
        }
    | -- | A SQL @NULL@ was encountered when the Haskell
      -- type did not permit it.
      UnexpectedNull
        { errSQLType :: Text
        , errSQLField :: Text
        , errHaskellType :: Text
        , errMessage :: Text
        }
    | -- | The SQL value could not be parsed, or could not
      -- be represented as a valid Haskell value, or an
      -- unexpected low-level error occurred (e.g. mismatch
      -- between metadata and actual data in a row).
      ConversionFailed
        { errSQLType :: Text
        , errSQLField :: Text
        , errHaskellType :: Text
        , errMessage :: Text
        }
    deriving (Eq, Show, Typeable)

instance Exception ResultError

{- | Parser used by 'FromField' instances and utilities such as
'Database.DuckDB.Simple.FromRow.fieldWith'. The supplied 'Field' contains
column metadata and an already-decoded 'FieldValue'; callers should return
'Ok' on success or 'Errors' (typically wrapping a 'ResultError') when the
conversion fails.
-}
type FieldParser a = Field -> Ok a

-- | Types that can be constructed from a DuckDB column.
class FromField a where
    fromField :: FieldParser a

instance FromField FieldValue where
    fromField Field{fieldValue} = Ok fieldValue

instance FromField Null where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldNull -> Ok Null
            _ -> returnError Incompatible f "expected NULL"

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

instance FromField Bool where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldBool b -> Ok b
            FieldInt i -> Ok (i /= 0)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Int8 where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> Ok (fromIntegral i)
            FieldHugeInt value -> boundedFromInteger f value
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> boundedFromInteger f (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Int64 where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> Ok (fromIntegral i)
            FieldHugeInt value -> boundedFromInteger f value
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> Ok (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Int32 where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> boundedIntegral f i
            FieldHugeInt value -> boundedFromInteger f value
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> boundedFromInteger f (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Int16 where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> boundedIntegral f i
            FieldHugeInt value -> boundedFromInteger f value
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> boundedFromInteger f (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Int where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> boundedIntegral f i
            FieldHugeInt value -> boundedFromInteger f value
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> boundedFromInteger f (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

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
            FieldEnum value -> Ok (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Word64 where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> Ok (fromIntegral i)
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldWord w -> Ok (fromIntegral w)
            FieldHugeInt value
                | value >= 0 -> boundedFromInteger f value
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> Ok (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Word32 where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedIntegral f i
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldWord w -> Ok (fromIntegral w)
            FieldHugeInt value
                | value >= 0 -> boundedFromInteger f value
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> Ok value
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Word16 where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedIntegral f i
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldWord w -> Ok (fromIntegral w)
            FieldHugeInt value
                | value >= 0 -> boundedFromInteger f value
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> boundedFromInteger f (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Word8 where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedIntegral f i
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldWord w -> Ok (fromIntegral w)
            FieldHugeInt value
                | value >= 0 -> boundedFromInteger f value
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> boundedFromInteger f (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Word where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedFromInteger f (fromIntegral i)
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldWord w -> Ok w
            FieldHugeInt value
                | value >= 0 -> boundedFromInteger f value
                | otherwise ->
                    returnError ConversionFailed f "negative value cannot be converted to unsigned integer"
            FieldUHugeInt value -> boundedFromInteger f value
            FieldEnum value -> boundedFromInteger f (fromIntegral value)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Double where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldDouble d -> Ok d
            FieldInt i -> Ok (fromIntegral i)
            FieldDecimal DecimalValue{decimalInteger, decimalScale} ->
                Ok (realToFrac decimalInteger / 10 ^ decimalScale)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Float where
    fromField field =
        case (fromField field :: Ok Double) of
            Errors err -> Errors err
            Ok d -> Ok (realToFrac d)

instance FromField Text where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldText t -> Ok t
            FieldInt i -> Ok (Text.pack (show i))
            FieldDouble d -> Ok (Text.pack (show d))
            FieldBool b -> Ok (if b then Text.pack "1" else Text.pack "0")
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField String where
    fromField field = Text.unpack <$> fromField field

instance FromField BS.ByteString where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldBlob bs -> Ok bs
            FieldText t -> Ok (TextEncoding.encodeUtf8 t)
            FieldBit (BitString _ bits) -> Ok bits
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField BitString where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldBit b -> Ok b
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField BigNum where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldBigNum big -> Ok big
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance {-# OVERLAPPABLE #-} (Typeable a, FromField a) => FromField [a] where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldList entries ->
                traverse (uncurry parseElement) (zip [0 :: Int ..] entries)
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
             in base <> Text.pack "[" <> idxText <> Text.pack "]"

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
             in Text.pack "[" <> idxText <> Text.pack "]" <> Text.pack label

instance FromField DecimalValue where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldDecimal dec -> Ok dec
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField Day where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldDate day -> Ok day
            FieldTimestamp LocalTime{localDay} -> Ok localDay
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField TimeOfDay where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldTime tod -> Ok tod
            FieldTimestamp LocalTime{localTimeOfDay} -> Ok localTimeOfDay
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField TimeWithZone where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldTimeTZ tz -> Ok tz
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField LocalTime where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldTimestamp ts -> Ok ts
            FieldDate day -> Ok (LocalTime day midnight)
            FieldTimestampTZ utcTime -> Ok (utcToLocalTime utc utcTime)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""
      where
        midnight = TimeOfDay 0 0 0

instance FromField IntervalValue where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldInterval interval -> Ok interval
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField UTCTime where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldTimestamp ts -> Ok (localTimeToUTC utc ts)
            FieldTimestampTZ utcTime -> Ok utcTime
            FieldDate day -> Ok (localTimeToUTC utc (LocalTime day midnight))
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""
      where
        midnight = TimeOfDay 0 0 0

instance (FromField a) => FromField (Maybe a) where
    fromField Field{fieldValue = FieldNull} = Ok Nothing
    fromField field = Just <$> fromField field

-- | Helper for bounded integral conversions.
boundedIntegral :: forall a. (Integral a, Bounded a, Typeable a) => Field -> Int -> Ok a
boundedIntegral f@Field{} i
    | toInteger i < toInteger (minBound :: a) =
        returnError ConversionFailed f "integer value out of bounds"
    | toInteger i > toInteger (maxBound :: a) =
        returnError ConversionFailed f "integer value out of bounds"
    | otherwise = Ok (fromIntegral i)

boundedFromInteger :: forall a. (Integral a, Bounded a, Typeable a) => Field -> Integer -> Ok a
boundedFromInteger f@Field{} value
    | value < toInteger (minBound :: a) =
        returnError ConversionFailed f "integer value out of bounds"
    | value > toInteger (maxBound :: a) =
        returnError ConversionFailed f "integer value out of bounds"
    | otherwise = Ok (fromInteger value)

{- | Helper to construct a ResultError with field context.
based on postgresql-simple's implementation
-}
returnError :: forall b. (Typeable b) => (Text -> Text -> Text -> Text -> ResultError) -> Field -> Text -> Ok b
returnError mkError Field{fieldValue, fieldName} msg =
    Errors
        [ SomeException $
            mkError
                (fieldValueTypeName fieldValue)
                fieldName
                (Text.pack $ show (typeRep (Proxy :: Proxy b)))
                msg
        ]

fieldValueTypeName :: FieldValue -> Text
fieldValueTypeName = \case
    FieldNull -> "NULL"
    FieldInt8{} -> "INT1"
    FieldInt16{} -> "INT2"
    FieldInt32{} -> "INT4"
    FieldInt64{} -> "INT8"
    FieldWord8{} -> "UTINYINT"
    FieldWord16{} -> "USMALLINT"
    FieldWord32{} -> "UINTEGER"
    FieldWord64{} -> "UBIGINT"
    FieldFloat{} -> "FLOAT"
    FieldDouble{} -> "DOUBLE"
    FieldText{} -> "TEXT"
    FieldBool{} -> "BOOLEAN"
    FieldBlob{} -> "BLOB"
    FieldDate{} -> "DATE"
    FieldTime{} -> "TIME"
    FieldTimestamp{} -> "TIMESTAMP"
    FieldInterval{} -> "INTERVAL"
    FieldHugeInt{} -> "HUGEINT"
    FieldUHugeInt{} -> "UHUGEINT"
    FieldDecimal{} -> "DECIMAL"
    FieldTimestampTZ{} -> "TIMESTAMP_TZ"
    FieldTimeTZ{} -> "TIME_TZ"
    FieldBit{} -> "BIT"
    FieldBigNum{} -> "BIGNUM"
    FieldEnum{} -> "ENUM"
    FieldList{} -> "LIST"
    FieldMap{} -> "MAP"
    FieldUUID{} -> "UUID"