{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Database.DuckDB.Simple.FromField
Description : Conversion from DuckDB column values to Haskell types.
-}
module Database.DuckDB.Simple.FromField (
    Field (..),
    FieldValue (..),
    BitString (..),
    BigNum (..),
    DecimalValue (..),
    IntervalValue (..),
    TimeWithZone (..),
    ResultError (..),
    FieldParser,
    FromField (..),
    returnError
) where

import Control.Exception (Exception, SomeException (..))
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime
    ( LocalTime (..)
    , TimeOfDay (..)
    , TimeZone (..)
    , localTimeToUTC
    , utc
    , utcToLocalTime
    )
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Types (Null (..))
import Database.DuckDB.Simple.Ok
import Data.Data (Typeable, typeRep)
import Data.Proxy (Proxy (..))
import Numeric.Natural (Natural)

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
    , decimalApproximate :: !Double
    }
    deriving (Eq, Show)

data IntervalValue = IntervalValue
    { intervalMonths :: !Int32
    , intervalDays :: !Int32
    , intervalMicros :: !Int64
    }
    deriving (Eq, Show)

data BitString = BitString
    { bitStringLength :: !Word64
    , bitStringBytes :: !BS.ByteString
    }
    deriving (Eq, Show)

data BigNum = BigNum
    { bigNumIsNegative :: !Bool
    , bigNumMagnitude :: !BS.ByteString
    }
    deriving (Eq, Show)

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

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError = Incompatible { errSQLType :: Text
                                , errSQLField :: Text
                                , errHaskellType :: Text
                                , errMessage :: Text }
                 -- ^ The SQL and Haskell types are not compatible.
                 | UnexpectedNull { errSQLType :: Text
                                  , errSQLField :: Text
                                  , errHaskellType :: Text
                                  , errMessage :: Text }
                 -- ^ A SQL @NULL@ was encountered when the Haskell
                 -- type did not permit it.
                 | ConversionFailed { errSQLType :: Text
                                    , errSQLField :: Text
                                    , errHaskellType :: Text
                                    , errMessage :: Text }
                 -- ^ The SQL value could not be parsed, or could not
                 -- be represented as a valid Haskell value, or an
                 -- unexpected low-level error occurred (e.g. mismatch
                 -- between metadata and actual data in a row).
                   deriving (Eq, Show, Typeable)

instance Exception ResultError



-- | Parser used by 'FromField' instances and utilities such as
-- 'Database.DuckDB.Simple.FromRow.fieldWith'. The supplied 'Field' contains
-- column metadata and an already-decoded 'FieldValue'; callers should return
-- 'Ok' on success or 'Errors' (typically wrapping a 'ResultError') when the
-- conversion fails.
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
            _ ->  returnError Incompatible f "expected NULL"

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
            FieldDecimal DecimalValue{decimalApproximate} -> Ok decimalApproximate
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
            FieldBit bit -> Ok (bitStringBytes bit)
            FieldNull -> returnError UnexpectedNull f ""
            _ -> returnError Incompatible f ""

instance FromField BitString where
    fromField f@Field{fieldValue} =
        case fieldValue of
            FieldBit bit -> Ok bit
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
    fromField f@Field{ fieldValue} =
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

-- | Helper to construct a ResultError with field context.
-- based on postgresql-simple's implementation
returnError :: forall b. (Typeable b) => (Text -> Text -> Text -> Text -> ResultError) -> Field -> Text -> Ok b
returnError mkError Field{fieldValue, fieldName} msg =
    Errors [SomeException $ mkError (fieldValueTypeName fieldValue)
                                    fieldName
                                    (Text.pack $ show (typeRep (Proxy :: Proxy b)))
                                    msg]


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
