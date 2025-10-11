{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Database.DuckDB.Simple.FromField
Description : Conversion from DuckDB column values to Haskell types.
-}
module Database.DuckDB.Simple.FromField (
    Field (..),
    FieldValue (..),
    ResultError (..),
    FieldParser,
    FromField (..),
) where

import Control.Exception (Exception)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), localTimeToUTC, utc)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Types (Null (..))

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
    -- TODO: HugeInt and UHugeInt support

    deriving (-- | FieldInteger Integer
              -- | FieldNatural Natural
              Eq, Show)

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

-- | Detailed information about row conversion failures.
data ResultError
    = IncompatibleType
        { resultErrorColumn :: Int
        , resultErrorExpected :: String
        , resultErrorActual :: String
        }
    | UnexpectedNull
        { resultErrorColumn :: Int
        , resultErrorExpected :: String
        }
    | ColumnCountMismatch
        { resultErrorExpectedCols :: Int
        , resultErrorActualCols :: Int
        }
    | ConversionError
        { resultErrorColumn :: Int
        , resultErrorMessage :: Text
        }
    deriving (Eq, Show)

instance Exception ResultError

-- | Parser used by 'FromField' instances and utilities such as
-- 'Database.DuckDB.Simple.FromRow.fieldWith'. The supplied 'Field' contains
-- column metadata and an already-decoded 'FieldValue'; callers should return
-- either a successfully converted value or a 'ResultError' describing the
-- failure.
type FieldParser a = Field -> Either ResultError a

-- | Types that can be constructed from a DuckDB column.
class FromField a where
    fromField :: FieldParser a

instance FromField FieldValue where
    fromField Field{fieldValue} = Right fieldValue

instance FromField Null where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldNull -> Right Null
            other -> Left (IncompatibleType fieldIndex "NULL" (fieldValueTypeName other))

instance FromField Bool where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldBool b -> Right b
            FieldInt i -> Right (i /= 0)
            other -> Left (IncompatibleType fieldIndex "BOOL" (fieldValueTypeName other))

instance FromField Int8 where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldInt i -> Right (fromIntegral i)
            other -> Left (IncompatibleType fieldIndex "INTEGER" (fieldValueTypeName other))

instance FromField Int64 where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldInt i -> Right (fromIntegral i)
            other -> Left (IncompatibleType fieldIndex "INTEGER" (fieldValueTypeName other))

instance FromField Int32 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> boundedIntegral field i
            other -> Left (IncompatibleType (fieldIndex field) "INTEGER" (fieldValueTypeName other))

instance FromField Int16 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> boundedIntegral field i
            other -> Left (IncompatibleType (fieldIndex field) "INTEGER" (fieldValueTypeName other))

instance FromField Int where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> boundedIntegral field i
            other -> Left (IncompatibleType (fieldIndex field) "INTEGER" (fieldValueTypeName other))

instance FromField Word64 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> Right (fromIntegral i)
                | otherwise ->
                    Left $
                        ConversionError
                            { resultErrorColumn = fieldIndex field
                            , resultErrorMessage = Text.pack "negative value cannot be converted to unsigned integer"
                            }
            FieldWord w -> Right (fromIntegral w)
            other -> Left (IncompatibleType (fieldIndex field) "UNSIGNED" (fieldValueTypeName other))

instance FromField Word32 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedIntegral field i
                | otherwise ->
                    Left $
                        ConversionError
                            { resultErrorColumn = fieldIndex field
                            , resultErrorMessage = Text.pack "negative value cannot be converted to unsigned integer"
                            }
            FieldWord w -> Right (fromIntegral w)
            other -> Left (IncompatibleType (fieldIndex field) "UNSIGNED" (fieldValueTypeName other))

instance FromField Word16 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedIntegral field i
                | otherwise ->
                    Left $
                        ConversionError
                            { resultErrorColumn = fieldIndex field
                            , resultErrorMessage = Text.pack "negative value cannot be converted to unsigned integer"
                            }
            FieldWord w -> Right (fromIntegral w)
            other -> Left (IncompatibleType (fieldIndex field) "UNSIGNED" (fieldValueTypeName other))

instance FromField Word8 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedIntegral field i
                | otherwise ->
                    Left $
                        ConversionError
                            { resultErrorColumn = fieldIndex field
                            , resultErrorMessage = Text.pack "negative value cannot be converted to unsigned integer"
                            }
            FieldWord w -> Right (fromIntegral w)
            other -> Left (IncompatibleType (fieldIndex field) "UNSIGNED" (fieldValueTypeName other))

instance FromField Double where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldDouble d -> Right d
            FieldInt i -> Right (fromIntegral i)
            other -> Left (IncompatibleType fieldIndex "DOUBLE" (fieldValueTypeName other))

instance FromField Float where
    fromField field =
        case (fromField field :: Either ResultError Double) of
            Left err -> Left err
            Right d -> Right (realToFrac d)

instance FromField Text where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldText t -> Right t
            FieldInt i -> Right (Text.pack (show i))
            FieldDouble d -> Right (Text.pack (show d))
            FieldBool b -> Right (if b then Text.pack "1" else Text.pack "0")
            FieldNull ->
                Left
                    UnexpectedNull
                        { resultErrorColumn = fieldIndex
                        , resultErrorExpected = "TEXT"
                        }
            other -> Left (IncompatibleType fieldIndex "TEXT" (fieldValueTypeName other))

instance FromField String where
    fromField field = Text.unpack <$> fromField field

instance FromField BS.ByteString where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldBlob bs -> Right bs
            FieldText t -> Right (TextEncoding.encodeUtf8 t)
            FieldNull ->
                Left
                    UnexpectedNull
                        { resultErrorColumn = fieldIndex
                        , resultErrorExpected = "BLOB"
                        }
            other -> Left (IncompatibleType fieldIndex "BLOB" (fieldValueTypeName other))

instance FromField Day where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldDate day -> Right day
            FieldTimestamp LocalTime{localDay} -> Right localDay
            other -> Left (IncompatibleType fieldIndex "DATE" (fieldValueTypeName other))

instance FromField TimeOfDay where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldTime tod -> Right tod
            FieldTimestamp LocalTime{localTimeOfDay} -> Right localTimeOfDay
            other -> Left (IncompatibleType fieldIndex "TIME" (fieldValueTypeName other))

instance FromField LocalTime where
    fromField Field{fieldIndex, fieldValue} =
        case fieldValue of
            FieldTimestamp ts -> Right ts
            FieldDate day -> Right (LocalTime day midnight)
            other -> Left (IncompatibleType fieldIndex "TIMESTAMP" (fieldValueTypeName other))
      where
        midnight = TimeOfDay 0 0 0

instance FromField UTCTime where
    fromField field =
        case fromField field of
            Right (timestamp :: LocalTime) -> Right (localTimeToUTC utc timestamp)
            Left err -> Left err

instance (FromField a) => FromField (Maybe a) where
    fromField Field{fieldValue = FieldNull} = Right Nothing
    fromField field = Just <$> fromField field

-- | Helper for bounded integral conversions.
boundedIntegral :: forall a. (Integral a, Bounded a) => Field -> Int -> Either ResultError a
boundedIntegral Field{fieldIndex} i
    | toInteger i < toInteger (minBound :: a) =
        Left $
            ConversionError
                { resultErrorColumn = fieldIndex
                , resultErrorMessage = Text.pack "integer value out of bounds"
                }
    | toInteger i > toInteger (maxBound :: a) =
        Left $
            ConversionError
                { resultErrorColumn = fieldIndex
                , resultErrorMessage = Text.pack "integer value out of bounds"
                }
    | otherwise = Right (fromIntegral i)

fieldValueTypeName :: FieldValue -> String
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

-- TODO: Not supported yet
-- FieldInteger{} -> "HUGEINT"
-- FieldNatural{} -> "UHUGEINT"
