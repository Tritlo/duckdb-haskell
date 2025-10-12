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
    ResultError (..),
    FieldParser,
    FromField (..),
    left
) where

import Control.Exception (Exception, SomeException (..))
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
import Database.DuckDB.Simple.Ok
import Data.Data (Typeable, Proxy (..), typeRep)

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

-- | Exception thrown if conversion from a SQL value to a Haskell
-- value fails.
data ResultError = Incompatible { errSQLType :: Text
                                , errHaskellType :: Text
                                , errMessage :: Text }
                 -- ^ The SQL and Haskell types are not compatible.
                 | UnexpectedNull { errSQLType :: Text
                                  , errHaskellType :: Text
                                  , errMessage :: Text }
                 -- ^ A SQL @NULL@ was encountered when the Haskell
                 -- type did not permit it.
                 | ConversionFailed { errSQLType :: Text
                                    , errHaskellType :: Text
                                    , errMessage :: Text }
                 -- ^ The SQL value could not be parsed, or could not
                 -- be represented as a valid Haskell value, or an
                 -- unexpected low-level error occurred (e.g. mismatch
                 -- between metadata and actual data in a row).
                   deriving (Eq, Show, Typeable)

instance Exception ResultError

-- From sqlite-simple:
left :: Exception a => a -> Ok b
left = Errors . (:[]) . SomeException


-- | Parser used by 'FromField' instances and utilities such as
-- 'Database.DuckDB.Simple.FromRow.fieldWith'. The supplied 'Field' contains
-- column metadata and an already-decoded 'FieldValue'; callers should return
-- either a successfully converted value or a 'ResultError' describing the
-- failure.
type FieldParser a = Field -> Ok a

-- | Types that can be constructed from a DuckDB column.
class FromField a where
    fromField :: FieldParser a

instance FromField FieldValue where
    fromField Field{fieldValue} = Ok fieldValue

instance FromField Null where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldNull -> Ok Null
            other -> left (Incompatible "NULL" (fieldValueTypeName other) "expected NULL")

instance FromField Bool where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldBool b -> Ok b
            FieldInt i -> Ok (i /= 0)
            other -> left (Incompatible "BOOL" (fieldValueTypeName other) "invalid type for boolean")

instance FromField Int8 where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldInt i -> Ok (fromIntegral i)
            other -> left (Incompatible "INT1" (fieldValueTypeName other) "invalid type for Int8")

instance FromField Int64 where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldInt i -> Ok (fromIntegral i)
            other -> left (Incompatible "INT8" (fieldValueTypeName other) "invalid type for Int64")

instance FromField Int32 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> boundedIntegral field i
            other -> left (Incompatible "INT4" (fieldValueTypeName other) "invalid type for integer")

instance FromField Int16 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> boundedIntegral field i
            other -> left (Incompatible "INT2" (fieldValueTypeName other) "invalid type for integer")

instance FromField Int where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i -> boundedIntegral field i
            other -> left (Incompatible "INT4" (fieldValueTypeName other) "invalid type for integer")

instance FromField Word64 where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> Ok (fromIntegral i)
                | otherwise ->
                    left $
                        ConversionFailed "Word32" (fieldValueTypeName fieldValue) $
                                 Text.pack "negative value cannot be converted to unsigned integer"
            FieldWord w -> Ok (fromIntegral w)
            other -> left (Incompatible "Word64" (fieldValueTypeName other) "invalid type for Word64")

instance FromField Word32 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedIntegral field i
                | otherwise ->
                    left $
                        ConversionFailed "Word32" (fieldValueTypeName fieldValue) $
                                 Text.pack "negative value cannot be converted to unsigned integer"
            FieldWord w -> Ok (fromIntegral w)
            other -> left (Incompatible "Word32" (fieldValueTypeName other) "invalid type for Word32")

instance FromField Word16 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedIntegral field i
                | otherwise ->
                    left $
                        ConversionFailed "Word16" (fieldValueTypeName fieldValue) $
                                 Text.pack "negative value cannot be converted to unsigned integer"
            FieldWord w -> Ok (fromIntegral w)
            other -> left (Incompatible "Word16" (fieldValueTypeName other) "invalid type for Word16")

instance FromField Word8 where
    fromField field@Field{fieldValue} =
        case fieldValue of
            FieldInt i
                | i >= 0 -> boundedIntegral field i
                | otherwise ->
                    left $
                        ConversionFailed "Word8" (fieldValueTypeName fieldValue) $
                                 Text.pack "negative value cannot be converted to unsigned integer"
            FieldWord w -> Ok (fromIntegral w)
            other -> left (Incompatible "Word8" (fieldValueTypeName other) "invalid type for Word8")

instance FromField Double where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldDouble d -> Ok d
            FieldInt i -> Ok (fromIntegral i)
            other -> left (Incompatible "DOUBLE" (fieldValueTypeName other) "invalid type for double")

instance FromField Float where
    fromField field =
        case (fromField field :: Ok Double) of
            Errors err -> Errors err
            Ok d -> Ok (realToFrac d)

instance FromField Text where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldText t -> Ok t
            FieldInt i -> Ok (Text.pack (show i))
            FieldDouble d -> Ok (Text.pack (show d))
            FieldBool b -> Ok (if b then Text.pack "1" else Text.pack "0")
            FieldNull ->
                left $ UnexpectedNull "TEXT" (fieldValueTypeName fieldValue) $ Text.pack "unexpected null value"
            other -> left (Incompatible "TEXT" (fieldValueTypeName other) "invalid type for text")

instance FromField String where
    fromField field = Text.unpack <$> fromField field

instance FromField BS.ByteString where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldBlob bs -> Ok bs
            FieldText t -> Ok (TextEncoding.encodeUtf8 t)
            FieldNull ->
                left $ UnexpectedNull "BLOB" (fieldValueTypeName fieldValue) $ Text.pack "unexpected null value"
            other -> left (Incompatible "BLOB" (fieldValueTypeName other) "invalid type for blob")

instance FromField Day where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldDate day -> Ok day
            FieldTimestamp LocalTime{localDay} -> Ok localDay
            other -> left (Incompatible "DATE" (fieldValueTypeName other) "invalid type for date")

instance FromField TimeOfDay where
    fromField Field{fieldValue} =
        case fieldValue of
            FieldTime tod -> Ok tod
            FieldTimestamp LocalTime{localTimeOfDay} -> Ok localTimeOfDay
            other -> left (Incompatible "TIME" (fieldValueTypeName other) "invalid type for time")

instance FromField LocalTime where
    fromField Field{ fieldValue} =
        case fieldValue of
            FieldTimestamp ts -> Ok ts
            FieldDate day -> Ok (LocalTime day midnight)
            other -> left (Incompatible "TIMESTAMP" (fieldValueTypeName other) "invalid type for timestamp")
      where
        midnight = TimeOfDay 0 0 0

instance FromField UTCTime where
    fromField field =
        case fromField field of
            Ok (timestamp :: LocalTime) -> Ok (localTimeToUTC utc timestamp)
            Errors errs -> Errors errs

instance (FromField a) => FromField (Maybe a) where
    fromField Field{fieldValue = FieldNull} = Ok Nothing
    fromField field = Just <$> fromField field

-- | Helper for bounded integral conversions.
boundedIntegral :: forall a. (Integral a, Bounded a, Typeable a) => Field -> Int -> Ok a
boundedIntegral Field{fieldValue} i
    | toInteger i < toInteger (minBound :: a) =
        left $ ConversionFailed (fieldValueTypeName fieldValue) (Text.pack $ show (typeRep (Proxy :: Proxy a))) $ Text.pack "integer value out of bounds"
    | toInteger i > toInteger (maxBound :: a) =
        left $ ConversionFailed (fieldValueTypeName fieldValue) (Text.pack $ show (typeRep (Proxy :: Proxy a))) $ Text.pack "integer value out of bounds"
    | otherwise = Ok (fromIntegral i)

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

-- TODO: Not supported yet
-- FieldInteger{} -> "HUGEINT"
-- FieldNatural{} -> "UHUGEINT"
