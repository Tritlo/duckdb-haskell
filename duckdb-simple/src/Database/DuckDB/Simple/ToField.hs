{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.ToField
Description : Convert Haskell parameters into DuckDB bindable values.

The 'ToField' class mirrors the interface provided by @sqlite-simple@ while
delegating to the DuckDB C API under the hood.
-}
module Database.DuckDB.Simple.ToField (
    FieldBinding,
    ToField (..),
    DuckDBColumnType (..),
    NamedParam (..),
    duckdbColumnType,
    bindFieldBinding,
    renderFieldBinding,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import Data.Bits (complement)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), timeOfDayToTime, utc, utcToLocalTime)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (BigNum (..), toBigNumBytes)
import Database.DuckDB.Simple.Internal (
    SQLError (..),
    Statement (..),
    withStatementHandle,
 )
import Database.DuckDB.Simple.Types (Null (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CDouble (..))
import Foreign.Marshal (fromBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (poke)
import Numeric.Natural (Natural)
import qualified Data.UUID as UUID

-- | Represents a named parameter binding using the @:=@ operator.
data NamedParam where
    (:=) :: (ToField a) => Text -> a -> NamedParam

infixr 3 :=

-- | Encapsulates the action required to bind a single positional parameter, together with a textual description used in diagnostics.
data FieldBinding = FieldBinding
    { fieldBindingAction :: !(Statement -> DuckDBIdx -> IO ())
    , fieldBindingDisplay :: !String
    }

-- | Types that map to a concrete DuckDB column type when used with 'ToField'.
class (ToField a) => DuckDBColumnType a where
    duckdbColumnTypeFor :: Proxy a -> Text

-- | Report the DuckDB column type that best matches a given 'ToField' instance.
duckdbColumnType :: forall a. (DuckDBColumnType a) => Proxy a -> Text
duckdbColumnType = duckdbColumnTypeFor

-- | Apply a 'FieldBinding' to the given statement/index.
bindFieldBinding :: Statement -> DuckDBIdx -> FieldBinding -> IO ()
bindFieldBinding stmt idx FieldBinding{fieldBindingAction} = fieldBindingAction stmt idx

-- | Render a bound parameter for error reporting.
renderFieldBinding :: FieldBinding -> String
renderFieldBinding FieldBinding{fieldBindingDisplay} = fieldBindingDisplay

mkFieldBinding :: String -> (Statement -> DuckDBIdx -> IO ()) -> FieldBinding
mkFieldBinding display action =
    FieldBinding
        { fieldBindingAction = action
        , fieldBindingDisplay = display
        }

-- | Types that can be used as positional parameters.
class ToField a where
    toField :: a -> FieldBinding

instance ToField Null where
    toField Null = nullBinding "NULL"


instance ToField Bool where
    toField value =
        mkFieldBinding (show value) $ \stmt idx ->
            bindDuckValue stmt idx (c_duckdb_create_bool (if value then 1 else 0))

instance ToField Int where
    toField = intBinding . (fromIntegral :: Int -> Int64)

instance ToField Int16 where
    toField = intBinding . (fromIntegral :: Int16 -> Int64)

instance ToField Int32 where
    toField = intBinding . (fromIntegral :: Int32 -> Int64)

instance ToField Int64 where
    toField = intBinding

instance ToField BigNum where
    toField = bignumBinding

instance ToField UUID.UUID where
    toField = uuidBinding

instance ToField Integer where
    toField :: Integer -> FieldBinding
    toField = toField . BigNum

instance ToField Natural where
    toField = toField . BigNum . toInteger

instance ToField Word where
    toField value = uint64Binding (fromIntegral value)

instance ToField Word16 where
    toField value = uint16Binding value

instance ToField Word32 where
    toField value = uint32Binding value

instance ToField Word64 where
    toField value = uint64Binding value

instance ToField Word8 where
    toField value = uint8Binding value

instance ToField Double where
    toField value =
        mkFieldBinding
            (show value)
            \stmt idx ->
                bindDuckValue stmt idx (c_duckdb_create_double (CDouble value))

instance ToField Float where
    toField value =
        mkFieldBinding
            (show value)
            \stmt idx ->
                bindDuckValue stmt idx (c_duckdb_create_double (CDouble (realToFrac value)))

instance ToField Text where
    toField txt =
        mkFieldBinding
            (show txt)
            \stmt idx ->
                TextForeign.withCString txt $ \cstr ->
                    bindDuckValue stmt idx (c_duckdb_create_varchar cstr)

instance ToField String where
    toField str =
        mkFieldBinding
            (show str)
            \stmt idx ->
                TextForeign.withCString (Text.pack str) $ \cstr ->
                    bindDuckValue stmt idx (c_duckdb_create_varchar cstr)

instance ToField BS.ByteString where
    toField bs =
        mkFieldBinding
            ("<blob length=" <> show (BS.length bs) <> ">")
            \stmt idx ->
                BS.useAsCStringLen bs \(ptr, len) ->
                    bindDuckValue stmt idx (c_duckdb_create_blob (castPtr ptr :: Ptr Word8) (fromIntegral len))

instance ToField Day where
    toField day =
        mkFieldBinding
            (show day)
            \stmt idx ->
                bindDuckValue stmt idx $ do
                    duckDate <- encodeDay day
                    c_duckdb_create_date duckDate

instance ToField TimeOfDay where
    toField tod =
        mkFieldBinding
            (show tod)
            \stmt idx ->
                bindDuckValue stmt idx $ do
                    duckTime <- encodeTimeOfDay tod
                    c_duckdb_create_time duckTime

instance ToField LocalTime where
    toField ts =
        mkFieldBinding
            (show ts)
            \stmt idx ->
                bindDuckValue stmt idx $ do
                    duckTimestamp <- encodeLocalTime ts
                    c_duckdb_create_timestamp duckTimestamp

instance ToField UTCTime where
    toField utcTime =
        let FieldBinding{fieldBindingAction = action} = toField (utcToLocalTime utc utcTime)
         in FieldBinding
                { fieldBindingAction = action
                , fieldBindingDisplay = show utcTime
                }

instance (ToField a) => ToField (Maybe a) where
    toField Nothing = nullBinding "Nothing"
    toField (Just value) =
        let binding = toField value
         in binding
                { fieldBindingDisplay = "Just " <> renderFieldBinding binding
                }

instance DuckDBColumnType Null where
    duckdbColumnTypeFor _ = "NULL"

instance DuckDBColumnType Bool where
    duckdbColumnTypeFor _ = "BOOLEAN"

instance DuckDBColumnType Int where
    duckdbColumnTypeFor _ = "BIGINT"

instance DuckDBColumnType Int16 where
    duckdbColumnTypeFor _ = "SMALLINT"

instance DuckDBColumnType Int32 where
    duckdbColumnTypeFor _ = "INTEGER"

instance DuckDBColumnType Int64 where
    duckdbColumnTypeFor _ = "BIGINT"

instance DuckDBColumnType BigNum where
    duckdbColumnTypeFor _ = "BIGNUM"

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

instance (DuckDBColumnType a) => DuckDBColumnType (Maybe a) where
    duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy :: Proxy a)

-- | Helper for binding 'Null' values.
nullBinding :: String -> FieldBinding
nullBinding repr =
    mkFieldBinding
        repr
        \stmt idx ->
            bindDuckValue stmt idx c_duckdb_create_null_value

intBinding :: Int64 -> FieldBinding
intBinding value =
    mkFieldBinding
        (show value)
        \stmt idx ->
            bindDuckValue stmt idx (c_duckdb_create_int64 value)

uint64Binding :: Word64 -> FieldBinding
uint64Binding value =
    mkFieldBinding
        (show value)
        \stmt idx ->
            bindDuckValue stmt idx (c_duckdb_create_uint64 value)

uint32Binding :: Word32 -> FieldBinding
uint32Binding value =
    mkFieldBinding
        (show value)
        \stmt idx ->
            bindDuckValue stmt idx (c_duckdb_create_uint32 value)

uint16Binding :: Word16 -> FieldBinding
uint16Binding value =
    mkFieldBinding
        (show value)
        \stmt idx ->
            bindDuckValue stmt idx (c_duckdb_create_uint16 value)

uint8Binding :: Word8 -> FieldBinding
uint8Binding value =
    mkFieldBinding
        (show value)
        \stmt idx ->
            bindDuckValue stmt idx (c_duckdb_create_uint8 value)

uuidBinding :: UUID.UUID -> FieldBinding
uuidBinding uuid =
    mkFieldBinding
        (show uuid)
        \stmt idx ->
            bindDuckValue stmt idx $
              alloca $ \ptr -> do
                let (upper, lower) = UUID.toWords64 uuid
                poke ptr DuckDBUHugeInt
                    { duckDBUHugeIntLower = lower
                    , duckDBUHugeIntUpper = upper
                    }
                c_duckdb_create_uuid ptr

bignumBinding :: BigNum -> FieldBinding
bignumBinding (BigNum big) =
    mkFieldBinding
        (show big)
        \stmt idx ->
            bindDuckValue stmt idx $
                let neg = fromBool (big < 0)
                    big_num_bytes =
                        BS.pack $
                            if big < 0
                                then map complement (drop 3 $ toBigNumBytes big)
                                else drop 3 $ toBigNumBytes big
                 in if BS.null big_num_bytes
                        then alloca \ptr -> do
                            poke
                                ptr
                                DuckDBBignum
                                    { duckDBBignumData = nullPtr
                                    , duckDBBignumSize = 0
                                    , duckDBBignumIsNegative = neg
                                    }
                            c_duckdb_create_bignum ptr
                        else BS.useAsCStringLen big_num_bytes \(rawPtr, len) ->
                            alloca \ptr -> do
                                poke
                                    ptr
                                    DuckDBBignum
                                        { duckDBBignumData = castPtr rawPtr
                                        , duckDBBignumSize = fromIntegral len
                                        , duckDBBignumIsNegative = neg
                                        }
                                c_duckdb_create_bignum ptr

encodeDay :: Day -> IO DuckDBDate
encodeDay day =
    alloca \ptr -> do
        poke ptr (dayToDateStruct day)
        c_duckdb_to_date ptr

encodeTimeOfDay :: TimeOfDay -> IO DuckDBTime
encodeTimeOfDay tod =
    alloca \ptr -> do
        poke ptr (timeOfDayToStruct tod)
        c_duckdb_to_time ptr

encodeLocalTime :: LocalTime -> IO DuckDBTimestamp
encodeLocalTime LocalTime{localDay, localTimeOfDay} =
    alloca \ptr -> do
        poke
            ptr
            DuckDBTimestampStruct
                { duckDBTimestampStructDate = dayToDateStruct localDay
                , duckDBTimestampStructTime = timeOfDayToStruct localTimeOfDay
                }
        c_duckdb_to_timestamp ptr

dayToDateStruct :: Day -> DuckDBDateStruct
dayToDateStruct day =
    let (year, month, dayOfMonth) = toGregorian day
     in DuckDBDateStruct
            { duckDBDateStructYear = fromIntegral year
            , duckDBDateStructMonth = fromIntegral month
            , duckDBDateStructDay = fromIntegral dayOfMonth
            }

timeOfDayToStruct :: TimeOfDay -> DuckDBTimeStruct
timeOfDayToStruct tod =
    let totalPicoseconds = diffTimeToPicoseconds (timeOfDayToTime tod)
        totalMicros = totalPicoseconds `div` 1000000
        (hours, remHour) = totalMicros `divMod` (60 * 60 * 1000000)
        (minutes, remMinute) = remHour `divMod` (60 * 1000000)
        (seconds, micros) = remMinute `divMod` 1000000
     in DuckDBTimeStruct
            { duckDBTimeStructHour = fromIntegral hours
            , duckDBTimeStructMinute = fromIntegral minutes
            , duckDBTimeStructSecond = fromIntegral seconds
            , duckDBTimeStructMicros = fromIntegral micros
            }

bindDuckValue :: Statement -> DuckDBIdx -> IO DuckDBValue -> IO ()
bindDuckValue stmt idx makeValue =
    withStatementHandle stmt \handle ->
        bracket makeValue destroyValue \value -> do
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
