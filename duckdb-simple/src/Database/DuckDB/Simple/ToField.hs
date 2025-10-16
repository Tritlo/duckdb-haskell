{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
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
import Data.Array (Array, elems)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), timeOfDayToTime, utc, utcToLocalTime)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (BigNum (..), BitString (..), toBigNumBytes)
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
import Foreign.Marshal.Array (withArray)
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

class ToDuckValue a where
    toDuckValue :: a -> IO DuckDBValue

valueBinding :: String -> IO DuckDBValue -> FieldBinding
valueBinding display mkValue =
    mkFieldBinding display $ \stmt idx ->
        bindDuckValue stmt idx mkValue

-- | Types that map to a concrete DuckDB column type when used with 'ToField'.
class DuckDBColumnType a where
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
class (DuckDBColumnType a) => ToField a where
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
instance ToField UTCTime

instance ToField BigNum where
    toField big@(BigNum n) = valueBinding (show n) (bigNumDuckValue big)

instance DuckDBColumnType BitString where
    duckdbColumnTypeFor _ = "BIT"

instance ToField BS.ByteString where
    toField bs =
        valueBinding
            ("<blob length=" <> show (BS.length bs) <> ">")
            (toDuckValue bs)

instance (DuckDBColumnType a, ToDuckValue a) => ToField (Array Int a) where
    toField arr =
        valueBinding
            ("<array length=" <> show (length (elems arr)) <> ">")
            (arrayDuckValue arr)

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
floatDuckValue = c_duckdb_create_double . CDouble . realToFrac

textDuckValue :: Text -> IO DuckDBValue
textDuckValue txt =
    TextForeign.withCString txt c_duckdb_create_varchar

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
        poke ptr
            DuckDBUHugeInt
                { duckDBUHugeIntLower = lower
                , duckDBUHugeIntUpper = upper
                }
        c_duckdb_create_uuid ptr

bitDuckValue :: BitString -> IO DuckDBValue
bitDuckValue (BitString padding bits) =
    let withPacked action =
            if BS.null bits
                then alloca \ptr -> do
                    poke
                        ptr
                        DuckDBBit
                            { duckDBBitData = nullPtr
                            , duckDBBitSize = 0
                            }
                    action ptr
                else
                    let payload = BS.pack ((fromIntegral padding :: Word8) : BS.unpack bits)
                     in BS.useAsCStringLen payload \(rawPtr, len) ->
                            alloca \ptr -> do
                                poke
                                    ptr
                                    DuckDBBit
                                        { duckDBBitData = castPtr rawPtr
                                        , duckDBBitSize = fromIntegral len
                                        }
                                action ptr
     in withPacked c_duckdb_create_bit

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
    let local = utcToLocalTime utc utcTime
     in localTimeDuckValue local

arrayDuckValue ::
    forall a.
    (DuckDBColumnType a, ToDuckValue a) =>
    Array Int a ->
    IO DuckDBValue
arrayDuckValue arr =
    bracket (createElementLogicalType (Proxy :: Proxy a)) destroyLogicalType \elementType -> do
        let elemsList = elems arr
            count = length elemsList
        if count == 0
            then c_duckdb_create_array_value elementType nullPtr 0
            else do
                values <- mapM toDuckValue elemsList
                result <-
                    withArray values \ptr ->
                        c_duckdb_create_array_value elementType ptr (fromIntegral count)
                mapM_ destroyValue values
                pure result

createElementLogicalType :: forall a. (DuckDBColumnType a) => Proxy a -> IO DuckDBLogicalType
createElementLogicalType proxy =
    let typeName = duckdbColumnType proxy
     in case duckDBTypeFromName typeName of
            Just dtype -> c_duckdb_create_logical_type dtype
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

instance (ToDuckValue a) => ToDuckValue (Maybe a) where
    toDuckValue Nothing = nullDuckValue
    toDuckValue (Just value) = toDuckValue value

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
