{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database.DuckDB.Simple.Internal.ValueHelpers where

import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, LocalTime (..), toGregorian, diffTimeToPicoseconds, timeOfDayToTime, TimeZone (timeZoneMinutes), utcToLocalTime, utc)
import qualified Data.Text.Foreign as TextForeign

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.FromField (BigNum (..), BitString (..), IntervalValue (..), TimeWithZone (..), toBigNumBytes, DecimalValue (..))

import Database.DuckDB.FFI
import Foreign (alloca, Storable (poke), Ptr, castPtr)
import Foreign.C.Types (CDouble (..))
import Foreign.Marshal (fromBool)
import Foreign.Ptr (nullPtr)
import Control.Monad (when)
import Control.Exception (throwIO)
import Data.Bits

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
        poke
            ptr
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

intervalDuckValue :: IntervalValue -> IO DuckDBValue
intervalDuckValue IntervalValue{intervalMonths, intervalDays, intervalMicros} =
    alloca \ptr -> do
        poke ptr (DuckDBInterval intervalMonths intervalDays intervalMicros)
        c_duckdb_create_interval ptr

timeWithZoneDuckValue :: TimeWithZone -> IO DuckDBValue
timeWithZoneDuckValue TimeWithZone{timeWithZoneTime, timeWithZoneZone} = do
    let totalMicros = diffTimeToPicoseconds (timeOfDayToTime timeWithZoneTime) `div` 1000000
        offsetSeconds = timeZoneMinutes timeWithZoneZone * 60
    tzValue <- c_duckdb_create_time_tz (fromIntegral totalMicros) (fromIntegral offsetSeconds)
    c_duckdb_create_time_tz_value tzValue


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
