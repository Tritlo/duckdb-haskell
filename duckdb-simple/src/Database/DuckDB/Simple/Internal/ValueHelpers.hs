{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.DuckDB.Simple.Internal.ValueHelpers where

import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, LocalTime (..), toGregorian, diffTimeToPicoseconds, timeOfDayToTime, TimeZone (timeZoneMinutes), utcToLocalTime, utc, pattern YearMonthDay)

import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Data.Time.LocalTime (TimeOfDay(..))
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.FromField (BigNum (..), BitString (..), IntervalValue (..), TimeWithZone (..), toBigNumBytes, DecimalValue (..))

import Database.DuckDB.FFI
import Foreign (alloca, Storable (poke), Ptr, castPtr)
import Foreign.C.Types (CDouble (..), CFloat (CFloat))
import Foreign.Marshal (fromBool)
import Foreign.Ptr (nullPtr)
import Control.Monad (when)
import Control.Exception (throwIO)
import Data.Bits
import qualified Data.Text.Encoding as TextEncoding
import Database.DuckDB.Simple.Time

nullDuckValue :: IO DuckDBValue
nullDuckValue = c_duckdb_create_null_value
{-# INLINE nullDuckValue #-}

boolDuckValue :: Bool -> IO DuckDBValue
boolDuckValue value = c_duckdb_create_bool (if value then 1 else 0)
{-# INLINE boolDuckValue #-}

int8DuckValue :: Int8 -> IO DuckDBValue
int8DuckValue = c_duckdb_create_int8
{-# INLINE int8DuckValue #-}

int16DuckValue :: Int16 -> IO DuckDBValue
int16DuckValue = c_duckdb_create_int16
{-# INLINE int16DuckValue #-}

int32DuckValue :: Int32 -> IO DuckDBValue
int32DuckValue = c_duckdb_create_int32
{-# INLINE int32DuckValue #-}

int64DuckValue :: Int64 -> IO DuckDBValue
int64DuckValue = c_duckdb_create_int64
{-# INLINE int64DuckValue #-}

uint64DuckValue :: Word64 -> IO DuckDBValue
uint64DuckValue = c_duckdb_create_uint64
{-# INLINE uint64DuckValue #-}

uint32DuckValue :: Word32 -> IO DuckDBValue
uint32DuckValue = c_duckdb_create_uint32
{-# INLINE uint32DuckValue #-}

uint16DuckValue :: Word16 -> IO DuckDBValue
uint16DuckValue = c_duckdb_create_uint16
{-# INLINE uint16DuckValue #-}

uint8DuckValue :: Word8 -> IO DuckDBValue
uint8DuckValue = c_duckdb_create_uint8
{-# INLINE uint8DuckValue #-}

doubleDuckValue :: Double -> IO DuckDBValue
doubleDuckValue = c_duckdb_create_double . CDouble
{-# INLINE doubleDuckValue #-}

floatDuckValue :: Float -> IO DuckDBValue
floatDuckValue = c_duckdb_create_float . CFloat . realToFrac
{-# INLINE floatDuckValue #-}

textDuckValue :: Text -> IO DuckDBValue
textDuckValue txt =
    BS.useAsCStringLen (TextEncoding.encodeUtf8 txt) \(ptr, len) ->
        c_duckdb_create_varchar_length ptr (fromIntegral len)
{-# INLINE textDuckValue #-}

stringDuckValue :: String -> IO DuckDBValue
stringDuckValue = textDuckValue . Text.pack
{-# INLINE stringDuckValue #-}

blobDuckValue :: BS.ByteString -> IO DuckDBValue
blobDuckValue bs =
    BS.useAsCStringLen bs \(ptr, len) ->
        c_duckdb_create_blob (castPtr ptr :: Ptr Word8) (fromIntegral len)
{-# INLINE blobDuckValue #-}


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

dayToDateStruct :: Day -> Maybe DuckDBDateStruct
dayToDateStruct day | day < YearMonthDay (-5877642) 06 25 = Nothing
dayToDateStruct day | day > YearMonthDay 5881580 07 10 = Nothing
dayToDateStruct day =
    let (year, month, dayOfMonth) = toGregorian day
     in Just $ DuckDBDateStruct
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
    totalMicros <- timeOfDayUnits 1000000 timeWithZoneTime
    let offsetSeconds = toInteger (timeZoneMinutes timeWithZoneZone) * 60
    when (abs offsetSeconds > 57599) $
        throwIO (userError "duckdb-simple: TIME WITH TIME ZONE offset out of range")
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
