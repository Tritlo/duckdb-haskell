{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | Conversions from DuckDB's native date and time values. Result decoding and
the VARIANT decoder share them.
-}
module Database.DuckDB.Simple.Temporal (
    decodeDuckDBDate,
    decodeDuckDBTime,
    decodeDuckDBTimeNs,
    decodeDuckDBTimeTz,
    decodeDuckDBTimestamp,
    decodeDuckDBTimestampSeconds,
    decodeDuckDBTimestampMilliseconds,
    decodeDuckDBTimestampNanoseconds,
    decodeDuckDBTimestampUTCTime,
) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Int (Int64)
import Data.Ratio ((%))
import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeOfDay (..), minutesToTimeZone, utc, utcToLocalTime)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (TimeWithZone (..))
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

-- | Decode dates with exact epoch arithmetic and preserve infinity.
decodeDuckDBDate :: DuckDBDate -> IO Date
decodeDuckDBDate (DuckDBDate days) =
    pure (decodeUnbounded (\value -> addDays (toInteger value) (fromGregorian 1970 1 1)) days)

decodeDuckDBTime :: DuckDBTime -> IO TimeOfDay
decodeDuckDBTime raw =
    alloca $ \ptr -> do
        c_duckdb_from_time raw ptr
        timeStruct <- peek ptr
        pure (timeStructToTimeOfDay timeStruct)

decodeDuckDBTimestamp :: DuckDBTimestamp -> IO LocalTimestamp
decodeDuckDBTimestamp (DuckDBTimestamp micros) = decodeTimestampUnits 1000000 micros

-- | Interpret native infinity sentinels before converting a finite payload.
decodeUnbounded :: (Integral a, Bounded a) => (a -> b) -> a -> Unbounded b
decodeUnbounded decode value
    | value == maxBound = PosInfinity
    | value == negate maxBound = NegInfinity
    | otherwise = Finite (decode value)

-- | Decode timestamp units without overflowing an intermediate Int64.
decodeTimestampUnits :: Integer -> Int64 -> IO LocalTimestamp
decodeTimestampUnits units =
    pure . decodeUnbounded (utcToLocalTime utc . posixSecondsToUTCTime . fromRational . (% units) . toInteger)

decodeDuckDBTimeNs :: DuckDBTimeNs -> TimeOfDay
decodeDuckDBTimeNs (DuckDBTimeNs nanos) =
    let (hours, remainderHours) = nanos `divMod` (60 * 60 * 1000000000)
        (minutes, remainderMinutes) = remainderHours `divMod` (60 * 1000000000)
        (seconds, fractionalNanos) = remainderMinutes `divMod` 1000000000
        fractional = fromRational (toInteger fractionalNanos % 1000000000)
        totalSeconds = fromIntegral seconds + fractional
     in TimeOfDay
            (fromIntegral hours)
            (fromIntegral minutes)
            totalSeconds

decodeDuckDBTimeTz :: DuckDBTimeTz -> IO TimeWithZone
decodeDuckDBTimeTz raw =
    alloca $ \ptr -> do
        c_duckdb_from_time_tz raw ptr
        DuckDBTimeTzStruct{duckDBTimeTzStructTime = timeStruct, duckDBTimeTzStructOffset = offset} <- peek ptr
        when (offset `rem` 60 /= 0) $
            throwIO (userError "duckdb-simple: TIMETZ offset cannot be represented in whole minutes")
        let timeOfDay = timeStructToTimeOfDay timeStruct
            minutes = fromIntegral offset `div` 60
            zone = minutesToTimeZone minutes
        pure TimeWithZone{timeWithZoneTime = timeOfDay, timeWithZoneZone = zone}

decodeDuckDBTimestampSeconds :: DuckDBTimestampS -> IO LocalTimestamp
decodeDuckDBTimestampSeconds (DuckDBTimestampS seconds) =
    decodeTimestampUnits 1 seconds

decodeDuckDBTimestampMilliseconds :: DuckDBTimestampMs -> IO LocalTimestamp
decodeDuckDBTimestampMilliseconds (DuckDBTimestampMs millis) =
    decodeTimestampUnits 1000 millis

decodeDuckDBTimestampNanoseconds :: DuckDBTimestampNs -> IO LocalTimestamp
decodeDuckDBTimestampNanoseconds (DuckDBTimestampNs nanos) = decodeTimestampUnits 1000000000 nanos

decodeDuckDBTimestampUTCTime :: DuckDBTimestamp -> IO UTCTimestamp
decodeDuckDBTimestampUTCTime (DuckDBTimestamp micros) =
    pure (decodeUnbounded (posixSecondsToUTCTime . fromRational . (% 1000000) . toInteger) micros)

timeStructToTimeOfDay :: DuckDBTimeStruct -> TimeOfDay
timeStructToTimeOfDay DuckDBTimeStruct{duckDBTimeStructHour, duckDBTimeStructMinute, duckDBTimeStructSecond, duckDBTimeStructMicros} =
    let secondsInt = fromIntegral duckDBTimeStructSecond :: Integer
        micros = fromIntegral duckDBTimeStructMicros :: Integer
        fractional = fromRational (micros % 1000000)
        totalSeconds = fromInteger secondsInt + fractional
     in TimeOfDay
            (fromIntegral duckDBTimeStructHour)
            (fromIntegral duckDBTimeStructMinute)
            totalSeconds
