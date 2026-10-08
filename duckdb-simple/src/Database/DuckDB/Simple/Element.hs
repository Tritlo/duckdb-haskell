{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | Decoders for one native DuckDB element in vector memory. Result decoding
uses them for the types whose element needs no logical type metadata.
-}
module Database.DuckDB.Simple.Element (
    chunkIsRowValid,
    bitStringFromBytes,
    chunkDecodeBlob,
    decodeElement,
    duckDBHugeIntToInteger,
    vectorElementType,
    withVectorType,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import Data.Bits (clearBit, shiftL, xor, (.|.))
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Ratio ((%))
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeOfDay (..), minutesToTimeZone, utc, utcToLocalTime)
import qualified Data.UUID as UUID
import Data.Void (Void)
import Data.Word (Word64, Word8)
import Database.DuckDB.FFI.Compat
import Database.DuckDB.Simple.FromField (
    BigNum (..),
    BitString (..),
    FieldValue (..),
    IntervalValue (..),
    TimeWithZone (..),
    fromBigNumBytes,
 )
import Database.DuckDB.Simple.LogicalRep (destroyLogicalType)
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peekElemOff)

chunkIsRowValid :: Ptr Word64 -> DuckDBIdx -> IO Bool
chunkIsRowValid validity rowIdx
    | validity == nullPtr = pure True
    | otherwise = do
        CBool flag <- c_duckdb_validity_row_is_valid validity rowIdx
        pure (flag /= 0)

chunkDecodeBlob :: Ptr Void -> DuckDBIdx -> IO BS.ByteString
chunkDecodeBlob dataPtr rowIdx = do
    let base = castPtr dataPtr :: Ptr Word8
        offset = fromIntegral rowIdx * duckdbStringTSize
        stringPtr = castPtr (base `plusPtr` offset) :: Ptr DuckDBStringT
    len <- peek stringPtr >>= c_duckdb_string_t_length
    if len == 0
        then pure BS.empty
        else do
            ConstPtr ptr <- c_duckdb_string_t_data stringPtr
            BS.packCStringLen (ptr, fromIntegral len)

duckdbStringTSize :: Int
duckdbStringTSize = sizeOf (undefined :: DuckDBStringT)

{- | Decode DuckDB's BIT bytes: a padding count, then the data bytes. Clear the
unused high bits of the first data byte, which DuckDB sets.
-}
bitStringFromBytes :: BS.ByteString -> BitString
bitStringFromBytes bytes = case BS.unpack bytes of
    [] -> BitString 0 BS.empty
    [padding] -> BitString padding BS.empty
    padding : first : rest ->
        BitString padding (BS.pack (foldl clearBit first [8 - fromIntegral padding .. 7] : rest))

-- | Borrow the logical type of a vector. The type is destroyed after the action.
withVectorType :: DuckDBVector -> (DuckDBLogicalType -> IO a) -> IO a
withVectorType vector = bracket (c_duckdb_vector_get_column_type vector) destroyLogicalType

vectorElementType :: DuckDBVector -> IO DUCKDB_TYPE
vectorElementType vec =
    bracket (c_duckdb_vector_get_column_type vec) destroyLogicalType (fmap (\(DuckDBType dtype) -> dtype) . c_duckdb_get_type_id)

{- | Decode the element at an index of a vector's data. The type must not need
logical type metadata: DECIMAL, ENUM, GEOMETRY, and nested types use their own
decoders. The caller checks validity.
-}
decodeElement :: DUCKDB_TYPE -> Ptr Void -> Int -> IO FieldValue
decodeElement dtype dataPtr rowIdx = case dtype of
    DUCKDB_TYPE_BOOLEAN -> do
        raw <- peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
        pure (FieldBool (raw /= 0))
    DUCKDB_TYPE_TINYINT -> FieldInt8 <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_SMALLINT -> FieldInt16 <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_INTEGER -> FieldInt32 <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_BIGINT -> FieldInt64 <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_UTINYINT -> FieldWord8 <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_USMALLINT -> FieldWord16 <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_UINTEGER -> FieldWord32 <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_UBIGINT -> FieldWord64 <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_FLOAT -> FieldFloat <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_DOUBLE -> FieldDouble <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_VARCHAR -> FieldText . TextEncoding.decodeUtf8 <$> chunkDecodeBlob dataPtr index
    DUCKDB_TYPE_STRING_LITERAL -> FieldText . TextEncoding.decodeUtf8 <$> chunkDecodeBlob dataPtr index
    DUCKDB_TYPE_BLOB -> FieldBlob <$> chunkDecodeBlob dataPtr index
    DUCKDB_TYPE_UUID -> do
        DuckDBUHugeInt lower upperBiased <- peekElemOff (castPtr dataPtr :: Ptr DuckDBUHugeInt) rowIdx
        let upper = upperBiased `xor` (0x8000000000000000 :: Word64)
        pure (FieldUUID (UUID.fromWords64 (fromIntegral upper) lower))
    DUCKDB_TYPE_DATE -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldDate . decodeDuckDBDate
    DUCKDB_TYPE_TIME -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTime . decodeDuckDBTime
    DUCKDB_TYPE_TIME_NS -> FieldTime . decodeDuckDBTimeNs <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_TIME_TZ -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimeTZ . decodeDuckDBTimeTz
    DUCKDB_TYPE_TIMESTAMP -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestamp
    DUCKDB_TYPE_TIMESTAMP_S -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestampSeconds
    DUCKDB_TYPE_TIMESTAMP_MS -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestampMilliseconds
    DUCKDB_TYPE_TIMESTAMP_NS -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestampNanoseconds
    DUCKDB_TYPE_TIMESTAMP_TZ -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestampTZ . decodeDuckDBTimestampUTCTime
    DUCKDB_TYPE_INTERVAL -> FieldInterval . intervalValueFromDuckDB <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_HUGEINT -> FieldHugeInt . duckDBHugeIntToInteger <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_UHUGEINT -> FieldUHugeInt . duckDBUHugeIntToInteger <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_BIT -> FieldBit . bitStringFromBytes <$> chunkDecodeBlob dataPtr index
    DUCKDB_TYPE_BIGNUM -> do
        bytes <- chunkDecodeBlob dataPtr index
        pure (FieldBigNum (BigNum (if BS.length bytes < 3 then 0 else fromBigNumBytes (BS.unpack bytes))))
    DUCKDB_TYPE_INTEGER_LITERAL -> FieldInt64 <$> peekElemOff (castPtr dataPtr) rowIdx
    DUCKDB_TYPE_INVALID ->
        error "duckdb-simple: INVALID type in eager result"
    DUCKDB_TYPE_ANY ->
        error "duckdb-simple: ANY columns should not appear in results"
    other ->
        error ("duckdb-simple: UNKNOWN type in eager result: " <> show other)
  where
    index = fromIntegral rowIdx :: DuckDBIdx

-- | Decode dates with exact epoch arithmetic and preserve infinity.
decodeDuckDBDate :: DuckDBDate -> IO Date
decodeDuckDBDate (DuckDBDate days) =
    pure (decodeUnbounded (\value -> addDays (toInteger value) (fromGregorian 1970 1 1)) days)

decodeDuckDBTime :: DuckDBTime -> IO TimeOfDay
decodeDuckDBTime raw = timeStructToTimeOfDay <$> c_duckdb_from_time raw

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
decodeDuckDBTimeTz raw = do
    DuckDBTimeTzStruct timeStruct offset <- c_duckdb_from_time_tz raw
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

intervalValueFromDuckDB :: DuckDBInterval -> IntervalValue
intervalValueFromDuckDB (DuckDBInterval months days micros) =
    IntervalValue
        { intervalMonths = months
        , intervalDays = days
        , intervalMicros = micros
        }

duckDBHugeIntToInteger :: DuckDBHugeInt -> Integer
duckDBHugeIntToInteger (DuckDBHugeInt lower upper) =
    (fromIntegral upper `shiftL` 64) .|. fromIntegral lower

duckDBUHugeIntToInteger :: DuckDBUHugeInt -> Integer
duckDBUHugeIntToInteger (DuckDBUHugeInt lower upper) =
    (fromIntegral upper `shiftL` 64) .|. fromIntegral lower

timeStructToTimeOfDay :: DuckDBTimeStruct -> TimeOfDay
timeStructToTimeOfDay (DuckDBTimeStruct hour minute second micros) =
    let secondsInt = fromIntegral second :: Integer
        fractional = fromRational (toInteger micros % 1000000)
        totalSeconds = fromInteger secondsInt + fractional
     in TimeOfDay
            (fromIntegral hour)
            (fromIntegral minute)
            totalSeconds
