{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Bits (clearBit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Int (Int32, Int64)
import Data.Ratio ((%))
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeOfDay (..), minutesToTimeZone, utc, utcToLocalTime)
import qualified Data.UUID as UUID
import Data.Void (Void)
import Data.Word (Word64, Word8)
import Database.DuckDB.Simple.FromField (
    BigNum (..),
    BitString (..),
    FieldValue (..),
    IntervalValue (..),
    TimeWithZone (..),
 )
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peekElemOff)
import GHC.Records (getField)

-- | Read a validity bit. A null mask means that every row is valid.
chunkIsRowValid :: Ptr Word64 -> DuckDBIdx -> IO Bool
chunkIsRowValid validity row
    | validity == nullPtr = pure True
    | otherwise = (\word -> testBit word (fromIntegral (row `rem` 64))) <$> peekElemOff validity (fromIntegral (row `div` 64))

-- | Copy one generated V2 byte value, including interior NUL bytes.
chunkDecodeBlob :: Ptr Void -> DuckDBIdx -> IO BS.ByteString
chunkDecodeBlob dataPtr row = do
    let ptr = castPtr dataPtr `plusPtr` (fromIntegral row * sizeOf (undefined :: DuckDBV2Bytes)) :: Ptr DuckDBV2Bytes
    bytes <- peek ptr
    let payload = getField @"value" bytes
        len = getField @"length" (getField @"inlined" payload)
        source =
            if len <= 12
                then coerce (getField @"inlined" (getField @"inlined" (getField @"value" ptr)))
                else getField @"ptr" (getField @"pointer" payload)
    if len == 0 then pure BS.empty else BS.packCStringLen (source, fromIntegral len)

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
withVectorType :: DuckDBV2VectorHandle -> (DuckDBV2LogicalTypeHandle -> IO a) -> IO a
withVectorType vector = bracket (output (c_duckdb_v2_vector_get_logical_type vector)) destroyLogicalType

-- | Read the native logical type id of a vector.
vectorElementType :: DuckDBV2VectorHandle -> IO DuckDBV2LogicalTypeId
vectorElementType vector = withVectorType vector (output . c_duckdb_v2_logical_type_get_id)

{- | Decode the element at an index of a vector's data. The type must not need
logical type metadata: DECIMAL, ENUM, GEOMETRY, and nested types use their own
decoders. The caller checks validity.
-}
decodeElement :: DuckDBV2LogicalTypeId -> Ptr Void -> Int -> IO FieldValue
decodeElement dtype dataPtr rowIdx = case dtype of
    DuckDBV2LogicalTypeIdBoolean -> do
        raw <- peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
        pure (FieldBool (raw /= 0))
    DuckDBV2LogicalTypeIdTinyInt -> FieldInt8 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdSmallInt -> FieldInt16 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdInteger -> FieldInt32 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdBigInt -> FieldInt64 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdUTinyInt -> FieldWord8 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdUSmallInt -> FieldWord16 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdUInteger -> FieldWord32 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdUBigInt -> FieldWord64 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdFloat -> FieldFloat <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdDouble -> FieldDouble <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdVarchar -> FieldText . TextEncoding.decodeUtf8 <$> chunkDecodeBlob dataPtr index
    DuckDBV2LogicalTypeIdBlob -> FieldBlob <$> chunkDecodeBlob dataPtr index
    DuckDBV2LogicalTypeIdUUID -> do
        DuckDBV2HugeintT lower upperBiased <- peekElemOff (castPtr dataPtr :: Ptr DuckDBV2HugeintT) rowIdx
        let upper = fromIntegral upperBiased `xor` (0x8000000000000000 :: Word64)
        pure (FieldUUID (UUID.fromWords64 (fromIntegral upper) lower))
    DuckDBV2LogicalTypeIdDate -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldDate . decodeDuckDBDate
    DuckDBV2LogicalTypeIdTime -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTime . decodeDuckDBTime
    DuckDBV2LogicalTypeIdTimeNs -> FieldTime . decodeDuckDBTimeNs <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdTimeTz -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimeTZ . decodeDuckDBTimeTz
    DuckDBV2LogicalTypeIdTimestamp -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestamp
    DuckDBV2LogicalTypeIdTimestampSec -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestampSeconds
    DuckDBV2LogicalTypeIdTimestampMs -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestampMilliseconds
    DuckDBV2LogicalTypeIdTimestampNs -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestampNanoseconds
    DuckDBV2LogicalTypeIdTimestampTz -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestampTZ . decodeDuckDBTimestampUTCTime
    DuckDBV2LogicalTypeIdTimestampTzNs -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestampTZ . decodeDuckDBTimestampNsUTCTime
    DuckDBV2LogicalTypeIdInterval -> FieldInterval . intervalValueFromDuckDB <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdHugeInt -> FieldHugeInt . duckDBHugeIntToInteger <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdUHugeInt -> FieldUHugeInt . duckDBUHugeIntToInteger <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBV2LogicalTypeIdBit -> FieldBit . bitStringFromBytes <$> chunkDecodeBlob dataPtr index
    DuckDBV2LogicalTypeIdBigNum -> do
        bytes <- chunkDecodeBlob dataPtr index
        FieldBigNum . BigNum <$> decodeBigNum bytes
    DuckDBV2LogicalTypeIdInvalid ->
        error "duckdb-simple: INVALID type in eager result"
    DuckDBV2LogicalTypeIdAny ->
        error "duckdb-simple: ANY columns should not appear in results"
    other ->
        error ("duckdb-simple: UNKNOWN type in eager result: " <> show other)
  where
    index = fromIntegral rowIdx :: DuckDBIdx

-- | Decode dates with exact epoch arithmetic and preserve infinity.
decodeDuckDBDate :: Int32 -> IO Date
decodeDuckDBDate = pure . decodeUnbounded (\value -> addDays (toInteger value) (fromGregorian 1970 1 1))

decodeDuckDBTime :: Int64 -> IO TimeOfDay
decodeDuckDBTime = pure . decodeTimeUnits 1000000

decodeDuckDBTimestamp :: Int64 -> IO LocalTimestamp
decodeDuckDBTimestamp = decodeTimestampUnits 1000000

-- | Interpret native infinity sentinels before converting a finite payload.
decodeUnbounded :: (Integral a, Bounded a) => (a -> b) -> a -> Unbounded b
decodeUnbounded decode value
    | value == maxBound = PosInfinity
    | value == negate maxBound = NegInfinity
    | otherwise = Finite (decode value)

-- | Decode timestamp units without overflowing an intermediate Int64.
decodeTimestampUnits :: Integer -> Int64 -> IO LocalTimestamp
decodeTimestampUnits units = pure . decodeUnbounded (utcToLocalTime utc . posixSecondsToUTCTime . fromRational . (% units) . toInteger)

-- | Decode raw time units with exact fractional seconds.
decodeTimeUnits :: Int64 -> Int64 -> TimeOfDay
decodeTimeUnits units value =
    let (hours, remHours) = value `divMod` (3600 * units)
        (minutes, remMinutes) = remHours `divMod` (60 * units)
     in TimeOfDay (fromIntegral hours) (fromIntegral minutes) (fromRational (toInteger remMinutes % toInteger units))

decodeDuckDBTimeNs :: Int64 -> TimeOfDay
decodeDuckDBTimeNs = decodeTimeUnits 1000000000

decodeDuckDBTimeTz :: Word64 -> IO TimeWithZone
decodeDuckDBTimeTz raw = do
    let offset = 57599 - fromIntegral (raw .&. 0xFFFFFF) :: Int
    when (offset `rem` 60 /= 0) $ throwIO (userError "duckdb-simple: TIMETZ offset cannot be represented in whole minutes")
    pure
        TimeWithZone
            { timeWithZoneTime = decodeTimeUnits 1000000 (fromIntegral (raw `shiftR` 24))
            , timeWithZoneZone = minutesToTimeZone (offset `div` 60)
            }

decodeDuckDBTimestampSeconds :: Int64 -> IO LocalTimestamp
decodeDuckDBTimestampSeconds = decodeTimestampUnits 1

decodeDuckDBTimestampMilliseconds :: Int64 -> IO LocalTimestamp
decodeDuckDBTimestampMilliseconds = decodeTimestampUnits 1000

decodeDuckDBTimestampNanoseconds :: Int64 -> IO LocalTimestamp
decodeDuckDBTimestampNanoseconds = decodeTimestampUnits 1000000000

decodeDuckDBTimestampUTCTime :: Int64 -> IO UTCTimestamp
decodeDuckDBTimestampUTCTime = pure . decodeUnbounded (posixSecondsToUTCTime . fromRational . (% 1000000) . toInteger)

-- | Decode UTC nanoseconds and preserve infinity.
decodeDuckDBTimestampNsUTCTime :: Int64 -> IO UTCTimestamp
decodeDuckDBTimestampNsUTCTime = pure . decodeUnbounded (posixSecondsToUTCTime . fromRational . (% 1000000000) . toInteger)

intervalValueFromDuckDB :: DuckDBV2IntervalT -> IntervalValue
intervalValueFromDuckDB (DuckDBV2IntervalT months days micros) = IntervalValue months days micros

duckDBHugeIntToInteger :: DuckDBV2HugeintT -> Integer
duckDBHugeIntToInteger (DuckDBV2HugeintT lower upper) = (fromIntegral upper `shiftL` 64) .|. fromIntegral lower

duckDBUHugeIntToInteger :: DuckDBV2UhugeintT -> Integer
duckDBUHugeIntToInteger (DuckDBV2UhugeintT lower upper) = (fromIntegral upper `shiftL` 64) .|. fromIntegral lower

-- | Decode opaque BIGNUM storage through the V2 magnitude interface.
decodeBigNum :: BS.ByteString -> IO Integer
decodeBigNum bytes = BS.useAsCStringLen bytes \(raw, len) -> alloca \lengthSlot -> alloca \signSlot -> do
    let decode ptr capacity = checked (c_duckdb_v2_bignum_decode (ConstPtr (castPtr raw)) (fromIntegral len) ptr capacity lengthSlot signSlot)
    decode nullPtr 0
    count <- peek lengthSlot
    allocaBytes (fromIntegral count) \magnitude -> do
        decode magnitude count
        digits <- BS.packCStringLen (castPtr magnitude, fromIntegral count)
        negative <- peek signSlot
        let value = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0 digits
        pure (if negative == 0 then value else negate value)
