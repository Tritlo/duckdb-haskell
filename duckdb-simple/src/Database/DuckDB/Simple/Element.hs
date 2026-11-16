{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Database.DuckDB.Simple.Element
-- Description : Decoders for one native DuckDB element in vector memory.
--
-- Result decoding uses them for the types whose element needs no logical type
-- metadata.
module Database.DuckDB.Simple.Element (
  arraySizeReason,
  chunkIsRowValid,
  containsType,
  decodeElement,
  duckDBHugeIntToInteger,
  maxArraySize,
  timeTzMaxOffsetSeconds,
  timeTzOffsetBits,
  valueTypeName,
  vectorElementType,
  withVectorType,
) where

import Control.Exception (bracket, fromException, throwIO, try)
import Data.Bits (clearBit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Int (Int32, Int64)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeOfDay (..), utc, utcToLocalTime)
import qualified Data.UUID as UUID
import Data.Void (Void)
import Data.Word (Word64, Word8)
import Database.DuckDB.Simple.FromField (
  BigNum (..),
  BitString (..),
  Field (..),
  FieldValue (..),
  IntervalValue (..),
  ResultError (..),
  TimeWithZone (..),
  returnError,
 )
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), UnionMemberType (..))
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..), peekElemOff)
import GHC.Exts (Ptr (..), Word (W#))
import GHC.Num.Integer (integerFromAddr)
import GHC.Records (getField)

-- | The largest size of a DuckDB ARRAY: @ArrayType::MAX_ARRAY_SIZE@ in
-- @src/include/duckdb/common/types.hpp@ of DuckDB 2.0.0. See
-- Note [Sized ARRAY bounds].
maxArraySize :: Int
maxArraySize = 100000

-- | Give the reason why a size has no DuckDB ARRAY type, or 'Nothing' for a
-- valid size. See Note [Sized ARRAY bounds].
arraySizeReason :: Integer -> Maybe Text
arraySizeReason size
  | size < 1 = Just "a DuckDB ARRAY has at least one element"
  | size > toInteger maxArraySize = Just ("a DuckDB ARRAY has at most " <> Text.pack (show maxArraySize) <> " elements")
  | otherwise = Nothing

{- Note [Sized ARRAY bounds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A DuckDB ARRAY has 1 to maxArraySize elements. Its value API does not check
this limit, so both logical type construction and value binding must check
the size. Generic declarations check each ARRAY, also below an alias or a
container.

SizedArray has a type-level Nat, but LogicalTypeArray stores a Word64. The
generic logical type saturates a Nat above maxBound Word64 to that bound.
This is an invalid-size sentinel. It cannot wrap into a valid size, and the
declaration and native construction checks reject it. No pure conversion
throws an exception.
-}

{- Note [Vector memory layouts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some readers and writers use the memory of a vector directly. These are the
layouts that they use.

The validity mask. Each uint64_t word covers 64 rows: bit N of word W is row
W*64+N, and a set bit means valid. The header gives this layout in the comment
of duckdb_v2_vector_flat_get_validity_mutable.

- Read (chunkIsRowValid). The V2 API has no call that reads the validity of
  one row, so the reader reads the mask memory from duckdb_v2_vector_get_view.
  The header does not say what a null mask pointer means. In DuckDB 2.0.0 a
  null pointer means that the vector has no mask, and then every row is valid.

- Write (markValid and markInvalid). The writer sets or clears one bit of the
  mask from duckdb_v2_vector_flat_get_validity_mutable. The header says that
  this call allocates the mask on first use, so the pointer is never null. The
  header also says that a raw mask write marks only the rows of this vector,
  and that a NULL STRUCT or ARRAY row needs duckdb_v2_vector_set_null. The
  writer writes a mask bit only for a raw result type. A NULL of each other
  result type goes through duckdb_v2_vector_set_value. See
  Note [Result writers] in "Database.DuckDB.Simple.Function.Internal".

The bytes slot. A VARCHAR, BLOB, BIT, or BIGNUM element is a 16-byte
duckdb_v2_bytes value. The byte length is a uint32_t at byte 0. A value of at
most DUCKDB_V2_BYTES_INLINE_LENGTH (12) bytes follows the length, at byte 4.
For a longer value, the slot holds the first 4 bytes of the value at byte 4 and
the address of the value at byte 8. The header defines the slot in the comment
of struct duckdb_v2_bytes.

- Read (chunkDecodeBlob). The reader reads the generated fields of the union.

- Write (writeArenaText). The writer needs no factory. It writes the length and
  the bytes through the generated field pointers, as the reader reads them. A
  longer value goes to the arena of the vector.

The storage of a DECIMAL or ENUM element depends on the type (decimalStorage and
enumStorage). The header of duckdb_v2_vector_get_view gives both mappings.
-}

-- | Read a validity bit of a mask from @duckdb_v2_vector_get_view@. A null
-- mask pointer gives 'True' for each row. See Note [Vector memory layouts].
chunkIsRowValid :: Ptr Word64 -> DuckDBIdx -> IO Bool
chunkIsRowValid validity row
  | validity == nullPtr = pure True
  | otherwise = (\word -> testBit word (fromIntegral (row `rem` 64))) <$> peekElemOff validity (fromIntegral (row `div` 64))

-- | Copy one generated V2 byte value, including interior NUL bytes. See
-- Note [Vector memory layouts].
chunkDecodeBlob :: Ptr Void -> DuckDBIdx -> IO BS.ByteString
chunkDecodeBlob dataPtr row = do
  -- Read each field through its generated field pointer. A getField on the
  -- union value calls the bindings package. That call copies the union.
  let ptr = advancePtr @DuckDBV2Bytes (castPtr dataPtr) (fromIntegral row)
      value = getField @"value" ptr
  len <- peek (getField @"length" (getField @"inlined" value))
  source <-
    if len <= fromIntegral dUCKDB_V2_BYTES_INLINE_LENGTH
      then pure (coerce (getField @"inlined" (getField @"inlined" value)))
      else peek (getField @"ptr" (getField @"pointer" value))
  if len == 0 then pure BS.empty else BS.packCStringLen (source, fromIntegral len)

-- | Decode DuckDB's BIT bytes: a padding count, then the data bytes. Clear the
-- unused high bits of the first data byte, which DuckDB sets.
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

-- | Decode the element at an index of a vector's data. The type must not need
-- logical type metadata: DECIMAL, ENUM, GEOMETRY, and nested types use their own
-- decoders. The caller checks validity.
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
  DuckDBV2LogicalTypeIdTimeTz -> FieldTimeTZ . decodeDuckDBTimeTz <$> peekElemOff (castPtr dataPtr) rowIdx
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
    throwInternalError (Text.pack "INVALID type in eager result")
  DuckDBV2LogicalTypeIdAny ->
    throwInternalError (Text.pack "ANY columns should not appear in results")
  other ->
    throwInternalError (Text.pack ("UNKNOWN type in eager result: " <> show other))
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
  in  TimeOfDay (fromIntegral hours) (fromIntegral minutes) (fromRational (toInteger remMinutes % toInteger units))

decodeDuckDBTimeNs :: Int64 -> TimeOfDay
decodeDuckDBTimeNs = decodeTimeUnits 1000000000

-- | The largest UTC offset of a TIMETZ value in seconds: 15:59:59. DuckDB 2.0.0
-- defines it as @dtime_tz_t::MAX_OFFSET@ in
-- @src/include/duckdb/common/types/datetime.hpp@. The low 24 bits of a TIMETZ
-- value hold this maximum minus the offset (@encode_offset@ and
-- @decode_offset@ in that file). The V2 header says only "micros in the high
-- 40 bits, a biased offset in the low 24" (the comment of
-- @duckdb_v2_value_get_time_tz@). The header does not give the value of the
-- bias.
timeTzMaxOffsetSeconds :: Int
timeTzMaxOffsetSeconds = 16 * 60 * 60 - 1

-- | The number of low bits of a TIMETZ value that hold the biased offset. The
-- high 40 bits hold the time of day in microseconds. DuckDB 2.0.0 names the two
-- widths @dtime_tz_t::OFFSET_BITS@ and @dtime_tz_t::TIME_BITS@.
timeTzOffsetBits :: Int
timeTzOffsetBits = 24

-- | Decode the time of day and the offset in seconds of a TIMETZ value.
decodeDuckDBTimeTz :: Word64 -> TimeWithZone
decodeDuckDBTimeTz raw =
  TimeWithZone
    { timeWithZoneTime = decodeTimeUnits 1000000 (fromIntegral (raw `shiftR` timeTzOffsetBits))
    , timeWithZoneOffsetSeconds = fromIntegral (timeTzMaxOffsetSeconds - fromIntegral (raw .&. (1 `shiftL` timeTzOffsetBits - 1)))
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
      finish magnitude = do
        count <- peek lengthSlot
        negative <- peek signSlot
        value <- bigEndianInteger magnitude count
        pure $! if negative == 0 then value else negate value
  -- A buffer of the storage size usually holds the magnitude. If the buffer is
  -- too short, DuckDB gives the required size, and a second call decodes the value.
  allocaBytes len \first -> do
    outcome <- try (decode first (fromIntegral len))
    case outcome of
      Right () -> finish first
      Left err
        | sqlErrorType err == Just ErrorInputObjectSize -> do
            count <- peek lengthSlot
            allocaBytes (fromIntegral count) \magnitude -> decode magnitude count >> finish magnitude
        | otherwise -> throwIO err

-- | Read an unsigned big-endian magnitude in linear time.
bigEndianInteger :: Ptr Word8 -> DuckDBIdx -> IO Integer
bigEndianInteger (Ptr addr) count = case fromIntegral count of W# size -> integerFromAddr size addr 1#

-- | The SQL type name of a decoded value, as each read error gives it. An error
-- text names this type, not the value, because a value can be large. The error
-- of 'returnError' holds that name.
valueTypeName :: FieldValue -> Text
valueTypeName value = case returnError Incompatible (Field Text.empty 0 value (LogicalTypeScalar LogicalTypeIdVariant)) Text.empty :: Ok () of
  Errors (err : _) | Just result <- fromException err -> errSQLType result
  _ -> Text.pack "an unknown type"

-- | Test whether a type is or contains a type that the predicate accepts. The
-- test visits the element type of a LIST and an ARRAY, the key type and the
-- value type of a MAP, the field types of a STRUCT and a TUPLE, the member
-- types of a UNION, and the base type of an alias. See Note [Type aliases] in
-- "Database.DuckDB.Simple.LogicalRep".
containsType :: (LogicalTypeRep -> Bool) -> LogicalTypeRep -> Bool
containsType accepts rep =
  accepts rep || case rep of
    LogicalTypeAlias _ base -> containsType accepts base
    LogicalTypeList child -> containsType accepts child
    LogicalTypeArray child _ -> containsType accepts child
    LogicalTypeMap key value -> containsType accepts key || containsType accepts value
    LogicalTypeStruct fields -> any (containsType accepts . structFieldValue) fields
    LogicalTypeTuple fields -> any (containsType accepts) fields
    LogicalTypeUnion members -> any (containsType accepts . unionMemberType) members
    _ -> False
