{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | Decoders for one native DuckDB element in vector memory. Result decoding
uses them for the types whose element needs no logical type metadata.
-}
module Database.DuckDB.Simple.Element (
    chunkIsRowValid,
    chunkDecodeBlob,
    decodeElement,
    duckDBHugeIntToInteger,
    vectorElementType,
    withVectorType,
) where

import Control.Exception (bracket)
import Data.Bits (clearBit, shiftL, xor, (.|.))
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.UUID as UUID
import Data.Word (Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (
    BigNum (..),
    BitString (..),
    FieldValue (..),
    IntervalValue (..),
    fromBigNumBytes,
 )
import Database.DuckDB.Simple.LogicalRep (destroyLogicalType)
import Database.DuckDB.Simple.Temporal (
    decodeDuckDBDate,
    decodeDuckDBTime,
    decodeDuckDBTimeNs,
    decodeDuckDBTimeTz,
    decodeDuckDBTimestamp,
    decodeDuckDBTimestampMilliseconds,
    decodeDuckDBTimestampNanoseconds,
    decodeDuckDBTimestampSeconds,
    decodeDuckDBTimestampUTCTime,
 )
import Foreign.C.Types (CBool (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peekElemOff)

chunkIsRowValid :: Ptr Word64 -> DuckDBIdx -> IO Bool
chunkIsRowValid validity rowIdx
    | validity == nullPtr = pure True
    | otherwise = do
        CBool flag <- c_duckdb_validity_row_is_valid validity rowIdx
        pure (flag /= 0)

chunkDecodeBlob :: Ptr () -> DuckDBIdx -> IO BS.ByteString
chunkDecodeBlob dataPtr rowIdx = do
    let base = castPtr dataPtr :: Ptr Word8
        offset = fromIntegral rowIdx * duckdbStringTSize
        stringPtr = castPtr (base `plusPtr` offset) :: Ptr DuckDBStringT
    len <- c_duckdb_string_t_length stringPtr
    if len == 0
        then pure BS.empty
        else do
            ptr <- c_duckdb_string_t_data stringPtr
            BS.packCStringLen (ptr, fromIntegral len)

duckdbStringTSize :: Int
duckdbStringTSize = 16

-- | Borrow the logical type of a vector. The type is destroyed after the action.
withVectorType :: DuckDBVector -> (DuckDBLogicalType -> IO a) -> IO a
withVectorType vector = bracket (c_duckdb_vector_get_column_type vector) destroyLogicalType

vectorElementType :: DuckDBVector -> IO DuckDBType
vectorElementType vec =
    bracket (c_duckdb_vector_get_column_type vec) destroyLogicalType c_duckdb_get_type_id

{- | Decode the element at an index of a vector's data. The type must not need
logical type metadata: DECIMAL, ENUM, GEOMETRY, and nested types use their own
decoders. The caller checks validity.
-}
decodeElement :: DuckDBType -> Ptr () -> Int -> IO FieldValue
decodeElement dtype dataPtr rowIdx = case dtype of
    DuckDBTypeBoolean -> do
        raw <- peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
        pure (FieldBool (raw /= 0))
    DuckDBTypeTinyInt -> FieldInt8 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeSmallInt -> FieldInt16 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeInteger -> FieldInt32 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeBigInt -> FieldInt64 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeUTinyInt -> FieldWord8 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeUSmallInt -> FieldWord16 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeUInteger -> FieldWord32 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeUBigInt -> FieldWord64 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeFloat -> FieldFloat <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeDouble -> FieldDouble <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeVarchar -> FieldText . TextEncoding.decodeUtf8 <$> chunkDecodeBlob dataPtr index
    DuckDBTypeStringLiteral -> FieldText . TextEncoding.decodeUtf8 <$> chunkDecodeBlob dataPtr index
    DuckDBTypeBlob -> FieldBlob <$> chunkDecodeBlob dataPtr index
    DuckDBTypeUUID -> do
        DuckDBUHugeInt lower upperBiased <- peekElemOff (castPtr dataPtr :: Ptr DuckDBUHugeInt) rowIdx
        let upper = upperBiased `xor` (0x8000000000000000 :: Word64)
        pure (FieldUUID (UUID.fromWords64 (fromIntegral upper) lower))
    DuckDBTypeDate -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldDate . decodeDuckDBDate
    DuckDBTypeTime -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTime . decodeDuckDBTime
    DuckDBTypeTimeNs -> FieldTime . decodeDuckDBTimeNs <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeTimeTz -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimeTZ . decodeDuckDBTimeTz
    DuckDBTypeTimestamp -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestamp
    DuckDBTypeTimestampS -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestampSeconds
    DuckDBTypeTimestampMs -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestampMilliseconds
    DuckDBTypeTimestampNs -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestamp . decodeDuckDBTimestampNanoseconds
    DuckDBTypeTimestampTz -> peekElemOff (castPtr dataPtr) rowIdx >>= fmap FieldTimestampTZ . decodeDuckDBTimestampUTCTime
    DuckDBTypeInterval -> FieldInterval . intervalValueFromDuckDB <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeHugeInt -> FieldHugeInt . duckDBHugeIntToInteger <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeUHugeInt -> FieldUHugeInt . duckDBUHugeIntToInteger <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeBit -> do
        bytes <- BS.unpack <$> chunkDecodeBlob dataPtr index
        pure $ FieldBit case bytes of
            [] -> BitString 0 BS.empty
            [padding] -> BitString padding BS.empty
            paddingByte : first : rest ->
                BitString paddingByte (BS.pack (foldl clearBit first [8 - fromIntegral paddingByte .. 7] : rest))
    DuckDBTypeBigNum -> do
        bytes <- chunkDecodeBlob dataPtr index
        pure (FieldBigNum (BigNum (if BS.length bytes < 3 then 0 else fromBigNumBytes (BS.unpack bytes))))
    DuckDBTypeIntegerLiteral -> FieldInt64 <$> peekElemOff (castPtr dataPtr) rowIdx
    DuckDBTypeInvalid ->
        error "duckdb-simple: INVALID type in eager result"
    DuckDBTypeAny ->
        error "duckdb-simple: ANY columns should not appear in results"
    other ->
        error ("duckdb-simple: UNKNOWN type in eager result: " <> show other)
  where
    index = fromIntegral rowIdx :: DuckDBIdx

intervalValueFromDuckDB :: DuckDBInterval -> IntervalValue
intervalValueFromDuckDB DuckDBInterval{duckDBIntervalMonths, duckDBIntervalDays, duckDBIntervalMicros} =
    IntervalValue
        { intervalMonths = duckDBIntervalMonths
        , intervalDays = duckDBIntervalDays
        , intervalMicros = duckDBIntervalMicros
        }

duckDBHugeIntToInteger :: DuckDBHugeInt -> Integer
duckDBHugeIntToInteger DuckDBHugeInt{duckDBHugeIntLower, duckDBHugeIntUpper} =
    (fromIntegral duckDBHugeIntUpper `shiftL` 64) .|. fromIntegral duckDBHugeIntLower

duckDBUHugeIntToInteger :: DuckDBUHugeInt -> Integer
duckDBUHugeIntToInteger DuckDBUHugeInt{duckDBUHugeIntLower, duckDBUHugeIntUpper} =
    (fromIntegral duckDBUHugeIntUpper `shiftL` 64) .|. fromIntegral duckDBUHugeIntLower
