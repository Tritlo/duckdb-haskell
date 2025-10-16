{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database.DuckDB.Simple.Materialize (
    materializeValue,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, when)
import qualified Data.ByteString as BS
import Data.Bits (clearBit, shiftL, xor, (.|.))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Array (Array, listArray)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime
    ( LocalTime (..)
    , TimeOfDay (..)
    , minutesToTimeZone
    , utc
    , utcToLocalTime
    )
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField
    ( BigNum (..)
    , BitString (..)
    , DecimalValue (..)
    , FieldValue (..)
    , IntervalValue (..)
    , TimeWithZone (..)
    , fromBigNumBytes
    )
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peekByteOff, peekElemOff, pokeByteOff)

data DuckDBListEntry = DuckDBListEntry
    { duckDBListEntryOffset :: !Word64
    , duckDBListEntryLength :: !Word64
    }
    deriving (Eq, Show)

instance Storable DuckDBListEntry where
    sizeOf _ = listEntryWordSize * 2
    alignment _ = alignment (0 :: Word64)
    peek ptr = do
        offset <- peekByteOff ptr 0
        len <- peekByteOff ptr listEntryWordSize
        pure DuckDBListEntry{duckDBListEntryOffset = offset, duckDBListEntryLength = len}
    poke ptr DuckDBListEntry{duckDBListEntryOffset, duckDBListEntryLength} = do
        pokeByteOff ptr 0 duckDBListEntryOffset
        pokeByteOff ptr listEntryWordSize duckDBListEntryLength

listEntryWordSize :: Int
listEntryWordSize = sizeOf (0 :: Word64)

chunkIsRowValid :: Ptr Word64 -> DuckDBIdx -> IO Bool
chunkIsRowValid validity rowIdx
    | validity == nullPtr = pure True
    | otherwise = do
        CBool flag <- c_duckdb_validity_row_is_valid validity rowIdx
        pure (flag /= 0)

chunkDecodeText :: Ptr () -> DuckDBIdx -> IO Text
chunkDecodeText dataPtr rowIdx = do
    let base = castPtr dataPtr :: Ptr Word8
        offset = fromIntegral rowIdx * duckdbStringTSize
        stringPtr = castPtr (base `plusPtr` offset) :: Ptr DuckDBStringT
    len <- c_duckdb_string_t_length stringPtr
    if len == 0
        then pure Text.empty
        else do
            cstr <- c_duckdb_string_t_data stringPtr
            bytes <- BS.packCStringLen (cstr, fromIntegral len)
            pure (TextEncoding.decodeUtf8 bytes)

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

materializeValue :: DuckDBType -> DuckDBVector -> Ptr () -> Ptr Word64 -> Int -> IO FieldValue
materializeValue dtype vector dataPtr validity rowIdx = do
    let duckIdx = fromIntegral rowIdx :: DuckDBIdx
    valid <- chunkIsRowValid validity duckIdx
    if not valid
        then pure FieldNull
        else case dtype of
            DuckDBTypeBoolean -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
                pure (FieldBool (raw /= 0))
            DuckDBTypeTinyInt -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Int8) rowIdx
                pure (FieldInt8 raw)
            DuckDBTypeSmallInt -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Int16) rowIdx
                pure (FieldInt16 raw)
            DuckDBTypeInteger -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Int32) rowIdx
                pure (FieldInt32 raw)
            DuckDBTypeBigInt -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Int64) rowIdx
                pure (FieldInt64 raw)
            DuckDBTypeUTinyInt -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
                pure (FieldWord8 raw)
            DuckDBTypeUSmallInt -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Word16) rowIdx
                pure (FieldWord16 raw)
            DuckDBTypeUInteger -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Word32) rowIdx
                pure (FieldWord32 raw)
            DuckDBTypeUBigInt -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Word64) rowIdx
                pure (FieldWord64 raw)
            DuckDBTypeFloat -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Float) rowIdx
                pure (FieldFloat raw)
            DuckDBTypeDouble -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Double) rowIdx
                pure (FieldDouble raw)
            DuckDBTypeVarchar -> FieldText <$> chunkDecodeText dataPtr duckIdx
            DuckDBTypeUUID -> do
                DuckDBUHugeInt lower upperBiased <- peekElemOff (castPtr dataPtr :: Ptr DuckDBUHugeInt) rowIdx
                let upper = upperBiased `xor` (0x8000000000000000 :: Word64)
                pure (FieldUUID (UUID.fromWords64 (fromIntegral upper) lower))
            DuckDBTypeBlob -> FieldBlob <$> chunkDecodeBlob dataPtr duckIdx
            DuckDBTypeDate -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Int32) rowIdx
                FieldDate <$> decodeDuckDBDate (DuckDBDate raw)
            DuckDBTypeTime -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTime) rowIdx
                FieldTime <$> decodeDuckDBTime raw
            DuckDBTypeTimeNs -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimeNs) rowIdx
                pure (FieldTime (decodeDuckDBTimeNs raw))
            DuckDBTypeTimeTz -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimeTz) rowIdx
                FieldTimeTZ <$> decodeDuckDBTimeTz raw
            DuckDBTypeTimestamp -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestamp) rowIdx
                FieldTimestamp <$> decodeDuckDBTimestamp raw
            DuckDBTypeTimestampS -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestampS) rowIdx
                FieldTimestamp <$> decodeDuckDBTimestampSeconds raw
            DuckDBTypeTimestampMs -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestampMs) rowIdx
                FieldTimestamp <$> decodeDuckDBTimestampMilliseconds raw
            DuckDBTypeTimestampNs -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestampNs) rowIdx
                FieldTimestamp <$> decodeDuckDBTimestampNanoseconds raw
            DuckDBTypeTimestampTz -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestamp) rowIdx
                FieldTimestampTZ <$> decodeDuckDBTimestampUTCTime raw
            DuckDBTypeInterval -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBInterval) rowIdx
                pure (FieldInterval (intervalValueFromDuckDB raw))
            DuckDBTypeDecimal ->
                bracket
                    (c_duckdb_vector_get_column_type vector)
                    (\lty -> alloca $ \ptr -> poke ptr lty >> c_duckdb_destroy_logical_type ptr)
                    \logical -> do
                        width <- c_duckdb_decimal_width logical
                        scale <- c_duckdb_decimal_scale logical
                        internalTy <- c_duckdb_decimal_internal_type logical
                        rawValue <-
                            case internalTy of
                                DuckDBTypeSmallInt ->
                                    toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int16) rowIdx
                                DuckDBTypeInteger ->
                                    toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int32) rowIdx
                                DuckDBTypeBigInt ->
                                    toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int64) rowIdx
                                DuckDBTypeHugeInt ->
                                    toInteger . duckDBHugeIntToInteger
                                        <$> peekElemOff (castPtr dataPtr :: Ptr DuckDBHugeInt) rowIdx
                                _ ->
                                    error "duckdb-simple: unsupported decimal internal storage type"
                        pure (FieldDecimal (DecimalValue width scale rawValue))
            DuckDBTypeHugeInt -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBHugeInt) rowIdx
                pure (FieldHugeInt (duckDBHugeIntToInteger raw))
            DuckDBTypeUHugeInt -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr DuckDBUHugeInt) rowIdx
                pure (FieldUHugeInt (duckDBUHugeIntToInteger raw))
            DuckDBTypeBit -> do
                let base = castPtr dataPtr :: Ptr Word8
                    offset = fromIntegral rowIdx * duckdbStringTSize
                    stringPtr = castPtr (base `plusPtr` offset) :: Ptr DuckDBStringT
                len <- c_duckdb_string_t_length stringPtr
                ptr <- c_duckdb_string_t_data stringPtr
                bs <- BS.unpack <$> BS.packCStringLen (ptr, fromIntegral len)
                case bs of
                    [] -> pure (FieldBit (BitString 0 BS.empty))
                    [padding] -> pure (FieldBit (BitString padding BS.empty))
                    (paddingByte : b : bits) -> do
                        let cleared = foldl clearBit b [8 - fromIntegral paddingByte .. 7]
                        pure (FieldBit (BitString paddingByte (BS.pack (cleared : bits))))
            DuckDBTypeBigNum -> do
                let base = castPtr dataPtr :: Ptr Word8
                    offset = fromIntegral rowIdx * duckdbStringTSize
                    stringPtr = castPtr (base `plusPtr` offset) :: Ptr DuckDBStringT
                len <- c_duckdb_string_t_length stringPtr
                if len < 3
                    then pure (FieldBigNum (BigNum 0))
                    else do
                        ptr <- c_duckdb_string_t_data stringPtr
                        bytes <- BS.unpack <$> BS.packCStringLen (ptr, fromIntegral len)
                        pure (FieldBigNum (BigNum (fromBigNumBytes bytes)))
            DuckDBTypeArray -> FieldArray <$> decodeArrayElements vector rowIdx
            DuckDBTypeList -> FieldList <$> decodeListElements vector dataPtr rowIdx
            DuckDBTypeMap -> FieldMap <$> decodeMapPairs vector dataPtr rowIdx
            DuckDBTypeStruct ->
                error "duckdb-simple: STRUCT columns are not supported"
            DuckDBTypeUnion ->
                error "duckdb-simple: UNION columns are not supported"
            DuckDBTypeEnum ->
                bracket
                    (c_duckdb_vector_get_column_type vector)
                    (\lty -> alloca $ \ptr -> poke ptr lty >> c_duckdb_destroy_logical_type ptr)
                    \logical -> do
                        enumInternal <- c_duckdb_enum_internal_type logical
                        case enumInternal of
                            DuckDBTypeUTinyInt ->
                                FieldEnum . fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
                            DuckDBTypeUSmallInt ->
                                FieldEnum . fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Word16) rowIdx
                            DuckDBTypeUInteger ->
                                FieldEnum <$> peekElemOff (castPtr dataPtr :: Ptr Word32) rowIdx
                            _ ->
                                error "duckdb-simple: unsupported enum internal storage type"
            DuckDBTypeSQLNull ->
                pure FieldNull
            DuckDBTypeStringLiteral -> FieldText <$> chunkDecodeText dataPtr duckIdx
            DuckDBTypeIntegerLiteral -> do
                raw <- peekElemOff (castPtr dataPtr :: Ptr Int64) rowIdx
                pure (FieldInt64 raw)
            DuckDBTypeInvalid ->
                error "duckdb-simple: INVALID type in eager result"
            DuckDBTypeAny ->
                error "duckdb-simple: ANY columns should not appear in results"
            other ->
                error ("duckdb-simple: UNKNOWN type in eager result: " <> show other)

decodeArrayElements :: DuckDBVector -> Int -> IO (Array Int FieldValue)
decodeArrayElements vector rowIdx = do
    arraySize <-
        bracket
            (c_duckdb_vector_get_column_type vector)
            (\logical -> alloca $ \ptr -> poke ptr logical >> c_duckdb_destroy_logical_type ptr)
            \logical -> do
                sizeRaw <- c_duckdb_array_type_array_size logical
                let sizeWord = fromIntegral sizeRaw :: Word64
                ensureWithinIntRange (Text.pack "array size") sizeWord
    childVec <- c_duckdb_array_vector_get_child vector
    when (childVec == nullPtr) $
        throwIO (userError "duckdb-simple: array child vector is null")
    childType <- vectorElementType childVec
    childData <- c_duckdb_vector_get_data childVec
    childValidity <- c_duckdb_vector_get_validity childVec
    let baseIdx = rowIdx * arraySize
    values <-
        forM [0 .. arraySize - 1] \delta ->
            materializeValue childType childVec childData childValidity (baseIdx + delta)
    pure $
        if arraySize <= 0
            then listArray (0, -1) []
            else listArray (0, arraySize - 1) values

decodeListElements :: DuckDBVector -> Ptr () -> Int -> IO [FieldValue]
decodeListElements vector dataPtr rowIdx = do
    entry <- peekElemOff (castPtr dataPtr :: Ptr DuckDBListEntry) rowIdx
    (baseIdx, len) <- listEntryBounds (Text.pack "list") entry
    childVec <- c_duckdb_list_vector_get_child vector
    when (childVec == nullPtr) $
        throwIO (userError "duckdb-simple: list child vector is null")
    childType <- vectorElementType childVec
    childData <- c_duckdb_vector_get_data childVec
    childValidity <- c_duckdb_vector_get_validity childVec
    forM [0 .. len - 1] \delta ->
        materializeValue childType childVec childData childValidity (baseIdx + delta)

decodeMapPairs :: DuckDBVector -> Ptr () -> Int -> IO [(FieldValue, FieldValue)]
decodeMapPairs vector dataPtr rowIdx = do
    entry <- peekElemOff (castPtr dataPtr :: Ptr DuckDBListEntry) rowIdx
    (baseIdx, len) <- listEntryBounds (Text.pack "map") entry
    structVec <- c_duckdb_list_vector_get_child vector
    when (structVec == nullPtr) $
        throwIO (userError "duckdb-simple: map struct vector is null")
    keyVec <- c_duckdb_struct_vector_get_child structVec 0
    valueVec <- c_duckdb_struct_vector_get_child structVec 1
    when (keyVec == nullPtr || valueVec == nullPtr) $
        throwIO (userError "duckdb-simple: map child vectors are null")
    keyType <- vectorElementType keyVec
    valueType <- vectorElementType valueVec
    keyData <- c_duckdb_vector_get_data keyVec
    valueData <- c_duckdb_vector_get_data valueVec
    keyValidity <- c_duckdb_vector_get_validity keyVec
    valueValidity <- c_duckdb_vector_get_validity valueVec
    forM [0 .. len - 1] \delta -> do
        let childIdx = baseIdx + delta
        keyValue <- materializeValue keyType keyVec keyData keyValidity childIdx
        valueValue <- materializeValue valueType valueVec valueData valueValidity childIdx
        pure (keyValue, valueValue)

vectorElementType :: DuckDBVector -> IO DuckDBType
vectorElementType vec = do
    logical <- c_duckdb_vector_get_column_type vec
    dtype <- c_duckdb_get_type_id logical
    destroyLogicalType logical
    pure dtype

listEntryBounds :: Text -> DuckDBListEntry -> IO (Int, Int)
listEntryBounds context DuckDBListEntry{duckDBListEntryOffset, duckDBListEntryLength} = do
    base <- ensureWithinIntRange (context <> Text.pack " offset") duckDBListEntryOffset
    len <- ensureWithinIntRange (context <> Text.pack " length") duckDBListEntryLength
    let maxInt = toInteger (maxBound :: Int)
        upperBound = toInteger base + toInteger len - 1
    when (len > 0 && upperBound > maxInt) $
        throwIO (userError ("duckdb-simple: " <> Text.unpack context <> " bounds exceed Int range"))
    pure (base, len)

ensureWithinIntRange :: Text -> Word64 -> IO Int
ensureWithinIntRange context value =
    let actual = toInteger value
        limit = toInteger (maxBound :: Int)
     in if actual <= limit
            then pure (fromInteger actual)
            else throwIO (userError ("duckdb-simple: " <> Text.unpack context <> " exceeds Int range"))

decodeDuckDBDate :: DuckDBDate -> IO Day
decodeDuckDBDate raw =
    alloca $ \ptr -> do
        c_duckdb_from_date raw ptr
        dateStruct <- peek ptr
        pure (dateStructToDay dateStruct)

decodeDuckDBTime :: DuckDBTime -> IO TimeOfDay
decodeDuckDBTime raw =
    alloca $ \ptr -> do
        c_duckdb_from_time raw ptr
        timeStruct <- peek ptr
        pure (timeStructToTimeOfDay timeStruct)

decodeDuckDBTimestamp :: DuckDBTimestamp -> IO LocalTime
decodeDuckDBTimestamp raw =
    alloca $ \ptr -> do
        c_duckdb_from_timestamp raw ptr
        DuckDBTimestampStruct{duckDBTimestampStructDate = dateStruct, duckDBTimestampStructTime = timeStruct} <- peek ptr
        pure
            LocalTime
                { localDay = dateStructToDay dateStruct
                , localTimeOfDay = timeStructToTimeOfDay timeStruct
                }

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
        let timeOfDay = timeStructToTimeOfDay timeStruct
            minutes = fromIntegral offset `div` 60
            zone = minutesToTimeZone minutes
        pure TimeWithZone{timeWithZoneTime = timeOfDay, timeWithZoneZone = zone}

decodeDuckDBTimestampSeconds :: DuckDBTimestampS -> IO LocalTime
decodeDuckDBTimestampSeconds (DuckDBTimestampS seconds) =
    decodeDuckDBTimestamp (DuckDBTimestamp (seconds * 1000000))

decodeDuckDBTimestampMilliseconds :: DuckDBTimestampMs -> IO LocalTime
decodeDuckDBTimestampMilliseconds (DuckDBTimestampMs millis) =
    decodeDuckDBTimestamp (DuckDBTimestamp (millis * 1000))

decodeDuckDBTimestampNanoseconds :: DuckDBTimestampNs -> IO LocalTime
decodeDuckDBTimestampNanoseconds (DuckDBTimestampNs nanos) = do
    let utcTime = posixSecondsToUTCTime (fromRational (toInteger nanos % 1000000000))
    pure (utcToLocalTime utc utcTime)

decodeDuckDBTimestampUTCTime :: DuckDBTimestamp -> IO UTCTime
decodeDuckDBTimestampUTCTime (DuckDBTimestamp micros) =
    pure (posixSecondsToUTCTime (fromRational (toInteger micros % 1000000)))

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

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType logicalType =
    alloca $ \ptr -> do
        poke ptr logicalType
        c_duckdb_destroy_logical_type ptr

dateStructToDay :: DuckDBDateStruct -> Day
dateStructToDay DuckDBDateStruct{duckDBDateStructYear, duckDBDateStructMonth, duckDBDateStructDay} =
    fromGregorian
        (fromIntegral duckDBDateStructYear)
        (fromIntegral duckDBDateStructMonth)
        (fromIntegral duckDBDateStructDay)

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
