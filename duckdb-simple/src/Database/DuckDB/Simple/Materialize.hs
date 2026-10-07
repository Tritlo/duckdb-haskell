{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database.DuckDB.Simple.Materialize (
    prepareValueReader,
    prepareVectorReader,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, when)
import Data.Array (Array, elems, listArray)
import Data.Bits (clearBit, shiftL, xor, (.|.))
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (
    TimeOfDay (..),
    minutesToTimeZone,
    utc,
    utcToLocalTime,
 )
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (
    BigNum (..),
    BitString (..),
    DecimalValue (..),
    FieldValue (..),
    IntervalValue (..),
    RawGeometry (..),
    TimeWithZone (..),
    fromBigNumBytes,
 )
import Database.DuckDB.Simple.LogicalRep (
    LogicalTypeRep (..),
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..),
    logicalTypeToRep,
 )
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peekElemOff)

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

-- | Read the type, data, and validity of a vector, and prepare its reader.
prepareVectorReader :: DuckDBVector -> IO (Int -> IO FieldValue)
prepareVectorReader vector = do
    dtype <- vectorElementType vector
    dataPtr <- c_duckdb_vector_get_data vector
    validity <- c_duckdb_vector_get_validity vector
    prepareValueReader dtype vector dataPtr validity

-- | Prepare metadata once for a vector. The reader must not outlive its chunk.
prepareValueReader :: DuckDBType -> DuckDBVector -> Ptr () -> Ptr Word64 -> IO (Int -> IO FieldValue)
prepareValueReader dtype vector dataPtr validity = case dtype of
    DuckDBTypeGeometry -> whenValid FieldGeometry <$> prepareGeometryDecoder vector dataPtr
    DuckDBTypeStruct -> whenValid FieldStruct <$> prepareStructDecoder vector
    DuckDBTypeUnion -> whenValid FieldUnion <$> prepareUnionDecoder vector
    _ -> pure (materializeValue dtype vector dataPtr validity)
  where
    whenValid wrap decode row = do
        valid <- chunkIsRowValid validity (fromIntegral row)
        if valid then wrap <$> decode row else pure FieldNull

{- | Copy CRS metadata once for a vector. The decoder copies the WKB bytes of
a valid row. It must not outlive its chunk.
-}
prepareGeometryDecoder :: DuckDBVector -> Ptr () -> IO (Int -> IO RawGeometry)
prepareGeometryDecoder vector dataPtr = do
    crs <- bracket
        (c_duckdb_vector_get_column_type vector)
        (\logical -> alloca \ptr -> poke ptr logical >> c_duckdb_destroy_logical_type ptr)
        \logical -> do
            rep <- logicalTypeToRep logical
            case rep of
                LogicalTypeGeometry value -> pure value
                _ -> throwIO (userError "duckdb-simple: invalid GEOMETRY type")
    pure \row -> (`RawGeometry` crs) <$> chunkDecodeBlob dataPtr (fromIntegral row)

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
            DuckDBTypeGeometry -> FieldGeometry <$> (prepareGeometryDecoder vector dataPtr >>= ($ rowIdx))
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
            DuckDBTypeStruct -> FieldStruct <$> (prepareStructDecoder vector >>= ($ rowIdx))
            DuckDBTypeUnion -> FieldUnion <$> (prepareUnionDecoder vector >>= ($ rowIdx))
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
    readChild <- prepareVectorReader childVec
    let baseIdx = rowIdx * arraySize
    values <-
        forM [0 .. arraySize - 1] \delta ->
            readChild (baseIdx + delta)
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
    readChild <- prepareVectorReader childVec
    forM [0 .. len - 1] \delta ->
        readChild (baseIdx + delta)

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
    readKey <- prepareVectorReader keyVec
    readValue <- prepareVectorReader valueVec
    forM [0 .. len - 1] \delta -> do
        let childIdx = baseIdx + delta
        keyValue <- readKey childIdx
        valueValue <- readValue childIdx
        pure (keyValue, valueValue)

{- | Read the STRUCT type and prepare a reader for each child once for a vector.
The decoder reads a valid row. It must not outlive its chunk.
-}
prepareStructDecoder :: DuckDBVector -> IO (Int -> IO (StructValue FieldValue))
prepareStructDecoder vector =
    bracket
        (c_duckdb_vector_get_column_type vector)
        (\logical -> alloca $ \ptr -> poke ptr logical >> c_duckdb_destroy_logical_type ptr)
        \logical -> do
            structTypeRep <- logicalTypeToRep logical
            structFields <-
                case structTypeRep of
                    LogicalTypeStruct typeArray -> pure typeArray
                    other ->
                        throwIO
                            ( userError
                                ( "duckdb-simple: expected STRUCT logical type, but saw "
                                    <> show other
                                )
                            )
            let typeList = elems structFields
                count = length typeList
                indexMap =
                    Map.fromList (zip (map structFieldName typeList) [0 ..])
            childReaders <-
                forM (zip [0 .. count - 1] typeList) \(childIdx, StructField{structFieldName}) -> do
                    childVec <- c_duckdb_struct_vector_get_child vector (fromIntegral childIdx)
                    when (childVec == nullPtr) $
                        throwIO (userError "duckdb-simple: struct child vector is null")
                    readChild <- prepareVectorReader childVec
                    pure (structFieldName, readChild)
            pure \rowIdx -> do
                valueFields <-
                    forM childReaders \(name, readChild) -> do
                        value <- readChild rowIdx
                        pure StructField{structFieldName = name, structFieldValue = value}
                let fieldArray =
                        if count <= 0
                            then listArray (0, -1) []
                            else listArray (0, count - 1) valueFields
                pure
                    StructValue
                        { structValueFields = fieldArray
                        , structValueTypes = structFields
                        , structValueIndex = indexMap
                        }

{- | Read the UNION type and prepare the tag reader and a reader for each member
once for a vector. The decoder reads a valid row. It must not outlive its chunk.
-}
prepareUnionDecoder :: DuckDBVector -> IO (Int -> IO (UnionValue FieldValue))
prepareUnionDecoder vector =
    bracket
        (c_duckdb_vector_get_column_type vector)
        (\logical -> alloca $ \ptr -> poke ptr logical >> c_duckdb_destroy_logical_type ptr)
        \logical -> do
            unionTypeRep <- logicalTypeToRep logical
            membersArray <-
                case unionTypeRep of
                    LogicalTypeUnion members -> pure members
                    other ->
                        throwIO
                            ( userError
                                ( "duckdb-simple: expected UNION logical type, but saw "
                                    <> show other
                                )
                            )
            let membersList = elems membersArray
                memberCount = length membersList
            tagVec <- c_duckdb_struct_vector_get_child vector 0
            when (tagVec == nullPtr) $
                throwIO (userError "duckdb-simple: union tag vector is null")
            readTag <- prepareVectorReader tagVec
            memberReaders <-
                forM [1 .. memberCount] \childIdx -> do
                    memberVec <- c_duckdb_struct_vector_get_child vector (fromIntegral childIdx)
                    when (memberVec == nullPtr) $
                        throwIO (userError "duckdb-simple: union member vector is null")
                    prepareVectorReader memberVec
            pure \rowIdx -> do
                memberIdx <- readTag rowIdx >>= unionTagIndex
                when (memberIdx < 0 || memberIdx >= memberCount) $
                    throwIO (userError "duckdb-simple: union tag out of range")
                payload <- (memberReaders !! memberIdx) rowIdx
                pure
                    UnionValue
                        { unionValueIndex = fromIntegral memberIdx
                        , unionValueLabel = unionMemberName (membersList !! memberIdx)
                        , unionValuePayload = payload
                        , unionValueMembers = membersArray
                        }

-- | Convert a decoded UNION tag to a member index.
unionTagIndex :: FieldValue -> IO Int
unionTagIndex = \case
    FieldWord8 tagWord -> pure (fromIntegral tagWord :: Int)
    FieldWord16 tagWord -> pure (fromIntegral tagWord :: Int)
    FieldWord32 tagWord ->
        if tagWord <= fromIntegral (maxBound :: Word16)
            then pure (fromIntegral tagWord)
            else throwIO (userError "duckdb-simple: union tag exceeds Word16 range")
    FieldWord64 tagWord ->
        if tagWord <= fromIntegral (maxBound :: Word16)
            then pure (fromIntegral tagWord)
            else throwIO (userError "duckdb-simple: union tag exceeds Word16 range")
    FieldInt8 tagInt
        | tagInt >= 0 -> pure (fromIntegral tagInt)
        | otherwise -> throwIO (userError "duckdb-simple: union tag negative")
    FieldInt16 tagInt
        | tagInt >= 0 -> pure (fromIntegral tagInt)
        | otherwise -> throwIO (userError "duckdb-simple: union tag negative")
    FieldInt32 tagInt
        | tagInt >= 0 && tagInt <= fromIntegral (maxBound :: Word16) -> pure (fromIntegral tagInt)
        | tagInt < 0 -> throwIO (userError "duckdb-simple: union tag negative")
        | otherwise -> throwIO (userError "duckdb-simple: union tag exceeds Word16 range")
    FieldInt64 tagInt
        | tagInt >= 0 && tagInt <= fromIntegral (maxBound :: Word16) -> pure (fromIntegral tagInt)
        | tagInt < 0 -> throwIO (userError "duckdb-simple: union tag negative")
        | otherwise -> throwIO (userError "duckdb-simple: union tag exceeds Word16 range")
    FieldNull ->
        throwIO (userError "duckdb-simple: encountered NULL union tag")
    other ->
        throwIO
            ( userError
                ( "duckdb-simple: unexpected union tag value "
                    <> show other
                )
            )

vectorElementType :: DuckDBVector -> IO DuckDBType
vectorElementType vec =
    bracket (c_duckdb_vector_get_column_type vec) destroyLogicalType c_duckdb_get_type_id

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
