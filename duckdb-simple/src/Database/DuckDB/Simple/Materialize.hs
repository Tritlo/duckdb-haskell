{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database.DuckDB.Simple.Materialize (
    prepareValueReader,
    prepareVectorReader,
) where

import Control.Exception (throwIO)
import Control.Monad (forM, when)
import Data.Array (Array, elems, listArray)
import Data.Int (Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Element (
    chunkDecodeBlob,
    chunkIsRowValid,
    decodeElement,
    duckDBHugeIntToInteger,
    vectorElementType,
    withVectorType,
 )
import Database.DuckDB.Simple.FromField (
    DecimalValue (..),
    FieldValue (..),
    RawGeometry (..),
 )
import Database.DuckDB.Simple.LogicalRep (
    LogicalTypeRep (..),
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..),
    logicalTypeToRep,
 )
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..), peekElemOff)

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
    crs <- withVectorType vector \logical -> do
        rep <- logicalTypeToRep logical
        case rep of
            LogicalTypeGeometry value -> pure value
            _ -> throwIO (userError "duckdb-simple: invalid GEOMETRY type")
    pure \row -> (`RawGeometry` crs) <$> chunkDecodeBlob dataPtr (fromIntegral row)

materializeValue :: DuckDBType -> DuckDBVector -> Ptr () -> Ptr Word64 -> Int -> IO FieldValue
materializeValue dtype vector dataPtr validity rowIdx = do
    valid <- chunkIsRowValid validity (fromIntegral rowIdx)
    if not valid
        then pure FieldNull
        else case dtype of
            DuckDBTypeGeometry -> FieldGeometry <$> (prepareGeometryDecoder vector dataPtr >>= ($ rowIdx))
            DuckDBTypeDecimal ->
                withVectorType vector \logical -> do
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
                                duckDBHugeIntToInteger <$> peekElemOff (castPtr dataPtr :: Ptr DuckDBHugeInt) rowIdx
                            _ ->
                                error "duckdb-simple: unsupported decimal internal storage type"
                    pure (FieldDecimal (DecimalValue width scale rawValue))
            DuckDBTypeArray -> FieldArray <$> decodeArrayElements vector rowIdx
            DuckDBTypeList -> FieldList <$> decodeListElements vector dataPtr rowIdx
            DuckDBTypeMap -> FieldMap <$> decodeMapPairs vector dataPtr rowIdx
            DuckDBTypeStruct -> FieldStruct <$> (prepareStructDecoder vector >>= ($ rowIdx))
            DuckDBTypeUnion -> FieldUnion <$> (prepareUnionDecoder vector >>= ($ rowIdx))
            DuckDBTypeEnum ->
                withVectorType vector \logical -> do
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
            DuckDBTypeSQLNull -> pure FieldNull
            _ -> decodeElement dtype dataPtr rowIdx

decodeArrayElements :: DuckDBVector -> Int -> IO (Array Int FieldValue)
decodeArrayElements vector rowIdx = do
    arraySize <-
        withVectorType vector \logical -> do
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
        listArray (0, arraySize - 1) values

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
    withVectorType vector \logical -> do
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
                    listArray (0, count - 1) valueFields
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
    withVectorType vector \logical -> do
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
