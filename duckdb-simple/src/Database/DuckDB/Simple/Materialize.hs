{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Database.DuckDB.Simple.Materialize (
    VariantReader,
    prepareVariantReader,
    materializeValue,
    prepareValueReader,
    prepareVectorReader,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, when)
import Data.Array (Array, elems, listArray)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
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
    TupleValue (..),
    UnionMemberType (..),
    UnionValue (..),
    logicalTypeToRep,
 )
import Database.DuckDB.Simple.Native
import qualified Database.DuckDB.Simple.VariantCodec as VariantCodec
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..), peekElemOff)
import GHC.Records (getField)

-- | Prepare copied VARIANT rows with a dedicated metadata connection.
type VariantReader = DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)

-- | Copy VARIANT rows before releasing the borrowed metadata connection.
prepareVariantReader :: DuckDBV2ConnectionHandle -> VariantReader
prepareVariantReader = VariantCodec.prepareVariantDecoder materializeScalarValue

-- | Decode one scalar after the VARIANT decoder casts it to its exact type.
materializeScalarValue :: DuckDBV2ConnectionHandle -> DuckDBV2ValueHandle -> IO FieldValue
materializeScalarValue connection value = withConnectionFactory connection \factory ->
    materializeValue (\_ -> throwIO (userError "duckdb-simple: expected a scalar VARIANT cast")) factory value

-- | Copy one value through an owned vector. The caller retains the factory owner.
materializeValue :: VariantReader -> DuckDBV2FactoryHandle -> DuckDBV2ValueHandle -> IO FieldValue
materializeValue variantReader factory value =
    bracket (output (c_duckdb_v2_value_get_logical_type value)) (destroy c_duckdb_v2_logical_type_destroy) \logical ->
        withArray [logical] \types ->
            bracket (output (c_duckdb_v2_data_chunk_create factory (ConstPtr types) 1)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk -> do
                vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                checked (c_duckdb_v2_vector_set_size vector 1)
                checked (c_duckdb_v2_vector_set_value vector 0 value)
                prepareVectorReader variantReader vector >>= ($ 0)

-- | Read the type, data, and validity of a vector, and prepare its reader.
prepareVectorReader :: VariantReader -> DuckDBV2VectorHandle -> IO (Int -> IO FieldValue)
prepareVectorReader variantReader vector = do
    -- Flatten before borrowing buffers. Nested children then use physical row indices.
    checked (c_duckdb_v2_vector_flatten vector)
    dtype <- vectorElementType vector
    view <- output (c_duckdb_v2_vector_get_view vector)
    let ConstPtr dataPtr = getField @"data'" view
        ConstPtr validity = getField @"validity" view
        ConstPtr selection = getField @"sel" view
    readValue <- prepareValueReader variantReader dtype vector dataPtr validity
    pure \row -> do
        when (row < 0 || fromIntegral row >= getField @"count" view) $
            throwIO (userError "duckdb-simple: vector row index out of range")
        physical <- if selection == nullPtr then pure row else fromIntegral <$> peekElemOff selection row
        readValue physical

-- | Prepare metadata once for a vector. The reader must not outlive its chunk.
prepareValueReader :: VariantReader -> DuckDBV2LogicalTypeId -> DuckDBV2VectorHandle -> Ptr Void -> Ptr Word64 -> IO (Int -> IO FieldValue)
prepareValueReader variantReader dtype vector dataPtr validity = case dtype of
    DuckDBV2LogicalTypeIdVariant -> variantReader vector
    DuckDBV2LogicalTypeIdGeometry -> whenValid FieldGeometry <$> prepareGeometryDecoder vector dataPtr
    DuckDBV2LogicalTypeIdArray -> whenValid FieldArray <$> prepareArrayDecoder variantReader vector
    DuckDBV2LogicalTypeIdList -> whenValid FieldList <$> prepareListDecoder variantReader vector dataPtr
    DuckDBV2LogicalTypeIdMap -> whenValid FieldMap <$> prepareMapDecoder variantReader vector dataPtr
    DuckDBV2LogicalTypeIdStruct -> whenValid FieldStruct <$> prepareStructDecoder variantReader vector
    DuckDBV2LogicalTypeIdTuple -> whenValid FieldTuple <$> prepareTupleDecoder variantReader vector
    DuckDBV2LogicalTypeIdUnion -> whenValid FieldUnion <$> prepareUnionDecoder variantReader vector
    _ -> pure (materializeElement dtype vector dataPtr validity)
  where
    whenValid wrap decode row = do
        valid <- chunkIsRowValid validity (fromIntegral row)
        if valid then wrap <$> decode row else pure FieldNull

{- | Copy CRS metadata once for a vector. The decoder copies the WKB bytes of
a valid row. It must not outlive its chunk.
-}
prepareGeometryDecoder :: DuckDBV2VectorHandle -> Ptr Void -> IO (Int -> IO RawGeometry)
prepareGeometryDecoder vector dataPtr = do
    crs <- withVectorType vector \logical -> do
        rep <- logicalTypeToRep logical
        case rep of
            LogicalTypeGeometry value -> pure value
            _ -> throwIO (userError "duckdb-simple: invalid GEOMETRY type")
    pure \row -> (`RawGeometry` crs) <$> chunkDecodeBlob dataPtr (fromIntegral row)

materializeElement :: DuckDBV2LogicalTypeId -> DuckDBV2VectorHandle -> Ptr Void -> Ptr Word64 -> Int -> IO FieldValue
materializeElement dtype vector dataPtr validity rowIdx = do
    valid <- chunkIsRowValid validity (fromIntegral rowIdx)
    if not valid
        then pure FieldNull
        else case dtype of
            DuckDBV2LogicalTypeIdDecimal ->
                withVectorType vector \logical -> do
                    rep <- logicalTypeToRep logical
                    (width, scale) <- case rep of
                        LogicalTypeDecimal width scale -> pure (width, scale)
                        _ -> throwIO (userError "duckdb-simple: invalid DECIMAL type")
                    rawValue <-
                        if width <= 4
                            then toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int16) rowIdx
                            else
                                if width <= 9
                                    then toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int32) rowIdx
                                    else
                                        if width <= 18
                                            then toInteger <$> peekElemOff (castPtr dataPtr :: Ptr Int64) rowIdx
                                            else duckDBHugeIntToInteger <$> peekElemOff (castPtr dataPtr :: Ptr DuckDBV2HugeintT) rowIdx
                    pure (FieldDecimal (DecimalValue width scale rawValue))
            DuckDBV2LogicalTypeIdEnum ->
                withVectorType vector \logical -> do
                    count <- output (c_duckdb_v2_logical_type_get_param_count logical)
                    if count <= 255
                        then FieldEnum . fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Word8) rowIdx
                        else
                            if count <= 65535
                                then FieldEnum . fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Word16) rowIdx
                                else FieldEnum <$> peekElemOff (castPtr dataPtr :: Ptr Word32) rowIdx
            DuckDBV2LogicalTypeIdSQLNull -> pure FieldNull
            _ -> decodeElement dtype dataPtr rowIdx

prepareArrayDecoder :: VariantReader -> DuckDBV2VectorHandle -> IO (Int -> IO (Array Int FieldValue))
prepareArrayDecoder variantReader vector = do
    arraySize <-
        withVectorType vector \logical -> do
            rep <- logicalTypeToRep logical
            case rep of
                LogicalTypeArray _ size -> ensureWithinIntRange (Text.pack "array size") size
                _ -> throwIO (userError "duckdb-simple: invalid ARRAY type")
    childVec <- vectorChild vector 0
    when (childVec == DuckDBV2VectorHandle nullPtr) $
        throwIO (userError "duckdb-simple: array child vector is null")
    readChild <- prepareVectorReader variantReader childVec
    pure \rowIdx -> do
        let baseIdx = rowIdx * arraySize
        values <- forM [0 .. arraySize - 1] \delta -> readChild (baseIdx + delta)
        pure (listArray (0, arraySize - 1) values)

prepareListDecoder :: VariantReader -> DuckDBV2VectorHandle -> Ptr Void -> IO (Int -> IO [FieldValue])
prepareListDecoder variantReader vector dataPtr = do
    childVec <- vectorChild vector 0
    when (childVec == DuckDBV2VectorHandle nullPtr) $
        throwIO (userError "duckdb-simple: list child vector is null")
    readChild <- prepareVectorReader variantReader childVec
    pure \rowIdx -> do
        entry <- peekElemOff (castPtr dataPtr :: Ptr DuckDBV2ListEntry) rowIdx
        (baseIdx, len) <- listEntryBounds (Text.pack "list") entry
        forM [0 .. len - 1] \delta -> readChild (baseIdx + delta)

prepareMapDecoder :: VariantReader -> DuckDBV2VectorHandle -> Ptr Void -> IO (Int -> IO [(FieldValue, FieldValue)])
prepareMapDecoder variantReader vector dataPtr = do
    structVec <- vectorChild vector 0
    when (structVec == DuckDBV2VectorHandle nullPtr) $
        throwIO (userError "duckdb-simple: map struct vector is null")
    keyVec <- vectorChild structVec 0
    valueVec <- vectorChild structVec 1
    when (keyVec == DuckDBV2VectorHandle nullPtr || valueVec == DuckDBV2VectorHandle nullPtr) $
        throwIO (userError "duckdb-simple: map child vectors are null")
    readKey <- prepareVectorReader variantReader keyVec
    readValue <- prepareVectorReader variantReader valueVec
    pure \rowIdx -> do
        entry <- peekElemOff (castPtr dataPtr :: Ptr DuckDBV2ListEntry) rowIdx
        (baseIdx, len) <- listEntryBounds (Text.pack "map") entry
        forM [0 .. len - 1] \delta -> do
            let childIdx = baseIdx + delta
            keyValue <- readKey childIdx
            valueValue <- readValue childIdx
            pure (keyValue, valueValue)

{- | Read the STRUCT type and prepare a reader for each child once for a vector.
The decoder reads a valid row. It must not outlive its chunk.
-}
prepareStructDecoder :: VariantReader -> DuckDBV2VectorHandle -> IO (Int -> IO (StructValue FieldValue))
prepareStructDecoder variantReader vector =
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
                childVec <- vectorChild vector (fromIntegral childIdx)
                when (childVec == DuckDBV2VectorHandle nullPtr) $
                    throwIO (userError "duckdb-simple: struct child vector is null")
                readChild <- prepareVectorReader variantReader childVec
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

-- | Read positional field metadata and prepare readers that borrow the tuple.
prepareTupleDecoder :: VariantReader -> DuckDBV2VectorHandle -> IO (Int -> IO (TupleValue FieldValue))
prepareTupleDecoder variantReader vector = withVectorType vector \logical -> do
    rep <- logicalTypeToRep logical
    types <- case rep of
        LogicalTypeTuple fields -> pure fields
        _ -> throwIO (userError "duckdb-simple: invalid TUPLE type")
    let count = length (elems types)
    readers <- forM [0 .. count - 1] \index ->
        vectorChild vector (fromIntegral index) >>= prepareVectorReader variantReader
    pure \row -> do
        fields <- mapM ($ row) readers
        pure (TupleValue (listArray (0, count - 1) fields) types)

{- | Read the UNION type and prepare the tag reader and a reader for each member
once for a vector. The decoder reads a valid row. It must not outlive its chunk.
-}
prepareUnionDecoder :: VariantReader -> DuckDBV2VectorHandle -> IO (Int -> IO (UnionValue FieldValue))
prepareUnionDecoder variantReader vector =
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
        tagVec <- vectorChild vector 0
        when (tagVec == DuckDBV2VectorHandle nullPtr) $
            throwIO (userError "duckdb-simple: union tag vector is null")
        readTag <- prepareVectorReader variantReader tagVec
        memberReaders <-
            forM [1 .. memberCount] \childIdx -> do
                memberVec <- vectorChild vector (fromIntegral childIdx)
                when (memberVec == DuckDBV2VectorHandle nullPtr) $
                    throwIO (userError "duckdb-simple: union member vector is null")
                prepared <- newIORef Nothing
                pure \row -> do
                    cached <- readIORef prepared
                    readMember <- case cached of
                        Just reader -> pure reader
                        Nothing -> do
                            reader <- prepareVectorReader variantReader memberVec
                            writeIORef prepared (Just reader)
                            pure reader
                    readMember row
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

listEntryBounds :: Text -> DuckDBV2ListEntry -> IO (Int, Int)
listEntryBounds context (DuckDBV2ListEntry offset lenRaw) = do
    base <- ensureWithinIntRange (context <> Text.pack " offset") (fromIntegral offset)
    len <- ensureWithinIntRange (context <> Text.pack " length") (fromIntegral lenRaw)
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

-- | Borrow a nested vector from its parent chunk.
vectorChild :: DuckDBV2VectorHandle -> DuckDBIdx -> IO DuckDBV2VectorHandle
vectorChild vector index = output (c_duckdb_v2_vector_get_child vector index)
