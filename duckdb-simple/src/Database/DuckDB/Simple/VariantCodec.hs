{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Checked access to DuckDB 1.5's private VARIANT payload.
Only public C handles and vector accessors cross the native boundary.
-}
module Database.DuckDB.Simple.VariantCodec (
    decodeVariant,
    prepareVariantDecoder,
    decodeVariantPayload,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify')
import Data.Array (Array, bounds, listArray, (!))
import Data.Bits (complement, finiteBitSize, shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce (Coercible, coerce)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word32, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Element (bitStringFromBytes, chunkDecodeBlob, chunkIsRowValid, decodeElement)
import Database.DuckDB.Simple.FromField (
    BigNum (..),
    DecimalValue (..),
    FieldValue (..),
    RawGeometry (..),
    fromBigNumBytes,
 )
import Database.DuckDB.Simple.Internal (destroyLogicalType)
import Database.DuckDB.Simple.Variant (variantObject)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca, allocaBytesAligned)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, peek, peekElemOff, poke, sizeOf)
import Text.Read (readMaybe)

-- | Raise a codec error before an invalid native operation.
codecError :: String -> IO a
codecError message = throwIO (userError ("duckdb-simple: VARIANT: " <> message))

-- | Convert a checked pure result to an IO result.
checked :: Either String a -> IO a
checked = either codecError pure

-- | Reject native versions outside the supported private-format range.
checkVersion :: IO ()
checkVersion = do
    ConstPtr versionPtr <- duckdb_library_version
    version <- peekCString versionPtr
    let parts = Text.splitOn "." (Text.pack version)
        patch = case parts of
            ["v1", "5", p] -> readMaybe (Text.unpack p) :: Maybe Int
            _ -> Nothing
    unless (maybe False (>= 3) patch) (codecError ("unsupported native version " <> version))

-- | Check the byte order and pointer width required by native scalar loads.
checkPlatform :: IO ()
checkPlatform = alloca \ptr -> do
    poke ptr (1 :: Word32)
    first <- peek (castPtr ptr :: Ptr Word8)
    unless (first == 1 && sizeOf (nullPtr :: Ptr ()) == 8) $
        codecError "the private codec requires a 64-bit little-endian host"

-- | Acquire a non-NULL native handle.
nonNull :: (Coercible a (Ptr ())) => String -> IO a -> IO a
nonNull label action = do
    ptr <- action
    when (coerce ptr == (nullPtr :: Ptr ())) (codecError (label <> " returned NULL"))
    pure ptr

-- | Check a logical type's tag.
expectType :: DUCKDB_TYPE -> Duckdb_logical_type -> IO ()
expectType expected logical = do
    Duckdb_type actual <- duckdb_get_type_id logical
    unless (actual == expected) (codecError "unexpected physical child type")

-- | Check a STRUCT's names and owned child descriptors.
checkStruct :: Duckdb_logical_type -> [(Text, Duckdb_logical_type -> IO ())] -> IO ()
checkStruct logical fields = do
    count <- duckdb_struct_type_child_count logical
    unless (count == fromIntegral (length fields)) (codecError "unexpected physical child count")
    sequence_
        [ do
            bracket (nonNull "child name" (duckdb_struct_type_child_name logical index)) (duckdb_free . castPtr) \name -> do
                bytes <- BS.packCString name
                unless (bytes == Text.encodeUtf8 expectedName) (codecError "unexpected physical child name")
            bracket (nonNull "child type" (duckdb_struct_type_child_type logical index)) destroyLogicalType checkChild
        | (index, (expectedName, checkChild)) <- zip [0 ..] fields
        ]

-- | Check a LIST and its owned element descriptor.
checkList :: (Duckdb_logical_type -> IO ()) -> Duckdb_logical_type -> IO ()
checkList checkChild logical = do
    expectType DUCKDB_TYPE_LIST logical
    bracket (nonNull "list child type" (duckdb_list_type_child_type logical)) destroyLogicalType checkChild

-- | Check the complete, unshredded four-child VARIANT schema.
checkSchema :: Duckdb_logical_type -> IO ()
checkSchema logical = do
    expectType DUCKDB_TYPE_VARIANT logical
    checkStruct
        logical
        [ ("keys", checkList (expectType DUCKDB_TYPE_VARCHAR))
        ,
            ( "children"
            , checkList \child -> do
                expectType DUCKDB_TYPE_STRUCT child
                checkStruct child [("keys_index", expectType DUCKDB_TYPE_UINTEGER), ("values_index", expectType DUCKDB_TYPE_UINTEGER)]
            )
        ,
            ( "values"
            , checkList \child -> do
                expectType DUCKDB_TYPE_STRUCT child
                checkStruct child [("type_id", expectType DUCKDB_TYPE_UTINYINT), ("byte_offset", expectType DUCKDB_TYPE_UINTEGER)]
            )
        , ("data", expectType DUCKDB_TYPE_BLOB)
        ]

-- | Check an element index before native pointer arithmetic.
checkIndex :: Int -> Int -> IO ()
checkIndex width index =
    when (index < 0 || index > maxBound `div` width) (codecError "native element index exceeds Int range")

-- | Prepare validity access for a chunk. The caller checks row bounds.
prepareValidity :: Duckdb_vector -> IO (Int -> IO Bool)
prepareValidity vector = do
    validity <- duckdb_vector_get_validity vector
    pure (chunkIsRowValid validity . fromIntegral)

-- | Borrow a fixed-width buffer until its chunk is destroyed.
prepareElementReader :: (Storable a) => Duckdb_vector -> IO (Int -> IO a)
prepareElementReader vector = do
    ptr <- duckdb_vector_get_data vector
    valid <- prepareValidity vector
    pure (readAt valid (castPtr ptr))
  where
    readAt :: (Storable a) => (Int -> IO Bool) -> Ptr a -> Int -> IO a
    readAt valid ptr index = do
        checkIndex (sizeOfElement ptr) index
        when (ptr == nullPtr) (codecError "NULL vector data")
        present <- valid index
        unless present (codecError "NULL physical payload element")
        peekElemOff ptr index
    sizeOfElement :: (Storable a) => Ptr a -> Int
    sizeOfElement ptr = sizeOf (undefined `asTypeOf` element ptr)
    element :: Ptr a -> a
    element _ = undefined

-- | Borrow a string buffer and copy each requested value into Haskell memory.
prepareBytesReader :: Duckdb_vector -> IO (Int -> IO ByteString)
prepareBytesReader vector = do
    base <- duckdb_vector_get_data vector
    valid <- prepareValidity vector
    pure \index -> do
        checkIndex (sizeOf (undefined :: Duckdb_string_t)) index
        present <- valid index
        unless present (codecError "NULL physical string element")
        when (base == nullPtr) (codecError "NULL string vector data")
        chunkDecodeBlob base (fromIntegral index)

-- | Convert a nonnegative bounded integer to Int.
checkedInt :: Integer -> IO Int
checkedInt n
    | n < 0 || n > toInteger (maxBound :: Int) = codecError "payload size exceeds Int range"
    | otherwise = pure (fromInteger n)

-- | Prepare LIST bounds checks against the chunk's fixed child size.
prepareListBounds :: Duckdb_vector -> IO (Int -> IO (Int, Int))
prepareListBounds vector = do
    readEntry <- prepareElementReader vector
    Idx_t size <- duckdb_list_vector_get_size vector
    pure \row -> do
        Duckdb_list_entry offset count <- readEntry row
        unless (offset <= size && count <= size - offset) (codecError "LIST bounds exceed child size")
        start <- checkedInt (toInteger offset)
        len <- checkedInt (toInteger count)
        _ <- checkedInt (toInteger offset + toInteger count)
        pure (start, len)

-- | Decode one row while its flattened result chunk remains alive.
decodeVariant :: Duckdb_vector -> Int -> IO FieldValue
decodeVariant vector row = prepareVariantDecoder vector >>= ($ row)

{- | Check the format once and borrow buffers for a flattened result chunk.
The returned reader must not outlive the chunk. The caller supplies row indices
within that chunk. DuckDB result Fetch flattens nested vectors in 1.5.
Each read copies its payload into Haskell memory, including referenced keys.
-}
prepareVariantDecoder :: Duckdb_vector -> IO (Int -> IO FieldValue)
prepareVariantDecoder vector = do
    checkVersion
    checkPlatform
    when (vector == Duckdb_vector nullPtr) (codecError "NULL vector")
    bracket (nonNull "vector type" (duckdb_vector_get_column_type vector)) destroyLogicalType checkSchema
    valid <- prepareValidity vector
    keys <- child vector 0
    children <- child vector 1
    values <- child vector 2
    blob <- child vector 3
    keyBounds <- prepareListBounds keys
    childBounds <- prepareListBounds children
    valueBounds <- prepareListBounds values
    keyVector <- nonNull "keys vector" (duckdb_list_vector_get_child keys)
    childVector <- nonNull "children vector" (duckdb_list_vector_get_child children)
    valueVector <- nonNull "values vector" (duckdb_list_vector_get_child values)
    keyIndices <- child childVector 0
    valueIndices <- child childVector 1
    tags <- child valueVector 0
    offsets <- child valueVector 1
    readTag <- prepareElementReader tags
    readOffset <- prepareElementReader offsets
    hasKey <- prepareValidity keyIndices
    readKey <- prepareElementReader keyIndices
    readValue <- prepareElementReader valueIndices
    readKeyBytes <- prepareBytesReader keyVector
    readBlob <- prepareBytesReader blob
    pure \row -> do
        checkIndex (sizeOf (undefined :: Duckdb_list_entry)) row
        present <- valid row
        if not present
            then pure FieldNull
            else do
                (keyStart, keyCount) <- keyBounds row
                (childStart, childCount) <- childBounds row
                (valueStart, valueCount) <- valueBounds row
                valueRows <- forM [valueStart .. valueStart + valueCount - 1] \index ->
                    (,) <$> readTag index <*> readOffset index
                childRows <- forM [childStart .. childStart + childCount - 1] \index -> do
                    keyed <- hasKey index
                    key <- if keyed then Just <$> readKey index else pure Nothing
                    value <- readValue index
                    when (toInteger value >= toInteger valueCount) (codecError "child value index out of bounds")
                    case key of
                        Just k | toInteger k >= toInteger keyCount -> codecError "child key index out of bounds"
                        _ -> pure ()
                    pure (key, value)
                let usedKeys = Set.toList (Set.fromList [k | (Just k, _) <- childRows])
                keyRows <- forM usedKeys \index -> do
                    bytes <- readKeyBytes (keyStart + fromIntegral index)
                    text <- checked (either (Left . show) Right (Text.decodeUtf8' bytes))
                    pure (index, text)
                bytes <- readBlob row
                decodeVariantPayload valueRows childRows keyRows bytes
  where
    child parent index = nonNull "STRUCT vector child" (duckdb_struct_vector_get_child parent index)

{- | Decode copied 1.5 payload data for one non-NULL row.
Values are (tag, byte offset). Children are (optional key index, value index).
Keys pair an index with its text. Indices are relative to this row's LISTs.
The root value has index zero. This helper checks all metadata before decoding.
It raises an error for cycles. Shared values use a memo table.
-}
decodeVariantPayload :: [(Word8, Word32)] -> [(Maybe Word32, Word32)] -> [(Word32, Text)] -> ByteString -> IO FieldValue
decodeVariantPayload valueRows childRows keyRows bytes = do
    when (finiteBitSize (0 :: Int) < 64) (codecError "the private codec requires a 64-bit host")
    let values = listArray (0, length valueRows - 1) valueRows
        children = listArray (0, length childRows - 1) childRows
        keys = IntMap.fromList [(fromIntegral k, t) | (k, t) <- keyRows]
        valueCount = length valueRows
        childCount = length childRows
    when (null valueRows) (codecError "missing root value")
    unless (IntMap.size keys == length keyRows) (codecError "duplicate key dictionary index")
    forM_ valueRows \(tag, offset) -> do
        when (tag > 33) (codecError "unknown payload tag")
        when (toInteger offset > toInteger (BS.length bytes)) (codecError "byte offset exceeds data size")
    forM_ childRows \(key, value) -> do
        when (toInteger value >= toInteger valueCount) (codecError "child value index out of bounds")
        case key of
            Just k -> unless (IntMap.member (fromIntegral k) keys) (codecError "missing child key")
            Nothing -> pure ()
    evalStateT (visit values children keys childCount IntSet.empty 0) IntMap.empty
  where
    visit :: Array Int (Word8, Word32) -> Array Int (Maybe Word32, Word32) -> IntMap.IntMap Text -> Int -> IntSet.IntSet -> Int -> StateT (IntMap.IntMap FieldValue) IO FieldValue
    visit values children keys childCount ancestors index = do
        when (IntSet.member index ancestors) (lift (codecError "cyclic child reference"))
        cached <- gets (IntMap.lookup index)
        case cached of
            Just result -> pure result
            Nothing -> do
                (tag, offset) <- lift (checked (arrayElement values index))
                let payload = BS.drop (fromIntegral offset) bytes
                result <- case tag of
                    29 -> nested True payload
                    30 -> nested False payload
                    _ -> lift (decodeScalar tag payload)
                modify' (IntMap.insert index result)
                pure result
      where
        nested object payload = do
            (count, rest) <- lift (checked (readVarint payload))
            start <- if count == 0 then pure 0 else fst <$> lift (checked (readVarint rest))
            unless (toInteger start + toInteger count <= toInteger childCount) $
                lift (codecError "container child range out of bounds")
            entries <- forM [fromIntegral start .. fromIntegral start + fromIntegral count - 1] \childIndex -> do
                (key, childIndexValue) <- lift (checked (arrayElement children childIndex))
                name <- case (object, key) of
                    (True, Just k) -> case IntMap.lookup (fromIntegral k) keys of
                        Just text -> pure text
                        Nothing -> lift (codecError "missing object key")
                    (False, Nothing) -> pure Text.empty
                    _ -> lift (codecError "container key validity does not match its tag")
                item <- visit values children keys childCount (IntSet.insert index ancestors) (fromIntegral childIndexValue)
                pure (name, item)
            if object
                then do
                    unless (Set.size (Set.fromList (map fst entries)) == length entries) $
                        lift (codecError "duplicate object key")
                    pure (variantObject entries)
                else pure (FieldList (map snd entries))

-- | Read an array element after checking both bounds.
arrayElement :: Array Int a -> Int -> Either String a
arrayElement array index
    | index < lower || index > upper = Left "value index out of bounds"
    | otherwise = Right (array ! index)
  where
    (lower, upper) = bounds array

-- | Read an unsigned base-128 uint32 without overflow or truncation.
readVarint :: ByteString -> Either String (Word32, ByteString)
readVarint = go 0 0
  where
    go shift value input = case BS.uncons input of
        Nothing -> Left "truncated varint"
        Just (byte, rest)
            | shift == 28 && byte > 15 -> Left "varint exceeds uint32"
            | otherwise ->
                let result = value .|. (fromIntegral (byte .&. 127) `shiftL` shift)
                 in if byte .&. 128 == 0 then Right (result, rest) else go (shift + 7) result rest

-- | Read a bounded fixed-size payload in little-endian order.
readUnsigned :: Int -> ByteString -> Either String (Integer, ByteString)
readUnsigned count bytes
    | BS.length bytes < count = Left "truncated scalar payload"
    | otherwise =
        let (part, rest) = BS.splitAt count bytes
         in Right (BS.foldr (\byte value -> value `shiftL` 8 .|. toInteger byte) 0 part, rest)

-- | Read a two's-complement integer with a checked payload length.
readSigned :: Int -> ByteString -> Either String Integer
readSigned count bytes = do
    (value, _) <- readUnsigned count bytes
    pure (if value >= 2 ^ (count * 8 - 1) then value - 2 ^ (count * 8) else value)

-- | Read a length-prefixed string or blob.
readString :: ByteString -> Either String ByteString
readString bytes = do
    (count, rest) <- readVarint bytes
    if toInteger count > toInteger (BS.length rest)
        then Left "string length exceeds payload size"
        else Right (BS.take (fromIntegral count) rest)

-- | Decode the scalar tags from VariantLogicalType in DuckDB 1.5.
decodeScalar :: Word8 -> ByteString -> IO FieldValue
decodeScalar tag bytes = case tag of
    0 -> pure FieldNull
    1 -> pure (FieldBool True)
    2 -> pure (FieldBool False)
    15 -> checked do
        (precision, rest) <- readVarint bytes
        (scale, digits) <- readVarint rest
        when (precision < 1 || precision > 38 || scale > precision) (Left "invalid decimal metadata")
        let count = if precision <= 4 then 2 else if precision <= 9 then 4 else if precision <= 18 then 8 else 16
        value <- readSigned count digits
        when (abs value >= 10 ^ precision) (Left "unscaled decimal exceeds its precision")
        pure (FieldDecimal (DecimalValue (fromIntegral precision) (fromIntegral scale) value))
    16 -> checked do
        string <- readString bytes
        FieldText <$> either (Left . show) Right (Text.decodeUtf8' string)
    17 -> FieldBlob <$> checked (readString bytes)
    31 -> FieldBigNum . BigNum <$> checked (readString bytes >>= decodeBigNum)
    32 -> checked do
        bitBytes <- readString bytes
        case BS.unpack (BS.take 2 bitBytes) of
            [padding, first] -> do
                when (padding > 7) (Left "BIT padding exceeds seven")
                let maskBits = paddingMask padding
                unless (first .&. maskBits == maskBits) (Left "invalid native BIT padding")
                pure (FieldBit (bitStringFromBytes bitBytes))
            _ -> Left "BIT requires a padding byte and nonempty data"
    33 -> FieldGeometry . (`RawGeometry` Nothing) <$> checked (readString bytes)
    _ -> case fixedWidthTag tag of
        Just (dtype, size) -> decodeFixedWidth dtype size bytes
        Nothing -> codecError "unknown scalar tag"

{- | The type and the payload size of each fixed-width scalar tag. These
payloads have the memory layout of one vector element of the type.
-}
fixedWidthTag :: Word8 -> Maybe (DUCKDB_TYPE, Int)
fixedWidthTag = \case
    3 -> Just (DUCKDB_TYPE_TINYINT, 1)
    4 -> Just (DUCKDB_TYPE_SMALLINT, 2)
    5 -> Just (DUCKDB_TYPE_INTEGER, 4)
    6 -> Just (DUCKDB_TYPE_BIGINT, 8)
    7 -> Just (DUCKDB_TYPE_HUGEINT, 16)
    8 -> Just (DUCKDB_TYPE_UTINYINT, 1)
    9 -> Just (DUCKDB_TYPE_USMALLINT, 2)
    10 -> Just (DUCKDB_TYPE_UINTEGER, 4)
    11 -> Just (DUCKDB_TYPE_UBIGINT, 8)
    12 -> Just (DUCKDB_TYPE_UHUGEINT, 16)
    13 -> Just (DUCKDB_TYPE_FLOAT, 4)
    14 -> Just (DUCKDB_TYPE_DOUBLE, 8)
    18 -> Just (DUCKDB_TYPE_UUID, 16)
    19 -> Just (DUCKDB_TYPE_DATE, 4)
    20 -> Just (DUCKDB_TYPE_TIME, 8)
    21 -> Just (DUCKDB_TYPE_TIME_NS, 8)
    22 -> Just (DUCKDB_TYPE_TIMESTAMP_S, 8)
    23 -> Just (DUCKDB_TYPE_TIMESTAMP_MS, 8)
    24 -> Just (DUCKDB_TYPE_TIMESTAMP, 8)
    25 -> Just (DUCKDB_TYPE_TIMESTAMP_NS, 8)
    26 -> Just (DUCKDB_TYPE_TIME_TZ, 8)
    27 -> Just (DUCKDB_TYPE_TIMESTAMP_TZ, 8)
    28 -> Just (DUCKDB_TYPE_INTERVAL, 16)
    _ -> Nothing

-- | Copy a fixed-width payload to aligned memory and decode it as one element.
decodeFixedWidth :: DUCKDB_TYPE -> Int -> ByteString -> IO FieldValue
decodeFixedWidth dtype size bytes = do
    when (BS.length bytes < size) (codecError "truncated scalar payload")
    allocaBytesAligned size 16 \buffer -> do
        BS.useAsCStringLen bytes \(source, _) -> copyBytes buffer (castPtr source) size
        decodeElement dtype (castPtr buffer) 0

-- | Get the high-bit mask used for native BIT padding.
paddingMask :: Word8 -> Word8
paddingMask padding = complement ((1 `shiftL` (8 - fromIntegral padding)) - 1)

-- | Check BIGNUM's sign, three-byte length header, and magnitude, then decode it.
decodeBigNum :: ByteString -> Either String Integer
decodeBigNum bytes = do
    when (BS.length bytes < 4) (Left "truncated BIGNUM header or magnitude")
    let (header, raw) = BS.splitAt 3 bytes
        encoded = BS.foldl' (\n byte -> n `shiftL` 8 .|. fromIntegral byte) (0 :: Word32) header
        negative = encoded .&. 0x800000 == 0
        decoded = if negative then complement encoded .&. 0xffffff else encoded
        count = decoded .&. 0x7fffff
        magnitude = if negative then BS.map complement raw else raw
    unless (toInteger count == toInteger (BS.length magnitude)) (Left "BIGNUM header length mismatch")
    case BS.uncons magnitude of
        Just (0, rest) | not (BS.null rest) || negative -> Left "noncanonical BIGNUM magnitude"
        _ -> Right ()
    pure (fromBigNumBytes (BS.unpack bytes))
