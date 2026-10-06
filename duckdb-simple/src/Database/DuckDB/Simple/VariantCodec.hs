{-# LANGUAGE BlockArguments #-}
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
import Data.Bits (complement, finiteBitSize, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeOfDay (..), minutesToTimeZone, utc, utcToLocalTime)
import qualified Data.UUID as UUID
import Data.Word (Word32, Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (
    BigNum (..),
    BitString (BitString),
    DecimalValue (..),
    FieldValue (..),
    IntervalValue (..),
    RawGeometry (..),
    TimeWithZone (..),
 )
import Database.DuckDB.Simple.Internal (destroyLogicalType)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), StructValue (..))
import Database.DuckDB.Simple.Time (LocalTimestamp, Unbounded (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, peek, peekElemOff, poke, sizeOf)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.Num.Integer (integerFromWordList)
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
    version <- c_duckdb_library_version >>= peekCString
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
nonNull :: String -> IO (Ptr a) -> IO (Ptr a)
nonNull label action = do
    ptr <- action
    when (ptr == nullPtr) (codecError (label <> " returned NULL"))
    pure ptr

-- | Check a logical type's tag.
expectType :: DuckDBType -> DuckDBLogicalType -> IO ()
expectType expected logical = do
    actual <- c_duckdb_get_type_id logical
    unless (actual == expected) (codecError "unexpected physical child type")

-- | Check a STRUCT's names and owned child descriptors.
checkStruct :: DuckDBLogicalType -> [(Text, DuckDBLogicalType -> IO ())] -> IO ()
checkStruct logical fields = do
    count <- c_duckdb_struct_type_child_count logical
    unless (count == fromIntegral (length fields)) (codecError "unexpected physical child count")
    sequence_
        [ do
            bracket (nonNull "child name" (c_duckdb_struct_type_child_name logical index)) (c_duckdb_free . castPtr) \name -> do
                bytes <- BS.packCString name
                unless (bytes == Text.encodeUtf8 expectedName) (codecError "unexpected physical child name")
            bracket (nonNull "child type" (c_duckdb_struct_type_child_type logical index)) destroyLogicalType checkChild
        | (index, (expectedName, checkChild)) <- zip [0 ..] fields
        ]

-- | Check a LIST and its owned element descriptor.
checkList :: (DuckDBLogicalType -> IO ()) -> DuckDBLogicalType -> IO ()
checkList checkChild logical = do
    expectType DuckDBTypeList logical
    bracket (nonNull "list child type" (c_duckdb_list_type_child_type logical)) destroyLogicalType checkChild

-- | Check the complete, unshredded four-child VARIANT schema.
checkSchema :: DuckDBLogicalType -> IO ()
checkSchema logical = do
    expectType DuckDBTypeVariant logical
    checkStruct
        logical
        [ ("keys", checkList (expectType DuckDBTypeVarchar))
        ,
            ( "children"
            , checkList \child -> do
                expectType DuckDBTypeStruct child
                checkStruct child [("keys_index", expectType DuckDBTypeUInteger), ("values_index", expectType DuckDBTypeUInteger)]
            )
        ,
            ( "values"
            , checkList \child -> do
                expectType DuckDBTypeStruct child
                checkStruct child [("type_id", expectType DuckDBTypeUTinyInt), ("byte_offset", expectType DuckDBTypeUInteger)]
            )
        , ("data", expectType DuckDBTypeBlob)
        ]

-- | Check an element index before native pointer arithmetic.
checkIndex :: Int -> Int -> IO ()
checkIndex width index =
    when (index < 0 || index > maxBound `div` width) (codecError "native element index exceeds Int range")

-- | Prepare validity access for a chunk. The caller checks row bounds.
prepareValidity :: DuckDBVector -> IO (Int -> IO Bool)
prepareValidity vector = do
    validity <- c_duckdb_vector_get_validity vector
    pure \index ->
        if validity == nullPtr
            then pure True
            else do
                word <- peekElemOff validity (index `div` 64)
                pure (word .&. (1 `shiftL` (index `mod` 64)) /= 0)

-- | Borrow a fixed-width buffer until its chunk is destroyed.
prepareElementReader :: (Storable a) => DuckDBVector -> IO (Int -> IO a)
prepareElementReader vector = do
    ptr <- c_duckdb_vector_get_data vector
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
prepareBytesReader :: DuckDBVector -> IO (Int -> IO ByteString)
prepareBytesReader vector = do
    base <- c_duckdb_vector_get_data vector
    valid <- prepareValidity vector
    pure \index -> do
        checkIndex 16 index
        present <- valid index
        unless present (codecError "NULL physical string element")
        when (base == nullPtr) (codecError "NULL string vector data")
        let string = castPtr (base `plusPtr` (index * 16)) :: Ptr DuckDBStringT
        len <- c_duckdb_string_t_length string
        count <- checkedInt (toInteger len)
        if count == 0
            then pure BS.empty
            else do
                bytes <- nonNull "string data" (c_duckdb_string_t_data string)
                BS.packCStringLen (bytes, count)

-- | Convert a nonnegative bounded integer to Int.
checkedInt :: Integer -> IO Int
checkedInt n
    | n < 0 || n > toInteger (maxBound :: Int) = codecError "payload size exceeds Int range"
    | otherwise = pure (fromInteger n)

-- | Prepare LIST bounds checks against the chunk's fixed child size.
prepareListBounds :: DuckDBVector -> IO (Int -> IO (Int, Int))
prepareListBounds vector = do
    readEntry <- prepareElementReader vector
    size <- c_duckdb_list_vector_get_size vector
    pure \row -> do
        DuckDBListEntry offset count <- readEntry row
        unless (offset <= size && count <= size - offset) (codecError "LIST bounds exceed child size")
        start <- checkedInt (toInteger offset)
        len <- checkedInt (toInteger count)
        _ <- checkedInt (toInteger offset + toInteger count)
        pure (start, len)

-- | Decode one row while its flattened result chunk remains alive.
decodeVariant :: DuckDBVector -> Int -> IO FieldValue
decodeVariant vector row = prepareVariantDecoder vector >>= ($ row)

{- | Check the format once and borrow buffers for a flattened result chunk.
The returned reader must not outlive the chunk. The caller supplies row indices
within that chunk. DuckDB result Fetch flattens nested vectors in 1.5.
Each read copies its payload into Haskell memory, including referenced keys.
-}
prepareVariantDecoder :: DuckDBVector -> IO (Int -> IO FieldValue)
prepareVariantDecoder vector = do
    checkVersion
    checkPlatform
    when (vector == nullPtr) (codecError "NULL vector")
    bracket (nonNull "vector type" (c_duckdb_vector_get_column_type vector)) destroyLogicalType checkSchema
    valid <- prepareValidity vector
    keys <- child vector 0
    children <- child vector 1
    values <- child vector 2
    blob <- child vector 3
    keyBounds <- prepareListBounds keys
    childBounds <- prepareListBounds children
    valueBounds <- prepareListBounds values
    keyVector <- nonNull "keys vector" (c_duckdb_list_vector_get_child keys)
    childVector <- nonNull "children vector" (c_duckdb_list_vector_get_child children)
    valueVector <- nonNull "values vector" (c_duckdb_list_vector_get_child values)
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
        checkIndex 16 row
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
                checked (decodeVariantPayload valueRows childRows keyRows bytes)
  where
    child parent index = nonNull "STRUCT vector child" (c_duckdb_struct_vector_get_child parent index)

{- | Decode copied 1.5 payload data for one non-NULL row.
Values are (tag, byte offset). Children are (optional key index, value index).
Keys pair an index with its text. Indices are relative to this row's LISTs.
The root value has index zero. This helper checks all metadata before decoding.
It rejects cycles and nesting above 128 levels. Shared values use a memo table.
-}
decodeVariantPayload :: [(Word8, Word32)] -> [(Maybe Word32, Word32)] -> [(Word32, Text)] -> ByteString -> Either String FieldValue
decodeVariantPayload valueRows childRows keyRows bytes = do
    when (finiteBitSize (0 :: Int) < 64) (Left "the private codec requires a 64-bit host")
    let values = listArray (0, length valueRows - 1) valueRows
        children = listArray (0, length childRows - 1) childRows
        keys = IntMap.fromList [(fromIntegral k, t) | (k, t) <- keyRows]
        valueCount = length valueRows
        childCount = length childRows
    when (null valueRows) (Left "missing root value")
    unless (IntMap.size keys == length keyRows) (Left "duplicate key dictionary index")
    forM_ valueRows \(tag, offset) -> do
        when (tag > 33) (Left "unknown payload tag")
        when (toInteger offset > toInteger (BS.length bytes)) (Left "byte offset exceeds data size")
    forM_ childRows \(key, value) -> do
        when (toInteger value >= toInteger valueCount) (Left "child value index out of bounds")
        case key of
            Just k -> unless (IntMap.member (fromIntegral k) keys) (Left "missing child key")
            Nothing -> Right ()
    fst <$> evalStateT (visit values children keys childCount 0 IntSet.empty 0) IntMap.empty
  where
    visit :: Array Int (Word8, Word32) -> Array Int (Maybe Word32, Word32) -> IntMap.IntMap Text -> Int -> Int -> IntSet.IntSet -> Int -> StateT (IntMap.IntMap (FieldValue, Int)) (Either String) (FieldValue, Int)
    visit values children keys childCount depth ancestors index = do
        when (depth >= 128) (lift (Left "nesting exceeds 128 value levels"))
        when (IntSet.member index ancestors) (lift (Left "cyclic child reference"))
        cached <- gets (IntMap.lookup index)
        case cached of
            Just result@(_, height) -> do
                when (depth + height > 128) (lift (Left "nesting exceeds 128 value levels"))
                pure result
            Nothing -> do
                (tag, offset) <- lift (arrayElement values index)
                let payload = BS.drop (fromIntegral offset) bytes
                result <- case tag of
                    29 -> nested True payload
                    30 -> nested False payload
                    _ -> do
                        value <- lift (decodeScalar tag payload)
                        pure (value, 1)
                modify' (IntMap.insert index result)
                pure result
      where
        nested object payload = do
            (count, rest) <- lift (readVarint payload)
            start <- if count == 0 then pure 0 else fst <$> lift (readVarint rest)
            unless (toInteger start + toInteger count <= toInteger childCount) $
                lift (Left "container child range out of bounds")
            entries <- forM [fromIntegral start .. fromIntegral start + fromIntegral count - 1] \childIndex -> do
                (key, childIndexValue) <- lift (arrayElement children childIndex)
                name <- case (object, key) of
                    (True, Just k) -> case IntMap.lookup (fromIntegral k) keys of
                        Just text -> pure text
                        Nothing -> lift (Left "missing object key")
                    (False, Nothing) -> pure Text.empty
                    _ -> lift (Left "container key validity does not match its tag")
                (item, height) <- visit values children keys childCount (depth + 1) (IntSet.insert index ancestors) (fromIntegral childIndexValue)
                pure ((name, item), height)
            let height = 1 + maximum (0 : map snd entries)
                items = map fst entries
            if object
                then do
                    unless (Set.size (Set.fromList (map fst items)) == length items) $
                        lift (Left "duplicate object key")
                    pure (variantObject items, height)
                else pure (FieldList (map snd items), height)

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

-- | Validate decimal metadata and its unscaled value.
checkDecimal :: Word8 -> Word8 -> Integer -> Either String ()
checkDecimal precision scale value
    | precision < 1 || precision > 38 || scale > precision = Left "invalid decimal precision or scale"
    | abs value >= 10 ^ precision = Left "unscaled decimal exceeds its precision"
    | otherwise = Right ()

-- | Decode the scalar tags from VariantLogicalType in DuckDB 1.5.
decodeScalar :: Word8 -> ByteString -> Either String FieldValue
decodeScalar tag bytes = case tag of
    0 -> Right FieldNull
    1 -> Right (FieldBool True)
    2 -> Right (FieldBool False)
    3 -> FieldInt8 . fromInteger <$> readSigned 1 bytes
    4 -> FieldInt16 . fromInteger <$> readSigned 2 bytes
    5 -> FieldInt32 . fromInteger <$> readSigned 4 bytes
    6 -> FieldInt64 . fromInteger <$> readSigned 8 bytes
    7 -> FieldHugeInt <$> readSigned 16 bytes
    8 -> FieldWord8 . fromInteger . fst <$> readUnsigned 1 bytes
    9 -> FieldWord16 . fromInteger . fst <$> readUnsigned 2 bytes
    10 -> FieldWord32 . fromInteger . fst <$> readUnsigned 4 bytes
    11 -> FieldWord64 . fromInteger . fst <$> readUnsigned 8 bytes
    12 -> FieldUHugeInt . fst <$> readUnsigned 16 bytes
    13 -> FieldFloat . castWord32ToFloat . fromInteger . fst <$> readUnsigned 4 bytes
    14 -> FieldDouble . castWord64ToDouble . fromInteger . fst <$> readUnsigned 8 bytes
    15 -> do
        (precision, rest) <- readVarint bytes
        (scale, digits) <- readVarint rest
        when (precision < 1 || precision > 38 || scale > precision) (Left "invalid decimal metadata")
        let count = if precision <= 4 then 2 else if precision <= 9 then 4 else if precision <= 18 then 8 else 16
        value <- readSigned count digits
        checkDecimal (fromIntegral precision) (fromIntegral scale) value
        pure (FieldDecimal (DecimalValue (fromIntegral precision) (fromIntegral scale) value))
    16 -> do
        string <- readString bytes
        FieldText <$> either (Left . show) Right (Text.decodeUtf8' string)
    17 -> FieldBlob <$> readString bytes
    18 -> do
        (biased, _) <- readUnsigned 16 bytes
        let value = biased `xor` (1 `shiftL` 127)
        pure (FieldUUID (UUID.fromWords64 (fromInteger (value `shiftR` 64)) (fromInteger value)))
    19 -> FieldDate . unbounded (\days -> addDays (toInteger days) (fromGregorian 1970 1 1)) . (fromInteger :: Integer -> Int32) <$> readSigned 4 bytes
    20 -> FieldTime . timeOfDay 1000000 . fromInteger <$> readSigned 8 bytes
    21 -> FieldTime . timeOfDay 1000000000 . fromInteger <$> readSigned 8 bytes
    22 -> FieldTimestamp . localTimestamp 1 . fromInteger <$> readSigned 8 bytes
    23 -> FieldTimestamp . localTimestamp 1000 . fromInteger <$> readSigned 8 bytes
    24 -> FieldTimestamp . localTimestamp 1000000 . fromInteger <$> readSigned 8 bytes
    25 -> FieldTimestamp . localTimestamp 1000000000 . fromInteger <$> readSigned 8 bytes
    26 -> readUnsigned 8 bytes >>= timeWithZone . fromInteger . fst
    27 -> FieldTimestampTZ . unbounded (posixSecondsToUTCTime . fromRational . (% 1000000) . toInteger) . (fromInteger :: Integer -> Int64) <$> readSigned 8 bytes
    28 -> do
        months <- readSigned 4 bytes
        days <- readSigned 4 (BS.drop 4 bytes)
        micros <- readSigned 8 (BS.drop 8 bytes)
        pure (FieldInterval (IntervalValue (fromInteger months) (fromInteger days) (fromInteger micros)))
    31 -> FieldBigNum . BigNum <$> (readString bytes >>= decodeBigNum)
    32 -> do
        bitBytes <- readString bytes
        case BS.uncons bitBytes of
            Just (padding, dat) -> do
                checkBit padding dat
                case BS.uncons dat of
                    Just (first, rest) -> do
                        let maskBits = paddingMask padding
                        unless (first .&. maskBits == maskBits) (Left "invalid native BIT padding")
                        pure (FieldBit (BitString padding (BS.cons (first .&. complement maskBits) rest)))
                    Nothing -> Left "empty BIT data"
            Nothing -> Left "missing BIT header"
    33 -> FieldGeometry . (`RawGeometry` Nothing) <$> readString bytes
    _ -> Left "unknown scalar tag"

-- | Build an object whose fields have the VARIANT type.
variantObject :: [(Text, FieldValue)] -> FieldValue
variantObject entries =
    FieldStruct
        StructValue
            { structValueFields = indexed [StructField name value | (name, value) <- entries]
            , structValueTypes = indexed [StructField name (LogicalTypeScalar DuckDBTypeVariant) | (name, _) <- entries]
            , structValueIndex = Map.fromList (zip (map fst entries) [0 ..])
            }
  where
    indexed items = listArray (0, length items - 1) items

-- | Interpret DuckDB's infinity sentinels before converting a finite value.
unbounded :: (Integral a, Bounded a) => (a -> b) -> a -> Unbounded b
unbounded convert value
    | value == maxBound = PosInfinity
    | value == negate maxBound = NegInfinity
    | otherwise = Finite (convert value)

-- | Convert ticks since the epoch to a timestamp without a time zone.
localTimestamp :: Integer -> Int64 -> LocalTimestamp
localTimestamp units = unbounded (utcToLocalTime utc . posixSecondsToUTCTime . fromRational . (% units) . toInteger)

-- | Convert ticks since midnight to a time of day. Midnight at the end of a day is 24:00:00.
timeOfDay :: Integer -> Int64 -> TimeOfDay
timeOfDay units ticks =
    let (hours, rest) = toInteger ticks `divMod` (3600 * units)
        (minutes, seconds) = rest `divMod` (60 * units)
     in TimeOfDay (fromInteger hours) (fromInteger minutes) (fromRational (seconds % units))

{- | Unpack DuckDB's TIMETZ bits. The high 40 bits hold microseconds since
midnight. The low 24 bits hold 57599 minus the offset in seconds.
-}
timeWithZone :: Word64 -> Either String FieldValue
timeWithZone packed = do
    let micros = fromIntegral (packed `shiftR` 24) :: Int64
        offset = 57599 - fromIntegral (packed .&. 0xffffff) :: Int
    when (offset `rem` 60 /= 0) (Left "TIMETZ offset cannot be represented in whole minutes")
    pure (FieldTimeTZ (TimeWithZone (timeOfDay 1000000 micros) (minutesToTimeZone (offset `div` 60))))

-- | Check a BIT's nonempty data and left-padding count.
checkBit :: Word8 -> ByteString -> Either String ()
checkBit padding bytes
    | padding > 7 || BS.null bytes = Left "BIT requires nonempty data and padding from 0 to 7"
    | otherwise = Right ()

-- | Get the high-bit mask used for native BIT padding.
paddingMask :: Word8 -> Word8
paddingMask padding = complement ((1 `shiftL` (8 - fromIntegral padding)) - 1)

-- | Decode BIGNUM's sign, checked three-byte length header, and magnitude.
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
    let wordBytes = finiteBitSize (0 :: Word) `div` 8
        firstCount = BS.length magnitude `mod` wordBytes
        (first, rest) = BS.splitAt firstCount magnitude
        chunks input
            | BS.null input = []
            | otherwise = let (part, remaining) = BS.splitAt wordBytes input in part : chunks remaining
        limbs = (if BS.null first then id else (first :)) (chunks rest)
        toWord = BS.foldl' (\n byte -> n `shiftL` 8 .|. fromIntegral byte) 0
    pure (integerFromWordList negative (map toWord limbs))
