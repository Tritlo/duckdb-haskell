{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module ValueInterfaceTest (tests) where

import Control.Monad (when)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, peekCStringLen, withCString)
import Foreign.C.Types (CBool (..), CDouble (..), CFloat (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Marshal.Utils (with, withMany)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (destroyDuckValue, destroyLogicalType, withDuckValue)

tests :: TestTree
tests =
  testGroup
    "Value Interface"
    [ scalarCreatesRoundTrip
    , valueTypeReportsLogicalType
    , collectionValuesRoundTrip
    ]

scalarCreatesRoundTrip :: TestTree
scalarCreatesRoundTrip =
  testCase "scalar value constructors and accessors" $ do
    withDuckValue (c_duckdb_create_bool (CBool 1)) \val ->
      c_duckdb_get_bool val >>= (@?= CBool 1)

    withDuckValue (c_duckdb_create_int8 (-8)) \val ->
      c_duckdb_get_int8 val >>= (@?= (-8 :: Int8))

    withDuckValue (c_duckdb_create_uint8 250) \val ->
      c_duckdb_get_uint8 val >>= (@?= (250 :: Word8))

    withDuckValue (c_duckdb_create_int16 (-32000)) \val ->
      c_duckdb_get_int16 val >>= (@?= (-32000 :: Int16))

    withDuckValue (c_duckdb_create_uint16 65000) \val ->
      c_duckdb_get_uint16 val >>= (@?= (65000 :: Word16))

    withDuckValue (c_duckdb_create_int32 (-2000000000)) \val ->
      c_duckdb_get_int32 val >>= (@?= (-2000000000 :: Int32))

    withDuckValue (c_duckdb_create_uint32 4000000000) \val ->
      c_duckdb_get_uint32 val >>= (@?= (4000000000 :: Word32))

    withDuckValue (c_duckdb_create_int64 (-9000000000000000000)) \val ->
      c_duckdb_get_int64 val >>= (@?= (-9000000000000000000 :: Int64))

    withDuckValue (c_duckdb_create_uint64 10000000000000000000) \val ->
      c_duckdb_get_uint64 val >>= (@?= (10000000000000000000 :: Word64))

    alloca \hugePtr -> do
      poke hugePtr DuckDBHugeInt{duckDBHugeIntLower = 123, duckDBHugeIntUpper = -1}
      withDuckValue (c_duckdb_create_hugeint hugePtr) \val ->
        alloca \out -> do
          c_duckdb_get_hugeint val out
          peek out >>= (@?= DuckDBHugeInt 123 (-1))

    alloca \uhugePtr -> do
      poke uhugePtr DuckDBUHugeInt{duckDBUHugeIntLower = 321, duckDBUHugeIntUpper = 2}
      withDuckValue (c_duckdb_create_uhugeint uhugePtr) \val ->
        alloca \out -> do
          c_duckdb_get_uhugeint val out
          peek out >>= (@?= DuckDBUHugeInt 321 2)

    withArray (map (fromIntegral . fromEnum) "1234") \digitsPtr -> do
      let bignum = DuckDBBignum digitsPtr 4 (CBool 0)
      with bignum \bignumPtr ->
        withDuckValue (c_duckdb_create_bignum bignumPtr) \val ->
          alloca \outPtr -> do
            c_duckdb_get_bignum val outPtr
            DuckDBBignum outData outLen outNeg <- peek outPtr
            outLen @?= 4
            outNeg @?= CBool 0
            peekArray (fromIntegral outLen) outData >>= (@?= map (fromIntegral . fromEnum) "1234")
            when (outData /= nullPtr) $ c_duckdb_free (castPtr outData)

    let hugeValue = DuckDBHugeInt{duckDBHugeIntLower = 42, duckDBHugeIntUpper = 0}
        decimalValue = DuckDBDecimal{duckDBDecimalWidth = 10, duckDBDecimalScale = 2, duckDBDecimalValue = hugeValue}
    with decimalValue \decimalPtr ->
      withDuckValue (c_duckdb_create_decimal decimalPtr) \val ->
        alloca \out -> do
          c_duckdb_get_decimal val out
          DuckDBDecimal{duckDBDecimalWidth = w, duckDBDecimalScale = s, duckDBDecimalValue = v} <- peek out
          (w, s, v) @?= (10, 2, hugeValue)

    withDuckValue (c_duckdb_create_float (CFloat 1.25)) \val ->
      c_duckdb_get_float val >>= (@?= CFloat 1.25)

    withDuckValue (c_duckdb_create_double (CDouble 2.75)) \val ->
      c_duckdb_get_double val >>= (@?= CDouble 2.75)

    let sampleDate = DuckDBDate 12345
    withDuckValue (c_duckdb_create_date sampleDate) \val ->
      c_duckdb_get_date val >>= (@?= sampleDate)

    let sampleTime = DuckDBTime 987654321
    withDuckValue (c_duckdb_create_time sampleTime) \val ->
      c_duckdb_get_time val >>= (@?= sampleTime)

    let sampleTimeNs = DuckDBTimeNs 876543210
    withDuckValue (c_duckdb_create_time_ns sampleTimeNs) \val ->
      c_duckdb_get_time_ns val >>= (@?= sampleTimeNs)

    let sampleTimeTz = DuckDBTimeTz 5555
    withDuckValue (c_duckdb_create_time_tz_value sampleTimeTz) \val ->
      c_duckdb_get_time_tz val >>= (@?= sampleTimeTz)

    let sampleTimestamp = DuckDBTimestamp 444444
    withDuckValue (c_duckdb_create_timestamp sampleTimestamp) \val ->
      c_duckdb_get_timestamp val >>= (@?= sampleTimestamp)

    withDuckValue (c_duckdb_create_timestamp_tz sampleTimestamp) \val ->
      c_duckdb_get_timestamp_tz val >>= (@?= sampleTimestamp)

    let tsSeconds = DuckDBTimestampS 12
    withDuckValue (c_duckdb_create_timestamp_s tsSeconds) \val ->
      c_duckdb_get_timestamp_s val >>= (@?= tsSeconds)

    let tsMillis = DuckDBTimestampMs 12000
    withDuckValue (c_duckdb_create_timestamp_ms tsMillis) \val ->
      c_duckdb_get_timestamp_ms val >>= (@?= tsMillis)

    let tsNanos = DuckDBTimestampNs 12000000
    withDuckValue (c_duckdb_create_timestamp_ns tsNanos) \val ->
      c_duckdb_get_timestamp_ns val >>= (@?= tsNanos)

    let intervalVal = DuckDBInterval{duckDBIntervalMonths = 1, duckDBIntervalDays = 2, duckDBIntervalMicros = 3000}
    with intervalVal \intervalPtr ->
      withDuckValue (c_duckdb_create_interval intervalPtr) \val ->
        alloca \out -> do
          c_duckdb_get_interval val out
          peek out >>= (@?= intervalVal)

    let blobBytes = map (fromIntegral . fromEnum) "duckdb-blob"
    withArray blobBytes \blobPtr ->
      withDuckValue (c_duckdb_create_blob blobPtr (fromIntegral (length blobBytes))) \val ->
        alloca \blobOut -> do
          c_duckdb_get_blob val blobOut
          DuckDBBlob{duckDBBlobData = datPtr, duckDBBlobSize = size} <- peek blobOut
          size @?= fromIntegral (length blobBytes)
          peekArray (fromIntegral size) (castPtr datPtr :: Ptr Word8) >>= (@?= blobBytes)
          when (datPtr /= nullPtr) $ c_duckdb_free (castPtr datPtr)

    let bitBytes = [0xAA :: Word8]
    withArray bitBytes \bitDataPtr -> do
      let bitVal = DuckDBBit{duckDBBitData = bitDataPtr, duckDBBitSize = 8}
      with bitVal \bitPtr ->
        withDuckValue (c_duckdb_create_bit bitPtr) \val ->
          alloca \bitOut -> do
            c_duckdb_get_bit val bitOut
            DuckDBBit{duckDBBitData = datPtr, duckDBBitSize = size} <- peek bitOut
            size @?= 8
            peekArray (fromIntegral ((size + 7) `div` 8)) datPtr >>= (@?= bitBytes)
            when (datPtr /= nullPtr) $ c_duckdb_free (castPtr datPtr)

    let uuidValue = DuckDBUHugeInt{duckDBUHugeIntLower = 0x0011223344556677, duckDBUHugeIntUpper = 0x8899aabbccddeeff}
    with uuidValue \uuidPtr ->
      withDuckValue (c_duckdb_create_uuid uuidPtr) \val ->
        alloca \out -> do
          c_duckdb_get_uuid val out
          peek out >>= (@?= uuidValue)

    withCString "varchar literal" \str ->
      withDuckValue (c_duckdb_create_varchar str) \val -> do
        cPtr <- c_duckdb_get_varchar val
        peekCString cPtr >>= (@?= "varchar literal")
        c_duckdb_free (castPtr cPtr)

    let lenString = "hello\0world"
    withCString lenString \cStr -> do
      let byteLen = fromIntegral (length lenString)
      withDuckValue (c_duckdb_create_varchar_length cStr byteLen) \val -> do
        cPtr <- c_duckdb_get_varchar val
        peekCStringLen (cPtr, length "hello") >>= (@?= "hello")
        c_duckdb_free (castPtr cPtr)

    withDuckValue c_duckdb_create_null_value \val -> do
      c_duckdb_is_null_value val >>= (@?= CBool 1)
      strPtr <- c_duckdb_value_to_string val
      peekCString strPtr >>= (@?= "NULL")
      c_duckdb_free (castPtr strPtr)

valueTypeReportsLogicalType :: TestTree
valueTypeReportsLogicalType =
  testCase "value type identifiers track constructors" $ do
    withDuckValue (c_duckdb_create_int32 42) \intVal -> do
      intType <- c_duckdb_get_value_type intVal
      c_duckdb_get_type_id intType >>= (@?= DuckDBTypeInteger)

    withDuckValue (withCString "duckdb" c_duckdb_create_varchar) \strVal -> do
      strType <- c_duckdb_get_value_type strVal
      c_duckdb_get_type_id strType >>= (@?= DuckDBTypeVarchar)

    listChild <- c_duckdb_create_logical_type DuckDBTypeInteger
    listLogical <- c_duckdb_create_list_type listChild
    elemVal <- c_duckdb_create_int32 7
    withArray [elemVal] \valuesArray -> do
      let count = fromIntegral (1 :: Int)
      withDuckValue (c_duckdb_create_list_value listChild valuesArray count) \listVal -> do
        listType <- c_duckdb_get_value_type listVal
        c_duckdb_get_type_id listType >>= (@?= DuckDBTypeList)
    destroyDuckValue elemVal
    destroyLogicalType listLogical
    destroyLogicalType listChild

collectionValuesRoundTrip :: TestTree
collectionValuesRoundTrip =
  testCase "list/array/map/struct/enum/union constructors" $ do
    -- List value
    listChild <- c_duckdb_create_logical_type DuckDBTypeInteger
    listLogical <- c_duckdb_create_list_type listChild
    listVal1 <- c_duckdb_create_int32 1
    listVal2 <- c_duckdb_create_int32 2
    withArray [listVal1, listVal2] \listArray -> do
      let entryCount = fromIntegral (2 :: Int) :: DuckDBIdx
      withDuckValue (c_duckdb_create_list_value listChild listArray entryCount) \listVal -> do
        _ <- c_duckdb_get_list_size listVal
        child0 <- c_duckdb_get_list_child listVal 0
        c_duckdb_get_int32 child0 >>= (@?= 1)
        destroyDuckValue child0
        child1 <- c_duckdb_get_list_child listVal 1
        c_duckdb_get_int32 child1 >>= (@?= 2)
        destroyDuckValue child1
        listStr <- c_duckdb_value_to_string listVal
        peekCString listStr >>= (@?= "[1, 2]")
        c_duckdb_free (castPtr listStr)
    destroyDuckValue listVal1
    destroyDuckValue listVal2
    destroyLogicalType listLogical
    destroyLogicalType listChild

    -- Array value
    arrayChild <- c_duckdb_create_logical_type DuckDBTypeInteger
    arrayLogical <- c_duckdb_create_array_type arrayChild 2
    arrVal1 <- c_duckdb_create_int32 7
    arrVal2 <- c_duckdb_create_int32 8
    withArray [arrVal1, arrVal2] \arrArray -> do
      let entryCount = fromIntegral (2 :: Int) :: DuckDBIdx
      withDuckValue (c_duckdb_create_array_value arrayChild arrArray entryCount) \arrVal -> do
        _ <- c_duckdb_get_list_size arrVal
        arrStr <- c_duckdb_value_to_string arrVal
        peekCString arrStr >>= (@?= "[7, 8]")
        c_duckdb_free (castPtr arrStr)
    destroyDuckValue arrVal1
    destroyDuckValue arrVal2
    destroyLogicalType arrayLogical
    destroyLogicalType arrayChild

    -- Map value
    keyType <- c_duckdb_create_logical_type DuckDBTypeVarchar
    valType <- c_duckdb_create_logical_type DuckDBTypeInteger
    mapLogical <- c_duckdb_create_map_type keyType valType
    keyValue <- withCString "key" c_duckdb_create_varchar
    valValue <- c_duckdb_create_int32 99
    withArray [keyValue] \keyArray ->
      withArray [valValue] \valArray -> do
        let entryCount = fromIntegral (1 :: Int) :: DuckDBIdx
        withDuckValue (c_duckdb_create_map_value mapLogical keyArray valArray entryCount) \mapVal -> do
          c_duckdb_get_map_size mapVal >>= (@?= 1)
          keyHandle <- c_duckdb_get_map_key mapVal 0
          keyStrPtr <- c_duckdb_get_varchar keyHandle
          peekCString keyStrPtr >>= (@?= "key")
          c_duckdb_free (castPtr keyStrPtr)
          destroyDuckValue keyHandle
          valHandle <- c_duckdb_get_map_value mapVal 0
          c_duckdb_get_int32 valHandle >>= (@?= 99)
          destroyDuckValue valHandle
    destroyDuckValue keyValue
    destroyDuckValue valValue
    destroyLogicalType mapLogical
    destroyLogicalType keyType
    destroyLogicalType valType

    -- Struct value
    structInt <- c_duckdb_create_logical_type DuckDBTypeInteger
    structText <- c_duckdb_create_logical_type DuckDBTypeVarchar
    structLogical <-
      withMany withCString ["id", "name"] \namePtrs ->
        withArray [structInt, structText] \typeArray ->
          withArray namePtrs \nameArray ->
            c_duckdb_create_struct_type typeArray nameArray 2
    withDuckValue (c_duckdb_create_int32 1) \idVal ->
      withDuckValue (withCString "Alice" c_duckdb_create_varchar) \nameVal ->
        withArray [idVal, nameVal] \structValues ->
          withDuckValue (c_duckdb_create_struct_value structLogical structValues) \structVal -> do
            child <- c_duckdb_get_struct_child structVal 1
            namePtr <- c_duckdb_get_varchar child
            peekCString namePtr >>= (@?= "Alice")
            c_duckdb_free (castPtr namePtr)
            destroyDuckValue child
    destroyLogicalType structLogical
    destroyLogicalType structInt
    destroyLogicalType structText

    -- Enum value
    enumLogical <-
      withMany withCString ["Red", "Green", "Blue"] \namePtrs ->
        withArray namePtrs \nameArray ->
          c_duckdb_create_enum_type nameArray 3
    withDuckValue (c_duckdb_create_enum_value enumLogical 1) \enumVal ->
      c_duckdb_get_enum_value enumVal >>= (@?= 1)
    destroyLogicalType enumLogical

    -- Union value
    unionInt <- c_duckdb_create_logical_type DuckDBTypeInteger
    unionText <- c_duckdb_create_logical_type DuckDBTypeVarchar
    unionLogical <-
      withMany withCString ["int_member", "text_member"] \namePtrs ->
        withArray [unionInt, unionText] \typeArray ->
          withArray namePtrs \nameArray ->
            c_duckdb_create_union_type typeArray nameArray 2
    withDuckValue (c_duckdb_create_int32 42) \unionPayload ->
      withDuckValue (c_duckdb_create_union_value unionLogical 0 unionPayload) \unionVal -> do
        strPtr <- c_duckdb_value_to_string unionVal
        peekCString strPtr >>= (@?= "union_value(int_member := 42)")
        c_duckdb_free (castPtr strPtr)
    destroyLogicalType unionLogical
    destroyLogicalType unionInt
    destroyLogicalType unionText

