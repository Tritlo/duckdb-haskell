{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module ValueInterfaceTest (tests) where

import Control.Monad (when, (>=>))
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString, peekCStringLen)
import Foreign.C.Types (CBool (..), CDouble (..), CFloat (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Marshal.Utils (with, withMany)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import GHC.Records (getField)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (destroyDuckValue, destroyLogicalType, withConstCString, withDuckValue)

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
        withDuckValue (duckdb_create_bool (CBool 1)) (duckdb_get_bool >=> (@?= CBool 1))

        withDuckValue (duckdb_create_int8 (-8)) (duckdb_get_int8 >=> (@?= (-8 :: Int8)))

        withDuckValue (duckdb_create_uint8 250) (duckdb_get_uint8 >=> (@?= (250 :: Word8)))

        withDuckValue (duckdb_create_int16 (-32000)) (duckdb_get_int16 >=> (@?= (-32000 :: Int16)))

        withDuckValue (duckdb_create_uint16 65000) (duckdb_get_uint16 >=> (@?= (65000 :: Word16)))

        withDuckValue
            (duckdb_create_int32 (-2000000000))
            ( duckdb_get_int32
                >=> (@?= (-2000000000 :: Int32))
            )

        withDuckValue
            (duckdb_create_uint32 4000000000)
            ( duckdb_get_uint32
                >=> (@?= (4000000000 :: Word32))
            )

        withDuckValue
            (duckdb_create_int64 (-9000000000000000000))
            ( duckdb_get_int64
                >=> (@?= (-9000000000000000000 :: Int64))
            )

        withDuckValue
            (duckdb_create_uint64 10000000000000000000)
            ( duckdb_get_uint64
                >=> (@?= (10000000000000000000 :: Word64))
            )

        alloca \hugePtr -> do
            poke hugePtr Duckdb_hugeint{lower = 123, upper = -1}
            withDuckValue ((peek hugePtr >>= \rawValue -> duckdb_create_hugeint rawValue)) \val ->
                alloca \out -> do
                    (duckdb_get_hugeint val >>= poke out)
                    peek out >>= (@?= Duckdb_hugeint 123 (-1))

        alloca \uhugePtr -> do
            poke uhugePtr Duckdb_uhugeint{lower = 321, upper = 2}
            withDuckValue ((peek uhugePtr >>= \rawValue -> duckdb_create_uhugeint rawValue)) \val ->
                alloca \out -> do
                    (duckdb_get_uhugeint val >>= poke out)
                    peek out >>= (@?= Duckdb_uhugeint 321 2)

        withArray (map (fromIntegral . fromEnum) "1234") \digitsPtr -> do
            let bignum = Duckdb_bignum digitsPtr 4 (CBool 0)
            with bignum \bignumPtr ->
                withDuckValue ((peek bignumPtr >>= \rawValue -> duckdb_create_bignum rawValue)) \val ->
                    alloca \outPtr -> do
                        (duckdb_get_bignum val >>= poke outPtr)
                        Duckdb_bignum outData outLen outNeg <- peek outPtr
                        outLen @?= 4
                        outNeg @?= CBool 0
                        peekArray (fromIntegral outLen) outData >>= (@?= map (fromIntegral . fromEnum) "1234")
                        when (outData /= (coerce (nullPtr :: Ptr Void))) $ duckdb_free (coerce outData)

        let hugeValue = Duckdb_hugeint{lower = 42, upper = 0}
            decimalValue = Duckdb_decimal{width = 10, scale = 2, value = hugeValue}
        with decimalValue \decimalPtr ->
            withDuckValue ((peek decimalPtr >>= \rawValue -> duckdb_create_decimal rawValue)) \val ->
                alloca \out -> do
                    (duckdb_get_decimal val >>= poke out)
                    Duckdb_decimal{width = w, scale = s, value = v} <- peek out
                    (w, s, v) @?= (10, 2, hugeValue)

        withDuckValue (duckdb_create_float (CFloat 1.25)) (duckdb_get_float >=> (@?= CFloat 1.25))

        withDuckValue (duckdb_create_double (CDouble 2.75)) (duckdb_get_double >=> (@?= CDouble 2.75))

        let sampleDate = Duckdb_date 12345
        withDuckValue (duckdb_create_date sampleDate) (duckdb_get_date >=> (@?= sampleDate))

        let sampleTime = Duckdb_time 987654321
        withDuckValue (duckdb_create_time sampleTime) (duckdb_get_time >=> (@?= sampleTime))

        let sampleTimeNs = Duckdb_time_ns 876543210
        withDuckValue (duckdb_create_time_ns sampleTimeNs) (duckdb_get_time_ns >=> (@?= sampleTimeNs))

        let sampleTimeTz = Duckdb_time_tz 5555
        withDuckValue (duckdb_create_time_tz_value sampleTimeTz) (duckdb_get_time_tz >=> (@?= sampleTimeTz))

        let sampleTimestamp = Duckdb_timestamp 444444
        withDuckValue (duckdb_create_timestamp sampleTimestamp) (duckdb_get_timestamp >=> (@?= sampleTimestamp))

        withDuckValue (duckdb_create_timestamp_tz sampleTimestamp) (duckdb_get_timestamp_tz >=> (@?= sampleTimestamp))

        let tsSeconds = Duckdb_timestamp_s 12
        withDuckValue (duckdb_create_timestamp_s tsSeconds) (duckdb_get_timestamp_s >=> (@?= tsSeconds))

        let tsMillis = Duckdb_timestamp_ms 12000
        withDuckValue (duckdb_create_timestamp_ms tsMillis) (duckdb_get_timestamp_ms >=> (@?= tsMillis))

        let tsNanos = Duckdb_timestamp_ns 12000000
        withDuckValue (duckdb_create_timestamp_ns tsNanos) (duckdb_get_timestamp_ns >=> (@?= tsNanos))

        let intervalVal = Duckdb_interval{months = 1, days = 2, micros = 3000}
        with intervalVal \intervalPtr ->
            withDuckValue ((peek intervalPtr >>= \rawValue -> duckdb_create_interval rawValue)) \val ->
                alloca \out -> do
                    (duckdb_get_interval val >>= poke out)
                    peek out >>= (@?= intervalVal)

        let blobBytes = map (fromIntegral . fromEnum) "duckdb-blob"
        withArray blobBytes \blobPtr ->
            withDuckValue (duckdb_create_blob (coerce blobPtr) (fromIntegral (length blobBytes))) \val ->
                alloca \blobOut -> do
                    (duckdb_get_blob val >>= poke blobOut)
                    Duckdb_blob{data' = datPtr, size = size} <- peek blobOut
                    size @?= fromIntegral (length blobBytes)
                    peekArray (fromIntegral size) (coerce datPtr :: Ptr Word8) >>= (@?= blobBytes)
                    when (datPtr /= (coerce (nullPtr :: Ptr Void))) $ duckdb_free (coerce datPtr)

        let bitBytes = [0, 0xAA :: Word8]
        withArray bitBytes \bitDataPtr -> do
            let bitVal = Duckdb_bit{data' = bitDataPtr, size = fromIntegral (length bitBytes)}
            with bitVal \bitPtr ->
                withDuckValue ((peek bitPtr >>= \rawValue -> duckdb_create_bit rawValue)) \val ->
                    alloca \bitOut -> do
                        (duckdb_get_bit val >>= poke bitOut)
                        Duckdb_bit{data' = datPtr, size = size} <- peek bitOut
                        size @?= fromIntegral (length bitBytes)
                        peekArray (fromIntegral size) datPtr >>= (@?= bitBytes)
                        when (datPtr /= (coerce (nullPtr :: Ptr Void))) $ duckdb_free (coerce datPtr)

        let uuidValue = Duckdb_uhugeint{lower = 0x0011223344556677, upper = 0x8899aabbccddeeff}
        with uuidValue \uuidPtr ->
            withDuckValue ((peek uuidPtr >>= \rawValue -> duckdb_create_uuid rawValue)) \val ->
                alloca \out -> do
                    (duckdb_get_uuid val >>= poke out)
                    peek out >>= (@?= uuidValue)

        withConstCString "varchar literal" \str ->
            withDuckValue (duckdb_create_varchar str) \val -> do
                cPtr <- duckdb_get_varchar val
                (peekCString . coerce) cPtr >>= (@?= "varchar literal")
                duckdb_free (coerce cPtr)

        let lenString = "hello\0world"
        withConstCString lenString \cStr -> do
            let byteLen = fromIntegral (length lenString)
            withDuckValue (duckdb_create_varchar_length cStr byteLen) \val -> do
                cPtr <- duckdb_get_varchar val
                peekCStringLen (coerce cPtr, length "hello") >>= (@?= "hello")
                duckdb_free (coerce cPtr)

        withDuckValue duckdb_create_null_value \val -> do
            duckdb_is_null_value val >>= (@?= CBool 1)
            strPtr <- duckdb_value_to_string val
            (peekCString . coerce) strPtr >>= (@?= "NULL")
            duckdb_free (coerce strPtr)

valueTypeReportsLogicalType :: TestTree
valueTypeReportsLogicalType =
    testCase "value type identifiers track constructors" $ do
        withDuckValue (duckdb_create_int32 42) \intVal -> do
            intType <- duckdb_get_value_type intVal
            (fmap (getField @"unwrap") . duckdb_get_type_id) intType >>= (@?= DUCKDB_TYPE_INTEGER)

        withDuckValue (withConstCString "duckdb" duckdb_create_varchar) \strVal -> do
            strType <- duckdb_get_value_type strVal
            (fmap (getField @"unwrap") . duckdb_get_type_id) strType >>= (@?= DUCKDB_TYPE_VARCHAR)

        listChild <- duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)
        listLogical <- duckdb_create_list_type listChild
        elemVal <- duckdb_create_int32 7
        withArray [elemVal] \valuesArray -> do
            let count = fromIntegral (1 :: Int)
            withDuckValue (duckdb_create_list_value listChild valuesArray count) \listVal -> do
                listType <- duckdb_get_value_type listVal
                (fmap (getField @"unwrap") . duckdb_get_type_id) listType >>= (@?= DUCKDB_TYPE_LIST)
        destroyDuckValue elemVal
        destroyLogicalType listLogical
        destroyLogicalType listChild

collectionValuesRoundTrip :: TestTree
collectionValuesRoundTrip =
    testCase "list/array/map/struct/enum/union constructors" $ do
        -- List value
        listChild <- duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)
        listLogical <- duckdb_create_list_type listChild
        listVal1 <- duckdb_create_int32 1
        listVal2 <- duckdb_create_int32 2
        withArray [listVal1, listVal2] \listArray -> do
            let entryCount = fromIntegral (2 :: Int) :: Idx_t
            withDuckValue (duckdb_create_list_value listChild listArray entryCount) \listVal -> do
                _ <- duckdb_get_list_size listVal
                child0 <- duckdb_get_list_child listVal 0
                duckdb_get_int32 child0 >>= (@?= 1)
                destroyDuckValue child0
                child1 <- duckdb_get_list_child listVal 1
                duckdb_get_int32 child1 >>= (@?= 2)
                destroyDuckValue child1
                listStr <- duckdb_value_to_string listVal
                (peekCString . coerce) listStr >>= (@?= "[1, 2]")
                duckdb_free (coerce listStr)
        destroyDuckValue listVal1
        destroyDuckValue listVal2
        destroyLogicalType listLogical
        destroyLogicalType listChild

        -- Array value
        arrayChild <- duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)
        arrayLogical <- duckdb_create_array_type arrayChild 2
        arrVal1 <- duckdb_create_int32 7
        arrVal2 <- duckdb_create_int32 8
        withArray [arrVal1, arrVal2] \arrArray -> do
            let entryCount = fromIntegral (2 :: Int) :: Idx_t
            withDuckValue (duckdb_create_array_value arrayChild arrArray entryCount) \arrVal -> do
                _ <- duckdb_get_list_size arrVal
                arrStr <- duckdb_value_to_string arrVal
                (peekCString . coerce) arrStr >>= (@?= "[7, 8]")
                duckdb_free (coerce arrStr)
        destroyDuckValue arrVal1
        destroyDuckValue arrVal2
        destroyLogicalType arrayLogical
        destroyLogicalType arrayChild

        -- Map value
        keyType <- duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_VARCHAR)
        valType <- duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)
        mapLogical <- duckdb_create_map_type keyType valType
        keyValue <- withConstCString "key" duckdb_create_varchar
        valValue <- duckdb_create_int32 99
        withArray [keyValue] \keyArray ->
            withArray [valValue] \valArray -> do
                let entryCount = fromIntegral (1 :: Int) :: Idx_t
                withDuckValue (duckdb_create_map_value mapLogical keyArray valArray entryCount) \mapVal -> do
                    duckdb_get_map_size mapVal >>= (@?= 1)
                    keyHandle <- duckdb_get_map_key mapVal 0
                    keyStrPtr <- duckdb_get_varchar keyHandle
                    (peekCString . coerce) keyStrPtr >>= (@?= "key")
                    duckdb_free (coerce keyStrPtr)
                    destroyDuckValue keyHandle
                    valHandle <- duckdb_get_map_value mapVal 0
                    duckdb_get_int32 valHandle >>= (@?= 99)
                    destroyDuckValue valHandle
        destroyDuckValue keyValue
        destroyDuckValue valValue
        destroyLogicalType mapLogical
        destroyLogicalType keyType
        destroyLogicalType valType

        -- Struct value
        structInt <- duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)
        structText <- duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_VARCHAR)
        structLogical <-
            withMany withConstCString ["id", "name"] \namePtrs ->
                withArray [structInt, structText] \typeArray ->
                    withArray namePtrs \nameArray ->
                        duckdb_create_struct_type typeArray nameArray 2
        withDuckValue (duckdb_create_int32 1) \idVal ->
            withDuckValue (withConstCString "Alice" duckdb_create_varchar) \nameVal ->
                withArray [idVal, nameVal] \structValues ->
                    withDuckValue (duckdb_create_struct_value structLogical structValues) \structVal -> do
                        child <- duckdb_get_struct_child structVal 1
                        namePtr <- duckdb_get_varchar child
                        (peekCString . coerce) namePtr >>= (@?= "Alice")
                        duckdb_free (coerce namePtr)
                        destroyDuckValue child
        destroyLogicalType structLogical
        destroyLogicalType structInt
        destroyLogicalType structText

        -- Enum value
        enumLogical <-
            withMany withConstCString ["Red", "Green", "Blue"] \namePtrs ->
                withArray namePtrs (`duckdb_create_enum_type` 3)
        withDuckValue (duckdb_create_enum_value enumLogical 1) (duckdb_get_enum_value >=> (@?= 1))
        destroyLogicalType enumLogical

        -- Union value
        unionInt <- duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_INTEGER)
        unionText <- duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_VARCHAR)
        unionLogical <-
            withMany withConstCString ["int_member", "text_member"] \namePtrs ->
                withArray [unionInt, unionText] \typeArray ->
                    withArray namePtrs \nameArray ->
                        duckdb_create_union_type typeArray nameArray 2
        withDuckValue (duckdb_create_int32 42) \unionPayload ->
            withDuckValue (duckdb_create_union_value unionLogical 0 unionPayload) \unionVal -> do
                strPtr <- duckdb_value_to_string unionVal
                (peekCString . coerce) strPtr >>= (@?= "union_value(int_member := 42)")
                duckdb_free (coerce strPtr)
        destroyLogicalType unionLogical
        destroyLogicalType unionInt
        destroyLogicalType unionText
