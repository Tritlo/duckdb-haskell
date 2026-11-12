module V2ValueTest (tests) where

import Data.Bits (shiftL, (.|.))
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..), CDouble, CFloat)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (output, readView, withBytes, withConnection, withFactory, withHandle, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 values"
        [ testCase "typed getters return the payload of their own type" testRoundTrip
        , testCase "converting getters cast a copy and leave the value unchanged" testConversion
        , testCase "getters reject NULL values and unsupported conversions" testRejection
        ]

type Create a = DuckDBV2FactoryHandle -> a -> Ptr DuckDBV2ValueHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

type Get a = DuckDBV2ValueHandle -> Ptr a -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error

-- | Create a value from a payload and read the payload back with the getter of the same type.
roundTrip :: (Storable a, Eq a, Show a) => DuckDBV2FactoryHandle -> Create a -> Get a -> a -> IO ()
roundTrip factory create get payload =
    withHandle (create factory payload) c_duckdb_v2_value_destroy $ \value ->
        output (get value) >>= (@?= payload)

-- | Check that the getter rejects the value with ERROR_INPUT_INVALID.
rejects :: (Storable a) => Get a -> DuckDBV2ValueHandle -> IO ()
rejects get value = alloca $ \out -> get value out nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)

-- | Read the backing integer, the width and the scale of a DECIMAL value.
readDecimal :: DuckDBV2ValueHandle -> IO (DuckDBV2HugeintT, Word8, Word8)
readDecimal value = alloca $ \width -> alloca $ \scale -> do
    integer <- output (\out -> c_duckdb_v2_value_get_decimal value out width scale)
    (,,) integer <$> peek width <*> peek scale

-- | Widen a signed integer to the two's complement 128-bit form.
hugeint :: Int64 -> DuckDBV2HugeintT
hugeint n = DuckDBV2HugeintT (fromIntegral n) (if n < 0 then -1 else 0)

testRoundTrip :: IO ()
testRoundTrip = withConnection $ \conn -> withFactory conn $ \factory -> do
    roundTrip factory c_duckdb_v2_value_create_bool c_duckdb_v2_value_get_bool (CBool 1)
    roundTrip factory c_duckdb_v2_value_create_bool c_duckdb_v2_value_get_bool (CBool 0)
    roundTrip factory c_duckdb_v2_value_create_tinyint c_duckdb_v2_value_get_tinyint (minBound :: Int8)
    roundTrip factory c_duckdb_v2_value_create_smallint c_duckdb_v2_value_get_smallint (minBound :: Int16)
    roundTrip factory c_duckdb_v2_value_create_int c_duckdb_v2_value_get_int (minBound :: Int32)
    roundTrip factory c_duckdb_v2_value_create_usmallint c_duckdb_v2_value_get_usmallint (maxBound :: Word16)
    roundTrip factory c_duckdb_v2_value_create_uint c_duckdb_v2_value_get_uint (maxBound :: Word32)
    roundTrip factory c_duckdb_v2_value_create_ubigint c_duckdb_v2_value_get_ubigint (maxBound :: Word64)
    roundTrip factory c_duckdb_v2_value_create_float c_duckdb_v2_value_get_float (-1.5 :: CFloat)
    roundTrip factory c_duckdb_v2_value_create_double c_duckdb_v2_value_get_double (2.25e300 :: CDouble)
    -- Each temporal payload is a raw count in the unit that the header documents.
    roundTrip factory c_duckdb_v2_value_create_date c_duckdb_v2_value_get_date (-719528 :: Int32)
    roundTrip factory c_duckdb_v2_value_create_time c_duckdb_v2_value_get_time (86399999999 :: Int64)
    roundTrip factory c_duckdb_v2_value_create_time_ns c_duckdb_v2_value_get_time_ns (86399999999999 :: Int64)
    -- 01:00:00 at UTC+01:00. The low 24 bits hold the offset, biased by 57599 seconds.
    roundTrip factory c_duckdb_v2_value_create_time_tz c_duckdb_v2_value_get_time_tz ((3600000000 `shiftL` 24) .|. (57599 - 3600) :: Word64)
    roundTrip factory c_duckdb_v2_value_create_timestamp c_duckdb_v2_value_get_timestamp (-1 :: Int64)
    roundTrip factory c_duckdb_v2_value_create_timestamp_sec c_duckdb_v2_value_get_timestamp_sec (-86401 :: Int64)
    roundTrip factory c_duckdb_v2_value_create_timestamp_ms c_duckdb_v2_value_get_timestamp_ms (1700000000123 :: Int64)
    roundTrip factory c_duckdb_v2_value_create_timestamp_ns c_duckdb_v2_value_get_timestamp_ns (1700000000123456789 :: Int64)
    roundTrip factory c_duckdb_v2_value_create_timestamp_tz c_duckdb_v2_value_get_timestamp_tz (-1700000000123456 :: Int64)
    roundTrip factory c_duckdb_v2_value_create_timestamp_tz_ns c_duckdb_v2_value_get_timestamp_tz_ns (1700000000123456789 :: Int64)
    let uuid = DuckDBV2HugeintT 0x0123456789abcdef (-0x1032547698badcff)
    with uuid $ \ptr ->
        withHandle (c_duckdb_v2_value_create_uuid factory (ConstPtr ptr)) c_duckdb_v2_value_destroy $ \value ->
            output (c_duckdb_v2_value_get_uuid value) >>= (@?= uuid)
    -- A width of 4 selects 16-bit storage. The getter widens it to 128 bits with the sign.
    let decimals = [(hugeint (-1234), 4, 1), (DuckDBV2HugeintT 0xfedcba9876543210 0x7fffffffffff, 38, 10)]
    mapM_
        ( \(integer, width, scale) -> with integer $ \ptr ->
            withHandle (c_duckdb_v2_value_create_decimal factory (ConstPtr ptr) width scale) c_duckdb_v2_value_destroy $ \value ->
                readDecimal value >>= (@?= (integer, width, scale))
        )
        decimals
    let bytes = [0, 255, 1, 128, 0]
    withBytes bytes $ \input ->
        withHandle (c_duckdb_v2_value_create_blob factory input) c_duckdb_v2_value_destroy $ \value -> do
            DuckDBV2Str ptr len <- output (c_duckdb_v2_value_get_blob value)
            peekArray (fromIntegral len) (castPtr (unConstPtr ptr) :: Ptr Word8) >>= (@?= bytes)

testConversion :: IO ()
testConversion = withConnection $ \conn -> withFactory conn $ \factory -> do
    withView "-300" $ \text -> withHandle (c_duckdb_v2_value_create_varchar factory text) c_duckdb_v2_value_destroy $ \value -> do
        output (c_duckdb_v2_value_get_smallint value) >>= (@?= (-300))
        output (c_duckdb_v2_value_get_int value) >>= (@?= (-300))
        output (c_duckdb_v2_value_get_double value) >>= (@?= (-300))
        output (c_duckdb_v2_value_get_float value) >>= (@?= (-300))
        alloca $ \out -> c_duckdb_v2_value_get_tinyint value out nullPtr >>= assertBool "-300 does not fit a TINYINT" . (/= DuckDBV2ErrorNone)
        alloca $ \out -> c_duckdb_v2_value_get_usmallint value out nullPtr >>= assertBool "-300 does not fit a USMALLINT" . (/= DuckDBV2ErrorNone)
        -- The VARCHAR getter does not convert, so it succeeds only while the value is still a VARCHAR.
        output (c_duckdb_v2_value_get_varchar value) >>= readView >>= (@?= "-300")
    withHandle (c_duckdb_v2_value_create_ubigint factory 70000) c_duckdb_v2_value_destroy $ \value -> do
        output (c_duckdb_v2_value_get_uint value) >>= (@?= 70000)
        output (c_duckdb_v2_value_get_bool value) >>= (@?= CBool 1)
        output (c_duckdb_v2_value_get_ubigint value) >>= (@?= 70000)
    -- 1970-01-02 00:00:05 in microseconds since the epoch.
    withHandle (c_duckdb_v2_value_create_timestamp factory 86405000000) c_duckdb_v2_value_destroy $ \value -> do
        output (c_duckdb_v2_value_get_date value) >>= (@?= 1)
        output (c_duckdb_v2_value_get_time value) >>= (@?= 5000000)
        output (c_duckdb_v2_value_get_timestamp_sec value) >>= (@?= 86405)
        output (c_duckdb_v2_value_get_timestamp_ms value) >>= (@?= 86405000)
        output (c_duckdb_v2_value_get_timestamp_ns value) >>= (@?= 86405000000000)
        output (c_duckdb_v2_value_get_timestamp value) >>= (@?= 86405000000)

testRejection :: IO ()
testRejection = withConnection $ \conn -> withFactory conn $ \factory -> do
    withHandle (c_duckdb_v2_factory_create_type_from_id factory DuckDBV2LogicalTypeIdBigInt (coerce nullPtr) (coerce nullPtr) 0) c_duckdb_v2_logical_type_destroy $ \ty ->
        withHandle (c_duckdb_v2_value_create_null factory ty) c_duckdb_v2_value_destroy $ \value -> do
            mapM_
                ($ value)
                [ rejects c_duckdb_v2_value_get_bool
                , rejects c_duckdb_v2_value_get_tinyint
                , rejects c_duckdb_v2_value_get_smallint
                , rejects c_duckdb_v2_value_get_int
                , rejects c_duckdb_v2_value_get_usmallint
                , rejects c_duckdb_v2_value_get_uint
                , rejects c_duckdb_v2_value_get_ubigint
                , rejects c_duckdb_v2_value_get_float
                , rejects c_duckdb_v2_value_get_double
                , rejects c_duckdb_v2_value_get_date
                , rejects c_duckdb_v2_value_get_time
                , rejects c_duckdb_v2_value_get_time_ns
                , rejects c_duckdb_v2_value_get_time_tz
                , rejects c_duckdb_v2_value_get_timestamp
                , rejects c_duckdb_v2_value_get_timestamp_sec
                , rejects c_duckdb_v2_value_get_timestamp_ms
                , rejects c_duckdb_v2_value_get_timestamp_ns
                , rejects c_duckdb_v2_value_get_timestamp_tz
                , rejects c_duckdb_v2_value_get_timestamp_tz_ns
                , rejects c_duckdb_v2_value_get_blob
                , rejects c_duckdb_v2_value_get_uuid
                ]
            alloca $ \width -> alloca $ \scale -> alloca $ \out ->
                c_duckdb_v2_value_get_decimal value out width scale nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
    -- BLOB, UUID and DECIMAL getters do not convert, so another type id is invalid.
    withHandle (c_duckdb_v2_value_create_bigint factory 7) c_duckdb_v2_value_destroy $ \value -> do
        rejects c_duckdb_v2_value_get_blob value
        rejects c_duckdb_v2_value_get_uuid value
        alloca $ \width -> alloca $ \scale -> alloca $ \out ->
            c_duckdb_v2_value_get_decimal value out width scale nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
    -- The default cast set has no cast from BLOB to a number, a time or a timestamp.
    withBytes [1, 2] $ \input -> withHandle (c_duckdb_v2_value_create_blob factory input) c_duckdb_v2_value_destroy $ \value ->
        mapM_
            ($ value)
            [ rejects c_duckdb_v2_value_get_int
            , rejects c_duckdb_v2_value_get_double
            , rejects c_duckdb_v2_value_get_date
            , rejects c_duckdb_v2_value_get_timestamp
            ]
