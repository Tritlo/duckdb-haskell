{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module HelpersTest (tests) where

import Control.Exception (bracket)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Time.Calendar (diffDays, fromGregorian)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCStringLen)
import Foreign.C.Types (CBool (..), CDouble (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, pokeElemOff)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConstCString, withLogicalType)

tests :: TestTree
tests =
    testGroup
        "Helpers"
        [ testMallocFree
        , testVectorSize
        , testDateTimeHelpers
        , testStringHelpers
        , testHugeIntHelpers
        , testDecimalHelpers
        ]

testMallocFree :: TestTree
testMallocFree =
    testCase "duckdb_malloc/duckdb_free allocate and release memory" $ do
        ptr <- duckdb_malloc (CSize 128)
        assertBool "allocation should succeed" (ptr /= (coerce (nullPtr :: Ptr Void)))
        duckdb_free ptr

testVectorSize :: TestTree
testVectorSize =
    testCase "duckdb_vector_size returns positive number" $ do
        size <- duckdb_vector_size
        assertBool "vector size should be positive" (size > 0)

testDateTimeHelpers :: TestTree
testDateTimeHelpers =
    testCase "date/time/timestamp helper round-trips" $ do
        let day = fromGregorian 2023 6 1
            epoch = fromGregorian 1970 1 1
            days = diffDays day epoch
            duckDay = Duckdb_date (fromIntegral days)

        alloca \dateStructPtr -> do
            (duckdb_from_date duckDay >>= poke dateStructPtr)
            Duckdb_date_struct{year = y, month = m, day = d} <- peek dateStructPtr
            (y, m, d) @?= (2023, 6, 1)
            roundTrippedDay <- (peek dateStructPtr >>= \rawValue -> duckdb_to_date rawValue)
            roundTrippedDay @?= duckDay

        duckdb_is_finite_date duckDay >>= (@?= CBool 1)

        let microsPerSecond = 1000000
            timeMicros = ((12 * 60 + 34) * 60 + 56) * microsPerSecond
            duckTime = Duckdb_time (fromIntegral timeMicros)

        alloca \timeStructPtr -> do
            (duckdb_from_time duckTime >>= poke timeStructPtr)
            Duckdb_time_struct{hour = h, min = mi, sec = s, micros = mu} <- peek timeStructPtr
            (h, mi, s, mu) @?= (12, 34, 56, 0)
            roundTrippedTime <- (peek timeStructPtr >>= \rawValue -> duckdb_to_time rawValue)
            roundTrippedTime @?= duckTime

        Duckdb_time_tz tz <- duckdb_create_time_tz (fromIntegral @Integer timeMicros) 60
        alloca \timeTzPtr -> do
            (duckdb_from_time_tz (Duckdb_time_tz tz) >>= poke timeTzPtr)
            Duckdb_time_tz_struct{time = Duckdb_time_struct{hour = h', min = mi', sec = s'}, offset = offset} <- peek timeTzPtr
            (h', mi', s', offset) @?= (12, 34, 56, 60)

        let tsMicros :: Int64
            tsMicros = fromIntegral days * 86400000000 + fromIntegral timeMicros
            duckTimestamp = Duckdb_timestamp tsMicros

        alloca \tsStructPtr -> do
            (duckdb_from_timestamp duckTimestamp >>= poke tsStructPtr)
            Duckdb_timestamp_struct{date = Duckdb_date_struct{year = y', month = m', day = d'}, time = Duckdb_time_struct{hour = hour', min = minute', sec = sec', micros = micro'}} <-
                peek tsStructPtr
            (y', m', d', hour', minute', sec', micro') @?= (2023, 6, 1, 12, 34, 56, 0)
            roundTrippedTs <- (peek tsStructPtr >>= \rawValue -> duckdb_to_timestamp rawValue)
            roundTrippedTs @?= duckTimestamp

        duckdb_is_finite_timestamp duckTimestamp >>= (@?= CBool 1)
        duckdb_is_finite_timestamp_s (Duckdb_timestamp_s (tsMicros `div` 1000000)) >>= (@?= CBool 1)
        duckdb_is_finite_timestamp_ms (Duckdb_timestamp_ms (tsMicros `div` 1000)) >>= (@?= CBool 1)
        duckdb_is_finite_timestamp_ns (Duckdb_timestamp_ns (tsMicros * 1000)) >>= (@?= CBool 1)

testStringHelpers :: TestTree
testStringHelpers =
    testCase "string_t helpers inspect inline and heap strings" $ do
        inspectString "short" True
        inspectString (replicate 32 'x') False
  where
    inspectString text expectInline =
        withLogicalType (duckdb_create_logical_type (Duckdb_type DUCKDB_TYPE_VARCHAR)) \varcharType ->
            allocaArray 1 \typesPtr -> do
                pokeElemOff typesPtr 0 varcharType
                bracket
                    (duckdb_create_data_chunk typesPtr 1)
                    destroyDataChunk
                    \chunk -> do
                        vec <- duckdb_data_chunk_get_vector chunk 0
                        withConstCString text $ \cStr ->
                            duckdb_vector_assign_string_element vec 0 cStr
                        duckdb_data_chunk_set_size chunk 1
                        dataPtr <- duckdb_vector_get_data vec
                        let stringPtr = coerce dataPtr :: Ptr Duckdb_string_t
                        inlineFlag <- (peek stringPtr >>= \rawValue -> duckdb_string_is_inlined rawValue)
                        inlineFlag @?= if expectInline then CBool 1 else CBool 0
                        len <- (peek stringPtr >>= \rawValue -> duckdb_string_t_length rawValue)
                        len @?= fromIntegral (length text)
                        textPtr <- duckdb_string_t_data stringPtr
                        peekCStringLen (coerce textPtr, fromIntegral len) >>= (@?= text)

testHugeIntHelpers :: TestTree
testHugeIntHelpers =
    testCase "hugeint/u-hugeint helpers convert values" $ do
        alloca \hugePtr -> do
            let hugeVal = Duckdb_hugeint{lower = 123456789, upper = 0}
            poke hugePtr hugeVal
            CDouble dbl <- (peek hugePtr >>= \rawValue -> duckdb_hugeint_to_double rawValue)
            dbl @?= 123456789
            (duckdb_double_to_hugeint (CDouble 987654321) >>= poke hugePtr)
            Duckdb_hugeint{lower = lower, upper = upper} <- peek hugePtr
            (upper, lower) @?= (0, 987654321)

        alloca \uhugePtr -> do
            let uhugeVal = Duckdb_uhugeint{lower = 987654321, upper = 1}
            poke uhugePtr uhugeVal
            CDouble dbl <- (peek uhugePtr >>= \rawValue -> duckdb_uhugeint_to_double rawValue)
            dbl @?= fromIntegral @Integer (987654321 + 2 ^ (64 :: Int))
            (duckdb_double_to_uhugeint (CDouble 123456789) >>= poke uhugePtr)
            Duckdb_uhugeint{lower = lower, upper = upper} <- peek uhugePtr
            (upper, lower) @?= (0, 123456789)

testDecimalHelpers :: TestTree
testDecimalHelpers =
    testCase "decimal helper round-trip" $ do
        let input = CDouble 12345.67
        alloca \decimalPtr -> do
            (duckdb_double_to_decimal input 18 2 >>= poke decimalPtr)
            Duckdb_decimal{width = width, scale = scale} <- peek decimalPtr
            (width, scale) @?= (18, 2)
            result <- (peek decimalPtr >>= \rawValue -> duckdb_decimal_to_double rawValue)
            result @?= input

-- Utilities ----------------------------------------------------------------

destroyDataChunk :: Duckdb_data_chunk -> IO ()
destroyDataChunk chunk =
    alloca \ptr -> do
        poke ptr chunk
        duckdb_destroy_data_chunk ptr
