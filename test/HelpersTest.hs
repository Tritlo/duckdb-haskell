{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module HelpersTest (tests) where

import Data.Int (Int64)
import Data.Time.Calendar (diffDays, fromGregorian)
import Database.DuckDB.FFI
import Foreign.C.Types (CDouble (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Helpers"
    [ testMallocFree
    , testVectorSize
    , testDateTimeHelpers
    , testHugeIntHelpers
    , testDecimalHelpers
    ]

testMallocFree :: TestTree
testMallocFree =
  testCase "duckdb_malloc/duckdb_free allocate and release memory" $ do
    ptr <- c_duckdb_malloc (CSize 128)
    assertBool "allocation should succeed" (ptr /= nullPtr)
    c_duckdb_free ptr

testVectorSize :: TestTree
testVectorSize =
  testCase "duckdb_vector_size returns positive number" $ do
    size <- c_duckdb_vector_size
    assertBool "vector size should be positive" (size > 0)

testDateTimeHelpers :: TestTree
testDateTimeHelpers =
  testCase "date/time/timestamp helper round-trips" $ do
    let day = fromGregorian 2023 6 1
        epoch = fromGregorian 1970 1 1
        days = diffDays day epoch
        duckDay = DuckDBDate (fromIntegral days)

    alloca \dateStructPtr -> do
      c_duckdb_from_date duckDay dateStructPtr
      DuckDBDateStruct{duckDBDateStructYear = y, duckDBDateStructMonth = m, duckDBDateStructDay = d} <- peek dateStructPtr
      (y, m, d) @?= (2023, 6, 1)
      roundTrippedDay <- c_duckdb_to_date dateStructPtr
      roundTrippedDay @?= duckDay

    c_duckdb_is_finite_date duckDay >>= (@?= 1)

    let microsPerSecond = 1000000
        timeMicros = ((12 * 60 + 34) * 60 + 56) * microsPerSecond
        duckTime = DuckDBTime (fromIntegral timeMicros)

    alloca \timeStructPtr -> do
      c_duckdb_from_time duckTime timeStructPtr
      DuckDBTimeStruct{duckDBTimeStructHour = h, duckDBTimeStructMinute = mi, duckDBTimeStructSecond = s, duckDBTimeStructMicros = mu} <- peek timeStructPtr
      (h, mi, s, mu) @?= (12, 34, 56, 0)
      roundTrippedTime <- c_duckdb_to_time timeStructPtr
      roundTrippedTime @?= duckTime

    DuckDBTimeTz tz <- c_duckdb_create_time_tz (fromIntegral timeMicros) 60
    alloca \timeTzPtr -> do
      c_duckdb_from_time_tz (DuckDBTimeTz tz) timeTzPtr
      DuckDBTimeTzStruct{duckDBTimeTzStructTime = DuckDBTimeStruct{duckDBTimeStructHour = h', duckDBTimeStructMinute = mi', duckDBTimeStructSecond = s'}, duckDBTimeTzStructOffset = offset} <- peek timeTzPtr
      (h', mi', s', offset) @?= (12, 34, 56, 60)

    let tsMicros :: Int64
        tsMicros = fromIntegral days * 86400000000 + fromIntegral timeMicros
        duckTimestamp = DuckDBTimestamp tsMicros

    alloca \tsStructPtr -> do
      c_duckdb_from_timestamp duckTimestamp tsStructPtr
      DuckDBTimestampStruct{duckDBTimestampStructDate = DuckDBDateStruct{duckDBDateStructYear = y', duckDBDateStructMonth = m', duckDBDateStructDay = d'}, duckDBTimestampStructTime = DuckDBTimeStruct{duckDBTimeStructHour = hour', duckDBTimeStructMinute = minute', duckDBTimeStructSecond = sec', duckDBTimeStructMicros = micro'}} <-
        peek tsStructPtr
      (y', m', d', hour', minute', sec', micro') @?= (2023, 6, 1, 12, 34, 56, 0)
      roundTrippedTs <- c_duckdb_to_timestamp tsStructPtr
      roundTrippedTs @?= duckTimestamp

    c_duckdb_is_finite_timestamp duckTimestamp >>= (@?= 1)

testHugeIntHelpers :: TestTree
testHugeIntHelpers =
  testCase "hugeint/u-hugeint helpers convert values" $ do
    alloca \hugePtr -> do
      let hugeVal = DuckDBHugeInt{duckDBHugeIntLower = 123456789, duckDBHugeIntUpper = 0}
      poke hugePtr hugeVal
      CDouble dbl <- c_duckdb_hugeint_to_double hugePtr
      dbl @?= 123456789
      c_duckdb_double_to_hugeint (CDouble 987654321) hugePtr
      DuckDBHugeInt{duckDBHugeIntLower = lower, duckDBHugeIntUpper = upper} <- peek hugePtr
      (upper, lower) @?= (0, 987654321)

    alloca \uhugePtr -> do
      let uhugeVal = DuckDBUHugeInt{duckDBUHugeIntLower = 987654321, duckDBUHugeIntUpper = 1}
      poke uhugePtr uhugeVal
      CDouble dbl <- c_duckdb_uhugeint_to_double uhugePtr
      dbl @?= fromIntegral (987654321 + 2 ^ (64 :: Int))
      c_duckdb_double_to_uhugeint (CDouble 123456789) uhugePtr
      DuckDBUHugeInt{duckDBUHugeIntLower = lower, duckDBUHugeIntUpper = upper} <- peek uhugePtr
      (upper, lower) @?= (0, 123456789)

testDecimalHelpers :: TestTree
testDecimalHelpers =
  testCase "decimal helper round-trip" $ do
    let input = CDouble 12345.67
    alloca \decimalPtr -> do
      c_duckdb_double_to_decimal input 18 2 decimalPtr
      DuckDBDecimal{duckDBDecimalWidth = width, duckDBDecimalScale = scale} <- peek decimalPtr
      (width, scale) @?= (18, 2)
      result <- c_duckdb_decimal_to_double decimalPtr
      result @?= input
