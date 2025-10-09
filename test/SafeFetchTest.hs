{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module SafeFetchTest (tests) where

import Control.Monad (when)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Time.Calendar (diffDays, fromGregorian)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, peekCStringLen, withCString)
import Foreign.C.Types (CBool (..), CDouble (..), CFloat (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Safe Fetch Functions"
    [safeFetchRoundtrip]

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase action =
  withCString ":memory:" \path ->
    alloca \dbPtr -> do
      state <- c_duckdb_open path dbPtr
      state @?= DuckDBSuccess
      db <- peek dbPtr
      result <- action db
      c_duckdb_close dbPtr
      pure result

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db action =
  alloca \connPtr -> do
    state <- c_duckdb_connect db connPtr
    state @?= DuckDBSuccess
    conn <- peek connPtr
    result <- action conn
    c_duckdb_disconnect connPtr
    pure result

safeFetchRoundtrip :: TestTree
safeFetchRoundtrip =
  testCase "retrieve various types via safe fetch accessors" $
    withDatabase \db ->
      withConnection db \conn -> do
        let query =
              "SELECT "
                <> "TRUE AS b," -- 0
                <> "(-5)::TINYINT AS i8," -- 1
                <> "(-300)::SMALLINT AS i16," -- 2
                <> "(-4000000)::INTEGER AS i32," -- 3
                <> "(-5000000000000)::BIGINT AS i64," --4
                <> "('9223372036854775809')::HUGEINT AS huge," --5
                <> "200::UTINYINT AS u8," --6
                <> "60000::USMALLINT AS u16," --7
                <> "4000000000::UINTEGER AS u32," --8
                <> "('18446744073709551615')::UBIGINT AS u64," --9
                <> "('18446744073709551616')::HUGEINT AS uhuge," --10
                <> "1.5::REAL AS f32," --11
                <> "2.5::DOUBLE AS f64," --12
                <> "DATE '2021-07-20' AS d," --13
                <> "TIME '12:34:56' AS t," --14
                <> "TIMESTAMP '2021-07-20 12:34:56' AS ts," --15
                <> "INTERVAL '1 day 2 hours' AS iv," --16
                <> "('12345.67')::DECIMAL(18,2) AS dec," --17
                <> "'this string is definitely longer than inline storage'::VARCHAR AS txt," --18
                <> "CAST('abc' AS BLOB) AS blob_col," --19
                <> "NULL::INTEGER AS null_col" --20

        withCString query \cQuery ->
          alloca \resPtr -> do
            state <- c_duckdb_query conn cQuery resPtr
            when (state /= DuckDBSuccess) $ do
              errPtr <- c_duckdb_result_error resPtr
              errMsg <- if errPtr == nullPtr then pure "<no error>" else peekCString errPtr
              assertFailure ("duckdb_query failed: " <> errMsg)
            state @?= DuckDBSuccess

            -- Boolean
            c_duckdb_value_is_null resPtr 0 0 >>= (@?= CBool 0)
            c_duckdb_value_boolean resPtr 0 0 >>= (@?= CBool 1)

            -- Signed integers
            c_duckdb_value_int8 resPtr 1 0 >>= (@?= (-5 :: Int8))
            c_duckdb_value_int16 resPtr 2 0 >>= (@?= (-300 :: Int16))
            c_duckdb_value_int32 resPtr 3 0 >>= (@?= (-4000000 :: Int32))
            c_duckdb_value_int64 resPtr 4 0 >>= (@?= (-5000000000000 :: Int64))

            -- HugeInt
            alloca \hugePtr -> do
              c_duckdb_value_hugeint resPtr 5 0 hugePtr
              DuckDBHugeInt{duckDBHugeIntLower = lowerHuge, duckDBHugeIntUpper = upperHuge} <- peek hugePtr
              lowerHuge @?= 9223372036854775809
              upperHuge @?= 0

            -- Unsigned integers
            c_duckdb_value_uint8 resPtr 6 0 >>= (@?= (200 :: Word8))
            c_duckdb_value_uint16 resPtr 7 0 >>= (@?= (60000 :: Word16))
            c_duckdb_value_uint32 resPtr 8 0 >>= (@?= (4000000000 :: Word32))
            c_duckdb_value_uint64 resPtr 9 0 >>= (@?= (maxBound :: Word64))

            alloca \uhugePtr -> do
              c_duckdb_value_uhugeint resPtr 10 0 uhugePtr
              DuckDBUHugeInt{duckDBUHugeIntLower = lowerUHuge, duckDBUHugeIntUpper = upperUHuge} <- peek uhugePtr
              lowerUHuge @?= 0
              upperUHuge @?= 1

            -- Floating point
            valFloat <- c_duckdb_value_float resPtr 11 0
            realToFrac valFloat @?= (1.5 :: Double)
            valDouble <- c_duckdb_value_double resPtr 12 0
            realToFrac valDouble @?= (2.5 :: Double)

            -- Date/time/timestamp
            DuckDBDate dateVal <- c_duckdb_value_date resPtr 13 0
            let expectedDaysInteger = diffDays targetDay epochDay
                expectedDays = fromIntegral expectedDaysInteger :: Int32
            dateVal @?= expectedDays

            DuckDBTime timeVal <- c_duckdb_value_time resPtr 14 0
            let expectedTimeMicros :: Int64
                expectedTimeMicros = ((12 * 60 + 34) * 60 + 56) * 1000000
            timeVal @?= expectedTimeMicros

            DuckDBTimestamp tsVal <- c_duckdb_value_timestamp resPtr 15 0
            let expectedTimestampMicros :: Int64
                expectedTimestampMicros =
                  fromIntegral expectedDaysInteger * 86400000000 + expectedTimeMicros
            tsVal @?= expectedTimestampMicros

            -- Interval
            alloca \intervalPtr -> do
              c_duckdb_value_interval resPtr 16 0 intervalPtr
              DuckDBInterval{duckDBIntervalMonths = months, duckDBIntervalDays = days, duckDBIntervalMicros = micros} <- peek intervalPtr
              months @?= 0
              days @?= 1
              micros @?= 7200000000

            -- Decimal
            alloca \decimalPtr -> do
              c_duckdb_value_decimal resPtr 17 0 decimalPtr
              DuckDBDecimal{duckDBDecimalWidth = width, duckDBDecimalScale = scale, duckDBDecimalValue = decValue} <- peek decimalPtr
              width @?= 18
              scale @?= 2
              DuckDBHugeInt{duckDBHugeIntLower = decimalLower, duckDBHugeIntUpper = decimalUpper} <- pure decValue
              decimalLower @?= 1234567
              decimalUpper @?= 0

            -- Strings
            strVarchar <- c_duckdb_value_varchar resPtr 18 0
            varcharText <- peekCString strVarchar
            varcharText @?= stringLiteral
            c_duckdb_free (castPtr strVarchar)

            alloca \stringPtr -> do
              c_duckdb_value_string resPtr 18 0 stringPtr
              DuckDBString{duckDBStringData = datPtr, duckDBStringSize = datSize} <- peek stringPtr
              bytes <- peekCStringLen (datPtr, fromIntegral datSize)
              bytes @?= stringLiteral
              when (datPtr /= nullPtr) $ c_duckdb_free (castPtr datPtr)

            alloca \stringInternalPtr -> do
              c_duckdb_value_string_internal resPtr 18 0 stringInternalPtr
              DuckDBString{duckDBStringData = datPtr, duckDBStringSize = datSize} <- peek stringInternalPtr
              bytes <- peekCStringLen (datPtr, fromIntegral datSize)
              bytes @?= stringLiteral

            varcharInternal <- c_duckdb_value_varchar_internal resPtr 18 0
            peekCString varcharInternal >>= (@?= stringLiteral)

            -- Blob
            alloca \blobPtr -> do
              c_duckdb_value_blob resPtr 19 0 blobPtr
              DuckDBBlob{duckDBBlobData = blobData, duckDBBlobSize = blobSize} <- peek blobPtr
              let expectedBlob :: [Word8]
                  expectedBlob = map (fromIntegral . fromEnum) ("abc" :: String)
              peekArray (fromIntegral blobSize) (castPtr blobData :: Ptr Word8) >>= (@?= expectedBlob)
              c_duckdb_free (castPtr blobData)

            -- Null handling
            c_duckdb_value_is_null resPtr 20 0 >>= (@?= CBool 1)

            c_duckdb_destroy_result resPtr
  where
    stringLiteral :: String
    stringLiteral = "this string is definitely longer than inline storage"
    targetDay = fromGregorian 2021 7 20
    epochDay = fromGregorian 1970 1 1
