{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module BindValuesTest (tests) where

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (intercalate)
import Data.Time.Calendar (diffDays, fromGregorian)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString, peekCStringLen)
import Foreign.C.Types (CBool (..), CDouble (..), CFloat (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Bind Values"
        [bindValuesRoundtrip, bindTimestampTzAcrossSessions]

{- | Pin the session timezone so the tstz readback offset below is stable
regardless of the host OS timezone (see issue #8).
-}
setSessionTimeZoneSQL :: String
setSessionTimeZoneSQL = "SET TimeZone='UTC+02:00'"

bindValuesRoundtrip :: TestTree
bindValuesRoundtrip =
    testCase "bind every supported value type" $
        withDatabase \db ->
            withConnection db \conn -> do
                withConstCString setSessionTimeZoneSQL \tzSQL ->
                    alloca \resPtr -> do
                        st <- c_duckdb_query conn tzSQL resPtr
                        st @?= DuckDBSuccess
                        c_duckdb_destroy_result resPtr

                withConstCString createSQL \ddl ->
                    alloca \resPtr -> do
                        st <- c_duckdb_query conn ddl resPtr
                        st @?= DuckDBSuccess
                        c_duckdb_destroy_result resPtr

                -- values used for binding and verification
                let boolValue = CBool 1
                    tinyValue = (-5 :: Int8)
                    smallValue = (-300 :: Int16)
                    intValue = (-4000000 :: Int32)
                    bigValue = (-5000000000000 :: Int64)
                    u8Value = 200 :: Word8
                    u16Value = 60000 :: Word16
                    u32Value = 4000000000 :: Word32
                    u64Value = maxBound :: Word64
                    floatValue = CFloat 1.5
                    doubleValue = CDouble 2.5
                    dateDay = fromGregorian 2021 7 20
                    epoch = fromGregorian 1970 1 1
                    dateValue = DuckDBDate (fromIntegral (diffDays dateDay epoch))
                    timeMicros :: Integer
                    timeMicros = ((12 * 60 + 34) * 60 + 56) * 1000000
                    timeValue = DuckDBTime (fromIntegral timeMicros)
                    timestampValue = DuckDBTimestamp (fromIntegral (duckDBDateDays dateValue) * 86400000000 + fromIntegral timeMicros)
                    intervalValue = DuckDBInterval{months = 0, days = 1, micros = 7200000000}
                    decimalValue = DuckDBDecimal{width = 18, scale = 2, value = DuckDBHugeInt{lower = 1234567, upper = 0}}
                    hugeValue = DuckDBHugeInt{lower = 9223372036854775809, upper = 0}
                    uhugeValue = DuckDBUHugeInt{lower = 123456789, upper = 1}
                    varcharValue = "varchar binding"
                    varcharLenValue = "varchar length binding"
                    blobBytes :: [Word8]
                    blobBytes = map (fromIntegral . fromEnum) "abc"

                withConstCString insertSQL \cInsert ->
                    alloca \stmtPtr -> do
                        st <- c_duckdb_prepare conn cInsert stmtPtr
                        stmt <- peek stmtPtr
                        when (st /= DuckDBSuccess) $ do
                            msg <- if stmt == (coerce (nullPtr :: Ptr Void)) then pure "prepare failed" else c_duckdb_prepare_error stmt >>= (peekCString . coerce)
                            assertFailure msg
                        st @?= DuckDBSuccess
                        assertBool "prepared statement should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        withConstCString varcharValue \varcharPtr ->
                            withConstCString varcharLenValue \varcharLenPtr ->
                                withArray blobBytes \blobPtr ->
                                    alloca \hugePtr ->
                                        alloca \uhugePtr ->
                                            alloca \decimalPtr ->
                                                alloca \intervalPtr ->
                                                    alloca \valuePtr -> do
                                                        poke hugePtr hugeValue
                                                        poke uhugePtr uhugeValue
                                                        poke decimalPtr decimalValue
                                                        poke intervalPtr intervalValue

                                                        duckValue <- c_duckdb_create_bool boolValue
                                                        poke valuePtr duckValue
                                                        c_duckdb_bind_value stmt 1 duckValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_destroy_value valuePtr

                                                        c_duckdb_bind_boolean stmt 2 boolValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_int8 stmt 3 tinyValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_int16 stmt 4 smallValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_int32 stmt 5 intValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_int64 stmt 6 bigValue >>= (@?= DuckDBSuccess)
                                                        (peek hugePtr >>= \rawValue -> c_duckdb_bind_hugeint stmt 7 rawValue) >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_uint8 stmt 8 u8Value >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_uint16 stmt 9 u16Value >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_uint32 stmt 10 u32Value >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_uint64 stmt 11 u64Value >>= (@?= DuckDBSuccess)
                                                        (peek uhugePtr >>= \rawValue -> c_duckdb_bind_uhugeint stmt 12 rawValue) >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_float stmt 13 floatValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_double stmt 14 doubleValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_date stmt 15 dateValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_time stmt 16 timeValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_timestamp stmt 17 timestampValue >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_timestamp_tz stmt 18 timestampValue >>= (@?= DuckDBSuccess)
                                                        (peek intervalPtr >>= \rawValue -> c_duckdb_bind_interval stmt 19 rawValue) >>= (@?= DuckDBSuccess)
                                                        (peek decimalPtr >>= \rawValue -> c_duckdb_bind_decimal stmt 20 rawValue) >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_varchar stmt 21 varcharPtr >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_varchar_length stmt 22 varcharLenPtr (fromIntegral (length varcharLenValue) :: DuckDBIdx) >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_blob stmt 23 (coerce blobPtr) (fromIntegral (length blobBytes) :: DuckDBIdx) >>= (@?= DuckDBSuccess)
                                                        c_duckdb_bind_null stmt 24 >>= (@?= DuckDBSuccess)

                                                        alloca \execResPtr -> do
                                                            stExec <- c_duckdb_execute_prepared stmt execResPtr
                                                            stExec @?= DuckDBSuccess
                                                            c_duckdb_destroy_result execResPtr

                                                        c_duckdb_destroy_prepare stmtPtr

                -- Validate inserted row
                withConstCString selectSQL \cSelect ->
                    alloca \resPtr -> do
                        st <- c_duckdb_query conn cSelect resPtr
                        st @?= DuckDBSuccess

                        rowCount <- c_duckdb_row_count resPtr
                        rowCount @?= 1

                        c_duckdb_value_boolean resPtr 0 0 >>= (@?= CBool 1)
                        c_duckdb_value_boolean resPtr 1 0 >>= (@?= CBool 1)
                        c_duckdb_value_int8 resPtr 2 0 >>= (@?= (-5 :: Int8))
                        c_duckdb_value_int16 resPtr 3 0 >>= (@?= (-300 :: Int16))
                        c_duckdb_value_int32 resPtr 4 0 >>= (@?= (-4000000 :: Int32))
                        c_duckdb_value_int64 resPtr 5 0 >>= (@?= (-5000000000000 :: Int64))

                        alloca \hugePtr -> do
                            (c_duckdb_value_hugeint resPtr 6 0 >>= poke hugePtr)
                            peek hugePtr >>= (@?= DuckDBHugeInt{lower = 9223372036854775809, upper = 0})

                        c_duckdb_value_uint8 resPtr 7 0 >>= (@?= (200 :: Word8))
                        c_duckdb_value_uint16 resPtr 8 0 >>= (@?= (60000 :: Word16))
                        c_duckdb_value_uint32 resPtr 9 0 >>= (@?= (4000000000 :: Word32))
                        c_duckdb_value_uint64 resPtr 10 0 >>= (@?= maxBound)

                        alloca \uhugePtr -> do
                            (c_duckdb_value_uhugeint resPtr 11 0 >>= poke uhugePtr)
                            peek uhugePtr >>= (@?= DuckDBUHugeInt{lower = 123456789, upper = 1})

                        valFloat <- c_duckdb_value_float resPtr 12 0
                        realToFrac valFloat @?= (1.5 :: Double)
                        valDouble <- c_duckdb_value_double resPtr 13 0
                        realToFrac valDouble @?= (2.5 :: Double)

                        DuckDBDate fetchedDate <- c_duckdb_value_date resPtr 14 0
                        fetchedDate @?= duckDBDateDays dateValue

                        DuckDBTime fetchedTime <- c_duckdb_value_time resPtr 15 0
                        fetchedTime @?= duckDBTimeMicros timeValue

                        DuckDBTimestamp fetchedTs <- c_duckdb_value_timestamp resPtr 16 0
                        fetchedTs @?= duckDBTimestampMicros timestampValue

                        DuckDBTimestamp fetchedTsTz <- c_duckdb_value_timestamp resPtr 17 0
                        let tzDifference = fetchedTsTz - duckDBTimestampMicros timestampValue
                        tzDifference @?= 7200000000

                        alloca \intervalPtr -> do
                            (c_duckdb_value_interval resPtr 18 0 >>= poke intervalPtr)
                            peek intervalPtr >>= (@?= intervalValue)

                        alloca \decimalPtr -> do
                            (c_duckdb_value_decimal resPtr 19 0 >>= poke decimalPtr)
                            DuckDBDecimal{width = width, scale = scale} <- peek decimalPtr
                            (width, scale) @?= (18, 2)

                        varchar <- c_duckdb_value_varchar resPtr 20 0
                        (peekCString . coerce) varchar >>= (@?= varcharValue)
                        c_duckdb_free (coerce varchar)

                        alloca \stringPtr -> do
                            (c_duckdb_value_string resPtr 21 0 >>= poke stringPtr)
                            DuckDBString{data' = datPtr, size = datSize} <- peek stringPtr
                            peekCStringLen (coerce datPtr, fromIntegral datSize) >>= (@?= varcharLenValue)
                            when (datPtr /= (coerce (nullPtr :: Ptr Void))) $ c_duckdb_free (coerce datPtr)

                        alloca \blobPtr -> do
                            (c_duckdb_value_blob resPtr 22 0 >>= poke blobPtr)
                            DuckDBBlob{data' = blobDataPtr, size = blobSize} <- peek blobPtr
                            peekArray (fromIntegral blobSize) (coerce blobDataPtr :: Ptr Word8) >>= (@?= blobBytes)
                            c_duckdb_free (coerce blobDataPtr)

                        c_duckdb_value_is_null resPtr 23 0 >>= (@?= CBool 1)

                        c_duckdb_destroy_result resPtr

                -- Named parameter index lookup (separate statement)
                withConstCString "SELECT $named_param" \namedSQL ->
                    alloca \stmtPtr -> do
                        st <- c_duckdb_prepare conn namedSQL stmtPtr
                        stmt <- peek stmtPtr
                        when (st /= DuckDBSuccess) $ do
                            errPtr <- c_duckdb_prepare_error stmt
                            msg <- if errPtr == (coerce (nullPtr :: Ptr Void)) then pure "prepare failed" else (peekCString . coerce) errPtr
                            assertFailure msg
                        st @?= DuckDBSuccess
                        assertBool "named statement" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        alloca \idxPtr -> do
                            stIdx <- withConstCString "named_param" $ \name -> c_duckdb_bind_parameter_index stmt idxPtr name
                            when (stIdx /= DuckDBSuccess) $ do
                                errPtr <- c_duckdb_prepare_error stmt
                                msg <- if errPtr == (coerce (nullPtr :: Ptr Void)) then pure "bind_parameter_index failed" else (peekCString . coerce) errPtr
                                assertFailure msg
                            stIdx @?= DuckDBSuccess
                            idx <- peek idxPtr
                            idx @?= 1
                            bindState <- c_duckdb_bind_int32 stmt idx 42
                            when (bindState /= DuckDBSuccess) $ do
                                errPtr <- c_duckdb_prepare_error stmt
                                msg <- if errPtr == (coerce (nullPtr :: Ptr Void)) then pure "bind failed" else (peekCString . coerce) errPtr
                                assertFailure msg
                            bindState @?= DuckDBSuccess

                        alloca \execResPtr -> do
                            stExec <- c_duckdb_execute_prepared stmt execResPtr
                            stExec @?= DuckDBSuccess
                            c_duckdb_row_count execResPtr >>= (@?= 1)
                            c_duckdb_value_int32 execResPtr 0 0 >>= (@?= 42)
                            c_duckdb_destroy_result execResPtr

                        c_duckdb_destroy_prepare stmtPtr
  where
    createSQL =
        "CREATE TABLE bind_values ("
            <> "via_value BOOLEAN,"
            <> "bool_col BOOLEAN,"
            <> "tiny_col TINYINT,"
            <> "small_col SMALLINT,"
            <> "int_col INTEGER,"
            <> "big_col BIGINT,"
            <> "huge_col HUGEINT,"
            <> "uint8_col UTINYINT,"
            <> "uint16_col USMALLINT,"
            <> "uint32_col UINTEGER,"
            <> "uint64_col UBIGINT,"
            <> "uhuge_col HUGEINT,"
            <> "float_col FLOAT,"
            <> "double_col DOUBLE,"
            <> "date_col DATE,"
            <> "time_col TIME,"
            <> "ts_col TIMESTAMP,"
            <> "tstz_col TIMESTAMPTZ,"
            <> "interval_col INTERVAL,"
            <> "decimal_col DECIMAL(18,2),"
            <> "varchar_col VARCHAR,"
            <> "varchar_len_col VARCHAR,"
            <> "blob_col BLOB,"
            <> "named_null INTEGER"
            <> ")"

    insertSQL =
        "INSERT INTO bind_values VALUES ("
            <> intercalate ", " (replicate 24 "?")
            <> ")"

    -- Cast tstz_col back to TIMESTAMP so the deprecated safe-fetch
    -- 'duckdb_value_timestamp' can read it; the cast uses the pinned
    -- session TimeZone above, yielding bound + 02:00.
    selectSQL = "SELECT * REPLACE (tstz_col::TIMESTAMP AS tstz_col) FROM bind_values"

    duckDBDateDays (DuckDBDate d) = d
    duckDBTimeMicros (DuckDBTime t) = t
    duckDBTimestampMicros (DuckDBTimestamp t) = t

{- | Regression test for issue #8: reading a TIMESTAMPTZ column via
'duckdb_value_timestamp' converts to the session's TimeZone, so the
offset between the bound UTC instant and the readback must track the
session's TimeZone setting — not the host OS timezone.
-}
bindTimestampTzAcrossSessions :: TestTree
bindTimestampTzAcrossSessions =
    testCase "bind_timestamp_tz readback tracks session TimeZone" $
        withDatabase \db ->
            withConnection db \conn -> do
                withConstCString "CREATE TABLE tstz_only (ts TIMESTAMPTZ)" \ddl ->
                    alloca \resPtr -> do
                        c_duckdb_query conn ddl resPtr >>= (@?= DuckDBSuccess)
                        c_duckdb_destroy_result resPtr

                -- A fixed UTC instant: 2021-07-20 12:34:56 UTC
                let boundMicros :: Int64
                    boundMicros =
                        fromIntegral (diffDays (fromGregorian 2021 7 20) (fromGregorian 1970 1 1))
                            * 86400000000
                            + ((12 * 60 + 34) * 60 + 56) * 1000000
                    bound = DuckDBTimestamp boundMicros

                withConstCString "INSERT INTO tstz_only VALUES (?)" \cInsert ->
                    alloca \stmtPtr -> do
                        c_duckdb_prepare conn cInsert stmtPtr >>= (@?= DuckDBSuccess)
                        stmt <- peek stmtPtr
                        c_duckdb_bind_timestamp_tz stmt 1 bound >>= (@?= DuckDBSuccess)
                        alloca \execResPtr -> do
                            c_duckdb_execute_prepared stmt execResPtr >>= (@?= DuckDBSuccess)
                            c_duckdb_destroy_result execResPtr
                        c_duckdb_destroy_prepare stmtPtr

                -- For each session TimeZone, the readback wall-clock should be
                -- bound + offset, independent of the host OS TZ.
                let cases :: [(String, Int64)]
                    cases =
                        [ ("UTC", 0)
                        , ("UTC+02:00", 2 * 3600 * 1000000)
                        , ("UTC-05:00", (-5) * 3600 * 1000000)
                        , ("Etc/GMT-2", 2 * 3600 * 1000000)
                        ]

                mapM_ (assertTimeZoneOffset conn boundMicros) cases
  where
    assertTimeZoneOffset conn boundMicros (tz, expected) = do
        withConstCString ("SET TimeZone='" <> tz <> "'") \setTzSQL ->
            alloca \resPtr -> do
                c_duckdb_query conn setTzSQL resPtr >>= (@?= DuckDBSuccess)
                c_duckdb_destroy_result resPtr

        -- Cast TIMESTAMPTZ → TIMESTAMP in-query; the cast uses the
        -- session TimeZone just set, so the wall-clock readback carries
        -- the expected offset.
        withConstCString "SELECT ts::TIMESTAMP FROM tstz_only" \cSelect ->
            alloca \resPtr -> do
                c_duckdb_query conn cSelect resPtr >>= (@?= DuckDBSuccess)
                DuckDBTimestamp fetched <- c_duckdb_value_timestamp resPtr 0 0
                let msg = "TimeZone=" <> tz <> ": expected offset " <> show expected
                assertBool msg (fetched - boundMicros == expected)
                c_duckdb_destroy_result resPtr
