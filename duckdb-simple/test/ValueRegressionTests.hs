{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression tests for exact values and controlled conversion failures.
module ValueRegressionTests (main, valueRegressionTests) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_)
import Data.Array (Array, listArray)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), minutesToTimeZone, utc, utcToLocalTime)
import Data.Word (Word16, Word32, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (BitString (..), DecimalValue (..), FieldValue (..), TimeWithZone (..), bsFromBool)
import Database.DuckDB.Simple.Generic (ViaDuckDB (..), genericFromFieldValue, genericToStructValue)
import Database.DuckDB.Simple.Internal (withConnectionHandle)
import Database.DuckDB.Simple.LogicalRep
import Database.DuckDB.Simple.Time (Unbounded (..))
import Database.DuckDB.Simple.ToField (ToDuckValue (..))
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke)
import GHC.Generics (Generic)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

-- | Native timestamps used to test the full storage range.
data NativeTimestamp = Seconds Int64 | Milliseconds Int64 | Microseconds Int64 | Nanoseconds Int64
    deriving (Show)

instance DuckDBColumnType NativeTimestamp where
    duckdbColumnTypeFor _ = "TIMESTAMP"

instance ToDuckValue NativeTimestamp where
    toDuckValue (Seconds value) = c_duckdb_create_timestamp_s (DuckDBTimestampS value)
    toDuckValue (Milliseconds value) = c_duckdb_create_timestamp_ms (DuckDBTimestampMs value)
    toDuckValue (Microseconds value) = c_duckdb_create_timestamp (DuckDBTimestamp value)
    toDuckValue (Nanoseconds value) = c_duckdb_create_timestamp_ns (DuckDBTimestampNs value)

instance ToField NativeTimestamp

-- | Record with identical field types to detect positional decoding.
data NamedRecord = NamedRecord {firstValue :: Int64, secondValue :: Int64}
    deriving (Eq, Show, Generic)

-- | Sum with a payload to test NULL member handling.
data NullableSum = EmptyMember | DataMember Int64
    deriving (Eq, Show, Generic)

-- | Generic nullary constructors retain their existing UNION schema.
data Colour = Red | Blue
    deriving stock (Eq, Show, Generic)
    deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB Colour)

-- | Run this module without the main integration suite.
main :: IO ()
main = defaultMain valueRegressionTests

-- | Focused value regressions that also run against the baseline library.
valueRegressionTests :: TestTree
valueRegressionTests =
    testGroup "value regressions" $
        [ testCase "REAL decodes to Float and Double" $ withConnection ":memory:" \conn -> do
            (query_ conn "SELECT 1.25::REAL" :: IO [Only Float]) >>= (@?= [Only 1.25])
            (query_ conn "SELECT 1.25::REAL" :: IO [Only Double]) >>= (@?= [Only 1.25])
        , testCase "Float parameters still decode as Text" $ withConnection ":memory:" \conn ->
            (query conn "SELECT ?" (Only (1.25 :: Float)) :: IO [Only Text]) >>= (@?= [Only "1.25"])
        , testCase "generic nullary UNION constructors round trip through DuckDB" $ withConnection ":memory:" \conn ->
            forM_ [Red, Blue] \colour ->
                (query conn "SELECT ?" (Only colour) :: IO [Only Colour]) >>= (@?= [Only colour])
        , testCase "UNION NULL payload retains its member type" $ withConnection ":memory:" \conn -> do
            let members = listArray (0, 1) [UnionMemberType "number" (LogicalTypeScalar DuckDBTypeBigInt), UnionMemberType "text" (LogicalTypeScalar DuckDBTypeVarchar)]
                original = UnionValue 0 "number" FieldNull members
            (query conn "SELECT ?" (Only original) :: IO [Only (UnionValue FieldValue)]) >>= (@?= [Only original])
        , testCase "GEOMETRY decodes as well-known binary" $ withConnection ":memory:" \conn -> do
            [Only bytes] <- query_ conn "SELECT 'POINT(1 2)'::GEOMETRY" :: IO [Only BS.ByteString]
            BS.length bytes @?= 21
            (query conn "SELECT ST_AsText(ST_GeomFromWKB(?))" (Only bytes) :: IO [Only Text]) >>= (@?= [Only "POINT (1 2)"])
        , testCase "VARIANT has a controlled cast requirement" $ withConnection ":memory:" \conn -> do
            assertIOError (query_ conn "SELECT {'a': 1}::VARIANT" :: IO [Only FieldValue])
            (query_ conn "SELECT ({'a': 1}::VARIANT)::JSON" :: IO [Only Text]) >>= (@?= [Only "{\"a\":1}"])
        , testCase "Float parameter retains FLOAT type" $ withConnection ":memory:" \conn -> do
            (query conn "SELECT typeof(?), ?" (1.25 :: Float, 1.25 :: Float) :: IO [(Text, Float)]) >>= (@?= [("FLOAT", 1.25)])
        , testCase "Float special values survive decoding" $ withConnection ":memory:" \conn -> do
            [Only value] <- query_ conn "SELECT 'NaN'::REAL" :: IO [Only Float]
            assertBool "expected NaN" (isNaN value)
            [Only value'] <- query_ conn "SELECT 'Infinity'::REAL" :: IO [Only Double]
            assertBool "expected positive infinity" (isInfinite value' && value' > 0)
        , testCase "finite Double overflow to Float fails" $ withConnection ":memory:" \conn ->
            assertConversionError (query_ conn "SELECT 1e300::DOUBLE" :: IO [Only Float])
        , testCase "embedded NUL and Unicode Text round trip" $ withConnection ":memory:" \conn -> do
            let value = "before\0after íslenska λ 😀" :: Text
            (query conn "SELECT ?" (Only value) :: IO [Only Text]) >>= (@?= [Only value])
        , testCase "Int8 rejects both overflow directions" $ withConnection ":memory:" \conn -> do
            forM_ ["SELECT 128::BIGINT", "SELECT -129::SMALLINT"] \sql ->
                assertConversionError (query_ conn sql :: IO [Only Int8])
            (query_ conn "SELECT -128::BIGINT UNION ALL SELECT 127" :: IO [Only Int8]) >>= (@?= [Only (-128), Only 127])
        , testCase "unsigned narrowing rejects overflow" $ withConnection ":memory:" \conn -> do
            assertConversionError (query_ conn "SELECT 256::USMALLINT" :: IO [Only Word8])
            assertConversionError (query_ conn "SELECT 65536::UINTEGER" :: IO [Only Word16])
            assertConversionError (query_ conn "SELECT 4294967296::UBIGINT" :: IO [Only Word32])
        , testCase "finite timestamp extrema preserve all units" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
            forM_ [(Seconds, 1, "TIMESTAMP_S"), (Milliseconds, 1000, "TIMESTAMP_MS"), (Microseconds, 1000000, "TIMESTAMP"), (Nanoseconds, 1000000000, "TIMESTAMP_NS")] \(constructor, units, dtype) ->
                forM_ [minBound, negate (maxBound :: Int64) + 1, -1, 0, maxBound - 1] \value -> do
                    let expected = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (toInteger value % units)))
                    appendTimestamp conn dtype (toDuckValue (constructor value))
                    (query_ conn "SELECT value FROM native_timestamp" :: IO [Only LocalTime]) >>= (@?= [Only expected])
                    [Only original] <- query_ conn "SELECT {'value': value} FROM native_timestamp" :: IO [Only (StructValue FieldValue)]
                    -- DuckDB needs an explicit parameter type for extreme S/MS values.
                    query conn (fromString ("SELECT ?::STRUCT(value " <> dtype <> ")")) (Only original) >>= (@?= [Only original])
                    _ <- execute_ conn "DROP TABLE native_timestamp"
                    pure ()
        , testCase "composite timestamps reject infinity and storage overflow" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
            forM_ [(DuckDBTypeTimestampS, 1), (DuckDBTypeTimestampMs, 1000), (DuckDBTypeTimestamp, 1000000), (DuckDBTypeTimestampNs, 1000000000)] \(dtype, units) ->
                forM_ [toInteger (minBound :: Int64) - 1, negate (toInteger (maxBound :: Int64)), toInteger (maxBound :: Int64), toInteger (maxBound :: Int64) + 1] \value -> do
                    let timestamp = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (value % units)))
                        struct = singleField (LogicalTypeScalar dtype) (FieldTimestamp (Finite timestamp))
                    assertIOError (query conn "SELECT ?" (Only struct) :: IO [Only FieldValue])
        , testCase "composite timestamps floor fractional units before the epoch" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
            forM_ [(DuckDBTypeTimestampS, 1), (DuckDBTypeTimestampMs, 1000), (DuckDBTypeTimestamp, 1000000), (DuckDBTypeTimestampNs, 1000000000)] \(dtype, units) -> do
                let timestamp = utcToLocalTime utc (posixSecondsToUTCTime (fromRational ((-1) % (2 * units))))
                    expected = utcToLocalTime utc (posixSecondsToUTCTime (fromRational ((-1) % units)))
                    struct = singleField (LogicalTypeScalar dtype) (FieldTimestamp (Finite timestamp))
                (query conn "SELECT (?).value" (Only struct) :: IO [Only LocalTime]) >>= (@?= [Only expected])
        , testCase "composite TIME_NS rejects invalid clock components" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
            forM_ [TimeOfDay (-1) 0 0, TimeOfDay 25 0 0, TimeOfDay 0 60 0, TimeOfDay 0 0 (-1), TimeOfDay 24 0 0.000000001, TimeOfDay 23 59 60.000000001] \value ->
                assertIOError (query conn "SELECT ?" (Only (singleField (LogicalTypeScalar DuckDBTypeTimeNs) (FieldTime value))) :: IO [Only FieldValue])
        , testCase "microsecond timestamps retain finite extrema" $ withConnection ":memory:" \conn ->
            forM_ [minBound, negate (maxBound :: Int64) + 1, -1, 0, maxBound - 1] \value -> do
                let expected = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (toInteger value % 1000000)))
                _ <- execute_ conn "CREATE TABLE microsecond_timestamp(value TIMESTAMP)"
                _ <- execute conn "INSERT INTO microsecond_timestamp VALUES (?)" (Only expected)
                (query_ conn "SELECT value FROM microsecond_timestamp" :: IO [Only LocalTime]) >>= (@?= [Only expected])
                _ <- execute_ conn "DROP TABLE microsecond_timestamp"
                pure ()
        , testCase "out-of-range dates and timestamps fail without abort" $ withConnection ":memory:" \conn -> do
            let hugeDay = fromGregorian 10000000 1 1
                local = LocalTime hugeDay (TimeOfDay 0 0 0)
            assertIOError (query conn "SELECT ?" (Only hugeDay) :: IO [Only Day])
            assertIOError (query conn "SELECT ?" (Only local) :: IO [Only LocalTime])
            assertIOError (query conn "SELECT ?" (Only (UTCTime hugeDay 0)) :: IO [Only UTCTime])
        , testCase "date finite storage extrema round trip" $ withConnection ":memory:" \conn ->
            forM_ [minBound, negate (maxBound :: Int32) + 1, -1, 0, maxBound - 1] \value -> do
                let day = addDays (toInteger value) (fromGregorian 1970 1 1)
                (query conn "SELECT ?" (Only day) :: IO [Only Day]) >>= (@?= [Only day])
        , testCase "input infinity sentinels are rejected" $ withConnection ":memory:" \conn -> do
            forM_ [toInteger (maxBound :: Int32), negate (toInteger (maxBound :: Int32))] \days ->
                assertIOError (query conn "SELECT ?" (Only (addDays days (fromGregorian 1970 1 1))) :: IO [Only Day])
            forM_ [toInteger (maxBound :: Int64), negate (toInteger (maxBound :: Int64))] \micros -> do
                let value = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (micros % 1000000)))
                assertIOError (query conn "SELECT ?" (Only value) :: IO [Only LocalTime])
        , testCase "UTC binding is TIMESTAMPTZ under a non-UTC zone" $ withConnection ":memory:" \conn -> do
            _ <- execute_ conn "SET TimeZone = 'Pacific/Auckland'"
            let value = UTCTime (fromGregorian 2000 1 2) 12345
            (query conn "SELECT typeof(?), ?" (value, value) :: IO [(Text, UTCTime)]) >>= (@?= [("TIMESTAMP WITH TIME ZONE", value)])
            _ <- execute_ conn "CREATE TABLE utc_binding(value TIMESTAMPTZ)"
            _ <- execute conn "INSERT INTO utc_binding VALUES (?)" (Only value)
            (query_ conn "SELECT value FROM utc_binding" :: IO [Only UTCTime]) >>= (@?= [Only value])
        , testCase "DECIMAL(38) retains exact integer precision" $ withConnection ":memory:" \conn -> do
            let decimal = DecimalValue 38 9 12345678901234567890123456789012345678
                struct = singleField (LogicalTypeDecimal 38 9) (FieldDecimal decimal)
            [Only result] <- query conn "SELECT ?" (Only struct) :: IO [Only (StructValue FieldValue)]
            structValueFields result @?= structValueFields struct
        , testCase "invalid DECIMAL width scale and magnitude are rejected" $ withConnection ":memory:" \conn ->
            forM_ [DecimalValue 0 0 0, DecimalValue 39 0 0, DecimalValue 2 3 1, DecimalValue 2 0 100, DecimalValue 2 0 (-100)] \decimal ->
                assertIOError (query conn "SELECT ?" (Only (singleField (LogicalTypeDecimal (decimalWidth decimal) (decimalScale decimal)) (FieldDecimal decimal))) :: IO [Only FieldValue])
        , testCase "nested conversion failure leaves the connection usable" $ withConnection ":memory:" \conn -> do
            let days = listArray (0, 1) [fromGregorian 2000 1 1, fromGregorian 10000000 1 1] :: Array Int Day
            assertIOError (query conn "SELECT ?" (Only days) :: IO [Only FieldValue])
            (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        , testCase "generic NULL product and payload return Left" $ do
            assertLeft (genericFromFieldValue FieldNull :: Either String NamedRecord)
            let member = UnionMemberType "DataMember" (LogicalTypeScalar DuckDBTypeBigInt)
                union = UnionValue 0 "DataMember" FieldNull (listArray (0, 0) [member])
            assertLeft (genericFromFieldValue (FieldUnion union) :: Either String NullableSum)
        , testCase "generic records decode by name" $ withConnection ":memory:" \conn -> do
            [Only struct] <- query_ conn "SELECT {'secondValue': 2::BIGINT, 'firstValue': 1::BIGINT}" :: IO [Only (StructValue FieldValue)]
            genericFromFieldValue (FieldStruct struct) @?= Right (NamedRecord 1 2)
        , testCase "generic sums decode by member name" $ withConnection ":memory:" \conn -> do
            [Only union] <- query_ conn "SELECT union_value(DataMember := {'field1': 42::BIGINT})" :: IO [Only (UnionValue FieldValue)]
            genericFromFieldValue (FieldUnion union) @?= Right (DataMember 42)
        , testCase "union binding rejects inconsistent member name" $ withConnection ":memory:" \conn -> do
            let member = UnionMemberType "right" (LogicalTypeScalar DuckDBTypeBigInt)
                union = UnionValue 0 "wrong" (FieldInt64 42) (listArray (0, 0) [member])
            assertIOError (query conn "SELECT ?" (Only union) :: IO [Only FieldValue])
        , testCase "generic records reject wrong names" $ do
            case genericToStructValue (NamedRecord 1 2) of
                Nothing -> assertFailure "missing generic struct"
                Just struct -> do
                    let fields = listArray (0, 1) [StructField "wrong" (FieldInt64 1), StructField "secondValue" (FieldInt64 2)]
                    assertLeft (genericFromFieldValue (FieldStruct struct{structValueFields = fields}) :: Either String NamedRecord)
        , testCase "logical type Unicode names round trip" $ do
            let logical = LogicalTypeStruct (listArray (0, 0) [StructField "íslenska_λ_😀" (LogicalTypeScalar DuckDBTypeBigInt)])
            bracket (logicalTypeFromRep logical) destroyLogicalType \handle ->
                logicalTypeToRep handle >>= (@?= logical)
        , testCase "logical type names reject embedded NUL" $ do
            let logical = LogicalTypeStruct (listArray (0, 0) [StructField "before\0after" (LogicalTypeScalar DuckDBTypeBigInt)])
            assertIOError (bracket (logicalTypeFromRep logical) destroyLogicalType (const (pure ())))
        , testCase "invalid native composite constructor returns controlled error" $ withConnection ":memory:" \conn -> do
            let struct = singleField (LogicalTypeMap (LogicalTypeScalar DuckDBTypeBigInt) (LogicalTypeScalar DuckDBTypeBigInt)) (FieldMap [(FieldNull, FieldInt64 1)])
            assertIOError (query conn "SELECT ?" (Only struct) :: IO [Only FieldValue])
        , testCase "BIT padding preserves SQL bit_count" $ withConnection ":memory:" \conn -> do
            let bits = bsFromBool [True, False, True]
            (query conn "SELECT bit_count(?), ?" (bits, bits) :: IO [(Int64, BitString)]) >>= (@?= [(2, bits)])
        , testCase "unsupported BIT input fails before native use" $ withConnection ":memory:" \conn ->
            forM_ [BitString 0 BS.empty, BitString 8 (BS.singleton 1)] \bits ->
                assertIOError (query conn "SELECT ?" (Only bits) :: IO [Only BitString])
        , testCase "TIMETZ second offsets fail without rounding" $ withConnection ":memory:" \conn ->
            assertIOError (query_ conn "SELECT '12:00:00+01:23:45'::TIMETZ" :: IO [Only TimeWithZone])
        , testCase "ENUM payload index must be in its dictionary" $ withConnection ":memory:" \conn ->
            assertIOError (query conn "SELECT ?" (Only (singleField (LogicalTypeEnum (listArray (0, 1) ["a", "b"])) (FieldEnum 2))) :: IO [Only FieldValue])
        , testCase "TIMETZ input offset overflow fails before native use" $ withConnection ":memory:" \conn -> do
            let value = TimeWithZone (TimeOfDay 12 0 0) (minutesToTimeZone maxBound)
                struct = singleField (LogicalTypeScalar DuckDBTypeTimeTz) (FieldTimeTZ value)
            assertIOError (query conn "SELECT ?" (Only struct) :: IO [Only FieldValue])
        , testCase "invalid TIME input returns controlled error" $ withConnection ":memory:" \conn ->
            assertIOError (query conn "SELECT ?" (Only (TimeOfDay 1000000000 0 0)) :: IO [Only TimeOfDay])
        ]
            <> [ testCase ("composite temporal round trip: " <> dtype) $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
                    forM_ ("NULL" : map (\value -> "'" <> value <> "'") values) \value ->
                        assertTemporalRoundTrip conn dtype (value <> "::" <> dtype)
               | (dtype, values) <-
                    [(dtype, ["-infinity", "infinity", "1969-12-31 23:59:59.123456789", "2000-01-01 12:34:56.123456789"]) | dtype <- ["TIMESTAMP", "TIMESTAMP_S", "TIMESTAMP_MS", "TIMESTAMP_NS", "TIMESTAMPTZ"]]
                        <> [("DATE", ["-infinity", "infinity", "1969-12-31", "2000-01-01"])]
                        <> [("TIME_NS", ["00:00:00", "12:34:56.123456789", "23:59:59.999999999", "24:00:00"])]
               ]
            <> [ testCase (dtype <> " " <> value <> " rejects a finite result type") $ withConnection ":memory:" \conn ->
                    assertConversionError (query_ conn (fromString ("SELECT '" <> value <> "'::" <> dtype)) :: IO [Only LocalTime])
               | dtype <- ["DATE", "TIMESTAMP", "TIMESTAMP_S", "TIMESTAMP_MS", "TIMESTAMP_NS", "TIMESTAMPTZ"]
               , value <- ["infinity", "-infinity"]
               ]

-- | Rebind native temporal values in structs, unions, and nested collections.
assertTemporalRoundTrip :: Connection -> String -> String -> Assertion
assertTemporalRoundTrip conn dtype expression = do
    let sql =
            "WITH temporal AS (SELECT "
                <> expression
                <> " AS value) "
                <> "SELECT {'scalar': value, 'list': [value, NULL], 'array': [value, NULL]::"
                <> dtype
                <> "[2], 'map': map([1], [value])}, union_value(value := value) FROM temporal"
    [original] <- query_ conn (fromString sql) :: IO [(StructValue FieldValue, UnionValue FieldValue)]
    query conn "SELECT ?, ?" original >>= (@?= [original])

-- | Store raw timestamp units without the prepared-parameter type conversion.
appendTimestamp :: Connection -> String -> IO DuckDBValue -> IO ()
appendTimestamp conn dtype createValue = do
    _ <- execute_ conn (fromString ("CREATE TABLE native_timestamp(value " <> dtype <> ")"))
    withConnectionHandle conn \handle ->
        alloca \appenderPtr -> do
            poke appenderPtr nullPtr
            withCString "native_timestamp" \table ->
                c_duckdb_appender_create handle nullPtr table appenderPtr >>= (@?= DuckDBSuccess)
            bracket (peek appenderPtr) (const (c_duckdb_appender_destroy appenderPtr >> pure ())) \appender -> do
                bracket createValue (\value -> alloca \ptr -> poke ptr value >> c_duckdb_destroy_value ptr) \value ->
                    c_duckdb_append_value appender value >>= (@?= DuckDBSuccess)
                c_duckdb_appender_end_row appender >>= (@?= DuckDBSuccess)
                c_duckdb_appender_flush appender >>= (@?= DuckDBSuccess)

-- | Build a single-field struct for nested binding tests.
singleField :: LogicalTypeRep -> FieldValue -> StructValue FieldValue
singleField logical value =
    StructValue
        (listArray (0, 0) [StructField "value" value])
        (listArray (0, 0) [StructField "value" logical])
        (Map.singleton "value" 0)

-- | Assert a controlled input or materialization error.
assertIOError :: IO a -> Assertion
assertIOError action = do
    result <- try action
    case result of
        Left (_ :: IOException) -> pure ()
        Right _ -> assertFailure "expected IOException"

-- | Assert a controlled FromField conversion error.
assertConversionError :: IO a -> Assertion
assertConversionError action = do
    result <- try action
    case result of
        Left (_ :: SQLError) -> pure ()
        Right _ -> assertFailure "expected ResultError"

-- | Assert rejection without forcing a partial generic value.
assertLeft :: Either String a -> Assertion
assertLeft (Left _) = pure ()
assertLeft (Right _) = assertFailure "expected Left"
