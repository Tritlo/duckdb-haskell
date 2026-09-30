{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module VariantRegressionTests (tests) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_, void, when)
import Data.Array (Array, listArray)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Database.DuckDB.FFI (DuckDBTimeTz (..), c_duckdb_create_time_tz)
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Database.DuckDB.Simple.FromField (FieldValue (..), StructValue, UnionValue)
import Database.DuckDB.Simple.Generic (ViaDuckDB (..))
import Database.DuckDB.Simple.Geometry (Geometry (..))
import Database.DuckDB.Simple.Variant
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | A generic record checks the bridge to composite type metadata.
newtype VariantRecord = VariantRecord {payload :: Variant}
    deriving stock (Eq, Show, Generic)
    deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB VariantRecord)

-- | Check exact values against SQL constructors and native parameter types.
tests :: TestTree
tests =
    testGroup
        "variant integration"
        [ testGroup
            "scalar tags"
            [ testCase (Text.unpack sql) $ withConnection ":memory:" \conn -> do
                (query_ conn (Query ("SELECT (" <> sql <> ")::VARIANT")) :: IO [Only Variant]) >>= (@?= [Only expected])
                roundTrip conn expected
            | (sql, expected) <- scalarCases
            ]
        , testCase "floating special values retain type and bits" $ withConnection ":memory:" \conn -> do
            forM_ [0, -0.0, 1 / 0, -1 / 0, 0 / 0] \value -> do
                [Only actual] <- query conn "SELECT ?" (Only (VariantDouble value))
                case actual of
                    VariantDouble decoded
                        | isNaN value -> assertBool "expected Double NaN" (isNaN decoded)
                        | otherwise -> castDoubleToWord64 decoded @?= castDoubleToWord64 value
                    _ -> assertFailure (show actual)
            forM_ [0, -0.0, 1 / 0, -1 / 0, 0 / 0] \value -> do
                [Only actual] <- query conn "SELECT ?" (Only (VariantFloat value))
                case actual of
                    VariantFloat decoded
                        | isNaN value -> assertBool "expected Float NaN" (isNaN decoded)
                        | otherwise -> castFloatToWord32 decoded @?= castFloatToWord32 value
                    _ -> assertFailure (show actual)
        , testCase "TIMETZ keeps a seconds-level offset" $ withConnection ":memory:" \conn -> do
            DuckDBTimeTz bits <- c_duckdb_create_time_tz 123456789012 37
            roundTrip conn (VariantTimeTZ bits)
        , testCase "text and binary values preserve embedded NUL" $ withConnection ":memory:" \conn -> do
            roundTrip conn (VariantText "before\0after íslenska λ 😀")
            roundTrip conn (VariantBlob (BS.pack [0, 1, 127, 128, 255, 0]))
        , testCase "objects preserve case-sensitive names and heterogeneous children" $ withConnection ":memory:" \conn -> do
            let value =
                    VariantObject
                        [ ("Case", VariantInt64 9007199254740993)
                        , ("case", VariantDecimal 38 9 1234567890123456789)
                        , ("quote' íslenska λ", VariantArray [VariantNull, VariantText "x", VariantArray [], VariantObject []])
                        ]
            roundTrip conn value
            (query conn "SELECT variant_extract(?, 'Case')::BIGINT, variant_extract(?, 'case')::DECIMAL(38,9)::VARCHAR" (value, value) :: IO [(Int64, Text)])
                >>= (@?= [(9007199254740993, "1234567890.123456789")])
        , testCase "empty containers and NULL stay distinct" $ withConnection ":memory:" \conn -> do
            forM_ [VariantNull, VariantArray [], VariantObject [], VariantArray [VariantNull], VariantObject [("x", VariantNull)]] (roundTrip conn)
            (query_ conn "SELECT NULL::VARIANT" :: IO [Only (Maybe Variant)]) >>= (@?= [Only Nothing])
            (query conn "SELECT ?" (Only VariantNull) :: IO [Only FieldValue]) >>= (@?= [Only FieldNull])
        , testCase "ARRAY parameters and generic records contain VARIANT" $ withConnection ":memory:" \conn -> do
            let array = listArray (0, 2) [VariantInt8 1, VariantText "two", VariantNull]
            (query conn "SELECT ?" (Only array) :: IO [Only (Array Int Variant)]) >>= (@?= [Only array])
            let record = VariantRecord (VariantObject [("x", VariantWord64 maxBound)])
            (query conn "SELECT ?" (Only record) :: IO [Only VariantRecord]) >>= (@?= [Only record])
        , testCase "native LIST, ARRAY, MAP and STRUCT metadata round trip" $ withConnection ":memory:" \conn -> do
            [Only value] <-
                query_
                    conn
                    "SELECT {'xs': [1::VARIANT, 'two'::VARIANT, NULL], 'fixed': [42::VARIANT]::VARIANT[1], 'map': MAP {'x': {'a': 9}::VARIANT}}" ::
                    IO [Only (StructValue FieldValue)]
            (query conn "SELECT ?" (Only value) :: IO [Only (StructValue FieldValue)]) >>= (@?= [Only value])
        , testCase "UNION payloads retain VARIANT type, including NULL" $ withConnection ":memory:" \conn ->
            forM_ ["SELECT union_value(v := {'a': 42}::VARIANT)", "SELECT union_value(v := NULL::VARIANT)"] \sql -> do
                [Only value] <- query_ conn sql :: IO [Only (UnionValue FieldValue)]
                (query conn "SELECT ?" (Only value) :: IO [Only (UnionValue FieldValue)]) >>= (@?= [Only value])
        , testGroup
            "heterogeneous folds cross chunk boundaries"
            [ testCase mode $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
                count <- foldRows
                    conn
                    "SELECT CASE WHEN i % 3 = 0 THEN i::VARIANT WHEN i % 3 = 1 THEN 'text'::VARIANT ELSE {'n': i, 'xs': [NULL, i]}::VARIANT END FROM range(5000) t(i)"
                    (0 :: Int64)
                    \n (Only actual) -> do
                        let expected = case n `mod` 3 of
                                0 -> VariantInt64 n
                                1 -> VariantText "text"
                                _ -> VariantObject [("n", VariantInt64 n), ("xs", VariantArray [VariantNull, VariantInt64 n])]
                        actual @?= expected
                        pure (n + 1)
                count @?= 5000
            | (mode, foldRows) <- [("materialized", fold_), ("deprecated streaming", Streaming.fold_)]
            ]
        , testCase "filtered and reordered rows use the right child offsets" $ withConnection ":memory:" \conn -> do
            rows <- query_ conn "SELECT {'n': i, 'xs': [i, i + 1]}::VARIANT FROM range(10000) t(i) WHERE i % 97 = 0 ORDER BY i DESC LIMIT 40"
            let expected = [Only (VariantObject [("n", VariantInt64 n), ("xs", VariantArray [VariantInt64 n, VariantInt64 (n + 1)])]) | n <- take 40 (reverse [0, 97 .. 9999])]
            rows @?= expected
        , testCase "file-backed values survive checkpoint and reopen" $
            bracket newDatabase removeDatabase \path -> do
                withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                    void (execute_ conn "CREATE TABLE stored AS SELECT i, {'n': i, 'xs': [i, NULL], 'text': i::VARCHAR}::VARIANT AS value, 'POINT (1 2)'::GEOMETRY('OGC:CRS84') AS geometry FROM range(10000) t(i)")
                    void (execute_ conn "CHECKPOINT")
                withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                    rows <- query_ conn "SELECT value FROM stored WHERE i % 97 = 0 ORDER BY i DESC LIMIT 40" :: IO [Only Variant]
                    let expected =
                            [ Only (VariantObject [("n", VariantInt64 n), ("xs", VariantArray [VariantInt64 n, VariantNull]), ("text", VariantText (Text.pack (show n)))])
                            | n <- take 40 (reverse [0, 97 .. 9999])
                            ]
                    rows @?= expected
                    [Only geometry] <- query_ conn "SELECT geometry FROM stored LIMIT 1" :: IO [Only Geometry]
                    geometryCRS geometry @?= Just "OGC:CRS84"
                    BS.length (geometryWKB geometry) @?= 21
        , testCase "geometry payload keeps WKB; native VARIANT does not retain CRS" $ withConnection ":memory:" \conn -> do
            [Only geometry] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')" :: IO [Only Geometry]
            [Only value] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')::VARIANT" :: IO [Only Variant]
            value @?= VariantGeometry (geometryWKB geometry)
            roundTrip conn value
            (query conn "SELECT ST_CRS(?::GEOMETRY)" (Only value) :: IO [Only (Maybe Text)]) >>= (@?= [Only Nothing])
        , testCase "invalid values fail before binding and connection reuse succeeds" $ withConnection ":memory:" \conn -> do
            forM_
                [ VariantHugeInt (2 ^ (127 :: Int))
                , VariantUHugeInt (-1)
                , VariantUHugeInt (2 ^ (128 :: Int))
                , VariantDecimal 0 0 0
                , VariantDecimal 5 6 1
                , VariantDecimal 3 0 1000
                , VariantBit 8 (BS.singleton 0)
                , VariantGeometry BS.empty
                , VariantObject [("same", VariantNull), ("same", VariantBool True)]
                , VariantObject [("before\0after", VariantNull)]
                ]
                \value -> do
                    result <- try (query conn "SELECT ?" (Only value) :: IO [Only Variant]) :: IO (Either SomeException [Only Variant])
                    case result of
                        Left _ -> pure ()
                        Right rows -> assertFailure ("accepted invalid value: " <> show rows)
            (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        ]

-- | Bind without a caller-supplied cast and check the native parameter type.
roundTrip :: Connection -> Variant -> Assertion
roundTrip conn expected =
    (query conn "SELECT typeof(?) AS kind, ? AS value" (expected, expected) :: IO [(Text, Variant)]) >>= (@?= [("VARIANT", expected)])

-- | SQL constructors are independent oracles for each scalar payload tag.
scalarCases :: [(Text, Variant)]
scalarCases =
    [ ("NULL", VariantNull)
    , ("TRUE", VariantBool True)
    , ("FALSE", VariantBool False)
    , ("'-128'::TINYINT", VariantInt8 minBound)
    , ("'-32768'::SMALLINT", VariantInt16 minBound)
    , ("'-2147483648'::INTEGER", VariantInt32 minBound)
    , ("9007199254740993::BIGINT", VariantInt64 9007199254740993)
    , ("'-170141183460469231731687303715884105728'::HUGEINT", VariantHugeInt (negate (2 ^ (127 :: Int))))
    , ("255::UTINYINT", VariantWord8 maxBound)
    , ("65535::USMALLINT", VariantWord16 maxBound)
    , ("4294967295::UINTEGER", VariantWord32 maxBound)
    , ("18446744073709551615::UBIGINT", VariantWord64 maxBound)
    , ("'340282366920938463463374607431768211455'::UHUGEINT", VariantUHugeInt (2 ^ (128 :: Int) - 1))
    , ("1.25::FLOAT", VariantFloat 1.25)
    , ("1.25::DOUBLE", VariantDouble 1.25)
    , ("12.34::DECIMAL(4,2)", VariantDecimal 4 2 1234)
    , ("-1234.56::DECIMAL(9,2)", VariantDecimal 9 2 (-123456))
    , ("123456789012.345::DECIMAL(18,3)", VariantDecimal 18 3 123456789012345)
    , ("1234567890.123456789::DECIMAL(38,9)", VariantDecimal 38 9 1234567890123456789)
    , ("'text λ'::VARCHAR", VariantText "text λ")
    , ("'\\x00\\xFF'::BLOB", VariantBlob (BS.pack [0, 255]))
    , ("'01234567-89ab-cdef-fedc-ba9876543210'::UUID", VariantUUID (UUID.fromWords 0x01234567 0x89abcdef 0xfedcba98 0x76543210))
    , ("DATE '1970-01-02'", VariantDate 1)
    , ("DATE 'infinity'", VariantDate maxBound)
    , ("DATE '-infinity'", VariantDate (negate maxBound))
    , ("TIME '01:02:03.456789'", VariantTimeMicros 3723456789)
    , ("'01:02:03.456789012'::TIME_NS", VariantTimeNanos 3723456789012)
    , ("'1970-01-01 00:00:01'::TIMESTAMP_S", VariantTimestampSeconds 1)
    , ("'1969-12-31 23:59:59.999'::TIMESTAMP_MS", VariantTimestampMillis (-1))
    , ("'1970-01-01 00:00:01.234567'::TIMESTAMP", VariantTimestampMicros 1234567)
    , ("'1970-01-01 00:00:01.234567891'::TIMESTAMP_NS", VariantTimestampNanos 1234567891)
    , ("'infinity'::TIMESTAMP_NS", VariantTimestampNanos maxBound)
    , ("'1970-01-01 00:00:01.234567+00'::TIMESTAMPTZ", VariantTimestampTZ 1234567)
    , ("INTERVAL '-4 MONTHS 8 DAYS 123456789 MICROSECONDS'", VariantInterval (-4) 8 123456789)
    , ("'12345678901234567890123456789012345678901234567890'::BIGNUM", VariantBigNum 12345678901234567890123456789012345678901234567890)
    , ("'-12345678901234567890123456789012345678901234567890'::BIGNUM", VariantBigNum (-12345678901234567890123456789012345678901234567890))
    , ("0::BIGNUM", VariantBigNum 0)
    , ("'10101'::BIT", VariantBit 3 (BS.singleton 21))
    ]

-- | Reserve a unique path and let DuckDB create its file.
newDatabase :: IO FilePath
newDatabase = do
    directory <- getTemporaryDirectory
    (path, handle) <- openBinaryTempFile directory "duckdb-variant"
    hClose handle
    removeFile path
    pure path

-- | Remove files created by this test after DuckDB closes them.
removeDatabase :: FilePath -> IO ()
removeDatabase path =
    forM_ [path, path <> ".wal"] \file -> do
        exists <- doesFileExist file
        when exists (removeFile file)
