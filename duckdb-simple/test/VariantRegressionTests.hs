{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module VariantRegressionTests (tests) where

import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (forM_, void, when)
import Data.Array (Array, listArray)
import qualified Data.ByteString as BS
import qualified Data.Geometry as G
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Database.DuckDB.FFI (pattern DuckDBTypeVariant)
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Database.DuckDB.Simple.FromField (BitString (..), DecimalValue (..), FieldValue (..), StructValue (..), UnionValue)
import Database.DuckDB.Simple.Generic (ViaDuckDB (..))
import Database.DuckDB.Simple.Geometry (RawGeometry (..), toRawGeometry)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), destroyLogicalType, logicalTypeFromRep)
import Database.DuckDB.Simple.Variant
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord64ToDouble)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestUtils (assertFailureIO)

-- | A generic record checks the bridge to composite type metadata.
newtype VariantRecord = VariantRecord {payload :: Variant}
    deriving stock (Eq, Show, Generic)
    deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB VariantRecord)

-- | Check VARIANT payloads against native decoding and parameter round trips.
tests :: TestTree
tests =
    testGroup
        "variant integration"
        [ testGroup
            "scalar payloads decode like native values"
            [ testCase (Text.unpack sql) $ withConnection ":memory:" \conn -> do
                [(Variant actual, native)] <- query_ conn (Query ("SELECT (" <> sql <> ")::VARIANT, " <> sql)) :: IO [(Variant, FieldValue)]
                actual @?= native
                roundTrip conn native
            | sql <- scalarCases
            ]
        , testCase "explicit casts bind plain parameters" $ withConnection ":memory:" \conn -> do
            [Only record] <- query_ conn "SELECT {'a': 1, 'b': 'x'}" :: IO [Only (StructValue FieldValue)]
            (query conn "SELECT ?::VARIANT, ?::VARIANT, ?::VARIANT, ?::VARIANT" (42 :: Int64, "two" :: Text, record, listArray (0, 1) [1, 2] :: Array Int Int64) :: IO [(Variant, Variant, Variant, Variant)])
                >>= (@?= [(Variant (FieldInt64 42), Variant (FieldText "two"), Variant (variantObject [("a", FieldInt32 1), ("b", FieldText "x")]), Variant (FieldList [FieldInt64 1, FieldInt64 2]))])
            (query conn "SELECT [NULL::VARIANT, ?::VARIANT, ?::VARIANT]" (42 :: Int64, "two" :: Text) :: IO [Only [Variant]])
                >>= (@?= [Only [Variant FieldNull, Variant (FieldInt64 42), Variant (FieldText "two")]])
            _ <- execute_ conn "CREATE TABLE variants (v VARIANT)"
            _ <- executeMany conn "INSERT INTO variants VALUES (?)" [Only (7 :: Int64), Only 8]
            _ <- execute conn "INSERT INTO variants VALUES (?)" (Only record)
            (query_ conn "SELECT v FROM variants" :: IO [Only Variant])
                >>= (@?= [Only (Variant (FieldInt64 7)), Only (Variant (FieldInt64 8)), Only (Variant (variantObject [("a", FieldInt32 1), ("b", FieldText "x")]))])
        , testCase "existing FromField instances read VARIANT payloads" $ withConnection ":memory:" \conn -> do
            (query_ conn "SELECT 42::BIGINT::VARIANT, 'x'::VARIANT, [1, 2, 3]::VARIANT" :: IO [(Int64, Text, [Int64])])
                >>= (@?= [(42, "x", [1, 2, 3])])
            (query_ conn "SELECT {'payload': {'x': 18446744073709551615::UBIGINT}::VARIANT}" :: IO [Only VariantRecord])
                >>= (@?= [Only (VariantRecord (Variant (variantObject [("x", FieldWord64 maxBound)])))])
        , testCase "bound payloads keep integer widths and decimal scale" $ withConnection ":memory:" \conn ->
            forM_ [("'-128'::TINYINT", FieldInt8 minBound), ("12.34::DECIMAL(4,2)", FieldDecimal (DecimalValue 4 2 1234)), ("255::UTINYINT", FieldWord8 maxBound)] \(sql, value) ->
                (query conn (Query ("SELECT variant_typeof(?) = variant_typeof((" <> sql <> ")::VARIANT)")) (Only (Variant value)) :: IO [Only Bool])
                    >>= (@?= [Only True])
        , testCase "floating special values retain type and bits" $ withConnection ":memory:" \conn -> do
            forM_ [0, -0.0, 1 / 0, -1 / 0, 0 / 0] \value -> do
                [Only (Variant actual)] <- query conn "SELECT ?" (Only (Variant (FieldDouble value)))
                case actual of
                    FieldDouble decoded
                        | isNaN value -> assertBool "expected Double NaN" (isNaN decoded)
                        | otherwise -> castDoubleToWord64 decoded @?= castDoubleToWord64 value
                    _ -> assertFailure (show actual)
            forM_ [0, -0.0, 1 / 0, -1 / 0, 0 / 0] \value -> do
                [Only (Variant actual)] <- query conn "SELECT ?" (Only (Variant (FieldFloat value)))
                case actual of
                    FieldFloat decoded
                        | isNaN value -> assertBool "expected Float NaN" (isNaN decoded)
                        | otherwise -> castFloatToWord32 decoded @?= castFloatToWord32 value
                    _ -> assertFailure (show actual)
        , testCase "TIMETZ offsets decode like native values" $ withConnection ":memory:" \conn -> do
            forM_ ["'12:00:00+01:23'::TIMETZ", "'23:59:59.999999-15:59'::TIMETZ", "'00:00:00+00'::TIMETZ"] \sql -> do
                [(Variant actual, native)] <- query_ conn (Query ("SELECT (" <> sql <> ")::VARIANT, " <> sql)) :: IO [(Variant, FieldValue)]
                actual @?= native
                roundTrip conn native
            assertFailureIO (query_ conn "SELECT '12:00:00+01:23:45'::TIMETZ::VARIANT" :: IO [Only Variant])
        , testCase "text and binary values preserve embedded NUL" $ withConnection ":memory:" \conn -> do
            roundTrip conn (FieldText "before\0after íslenska λ 😀")
            roundTrip conn (FieldBlob (BS.pack [0, 1, 127, 128, 255, 0]))
        , testCase "objects preserve case-sensitive names and heterogeneous children" $ withConnection ":memory:" \conn -> do
            let value =
                    variantObject
                        [ ("Case", FieldInt64 9007199254740993)
                        , ("case", FieldDecimal (DecimalValue 38 9 1234567890123456789))
                        , ("quote' íslenska λ", FieldList [FieldNull, FieldText "x", FieldList [], variantObject []])
                        ]
            roundTrip conn value
            (query conn "SELECT variant_extract(?, 'Case')::BIGINT, variant_extract(?, 'case')::DECIMAL(38,9)::VARCHAR" (Variant value, Variant value) :: IO [(Int64, Text)])
                >>= (@?= [(9007199254740993, "1234567890.123456789")])
        , testCase "empty containers and NULL stay distinct" $ withConnection ":memory:" \conn -> do
            forM_ [FieldNull, FieldList [], variantObject [], FieldList [FieldNull], variantObject [("x", FieldNull)]] (roundTrip conn)
            (query_ conn "SELECT NULL::VARIANT" :: IO [Only (Maybe Variant)]) >>= (@?= [Only Nothing])
            (query conn "SELECT ?" (Only (Variant FieldNull)) :: IO [Only FieldValue]) >>= (@?= [Only FieldNull])
        , testCase "ARRAY parameters and generic records contain VARIANT" $ withConnection ":memory:" \conn -> do
            let array = listArray (0, 2) [Variant (FieldInt8 1), Variant (FieldText "two"), Variant FieldNull]
            (query conn "SELECT ?" (Only array) :: IO [Only (Array Int Variant)]) >>= (@?= [Only array])
            let record = VariantRecord (Variant (variantObject [("x", FieldWord64 maxBound)]))
            (query conn "SELECT ?" (Only record) :: IO [Only VariantRecord]) >>= (@?= [Only record])
        , testCase "nullable ARRAY parameters preserve heterogeneous VARIANT values" $ withConnection ":memory:" \conn -> do
            let values = listArray (0, 3) [Nothing, Just (Variant (FieldInt64 42)), Just (Variant (FieldText "two")), Just (Variant (variantObject [("xs", FieldList [FieldNull, FieldBool True])]))]
            (query conn "SELECT typeof(?), ?" (values, values) :: IO [(Text, Array Int (Maybe Variant))]) >>= (@?= [("VARIANT[4]", values)])
        , testCase "empty ARRAY parameters retain VARIANT element type" $ withConnection ":memory:" \conn -> do
            let values = listArray (0, -1) [] :: Array Int (Maybe Variant)
            (query conn "SELECT typeof(?)" (Only values) :: IO [Only Text]) >>= (@?= [Only "VARIANT[ANY]"])
        , testCase "parameter binding supplies a complete VARIANT type" $ withConnection ":memory:" \conn -> do
            let value = variantObject [("xs", FieldList [FieldNull, FieldInt64 42])]
            roundTrip conn value
            (query conn "SELECT typeof(?)" (Only (Variant value)) :: IO [Only Text]) >>= (@?= [Only "VARIANT"])
        , testCase "the first VARIANT parameter does not end a streaming result" $ withConnection ":memory:" \conn ->
            withStatement conn "SELECT ?" \stmt -> do
                total <- Streaming.fold_ conn "SELECT i FROM range(100000) t(i)" 0 \acc (Only i) -> do
                    when (i == 0) (bind stmt [toField (Variant (FieldInt64 42))])
                    pure (acc + i)
                total @?= (sum [0 .. 99999] :: Int64)
        , testCase "VARIANT type construction raises an error" $ do
            result <- try (bracket (logicalTypeFromRep (LogicalTypeScalar DuckDBTypeVariant)) destroyLogicalType (const (pure ())))
            case result of
                Left err -> assertBool (displayException (err :: SomeException)) ("?::VARIANT" `isInfixOf` displayException err)
                Right () -> assertFailure "expected VARIANT type rejection"
        , testCase "VARIANT payloads and GEOMETRY CRS metadata bind together" $ withConnection ":memory:" \conn -> do
            [Only value] <- query_ conn "SELECT {'payload': 42::VARIANT, 'shape': NULL::GEOMETRY('OGC:CRS84')}" :: IO [Only (StructValue FieldValue)]
            (query conn "SELECT typeof(?)" (Only value) :: IO [Only Text]) >>= (@?= [Only "STRUCT(payload VARIANT, shape GEOMETRY('OGC:CRS84'))"])
            (query conn "SELECT ?" (Only value) :: IO [Only (StructValue FieldValue)]) >>= (@?= [Only value])
        , testCase "inactive VARIANT UNION members have complete types" $ withConnection ":memory:" \conn -> do
            [Only value] <- query_ conn "SELECT union_value(number := 42::BIGINT)::UNION(number BIGINT, payload VARIANT)" :: IO [Only (UnionValue FieldValue)]
            (query conn "SELECT ?" (Only value) :: IO [Only (UnionValue FieldValue)]) >>= (@?= [Only value])
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
                    \n (Only (Variant actual)) -> do
                        let expected = case n `mod` 3 of
                                0 -> FieldInt64 n
                                1 -> FieldText "text"
                                _ -> variantObject [("n", FieldInt64 n), ("xs", FieldList [FieldNull, FieldInt64 n])]
                        actual @?= expected
                        pure (n + 1)
                count @?= 5000
            | (mode, foldRows) <- [("materialized", fold_), ("deprecated streaming", Streaming.fold_)]
            ]
        , testCase "filtered and reordered rows use the right child offsets" $ withConnection ":memory:" \conn -> do
            rows <- query_ conn "SELECT {'n': i, 'xs': [i, i + 1]}::VARIANT FROM range(10000) t(i) WHERE i % 97 = 0 ORDER BY i DESC LIMIT 40"
            let expected = [Only (Variant (variantObject [("n", FieldInt64 n), ("xs", FieldList [FieldInt64 n, FieldInt64 (n + 1)])])) | n <- take 40 (reverse [0, 97 .. 9999])]
            rows @?= expected
        , testCase "file-backed values survive checkpoint and reopen" $
            bracket newDatabase removeDatabase \path -> do
                withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                    void (execute_ conn "CREATE TABLE stored AS SELECT i, {'n': i, 'xs': [i, NULL], 'text': i::VARCHAR}::VARIANT AS value, 'POINT (1 2)'::GEOMETRY('OGC:CRS84') AS geometry FROM range(10000) t(i)")
                    void (execute_ conn "CHECKPOINT")
                withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                    rows <- query_ conn "SELECT value FROM stored WHERE i % 97 = 0 ORDER BY i DESC LIMIT 40" :: IO [Only Variant]
                    let expected =
                            [ Only (Variant (variantObject [("n", FieldInt64 n), ("xs", FieldList [FieldInt64 n, FieldNull]), ("text", FieldText (Text.pack (show n)))]))
                            | n <- take 40 (reverse [0, 97 .. 9999])
                            ]
                    rows @?= expected
                    [Only geometry] <- query_ conn "SELECT geometry FROM stored LIMIT 1" :: IO [Only RawGeometry]
                    rawGeometryCRS geometry @?= Just "OGC:CRS84"
                    BS.length (rawGeometryWKB geometry) @?= 21
        , testCase "geometry payload keeps WKB; native VARIANT does not retain CRS" $ withConnection ":memory:" \conn -> do
            [Only geometry] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
            [Only value] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')::VARIANT" :: IO [Only Variant]
            value @?= geometryPayload (rawGeometryWKB geometry)
            (query conn "SELECT system.main.ST_GeomFromWKB(?)::VARIANT" (Only (rawGeometryWKB geometry)) :: IO [Only Variant]) >>= (@?= [Only value])
            (query conn "SELECT system.main.ST_CRS((system.main.ST_GeomFromWKB(?)::VARIANT)::GEOMETRY)" (Only (rawGeometryWKB geometry)) :: IO [Only (Maybe Text)]) >>= (@?= [Only Nothing])
            forM_ [variantPayload value, FieldList [variantPayload value], variantObject [("shape", variantPayload value)]] \input -> do
                result <- try (query conn "SELECT ?" (Only (Variant input)) :: IO [Only Variant]) :: IO (Either SomeException [Only Variant])
                case result of
                    Left err -> assertBool (displayException err) ("raw GEOMETRY binding requires explicit" `isInfixOf` displayException err)
                    Right _ -> assertFailure "expected raw geometry binding rejection"
            (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        , testCase "geometry payload retains empty layout tags and native NaN points" $ withConnection ":memory:" \conn ->
            forM_ ["GEOMETRYCOLLECTION ZM EMPTY", "MULTIPOLYGON M EMPTY", "POINT Z (NaN NaN 7)"] \wkt -> do
                [Only geometry] <- query conn "SELECT ?::GEOMETRY" (Only (wkt :: Text)) :: IO [Only RawGeometry]
                (query conn "SELECT system.main.ST_GeomFromWKB(?)::VARIANT" (Only (rawGeometryWKB geometry)) :: IO [Only Variant]) >>= (@?= [Only (geometryPayload (rawGeometryWKB geometry))])
        , testCase "geometry payload conversion ignores user macros with built-in names" $ withConnection ":memory:" \conn -> do
            [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY" :: IO [Only RawGeometry]
            _ <- execute_ conn "CREATE MACRO ST_AsText(x) AS 42"
            _ <- execute_ conn "CREATE MACRO ST_GeomFromWKB(x) AS 'POINT (9 9)'::GEOMETRY"
            (query conn "SELECT system.main.ST_GeomFromWKB(?)::VARIANT" (Only (rawGeometryWKB geometry)) :: IO [Only Variant]) >>= (@?= [Only (geometryPayload (rawGeometryWKB geometry))])
        , testCase "binary geometry payloads retain NaN bits and mixed member layouts" $ withConnection ":memory:" \conn ->
            forM_
                [ G.PointGeometry (G.PointXYZ (G.XYZ (castWord64ToDouble 0x7ff8000000000001) (castWord64ToDouble 0x7ff8000000000002) 7))
                , G.GeometryCollection (V.fromList [G.PointGeometry (G.PointXY (G.XY 1 2)), G.PointGeometry (G.PointXYZ (G.XYZ 3 4 5))])
                ]
                \shape -> do
                    raw <- either assertFailure pure (toRawGeometry shape)
                    (query conn "SELECT system.main.ST_GeomFromWKB(?)::VARIANT" (Only (rawGeometryWKB raw)) :: IO [Only Variant]) >>= (@?= [Only (geometryPayload (rawGeometryWKB raw))])
        , testCase "invalid values fail before binding and connection reuse succeeds" $ withConnection ":memory:" \conn -> do
            forM_
                [ FieldHugeInt (2 ^ (127 :: Int))
                , FieldUHugeInt (-1)
                , FieldUHugeInt (2 ^ (128 :: Int))
                , FieldDecimal (DecimalValue 0 0 0)
                , FieldDecimal (DecimalValue 5 6 1)
                , FieldDecimal (DecimalValue 3 0 1000)
                , FieldBit (BitString 8 (BS.singleton 0))
                , variantObject [("same", FieldNull), ("same", FieldBool True)]
                , variantObject [("", FieldBool True)]
                , variantObject [("first", FieldBool False), ("", FieldBool True)]
                , variantObject [("before\0after", FieldNull)]
                ]
                \value -> assertFailureIO (query conn "SELECT ?" (Only (Variant value)) :: IO [Only Variant])
            (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        ]

-- | Bind a payload as VARIANT without a cast and read it back.
roundTrip :: Connection -> FieldValue -> Assertion
roundTrip conn expected =
    (query conn "SELECT typeof(?) AS kind, ? AS value" (Variant expected, Variant expected) :: IO [(Text, Variant)]) >>= (@?= [("VARIANT", Variant expected)])

-- | A raw geometry payload has no CRS inside a VARIANT.
geometryPayload :: BS.ByteString -> Variant
geometryPayload wkb = Variant (FieldGeometry (RawGeometry wkb Nothing))

-- | SQL constructors for each scalar payload tag.
scalarCases :: [Text]
scalarCases =
    [ "NULL"
    , "TRUE"
    , "FALSE"
    , "'-128'::TINYINT"
    , "'-32768'::SMALLINT"
    , "'-2147483648'::INTEGER"
    , "9007199254740993::BIGINT"
    , "'-170141183460469231731687303715884105728'::HUGEINT"
    , "255::UTINYINT"
    , "65535::USMALLINT"
    , "4294967295::UINTEGER"
    , "18446744073709551615::UBIGINT"
    , "'340282366920938463463374607431768211455'::UHUGEINT"
    , "1.25::FLOAT"
    , "1.25::DOUBLE"
    , "12.34::DECIMAL(4,2)"
    , "-1234.56::DECIMAL(9,2)"
    , "123456789012.345::DECIMAL(18,3)"
    , "1234567890.123456789::DECIMAL(38,9)"
    , "'text λ'::VARCHAR"
    , "'\\x00\\xFF'::BLOB"
    , "'01234567-89ab-cdef-fedc-ba9876543210'::UUID"
    , "DATE '1970-01-02'"
    , "DATE 'infinity'"
    , "DATE '-infinity'"
    , "TIME '01:02:03.456789'"
    , "'01:02:03.456789012'::TIME_NS"
    , "'1970-01-01 00:00:01'::TIMESTAMP_S"
    , "'1969-12-31 23:59:59.999'::TIMESTAMP_MS"
    , "'1970-01-01 00:00:01.234567'::TIMESTAMP"
    , "'1970-01-01 00:00:01.234567891'::TIMESTAMP_NS"
    , "'infinity'::TIMESTAMP_NS"
    , "'1970-01-01 00:00:01.234567+00'::TIMESTAMPTZ"
    , "INTERVAL '-4 MONTHS 8 DAYS 123456789 MICROSECONDS'"
    , "'12345678901234567890123456789012345678901234567890'::BIGNUM"
    , "'-12345678901234567890123456789012345678901234567890'::BIGNUM"
    , "0::BIGNUM"
    , "'10101'::BIT"
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
