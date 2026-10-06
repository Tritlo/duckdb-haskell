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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Database.DuckDB.FFI (pattern DuckDBTypeVariant)
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Database.DuckDB.Simple.FromField (DecimalValue (..), FieldValue (..), StructValue (..), UnionValue (..))
import Database.DuckDB.Simple.Generic (ViaDuckDB (..))
import Database.DuckDB.Simple.Geometry (RawGeometry (..), toRawGeometry)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), destroyLogicalType, logicalTypeFromRep)
import Database.DuckDB.Simple.Variant
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord64ToDouble)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | A generic record checks the bridge to composite type metadata.
newtype VariantRecord = VariantRecord {payload :: Variant}
    deriving stock (Eq, Show, Generic)
    deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB VariantRecord)

-- | Check VARIANT payloads against native decoding and explicit parameter casts.
tests :: TestTree
tests =
    testGroup
        "variant integration"
        [ testGroup
            "scalar payloads decode like native values"
            [ testCase (Text.unpack sql) $ withConnection ":memory:" \conn -> do
                [(Variant actual, native)] <- query_ conn (Query ("SELECT (" <> sql <> ")::VARIANT, " <> sql)) :: IO [(Variant, FieldValue)]
                actual @?= native
            | sql <- scalarCases
            ]
        , testCase "explicit casts bind plain parameters" $ withConnection ":memory:" \conn -> do
            [Only record] <- query_ conn "SELECT {'a': 1, 'b': 'x'}" :: IO [Only (StructValue FieldValue)]
            (query conn "SELECT ?::VARIANT, ?::VARIANT, ?::VARIANT, ?::VARIANT" (42 :: Int64, "two" :: Text, record, listArray (0, 1) [1, 2] :: Array Int Int64) :: IO [(Variant, Variant, Variant, Variant)])
                >>= (@?= [(Variant (FieldInt64 42), Variant (FieldText "two"), Variant (object [("a", FieldInt32 1), ("b", FieldText "x")]), Variant (FieldList [FieldInt64 1, FieldInt64 2]))])
            (query conn "SELECT [NULL::VARIANT, ?::VARIANT, ?::VARIANT]" (42 :: Int64, "two" :: Text) :: IO [Only [Variant]])
                >>= (@?= [Only [Variant FieldNull, Variant (FieldInt64 42), Variant (FieldText "two")]])
            _ <- execute_ conn "CREATE TABLE variants (v VARIANT)"
            _ <- executeMany conn "INSERT INTO variants VALUES (?)" [Only (7 :: Int64), Only 8]
            _ <- execute conn "INSERT INTO variants VALUES (?)" (Only record)
            (query_ conn "SELECT v FROM variants" :: IO [Only Variant])
                >>= (@?= [Only (Variant (FieldInt64 7)), Only (Variant (FieldInt64 8)), Only (Variant (object [("a", FieldInt32 1), ("b", FieldText "x")]))])
        , testCase "floating special values retain type and bits" $ withConnection ":memory:" \conn -> do
            forM_ [0, -0.0, 1 / 0, -1 / 0, 0 / 0] \value -> do
                [Only (Variant actual)] <- query conn "SELECT ?::VARIANT" (Only (value :: Double))
                case actual of
                    FieldDouble decoded
                        | isNaN value -> assertBool "expected Double NaN" (isNaN decoded)
                        | otherwise -> castDoubleToWord64 decoded @?= castDoubleToWord64 value
                    _ -> assertFailure (show actual)
            forM_ [0, -0.0, 1 / 0, -1 / 0, 0 / 0] \value -> do
                [Only (Variant actual)] <- query conn "SELECT ?::VARIANT" (Only (value :: Float))
                case actual of
                    FieldFloat decoded
                        | isNaN value -> assertBool "expected Float NaN" (isNaN decoded)
                        | otherwise -> castFloatToWord32 decoded @?= castFloatToWord32 value
                    _ -> assertFailure (show actual)
        , testCase "TIMETZ offsets decode like native values" $ withConnection ":memory:" \conn -> do
            forM_ ["'12:00:00+01:23'::TIMETZ", "'23:59:59.999999-15:59'::TIMETZ", "'00:00:00+00'::TIMETZ"] \sql -> do
                [(Variant actual, native)] <- query_ conn (Query ("SELECT (" <> sql <> ")::VARIANT, " <> sql)) :: IO [(Variant, FieldValue)]
                actual @?= native
            assertFailureIO (query_ conn "SELECT '12:00:00+01:23:45'::TIMETZ::VARIANT" :: IO [Only Variant])
        , testCase "text and binary values preserve embedded NUL" $ withConnection ":memory:" \conn -> do
            (query conn "SELECT ?::VARIANT" (Only ("before\0after íslenska λ 😀" :: Text)) :: IO [Only Variant]) >>= (@?= [Only (Variant (FieldText "before\0after íslenska λ 😀"))])
            (query conn "SELECT ?::VARIANT" (Only (BS.pack [0, 1, 127, 128, 255, 0])) :: IO [Only Variant]) >>= (@?= [Only (Variant (FieldBlob (BS.pack [0, 1, 127, 128, 255, 0])))])
        , testCase "objects preserve entry order and heterogeneous children" $ withConnection ":memory:" \conn -> do
            let sql :: Text
                sql = "SELECT {'zeta': 9007199254740993::BIGINT, 'alpha': 1234567890.123456789::DECIMAL(38,9), 'quote'' íslenska λ': [NULL::VARIANT, 'x'::VARIANT, []::INTEGER[]::VARIANT, '{}'::JSON::VARIANT]}::VARIANT AS value"
            (query_ conn (Query sql) :: IO [Only Variant])
                >>= ( @?=
                        [ Only
                            ( Variant
                                ( object
                                    [ ("zeta", FieldInt64 9007199254740993)
                                    , ("alpha", FieldDecimal (DecimalValue 38 9 1234567890123456789))
                                    , ("quote' íslenska λ", FieldList [FieldNull, FieldText "x", FieldList [], object []])
                                    ]
                                )
                            )
                        ]
                    )
            (query_ conn (Query ("SELECT variant_extract(value, 'zeta')::BIGINT, variant_extract(value, 'alpha')::DECIMAL(38,9)::VARCHAR FROM (" <> sql <> ")")) :: IO [(Int64, Text)])
                >>= (@?= [(9007199254740993, "1234567890.123456789")])
        , testCase "empty containers and NULL stay distinct" $ withConnection ":memory:" \conn -> do
            (query_ conn "SELECT []::INTEGER[]::VARIANT, '{}'::JSON::VARIANT, [NULL]::INTEGER[]::VARIANT, {'x': NULL}::VARIANT" :: IO [(Variant, Variant, Variant, Variant)])
                >>= (@?= [(Variant (FieldList []), Variant (object []), Variant (FieldList [FieldNull]), Variant (object [("x", FieldNull)]))])
            (query_ conn "SELECT NULL::VARIANT" :: IO [Only (Maybe Variant)]) >>= (@?= [Only Nothing])
            (query_ conn "SELECT NULL::VARIANT" :: IO [Only FieldValue]) >>= (@?= [Only FieldNull])
        , testCase "existing FromField instances read VARIANT payloads" $ withConnection ":memory:" \conn -> do
            (query_ conn "SELECT 42::BIGINT::VARIANT, 'x'::VARIANT, [1, 2, 3]::VARIANT" :: IO [(Int64, Text, [Int64])])
                >>= (@?= [(42, "x", [1, 2, 3])])
            (query_ conn "SELECT {'payload': {'x': 18446744073709551615::UBIGINT}::VARIANT}" :: IO [Only VariantRecord])
                >>= (@?= [Only (VariantRecord (Variant (object [("x", FieldWord64 maxBound)])))])
        , testCase "native LIST, ARRAY and MAP containers decode VARIANT elements" $ withConnection ":memory:" \conn ->
            (query_ conn "SELECT [1::VARIANT, 'two'::VARIANT, NULL], [42::VARIANT]::VARIANT[1], MAP {'x': {'a': 9}::VARIANT}" :: IO [([Variant], Array Int Variant, Map Text Variant)])
                >>= (@?= [([Variant (FieldInt32 1), Variant (FieldText "two"), Variant FieldNull], listArray (0, 0) [Variant (FieldInt32 42)], Map.fromList [("x", Variant (object [("a", FieldInt32 9)]))])])
        , testCase "UNION payloads decode VARIANT, including NULL" $ withConnection ":memory:" \conn -> do
            [Only value] <- query_ conn "SELECT union_value(v := {'a': 42}::VARIANT)" :: IO [Only (UnionValue FieldValue)]
            unionValuePayload value @?= object [("a", FieldInt32 42)]
            [Only nullValue] <- query_ conn "SELECT union_value(v := NULL::VARIANT)" :: IO [Only (UnionValue FieldValue)]
            unionValuePayload nullValue @?= FieldNull
        , testCase "VARIANT type construction raises an error" $ do
            result <- try (bracket (logicalTypeFromRep (LogicalTypeScalar DuckDBTypeVariant)) destroyLogicalType (const (pure ())))
            case result of
                Left err -> assertBool (displayException (err :: SomeException)) ("?::VARIANT" `isInfixOf` displayException err)
                Right () -> assertFailure "expected VARIANT type rejection"
        , testCase "composite parameters with VARIANT members raise an error" $ withConnection ":memory:" \conn -> do
            [Only struct] <- query_ conn "SELECT {'payload': 42::VARIANT, 'shape': NULL::GEOMETRY('OGC:CRS84')}" :: IO [Only (StructValue FieldValue)]
            [Only nullStruct] <- query_ conn "SELECT {'payload': NULL::VARIANT}" :: IO [Only (StructValue FieldValue)]
            [Only inactive] <- query_ conn "SELECT union_value(number := 42::BIGINT)::UNION(number BIGINT, payload VARIANT)" :: IO [Only (UnionValue FieldValue)]
            [Only record] <- query_ conn "SELECT {'payload': 42::VARIANT}" :: IO [Only VariantRecord]
            assertVariantRejection (query conn "SELECT ?" (Only struct) :: IO [Only (StructValue FieldValue)])
            assertVariantRejection (query conn "SELECT ?" (Only nullStruct) :: IO [Only (StructValue FieldValue)])
            assertVariantRejection (query conn "SELECT ?" (Only inactive) :: IO [Only (UnionValue FieldValue)])
            assertVariantRejection (query conn "SELECT ?" (Only record) :: IO [Only VariantRecord])
            (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
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
                                _ -> object [("n", FieldInt64 n), ("xs", FieldList [FieldNull, FieldInt64 n])]
                        actual @?= expected
                        pure (n + 1)
                count @?= 5000
            | (mode, foldRows) <- [("materialized", fold_), ("deprecated streaming", Streaming.fold_)]
            ]
        , testCase "filtered and reordered rows use the right child offsets" $ withConnection ":memory:" \conn -> do
            rows <- query_ conn "SELECT {'n': i, 'xs': [i, i + 1]}::VARIANT FROM range(10000) t(i) WHERE i % 97 = 0 ORDER BY i DESC LIMIT 40"
            let expected = [Only (Variant (object [("n", FieldInt64 n), ("xs", FieldList [FieldInt64 n, FieldInt64 (n + 1)])])) | n <- take 40 (reverse [0, 97 .. 9999])]
            rows @?= expected
        , testCase "file-backed values survive checkpoint and reopen" $
            bracket newDatabase removeDatabase \path -> do
                withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                    void (execute_ conn "CREATE TABLE stored AS SELECT i, {'n': i, 'xs': [i, NULL], 'text': i::VARCHAR}::VARIANT AS value, 'POINT (1 2)'::GEOMETRY('OGC:CRS84') AS geometry FROM range(10000) t(i)")
                    void (execute_ conn "CHECKPOINT")
                withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                    rows <- query_ conn "SELECT value FROM stored WHERE i % 97 = 0 ORDER BY i DESC LIMIT 40" :: IO [Only Variant]
                    let expected =
                            [ Only (Variant (object [("n", FieldInt64 n), ("xs", FieldList [FieldInt64 n, FieldNull]), ("text", FieldText (Text.pack (show n)))]))
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
        ]

-- | Build the object payload that VARIANT decoding produces.
object :: [(Text, FieldValue)] -> FieldValue
object entries =
    FieldStruct
        StructValue
            { structValueFields = indexed [StructField name value | (name, value) <- entries]
            , structValueTypes = indexed [StructField name (LogicalTypeScalar DuckDBTypeVariant) | (name, _) <- entries]
            , structValueIndex = Map.fromList (zip (map fst entries) [0 ..])
            }
  where
    indexed items = listArray (0, length items - 1) items

-- | A raw geometry payload has no CRS inside a VARIANT.
geometryPayload :: BS.ByteString -> Variant
geometryPayload wkb = Variant (FieldGeometry (RawGeometry wkb Nothing))

-- | Require an exception without depending on native error text.
assertFailureIO :: IO a -> Assertion
assertFailureIO action = do
    result <- try (action >> pure ()) :: IO (Either SomeException ())
    case result of
        Left _ -> pure ()
        Right () -> assertFailure "expected an exception"

-- | Check that a parameter with VARIANT metadata asks for an explicit cast.
assertVariantRejection :: IO a -> Assertion
assertVariantRejection action = do
    result <- try (action >> pure ()) :: IO (Either SomeException ())
    case result of
        Left err -> assertBool (displayException err) ("?::VARIANT" `isInfixOf` displayException err)
        Right () -> assertFailure "expected VARIANT parameter rejection"

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
