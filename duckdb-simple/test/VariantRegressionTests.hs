{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VARIANT binding and explicit SQL conversion through the V2 API.
module VariantRegressionTests (tests) where

import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (forM_, void, when)
import Data.Array (Array, elems, listArray)
import qualified Data.ByteString as BS
import qualified Data.Geometry as G
import Data.Int (Int64)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Streaming as Streaming
import Database.DuckDB.Simple.FromField (BitString (..), DecimalValue (..), FieldValue (..), StructValue (..), UnionValue (..))
import Database.DuckDB.Simple.Generic (ViaDuckDB (..))
import Database.DuckDB.Simple.Geometry (RawGeometry (..), toRawGeometry)
import Database.DuckDB.Simple.Internal (withConnectionHandle)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), UnionMemberType (..), destroyLogicalType, logicalTypeFromRep, logicalTypeToRep)
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

-- | Check each native scalar through an explicit cast and parameter binding.
tests :: TestTree
tests =
    testGroup
        "variant integration"
        [ testGroup
            "scalar payloads support explicit SQL casts"
            [ testCase (Text.unpack sql) $ withConnection ":memory:" \conn -> do
                [Only native] <- query_ conn (Query ("SELECT " <> sql)) :: IO [Only FieldValue]
                if sql == "NULL"
                    then (query conn "SELECT ? IS NULL" (Only (Variant FieldNull)) :: IO [Only Bool]) >>= (@?= [Only True])
                    else do
                        (query_ conn (Query ("SELECT cast_to_type((" <> sql <> ")::VARIANT, " <> sql <> ")")) :: IO [Only FieldValue]) >>= (@?= [Only native])
                        (query conn (Query ("SELECT cast_to_type(?, " <> sql <> ")")) (Only (Variant native)) :: IO [Only FieldValue]) >>= (@?= [Only native])
            | sql <- scalarCases
            ]
        , testGroup
            "automatic payload decoding is unavailable"
            [ testCase (Text.unpack sql) $ withConnection ":memory:" \conn ->
                assertVariantUnavailable (query_ conn (Query ("SELECT " <> sql)) :: IO [Only FieldValue])
            | sql <- ["42::VARIANT", "[1, 'two'::VARIANT]::VARIANT", "{'payload': 42::VARIANT}", "[42::VARIANT]", "MAP {'x': 42::VARIANT}", "union_value(payload := 42::VARIANT)"]
            ]
        , testCase "explicit casts bind plain parameters" $ withConnection ":memory:" \conn -> do
            [Only record] <- query_ conn "SELECT {'a': 1, 'b': 'x'}" :: IO [Only (StructValue FieldValue)]
            (query conn "SELECT (?::VARIANT)::BIGINT, (?::VARIANT)::VARCHAR, (?::VARIANT)::STRUCT(a INTEGER,b VARCHAR), (?::VARIANT)::BIGINT[]" (42 :: Int64, "two" :: Text, record, listArray (0, 1) [1, 2] :: Array Int Int64) :: IO [(Int64, Text, StructValue FieldValue, [Int64])]) >>= (@?= [(42, "two", record, [1, 2])])
            void (execute_ conn "CREATE TABLE variants (v VARIANT)")
            void (executeMany conn "INSERT INTO variants VALUES (?)" [Only (7 :: Int64), Only 8])
            void (execute conn "INSERT INTO variants VALUES (?)" (Only record))
            (query_ conn "SELECT v::BIGINT FROM variants LIMIT 2" :: IO [Only Int64]) >>= (@?= [Only 7, Only 8])
            (query_ conn "SELECT variant_extract(v,'a')::INTEGER FROM variants OFFSET 2" :: IO [Only Int]) >>= (@?= [Only 1])
        , testCase "existing FromField instances read explicit SQL casts" $ withConnection ":memory:" \conn ->
            (query_ conn "SELECT (42::BIGINT::VARIANT)::BIGINT, ('x'::VARIANT)::VARCHAR, ([1,2,3]::VARIANT)::BIGINT[]" :: IO [(Int64, Text, [Int64])]) >>= (@?= [(42, "x", [1, 2, 3])])
        , testCase "bound payloads keep integer widths and decimal scale" $ withConnection ":memory:" \conn ->
            forM_ [("'-128'::TINYINT", FieldInt8 minBound), ("12.34::DECIMAL(4,2)", FieldDecimal (DecimalValue 4 2 1234)), ("255::UTINYINT", FieldWord8 maxBound)] \(sql, value) ->
                (query conn (Query ("SELECT variant_typeof(?) = variant_typeof((" <> sql <> ")::VARIANT)")) (Only (Variant value)) :: IO [Only Bool]) >>= (@?= [Only True])
        , testCase "floating special values retain type and bits" $ withConnection ":memory:" \conn -> do
            forM_ [0, -0.0, 1 / 0, -1 / 0, 0 / 0] \value -> do
                [Only decoded] <- query conn "SELECT ?::DOUBLE" (Only (Variant (FieldDouble value)))
                if isNaN value then assertBool "expected Double NaN" (isNaN decoded) else castDoubleToWord64 decoded @?= castDoubleToWord64 value
            forM_ [0, -0.0, 1 / 0, -1 / 0, 0 / 0] \value -> do
                [Only decoded] <- query conn "SELECT ?::FLOAT" (Only (Variant (FieldFloat value)))
                if isNaN value then assertBool "expected Float NaN" (isNaN decoded) else castFloatToWord32 decoded @?= castFloatToWord32 value
        , testCase "TIMETZ explicit casts decode like native values" $ withConnection ":memory:" \conn -> do
            forM_ ["'12:00:00+01:23'::TIMETZ", "'23:59:59.999999-15:59'::TIMETZ", "'00:00:00+00'::TIMETZ"] \sql -> do
                [(actual, native)] <- query_ conn (Query ("SELECT (" <> sql <> ")::VARIANT::TIMETZ, " <> sql)) :: IO [(FieldValue, FieldValue)]
                actual @?= native
                (query conn "SELECT ?::TIMETZ" (Only (Variant native)) :: IO [Only FieldValue]) >>= (@?= [Only native])
            assertFailureIO (query_ conn "SELECT '12:00:00+01:23:45'::TIMETZ::VARIANT::TIMETZ" :: IO [Only FieldValue])
        , testCase "text and binary casts preserve embedded NUL" $ withConnection ":memory:" \conn -> do
            let text = "before\0after íslenska λ 😀"
                bytes = BS.pack [0, 1, 127, 128, 255, 0]
            (query conn "SELECT ?::VARCHAR" (Only (Variant (FieldText text))) :: IO [Only Text]) >>= (@?= [Only text])
            (query conn "SELECT ?::BLOB" (Only (Variant (FieldBlob bytes))) :: IO [Only BS.ByteString]) >>= (@?= [Only bytes])
        , testCase "objects preserve case-sensitive names and heterogeneous children" $ withConnection ":memory:" \conn -> do
            let value = variantObject [("Case", FieldInt64 9007199254740993), ("case", FieldDecimal (DecimalValue 38 9 1234567890123456789)), ("quote' íslenska λ", FieldList [FieldNull, FieldText "x", FieldList [], variantObject []])]
            (query conn "SELECT variant_extract(?,'Case')::BIGINT, variant_extract(?,'case')::DECIMAL(38,9)::VARCHAR" (Variant value, Variant value) :: IO [(Int64, Text)]) >>= (@?= [(9007199254740993, "1234567890.123456789")])
            assertVariantUnavailable (query conn "SELECT ?" (Only (Variant value)) :: IO [Only Variant])
        , testCase "empty containers and SQL NULL stay distinct" $ withConnection ":memory:" \conn -> do
            (query_ conn "SELECT NULL::VARIANT" :: IO [Only (Maybe Variant)]) >>= (@?= [Only Nothing])
            (query conn "SELECT ?" (Only (Variant FieldNull)) :: IO [Only FieldValue]) >>= (@?= [Only FieldNull])
            forM_ [(FieldList [], "[]"), (variantObject [], "{}"), (FieldList [FieldNull], "[null]"), (variantObject [("x", FieldNull)], "{\"x\":null}")] \(value, json) ->
                (query conn "SELECT ? IS NULL, ? = ?::JSON::VARIANT" (Variant value, Variant value, json :: Text) :: IO [(Bool, Bool)]) >>= (@?= [(False, True)])
        , testCase "empty object keys survive supported SQL projections" $ withConnection ":memory:" \conn -> do
            (query_ conn "SELECT variant_extract('{\"named\":1,\"\":42}'::JSON::VARIANT,'')::BIGINT" :: IO [Only Int64]) >>= (@?= [Only 42])
            let value = Variant (variantObject [("named", FieldBool True), ("", FieldText "λ")])
            (query conn "SELECT variant_extract(?,'')::VARCHAR" (Only value) :: IO [Only Text]) >>= (@?= [Only "λ"])
            soleEmpty <- try (query conn "SELECT typeof(?)" (Only (Variant (variantObject [("", FieldInt64 42)])))) :: IO (Either IOException [Only Text])
            case soleEmpty of
                Left err -> assertBool "must identify constructor limit" ("only an empty key" `isInfixOf` displayException err)
                Right _ -> assertFailure "expected the native constructor limit"
        , testCase "deep native arrays support explicit SQL extraction" $ withConnection ":memory:" \conn ->
            forM_ [128, 256 :: Int] \depth -> do
                let sql = foldr (\_ value -> "variant_extract(" <> value <> ",1)") "?::JSON::VARIANT" [1 .. depth]
                    json = Text.replicate depth "[" <> "7" <> Text.replicate depth "]"
                (query conn (Query ("SELECT " <> sql <> "::BIGINT AS payload")) (Only json) :: IO [Only Int64]) >>= (@?= [Only 7])
        , testCase "ARRAY parameters and generic records contain VARIANT" $ withConnection ":memory:" \conn -> do
            let values = listArray (0 :: Int, 2) [Variant (FieldInt8 1), Variant (FieldText "two"), Variant FieldNull]
                record = VariantRecord (Variant (variantObject [("x", FieldWord64 maxBound)]))
            (query conn "SELECT (?)[1]::TINYINT, (?)[2]::VARCHAR, (?)[3] IS NULL" (values, values, values) :: IO [(Int, Text, Bool)]) >>= (@?= [(1, "two", True)])
            (query conn "SELECT variant_extract((?).payload,'x')::UBIGINT" (Only record) :: IO [Only Word]) >>= (@?= [Only maxBound])
        , testCase "nullable ARRAY parameters retain VARIANT types" $ withConnection ":memory:" \conn -> do
            let values = listArray (0 :: Int, 3) [Nothing, Just (Variant (FieldInt64 42)), Just (Variant (FieldText "two")), Just (Variant (variantObject [("xs", FieldList [FieldNull, FieldBool True])]))]
            (query conn "SELECT typeof(?), (?)[1] IS NULL, (?)[2]::BIGINT, (?)[3]::VARCHAR, variant_extract((?)[4],'xs')::BOOLEAN[]" (values, values, values, values, values) :: IO [(Text, Bool, Int64, Text, [Maybe Bool])]) >>= (@?= [("VARIANT[4]", True, 42, "two", [Nothing, Just True])])
        , testCase "empty ARRAY parameters report the V2 limit" $ withConnection ":memory:" \conn -> do
            let values = listArray (0, -1) [] :: Array Int (Maybe Variant)
            result <- try (query conn "SELECT typeof(?)" (Only values))
            case result of
                Left SQLError{sqlErrorMessage} -> assertBool "must identify empty ARRAY" ("empty ARRAY" `Text.isInfixOf` sqlErrorMessage)
                Right (_ :: [Only Text]) -> assertFailure "expected the V2 empty ARRAY limit"
        , testCase "parameter binding supplies a complete VARIANT type" $ withConnection ":memory:" \conn ->
            (query conn "SELECT typeof(?)" (Only (Variant (variantObject [("xs", FieldList [FieldNull, FieldInt64 42])]))) :: IO [Only Text]) >>= (@?= [Only "VARIANT"])
        , testCase "the first VARIANT parameter does not end a streaming result" $ withConnection ":memory:" \conn ->
            withStatement conn "SELECT ?" \stmt -> do
                total <- Streaming.fold_ conn "SELECT i FROM range(100000) t(i)" 0 \acc (Only i) -> do
                    when (i == 0) (bind stmt [toField (Variant (FieldInt64 42))])
                    pure (acc + i)
                total @?= (sum [0 .. 99999] :: Int64)
        , testCase "VARIANT type construction uses the supplied connection" $ withConnection ":memory:" \conn ->
            withConnectionHandle conn \native ->
                bracket (logicalTypeFromRep native (LogicalTypeScalar DuckDBV2LogicalTypeIdVariant)) destroyLogicalType \logical ->
                    logicalTypeToRep logical >>= (@?= LogicalTypeScalar DuckDBV2LogicalTypeIdVariant)
        , testCase "VARIANT and GEOMETRY CRS metadata bind together" $ withConnection ":memory:" \conn -> do
            let types = listArray (0, 1) [StructField "payload" (LogicalTypeScalar DuckDBV2LogicalTypeIdVariant), StructField "shape" (LogicalTypeGeometry (Just "OGC:CRS84"))]
                fields = listArray (0, 1) [StructField "payload" (FieldInt32 42), StructField "shape" FieldNull]
                value = StructValue fields types (Map.fromList [("payload", 0), ("shape", 1)])
            (query conn "SELECT (?).payload::INTEGER, typeof(?)" (value, value) :: IO [(Int, Text)]) >>= (@?= [(42, "STRUCT(payload VARIANT, shape GEOMETRY('OGC:CRS84'))")])
        , testCase "inactive VARIANT UNION members have complete types" $ withConnection ":memory:" \conn -> do
            [Only value] <- query_ conn "SELECT union_value(number := 42::BIGINT)::UNION(number BIGINT,payload VARIANT)" :: IO [Only (UnionValue FieldValue)]
            (query conn "SELECT ?" (Only value) :: IO [Only (UnionValue FieldValue)]) >>= (@?= [Only value])
        , testCase "UNION NULL VARIANT payload retains its type" $ withConnection ":memory:" \conn -> do
            let value = UnionValue 0 "payload" FieldNull (listArray (0, 0) [UnionMemberType "payload" (LogicalTypeScalar DuckDBV2LogicalTypeIdVariant)])
            (query conn "SELECT ?" (Only value) :: IO [Only (UnionValue FieldValue)]) >>= (@?= [Only value])
        , testGroup
            "heterogeneous folds cross chunk boundaries"
            [ testCase mode $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
                count <- foldRows conn "SELECT i, CASE WHEN i%3=0 THEN v::BIGINT END, CASE WHEN i%3=1 THEN v::VARCHAR END, CASE WHEN i%3=2 THEN variant_extract(v,'xs')::BIGINT[] END FROM (SELECT i, CASE WHEN i%3=0 THEN i::VARIANT WHEN i%3=1 THEN 'text'::VARIANT ELSE {'n':i,'xs':[NULL,i]}::VARIANT END AS v FROM range(5000) t(i))" (0 :: Int64) \n (i, number, text, items) -> do
                    i @?= n
                    number @?= if n `mod` 3 == 0 then Just n else Nothing
                    text @?= if n `mod` 3 == 1 then Just ("text" :: Text) else Nothing
                    items @?= if n `mod` 3 == 2 then Just [Nothing, Just n] else Nothing
                    pure (n + 1)
                count @?= 5000
            | (mode, foldRows) <- [("materialized", fold_), ("native streaming", Streaming.fold_)]
            ]
        , testCase "filtered rows preserve explicit nested projection offsets" $ withConnection ":memory:" \conn -> do
            rows <- query_ conn "SELECT {'n':variant_extract(v,'n')::BIGINT,'xs':variant_extract(v,'xs')::BIGINT[]} FROM (SELECT i, {'n':i,'xs':[i,i+1]}::VARIANT AS v FROM range(10000) t(i)) WHERE i%97=0 ORDER BY i DESC LIMIT 40" :: IO [Only (StructValue FieldValue)]
            length rows @?= 40
            forM_ (zip rows (take 40 (reverse [0, 97 .. 9999]))) \(Only value, n) ->
                map structFieldValue (elems (structValueFields value)) @?= [FieldInt64 n, FieldList [FieldInt64 n, FieldInt64 (n + 1)]]
        , testCase "file-backed values survive checkpoint and reopen" $ bracket newDatabase removeDatabase \path -> do
            withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                void (execute_ conn "CREATE TABLE stored AS SELECT i, {'n':i,'xs':[i,NULL],'text':i::VARCHAR}::VARIANT AS value, 'POINT(1 2)'::GEOMETRY('OGC:CRS84') AS geometry FROM range(10000) t(i)")
                void (execute_ conn "CHECKPOINT")
            withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                rows <- query_ conn "SELECT variant_extract(value,'n')::BIGINT,variant_extract(value,'xs')::BIGINT[],variant_extract(value,'text')::VARCHAR FROM stored WHERE i%97=0 ORDER BY i DESC LIMIT 40" :: IO [(Int64, [Maybe Int64], Text)]
                rows @?= [(n, [Just n, Nothing], Text.pack (show n)) | n <- take 40 (reverse [0, 97 .. 9999])]
                [Only geometry] <- query_ conn "SELECT geometry FROM stored LIMIT 1" :: IO [Only RawGeometry]
                rawGeometryCRS geometry @?= Just "OGC:CRS84"
                BS.length (rawGeometryWKB geometry) @?= 21
        , testCase "geometry payload casts keep WKB and remove CRS" $ withConnection ":memory:" \conn -> do
            [Only original] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
            [Only actual] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')::VARIANT::GEOMETRY" :: IO [Only RawGeometry]
            rawGeometryWKB actual @?= rawGeometryWKB original
            rawGeometryCRS actual @?= Nothing
            forM_ [FieldGeometry actual, FieldList [FieldGeometry actual], variantObject [("shape", FieldGeometry actual)]] \value -> assertFailureIO (query conn "SELECT typeof(?)" (Only (Variant value)) :: IO [Only Text])
        , testCase "geometry payload casts retain empty layouts and native NaN points" $ withConnection ":memory:" \conn ->
            forM_ ["GEOMETRYCOLLECTION ZM EMPTY", "MULTIPOLYGON M EMPTY", "POINT Z (NaN NaN 7)"] \wkt -> do
                [Only original] <- query conn "SELECT ?::GEOMETRY" (Only (wkt :: Text)) :: IO [Only RawGeometry]
                (query conn "SELECT system.main.ST_GeomFromWKB(?)::VARIANT::GEOMETRY" (Only (rawGeometryWKB original)) :: IO [Only RawGeometry]) >>= (@?= [Only original])
        , testCase "geometry payload conversion ignores shadowing macros" $ withConnection ":memory:" \conn -> do
            [Only original] <- query_ conn "SELECT 'POINT(1 2)'::GEOMETRY" :: IO [Only RawGeometry]
            void (execute_ conn "CREATE MACRO ST_GeomFromWKB(x) AS 'POINT(9 9)'::GEOMETRY")
            (query conn "SELECT system.main.ST_GeomFromWKB(?)::VARIANT::GEOMETRY" (Only (rawGeometryWKB original)) :: IO [Only RawGeometry]) >>= (@?= [Only original])
        , testCase "binary geometry casts retain NaN bits and mixed layouts" $ withConnection ":memory:" \conn ->
            forM_ [G.PointGeometry (G.PointXYZ (G.XYZ (castWord64ToDouble 0x7ff8000000000001) (castWord64ToDouble 0x7ff8000000000002) 7)), G.GeometryCollection (V.fromList [G.PointGeometry (G.PointXY (G.XY 1 2)), G.PointGeometry (G.PointXYZ (G.XYZ 3 4 5))])] \shape -> do
                raw <- either assertFailure pure (toRawGeometry shape)
                [Only actual] <- query conn "SELECT system.main.ST_GeomFromWKB(?)::VARIANT::GEOMETRY" (Only (rawGeometryWKB raw)) :: IO [Only RawGeometry]
                rawGeometryWKB actual @?= rawGeometryWKB raw
        , testCase "invalid values fail before binding and the connection remains usable" $ withConnection ":memory:" \conn -> do
            forM_ [FieldHugeInt (2 ^ (127 :: Int)), FieldUHugeInt (-1), FieldUHugeInt (2 ^ (128 :: Int)), FieldDecimal (DecimalValue 0 0 0), FieldDecimal (DecimalValue 5 6 1), FieldDecimal (DecimalValue 3 0 1000), FieldBit (BitString 8 (BS.singleton 0)), variantObject [("same", FieldNull), ("same", FieldBool True)], variantObject [("", FieldBool True)], variantObject [("before\0after", FieldNull)]] \value -> assertFailureIO (query conn "SELECT typeof(?)" (Only (Variant value)) :: IO [Only Text])
            (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        ]

-- | Require the documented V2 diagnostic for a non-NULL VARIANT payload.
assertVariantUnavailable :: IO a -> Assertion
assertVariantUnavailable action = do
    result <- try action
    case result of
        Left SQLError{sqlErrorMessage} -> assertBool "must identify unavailable VARIANT decoding" ("automatic VARIANT decoding is unavailable" `Text.isInfixOf` sqlErrorMessage)
        Right _ -> assertFailure "expected the V2 VARIANT decoding limit"

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
