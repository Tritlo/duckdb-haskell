{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GeometryRegressionTests (tests) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Array (Array, listArray)
import qualified Data.ByteString as BS
import qualified Data.Geometry as G
import qualified Data.Geometry.WKB as WKB
import qualified Data.Geometry.WKT as WKT
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Database.DuckDB.Simple.FromField (FieldValue, StructValue, UnionValue)
import Database.DuckDB.Simple.Geometry (RawGeometry (..), fromRawGeometry, toRawGeometry)
import GHC.Float (castWord64ToDouble)
import System.Mem (performMajorGC)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Check native construction, metadata, nesting, and streaming.
tests :: TestTree
tests =
    testGroup
        "geometry integration"
        [ testCase "Haskell shapes bind directly in every coordinate layout" $
            withConnection ":memory:" \conn -> do
                shapeRoundTrips conn G.DimXY G.PointXY G.CoordinatesXY (G.XY 1 2) (G.XY 3 4)
                shapeRoundTrips conn G.DimXYZ G.PointXYZ G.CoordinatesXYZ (G.XYZ 1 2 3) (G.XYZ 4 5 6)
                shapeRoundTrips conn G.DimXYM G.PointXYM G.CoordinatesXYM (G.XYM 1 2 3) (G.XYM 4 5 6)
                shapeRoundTrips conn G.DimXYZM G.PointXYZM G.CoordinatesXYZM (G.XYZM 1 2 3 4) (G.XYZM 5 6 7 8)
        , testCase "large coordinate buffers survive native round trips" $
            withConnection ":memory:" \conn -> do
                let coordinates = U.generate 10000 (\i -> G.XY (fromIntegral i) (fromIntegral (i * 2)))
                    points = U.imap (\i coordinate -> if i `mod` 17 == 0 then G.EmptyPoint G.DimXY else G.PointXY coordinate) coordinates
                forM_ [G.LineString (G.CoordinatesXY coordinates), G.MultiPoint points] \shape -> do
                    (query conn "SELECT ?" (Only shape)) >>= (@?= [Only shape])
                    raw <- either assertFailure pure (toRawGeometry shape)
                    (query conn "SELECT ?" (Only raw) :: IO [Only RawGeometry]) >>= (@?= [Only raw])
        , testCase "WKT binding preserves exact finite coordinate bits" $
            withConnection ":memory:" \conn ->
                forM_ [0, 0x8000000000000000, 1, 0x8000000000000001, 0x000fffffffffffff, 0x0010000000000000, 0x3fb999999999999b, 0x3ff0000000000001, 0x44b52d02c7e14af6, 0x7fefffffffffffff, 0xffefffffffffffff] \bits -> do
                    let shape = G.PointGeometry (G.PointXY (G.XY (castWord64ToDouble bits) (-0.0)))
                    expected <- either assertFailure pure (WKB.encodeWKB shape)
                    [(actual, wkt)] <- query conn "SELECT ST_AsWKB(?), ST_AsText(?)" (shape, shape) :: IO [(BS.ByteString, Text)]
                    actual @?= expected
                    (WKT.decodeWKT wkt >>= WKB.encodeWKB) @?= Right expected
                    (query conn "SELECT ST_AsWKB(?)" (Only (RawGeometry expected Nothing)) :: IO [Only BS.ByteString])
                        >>= (@?= [Only expected])
        , testCase "pure WKT decoding agrees with native geometry results" $
            withConnection ":memory:" \conn ->
                forM_ shapes \wkt -> do
                    [(native, rendered)] <- query conn "SELECT ?::GEOMETRY, ST_AsText(?::GEOMETRY)" (wkt, wkt) :: IO [(G.Geometry, Text)]
                    WKT.decodeWKT wkt @?= Right native
                    WKT.decodeWKT rendered @?= Right native
        , testCase "native bare multipoint EMPTY syntax parses as WKT" $
            withConnection ":memory:" \conn -> do
                [(native, bytes, rendered)] <-
                    query_ conn "SELECT g, ST_AsWKB(g), ST_AsText(g) FROM (SELECT 'MULTIPOINT ((1 2), EMPTY, (3 4))'::GEOMETRY AS g)" :: IO [(G.Geometry, BS.ByteString, Text)]
                native @?= G.MultiPoint (U.fromList [G.PointXY (G.XY 1 2), G.EmptyPoint G.DimXY, G.PointXY (G.XY 3 4)])
                WKB.decodeWKB bytes @?= Right native
                WKT.decodeWKT rendered @?= Right native
                (WKT.encodeWKT native >>= WKT.decodeWKT) @?= Right native
        , testCase "standalone WKT syntax binds through native geometry parameters" $
            withConnection ":memory:" \conn ->
                forM_ ["point(+.5 -1.e+2)", "MULTIPOINT (EMPTY, (1 2), (3 4))", "MULTIPOINT (EMPTY, 1 2, EMPTY, 3 4, EMPTY)"] \wkt -> do
                    shape <- either assertFailure pure (WKT.decodeWKT wkt)
                    (query conn "SELECT ?" (Only shape)) >>= (@?= [Only shape])
        , testCase "native geometry depth limit produces a recoverable error" $
            withConnection ":memory:" \conn -> do
                let nested = iterate (G.GeometryCollection . V.singleton) (G.PointGeometry (G.PointXY (G.XY 1 2)))
                    accepted = nested !! 15
                    rejected = nested !! 16
                (query conn "SELECT ?" (Only accepted)) >>= (@?= [Only accepted])
                assertFailureIO (query conn "SELECT ?" (Only rejected) :: IO [Only G.Geometry])
                (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        , testCase "decoded native shapes round trip through parameters and WKB" $
            withConnection ":memory:" \conn ->
                forM_ shapes \wkt -> do
                    [Only geometry] <- query conn "SELECT ?::GEOMETRY" (Only wkt) :: IO [Only G.Geometry]
                    (query conn "SELECT ?" (Only geometry) :: IO [Only G.Geometry]) >>= (@?= [Only geometry])
                    raw <- either assertFailure pure (toRawGeometry geometry)
                    fromRawGeometry raw @?= Right geometry
        , testCase "decoded rows retain point and sequence layouts, including empties" $
            withConnection ":memory:" \conn ->
                forM_
                    [ ("POINT Z EMPTY", G.PointGeometry (G.EmptyPoint G.DimXYZ))
                    , ("LINESTRING M EMPTY", G.LineString (G.CoordinatesXYM U.empty))
                    , ("POINT Z (1 2 3)", G.PointGeometry (G.PointXYZ (G.XYZ 1 2 3)))
                    , ("POINT EMPTY", G.PointGeometry (G.EmptyPoint G.DimXY))
                    ]
                    \(wkt, expected) ->
                        (query conn "SELECT ?::GEOMETRY" (Only (wkt :: Text)) :: IO [Only G.Geometry])
                            >>= (@?= [Only expected])
        , testCase "decoded arrays preserve shapes and NULL elements without CRS" $
            withConnection ":memory:" \conn -> do
                let geometry = G.PointGeometry (G.PointXYM (G.XYM 1 2 3))
                    values = listArray (0, 2) [Nothing, Just geometry, Just (G.PointGeometry (G.EmptyPoint G.DimXYM))]
                (query conn "SELECT ?, ST_CRS((?)[2])" (values, values) :: IO [(Array Int (Maybe G.Geometry), Maybe Text)])
                    >>= (@?= [(values, Nothing)])
        , testCase "only raw geometry retains CRS metadata" $
            withConnection ":memory:" \conn -> do
                [(raw, shape)] <- query_ conn "SELECT g, g FROM (SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84') AS g)" :: IO [(RawGeometry, G.Geometry)]
                rawGeometryCRS raw @?= Just "OGC:CRS84"
                shape @?= G.PointGeometry (G.PointXYZM (G.XYZM 1 2 3 4))
                fromRawGeometry raw @?= Right shape
                unannotated <- either assertFailure pure (toRawGeometry shape)
                rawGeometryCRS unannotated @?= Nothing
                rawGeometryWKB unannotated @?= rawGeometryWKB raw
                (query conn "SELECT ST_CRS(?), ST_CRS(?), ST_CRS(?), ?" (raw, shape, unannotated{rawGeometryCRS = rawGeometryCRS raw}, shape) :: IO [(Maybe Text, Maybe Text, Maybe Text, G.Geometry)])
                    >>= (@?= [(Just "OGC:CRS84", Nothing, Just "OGC:CRS84", shape)])
        , testCase "decoded coordinates remain usable after closing their connection" $ do
            geometry <- withConnection ":memory:" \conn -> do
                [Only value] <- query_ conn "SELECT 'LINESTRING (1 2, 3 4, 5 6)'::GEOMETRY('OGC:CRS84')" :: IO [Only G.Geometry]
                pure value
            performMajorGC
            case geometry of
                G.LineString (G.CoordinatesXY coords) -> U.foldl' (\acc (G.XY x y) -> acc + x + y) 0 coords @?= 21
                other -> assertFailure ("unexpected geometry: " <> show other)
        , testCase "native WKB round trips through GEOMETRY parameters" $
            withConnection ":memory:" \conn ->
                forM_ shapes \wkt -> do
                    [Only geometry] <- query conn "SELECT ?::GEOMETRY" (Only wkt)
                    (query conn "SELECT typeof(?), ?" (geometry, geometry) :: IO [(Text, RawGeometry)])
                        >>= (@?= [("GEOMETRY", geometry)])
        , testCase "raw empty containers retain layouts that decoded values do not store" $
            withConnection ":memory:" \conn ->
                forM_
                    ( [ (family <> " " <> dimensions <> " EMPTY", family <> " EMPTY")
                      | family <- ["MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON", "GEOMETRYCOLLECTION"]
                      , dimensions <- ["Z", "M", "ZM"]
                      ]
                        ++ [("GEOMETRYCOLLECTION ZM (MULTIPOINT ZM EMPTY, GEOMETRYCOLLECTION ZM EMPTY)", "GEOMETRYCOLLECTION (MULTIPOINT EMPTY, GEOMETRYCOLLECTION EMPTY)")]
                    )
                    \(wkt, normalizedWKT) -> do
                        [Only raw] <- query conn "SELECT ?::GEOMETRY('OGC:CRS84')" (Only (wkt :: Text)) :: IO [Only RawGeometry]
                        (query conn "SELECT ?" (Only raw) :: IO [Only RawGeometry]) >>= (@?= [Only raw])
                        decoded <- either assertFailure pure (fromRawGeometry raw)
                        WKT.encodeWKT decoded @?= Right normalizedWKT
                        normalized <- either assertFailure pure (toRawGeometry decoded)
                        rawGeometryCRS normalized @?= Nothing
                        assertBool "decoded empty containers must not retain an unstored dimension tag" (rawGeometryWKB normalized /= rawGeometryWKB raw)
        , testCase "mixed-layout collections fail binding without poisoning the connection" $
            withConnection ":memory:" \conn -> do
                let geometry = G.GeometryCollection (V.fromList [G.PointGeometry (G.PointXY (G.XY 1 2)), G.PointGeometry (G.PointXYZ (G.XYZ 3 4 5))])
                raw <- either assertFailure pure (toRawGeometry geometry)
                assertFailureIO (query conn "SELECT ?" (Only geometry) :: IO [Only G.Geometry])
                assertFailureIO (query conn "SELECT ?" (Only raw) :: IO [Only RawGeometry])
                (query conn "SELECT ST_GeomFromWKB(?)" (Only (rawGeometryWKB raw)) :: IO [Only G.Geometry])
                    >>= (@?= [Only geometry])
                (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        , testCase "mixed multipoint layouts promote missing ordinates to NaN" $
            withConnection ":memory:" \conn -> do
                let geometry = G.MultiPoint (U.fromList [G.PointXY (G.XY 1 2), G.PointXYZ (G.XYZ 3 4 5)])
                [Only actual] <- query conn "SELECT ?" (Only geometry) :: IO [Only G.Geometry]
                case actual of
                    G.MultiPoint points -> do
                        U.length points @?= 2
                        case points U.! 0 of
                            G.PointXYZ (G.XYZ x y z) -> do
                                (x, y) @?= (1, 2)
                                assertBool "promoted Z must be NaN" (isNaN z)
                            other -> assertFailure ("unexpected promoted point: " <> show other)
                        points U.! 1 @?= G.PointXYZ (G.XYZ 3 4 5)
                    other -> assertFailure ("unexpected geometry: " <> show other)
        , testCase "raw NaN points retain ordinates that decoded emptiness normalizes" $
            withConnection ":memory:" \conn -> do
                [Only raw] <- query_ conn "SELECT 'POINT Z (NaN NaN 7)'::GEOMETRY" :: IO [Only RawGeometry]
                (query conn "SELECT ?" (Only raw) :: IO [Only RawGeometry]) >>= (@?= [Only raw])
                decoded <- either assertFailure pure (fromRawGeometry raw)
                decoded @?= G.PointGeometry (G.EmptyPoint G.DimXYZ)
                normalized <- either assertFailure pure (toRawGeometry decoded)
                assertBool "decoding an XY-NaN point normalizes its extra ordinates" (rawGeometryWKB normalized /= rawGeometryWKB raw)
                (query conn "SELECT ?" (Only decoded) :: IO [Only G.Geometry]) >>= (@?= [Only decoded])
        , testCase "decoded polygon writing normalizes all-empty rings" $
            withConnection ":memory:" \conn -> do
                let emptyRing = G.CoordinatesXYZ U.empty
                    geometry = G.Polygon (G.PolygonRings emptyRing (V.singleton emptyRing))
                    expected = G.Polygon (G.PolygonRings emptyRing V.empty)
                (query conn "SELECT ?" (Only geometry) :: IO [Only G.Geometry]) >>= (@?= [Only expected])
        , testCase "invalid decoded construction fails without poisoning the connection" $
            withConnection ":memory:" \conn -> do
                let closedRing = G.CoordinatesXY (U.fromList [G.XY 0 0, G.XY 1 0, G.XY 1 1, G.XY 0 0])
                    openRing = G.CoordinatesXY (U.fromList [G.XY 0 0, G.XY 1 0, G.XY 1 1])
                    emptyRing = G.CoordinatesXY U.empty
                forM_
                    [ G.LineString (G.CoordinatesXY (U.singleton (G.XY 1 2)))
                    , G.Polygon (G.PolygonRings openRing V.empty)
                    , G.Polygon (G.PolygonRings emptyRing (V.singleton closedRing))
                    ]
                    \shape ->
                        assertFailureIO (query conn "SELECT ?" (Only shape) :: IO [Only G.Geometry])
                [Only raw] <- query_ conn "SELECT 'LINESTRING (1 2)'::GEOMETRY" :: IO [Only RawGeometry]
                assertBool "raw result bytes remain available" (not (BS.null (rawGeometryWKB raw)))
                assertFailureIO (query_ conn "SELECT 'LINESTRING (1 2)'::GEOMETRY" :: IO [Only G.Geometry])
                (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        , testCase "CRS survives parameter binding and table insertion" $
            withConnection ":memory:" \conn -> do
                [Only geometry] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')"
                rawGeometryCRS geometry @?= Just "OGC:CRS84"
                (query conn "SELECT ST_CRS(?), ?" (geometry, geometry) :: IO [(Text, RawGeometry)])
                    >>= (@?= [("OGC:CRS84", geometry)])
                _ <- execute_ conn "CREATE TABLE shapes (shape GEOMETRY('OGC:CRS84'))"
                _ <- execute conn "INSERT INTO shapes VALUES (?)" (Only geometry)
                (query_ conn "SELECT shape FROM shapes" :: IO [Only RawGeometry]) >>= (@?= [Only geometry])
        , testCase "CRS parameters preserve quotes and Unicode" $
            withConnection ":memory:" \conn -> do
                [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY"
                let annotated = geometry{rawGeometryCRS = Just "local' íslenska λ"}
                (query conn "SELECT ?" (Only annotated) :: IO [Only RawGeometry]) >>= (@?= [Only annotated])
        , testCase "geometry construction ignores user macros with built-in names" $
            withConnection ":memory:" \conn -> do
                [Only raw] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
                decoded <- either assertFailure pure (fromRawGeometry raw)
                _ <- execute_ conn "CREATE MACRO ST_AsText(x) AS 42"
                _ <- execute_ conn "CREATE MACRO ST_GeomFromWKB(x) AS 'POINT (9 9)'::GEOMETRY"
                _ <- execute_ conn "CREATE MACRO ST_SetCRS(x, crs) AS 42"
                (query conn "SELECT ?, ?" (raw, decoded) :: IO [(RawGeometry, G.Geometry)])
                    >>= (@?= [(raw, decoded)])
        , testCase "NULL and empty geometry stay distinct" $
            withConnection ":memory:" \conn -> do
                [Only empty] <- query_ conn "SELECT 'POINT EMPTY'::GEOMETRY" :: IO [Only RawGeometry]
                (query conn "SELECT ?, ?" (Nothing :: Maybe RawGeometry, Just empty) :: IO [(Maybe RawGeometry, Maybe RawGeometry)])
                    >>= (@?= [(Nothing, Just empty)])
        , testCase "arrays keep their common CRS, including NULL elements" $
            withConnection ":memory:" \conn -> do
                [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
                let values = listArray (0, 2) [Nothing, Just geometry, Nothing]
                (query conn "SELECT ?" (Only values) :: IO [Only (Array Int (Maybe RawGeometry))]) >>= (@?= [Only values])
                assertFailureIO (query conn "SELECT ?" (Only (listArray (0 :: Int, 1) [geometry, geometry{rawGeometryCRS = Nothing}])) :: IO [Only FieldValue])
        , testCase "nested LIST, ARRAY and MAP values preserve CRS" $
            withConnection ":memory:" \conn -> do
                [Only value] <-
                    query_
                        conn
                        "SELECT {'xs': ['POINT (1 2)'::GEOMETRY('OGC:CRS84'), NULL], 'fixed': ['POINT EMPTY'::GEOMETRY('OGC:CRS84')]::GEOMETRY('OGC:CRS84')[1], 'map': MAP {'one': 'POINT (3 4)'::GEOMETRY('OGC:CRS84')}}" ::
                        IO [Only (StructValue FieldValue)]
                (query conn "SELECT ?" (Only value) :: IO [Only (StructValue FieldValue)]) >>= (@?= [Only value])
        , testCase "UNION payloads preserve geometry types, including NULL" $
            withConnection ":memory:" \conn ->
                forM_ ["SELECT union_value(shape := 'POINT (1 2)'::GEOMETRY('OGC:CRS84'))", "SELECT union_value(shape := NULL::GEOMETRY('OGC:CRS84'))"] \sql -> do
                    [Only value] <- query_ conn sql :: IO [Only (UnionValue FieldValue)]
                    (query conn "SELECT ?" (Only value) :: IO [Only (UnionValue FieldValue)]) >>= (@?= [Only value])
        , testGroup
            "folds cross chunk boundaries and preserve CRS"
            [ testCase mode $
                withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
                    [Only expected] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
                    count <- foldRows conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84') FROM range(5000)" (0 :: Int64) \n (Only actual) -> do
                        actual @?= expected
                        pure (n + 1)
                    count @?= 5000
            | (mode, foldRows) <- [("materialized", fold_), ("deprecated streaming", Streaming.fold_)]
            ]
        , testCase "invalid WKB, empty CRS and NUL CRS fail without poisoning the connection" $
            withConnection ":memory:" \conn -> do
                assertFailureIO (query conn "SELECT ?" (Only (RawGeometry BS.empty Nothing)) :: IO [Only RawGeometry])
                [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY" :: IO [Only RawGeometry]
                assertFailureIO (query conn "SELECT ?" (Only geometry{rawGeometryCRS = Just ""}) :: IO [Only RawGeometry])
                assertFailureIO (query conn "SELECT ?" (Only geometry{rawGeometryCRS = Just "OGC:CRS84\0bad"}) :: IO [Only RawGeometry])
                (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        ]

-- | Exercise all coordinate layouts with explicit point and sequence constructors.
shapeRoundTrips :: (G.Coordinate coord) => Connection -> G.Dimensions -> (coord -> G.Point) -> (U.Vector coord -> G.Coordinates) -> coord -> coord -> Assertion
shapeRoundTrips conn dimensions point coordinates first second = do
    let line = coordinates (U.fromList [first, second])
        ring = coordinates (U.fromList [first, second, first, first])
        emptyLine = coordinates U.empty
        emptyPoint = G.EmptyPoint dimensions
        polygon = G.PolygonRings ring V.empty
        emptyPolygon = G.PolygonRings emptyLine V.empty
        values =
            [ G.PointGeometry (point first)
            , G.PointGeometry emptyPoint
            , G.LineString line
            , G.LineString emptyLine
            , G.Polygon polygon
            , G.Polygon emptyPolygon
            , G.MultiPoint (U.fromList [point first, emptyPoint, point second])
            , G.MultiPoint U.empty
            , G.MultiLineString (V.fromList [line, emptyLine])
            , G.MultiPolygon (V.fromList [polygon, emptyPolygon])
            , G.GeometryCollection (V.fromList [G.PointGeometry emptyPoint, G.LineString line, G.GeometryCollection V.empty])
            ]
    forM_ values \shape -> do
        rows <- query conn "SELECT ?, ST_CRS(?)" (shape, shape)
        rows @?= [(shape, Nothing :: Maybe Text)]
        raw <- either assertFailure pure (toRawGeometry shape)
        fromRawGeometry raw @?= Right shape
        let annotated = raw{rawGeometryCRS = Just "local' íslenska λ"}
        (query conn "SELECT ?, ST_CRS(?)" (annotated, annotated) :: IO [(G.Geometry, Maybe Text)])
            >>= (@?= [(shape, rawGeometryCRS annotated)])

-- | Reject an invalid parameter without depending on native error text.
assertFailureIO :: IO a -> Assertion
assertFailureIO action = do
    result <- try (action >> pure ()) :: IO (Either SomeException ())
    case result of
        Left _ -> pure ()
        Right () -> assertFailure "expected parameter rejection"

-- | Native shapes cover all families, dimensions, empty children, and exact doubles.
shapes :: [Text]
shapes =
    [ "POINT (0.10000000000000002 -0.0)"
    , "POINT Z (1 2 3)"
    , "POINT M (1 2 3)"
    , "POINT ZM (1 2 3 4)"
    , "POINT EMPTY"
    , "LINESTRING EMPTY"
    , "LINESTRING (1 2, 3 4)"
    , "POLYGON ((0 0, 1 0, 1 1, 0 0))"
    , "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (2 2, 2 4, 4 4, 4 2, 2 2))"
    , "MULTIPOINT ((1 2), EMPTY, (3 4))"
    , "MULTILINESTRING ((1 2, 3 4), EMPTY)"
    , "MULTIPOLYGON (((0 0, 1 0, 1 1, 0 0)), EMPTY)"
    , "GEOMETRYCOLLECTION (POINT (1 2), LINESTRING (0 0, 1 1))"
    , "GEOMETRYCOLLECTION ZM (POINT ZM (1 2 3 4), POINT ZM EMPTY)"
    , "POINT Z EMPTY"
    , "LINESTRING M EMPTY"
    , "GEOMETRYCOLLECTION ZM EMPTY"
    ]
