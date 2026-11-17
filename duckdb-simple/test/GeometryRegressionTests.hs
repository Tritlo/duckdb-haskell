{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : GeometryRegressionTests
-- Description : Regression tests for GEOMETRY construction, metadata, nesting, and streaming.
module GeometryRegressionTests (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try)
import Control.Monad (forM, forM_, when)
import Data.Array (elems, listArray)
import qualified Data.ByteString as BS
import qualified Data.Geometry as G
import qualified Data.Geometry.WKB as WKB
import qualified Data.Geometry.WKT as WKT
import Data.IORef (mkWeakIORef, newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue (..), StructValue (..), UnionValue (..))
import Database.DuckDB.Simple.Geometry (RawGeometry (..), fromRawGeometry, toRawGeometry)
import Database.DuckDB.Simple.Internal (withConnectionFactory, withConnectionHandle)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), UnionMemberType (..))
import qualified Database.DuckDB.Simple.Streaming as Streaming
import Database.DuckDB.Simple.V2.LogicalRep (destroyLogicalType, logicalTypeFromRep, logicalTypeToRep)
import Database.DuckDB.Simple.Variant (Variant (..))
import GHC.Float (castWord64ToDouble)
import System.Mem (performMajorGC)
import System.Mem.Weak (deRefWeak)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestUtils (assertFailureIO, assertSqlErrorType, sized)

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
            (importRaw conn raw) >>= (@?= [Only raw])
    , testCase "WKT binding preserves exact finite coordinate bits" $
        withConnection ":memory:" \conn ->
          forM_
            [ 0
            , 0x8000000000000000
            , 1
            , 0x8000000000000001
            , 0x000fffffffffffff
            , 0x0010000000000000
            , 0x3fb999999999999b
            , 0x3ff0000000000001
            , 0x44b52d02c7e14af6
            , 0x7fefffffffffffff
            , 0xffefffffffffffff
            ]
            \bits -> do
              let shape = G.PointGeometry (G.PointXY (G.XY (castWord64ToDouble bits) (-0.0)))
              expected <- either assertFailure pure (WKB.encodeWKB shape)
              [(actual, wkt)] <- query conn "SELECT ST_AsWKB(?), ST_AsText(?)" (shape, shape) :: IO [(BS.ByteString, Text)]
              actual @?= expected
              (WKT.decodeWKT wkt >>= WKB.encodeWKB) @?= Right expected
              (query conn "SELECT system.main.ST_AsWKB(system.main.ST_GeomFromWKB(?))" (Only expected) :: IO [Only BS.ByteString])
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
            query_ conn "SELECT g, ST_AsWKB(g), ST_AsText(g) FROM (SELECT 'MULTIPOINT ((1 2), EMPTY, (3 4))'::GEOMETRY AS g)"
              :: IO [(G.Geometry, BS.ByteString, Text)]
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
          assertSqlErrorType ErrorInputInvalid (query conn "SELECT ?" (Only rejected) :: IO [Only G.Geometry])
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
              values = sized [Nothing, Just geometry, Just (G.PointGeometry (G.EmptyPoint G.DimXYM))] :: SizedArray 3 (Maybe G.Geometry)
          (query conn "SELECT ?, ST_CRS((?)[2])" (values, values) :: IO [(SizedArray 3 (Maybe G.Geometry), Maybe Text)])
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
          ( query conn "SELECT system.main.ST_CRS(system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?))" (rawParameters raw)
              :: IO [Only (Maybe Text)]
            )
            >>= (@?= [Only (Just "OGC:CRS84")])
          (query conn "SELECT ST_CRS(?), ?" (shape, shape) :: IO [(Maybe Text, G.Geometry)])
            >>= (@?= [(Nothing, shape)])
    , testCase "decoded coordinates remain usable after closing their connection" $ do
        geometry <- withConnection ":memory:" \conn -> do
          [Only value] <- query_ conn "SELECT 'LINESTRING (1 2, 3 4, 5 6)'::GEOMETRY('OGC:CRS84')" :: IO [Only G.Geometry]
          pure value
        performMajorGC
        case geometry of
          G.LineString (G.CoordinatesXY coords) -> U.foldl' (\acc (G.XY x y) -> acc + x + y) 0 coords @?= 21
          other -> assertFailure ("unexpected geometry: " <> show other)
    , testCase "native WKB round trips through explicit binary import" $
        withConnection ":memory:" \conn ->
          forM_ shapes \wkt -> do
            [Only geometry] <- query conn "SELECT ?::GEOMETRY" (Only wkt)
            ( query conn "SELECT typeof(system.main.ST_GeomFromWKB(?)), system.main.ST_GeomFromWKB(?)" (rawGeometryWKB geometry, rawGeometryWKB geometry)
                :: IO [(Text, RawGeometry)]
              )
              >>= (@?= [("GEOMETRY", geometry)])
    , testCase "raw empty containers retain layouts that decoded values do not store" $
        withConnection ":memory:" \conn ->
          forM_
            ( [ (family <> " " <> dimensions <> " EMPTY", family <> " EMPTY")
              | family <- ["MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON", "GEOMETRYCOLLECTION"]
              , dimensions <- ["Z", "M", "ZM"]
              ]
                ++ [
                     ( "GEOMETRYCOLLECTION ZM (MULTIPOINT ZM EMPTY, GEOMETRYCOLLECTION ZM EMPTY)"
                     , "GEOMETRYCOLLECTION (MULTIPOINT EMPTY, GEOMETRYCOLLECTION EMPTY)"
                     )
                   ]
            )
            \(wkt, normalizedWKT) -> do
              [Only raw] <- query conn "SELECT ?::GEOMETRY('OGC:CRS84')" (Only (wkt :: Text)) :: IO [Only RawGeometry]
              (importRaw conn raw) >>= (@?= [Only raw])
              decoded <- either assertFailure pure (fromRawGeometry raw)
              WKT.encodeWKT decoded @?= Right normalizedWKT
              normalized <- either assertFailure pure (toRawGeometry decoded)
              rawGeometryCRS normalized @?= Nothing
              assertBool "decoded empty containers must not retain an unstored dimension tag" (rawGeometryWKB normalized /= rawGeometryWKB raw)
    , testCase "mixed-layout collections retain WKB but native import rejects them" $
        withConnection ":memory:" \conn -> do
          let geometry = G.GeometryCollection (V.fromList [G.PointGeometry (G.PointXY (G.XY 1 2)), G.PointGeometry (G.PointXYZ (G.XYZ 3 4 5))])
          raw <- either assertFailure pure (toRawGeometry geometry)
          fromRawGeometry raw @?= Right geometry
          (fromRawGeometry raw >>= toRawGeometry) @?= Right raw
          assertSqlErrorType ErrorInputInvalid (query conn "SELECT ?" (Only geometry) :: IO [Only G.Geometry])
          assertMixedLayoutFailure ErrorInputInvalid (importRaw conn raw)
          assertMixedLayoutFailure
            ErrorInputInvalid
            (query conn "SELECT system.main.ST_GeomFromWKB(?)" (Only (rawGeometryWKB raw)) :: IO [Only G.Geometry])
          assertMixedLayoutFailure ErrorTypeConversion (query conn "SELECT ?::GEOMETRY" (Only (rawGeometryWKB raw)) :: IO [Only RawGeometry])
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
          (importRaw conn raw) >>= (@?= [Only raw])
          decoded <- either assertFailure pure (fromRawGeometry raw)
          decoded @?= G.PointGeometry (G.EmptyPoint G.DimXYZ)
          normalized <- either assertFailure pure (toRawGeometry decoded)
          assertBool "decoding an XY-NaN point normalizes its extra ordinates" (rawGeometryWKB normalized /= rawGeometryWKB raw)
          (query conn "SELECT ?" (Only decoded) :: IO [Only G.Geometry]) >>= (@?= [Only decoded])
    , testCase "binary import preserves distinct NaN payload bits" $
        withConnection ":memory:" \conn -> do
          let shape = G.PointGeometry (G.PointXYZ (G.XYZ (castWord64ToDouble 0x7ff8000000000001) (castWord64ToDouble 0x7ff8000000000002) 7))
          raw <- either assertFailure pure (toRawGeometry shape)
          (importRaw conn raw) >>= (@?= [Only raw])
          fromRawGeometry raw @?= Right (G.PointGeometry (G.EmptyPoint G.DimXYZ))
    , testCase "mixed empty and nonempty layouts retain WKB but native import rejects them" $
        withConnection ":memory:" \conn -> do
          let shape = G.GeometryCollection (V.fromList [G.PointGeometry (G.EmptyPoint G.DimXY), G.PointGeometry (G.PointXYZ (G.XYZ 1 2 3))])
          raw <- either assertFailure pure (toRawGeometry shape)
          fromRawGeometry raw @?= Right shape
          (fromRawGeometry raw >>= toRawGeometry) @?= Right raw
          assertMixedLayoutFailure ErrorInputInvalid (importRaw conn raw)
          assertMixedLayoutFailure
            ErrorInputInvalid
            (query conn "SELECT system.main.ST_GeomFromWKB(?)" (Only (rawGeometryWKB raw)) :: IO [Only G.Geometry])
          (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
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
    , testCase "CRS survives explicit binary import and table insertion" $
        withConnection ":memory:" \conn -> do
          [Only geometry] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')"
          rawGeometryCRS geometry @?= Just "OGC:CRS84"
          (importRaw conn geometry)
            >>= (@?= [Only geometry])
          _ <- execute_ conn "CREATE TABLE shapes (shape GEOMETRY('OGC:CRS84'))"
          _ <- execute conn "INSERT INTO shapes VALUES (system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?))" (rawParameters geometry)
          (query_ conn "SELECT shape FROM shapes" :: IO [Only RawGeometry]) >>= (@?= [Only geometry])
    , testCase "CRS parameters preserve custom names and full definitions" $
        withConnection ":memory:" \conn -> do
          [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY"
          forM_
            [ "local' íslenska λ"
            , "ENGCRS[\"Local grid\",EDATUM[\"Local datum\"],CS[Cartesian,2],AXIS[\"x\",east,ORDER[1],LENGTHUNIT[\"metre\",1]],AXIS[\"y\",north,ORDER[2],LENGTHUNIT[\"metre\",1]]]"
            , "{\"type\":\"EngineeringCRS\",\"name\":\"Local grid\",\"datum\":{\"type\":\"EngineeringDatum\",\"name\":\"Local datum\"},\"coordinate_system\":{\"subtype\":\"Cartesian\",\"axis\":[{\"name\":\"x\",\"abbreviation\":\"x\",\"direction\":\"east\",\"unit\":\"metre\"},{\"name\":\"y\",\"abbreviation\":\"y\",\"direction\":\"north\",\"unit\":\"metre\"}]}}"
            ]
            \crs -> do
              let annotated = geometry{rawGeometryCRS = Just crs}
              (importRaw conn annotated) >>= (@?= [Only annotated])
    , testCase "geometry construction ignores user macros with built-in names" $
        withConnection ":memory:" \conn -> do
          [Only raw] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
          decoded <- either assertFailure pure (fromRawGeometry raw)
          _ <- execute_ conn "CREATE MACRO ST_AsText(x) AS 42"
          _ <- execute_ conn "CREATE MACRO ST_GeomFromWKB(x) AS 'POINT (9 9)'::GEOMETRY"
          _ <- execute_ conn "CREATE MACRO ST_SetCRS(x, crs) AS 42"
          ( query conn "SELECT system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?), ?" (rawGeometryWKB raw, rawGeometryCRS raw, decoded)
              :: IO [(RawGeometry, G.Geometry)]
            )
            >>= (@?= [(raw, decoded)])
    , testCase "NULL and empty geometry stay distinct" $
        withConnection ":memory:" \conn -> do
          [Only empty] <- query_ conn "SELECT 'POINT EMPTY'::GEOMETRY" :: IO [Only RawGeometry]
          ( query
              conn
              "SELECT system.main.ST_GeomFromWKB(?), system.main.ST_GeomFromWKB(?)"
              (Nothing :: Maybe BS.ByteString, Just (rawGeometryWKB empty))
              :: IO [(Maybe RawGeometry, Maybe RawGeometry)]
            )
            >>= (@?= [(Nothing, Just empty)])
    , testCase "binary import constructs arrays with a common CRS and NULL elements" $
        withConnection ":memory:" \conn -> do
          [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
          let values = sized [Nothing, Just geometry, Nothing] :: SizedArray 3 (Maybe RawGeometry)
          ( query conn "SELECT [NULL, system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?), NULL]::GEOMETRY('OGC:CRS84')[3]" (rawParameters geometry)
              :: IO [Only (SizedArray 3 (Maybe RawGeometry))]
            )
            >>= (@?= [Only values])
    , testCase "nested STRUCT, LIST, ARRAY and MAP results bind again with their bytes and CRS" $
        withConnection ":memory:" \conn -> do
          wkbs <- shapeBytes conn
          forM_ [("GEOMETRY", Nothing), ("GEOMETRY('OGC:CRS84')", Just "OGC:CRS84")] \(geometryType, crs) ->
            forM_ wkbs \wkb -> do
              [(value, valueType)] <-
                query
                  conn
                  ( Query
                      ( "SELECT v, typeof(v) FROM (SELECT {'shape': g, 'xs': [g, NULL], 'fixed': [g]::"
                          <> geometryType
                          <> "[1], 'map': MAP {'one': g}} AS v FROM (SELECT system.main.ST_GeomFromWKB(?)::"
                          <> geometryType
                          <> " AS g))"
                      )
                  )
                  (Only wkb)
                  :: IO [(StructValue FieldValue, Text)]
              take 1 (elems (structValueFields value)) @?= [StructField "shape" (FieldGeometry (RawGeometry wkb crs))]
              (query conn "SELECT ?, typeof(?)" (value, value)) >>= (@?= [(value, valueType)])
    , testCase "UNION geometry results retain CRS and bind again" $
        withConnection ":memory:" \conn -> do
          [(value, valueType)] <-
            query_ conn "SELECT u, typeof(u) FROM (SELECT union_value(shape := 'POINT (1 2)'::GEOMETRY('OGC:CRS84')) AS u)"
              :: IO [(UnionValue FieldValue, Text)]
          (query conn "SELECT ?, typeof(?)" (value, value)) >>= (@?= [(value, valueType)])
          [Only nullValue] <- query_ conn "SELECT union_value(shape := NULL::GEOMETRY('OGC:CRS84'))" :: IO [Only (UnionValue FieldValue)]
          (query conn "SELECT ?" (Only nullValue) :: IO [Only (UnionValue FieldValue)]) >>= (@?= [Only nullValue])
          (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
    , testCase "a raw geometry binds only as a GEOMETRY with the same CRS" $
        withConnection ":memory:" \conn -> do
          [Only raw] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
          let plain = raw{rawGeometryCRS = Nothing}
              fromCRS = "duckdb-simple: a GEOMETRY value with the CRS 'OGC:CRS84' cannot bind as a GEOMETRY with no CRS"
              toCRS = "duckdb-simple: a GEOMETRY value with no CRS cannot bind as a GEOMETRY with the CRS 'OGC:CRS84'"
          forM_
            [ ("GEOMETRY", LogicalTypeGeometry Nothing, raw, fromCRS)
            , ("GEOMETRY", LogicalTypeScalar LogicalTypeIdGeometry, raw, fromCRS)
            , ("GEOMETRY('OGC:CRS84')", LogicalTypeGeometry (Just "OGC:CRS84"), plain, toCRS)
            ]
            \(columnType, declared, value, message) -> do
              _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE stored (v STRUCT(shape " <> columnType <> "))"))
              result <- try (execute conn "INSERT INTO stored VALUES (?)" (Only (geometryStruct declared (FieldGeometry value))))
              case result of
                Left err -> do
                  sqlErrorMessage err @?= message
                  sqlErrorType err @?= Just ErrorInputInvalid
                Right _ -> assertFailure "expected a CRS mismatch error"
              (query_ conn "SELECT count(*) FROM stored" :: IO [Only Int64]) >>= (@?= [Only 0])
          (query conn "SELECT ?" (Only (geometryStruct (LogicalTypeScalar LogicalTypeIdGeometry) (FieldGeometry plain))))
            >>= (@?= [Only (geometryStruct (LogicalTypeGeometry Nothing) (FieldGeometry plain))])
    , testCase "a raw geometry binds as a top-level parameter with its bytes and CRS" $
        withConnection ":memory:" \conn -> do
          wkbs <- shapeBytes conn
          forM_ [("GEOMETRY", Nothing), ("GEOMETRY('OGC:CRS84')", Just "OGC:CRS84")] \(geometryType, crs) -> do
            let values = [RawGeometry wkb crs | wkb <- wkbs]
            forM_ values \value ->
              (query conn "SELECT ?, typeof(?)" (value, value) :: IO [(RawGeometry, Text)]) >>= (@?= [(value, geometryType)])
            _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE stored (i BIGINT, v " <> geometryType <> ")"))
            _ <- executeMany conn "INSERT INTO stored VALUES (?, ?)" (zip [0 :: Int64 ..] values)
            (query_ conn "SELECT v FROM stored ORDER BY i" :: IO [Only RawGeometry]) >>= (@?= map Only values)
    , testCase "a top-level raw geometry follows the CRS casts of DuckDB" $
        withConnection ":memory:" \conn -> do
          [Only raw] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
          let plain = raw{rawGeometryCRS = Nothing}
              other = raw{rawGeometryCRS = Just "OGC:CRS83"}
              mismatch = "Binder Error: Cannot cast GEOMETRY with CRS 'OGC:CRS83' to GEOMETRY with different CRS 'OGC:CRS84'"
              store column value = do
                _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE stored (v " <> column <> ")"))
                _ <- execute conn "INSERT INTO stored VALUES (?)" (Only value)
                query_ conn "SELECT v FROM stored" :: IO [Only RawGeometry]
          -- A value without a CRS takes the CRS of its column.
          store "GEOMETRY('OGC:CRS84')" plain >>= (@?= [Only raw])
          -- A column without a CRS removes the CRS of the value.
          store "GEOMETRY" raw >>= (@?= [Only plain])
          store "GEOMETRY" other >>= (@?= [Only plain])
          -- DuckDB does not cast a GEOMETRY to a GEOMETRY with another CRS.
          _ <- execute_ conn "CREATE OR REPLACE TABLE stored (v GEOMETRY('OGC:CRS84'))"
          inserted <- try (execute conn "INSERT INTO stored VALUES (?)" (Only other))
          case inserted of
            Left err -> do
              sqlErrorMessage err @?= mismatch
              sqlErrorType err @?= Just ErrorQueryBinder
            Right _ -> assertFailure "expected the CRS cast error of DuckDB"
          (query_ conn "SELECT count(*) FROM stored" :: IO [Only Int64]) >>= (@?= [Only 0])
          -- A comparison follows the same casts.
          _ <- execute conn "INSERT INTO stored VALUES (?)" (Only raw)
          (query conn "SELECT count(*) FROM stored WHERE v = ?" (Only plain) :: IO [Only Int64]) >>= (@?= [Only 1])
          compared <- try (query conn "SELECT count(*) FROM stored WHERE v = ?" (Only other) :: IO [Only Int64])
          case compared of
            Left err -> do
              sqlErrorMessage err @?= mismatch
              sqlErrorType err @?= Just ErrorQueryBinder
            Right rows -> assertFailure ("expected the CRS cast error of DuckDB, got " <> show rows)
    , testCase "a Maybe RawGeometry parameter binds NULL and a value" $
        withConnection ":memory:" \conn -> do
          [Only raw] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
          _ <- execute_ conn "CREATE TABLE stored (i BIGINT, v GEOMETRY('OGC:CRS84'))"
          _ <- executeMany conn "INSERT INTO stored VALUES (?, ?)" [(0 :: Int64, Just raw), (1, Nothing)]
          (query_ conn "SELECT v FROM stored ORDER BY i" :: IO [Only (Maybe RawGeometry)]) >>= (@?= [Only (Just raw), Only Nothing])
          (query conn "SELECT ? IS NULL" (Only (Nothing :: Maybe RawGeometry)) :: IO [Only Bool]) >>= (@?= [Only True])
    , testCase "inactive UNION geometry members keep the CRS of their declared type" $
        withConnection ":memory:" \conn -> do
          [Only value] <-
            query_ conn "SELECT union_value(number := 42::BIGINT)::UNION(number BIGINT, shape GEOMETRY('OGC:CRS84'))"
              :: IO [Only (UnionValue FieldValue)]
          (query conn "SELECT typeof(?)" (Only value) :: IO [Only Text]) >>= (@?= [Only "UNION(number BIGINT, shape GEOMETRY('OGC:CRS84'))"])
          (query conn "SELECT ?" (Only value) :: IO [Only (UnionValue FieldValue)]) >>= (@?= [Only value])
    , testCase "NULL and empty nested geometry collections keep the CRS of their declared type" $
        withConnection ":memory:" \conn -> do
          _ <-
            execute_
              conn
              "CREATE TABLE nested AS SELECT {'null_list': NULL::GEOMETRY('OGC:CRS84')[], 'empty_list': []::GEOMETRY('OGC:CRS84')[], 'null_array': NULL::GEOMETRY('OGC:CRS84')[2], 'array': [NULL, NULL]::GEOMETRY('OGC:CRS84')[2], 'null_map': NULL::MAP(VARCHAR, GEOMETRY('OGC:CRS84')), 'empty_map': map([], [])::MAP(VARCHAR, GEOMETRY('OGC:CRS84')), 'map': map(['one'], [NULL::GEOMETRY('OGC:CRS84')])} AS v"
          [Only value] <- query_ conn "SELECT v FROM nested" :: IO [Only (StructValue FieldValue)]
          (query conn "SELECT ?" (Only value) :: IO [Only (StructValue FieldValue)]) >>= (@?= [Only value])
          _ <- execute conn "INSERT INTO nested VALUES (?)" (Only value)
          (query_ conn "SELECT v FROM nested" :: IO [Only (StructValue FieldValue)]) >>= (@?= [Only value, Only value])
    , testCase "configured custom CRS and recognized definitions retain metadata" $ do
        let crs = "local' íslenska λ"
        withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionGeometryCRS = [crs]} \conn -> do
          [Only typed] <- query conn "SELECT typeof(?)" (Only (nullShape crs)) :: IO [Only Text]
          assertBool (Text.unpack typed) ("íslenska λ" `Text.isInfixOf` typed)
          (query conn "SELECT ?" (Only (nullShape crs))) >>= (@?= [Only (nullShape crs)])
          (query conn "SELECT typeof(?)" (Only (nullShape "OGC:CRS84")) :: IO [Only Text]) >>= (@?= [Only "STRUCT(shape GEOMETRY('OGC:CRS84'))"])
          (query conn "SELECT ?" (Only (nullShape "OGC:CRS84"))) >>= (@?= [Only (nullShape "OGC:CRS84")])
    , testCase "configured CRS survives nested parameter types and prepared rebinding" $ do
        let crs = "local' íslenska λ"
            value = nestedNullShapes crs
            array = sized [value] :: SizedArray 1 (StructValue FieldValue)
        withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionGeometryCRS = [crs, crs]} \conn -> do
          (query conn "SELECT ?" (Only value)) >>= (@?= [Only value])
          (queryNamed conn "SELECT $shape" ["shape" := value]) >>= (@?= [Only value])
          (query conn "SELECT ?" (Only array)) >>= (@?= [Only array])
          withStatement conn "SELECT ?" \stmt -> do
            bind stmt [toField value]
            nextRow stmt >>= (@?= Just (Only value))
            bind stmt [toField value]
            nextRow stmt >>= (@?= Just (Only value))
    , testCase "a configured CRS bind leaves an active caller stream usable" $ do
        let crs = "local' íslenska λ"
        withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionGeometryCRS = [crs]} \conn ->
          withStatement conn "SELECT ?" \parameter -> do
            withStatement conn "SELECT i::BIGINT FROM range(3) t(i)" \stmt -> do
              Streaming.nextRow stmt >>= (@?= Just (Only (0 :: Int64)))
              bind parameter [toField (nullShape crs)]
              performMajorGC
              Streaming.nextRow stmt >>= (@?= Just (Only (1 :: Int64)))
            nextRow parameter >>= (@?= Just (Only (nullShape crs)))
    , testCase "unknown CRS cannot silently drop metadata" $
        withConnectionWithConfig ":memory:" [("ignore_unknown_crs", "true")] \conn -> do
          result <- try (query conn "SELECT ?" (Only (nullShape "local grid"))) :: IO (Either SQLError [Only (StructValue FieldValue)])
          case result of
            Left err -> do
              sqlErrorType err @?= Just ErrorInputInvalid
              assertBool (show err) ("duckdb-simple: DuckDB did not preserve the requested GEOMETRY CRS" `Text.isInfixOf` sqlErrorMessage err)
            Right _ -> assertFailure "expected the unknown CRS to fail"
    , testCase "connection options validate CRS definitions and pass configuration flags" $ do
        forM_ ["", "OGC:CRS84\0bad", " "] \crs -> do
          result <- try (withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionGeometryCRS = [crs]} (const (pure ())))
          case result of
            Left err -> sqlErrorType err @?= Just ErrorInputInvalid
            Right () -> assertFailure ("expected the open to fail for " <> show crs)
        let options = defaultConnectionOptions{connectionConfig = [("threads", "1")], connectionGeometryCRS = ["local grid", "OGC:CRS84", "OGC:CRS84"]}
        withConnectionWithOptions ":memory:" options \conn ->
          (query_ conn "SELECT current_setting('threads')" :: IO [Only Int64]) >>= (@?= [Only 1])
    , testCase "logical type construction preserves CRS" $
        withConnection ":memory:" \conn -> withConnectionHandle conn \native -> withConnectionFactory native \factory -> do
          let logical = LogicalTypeList (LogicalTypeGeometry (Just "OGC:CRS84"))
          bracket (logicalTypeFromRep factory logical) destroyLogicalType (logicalTypeToRep factory) >>= (@?= logical)
    , testGroup
        "folds cross chunk boundaries and preserve CRS"
        [ testCase mode $
            withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
              [Only expected] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
              count <- foldRows conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84') FROM range(5000)" (0 :: Int64) \n (Only actual) -> do
                actual @?= expected
                pure (n + 1)
              count @?= 5000
        | (mode, foldRows) <- [("materialized", fold_), ("native streaming", Streaming.fold_)]
        ]
    , testCase "invalid binary import leaves the connection usable" $
        withConnection ":memory:" \conn -> do
          assertSqlErrorType
            ErrorInputInvalid
            (query conn "SELECT system.main.ST_GeomFromWKB(?)" (Only (BS.pack [1, 1, 0, 0, 0])) :: IO [Only RawGeometry])
          (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
    , testCase "configured recognized, custom, and complete CRS names bind with their CRS" $ do
        let names = ["OGC:CRS84", "local' íslenska λ", localGridWKT2]
        withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionGeometryCRS = names} \conn ->
          forM_ names \crs -> (query conn "SELECT ?" (Only (nullShape crs))) >>= (@?= [Only (nullShape crs)])
    , testCase "a default connection runs no CRS query, also for a VARIANT read" $
        withLoggedInstance \inst countQueries -> withConnectionOn inst \conn -> do
          (query conn "SELECT ?" (Only (nullShape "OGC:CRS84"))) >>= (@?= [Only (nullShape "OGC:CRS84")])
          (query_ conn "SELECT 42::VARIANT" :: IO [Only Variant]) >>= (@?= [Only (Variant (FieldInt32 42))])
          countQueries crsQuery >>= (@?= 0)
          -- The log records metadata preparation and execution on the helper connection.
          countQueries "variant_typeof" >>= (@?= 2)
    , testCase "a custom CRS runs one CRS query when the connection opens, and a VARIANT read runs none" $
        withLoggedInstance \inst countQueries ->
          bracket (connectWithOptions inst defaultConnectionOptions{connectionGeometryCRS = ["local grid"]}) close \conn -> do
            countQueries crsQuery >>= (@?= 1)
            (query conn "SELECT ?" (Only (nullShape "local grid"))) >>= (@?= [Only (nullShape "local grid")])
            (query_ conn "SELECT 42::VARIANT" :: IO [Only Variant]) >>= (@?= [Only (Variant (FieldInt32 42))])
            countQueries crsQuery >>= (@?= 1)
            countQueries "variant_typeof" >>= (@?= 2)
    , testCase "a configured recognized CRS binds in either spelling" $
        withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionGeometryCRS = ["ogc:crs84"]} \conn ->
          forM_ ["ogc:crs84", "OGC:CRS84"] \crs -> (query conn "SELECT ?" (Only (nullShape crs))) >>= (@?= [Only (nullShape "OGC:CRS84")])
    , testCase "a configured CRS that DuckDB drops fails the open and releases its connection" $
        withEnvironment \env -> do
          inst <- createInstance env []
          -- The database keeps the function, and the function keeps the token.
          weak <- withConnectionOn inst \conn -> do
            token <- newIORef (42 :: Int64)
            createFunction conn "crs_token" (readIORef token)
            mkWeakIORef token (pure ())
          -- The open keeps the type of the first name before the second name fails.
          result <- try (connectWithOptions inst defaultConnectionOptions{connectionGeometryCRS = ["OGC:CRS84", " "]})
          case result of
            Left err -> do
              sqlErrorType err @?= Just ErrorInputInvalid
              assertBool (Text.unpack (sqlErrorMessage err)) ("did not preserve a configured GEOMETRY CRS" `Text.isInfixOf` sqlErrorMessage err)
            Right conn -> close conn >> assertFailure "expected the open to fail"
          -- DuckDB destroys the instance and the function after the last native
          -- connection. A connection that the failed open kept would keep the token.
          closeInstance inst
          waitForRelease weak (500 :: Int)
    ]
  where
    crsQuery = "system.main.ST_SetCRS(system.main.ST_GeomFromWKB("
    waitForRelease weak attempts
      | attempts == 0 = assertFailure "the failed open kept a native connection"
      | otherwise = do
          performMajorGC
          alive <- isJust <$> deRefWeak weak
          when alive (threadDelay 10000 >> waitForRelease weak (attempts - 1))

-- | Run an action on an instance that logs each query. The second argument
-- counts the logged queries whose text contains a part. The count query binds
-- the part as a value, so its own text does not match.
withLoggedInstance :: (Instance -> (Text -> IO Int64) -> IO a) -> IO a
withLoggedInstance action = withEnvironment \env -> withInstance env [] \inst -> withConnectionOn inst \logger -> do
  _ <- execute_ logger "CALL enable_logging(level := 'info')"
  let countQueries part = do
        rows <- query logger "SELECT count(*) FROM duckdb_logs() WHERE type = 'QueryLog' AND contains(message, ?)" (Only part)
        case rows of
          [Only count] -> pure count
          _ -> assertFailure "expected one count"
  action inst countQueries

-- | A complete WKT2 definition that no DuckDB catalog entry identifies.
localGridWKT2 :: Text
localGridWKT2 =
  "ENGCRS[\"Local grid\",EDATUM[\"Local datum\"],CS[Cartesian,2],AXIS[\"x\",east,ORDER[1],LENGTHUNIT[\"metre\",1]],AXIS[\"y\",north,ORDER[2],LENGTHUNIT[\"metre\",1]]]"

-- | A one-field STRUCT with a NULL GEOMETRY of the given CRS.
nullShape :: Text -> StructValue FieldValue
nullShape crs = geometryStruct (LogicalTypeGeometry (Just crs)) FieldNull

-- | A one-field STRUCT with a value of the given declared GEOMETRY type.
geometryStruct :: LogicalTypeRep -> FieldValue -> StructValue FieldValue
geometryStruct logical value =
  StructValue
    { structValueFields = listArray (0, 0) [StructField "shape" value]
    , structValueTypes = listArray (0, 0) [StructField "shape" logical]
    , structValueIndex = Map.singleton "shape" 0
    }

-- | Retain custom CRS metadata in NULL, empty, and inactive nested values.
nestedNullShapes :: Text -> StructValue FieldValue
nestedNullShapes crs =
  StructValue
    { structValueFields = listArray (0, length fields - 1) [StructField name value | (name, value, _) <- fields]
    , structValueTypes = listArray (0, length fields - 1) [StructField name logical | (name, _, logical) <- fields]
    , structValueIndex = Map.fromList (zip [name | (name, _, _) <- fields] [0 ..])
    }
  where
    geometry = LogicalTypeGeometry (Just crs)
    list = LogicalTypeList geometry
    array = LogicalTypeArray geometry 2
    mapping = LogicalTypeMap (LogicalTypeScalar LogicalTypeIdVarchar) geometry
    members = listArray (0, 1) [UnionMemberType "number" (LogicalTypeScalar LogicalTypeIdBigInt), UnionMemberType "shape" geometry]
    inactive = UnionValue 0 "number" (FieldInt64 42) members
    shape = nullShape crs
    fields =
      [ ("null_list", FieldNull, list)
      , ("empty_list", FieldList [], list)
      , ("null_array", FieldNull, array)
      , ("array", FieldArray (listArray (0, 1) [FieldNull, FieldNull]), array)
      , ("null_map", FieldNull, mapping)
      , ("empty_map", FieldMap [], mapping)
      , ("map", FieldMap [(FieldText "one", FieldNull)], mapping)
      , ("inactive", FieldUnion inactive, LogicalTypeUnion members)
      , ("nested", FieldStruct shape, LogicalTypeStruct (structValueTypes shape))
      ]

-- | Exercise all coordinate layouts with explicit point and sequence constructors.
shapeRoundTrips
  :: (G.Coordinate coord) => Connection -> G.Dimensions -> (coord -> G.Point) -> (U.Vector coord -> G.Coordinates) -> coord -> coord -> Assertion
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
    (importRaw conn annotated)
      >>= (@?= [Only annotated])

-- | Import WKB without converting a missing CRS to SQL NULL.
importRaw :: Connection -> RawGeometry -> IO [Only RawGeometry]
importRaw conn raw = case rawGeometryCRS raw of
  Nothing -> query conn "SELECT system.main.ST_GeomFromWKB(?)" (Only (rawGeometryWKB raw))
  Just crs -> query conn "SELECT system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?)" (rawGeometryWKB raw, crs)

-- | Bind raw bytes and CRS as separate SQL parameters.
rawParameters :: RawGeometry -> (BS.ByteString, Maybe Text)
rawParameters raw = (rawGeometryWKB raw, rawGeometryCRS raw)

-- | Check the native rejection without accepting unrelated failures.
assertMixedLayoutFailure :: ErrorType -> IO a -> Assertion
assertMixedLayoutFailure expectedCode action = do
  result <- try (action >> pure ()) :: IO (Either SQLError ())
  case result of
    Left err -> do
      sqlErrorType err @?= Just expectedCode
      assertBool (show err) ("inconsistent Z/M dimensions" `Text.isInfixOf` sqlErrorMessage err)
    Right () -> assertFailure "expected native mixed-layout geometry rejection"

-- | The WKB bytes of each native shape and of a point with NaN payload bits.
shapeBytes :: Connection -> IO [BS.ByteString]
shapeBytes conn = do
  native <- forM shapes \wkt -> do
    [Only raw] <- query conn "SELECT ?::GEOMETRY" (Only wkt) :: IO [Only RawGeometry]
    pure (rawGeometryWKB raw)
  nanPoint <-
    either
      assertFailure
      pure
      (WKB.encodeWKB (G.PointGeometry (G.PointXYZ (G.XYZ (castWord64ToDouble 0x7ff8000000000001) (castWord64ToDouble 0x7ff8000000000002) 7))))
  pure (native <> [nanPoint])

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
