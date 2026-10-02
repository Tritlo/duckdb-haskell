{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GeometryRegressionTests (tests) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Array (Array, listArray)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Database.DuckDB.Simple.FromField (FieldValue, StructValue, UnionValue)
import Database.DuckDB.Simple.Geometry
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Check native construction, metadata, nesting, and streaming.
tests :: TestTree
tests =
    testGroup
        "geometry integration"
        [ testCase "native WKB round trips through GEOMETRY parameters" $
            withConnection ":memory:" \conn ->
                forM_ shapes \wkt -> do
                    [Only geometry] <- query conn "SELECT ?::GEOMETRY" (Only wkt)
                    (query conn "SELECT typeof(?), ?" (geometry, geometry) :: IO [(Text, Geometry)])
                        >>= (@?= [("GEOMETRY", geometry)])
        , testCase "CRS survives parameter binding and table insertion" $
            withConnection ":memory:" \conn -> do
                [Only geometry] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')"
                geometryCRS geometry @?= Just "OGC:CRS84"
                (query conn "SELECT ST_CRS(?), ?" (geometry, geometry) :: IO [(Text, Geometry)])
                    >>= (@?= [("OGC:CRS84", geometry)])
                _ <- execute_ conn "CREATE TABLE shapes (shape GEOMETRY('OGC:CRS84'))"
                _ <- execute conn "INSERT INTO shapes VALUES (?)" (Only geometry)
                (query_ conn "SELECT shape FROM shapes" :: IO [Only Geometry]) >>= (@?= [Only geometry])
        , testCase "CRS parameters preserve quotes and Unicode" $
            withConnection ":memory:" \conn -> do
                [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY"
                let annotated = geometry{geometryCRS = Just "local' íslenska λ"}
                (query conn "SELECT ?" (Only annotated) :: IO [Only Geometry]) >>= (@?= [Only annotated])
        , testCase "NULL and empty geometry stay distinct" $
            withConnection ":memory:" \conn -> do
                [Only empty] <- query_ conn "SELECT 'POINT EMPTY'::GEOMETRY" :: IO [Only Geometry]
                (query conn "SELECT ?, ?" (Nothing :: Maybe Geometry, Just empty) :: IO [(Maybe Geometry, Maybe Geometry)])
                    >>= (@?= [(Nothing, Just empty)])
        , testCase "arrays keep their common CRS, including NULL elements" $
            withConnection ":memory:" \conn -> do
                [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only Geometry]
                let values = listArray (0, 2) [Nothing, Just geometry, Nothing]
                (query conn "SELECT ?" (Only values) :: IO [Only (Array Int (Maybe Geometry))]) >>= (@?= [Only values])
                assertFailureIO (query conn "SELECT ?" (Only (listArray (0 :: Int, 1) [geometry, geometry{geometryCRS = Nothing}])) :: IO [Only FieldValue])
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
                    [Only expected] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only Geometry]
                    count <- foldRows conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84') FROM range(5000)" (0 :: Int64) \n (Only actual) -> do
                        actual @?= expected
                        pure (n + 1)
                    count @?= 5000
            | (mode, foldRows) <- [("materialized", fold_), ("deprecated streaming", Streaming.fold_)]
            ]
        , testCase "invalid WKB, empty CRS and NUL CRS fail without poisoning the connection" $
            withConnection ":memory:" \conn -> do
                assertFailureIO (query conn "SELECT ?" (Only (Geometry BS.empty Nothing)) :: IO [Only Geometry])
                [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY" :: IO [Only Geometry]
                assertFailureIO (query conn "SELECT ?" (Only geometry{geometryCRS = Just ""}) :: IO [Only Geometry])
                assertFailureIO (query conn "SELECT ?" (Only geometry{geometryCRS = Just "OGC:CRS84\0bad"}) :: IO [Only Geometry])
                (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
        ]

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
    , "MULTIPOINT ((1 2), EMPTY, (3 4))"
    , "MULTILINESTRING ((1 2, 3 4), EMPTY)"
    , "MULTIPOLYGON (((0 0, 1 0, 1 1, 0 0)), EMPTY)"
    , "GEOMETRYCOLLECTION (POINT (1 2), LINESTRING (0 0, 1 1))"
    , "GEOMETRYCOLLECTION ZM (POINT ZM (1 2 3 4), POINT ZM EMPTY)"
    ]
