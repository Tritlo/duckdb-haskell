{-# LANGUAGE OverloadedStrings #-}

-- | Pure ISO WKB rendering and malformed input tests.
module GeometryTests (tests) where

import Control.Monad (forM_)
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32, Word64)
import Database.DuckDB.Simple.Geometry (Geometry (..), geometryToWKT)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- | Tests that require no native library or database connection.
tests :: TestTree
tests =
    testGroup
        "geometry WKB"
        [ testGroup
            "known point bytes"
            [ testCase "little endian" $
                render (hexBytes "0101000000000000000000f03f0000000000000040") @?= Right "POINT (1.0 2.0)"
            , testCase "big endian" $
                render (hexBytes "00000000013ff00000000000004000000000000000") @?= Right "POINT (1.0 2.0)"
            , testCase "empty NaN point" $
                render (hexBytes "0101000000000000000000f87f000000000000f87f") @?= Right "POINT EMPTY"
            ]
        , testGroup
            "families, dimensions, and byte orders"
            [ testCase label $ render bytes @?= Right expected
            | (label, bytes, expected) <- samples
            ]
        , testCase "CRS metadata is independent of WKT" $ do
            let bytes = point True 0 [1, 2]
                geometry = Geometry bytes (Just "OGC:CRS84")
            geometryWKB geometry @?= bytes
            geometryCRS geometry @?= Just "OGC:CRS84"
            geometryToWKT geometry @?= render bytes
            assertBool "CRS participates in equality" (geometry /= Geometry bytes Nothing)
        , testCase "mixed collection families, dimensions, nesting, and byte orders" $ do
            let inner = children False 2007 [point True 2000 [3, 4, 5], wkb False 1002 (count False 0)]
                outer = children True 3007 [point False 0 [1, 2], inner, point True 3000 (replicate 4 (0 / 0))]
            render outer @?= Right "GEOMETRYCOLLECTION ZM (POINT (1.0 2.0), GEOMETRYCOLLECTION M (POINT M (3.0 4.0 5.0), LINESTRING Z EMPTY), POINT ZM EMPTY)"
        , testCase "finite Double values retain exact bits and negative zero" $
            forM_ finiteWords $ \word -> do
                let value = castWord64ToDouble word
                forM_ [False, True] $ \little ->
                    case render (point little 0 [value, -0.0]) of
                        Left message -> assertFailure message
                        Right wkt -> case Text.words (Text.dropEnd 1 (Text.drop 7 wkt)) of
                            [x, y] -> do
                                castDoubleToWord64 (read (Text.unpack x) :: Double) @?= word
                                castDoubleToWord64 (read (Text.unpack y) :: Double) @?= 0x8000000000000000
                            _ -> assertFailure ("Unexpected WKT: " ++ Text.unpack wkt)
        , testCase "empty rings and empty polygon children" $ do
            let polygon = wkb True 3 (count True 1 <> count True 0)
            render polygon @?= Right "POLYGON (EMPTY)"
            render (children False 6 [polygon, wkb True 3 (count True 0)]) @?= Right "MULTIPOLYGON ((EMPTY), EMPTY)"
        , testCase "every proper prefix is truncated" $
            forM_ samples $ \(label, bytes, _) ->
                forM_ [0 .. BS.length bytes - 1] $ \size ->
                    assertRejected (label ++ " prefix " ++ show size) (BS.take size bytes)
        , testCase "trailing bytes are rejected" $
            forM_ samples $ \(label, bytes, _) ->
                assertRejected label (bytes <> BS.singleton 0)
        , testGroup
            "invalid input"
            [ testCase label $ assertRejected label bytes
            | (label, bytes) <- malformed
            ]
        , testCase "16 geometry levels are accepted" $
            render (nested 15) @?= Right (nestWKT 15)
        , testCase "17 geometry levels are rejected" $
            assertRejected "depth limit" (nested 16)
        , testCase "large shallow collection is accepted" $ do
            let child = wkb True 7 (count True 0)
                bytes = children True 7 (replicate 4096 child)
            render bytes @?= Right ("GEOMETRYCOLLECTION (" <> Text.intercalate ", " (replicate 4096 "GEOMETRYCOLLECTION EMPTY") <> ")")
        ]

-- | Render a fixture without CRS metadata.
render :: ByteString -> Either String Text
render bytes = geometryToWKT (Geometry bytes Nothing)

-- | Require a controlled parse failure.
assertRejected :: String -> ByteString -> IO ()
assertRejected label bytes = case render bytes of
    Left _ -> pure ()
    Right wkt -> assertFailure (label ++ " accepted: " ++ Text.unpack wkt)

-- | Known WKB cases for every standard family, dimension, and byte order.
samples :: [(String, ByteString, Text)]
samples = do
    little <- [False, True]
    dimension <- [0 .. 3]
    let offset = dimension * 1000
        size = if dimension == 3 then 4 else if dimension == 0 then 2 else 3
        values = take size [1, 2, 3, 4]
        coord = Text.intercalate " " (map (Text.pack . show) values)
        suffix = ["", " Z", " M", " ZM"] !! fromIntegral dimension
        emptyPoint = point (not little) offset (replicate size (0 / 0))
        fullPoint = point (not little) offset values
        linePayload = count little 2 <> coordinates little values <> coordinates little values
        fullLine = wkb little (offset + 2) linePayload
        emptyLine = wkb (not little) (offset + 2) (count (not little) 0)
        ringPayload = count little 4 <> BS.concat (replicate 4 (coordinates little values))
        fullPolygon = wkb little (offset + 3) (count little 1 <> ringPayload)
        emptyPolygon = wkb (not little) (offset + 3) (count (not little) 0)
        lineBody = "(" <> coord <> ", " <> coord <> ")"
        polygonBody = "((" <> Text.intercalate ", " (replicate 4 coord) <> "))"
        cases =
            [ (1, "POINT", point little offset values, "(" <> coord <> ")")
            , (2, "LINESTRING", fullLine, lineBody)
            , (3, "POLYGON", fullPolygon, polygonBody)
            , (4, "MULTIPOINT", children little (offset + 4) [fullPoint, emptyPoint], "((" <> coord <> "), EMPTY)")
            , (5, "MULTILINESTRING", children little (offset + 5) [fullLine, emptyLine], "(" <> lineBody <> ", EMPTY)")
            , (6, "MULTIPOLYGON", children little (offset + 6) [fullPolygon, emptyPolygon], "(" <> polygonBody <> ", EMPTY)")
            , (7, "GEOMETRYCOLLECTION", children little (offset + 7) [fullPoint, emptyLine], "(POINT" <> suffix <> " (" <> coord <> "), LINESTRING" <> suffix <> " EMPTY)")
            ]
    (family, name, full, body) <- cases
    empty <- [False, True]
    let bytes = if empty then if family == 1 then point little offset (replicate size (0 / 0)) else wkb little (offset + family) (count little 0) else full
        label = Text.unpack (name <> suffix) ++ " " ++ show little ++ " empty=" ++ show empty
    pure (label, bytes, name <> suffix <> " " <> if empty then "EMPTY" else body)

-- | Invalid encodings, counts, coordinates, and multi-geometry children.
malformed :: [(String, ByteString)]
malformed =
    [("byte order " ++ show marker, BS.singleton marker <> BS.drop 1 (point True 0 [1, 2])) | marker <- [2, 255]]
        ++ [("type " ++ show tag, wkb True tag (coordinates True [1, 2])) | tag <- [0, 8, 1000, 4001, 0x80000001, 0x20000001, maxBound]]
        ++ [ ("count " ++ show family ++ " " ++ show little, wkb little family (count little maxBound))
           | little <- [False, True]
           , family <- [2 .. 7]
           ]
        ++ [ ("line count too large", wkb True 2 (count True 2 <> coordinates True [1, 2]))
           , ("coordinate count multiplication overflow", wkb True 2 (count True 0x10000000 <> coordinates True [1, 2]))
           , ("ring count too large", wkb True 3 (count True 2 <> count True 0))
           , ("ring coordinate count too large", wkb True 3 (count True 1 <> count True maxBound))
           , ("child count too large", wkb True 7 (count True 2 <> wkb True 7 (count True 0)))
           , ("point partially NaN", point True 0 [0 / 0, 2])
           , ("point positive infinity", point True 0 [1 / 0, 2])
           , ("point negative infinity", point False 0 [1, -1 / 0])
           , ("line NaN coordinate", wkb True 2 (count True 1 <> coordinates True [0 / 0, 0 / 0]))
           , ("point Z partial NaN", point True 1000 [0 / 0, 0 / 0, 3])
           , ("point ZM infinite measure", point False 3000 [1, 2, 3, 1 / 0])
           , ("collection child byte order", children True 7 [BS.singleton 2 <> BS.drop 1 (point True 0 [1, 2])])
           , ("collection child type", children True 7 [wkb False 8 (count False 0)])
           ]
        ++ [ ("multi family mismatch " ++ show family, children True family [wkb False 7 (count False 0)])
           | family <- [4 .. 6]
           ]
        ++ [ ("multi dimensions mismatch " ++ show family, children True family [wkb False (1000 + family - 3) payload])
           | family <- [4 .. 6]
           , let payload = if family == 4 then coordinates False [1, 2, 3] else count False 0
           ]
        ++ [("multi M is not Z", children True 2004 [point False 1000 [1, 2, 3]])]

-- | Finite IEEE 754 values with subnormal, boundary, and precision cases.
finiteWords :: [Word64]
finiteWords =
    [ 0
    , 0x8000000000000000
    , 1
    , 0x8000000000000001
    , 0x000fffffffffffff
    , 0x0010000000000000
    , 0x3fb999999999999b
    , 0x3ff0000000000001
    , 0x4340000000000001
    , 0x7fefffffffffffff
    , 0xffefffffffffffff
    ]

-- | Build an ISO WKB geometry header and payload.
wkb :: Bool -> Word32 -> ByteString -> ByteString
wkb little tag payload = BS.singleton (if little then 1 else 0) <> count little tag <> payload

-- | Encode a WKB point with explicit dimension metadata.
point :: Bool -> Word32 -> [Double] -> ByteString
point little offset values = wkb little (offset + 1) (coordinates little values)

-- | Encode a child sequence with its own geometry headers.
children :: Bool -> Word32 -> [ByteString] -> ByteString
children little tag parts = wkb little tag (count little (fromIntegral (length parts)) <> BS.concat parts)

-- | Encode a WKB coordinate count or type tag.
count :: Bool -> Word32 -> ByteString
count little value = wordBytes little 4 (fromIntegral value)

-- | Encode IEEE 754 coordinates in the selected byte order.
coordinates :: Bool -> [Double] -> ByteString
coordinates little = BS.concat . map (wordBytes little 8 . castDoubleToWord64)

-- | Encode an unsigned word in the selected byte order.
wordBytes :: Bool -> Int -> Word64 -> ByteString
wordBytes little size value =
    BS.pack [fromIntegral (shiftR value (8 * position)) | position <- if little then [0 .. size - 1] else reverse [0 .. size - 1]]

-- | Decode fixed hexadecimal fixture bytes.
hexBytes :: String -> ByteString
hexBytes = BS.pack . go
  where
    go [] = []
    go (a : b : rest) = fromIntegral (16 * digitToInt a + digitToInt b) : go rest
    go _ = error "Odd hexadecimal fixture length"

-- | Build a point within the selected number of nested collections.
nested :: Int -> ByteString
nested 0 = point True 0 [1, 2]
nested depth = children True 7 [nested (depth - 1)]

-- | Expected WKT for the nested collection fixture.
nestWKT :: Int -> Text
nestWKT 0 = "POINT (1.0 2.0)"
nestWKT depth = "GEOMETRYCOLLECTION (" <> nestWKT (depth - 1) <> ")"
