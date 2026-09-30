{-# LANGUAGE OverloadedStrings #-}

-- | Geometry values with separate coordinate reference system metadata.
module Database.DuckDB.Simple.Geometry (
    Geometry (..),
    geometryToWKT,
) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Float (castWord64ToDouble)

-- | ISO WKB bytes and optional coordinate reference system metadata.
data Geometry = Geometry
    { geometryWKB :: !ByteString
    , geometryCRS :: !(Maybe Text)
    }
    deriving (Eq, Show)

{- | Check ISO WKB and render the seven standard geometry families as WKT.
The renderer supports XY, XYZ, XYM, XYZM, and either byte order.
It preserves finite Double values and negative zero. All-NaN points are empty.
It rejects other non-finite coordinates, invalid counts, and trailing bytes.
Multi-geometries require matching child families and dimensions.
Collections can contain mixed families and dimensions.
The limit is 16 geometry levels, including the root, as in DuckDB 1.5.
CRS metadata does not form part of WKT. This check does not validate topology.
-}
geometryToWKT :: Geometry -> Either String Text
geometryToWKT geometry = do
    (wkt, _, rest) <- readGeometry 0 Nothing (geometryWKB geometry)
    if BS.null rest
        then Right wkt
        else Left "Geometry WKB has trailing bytes"

-- | Read a geometry and return its full WKT, body, and remaining bytes.
readGeometry :: Int -> Maybe (Int, Int) -> ByteString -> Either String (Text, Text, ByteString)
readGeometry depth expected bytes
    | depth >= 16 = Left "Geometry WKB exceeds 16 geometry levels"
    | otherwise = do
        (little, afterEndian) <- readEndian bytes
        (tag, afterTag) <- readWord little 4 afterEndian
        let (dimensionTag, familyTag) = tag `quotRem` 1000
        if dimensionTag > 3 || familyTag < 1 || familyTag > 7
            then Left "Geometry WKB has an unsupported type"
            else do
                let family = fromIntegral familyTag
                    dimension = fromIntegral dimensionTag
                    dimensions = if dimension == 3 then 4 else if dimension == 0 then 2 else 3
                    suffix = case dimension of
                        0 -> ""
                        1 -> " Z"
                        2 -> " M"
                        _ -> " ZM"
                    name = case family of
                        1 -> "POINT"
                        2 -> "LINESTRING"
                        3 -> "POLYGON"
                        4 -> "MULTIPOINT"
                        5 -> "MULTILINESTRING"
                        6 -> "MULTIPOLYGON"
                        _ -> "GEOMETRYCOLLECTION"
                case expected of
                    Just (expectedFamily, expectedDimension)
                        | family /= expectedFamily -> Left "Geometry WKB multi child has the wrong family"
                        | dimension /= expectedDimension -> Left "Geometry WKB multi child has the wrong dimensions"
                    _ -> Right ()
                (body, rest) <- case family of
                    1 -> do
                        (point, remaining) <- readCoordinate little dimensions True afterTag
                        Right (if point == "EMPTY" then point else "(" <> point <> ")", remaining)
                    2 -> readLine little dimensions afterTag
                    3 -> do
                        (count, remaining) <- readCount little 4 afterTag
                        (rings, final) <- readItems count (readLine little dimensions) remaining
                        Right (renderParts rings, final)
                    _ -> do
                        (count, remaining) <- readCount little 9 afterTag
                        let childType = if family == 7 then Nothing else Just (family - 3, dimension)
                            readChild input = do
                                (full, childBody, final) <- readGeometry (depth + 1) childType input
                                Right (if family == 7 then full else childBody, final)
                        (children, final) <- readItems count readChild remaining
                        Right (renderParts children, final)
                Right (name <> suffix <> " " <> body, body, rest)

-- | Read and check the WKB byte order marker.
readEndian :: ByteString -> Either String (Bool, ByteString)
readEndian bytes = case BS.uncons bytes of
    Just (0, rest) -> Right (False, rest)
    Just (1, rest) -> Right (True, rest)
    Just _ -> Left "Geometry WKB has an invalid byte order"
    Nothing -> Left "Geometry WKB is truncated at its byte order"

-- | Read a checked unsigned word of at most eight bytes.
readWord :: Bool -> Int -> ByteString -> Either String (Word64, ByteString)
readWord little size bytes
    | BS.length bytes < size = Left "Geometry WKB is truncated"
    | otherwise =
        let (wordBytes, rest) = BS.splitAt size bytes
            ordered = if little then BS.reverse wordBytes else wordBytes
            value = BS.foldl' (\acc byte -> shiftL acc 8 .|. fromIntegral byte) 0 ordered
         in Right (value, rest)

-- | Check a count against the remaining bytes before conversion to Int.
readCount :: Bool -> Int -> ByteString -> Either String (Int, ByteString)
readCount little minimumSize bytes = do
    (count, rest) <- readWord little 4 bytes
    if count > fromIntegral (BS.length rest `div` minimumSize)
        then Left "Geometry WKB count exceeds the remaining bytes"
        else Right (fromIntegral count, rest)

-- | Read a bounded sequence with a tail-recursive loop.
readItems :: Int -> (ByteString -> Either String (a, ByteString)) -> ByteString -> Either String ([a], ByteString)
readItems count readItem = go count []
  where
    go 0 items rest = Right (reverse items, rest)
    go remaining items rest = do
        (item, final) <- readItem rest
        go (remaining - 1) (item : items) final

-- | Read a coordinate and check its finite or empty-point representation.
readCoordinate :: Bool -> Int -> Bool -> ByteString -> Either String (Text, ByteString)
readCoordinate little dimensions allowEmpty bytes = do
    (values, rest) <- readItems dimensions readDouble bytes
    if allowEmpty && all isNaN values
        then Right ("EMPTY", rest)
        else
            if any (\value -> isNaN value || isInfinite value) values
                then Left "Geometry WKB has a non-finite coordinate"
                else Right (Text.intercalate " " (map (Text.pack . show) values), rest)
  where
    readDouble input = do
        (word, rest) <- readWord little 8 input
        Right (castWord64ToDouble word, rest)

-- | Read a line or polygon ring with a checked coordinate count.
readLine :: Bool -> Int -> ByteString -> Either String (Text, ByteString)
readLine little dimensions bytes = do
    (count, rest) <- readCount little (8 * dimensions) bytes
    (points, final) <- readItems count (readCoordinate little dimensions False) rest
    Right (renderParts points, final)

-- | Render a sequence as an empty body or a parenthesized WKT body.
renderParts :: [Text] -> Text
renderParts [] = "EMPTY"
renderParts parts = "(" <> Text.intercalate ", " parts <> ")"
