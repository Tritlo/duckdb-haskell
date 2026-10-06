-- | Raw geometry values and conversion to 'G.Geometry' from @geometry-simple@.
module Database.DuckDB.Simple.Geometry (
    RawGeometry (..),
    fromRawGeometry,
    toRawGeometry,
) where

import Data.ByteString (ByteString)
import qualified Data.Geometry as G
import Data.Geometry.WKB (decodeWKB, encodeWKB)
import Data.Text (Text)

{- | ISO WKB bytes with optional CRS metadata.
Reading this type copies the bytes without decoding the coordinates.
Binding checks the bytes. Empty or NUL-containing CRS strings are rejected.
Derived equality compares the bytes and CRS, not spatial equivalence.
-}
data RawGeometry = RawGeometry
    { rawGeometryWKB :: !ByteString
    , rawGeometryCRS :: !(Maybe Text)
    }
    deriving (Eq, Show, Read)

{- | Decode owned WKB with the standalone codec. The shape does not store CRS
metadata. Empty multi-geometries and collections have no layout tag in the
decoded representation. Keep the raw value when these details are needed.
-}
fromRawGeometry :: RawGeometry -> Either String G.Geometry
fromRawGeometry = decodeWKB . rawGeometryWKB

{- | Encode a shape as ISO WKB with no CRS. Set 'rawGeometryCRS' on the result
to supply a CRS label. This does not transform the coordinates.
-}
toRawGeometry :: G.Geometry -> Either String RawGeometry
toRawGeometry shape = (`RawGeometry` Nothing) <$> encodeWKB shape
