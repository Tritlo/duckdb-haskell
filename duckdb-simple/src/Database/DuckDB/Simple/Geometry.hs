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

{- | ISO WKB bytes with an optional CRS definition.
The definition can be an identifier, a custom name, or WKT2\/PROJJSON text.
Reading this type copies the bytes without decoding the coordinates.
This type has no @ToField@ instance. Bind 'rawGeometryWKB' with
@ST_GeomFromWKB(?)@. Apply 'rawGeometryCRS' with @ST_SetCRS(..., ?)@.
Omit @ST_SetCRS@ when the CRS is 'Nothing'. SQL NULL propagates to the geometry.
DuckDB can normalize the byte order and CRS definition during import.
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
to supply a CRS definition for explicit SQL import. This does not transform
the coordinates.
-}
toRawGeometry :: G.Geometry -> Either String RawGeometry
toRawGeometry shape = (`RawGeometry` Nothing) <$> encodeWKB shape
