{-# LANGUAGE StrictData #-}

-- |
-- Module      : Database.DuckDB.Simple.Geometry
-- Description : Raw geometry values and conversion to 'G.Geometry' from @geometry-simple@.
module Database.DuckDB.Simple.Geometry (
  RawGeometry (..),
  fromRawGeometry,
  toRawGeometry,
) where

import Data.ByteString (ByteString)
import qualified Data.Geometry as G
import Data.Geometry.WKB (decodeWKB, encodeWKB)
import Data.Text (Text)

-- | ISO WKB bytes with an optional CRS definition.
-- The definition can be an identifier, a custom name, or WKT2\/PROJJSON text.
-- Reading this type copies the bytes without decoding the coordinates.
-- A parameter of this type binds as a GEOMETRY value with the CRS
-- 'rawGeometryCRS'. DuckDB then casts it to the type of the parameter. A value
-- without a CRS takes the CRS of the target. A target without a CRS removes the
-- CRS of the value. A target with another CRS raises an error. Use
-- @Maybe RawGeometry@ for SQL NULL. A @SizedArray@, a list, or a typed row cannot
-- hold this type. A STRUCT, TUPLE, or UNION parameter binds a @FieldGeometry@
-- value with its CRS. A VARIANT parameter binds one only when it has no CRS. See
-- Note [Raw GEOMETRY parameters] in "Database.DuckDB.Simple.ToField".
-- DuckDB can normalize the byte order and CRS definition during import.
-- The pinned native API requires the same Z/M dimensions for all collection
-- members, including empty members. Native import rejects mixed member layouts.
-- The standalone WKB codec preserves these layouts.
-- Derived equality compares the bytes and CRS, not spatial equivalence.
data RawGeometry = RawGeometry
  { rawGeometryWKB :: ByteString
  , rawGeometryCRS :: Maybe Text
  }
  deriving (Eq, Show, Read)

-- | Decode owned WKB with the standalone codec. The shape does not store CRS
-- metadata. Empty multi-geometries and collections have no layout tag in the
-- decoded representation. Keep the raw value when these details are needed.
fromRawGeometry :: RawGeometry -> Either String G.Geometry
fromRawGeometry = decodeWKB . rawGeometryWKB

-- | Encode a shape as ISO WKB with no CRS. Set 'rawGeometryCRS' on the result
-- to give the value a CRS definition. This does not transform
-- the coordinates. The standalone codec can encode mixed collection layouts
-- that the pinned native API rejects during import.
toRawGeometry :: G.Geometry -> Either String RawGeometry
toRawGeometry shape = (`RawGeometry` Nothing) <$> encodeWKB shape
