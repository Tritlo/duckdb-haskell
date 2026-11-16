{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : Database.DuckDB.Simple.Variant
-- Description : DuckDB VARIANT values.
module Database.DuckDB.Simple.Variant (
  Variant (..),
  variantObject,
) where

import Data.Array (elems, listArray)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Element (containsType)
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), FromField (..), ResultError (..), returnError)
import Database.DuckDB.Simple.Geometry (RawGeometry (..))
import Database.DuckDB.Simple.LogicalRep (LogicalTypeId (..), LogicalTypeRep (..), StructField (..), StructValue (..), withoutAliases)

-- | A VARIANT value. The payload is a 'FieldValue' with the native type of the
-- stored value. Arrays are 'FieldList' values. Objects are 'FieldStruct' values
-- whose fields have the VARIANT type. Decoded object keys use DuckDB's native
-- lexicographic order. Array elements retain their order. SQL NULL is 'FieldNull'.
-- A t'Variant' parameter binds its payload as a VARIANT. A VARIANT does not keep
-- a UNION, a MAP, an ENUM, or the CRS of a GEOMETRY, so a parameter with such a
-- payload, at any depth, raises an error. Results preserve their native scalar
-- types and nested values through public V2 operations.
newtype Variant = Variant {variantPayload :: FieldValue}
  deriving (Eq, Show)

-- | Build the payload of a VARIANT object. Each field has the VARIANT type,
-- in the order of the entries.
variantObject :: [(Text, FieldValue)] -> FieldValue
variantObject entries =
  FieldStruct
    StructValue
      { structValueFields = listArray (0, length entries - 1) [StructField name value | (name, value) <- entries]
      , structValueTypes = listArray (0, length entries - 1) [StructField name (LogicalTypeScalar LogicalTypeIdVariant) | (name, _) <- entries]
      , structValueIndex = Map.fromList (zip (map fst entries) [0 ..])
      }

-- | Reads a value that a VARIANT can hold: the value of a VARIANT column, or a
-- value of another SQL type with that shape. The shape is a scalar that is not
-- an ENUM, a GEOMETRY with no CRS, a LIST of such values, or a STRUCT in the
-- form of a VARIANT object: its fields have the type VARIANT and hold such
-- values, and its keys are in ascending order. A value of another shape fails
-- with 'Incompatible', because the cast of DuckDB to VARIANT gives another
-- value. Write @c::VARIANT@ in SQL for such a column. A column whose type is or
-- contains JSON, at each depth, also fails with 'Incompatible': DuckDB casts
-- JSON to VARIANT by parsing the text, so @c::VARIANT@ gives the parsed value,
-- not the text. See Note [Reads of another SQL type] in
-- "Database.DuckDB.Simple.FromField".
instance FromField Variant where
  fromField field@Field{fieldName, fieldValue, fieldType} = do
    logical <- fieldType
    if containsType isJson logical
      then
        returnError
          Incompatible
          field
          (Text.concat [Text.pack "DuckDB casts JSON to VARIANT by parsing the text; write ", column, Text.pack "::VARIANT"])
      else
        if variantShape fieldValue
          then pure (Variant fieldValue)
          else returnError Incompatible field (Text.pack "a VARIANT does not hold a value of this shape; write c::VARIANT")
    where
      isJson (LogicalTypeAlias name _) = name == Text.pack "JSON"
      isJson _ = False
      column = if Text.null fieldName then Text.pack "c" else fieldName

-- | Whether a VARIANT can hold a decoded value. A VARIANT column decodes only to
-- these values: a scalar that is not an ENUM, a GEOMETRY with no CRS, a
-- 'FieldList' of such values, and a 'FieldStruct' in the form of a VARIANT
-- object. Such an object has fields of the type VARIANT that hold such values,
-- and its keys are in ascending order: DuckDB sorts the keys of an object by
-- their UTF-8 bytes. The order of 'Text' is the order of code points, and UTF-8
-- keeps that order in its bytes, so the two orders are the same. Each other
-- value has no VARIANT form: a 'FieldArray', a 'FieldTuple', another
-- 'FieldStruct', a 'FieldEnum', a 'FieldMap', a 'FieldUnion', and a GEOMETRY
-- with a CRS.
variantShape :: FieldValue -> Bool
variantShape = \case
  FieldEnum{} -> False
  FieldGeometry RawGeometry{rawGeometryCRS = Just _} -> False
  FieldMap{} -> False
  FieldUnion{} -> False
  FieldArray{} -> False
  FieldTuple{} -> False
  FieldList values -> all variantShape values
  FieldStruct StructValue{structValueFields, structValueTypes} ->
    let names = map structFieldName (elems structValueTypes)
    in  all ((== LogicalTypeScalar LogicalTypeIdVariant) . withoutAliases . structFieldValue) (elems structValueTypes)
          && and (zipWith (<) names (drop 1 names))
          && all (variantShape . structFieldValue) (elems structValueFields)
  _ -> True
