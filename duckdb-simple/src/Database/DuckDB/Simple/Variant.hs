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
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), FromField (..), ResultError (..), returnError)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeId (..), LogicalTypeRep (..), StructField (..), StructValue (..), TupleValue (..))
import Database.DuckDB.Simple.Ok (Ok (..))

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

-- | Reads VARIANT, and wraps the decoded value of each other SQL type. A value
-- that holds an ENUM, a MAP, or a UNION at any depth raises 'Incompatible',
-- because the cast of DuckDB to VARIANT gives another value: the label as a
-- VARCHAR, a list of key and value objects, or the member value. Write
-- @c::VARIANT@ in SQL for such a column. A VARIANT column does not decode to
-- these values. See Note [Reads of another SQL type] in
-- "Database.DuckDB.Simple.FromField".
instance FromField Variant where
  fromField field@Field{fieldValue}
    | changedByCast fieldValue =
        returnError Incompatible field (Text.pack "the cast to VARIANT changes an ENUM, a MAP, or a UNION; write c::VARIANT")
    | otherwise = Ok (Variant fieldValue)

-- | Whether a decoded value holds an ENUM, a MAP, or a UNION at any depth.
changedByCast :: FieldValue -> Bool
changedByCast = \case
  FieldEnum{} -> True
  FieldMap{} -> True
  FieldUnion{} -> True
  FieldList values -> any changedByCast values
  FieldArray values -> any changedByCast (elems values)
  FieldStruct StructValue{structValueFields} -> any (changedByCast . structFieldValue) (elems structValueFields)
  FieldTuple TupleValue{tupleValueFields} -> any changedByCast (elems tupleValueFields)
  _ -> False
