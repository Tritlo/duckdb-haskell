{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

-- | DuckDB VARIANT values.
module Database.DuckDB.Simple.Variant (
    Variant (..),
    variantObject,
) where

import Data.Array (listArray)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Database.DuckDB.FFI (pattern DUCKDB_TYPE_VARIANT)
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), FromField (..))
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), StructValue (..))
import Database.DuckDB.Simple.Ok (Ok (..))

{- | A VARIANT value. The payload is a 'FieldValue' with the native type of the
stored value. Arrays are 'FieldList' values. Objects are 'FieldStruct' values
whose fields have the VARIANT type. SQL NULL is 'FieldNull'.
A t'Variant' parameter binds its payload as a VARIANT.
-}
newtype Variant = Variant {variantPayload :: FieldValue}
    deriving (Eq, Show)

{- | Build the payload of a VARIANT object. Each field has the VARIANT type,
in the order of the entries.
-}
variantObject :: [(Text, FieldValue)] -> FieldValue
variantObject entries =
    FieldStruct
        StructValue
            { structValueFields = listArray (0, length entries - 1) [StructField name value | (name, value) <- entries]
            , structValueTypes = listArray (0, length entries - 1) [StructField name (LogicalTypeScalar DUCKDB_TYPE_VARIANT) | (name, _) <- entries]
            , structValueIndex = Map.fromList (zip (map fst entries) [0 ..])
            }

-- | Read any column. A VARIANT column gives its decoded payload.
instance FromField Variant where
    fromField Field{fieldValue} = Ok (Variant fieldValue)
