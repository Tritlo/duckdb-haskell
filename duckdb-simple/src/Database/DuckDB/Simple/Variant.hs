{-# LANGUAGE NamedFieldPuns #-}

-- | DuckDB VARIANT values.
module Database.DuckDB.Simple.Variant (Variant (..)) where

import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), FromField (..))
import Database.DuckDB.Simple.Ok (Ok (..))

{- | A VARIANT value. The payload is a 'FieldValue' with the native type of the
stored value. Arrays are 'FieldList' values. Objects are 'FieldStruct' values
whose fields have the VARIANT type. SQL NULL is 'FieldNull'.
This type has no parameter instance. Cast a plain value with @?::VARIANT@.
-}
newtype Variant = Variant {variantPayload :: FieldValue}
    deriving (Eq, Show)

-- | Read any column. A VARIANT column gives its decoded payload.
instance FromField Variant where
    fromField Field{fieldValue} = Ok (Variant fieldValue)
