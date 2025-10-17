{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Database.DuckDB.Simple.Generic
Description : Generic helpers for encoding Haskell ADTs as DuckDB structs/unions.

This module provides the glue needed to reuse the existing 'ToField'/'FromField'
machinery with algebraic data types via GHC generics.  The supported mapping is
currently intentionally conservative:

* /Product types/ (records or tuples) whose fields already satisfy 'DuckValue'
  are encoded as STRUCT values.  Record fields retain their selector name;
  positional products fall back to @field1@, @field2@, …
* /Sum types/ (:+:) become UNION values.  Each constructor becomes a union
  member; payloads are encoded as structs (or @NULL@ for nullary constructors).

Recursive types are supported as long as every payload is itself encodable
through 'DuckValue'.  Note that sum types must have constructor fields that are
structural products (i.e. we do not yet expose mixed union/record nesting for
non-record constructors).

Typical usage looks like:

> data User = User { userId :: Int64, userName :: Text }
>   deriving stock (Generic)
>
> instance DuckValue User
>
> toField (genericToFieldValue user) -- Struct {"userId" := ..., ...}

For sum types:

> data Shape
>   = Circle Double
>   | Rectangle Double Double
>   | Origin
>   deriving stock (Generic)
>
> instance DuckValue Shape

Constructors are turned into a union with members @Circle{radius}@,
@Rectangle{width,height}@, and @Origin@ (null payload).

You can also lean on @DerivingVia@ using the exported 'ViaDuckDB' newtype:

> data User = User { userId :: Int64, userName :: Text }
>   deriving stock (Generic)
>   deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB User)

The derived instances automatically encode/decode via STRUCT/UNION representations.

=== Extending this module

The rest of this file is organised so that each building block is reusable:

* 'DuckValue' covers leaf-level conversions between Haskell values, DuckDB
  'FieldValue's, and logical type metadata.
* 'GToField' and friends walk the generic representation to assemble structs or
  unions and carry around the logical type information we later need when
  binding parameters.
* 'ViaDuckDB' wires everything together for deriving via.

When adding new features, mimic the structure used here (and document new
classes the way the existing ones are documented) so other backends can take
inspiration from this implementation.
-}
module Database.DuckDB.Simple.Generic (
    -- * Field-level primitives
    DuckValue (..),

    -- * Generic encoding/decoding for ADTs
    genericToFieldValue,
    genericFromFieldValue,
    genericLogicalType,
    genericToStructValue,
    genericToUnionValue,

    -- * DerivingVia helper
    ViaDuckDB (..),
) where

import Control.Exception (displayException)
import Control.Monad (unless)
import Data.Array (Array, elems, listArray)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Time (UTCTime)
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Data.ByteString as BS
import qualified Data.UUID as UUID
import GHC.Generics
import Numeric.Natural (Natural)
import Data.Typeable (Typeable)

import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (
    Field (..),
    FieldValue (..),
    FromField (..),
    IntervalValue (..),
    ResultError (..),
    TimeWithZone (..),
    returnError,
 )
import Database.DuckDB.Simple.LogicalRep (
    LogicalTypeRep (..),
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..),
 )
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), ToField (..))

--------------------------------------------------------------------------------
-- DuckValue: bridge between Haskell scalars and FieldValue/LogicalTypeRep

-- | Types that can appear inside generated structs/unions.
--
-- A 'DuckValue' instance must provide:
--
-- * encoding to 'FieldValue'
-- * logical type metadata ('duckLogicalType')
-- * decoding from 'FieldValue'
--
-- The primitive instances below are the canonical source for how scalar types
-- should be represented; both the generic implementation and the manual
-- 'ToField'/'FromField' instances rely on them.
class DuckValue a where
    duckToField :: a -> FieldValue
    duckFromField :: FieldValue -> Either String a
    duckLogicalType :: Proxy a -> LogicalTypeRep

    default duckFromField :: (FromField a, Show a) => FieldValue -> Either String a
    duckFromField fv =
        case fromField Field{fieldName = Text.empty, fieldIndex = 0, fieldValue = fv} of
            Ok x -> Right x
            Errors errs -> Left (unlines (map displayException errs))

instance DuckValue Bool where
    duckToField = FieldBool
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeBoolean

instance DuckValue Int where
    duckToField = FieldInt64 . fromIntegral
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeBigInt

instance DuckValue Int8 where
    duckToField = FieldInt8
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeTinyInt

instance DuckValue Int16 where
    duckToField = FieldInt16
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeSmallInt

instance DuckValue Int32 where
    duckToField = FieldInt32
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeInteger

instance DuckValue Int64 where
    duckToField = FieldInt64
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeBigInt

instance DuckValue Integer where
    duckToField = FieldHugeInt
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeHugeInt

instance DuckValue Natural where
    duckToField = FieldUHugeInt . toInteger
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeUHugeInt

instance DuckValue Word where
    duckToField = FieldWord64 . fromIntegral
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeUBigInt

instance DuckValue Word8 where
    duckToField = FieldWord8
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeUTinyInt

instance DuckValue Word16 where
    duckToField = FieldWord16
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeUSmallInt

instance DuckValue Word32 where
    duckToField = FieldWord32
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeUInteger

instance DuckValue Word64 where
    duckToField = FieldWord64
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeUBigInt

instance DuckValue Float where
    duckToField = FieldFloat
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeFloat

instance DuckValue Double where
    duckToField = FieldDouble
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeDouble

instance DuckValue Text where
    duckToField = FieldText
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeVarchar

instance DuckValue String where
    duckToField = FieldText . Text.pack
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeVarchar
    duckFromField fv = Text.unpack <$> duckFromField fv

instance DuckValue BS.ByteString where
    duckToField = FieldBlob
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeBlob

instance DuckValue Day where
    duckToField = FieldDate
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeDate

instance DuckValue TimeOfDay where
    duckToField = FieldTime
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeTime

instance DuckValue LocalTime where
    duckToField = FieldTimestamp
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeTimestamp

instance DuckValue UTCTime where
    duckToField = FieldTimestampTZ
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeTimestampTz

instance DuckValue UUID.UUID where
    duckToField = FieldUUID
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeUUID

instance DuckValue IntervalValue where
    duckToField = FieldInterval
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeInterval

instance DuckValue TimeWithZone where
    duckToField = FieldTimeTZ
    duckLogicalType _ = LogicalTypeScalar DuckDBTypeTimeTz

instance (DuckValue a) => DuckValue (Maybe a) where
    duckToField (Just x) = duckToField x
    duckToField Nothing = FieldNull
    duckLogicalType _ = duckLogicalType (Proxy :: Proxy a)
    duckFromField FieldNull = Right Nothing
    duckFromField other = Just <$> duckFromField other

--------------------------------------------------------------------------------
-- Generic machinery

-- | Internal representation used while traversing the generic structure.  We
-- keep both the encoded value and its logical type so we can re-use the same
-- traversal when generating metadata ('genericLogicalType') and when producing
-- concrete values ('genericToFieldValue').
data Encoded
    = EncodedStruct (StructValue FieldValue) (Array Int (StructField LogicalTypeRep))
    | EncodedUnion (UnionValue FieldValue)
    | EncodedNull

encodedValue :: Encoded -> FieldValue
encodedValue = \case
    EncodedStruct sv _ -> FieldStruct sv
    EncodedUnion uv -> FieldUnion uv
    EncodedNull -> FieldNull

-- | Convert a Haskell value (using its generic representation) into a DuckDB 'FieldValue'.
genericToFieldValue :: forall a. (Generic a, GToField (Rep a)) => a -> FieldValue
genericToFieldValue = encodedValue . gToField . from

-- | Extract the logical DuckDB type corresponding to a Haskell value.
genericLogicalType :: forall a. (Generic a, GToField (Rep a)) => Proxy a -> LogicalTypeRep
genericLogicalType _ = gLogicalType (Proxy :: Proxy (Rep a ()))

-- | Decode a DuckDB 'FieldValue' back into a Haskell value using its generic representation.
genericFromFieldValue :: forall a. (Generic a, GFromField (Rep a)) => FieldValue -> Either String a
genericFromFieldValue fv = to <$> gFromField fv

-- | Convenience helpers that project out structured values directly.
genericToStructValue :: forall a. (Generic a, GToField (Rep a)) => a -> Maybe (StructValue FieldValue)
genericToStructValue value =
    case gToField (from value) of
        EncodedStruct sv _ -> Just sv
        EncodedNull -> Just emptyStruct
        _ -> Nothing
  where
    emptyStruct =
        StructValue
            { structValueFields = listArray (0, -1) []
            , structValueTypes = listArray (0, -1) []
            , structValueIndex = Map.empty
            }

genericToUnionValue :: forall a. (Generic a, GToField (Rep a)) => a -> Maybe (UnionValue FieldValue)
genericToUnionValue value =
    case gToField (from value) of
        EncodedUnion uv -> Just uv
        _ -> Nothing

-- | Wrapper for deriving-via so that @instance ToField (ViaDuckDB a)@ picks up
-- the generic encoding provided by this module.
newtype ViaDuckDB a = ViaDuckDB { getViaDuckDB :: a }

-- Type family to decide whether a representation is a sum.
-- | Type family evaluating to 'True for sum-of-constructors generic
-- representations.  We use this to select the appropriate encoding strategy.
type family IsSum f :: Bool where
    IsSum (f :+: g) = 'True
    IsSum (M1 D _ f) = IsSum f
    IsSum (M1 C _ f) = IsSum f
    IsSum _ = 'False

-- | Generic encoding to the intermediate 'Encoded' representation.  Every
-- instance must also supply the corresponding logical type description.
class GToField f where
    gToField :: f p -> Encoded
    gLogicalType :: Proxy (f p) -> LogicalTypeRep

instance (GToField' (IsSum f) f) => GToField f where
    gToField = gToField' (Proxy :: Proxy (IsSum f))
    gLogicalType _ = gLogicalType' (Proxy :: Proxy (IsSum f)) (Proxy :: Proxy f)

-- | Helper class that splits the product and sum handling using the 'IsSum'
-- type family.  We specialise on products ('False) and sums ('True) to keep
-- the core logic small and easy to reason about.
class GToField' (isSum :: Bool) f where
    gToField' :: Proxy isSum -> f p -> Encoded
    gLogicalType' :: Proxy isSum -> Proxy f -> LogicalTypeRep

-- Products (single constructor records)
-- | Product encoding: single-constructor datatypes become STRUCT values.
instance (GStruct f) => GToField' 'False (M1 D meta (M1 C c f)) where
    gToField' _ (M1 (M1 inner)) =
        let comps = gStructValues inner
            typeComps = gStructTypes (Proxy :: Proxy (f p))
         in case comps of
                [] -> EncodedNull
                _ ->
                    let names = resolveNames (zip progIndices (map fcName comps))
                        valueArray = listArrayFrom names (map fcValue comps)
                        typeArray = listArrayFrom names (map fcValue typeComps)
                        indexMap = Map.fromList (zip names [0 ..])
                     in EncodedStruct
                            StructValue
                                { structValueFields = valueArray
                                , structValueTypes = typeArray
                                , structValueIndex = indexMap
                                }
                            typeArray
      where
        progIndices = [0 :: Int ..]
    gLogicalType' _ _ =
        let typeComps = gStructTypes (Proxy :: Proxy (f p))
            names = resolveNames (zip [0 :: Int ..] (map fcName typeComps))
            typeArray = listArrayFrom names (map fcValue typeComps)
         in LogicalTypeStruct typeArray

-- Sums (encode as union)
-- | Sum encoding: multi-constructor datatypes become UNION values.
instance (GSum f) => GToField' 'True (M1 D meta f) where
    gToField' _ (M1 value) =
        let members = gSumMembers (Proxy :: Proxy (f p))
            membersArray =
                case members of
                    [] -> listArray (0, -1) []
                    _ -> listArray (0, length members - 1) members
            (idx, payload) = gSumEncode value
            label = unionMemberName (members !! idx)
         in EncodedUnion
                UnionValue
                    { unionValueIndex = fromIntegral idx
                    , unionValueLabel = label
                    , unionValuePayload = payload
                    , unionValueMembers = membersArray
                    }
    gLogicalType' _ _ =
        let members = gSumMembers (Proxy :: Proxy (f p))
            membersArray =
                case members of
                    [] -> listArray (0, -1) []
                    _ -> listArray (0, length members - 1) members
         in LogicalTypeUnion membersArray

--------------------------------------------------------------------------------
-- GStruct: products

data FieldComponent a = FieldComponent
    { fcName :: Maybe Text
    , fcValue :: a
    }

resolveNames :: [(Int, Maybe Text)] -> [Text]
-- | Assign canonical names to struct fields.  We preserve any selector names
-- provided by GHC.Generics and fall back to @fieldN@ for positional products.
resolveNames =
    map pick
  where
    pick (_, Just n) = n
    pick (idx, Nothing) = Text.pack ("field" <> show (idx + 1))

-- | Helper that builds an 'Array' of struct fields from parallel lists of names
-- and payloads.
listArrayFrom :: [Text] -> [b] -> Array Int (StructField b)
listArrayFrom names values =
    case values of
        [] -> listArray (0, -1) []
        _ ->
            listArray
                (0, length values - 1)
                (zipWith (\n v -> StructField{structFieldName = n, structFieldValue = v}) names values)

-- | Collect the components (values and types) of a product.  Implementations
-- produce parallel lists so we can zip them during encoding and decoding.
class GStruct f where
    gStructValues :: f p -> [FieldComponent FieldValue]
    gStructTypes :: Proxy (f p) -> [FieldComponent LogicalTypeRep]

instance GStruct U1 where
    gStructValues _ = []
    gStructTypes _ = []

instance (GStruct a, GStruct b) => GStruct (a :*: b) where
    gStructValues (a :*: b) = gStructValues a ++ gStructValues b
    gStructTypes _ = gStructTypes (Proxy :: Proxy (a p)) ++ gStructTypes (Proxy :: Proxy (b p))

instance (Selector s, DuckValue a) => GStruct (M1 S s (K1 i a)) where
    gStructValues m@(M1 (K1 x)) =
        let name = toMaybe (selName m)
         in [FieldComponent name (duckToField x)]
    gStructTypes _ =
        let raw = selName (undefined :: M1 S s (K1 i a) ())
            name = toMaybe raw
         in [FieldComponent name (duckLogicalType (Proxy :: Proxy a))]

instance (GStruct f) => GStruct (M1 C c f) where
    gStructValues (M1 x) = gStructValues x
    gStructTypes _ = gStructTypes (Proxy :: Proxy (f p))

toMaybe :: String -> Maybe Text
toMaybe name
    | null name = Nothing
    | otherwise = Just (Text.pack name)

--------------------------------------------------------------------------------
-- Sums (unions)

-- | Sum type encoding.  We gather the metadata ('gSumMembers'), convert a value
-- to its discriminant and payload ('gSumEncode'), and provide the inverse
-- ('gSumDecode').
class GSum f where
    gSumMembers :: Proxy (f p) -> [UnionMemberType]
    gSumEncode :: f p -> (Int, FieldValue)
    gSumDecode :: Int -> FieldValue -> Either String (f p)

instance (GSum a, GSum b) => GSum (a :+: b) where
    gSumMembers _ = gSumMembers (Proxy :: Proxy (a p)) ++ gSumMembers (Proxy :: Proxy (b p))
    gSumEncode (L1 x) = gSumEncode x
    gSumEncode (R1 x) =
        let leftCount = length (gSumMembers (Proxy :: Proxy (a p)))
            (idx, payload) = gSumEncode x
         in (idx + leftCount, payload)
    gSumDecode idx payload =
        let leftCount = length (gSumMembers (Proxy :: Proxy (a p)))
         in if idx < leftCount
                then L1 <$> gSumDecode idx payload
                else R1 <$> gSumDecode (idx - leftCount) payload

instance (Constructor c, GStruct f, GStructDecode f) => GSum (M1 C c f) where
    gSumMembers _ =
        [ UnionMemberType
            { unionMemberName = Text.pack (conName (undefined :: M1 C c f p))
            , unionMemberType =
                let typeComps = gStructTypes (Proxy :: Proxy (f p))
                    names = resolveNames (zip [0 :: Int ..] (map fcName typeComps))
                 in LogicalTypeStruct (listArrayFrom names (map fcValue typeComps))
            }
        ]
    gSumEncode (M1 x) =
        case gStructValues x of
            [] -> (0, FieldNull)
            comps ->
                let typeComps = gStructTypes (Proxy :: Proxy (f p))
                    names = resolveNames (zip [0 :: Int ..] (map fcName comps))
                    valueArray = listArrayFrom names (map fcValue comps)
                    typeArray = listArrayFrom names (map fcValue typeComps)
                    indexMap = Map.fromList (zip names [0 ..])
                 in (0, FieldStruct StructValue{structValueFields = valueArray, structValueTypes = typeArray, structValueIndex = indexMap})
    gSumDecode idx payload
        | idx /= 0 = Left "duckdb-simple: union tag mismatch"
        | otherwise =
            case payload of
                FieldNull -> pure (M1 (gStructNull (Proxy :: Proxy (f p))))
                FieldStruct structVal -> M1 <$> gStructDecodeStruct (Proxy :: Proxy (f p)) structVal
                other -> Left ("duckdb-simple: expected STRUCT payload, got " <> show other)

--------------------------------------------------------------------------------
-- GStructDecode: inverse of GStruct for decoding

-- | Inverse of 'GStruct': decode struct payloads back into a generic product.
class GStructDecode f where
    gStructDecodeStruct :: Proxy (f p) -> StructValue FieldValue -> Either String (f p)
    gStructNull :: Proxy (f p) -> f p
    -- | Consume a prefix of fields from left to right while decoding, returning
    -- the reconstructed value and any remaining fields.
    gStructDecodeList :: Proxy (f p) -> [FieldValue] -> Either String (f p, [FieldValue])

instance GStructDecode U1 where
    gStructDecodeStruct _ structVal =
        if null (elems (structValueFields structVal))
            then Right U1
            else Left "duckdb-simple: expected empty struct"
    gStructNull _ = U1
    gStructDecodeList _ xs = Right (U1, xs)

instance (GStructDecode a, GStructDecode b) => GStructDecode (a :*: b) where
    gStructDecodeStruct _ structVal = do
        let values = map structFieldValue (elems (structValueFields structVal))
        (leftVal, rest) <- gStructDecodeList (Proxy :: Proxy (a p)) values
        (rightVal, rest') <- gStructDecodeList (Proxy :: Proxy (b p)) rest
        unless (null rest') $
            Left "duckdb-simple: extra fields when decoding struct"
        pure (leftVal :*: rightVal)
    gStructNull _ = gStructNull (Proxy :: Proxy (a p)) :*: gStructNull (Proxy :: Proxy (b p))
    gStructDecodeList _ xs = do
        (leftVal, rest) <- gStructDecodeList (Proxy :: Proxy (a p)) xs
        (rightVal, rest') <- gStructDecodeList (Proxy :: Proxy (b p)) rest
        pure (leftVal :*: rightVal, rest')

instance (Selector s, DuckValue a) => GStructDecode (M1 S s (K1 i a)) where
    gStructDecodeStruct _ structVal =
        case map structFieldValue (elems (structValueFields structVal)) of
            [fv] -> M1 . K1 <$> duckFromField fv
            [] -> Left "duckdb-simple: missing struct field"
            _ -> Left "duckdb-simple: expected single field struct"
    gStructNull _ = error "duckdb-simple: cannot derive null struct for selector"
    gStructDecodeList _ [] = Left "duckdb-simple: missing struct field"
    gStructDecodeList _ (fv : rest) = do
        val <- duckFromField fv
        pure (M1 (K1 val), rest)

instance (GStructDecode f) => GStructDecode (M1 C c f) where
    gStructDecodeStruct _ structVal = M1 <$> gStructDecodeStruct (Proxy :: Proxy (f p)) structVal
    gStructNull _ = M1 (gStructNull (Proxy :: Proxy (f p)))
    gStructDecodeList _ values = do
        (inner, rest) <- gStructDecodeList (Proxy :: Proxy (f p)) values
        pure (M1 inner, rest)

--------------------------------------------------------------------------------
-- GFromField (inverse generic)

-- | Generic decoding entry point mirroring 'GToField'.  This is used both by
-- @genericFromFieldValue@ and the 'Generically' deriving helper.
class GFromField f where
    gFromField :: FieldValue -> Either String (f p)

instance (GFromField' (IsSum f) f) => GFromField f where
    gFromField = gFromField' (Proxy :: Proxy (IsSum f))

class GFromField' (isSum :: Bool) f where
    gFromField' :: Proxy isSum -> FieldValue -> Either String (f p)

instance (GStruct f, GStructDecode f) => GFromField' 'False (M1 D meta (M1 C c f)) where
    gFromField' _ = \case
        FieldNull -> pure (M1 (M1 (gStructNull (Proxy :: Proxy (f p)))))
        FieldStruct sv -> M1 . M1 <$> gStructDecodeStruct (Proxy :: Proxy (f p)) sv
        other -> Left ("duckdb-simple: expected STRUCT value, got " <> show other)

instance (GSum f) => GFromField' 'True (M1 D meta f) where
    gFromField' _ = \case
        FieldUnion uv -> M1 <$> gSumDecode (fromIntegral (unionValueIndex uv)) (unionValuePayload uv)
        other -> Left ("duckdb-simple: expected UNION value, got " <> show other)

instance GFromField' 'False (M1 D meta U1) where
    gFromField' _ _ = Right (M1 U1)

--------------------------------------------------------------------------------
-- ViaDuckDB instances

-- | The deriving-via version of 'DuckDBColumnType'.  We look at the generic
-- logical type and map it back to a textual name.  The textual names are only
-- used for diagnostics (errors and column metadata).
instance (Generic a, GToField (Rep a)) => DuckDBColumnType (ViaDuckDB a) where
    duckdbColumnTypeFor _ =
        case genericLogicalType (Proxy :: Proxy a) of
            LogicalTypeStruct{} -> Text.pack "STRUCT"
            LogicalTypeUnion{} -> Text.pack "UNION"
            LogicalTypeList{} -> Text.pack "LIST"
            LogicalTypeArray{} -> Text.pack "ARRAY"
            LogicalTypeMap{} -> Text.pack "MAP"
            LogicalTypeScalar dtype -> duckdbTypeToName dtype
            LogicalTypeDecimal{} -> Text.pack "DECIMAL"
            LogicalTypeEnum{} -> Text.pack "ENUM"

-- | Deriving-via 'ToField' instance.  We reuse the helpers above to decide
-- whether the top-level representation is a union, struct, or scalar and then
-- delegate to the existing 'ToField' instances for those composite types.
instance (Generic a, GToField (Rep a)) => ToField (ViaDuckDB a) where
    toField (ViaDuckDB x) =
        case genericToUnionValue x of
            Just unionVal -> toField unionVal
            Nothing ->
                case genericToStructValue x of
                    Just structVal -> toField structVal
                    Nothing ->
                        case genericToFieldValue x of
                            FieldUnion uv -> toField uv
                            FieldStruct sv -> toField sv
                            FieldNull -> toField (Nothing :: Maybe Int)
                            other -> error ("duckdb-simple: unsupported generic encoding " <> show other)

-- | Deriving-via 'FromField' instance.  Errors are rewrapped using the existing
-- 'returnError' helper so callers receive a proper 'ResultError'.
instance (Generic a, GFromField (Rep a), Typeable a) => FromField (ViaDuckDB a) where
    fromField f@Field{fieldValue} =
        case genericFromFieldValue fieldValue of
            Right value -> pure (ViaDuckDB value)
            Left err ->
                returnError ConversionFailed f (Text.pack err)

duckdbTypeToName :: DuckDBType -> Text
-- | Translate a 'DuckDBType' into a textual label for diagnostics and
-- documentation.  This mirrors the naming used in "Database.DuckDB.Simple.ToField".
duckdbTypeToName dtype
    | dtype == DuckDBTypeBoolean = Text.pack "BOOLEAN"
    | dtype == DuckDBTypeTinyInt = Text.pack "TINYINT"
    | dtype == DuckDBTypeSmallInt = Text.pack "SMALLINT"
    | dtype == DuckDBTypeInteger = Text.pack "INTEGER"
    | dtype == DuckDBTypeBigInt = Text.pack "BIGINT"
    | dtype == DuckDBTypeUTinyInt = Text.pack "UTINYINT"
    | dtype == DuckDBTypeUSmallInt = Text.pack "USMALLINT"
    | dtype == DuckDBTypeUInteger = Text.pack "UINTEGER"
    | dtype == DuckDBTypeUBigInt = Text.pack "UBIGINT"
    | dtype == DuckDBTypeFloat = Text.pack "FLOAT"
    | dtype == DuckDBTypeDouble = Text.pack "DOUBLE"
    | dtype == DuckDBTypeVarchar = Text.pack "VARCHAR"
    | dtype == DuckDBTypeBlob = Text.pack "BLOB"
    | dtype == DuckDBTypeDate = Text.pack "DATE"
    | dtype == DuckDBTypeTime = Text.pack "TIME"
    | dtype == DuckDBTypeTimestamp = Text.pack "TIMESTAMP"
    | dtype == DuckDBTypeTimestampTz = Text.pack "TIMESTAMP_TZ"
    | dtype == DuckDBTypeUUID = Text.pack "UUID"
    | dtype == DuckDBTypeInterval = Text.pack "INTERVAL"
    | dtype == DuckDBTypeHugeInt = Text.pack "HUGEINT"
    | dtype == DuckDBTypeUHugeInt = Text.pack "UHUGEINT"
    | dtype == DuckDBTypeBigNum = Text.pack "BIGNUM"
    | dtype == DuckDBTypeTimeTz = Text.pack "TIME_TZ"
    | otherwise = Text.pack (show dtype)

--------------------------------------------------------------------------------
-- DuckDB type constructors (re-exported patterns)

-- These pattern synonyms come from duckdb-ffi; re-exporting to avoid users having to import it.
