{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Database.DuckDB.Simple.Generic
Description : Generic helpers for encoding Haskell ADTs as DuckDB structs/unions.

This module provides GHC generics based building blocks that translate product
types (records) into DuckDB STRUCT values and sum types into DuckDB UNION values.
It is intentionally conservative: it currently supports single-constructor
records whose fields already have 'DuckValue' instances, as well as simple
enumerations (nullary constructors) and constructor variants whose payloads are
again DuckDB encodable.
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

import Database.DuckDB.FFI (DuckDBType (..))
import Database.DuckDB.Simple.FromField (
    Field (..),
    FieldValue (..),
    FromField (..),
    IntervalValue (..),
    TimeWithZone (..),
 )
import Database.DuckDB.Simple.LogicalRep (
    LogicalTypeRep (..),
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..),
 )
import Database.DuckDB.Simple.Ok (Ok (..))

--------------------------------------------------------------------------------
-- DuckValue: bridge between Haskell scalars and FieldValue/LogicalTypeRep

-- | Types that can appear inside generated structs/unions.
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
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 1)

instance DuckValue Int where
    duckToField = FieldInt64 . fromIntegral
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 5)

instance DuckValue Int8 where
    duckToField = FieldInt8
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 2)

instance DuckValue Int16 where
    duckToField = FieldInt16
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 3)

instance DuckValue Int32 where
    duckToField = FieldInt32
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 4)

instance DuckValue Int64 where
    duckToField = FieldInt64
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 5)

instance DuckValue Integer where
    duckToField = FieldHugeInt
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 24)

instance DuckValue Natural where
    duckToField = FieldUHugeInt . toInteger
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 25)

instance DuckValue Word where
    duckToField = FieldWord64 . fromIntegral
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 9)

instance DuckValue Word8 where
    duckToField = FieldWord8
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 6)

instance DuckValue Word16 where
    duckToField = FieldWord16
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 7)

instance DuckValue Word32 where
    duckToField = FieldWord32
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 8)

instance DuckValue Word64 where
    duckToField = FieldWord64
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 9)

instance DuckValue Float where
    duckToField = FieldFloat
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 10)

instance DuckValue Double where
    duckToField = FieldDouble
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 11)

instance DuckValue Text where
    duckToField = FieldText
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 12)

instance DuckValue String where
    duckToField = FieldText . Text.pack
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 12)
    duckFromField fv = Text.unpack <$> duckFromField fv

instance DuckValue BS.ByteString where
    duckToField = FieldBlob
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 16)

instance DuckValue Day where
    duckToField = FieldDate
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 18)

instance DuckValue TimeOfDay where
    duckToField = FieldTime
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 19)

instance DuckValue LocalTime where
    duckToField = FieldTimestamp
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 21)

instance DuckValue UTCTime where
    duckToField = FieldTimestampTZ
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 32)

instance DuckValue UUID.UUID where
    duckToField = FieldUUID
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 28)

instance DuckValue IntervalValue where
    duckToField = FieldInterval
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 26)

instance DuckValue TimeWithZone where
    duckToField = FieldTimeTZ
    duckLogicalType _ = LogicalTypeScalar (DuckDBType 31)

instance (DuckValue a) => DuckValue (Maybe a) where
    duckToField (Just x) = duckToField x
    duckToField Nothing = FieldNull
    duckLogicalType _ = duckLogicalType (Proxy :: Proxy a)
    duckFromField FieldNull = Right Nothing
    duckFromField other = Just <$> duckFromField other

--------------------------------------------------------------------------------
-- Generic machinery

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

-- Type family to decide whether a representation is a sum.
type family IsSum f :: Bool where
    IsSum (f :+: g) = 'True
    IsSum (M1 D _ f) = IsSum f
    IsSum (M1 C _ f) = IsSum f
    IsSum _ = 'False

class GToField f where
    gToField :: f p -> Encoded
    gLogicalType :: Proxy (f p) -> LogicalTypeRep

instance (GToField' (IsSum f) f) => GToField f where
    gToField = gToField' (Proxy :: Proxy (IsSum f))
    gLogicalType _ = gLogicalType' (Proxy :: Proxy (IsSum f)) (Proxy :: Proxy f)

class GToField' (isSum :: Bool) f where
    gToField' :: Proxy isSum -> f p -> Encoded
    gLogicalType' :: Proxy isSum -> Proxy f -> LogicalTypeRep

-- Products (single constructor records)
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
resolveNames =
    map pick
  where
    pick (_, Just n) = n
    pick (idx, Nothing) = Text.pack ("field" <> show (idx + 1))

listArrayFrom :: [Text] -> [b] -> Array Int (StructField b)
listArrayFrom names values =
    case values of
        [] -> listArray (0, -1) []
        _ ->
            listArray
                (0, length values - 1)
                (zipWith (\n v -> StructField{structFieldName = n, structFieldValue = v}) names values)

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

class GStructDecode f where
    gStructDecodeStruct :: Proxy (f p) -> StructValue FieldValue -> Either String (f p)
    gStructNull :: Proxy (f p) -> f p
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
-- DuckDB type constructors (re-exported patterns)

-- These pattern synonyms come from duckdb-ffi; re-exporting to avoid users having to import it.
