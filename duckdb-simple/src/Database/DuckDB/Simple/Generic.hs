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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.DuckDB.Simple.Generic
-- Description : Generic helpers for encoding Haskell ADTs as DuckDB structs/unions.
--
-- Encode and decode algebraic data types through @ToField@/@FromField@ with
-- GHC generics. The mapping is:
--
-- * /Product types/ (records or tuples) whose fields already satisfy @DuckValue@
--   are encoded as STRUCT values. Record fields retain their selector name;
--   positional products fall back to @field1@, @field2@, …
-- * /Sum types/ (:+:) become UNION values. Each constructor becomes a union
--   member; payloads are encoded as structs (a STRUCT without fields for a
--   constructor without fields).
--
-- Each field needs a @DuckValue@ instance with a finite logical type. A nested
-- user-defined type needs its own instance.
--
-- Encode a product:
--
-- > data User = User { userId :: Int64, userName :: Text }
-- >   deriving stock (Generic)
-- >
-- > genericToFieldValue (User 1 "alice")
-- > -- A FieldStruct with the fields userId = FieldInt64 1 and userName = FieldText "alice".
--
-- For sum types:
--
-- > data Shape
-- >   = Circle Double
-- >   | Rectangle Double Double
-- >   | Origin
-- >   deriving stock (Generic)
--
-- Constructors are turned into a union with the members @Circle@ (a STRUCT with
-- the field @field1@), @Rectangle@ (a STRUCT with the fields @field1@ and
-- @field2@), and @Origin@ (a STRUCT without fields).
--
-- Derive instances with @DerivingVia@ and the @ViaDuckDB@ newtype:
--
-- > data User = User { userId :: Int64, userName :: Text }
-- >   deriving stock (Generic)
-- >   deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB User)
--
-- The derived instances encode and decode STRUCT/UNION representations.
-- Also derive @ToDuckValue@ to use the type as a column of a typed table function,
-- as an argument or result of a typed aggregate function, or as the result of
-- a scalar function.
--
-- === Extending this module
--
-- The implementation has these parts:
--
-- * @DuckValue@ covers leaf-level conversions between Haskell values, DuckDB
--   @FieldValue@s, and logical type metadata.
-- * @GToField@ and the related classes convert the generic representation to
--   structs or unions. They keep the logical type information for parameter
--   binding.
-- * @ViaDuckDB@ supplies the deriving-via instances.
--
-- Follow this structure when you add a feature, and document each new class.
module Database.DuckDB.Simple.Generic (
  -- * Field-level primitives
  DuckValue (..),
  typedFromField,
  GToField (),
  GFromField (),

  -- * Generic encoding/decoding for ADTs
  genericToFieldValue,
  genericFromFieldValue,
  genericLogicalType,
  genericToStructValue,
  genericToUnionValue,

  -- * DerivingVia helper
  ViaDuckDB (..),
) where

import Control.Exception (toException)
import Control.Monad (unless)
import Data.Array (Array, elems, listArray)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (intercalate, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Typeable (Typeable)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Conversion (
  Conversion,
  conversionError,
  mapConversionErrors,
  mapConversionErrorsWith,
 )
import Database.DuckDB.Simple.Element (containsType, valueTypeName)
import Database.DuckDB.Simple.FromField (
  Field (..),
  FieldValue (..),
  FromField (..),
  IntervalValue (..),
  ResultError (..),
  SizedArray,
  TimeWithZone (..),
  fieldTypeName,
  returnError,
  sizedArrayFromList,
  sizedArrayToArray,
 )
import Database.DuckDB.Simple.Internal (duckDBTypeToName)
import Database.DuckDB.Simple.LogicalRep (
  LogicalTypeRep (..),
  StructField (..),
  StructValue (..),
  TypeDeclaration (..),
  UnionMemberType (..),
  UnionValue (..),
  listElementType,
  mapEntryTypes,
 )
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Time (Unbounded (..))
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), ToDuckValue (..), ToField (..))
import Database.DuckDB.Simple.Variant (Variant (..))
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal)
import Numeric.Natural (Natural)

--------------------------------------------------------------------------------
-- DuckValue: bridge between Haskell scalars and FieldValue/LogicalTypeRep

-- | Types that can appear inside generated structs/unions.
--
-- A @DuckValue@ instance must provide:
--
-- * encoding to @FieldValue@
-- * logical type metadata (@duckLogicalType@)
-- * decoding from @FieldValue@
--
-- The primitive instances define the scalar representations for generic
-- conversion. The default decoder uses the @FromField@ instance of the type.
-- See Note [Generic decoding].
class DuckValue a where
  -- | Encode a field value.
  duckToField :: a -> FieldValue

  -- | Decode a field value. Run it with @runConversion@.
  duckFromField :: FieldValue -> Conversion a

  -- | Give the SQL type used to encode the field.
  duckLogicalType :: Proxy a -> LogicalTypeRep

  -- | Decode a value with the SQL type of its place, for example the type of a
  -- field of a STRUCT or of an element of a LIST. The default runs
  -- 'duckFromField' and ignores the supplied type. An instance that reads
  -- through 'FromField' can use
  -- 'typedFromField', so that 'fieldType' returns the type. See
  -- Note [Column types] in "Database.DuckDB.Simple.FromField".
  duckFromTypedField :: LogicalTypeRep -> FieldValue -> Conversion a
  duckFromTypedField _ = duckFromField

  default duckFromField :: (FromField a) => FieldValue -> Conversion a
  duckFromField = typedFromField (LogicalTypeScalar LogicalTypeIdVariant)

{- Note [Generic decoding]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Generic decoding uses Conversion because a FromField instance can request SQL
metadata. Encoding and logical type declarations remain pure. Error context is
added only when a conversion's failure is selected. A discarded Alternative
branch does not request metadata for its diagnostic.
-}

-- | Decode a value through 'FromField', with a t'Field' of the given SQL type.
-- See Note [Generic decoding] and Note [Column types] in
-- "Database.DuckDB.Simple.FromField".
typedFromField :: (FromField a) => LogicalTypeRep -> FieldValue -> Conversion a
typedFromField logical fv =
  fromField Field{fieldName = Text.empty, fieldIndex = 0, fieldValue = fv, fieldType = pure logical}

instance DuckValue Variant where
  duckToField = variantPayload
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdVariant
  duckFromTypedField = typedFromField

instance DuckValue Bool where
  duckToField = FieldBool
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdBoolean
  duckFromTypedField = typedFromField

instance DuckValue Int where
  duckToField = FieldInt64 . fromIntegral
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdBigInt
  duckFromTypedField = typedFromField

instance DuckValue Int8 where
  duckToField = FieldInt8
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdTinyInt
  duckFromTypedField = typedFromField

instance DuckValue Int16 where
  duckToField = FieldInt16
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdSmallInt
  duckFromTypedField = typedFromField

instance DuckValue Int32 where
  duckToField = FieldInt32
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdInteger
  duckFromTypedField = typedFromField

instance DuckValue Int64 where
  duckToField = FieldInt64
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdBigInt
  duckFromTypedField = typedFromField

-- | A field of a generic record has the type HUGEINT. A value outside that range
-- raises an error. A top-level parameter of this type also binds as HUGEINT
-- when the value is in that range. See Note [Integer parameters] in
-- "Database.DuckDB.Simple.ToField".
instance DuckValue Integer where
  duckToField = FieldHugeInt
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdHugeInt
  duckFromTypedField = typedFromField

-- | A field of a generic record has the type UHUGEINT. A value outside that range
-- raises an error. A top-level parameter of this type binds as HUGEINT when the
-- value is in the HUGEINT range. See Note [Integer parameters] in
-- "Database.DuckDB.Simple.ToField".
instance DuckValue Natural where
  duckToField = FieldUHugeInt . toInteger
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdUHugeInt
  duckFromTypedField = typedFromField

instance DuckValue Word where
  duckToField = FieldWord64 . fromIntegral
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdUBigInt
  duckFromTypedField = typedFromField

instance DuckValue Word8 where
  duckToField = FieldWord8
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdUTinyInt
  duckFromTypedField = typedFromField

instance DuckValue Word16 where
  duckToField = FieldWord16
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdUSmallInt
  duckFromTypedField = typedFromField

instance DuckValue Word32 where
  duckToField = FieldWord32
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdUInteger
  duckFromTypedField = typedFromField

instance DuckValue Word64 where
  duckToField = FieldWord64
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdUBigInt
  duckFromTypedField = typedFromField

instance DuckValue Float where
  duckToField = FieldFloat
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdFloat
  duckFromTypedField = typedFromField

instance DuckValue Double where
  duckToField = FieldDouble
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdDouble
  duckFromTypedField = typedFromField

instance DuckValue Text where
  duckToField = FieldText
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdVarchar
  duckFromTypedField = typedFromField

instance DuckValue String where
  duckToField = FieldText . Text.pack
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdVarchar
  duckFromField fv = Text.unpack <$> duckFromField fv
  duckFromTypedField logical fv = Text.unpack <$> duckFromTypedField logical fv

instance DuckValue BS.ByteString where
  duckToField = FieldBlob
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdBlob
  duckFromTypedField = typedFromField

instance DuckValue Day where
  duckToField = FieldDate . Finite
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdDate
  duckFromTypedField = typedFromField

instance DuckValue TimeOfDay where
  duckToField = FieldTime
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdTime
  duckFromTypedField = typedFromField

instance DuckValue LocalTime where
  duckToField = FieldTimestamp . Finite
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdTimestamp
  duckFromTypedField = typedFromField

instance DuckValue UTCTime where
  duckToField = FieldTimestampTZ . Finite
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdTimestampTz
  duckFromTypedField = typedFromField

instance DuckValue (Unbounded Day) where
  duckToField = FieldDate
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdDate
  duckFromTypedField = typedFromField

instance DuckValue (Unbounded LocalTime) where
  duckToField = FieldTimestamp
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdTimestamp
  duckFromTypedField = typedFromField

instance DuckValue (Unbounded UTCTime) where
  duckToField = FieldTimestampTZ
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdTimestampTz
  duckFromTypedField = typedFromField

instance DuckValue UUID.UUID where
  duckToField = FieldUUID
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdUUID
  duckFromTypedField = typedFromField

instance DuckValue IntervalValue where
  duckToField = FieldInterval
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdInterval
  duckFromTypedField = typedFromField

instance DuckValue TimeWithZone where
  duckToField = FieldTimeTZ
  duckLogicalType _ = LogicalTypeScalar LogicalTypeIdTimeTz
  duckFromTypedField = typedFromField

instance (DuckValue a) => DuckValue (Maybe a) where
  duckToField (Just x) = duckToField x
  duckToField Nothing = FieldNull
  duckLogicalType _ = duckLogicalType (Proxy @a)
  duckFromField FieldNull = pure Nothing
  duckFromField other = Just <$> duckFromField other
  duckFromTypedField _ FieldNull = pure Nothing
  duckFromTypedField logical other = Just <$> duckFromTypedField logical other

-- | List values encode as DuckDB LIST (variable-length). A field reads a LIST
-- and an ARRAY, as the top-level @FromField@ instance of a list does. 'String'
-- keeps its own instance.
instance {-# OVERLAPPABLE #-} (DuckValue a) => DuckValue [a] where
  duckToField xs = FieldList (map duckToField xs)
  duckLogicalType _ = LogicalTypeList (duckLogicalType (Proxy @a))
  duckFromField = duckFromTypedField (LogicalTypeScalar LogicalTypeIdVariant)
  duckFromTypedField logical (FieldList fvs) = traverse (duckFromTypedField (listElementType logical)) fvs
  duckFromTypedField logical (FieldArray arr) = traverse (duckFromTypedField (listElementType logical)) (elems arr)
  duckFromTypedField logical other = unexpectedTypedValue logical "LIST or ARRAY" other

-- | A 'SizedArray' field is an ARRAY of size @n@. It reads an ARRAY and a LIST of
-- @n@ elements, as the 'FromField' instance does. See Note [Sized arrays] in
-- "Database.DuckDB.Simple.ToField".
instance (KnownNat n, DuckValue a) => DuckValue (SizedArray n a) where
  duckToField values = FieldArray (fmap duckToField (sizedArrayToArray values))

  -- See Note [Sized ARRAY bounds] in "Database.DuckDB.Simple.Native".
  duckLogicalType _ = LogicalTypeArray (duckLogicalType (Proxy @a)) (fromInteger (min (natVal (Proxy @n)) (toInteger (maxBound :: Word64))))
  duckFromField = duckFromTypedField (LogicalTypeScalar LogicalTypeIdVariant)
  duckFromTypedField logical value = case value of
    FieldArray arr -> sized (elems arr)
    FieldList entries -> sized entries
    other -> unexpectedTypedValue logical "ARRAY" other
    where
      size = natVal (Proxy @n)
      sized entries
        | Just reason <- arraySizeReason size = conversionError (sizeError reason)
        | otherwise = case sizedArrayFromList entries of
            Nothing -> conversionError (sizeError (Text.pack ("the value has " <> show (length entries) <> " elements")))
            Just values -> traverse (duckFromTypedField (listElementType logical)) values
      sizeError reason =
        Incompatible
          (fieldTypeName logical value)
          Text.empty
          (Text.pack ("SizedArray " <> show size))
          reason

-- | Map values encode as DuckDB MAP.
instance (Ord k, DuckValue k, DuckValue v) => DuckValue (Map.Map k v) where
  duckToField m =
    let pairs = Map.toList m
    in  FieldMap [(duckToField k, duckToField v) | (k, v) <- pairs]
  duckLogicalType _ =
    LogicalTypeMap
      (duckLogicalType (Proxy @k))
      (duckLogicalType (Proxy @v))
  duckFromField = duckFromTypedField (LogicalTypeScalar LogicalTypeIdVariant)
  duckFromTypedField logical (FieldMap pairs) = do
    result <- Map.fromList <$> traverse decodePair pairs
    if Map.size result == length pairs
      then pure result
      else decodingError "duplicate MAP key after conversion"
    where
      (keyType, valueType) = mapEntryTypes logical
      decodePair (kfv, vfv) = do
        k <- duckFromTypedField keyType kfv
        v <- duckFromTypedField valueType vfv
        pure (k, v)
  duckFromTypedField logical other = unexpectedTypedValue logical "MAP" other

-- | The error of a decoder that does not read a value. The text has the form of
-- each other read error: it names the SQL type of the value, not the value,
-- because a value can be large.
unexpectedValue :: String -> FieldValue -> Conversion a
unexpectedValue expected value = conversionError (Incompatible (valueTypeName value) Text.empty (Text.pack expected) Text.empty)

-- | The error of a decoder that cannot read a value with the given SQL type.
-- It names the type as 'returnError' does.
unexpectedTypedValue :: LogicalTypeRep -> String -> FieldValue -> Conversion a
unexpectedTypedValue logical expected value = conversionError (Incompatible (fieldTypeName logical value) Text.empty (Text.pack expected) Text.empty)

--------------------------------------------------------------------------------
-- Generic machinery

-- | A STRUCT or UNION value with its logical type metadata, produced by the
-- generic traversal for @genericToFieldValue@. @genericLogicalType@ produces
-- the type metadata without a value.
data Encoded
  = EncodedStruct (StructValue FieldValue)
  | EncodedUnion (UnionValue FieldValue)

encodedValue :: Encoded -> FieldValue
encodedValue = \case
  EncodedStruct sv -> FieldStruct sv
  EncodedUnion uv -> FieldUnion uv

-- | Convert a Haskell value (using its generic representation) into a DuckDB @FieldValue@.
genericToFieldValue :: forall a. (Generic a, GToField (Rep a)) => a -> FieldValue
genericToFieldValue = encodedValue . gToField . from

-- | Give the logical DuckDB type of a Haskell type.
genericLogicalType :: forall a. (Generic a, GToField (Rep a)) => Proxy a -> LogicalTypeRep
genericLogicalType _ = gLogicalType (Proxy @(Rep a ()))

-- | Decode a DuckDB @FieldValue@ with its generic representation. Run the
-- result with @runConversion@. See Note [Generic decoding].
genericFromFieldValue :: forall a. (Generic a, GFromField (Rep a)) => FieldValue -> Conversion a
genericFromFieldValue fv = to <$> gFromField fv

-- | Extract the STRUCT encoding when the value produces one.
genericToStructValue :: forall a. (Generic a, GToField (Rep a)) => a -> Maybe (StructValue FieldValue)
genericToStructValue value =
  case gToField (from value) of
    EncodedStruct sv -> Just sv
    _ -> Nothing

-- | Extract a UNION-shaped generic encoding directly when one is produced.
genericToUnionValue :: forall a. (Generic a, GToField (Rep a)) => a -> Maybe (UnionValue FieldValue)
genericToUnionValue value =
  case gToField (from value) of
    EncodedUnion uv -> Just uv
    _ -> Nothing

-- | Wrapper for deriving-via so that @instance ToField (ViaDuckDB a)@ picks up
-- the generic encoding provided by this module.
newtype ViaDuckDB a = ViaDuckDB {getViaDuckDB :: a}

-- Type family to decide whether a representation is a sum.

-- | Select sum encoding with @'True@ and product encoding with @'False@.
type family IsSum f :: Bool where
  IsSum (f :+: g) = 'True
  IsSum (M1 D _ f) = IsSum f
  IsSum (M1 C _ f) = IsSum f
  IsSum _ = 'False

-- | Generic encoding to the intermediate @Encoded@ representation. Every
-- instance must also supply the corresponding logical type description.
class GToField f where
  gToField :: f p -> Encoded
  gLogicalType :: Proxy (f p) -> LogicalTypeRep

instance (GToField' (IsSum f) f) => GToField f where
  gToField = gToField' (Proxy @(IsSum f))
  gLogicalType _ = gLogicalType' (Proxy @(IsSum f)) (Proxy @f)

-- | Select product (@'False@) or sum (@'True@) encoding with the @IsSum@ type
-- family.
class GToField' (isSum :: Bool) f where
  gToField' :: Proxy isSum -> f p -> Encoded
  gLogicalType' :: Proxy isSum -> Proxy f -> LogicalTypeRep

-- Products (single constructor records)

{- Note [Types without fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A product type with no fields encodes as the STRUCT without fields. The payload
of a constructor with no fields in a sum type is also the STRUCT without fields,
and the type of its UNION member is the STRUCT type without fields. DuckDB shows
this value as {}. The value is not NULL.

- Why not NULL. NULL is the encoding of Nothing in a field of the type Maybe a.
  If a value without fields encodes as NULL, then Just x and Nothing are the
  same value in a field.

- The decoders also accept NULL for a product type with no fields and for the
  payload of a constructor with no fields. NULL decodes as the value with
  no fields.
-}

-- | Product encoding: single-constructor datatypes become STRUCT values. See
-- Note [Types without fields].
instance (GStruct f) => GToField' 'False (M1 D meta (M1 C c f)) where
  gToField' _ (M1 (M1 inner)) =
    let comps = gStructValues inner
        typeComps = gStructTypes (Proxy @(f _))
        names = resolveNames (zip progIndices (map fcName comps))
        valueArray = listArrayFrom names (map fcValue comps)
        typeArray = listArrayFrom names (map fcValue typeComps)
        indexMap = Map.fromList (zip names [0 ..])
    in  EncodedStruct
          StructValue
            { structValueFields = valueArray
            , structValueTypes = typeArray
            , structValueIndex = indexMap
            }
    where
      progIndices = [0 :: Int ..]
  gLogicalType' _ _ =
    let typeComps = gStructTypes (Proxy @(f _))
        names = resolveNames (zip [0 :: Int ..] (map fcName typeComps))
        typeArray = listArrayFrom names (map fcValue typeComps)
    in  LogicalTypeStruct typeArray

-- Sums (encode as union)

-- | Sum encoding: multi-constructor datatypes become UNION values.
instance (GSum f) => GToField' 'True (M1 D meta f) where
  gToField' _ (M1 value) =
    let members = gSumMembers (Proxy @(f _))
        membersArray = listArray (0, length members - 1) members
        (idx, payload) = gSumEncode value
        label = unionMemberName (members !! idx)
    in  EncodedUnion
          UnionValue
            { unionValueIndex = fromIntegral idx
            , unionValueLabel = label
            , unionValuePayload = payload
            , unionValueMembers = membersArray
            }
  gLogicalType' _ _ =
    let members = gSumMembers (Proxy @(f _))
        membersArray = listArray (0, length members - 1) members
    in  LogicalTypeUnion membersArray

--------------------------------------------------------------------------------
-- GStruct: products

data FieldComponent a = FieldComponent
  { fcName :: Maybe Text
  , fcValue :: a
  }

-- | Keep the selector names from GHC.Generics. Use @fieldN@ for positional
-- products.
resolveNames :: [(Int, Maybe Text)] -> [Text]
resolveNames =
  map pick
  where
    pick (_, Just n) = n
    pick (idx, Nothing) = Text.pack ("field" <> show (idx + 1))

-- | Helper that builds an @Array@ of struct fields from parallel lists of names
-- and payloads.
listArrayFrom :: [Text] -> [b] -> Array Int (StructField b)
listArrayFrom names values =
  listArray
    (0, length values - 1)
    (zipWith (\n v -> StructField{structFieldName = n, structFieldValue = v}) names values)

-- | Collect the values and types of a product in parallel lists for encoding
-- and decoding.
class GStruct f where
  gStructValues :: f p -> [FieldComponent FieldValue]
  gStructTypes :: Proxy (f p) -> [FieldComponent LogicalTypeRep]

instance GStruct U1 where
  gStructValues _ = []
  gStructTypes _ = []

instance (GStruct a, GStruct b) => GStruct (a :*: b) where
  gStructValues (a :*: b) = gStructValues a ++ gStructValues b
  gStructTypes _ = gStructTypes (Proxy @(a _)) ++ gStructTypes (Proxy @(b _))

instance (Selector s, DuckValue a) => GStruct (M1 S s (K1 i a)) where
  gStructValues m@(M1 (K1 x)) =
    let name = toMaybe (selName m)
    in  [FieldComponent name (duckToField x)]
  gStructTypes _ =
    let raw = selName (undefined :: M1 S s (K1 i a) ())
        name = toMaybe raw
    in  [FieldComponent name (duckLogicalType (Proxy @a))]

instance (GStruct f) => GStruct (M1 C c f) where
  gStructValues (M1 x) = gStructValues x
  gStructTypes _ = gStructTypes (Proxy @(f _))

toMaybe :: String -> Maybe Text
toMaybe name
  | null name = Nothing
  | otherwise = Just (Text.pack name)

--------------------------------------------------------------------------------
-- Sums (unions)

-- | Describe the members of a sum type (@gSumMembers@). Encode a value as its
-- tag and payload (@gSumEncode@), or decode them (@gSumDecode@).
class GSum f where
  gSumMembers :: Proxy (f p) -> [UnionMemberType]
  gSumEncode :: f p -> (Int, FieldValue)
  gSumDecode :: Int -> FieldValue -> Conversion (f p)

instance (GSum a, GSum b) => GSum (a :+: b) where
  gSumMembers _ = gSumMembers (Proxy @(a _)) ++ gSumMembers (Proxy @(b _))
  gSumEncode (L1 x) = gSumEncode x
  gSumEncode (R1 x) =
    let leftCount = length (gSumMembers (Proxy @(a _)))
        (idx, payload) = gSumEncode x
    in  (idx + leftCount, payload)
  gSumDecode idx payload =
    let leftCount = length (gSumMembers (Proxy @(a _)))
    in  if idx < leftCount
          then L1 <$> gSumDecode idx payload
          else R1 <$> gSumDecode (idx - leftCount) payload

instance (Constructor c, GStruct f, GStructDecode f) => GSum (M1 C c f) where
  gSumMembers _ =
    [ UnionMemberType
        { unionMemberName = Text.pack (conName (undefined :: M1 C c f p))
        , unionMemberType =
            let typeComps = gStructTypes (Proxy @(f _))
                names = resolveNames (zip [0 :: Int ..] (map fcName typeComps))
            in  LogicalTypeStruct (listArrayFrom names (map fcValue typeComps))
        }
    ]

  -- See Note [Types without fields].
  gSumEncode (M1 x) =
    let comps = gStructValues x
        typeComps = gStructTypes (Proxy @(f _))
        names = resolveNames (zip [0 :: Int ..] (map fcName comps))
        valueArray = listArrayFrom names (map fcValue comps)
        typeArray = listArrayFrom names (map fcValue typeComps)
        indexMap = Map.fromList (zip names [0 ..])
    in  (0, FieldStruct StructValue{structValueFields = valueArray, structValueTypes = typeArray, structValueIndex = indexMap})
  gSumDecode idx payload
    | idx /= 0 = decodingError ("duckdb-simple: union tag mismatch (expected 0, got " <> show idx <> ")")
    | otherwise =
        case payload of
          -- NULL also decodes as a value with no fields. See Note [Types without fields].
          FieldNull -> M1 . fst <$> gStructDecodeList (Proxy @(f _)) []
          FieldStruct structVal -> do
            ordered <- either decodingError pure (orderStructFields (gStructTypes (Proxy @(f _))) structVal)
            M1 <$> gStructDecodeStruct (Proxy @(f _)) ordered
          other -> unexpectedValue "STRUCT for a member of a sum type" other

-- | Match decoded fields to their selector names before product decoding.
orderStructFields :: [FieldComponent LogicalTypeRep] -> StructValue FieldValue -> Either String (StructValue FieldValue)
orderStructFields components sv
  -- The fields have the order of the selectors, so the names are unique.
  | map structFieldName fields == names = Right sv
  | otherwise = reorder
  where
    names = resolveNames (zip [0 ..] (map fcName components))
    fields = elems (structValueFields sv)
    reorder = do
      let byName = Map.fromList [(structFieldName field, field) | field <- fields]
      unless (length fields <= length names) $
        Left "duckdb-simple: extra fields when decoding struct"
      unless (length fields == length names && Map.size byName == length names) $
        Left "duckdb-simple: struct field names or count mismatch"
      ordered <-
        traverse (\name -> maybe (Left ("duckdb-simple: missing struct field " <> Text.unpack name)) Right (Map.lookup name byName)) names
      -- The types keep the order of the fields. See Note [Column types] in
      -- "Database.DuckDB.Simple.FromField".
      let typeByName = Map.fromList [(structFieldName field, field) | field <- elems (structValueTypes sv)]
          variant name = StructField name (LogicalTypeScalar LogicalTypeIdVariant)
          orderedTypes = [fromMaybe (variant name) (Map.lookup name typeByName) | name <- names]
      pure
        sv{structValueFields = listArray (0, length ordered - 1) ordered, structValueTypes = listArray (0, length orderedTypes - 1) orderedTypes}

--------------------------------------------------------------------------------
-- GStructDecode: inverse of GStruct for decoding

-- | Inverse of @GStruct@: decode struct payloads back into a generic product.
class GStructDecode f where
  gStructDecodeStruct :: Proxy (f p) -> StructValue FieldValue -> Conversion (f p)

  -- | Consume a prefix of fields from left to right while decoding, returning
  --     the reconstructed value and any remaining fields. Each field has its
  --     type and its value.
  gStructDecodeList :: Proxy (f p) -> [(LogicalTypeRep, FieldValue)] -> Conversion (f p, [(LogicalTypeRep, FieldValue)])

instance GStructDecode U1 where
  gStructDecodeStruct _ structVal =
    if null (elems (structValueFields structVal))
      then pure U1
      else decodingError ("duckdb-simple: expected empty struct, but got " <> show (length (elems (structValueFields structVal))) <> " field(s)")
  gStructDecodeList _ xs = pure (U1, xs)

instance (GStructDecode a, GStructDecode b) => GStructDecode (a :*: b) where
  gStructDecodeStruct proxy structVal = do
    (decoded, rest) <- gStructDecodeList proxy (typedFields structVal)
    unless (null rest) $
      decodingError ("duckdb-simple: extra " <> show (length rest) <> " field(s) when decoding struct (too many fields provided)")
    pure decoded
  gStructDecodeList _ xs = do
    (leftVal, rest) <- gStructDecodeList (Proxy @(a _)) xs
    (rightVal, rest') <- gStructDecodeList (Proxy @(b _)) rest
    pure (leftVal :*: rightVal, rest')

instance (Selector s, DuckValue a) => GStructDecode (M1 S s (K1 i a)) where
  gStructDecodeStruct _ structVal =
    case typedFields structVal of
      [(logical, fv)] -> M1 . K1 <$> namedField (selName (undefined :: M1 S s (K1 i a) ())) (duckFromTypedField logical fv)
      [] -> decodingError "duckdb-simple: missing struct field (expected 1, got 0)"
      xs -> decodingError ("duckdb-simple: expected single field struct, but got " <> show (length xs) <> " fields")

  gStructDecodeList _ [] = decodingError "duckdb-simple: missing struct field (expected field but list is empty)"
  gStructDecodeList _ ((logical, fv) : rest) = do
    val <- namedField (selName (undefined :: M1 S s (K1 i a) ())) (duckFromTypedField logical fv)
    pure (M1 (K1 val), rest)

-- | The type and the value of each field of a STRUCT value, in order. A field
-- with no type in the value has the type VARIANT. See Note [Column types] in
-- "Database.DuckDB.Simple.FromField".
typedFields :: StructValue FieldValue -> [(LogicalTypeRep, FieldValue)]
typedFields StructValue{structValueFields, structValueTypes} =
  zip
    (map structFieldValue (elems structValueTypes) <> repeat (LogicalTypeScalar LogicalTypeIdVariant))
    (map structFieldValue (elems structValueFields))

-- | Name the record field in the error of its decoder, after the prefix of the
-- library. A positional field has no name. See Note [Generic decoding].
namedField :: String -> Conversion a -> Conversion a
namedField name
  | null name = id
  | otherwise = mapConversionErrors \errors ->
      [ toException
          ( ConversionFailed
              Text.empty
              Text.empty
              Text.empty
              (Text.pack ("duckdb-simple: field " <> name <> ": " <> withoutPrefix (intercalate "; " (map exceptionText errors))))
          )
      ]
  where
    withoutPrefix message = fromMaybe message (stripPrefix "duckdb-simple: " message)

-- | Fail a generic decoder with the supplied diagnostic text.
decodingError :: String -> Conversion a
decodingError = conversionError . ConversionFailed Text.empty Text.empty Text.empty . Text.pack

instance (GStructDecode f) => GStructDecode (M1 C c f) where
  gStructDecodeStruct _ structVal = M1 <$> gStructDecodeStruct (Proxy @(f _)) structVal
  gStructDecodeList _ values = do
    (inner, rest) <- gStructDecodeList (Proxy @(f _)) values
    pure (M1 inner, rest)

--------------------------------------------------------------------------------
-- GFromField (inverse generic)

-- | Generic decoding entry point mirroring @GToField@. This is used both by
-- @genericFromFieldValue@ and the @ViaDuckDB@ deriving helper.
class GFromField f where
  gFromField :: FieldValue -> Conversion (f p)

instance (GFromField' (IsSum f) f) => GFromField f where
  gFromField = gFromField' (Proxy @(IsSum f))

class GFromField' (isSum :: Bool) f where
  gFromField' :: Proxy isSum -> FieldValue -> Conversion (f p)

instance (GStruct f, GStructDecode f) => GFromField' 'False (M1 D meta (M1 C c f)) where
  gFromField' _ = \case
    -- NULL also decodes as a value with no fields. See Note [Types without fields].
    FieldNull -> M1 . M1 . fst <$> gStructDecodeList (Proxy @(f _)) []
    FieldStruct sv -> do
      ordered <- either decodingError pure (orderStructFields (gStructTypes (Proxy @(f _))) sv)
      M1 . M1 <$> gStructDecodeStruct (Proxy @(f _)) ordered
    other -> unexpectedValue "STRUCT for a product type" other

instance (GSum f) => GFromField' 'True (M1 D meta f) where
  gFromField' _ = \case
    FieldUnion uv -> do
      let actualMembers = elems (unionValueMembers uv)
          actualIndex = fromIntegral (unionValueIndex uv)
      unless (actualIndex < length actualMembers) $
        decodingError "duckdb-simple: union tag out of range"
      unless (unionMemberName (actualMembers !! actualIndex) == unionValueLabel uv) $
        decodingError "duckdb-simple: union tag and member name mismatch"
      let members = gSumMembers (Proxy @(f _))
          indexed = zip [0 ..] members
          matching = [idx | (idx, member) <- indexed, unionMemberName member == unionValueLabel uv]
      case matching of
        [idx] -> M1 <$> gSumDecode idx (unionValuePayload uv)
        _ -> decodingError "duckdb-simple: unknown union member name"
    other -> unexpectedValue "UNION for a sum type" other

instance GFromField' 'False (M1 D meta U1) where
  gFromField' _ _ = pure (M1 U1)

--------------------------------------------------------------------------------
-- ViaDuckDB instances

-- | The deriving-via version of @DuckDBColumnType@. The text is the bare name
-- of the generic logical type, for diagnostics. The declaration is the
-- structured generic logical type. Each field type comes from 'DuckValue'.
-- See Note [Type declarations] in "Database.DuckDB.Simple.ToField".
-- See Note [Sized ARRAY bounds] in
-- <src/Database.DuckDB.Simple.Native.html Database.DuckDB.Simple.Native>.
instance (Generic a, GToField (Rep a)) => DuckDBColumnType (ViaDuckDB a) where
  duckdbTypeDeclaration _
    | containsType invalidArray logical =
        Left (Text.pack ("a generic field has no ARRAY type, because a DuckDB ARRAY must have 1 to " <> show maxArraySize <> " elements"))
    | otherwise = Right (StructuredType logical)
    where
      logical = genericLogicalType (Proxy @a)
      invalidArray (LogicalTypeArray _ size) = isJust (arraySizeReason (toInteger size))
      invalidArray _ = False
  duckdbColumnTypeFor _ =
    case genericLogicalType (Proxy @a) of
      LogicalTypeStruct{} -> Text.pack "STRUCT"
      LogicalTypeTuple{} -> Text.pack "TUPLE"
      LogicalTypeUnion{} -> Text.pack "UNION"
      LogicalTypeList{} -> Text.pack "LIST"
      LogicalTypeArray{} -> Text.pack "ARRAY"
      LogicalTypeMap{} -> Text.pack "MAP"
      LogicalTypeScalar dtype -> duckDBTypeToName dtype
      LogicalTypeDecimal{} -> Text.pack "DECIMAL"
      LogicalTypeEnum{} -> Text.pack "ENUM"
      LogicalTypeGeometry{} -> Text.pack "GEOMETRY"
      LogicalTypeCollatedVarchar{} -> Text.pack "VARCHAR"
      LogicalTypeAlias name _ -> name

-- | Deriving-via @ToField@ instance. A sum type binds as a UNION and a product
-- type as a STRUCT, through the @ToField@ instances of those values. A product
-- type with no fields binds as a STRUCT without fields. See
-- Note [Types without fields].
instance (Generic a, GToField (Rep a)) => ToField (ViaDuckDB a) where
  toField (ViaDuckDB x) = case gToField (from x) of
    EncodedUnion unionVal -> toField unionVal
    EncodedStruct structVal -> toField structVal

-- | Deriving-via @ToDuckValue@ instance. A sum type gives a UNION value and a
-- product type a STRUCT value, through the @ToDuckValue@ instances of those
-- values. See Note [Types without fields].
instance (Generic a, GToField (Rep a)) => ToDuckValue (ViaDuckDB a) where
  toDuckValue factory (ViaDuckDB x) = case gToField (from x) of
    EncodedUnion unionVal -> toDuckValue factory unionVal
    EncodedStruct structVal -> toDuckValue factory structVal

-- | Deriving-via @FromField@ instance. It reads a STRUCT for a product type and
-- a UNION for a sum type. Convert decoder errors to @ResultError@ with
-- @returnError@. See Note [Generic decoding].
instance (Generic a, GFromField (Rep a), Typeable a) => FromField (ViaDuckDB a) where
  fromField f@Field{fieldValue} =
    mapConversionErrorsWith
      (\errors -> [] <$ returnError @(ViaDuckDB a) ConversionFailed f (Text.pack (intercalate "; " (map exceptionText errors))))
      (ViaDuckDB <$> genericFromFieldValue fieldValue)
