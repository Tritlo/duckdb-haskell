{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Database.DuckDB.Simple.ToField
-- Description : Convert Haskell parameters into DuckDB bindable values.
--
-- The @ToField@ class mirrors the interface provided by @sqlite-simple@ while
-- delegating to the DuckDB C API under the hood.
--
-- A 'ToDuckValue' instance of a new type needs no call of the C API. A newtype
-- instance uses the instance of the wrapped type, for example
-- @toDuckValue factory (Age years) = toDuckValue factory years@, and the
-- newtype also needs a 'DuckDBColumnType' instance. A record or a sum type gets
-- both instances through 'Database.DuckDB.Simple.Generic.ViaDuckDB':
--
-- > data User = User {userId :: Int64, userName :: Text}
-- >   deriving stock (Generic)
-- >   deriving (DuckDBColumnType, ToDuckValue) via (ViaDuckDB User)
module Database.DuckDB.Simple.ToField (
  FieldBinding,
  ToDuckValue (..),
  ValueFactory,
  NativeValue,
  ToField (..),
  DuckDBColumnType (..),
  NamedParam (..),
  duckdbColumnType,
  bindFieldBinding,
  renderFieldBinding,
) where

import Control.Exception (bracket, mask_, onException)
import Control.Monad (foldM, forM_, unless, when)
import Data.Array (Array, bounds, elems, listArray)
import Data.Bits (complement, shiftL, shiftR, xor, (.|.))
import qualified Data.ByteString as BS
import Data.Fixed (Pico)
import qualified Data.Geometry as G
import qualified Data.Geometry.WKT as WKT
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), timeOfDayToTime, utc, utcToLocalTime)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Element (containsType, timeTzMaxOffsetSeconds, timeTzOffsetBits, valueTypeName)
import Database.DuckDB.Simple.FromField (
  BigNum (..),
  BitString (..),
  DecimalValue (..),
  FieldValue (..),
  IntervalValue (..),
  SizedArray,
  TimeWithZone (..),
  sizedArrayToArray,
 )
import Database.DuckDB.Simple.Geometry (RawGeometry (..))
import Database.DuckDB.Simple.Internal (
  Statement (..),
  duckDBTypeFromName,
  duckDBTypeToName,
  preparedParameterType,
  storeBoundValue,
  withTypeCache,
 )
import Database.DuckDB.Simple.LogicalRep (
  LogicalTypeRep (..),
  StructField (..),
  StructValue (..),
  TupleValue (..),
  TypeDeclaration (..),
  UnionMemberType (..),
  UnionValue (..),
  logicalTypeSQL,
  structValueTypeRep,
  tupleValueTypeRep,
  unionValueTypeRep,
  withoutAliases,
 )
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import Database.DuckDB.Simple.TypeCache (cachedLogicalType, typeCacheConnection)
import Database.DuckDB.Simple.Types (Null (..))
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeFromRep, logicalTypeToBaseRep, logicalTypeToRep)
import Database.DuckDB.Simple.Variant (Variant (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CDouble (..), CFloat (..))
import Foreign.Marshal (fromBool)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import GHC.Exts (Ptr (..))
import GHC.Num.Integer (integerLog2, integerToAddr)
import GHC.TypeLits (KnownNat, natVal)
import Numeric.Natural (Natural)

-- | Represents a named parameter binding using the @:=@ operator.
data NamedParam where
  (:=) :: (ToField a) => Text -> a -> NamedParam

infixr 3 :=

-- | Encapsulates the action required to bind a single positional parameter, together with a textual description used in diagnostics.
data FieldBinding = FieldBinding
  { fieldBindingValue :: !(ValueBuilder -> IO (Maybe LogicalTypeRep) -> IO DuckDBV2ValueHandle)
  , fieldBindingDisplay :: !String
  }

-- | A factory and a type constructor of the same scope. The builder of a
-- statement resolves the configured CRS types of its connection. See
-- Note [CRS types of a connection] in "Database.DuckDB.Simple.TypeCache".
data ValueBuilder = ValueBuilder
  { builderFactory :: !DuckDBV2FactoryHandle
  , builderType :: !(LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle)
  }

-- | A builder that constructs each type directly through the factory.
plainBuilder :: DuckDBV2FactoryHandle -> ValueBuilder
plainBuilder factory = ValueBuilder factory (logicalTypeFromRep factory)

-- | Low-level class for values that convert directly to a 'NativeValue'.
class (DuckDBColumnType a) => ToDuckValue a where
  -- | Construct an owned value through a borrowed native factory. Keep the
  --     owner of the factory (a connection, a callback context, or an instance)
  --     alive until you destroy the value. This is a rule of the library. The
  --     header says that a factory is valid for as long as its owner is. The
  --     header gives no rule for a value after the owner of its factory ends.
  --     A value with a GEOMETRY type that has a CRS needs the factory of a
  --     connection or of a callback context, because the factory of an
  --     instance cannot make that type. Use 'ToField' to resolve arbitrary CRS
  --     labels from connection options.
  toDuckValue :: ValueFactory -> a -> IO NativeValue

valueBinding :: String -> (DuckDBV2FactoryHandle -> IO DuckDBV2ValueHandle) -> FieldBinding
valueBinding display make = builderValueBinding display (make . builderFactory)

-- | Construct a value with the builder of the statement's connection.
builderValueBinding :: String -> (ValueBuilder -> IO DuckDBV2ValueHandle) -> FieldBinding
builderValueBinding display makeValue =
  targetValueBinding display (\builder _ -> makeValue builder)

-- | Construct a value with the prepared parameter type, when it is available.
targetValueBinding :: String -> (ValueBuilder -> IO (Maybe LogicalTypeRep) -> IO DuckDBV2ValueHandle) -> FieldBinding
targetValueBinding display makeValue =
  FieldBinding
    { fieldBindingValue = makeValue
    , fieldBindingDisplay = display
    }

{- Note [Nanosecond parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A TimeOfDay, a LocalTime, and a UTCTime hold picoseconds. A TIME, a TIMESTAMP,
and a TIMESTAMPTZ hold microseconds. TIME_NS, TIMESTAMP_NS, and TIMESTAMPTZ_NS
hold nanoseconds. timeValueBinding binds these Haskell values, and their
Unbounded forms, with this rule:

- A value with no digits below a microsecond binds as a microsecond value. The
  binding does not read the parameter type.
- A value with digits below a microsecond makes the binding read the parameter
  type. A nanosecond parameter (TIME_NS, TIMESTAMP_NS, or TIMESTAMPTZ_NS for
  the three types) gets a nanosecond value. A VARIANT parameter also gets a
  nanosecond value, when the value fits the range of the nanosecond type
  (fitsNanoseconds). A VARIANT holds TIME_NS, TIMESTAMP_NS, and TIMESTAMPTZ_NS
  values. A value outside that range, for example in the year 3000, gets the
  microsecond value, as for a parameter with no known type. The payload of a
  Variant follows the same rule (variantPayloadType). Each other parameter,
  and a parameter with no known type, gets the microsecond value. The binding
  discards the digits below one microsecond, as the SQL literal does.
- The binding discards the digits below one nanosecond in each case.

A value in a SizedArray, a list, a Map, a STRUCT, a TUPLE, or a UNION parameter
follows the type of its leaf in the target: timestampTypeForTarget gives a
nanosecond type to a TIME, TIMESTAMP, or TIMESTAMPTZ leaf of a nanosecond
target leaf. These parameters read the parameter type for each value. A
VARIANT leaf does not give a nanosecond type: bindElement gives an element of
a VARIANT list or map no target, and timestampTypeForTarget has no VARIANT
case. A list, a SizedArray, or a Map for a VARIANT parameter has no element
type in the target (collectionChildType and mapPartTypes give Nothing), so its
time elements also get the microsecond value. One LIST has one element type
for all its elements. For a Haskell list, a SizedArray, or a Map of TimeOfDay,
LocalTime, or UTCTime values, withElementType declares the microsecond type of
the element, for example TIMESTAMP for a LocalTime, so a nanosecond element
would be cast to that type, and the cast rounds. A FieldList or a FieldMap
takes the type that its elements give, as the SQL list literal does
(withValueType). TIME and TIME_NS elements, and
TIMESTAMPTZ and TIMESTAMPTZ_NS elements, have no common type, and DuckDB raises
an error. TIMESTAMP and TIMESTAMP_NS elements give TIMESTAMP_NS[], but an
element outside the range of TIMESTAMP_NS stays a TIMESTAMP (fitsNanoseconds),
and DuckDB cannot cast that element to TIMESTAMP_NS. Thus a time element gets
the microsecond value in each of these collections, because a nanosecond value
would round or raise an error in some of them.

- Why the binding reads the parameter type only for such digits. A read of the
  parameter type runs the binder of DuckDB. See Note [Statement schemas] in
  "Database.DuckDB.Simple.Internal". A microsecond value gives the same result
  for each parameter type, so the read has no use for it.

- Why a microsecond parameter gets the microsecond value. In DuckDB 2.0.0, the
  SQL literal '03:04:05.123456999'::TIME gives 03:04:05.123456, but a cast from
  TIME_NS to TIME rounds and gives 03:04:05.123457. The same holds for
  TIMESTAMP. A nanosecond value would thus change what a microsecond column
  stores.

- An explicit cast of the parameter to a nanosecond type, as in
  ?::TIMESTAMP_NS, makes it a nanosecond parameter. DuckDB then rounds when it
  casts the value to a microsecond type, for example for a TIMESTAMP column or
  in ?::TIMESTAMP_NS + INTERVAL 1 DAY. A plain parameter for a microsecond
  type cuts the digits.

- A LocalTime for a TIMESTAMP_NS parameter, or for the TIMESTAMP_NS elements
  of a SizedArray, a list, or a Map, is in one of three zones:
  * In the range of TIMESTAMP_NS (about the years 1677 to 2262): no error.
  * Outside the range, when DuckDB also cannot cast the microsecond value to
    TIMESTAMP_NS (outsideNanosecondRange): the value binds as a microsecond
    value. For a parameter, DuckDB raises its SQLError "Could not convert
    Timestamp to higher precision", of the type ErrorTypeConversion, when it
    casts the value. For an element, the native constructor of the LIST, the
    ARRAY, or the MAP casts each element when the value binds, and it would
    raise the ErrorInputInvalid "Failed to cast value". Before that cast,
    checkNanosecondElements raises the SQLError of the library "duckdb-simple:
    TIMESTAMP value out of finite range", of the type ErrorTypeConversion, for
    each element that the cast cannot hold. Thus the element gets the type of
    a parameter, and no element gets an error that the cast does not raise.
  * From 2262-04-11 23:47:16.854775807 to 23:47:16.854775999: DuckDB can cast
    the microsecond value, but the cast cuts the digits with no error. Thus
    the encoder raises the SQLError of the library "duckdb-simple: TIMESTAMP
    value out of finite range", of the type ErrorTypeConversion. The low end of
    the range has no such zone, because the microsecond value is cut toward
    the earlier time.
  A UTCTime for a TIMESTAMPTZ_NS parameter, or for the TIMESTAMPTZ_NS elements
  of a SizedArray, a list, or a Map, is in the same three zones, on its instant in
  UTC. Each leaf of a STRUCT, TUPLE, or UNION outside the range gets the error
  of the encoder, because in TRY_CAST DuckDB keeps the other leaves of a
  microsecond value with their digits cut.
-}

-- | Test whether a time of day has digits below one microsecond. See
-- Note [Nanosecond parameters].
hasSubMicroseconds :: TimeOfDay -> Bool
hasSubMicroseconds time = snd (properFraction (todSec time * 1000000) :: (Integer, Pico)) /= 0

-- | Bind a TIME, TIMESTAMP, or TIMESTAMPTZ value. The source is the microsecond
-- type of the value, and the last argument makes the microsecond value. See
-- Note [Nanosecond parameters].
timeValueBinding :: String -> LogicalTypeRep -> FieldValue -> (ValueBuilder -> IO DuckDBV2ValueHandle) -> FieldBinding
timeValueBinding display source value makeDefault
  | subMicroseconds value = temporalValueBinding display source value makeDefault
  | otherwise = builderValueBinding display makeDefault
  where
    subMicroseconds (FieldTime time) = hasSubMicroseconds time
    subMicroseconds (FieldTimestamp (Finite LocalTime{localTimeOfDay})) = hasSubMicroseconds localTimeOfDay
    subMicroseconds (FieldTimestampTZ (Finite instant)) = hasSubMicroseconds (localTimeOfDay (utcToLocalTime utc instant))
    subMicroseconds _ = False

-- | Bind a value with the nanosecond types of the prepared parameter. See
-- Note [Nanosecond parameters].
temporalValueBinding :: String -> LogicalTypeRep -> FieldValue -> (ValueBuilder -> IO DuckDBV2ValueHandle) -> FieldBinding
temporalValueBinding display source value makeDefault =
  targetValueBinding display \builder readTarget -> do
    target <- readTarget
    let precise = case (target, source) of
          -- A time value with digits below a microsecond for a VARIANT
          -- parameter. See Note [Nanosecond parameters].
          (Just (LogicalTypeScalar LogicalTypeIdVariant), LogicalTypeScalar _)
            | fitsNanoseconds value -> nanosecondType source
          _ -> timestampTypeForTarget target source
    if precise == source || outsideNanosecondRange precise value
      then makeDefault builder
      else fieldValueWithTypeDuckValue builder precise (valueWithTypeMetadata precise value)

-- | Give the nanosecond type of a TIME, TIMESTAMP, or TIMESTAMPTZ type.
nanosecondType :: LogicalTypeRep -> LogicalTypeRep
nanosecondType (LogicalTypeScalar LogicalTypeIdTime) = LogicalTypeScalar LogicalTypeIdTimeNs
nanosecondType (LogicalTypeScalar LogicalTypeIdTimestamp) = LogicalTypeScalar LogicalTypeIdTimestampNs
nanosecondType (LogicalTypeScalar LogicalTypeIdTimestampTz) = LogicalTypeScalar LogicalTypeIdTimestampTzNs
nanosecondType rep = rep

-- | Test whether the nanosecond value of a time value fits the range of its
-- nanosecond type, with the range test of the encoder. Each time of day fits.
fitsNanoseconds :: FieldValue -> Bool
fitsNanoseconds (FieldTimestamp (Finite time)) = inFiniteRange (minBound :: Int64) maxBound (timestampUnits 1000000000 time)
fitsNanoseconds (FieldTimestampTZ (Finite instant)) = fitsNanoseconds (FieldTimestamp (Finite (utcToLocalTime utc instant)))
fitsNanoseconds _ = True

-- | Bind an element of a LIST, an ARRAY, or a MAP for its element type in the
-- target. A VARIANT element type gives no target, so the element binds as for
-- a parameter with no known type. See Note [Nanosecond parameters].
bindElement :: (ToField a) => ValueBuilder -> Maybe LogicalTypeRep -> a -> IO DuckDBV2ValueHandle
bindElement builder target value = fieldBindingValue (toField value) builder (pure elementTarget)
  where
    elementTarget = case target of
      Just (LogicalTypeScalar LogicalTypeIdVariant) -> Nothing
      _ -> target

-- | Test whether a t'LocalTime' for a TIMESTAMP_NS parameter, or a t'UTCTime'
-- for a TIMESTAMPTZ_NS parameter on its instant in UTC, is outside the range of
-- the nanosecond type, and DuckDB also cannot cast its microsecond value to that
-- type. See Note [Nanosecond parameters].
outsideNanosecondRange :: LogicalTypeRep -> FieldValue -> Bool
outsideNanosecondRange (LogicalTypeScalar LogicalTypeIdTimestampNs) (FieldTimestamp (Finite time)) =
  not (fits (timestampUnits 1000000000 time)) && not (fits (1000 * timestampUnits 1000000 time))
  where
    fits = inFiniteRange (minBound :: Int64) maxBound
outsideNanosecondRange (LogicalTypeScalar LogicalTypeIdTimestampTzNs) (FieldTimestampTZ (Finite instant)) =
  outsideNanosecondRange (LogicalTypeScalar LogicalTypeIdTimestampNs) (FieldTimestamp (Finite (utcToLocalTime utc instant)))
outsideNanosecondRange _ _ = False

-- | Read the element type of a prepared LIST or ARRAY parameter.
collectionChildType :: Maybe LogicalTypeRep -> Maybe LogicalTypeRep
collectionChildType (Just (LogicalTypeList child)) = Just child
collectionChildType (Just (LogicalTypeArray child _)) = Just child
collectionChildType _ = Nothing

-- | Read the key type and the value type of a prepared MAP parameter.
mapPartTypes :: Maybe LogicalTypeRep -> (Maybe LogicalTypeRep, Maybe LogicalTypeRep)
mapPartTypes (Just (LogicalTypeMap key value)) = (Just key, Just value)
mapPartTypes _ = (Nothing, Nothing)

-- | Give a nanosecond type to each TIME, TIMESTAMP, and TIMESTAMPTZ leaf of the
-- source whose target leaf has that nanosecond type. See
-- Note [Nanosecond parameters].
timestampTypeForTarget :: Maybe LogicalTypeRep -> LogicalTypeRep -> LogicalTypeRep
timestampTypeForTarget target source = case (target, source) of
  -- Keep an alias only when its base type stays unchanged.
  (_, LogicalTypeAlias _ base) ->
    let promoted = timestampTypeForTarget target base
    in  if promoted == base then source else promoted
  (Just (LogicalTypeScalar LogicalTypeIdTimestampTzNs), LogicalTypeScalar LogicalTypeIdTimestampTz) ->
    LogicalTypeScalar LogicalTypeIdTimestampTzNs
  (Just (LogicalTypeScalar LogicalTypeIdTimestampNs), LogicalTypeScalar LogicalTypeIdTimestamp) ->
    LogicalTypeScalar LogicalTypeIdTimestampNs
  (Just (LogicalTypeScalar LogicalTypeIdTimeNs), LogicalTypeScalar LogicalTypeIdTime) ->
    LogicalTypeScalar LogicalTypeIdTimeNs
  (_, LogicalTypeList child) ->
    maybe source (\expected -> LogicalTypeList (timestampTypeForTarget (Just expected) child)) (collectionChildType target)
  (_, LogicalTypeArray child size) ->
    maybe source (\expected -> LogicalTypeArray (timestampTypeForTarget (Just expected) child) size) (collectionChildType target)
  (Just (LogicalTypeMap targetKey targetValue), LogicalTypeMap key value) ->
    LogicalTypeMap (timestampTypeForTarget (Just targetKey) key) (timestampTypeForTarget (Just targetValue) value)
  (Just (LogicalTypeStruct targetFields), LogicalTypeStruct fields) ->
    let sourceFields = elems fields
        targets = [(identifier name, rep) | StructField name rep <- elems targetFields]
        matching = [lookup (identifier (structFieldName field)) targets | field <- sourceFields]
        updated = zipWith (\field expected -> field{structFieldValue = timestampTypeForTarget expected (structFieldValue field)}) sourceFields matching
    in  LogicalTypeStruct (listArray (bounds fields) updated)
  (Just (LogicalTypeTuple targetFields), LogicalTypeStruct fields) ->
    let matching = map Just (elems targetFields) <> repeat Nothing
        updated = zipWith (\field expected -> field{structFieldValue = timestampTypeForTarget expected (structFieldValue field)}) (elems fields) matching
    in  LogicalTypeStruct (listArray (bounds fields) updated)
  (Just (LogicalTypeTuple targetFields), LogicalTypeTuple fields) ->
    positionalTuple (elems targetFields) fields
  (Just (LogicalTypeStruct targetFields), LogicalTypeTuple fields) ->
    positionalTuple (map structFieldValue (elems targetFields)) fields
  (Just (LogicalTypeUnion targetMembers), LogicalTypeUnion members) ->
    let targets = [(identifier name, rep) | UnionMemberType name rep <- elems targetMembers]
    in  LogicalTypeUnion
          ( fmap
              (\member -> member{unionMemberType = timestampTypeForTarget (lookup (identifier (unionMemberName member)) targets) (unionMemberType member)})
              members
          )
  _ -> source
  where
    -- DuckDB compares identifiers without ASCII case.
    identifier = foldIdentifier
    positionalTuple targets fields = LogicalTypeTuple (listArray (bounds fields) (zipWith timestampTypeForTarget (map Just targets <> repeat Nothing) (elems fields)))

-- | Copy adapted type metadata into nested values without changing their payloads.
valueWithTypeMetadata :: LogicalTypeRep -> FieldValue -> FieldValue
valueWithTypeMetadata logical value = case (logical, value) of
  (LogicalTypeList child, FieldList values) -> FieldList (map (valueWithTypeMetadata child) values)
  (LogicalTypeArray child _, FieldArray values) -> FieldArray (fmap (valueWithTypeMetadata child) values)
  (LogicalTypeMap keyType valueType, FieldMap pairs) ->
    FieldMap [(valueWithTypeMetadata keyType key, valueWithTypeMetadata valueType item) | (key, item) <- pairs]
  (LogicalTypeStruct types, FieldStruct struct) ->
    let sourceFields = elems (structValueFields struct)
        fields = [(structFieldName field, structFieldValue field) | field <- elems types]
        matching = [lookup (structFieldName field) fields | field <- sourceFields]
        update field expected = case expected of
          Nothing -> field
          Just rep -> field{structFieldValue = valueWithTypeMetadata rep (structFieldValue field)}
    in  FieldStruct
          struct{structValueTypes = types, structValueFields = listArray (bounds (structValueFields struct)) (zipWith update sourceFields matching)}
  (LogicalTypeTuple types, FieldTuple tuple) ->
    let update item expected = maybe item (`valueWithTypeMetadata` item) expected
        fields = zipWith update (elems (tupleValueFields tuple)) (map Just (elems types) <> repeat Nothing)
    in  FieldTuple tuple{tupleValueTypes = types, tupleValueFields = listArray (bounds (tupleValueFields tuple)) fields}
  (LogicalTypeUnion members, FieldUnion union) ->
    let payload = case drop (fromIntegral (unionValueIndex union)) (elems members) of
          member : _ -> valueWithTypeMetadata (unionMemberType member) (unionValuePayload union)
          [] -> unionValuePayload union
    in  FieldUnion union{unionValueMembers = members, unionValuePayload = payload}
  _ -> value

{- Note [Type declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
duckdbTypeDeclaration gives the SQL type that a typed registration declares for
a Haskell type: an argument or the result of a scalar function, a parameter
or a column of a typed table function, and an argument or the result of a
typed aggregate function. Right gives the declaration. Left gives the reason
why the type has no declaration.

- The default is the text of duckdbColumnTypeFor. DuckDB parses the text when
  the registration declares the type. Thus a newtype with its own
  DuckDBColumnType instance gives its own type text, for example
  DECIMAL(18,3).
- Integer and Natural give HUGEINT and UHUGEINT, but their text is BIGNUM.
  See Note [Integer parameters].
- ViaDuckDB gives the structured type of the record or the sum type
  (genericLogicalType in "Database.DuckDB.Simple.Generic"). A SizedArray
  field, or a field of a newtype whose DuckValue instance gives the size,
  declares an ARRAY.
- Maybe a has the declaration of a.
- A list gives a LIST (listDeclaration): the text T[] for the element text T,
  or a structured LIST for a structured element type.
- A SizedArray gives the ARRAY of its size (arrayDeclaration, see
  Note [Sized arrays]). A newtype with the type text of one size, for example
  BIGINT[3], also declares that ARRAY. DuckDB then rejects an ARRAY of another
  size when it binds the call (a Binder Error). It casts a LIST of the same length to the ARRAY with no error,
  because the cast from LIST to ARRAY is implicit in DuckDB. A LIST of
  another length raises the conversion error of DuckDB when the statement
  runs.
- A Map gives a MAP (mapDeclaration). Two texts give the text MAP(K, V). In
  the other cases, a structured MAP needs a scalar type name as the text of a
  part (duckDBTypeFromName). With another text, a structured part that is a
  scalar type, or a LIST or a MAP of scalar types, gives its text, and the
  declaration is the text MAP(K, V), for example MAP(INTEGER[], HUGEINT[]).
  Each other case gives Left.
- StructValue, TupleValue, and UnionValue give Left. They have no static
  field types. Their text is the bare name STRUCT, TUPLE, or UNION. The bare
  text STRUCT is a valid type, a STRUCT with no fields. That type is also the
  type of a record with no fields. Thus a check of the declared type cannot
  find these three types.

A registration raises the Left text as an SQLError before it registers the
SQL object. For a parameter with no known type, a LIST or a MAP value with no
element that is not NULL takes its element type from a structured declaration
(withElementType). See Note [Element types of a list]. The ToDuckValue
instances of a list, a SizedArray, and a Map give the structured declaration of
the element as its target (declaredTarget).
-}

-- | Types that map to a concrete DuckDB column type when used with @ToField@.
-- A newtype with its own instance gives its own type text. A 'SizedArray' gives the
-- text of its ARRAY type, for example @BIGINT[3]@ for @SizedArray 3 Int64@.
-- See Note [Type declarations].
class DuckDBColumnType a where
  -- | The DuckDB type text of @a@.
  duckdbColumnTypeFor :: Proxy a -> Text

  -- | The SQL type that a typed registration declares for @a@, or the reason
  -- why @a@ has no declaration. The default is the text of
  -- 'duckdbColumnTypeFor'. A list declares a LIST, and a 'SizedArray' an
  -- ARRAY. See Note [Type declarations].
  duckdbTypeDeclaration :: Proxy a -> Either Text TypeDeclaration
  duckdbTypeDeclaration proxy = Right (TypeExpression (duckdbColumnTypeFor proxy))

-- | The structured declaration of a type, as the target type of an element.
-- 'Nothing' for a type text or a type with no declaration. See
-- Note [Type declarations].
declaredTarget :: (DuckDBColumnType a) => Proxy a -> Maybe LogicalTypeRep
declaredTarget proxy = case duckdbTypeDeclaration proxy of
  Right (StructuredType rep) -> Just rep
  _ -> Nothing

-- | Declare a LIST with the declaration of its element type. See
-- Note [Type declarations].
listDeclaration :: TypeDeclaration -> TypeDeclaration
listDeclaration (TypeExpression element) = TypeExpression (element <> "[]")
listDeclaration (StructuredType element) = StructuredType (LogicalTypeList element)

-- | Declare a MAP with the declarations of its key type and its value type.
-- See Note [Type declarations].
mapDeclaration :: TypeDeclaration -> TypeDeclaration -> Either Text TypeDeclaration
mapDeclaration key value = case (key, value) of
  (TypeExpression keyName, TypeExpression valueName) -> Right (TypeExpression (mapText keyName valueName))
  _ -> case LogicalTypeMap <$> structured key <*> structured value of
    Right rep -> Right (StructuredType rep)
    Left reason -> maybe (Left reason) (Right . TypeExpression) (mapText <$> typeText key <*> typeText value)
  where
    mapText keyName valueName = "MAP(" <> keyName <> ", " <> valueName <> ")"
    -- A scalar type, and a LIST or a MAP of such types, has a type text, for
    -- example HUGEINT[] for a list of Integer values.
    typeText (TypeExpression text) = Just text
    typeText (StructuredType rep) = repText rep
    repText (LogicalTypeScalar dtype) = Just (duckDBTypeToName dtype)
    repText (LogicalTypeList element) = (<> "[]") <$> repText element
    repText (LogicalTypeMap keyRep valueRep) = mapText <$> repText keyRep <*> repText valueRep
    repText _ = Nothing
    structured (StructuredType rep) = Right rep
    structured (TypeExpression text) = case duckDBTypeFromName text of
      Just dtype -> Right (LogicalTypeScalar dtype)
      Nothing ->
        Left
          ( "a MAP with the type text "
              <> text
              <> " and a structured type has no declaration. Use a newtype of the Map with its own DuckDBColumnType instance."
          )

-- | The reason why a type with no static field types has no declaration. See
-- Note [Type declarations].
noStaticFieldTypes :: Text -> Text
noStaticFieldTypes name =
  name <> " has no static field types. Use a record with ViaDuckDB, or a newtype with its own DuckDBColumnType instance."

-- | Report the DuckDB column type that best matches a given @ToField@ instance.
duckdbColumnType :: forall a. (DuckDBColumnType a) => Proxy a -> Text
duckdbColumnType = duckdbColumnTypeFor

-- | Apply a @FieldBinding@ to the given statement/index. The builder takes its
-- factory and its CRS types from the type cache of the statement's connection.
-- See Note [CRS types of a connection] in "Database.DuckDB.Simple.TypeCache".
bindFieldBinding :: Statement -> DuckDBIdx -> FieldBinding -> IO ()
bindFieldBinding stmt idx FieldBinding{fieldBindingValue} =
  withTypeCache (statementConnection stmt) \cache -> withConnectionFactory (typeCacheConnection cache) \factory ->
    bindDuckValue stmt idx (fieldBindingValue (ValueBuilder factory (cachedLogicalType cache)) (preparedParameterType stmt idx))

-- | Render a bound parameter for error reporting.
renderFieldBinding :: FieldBinding -> String
renderFieldBinding FieldBinding{fieldBindingDisplay} = fieldBindingDisplay

-- | Types that can be used as positional parameters.
class ToField a where
  toField :: a -> FieldBinding
  default toField :: (Show a, ToDuckValue a) => a -> FieldBinding
  toField value = valueBinding (show value) (\factory -> toDuckValue factory value)

instance ToField Null where
  toField Null = nullBinding "NULL"

instance ToField Bool

instance ToField Int

instance ToField Int8

instance ToField Int16

instance ToField Int32

instance ToField Int64

-- | A value from -2^127 to 2^127 - 1 binds as a HUGEINT. A value outside this
-- range binds as a UHUGEINT for a UHUGEINT parameter that can hold it, and as a
-- BIGNUM for a parameter that has no known type or is not an integer or DECIMAL
-- type. For another integer or DECIMAL parameter, such a value raises
-- t'SQLError'. Bind a t'BigNum' to bind a BIGNUM value in each case. See
-- Note [Integer parameters].
instance ToField Integer where
  toField = integerBinding

-- | A value from 0 to 2^127 - 1 binds as a HUGEINT. A larger value binds as a
-- UHUGEINT for a UHUGEINT parameter that can hold it, and as a BIGNUM for a
-- parameter that has no known type or is not an integer or DECIMAL type. For
-- another integer or DECIMAL parameter, such a value raises t'SQLError'. Bind a
-- t'BigNum' to bind a BIGNUM value in each case. See Note [Integer parameters].
instance ToField Natural where
  toField = integerBinding . toInteger

{- Note [Integer parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
integerBinding binds an Integer or a Natural parameter with this rule:

- A value from -2^127 to 2^127 - 1 binds as a HUGEINT. The binding does not
  read the parameter type.
- A value outside this range makes the binding read the parameter type. A
  UHUGEINT parameter that can hold the value gets a UHUGEINT value. Another
  integer parameter (TINYINT to UHUGEINT) or a DECIMAL parameter gets an
  SQLError with the type ErrorTypeConversion. Each other parameter, and a
  parameter with no known type, gets a BIGNUM value.

- Why a HUGEINT. In DuckDB 2.0.0, the casts from BIGNUM to the integer types
  have defects (Bignum::BignumToInt in
  src/include/duckdb/common/types/bignum.hpp):
  - A cast to HUGEINT or UHUGEINT fails for each value except 0 and -1.
  - A cast of a negative value to an unsigned type gives a wrapped value and
    no error. For example, -5 for a UBIGINT gives 18446744073709551611.
  - A cast reduces a value of 2^128 or more modulo 2^128 before it checks the
    range. For example, 2^128 + 5 for a BIGINT gives 5.
  DuckDB 2.0.0 has no cast from BIGNUM to DECIMAL. DuckDB checks the range of
  each cast from HUGEINT and UHUGEINT. For TINYINT to UBIGINT, DOUBLE,
  VARCHAR, BIGNUM, JSON, and VARIANT, the cast from HUGEINT gives the same
  result as the cast from BIGNUM where the cast from BIGNUM has no defect.

- Why the binding reads the parameter type only outside the HUGEINT range. A
  read of the parameter type runs the binder of DuckDB. See
  Note [Statement schemas] in "Database.DuckDB.Simple.Internal". A value in the
  HUGEINT range does not have this cost.

- What stays BIGNUM:
  - A value outside the HUGEINT range for a parameter with no known type, as
    in SELECT ?, binds as a BIGNUM. A cast that DuckDB adds later can still
    have the defects above.
  - A BigNum always binds as a BIGNUM.
  - A SizedArray or a list of Integer values for a target with no known type, or
    for a target with an element type that is not an integer type or a DECIMAL
    type, has BIGNUM elements. A cast that DuckDB adds later can still have
    the defects above.

- toDuckValue. ToDuckValue Integer and ToDuckValue Natural use the same range
  rule (hugeIntInRange): a value from -2^127 to 2^127 - 1 gives a HUGEINT
  value. A Natural from 2^127 to 2^128 - 1 gives a UHUGEINT value. Each other
  value raises an SQLError of the type ErrorTypeConversion (outsideRange).
  toDuckValue has no target type. A BIGNUM value would thus reach a HUGEINT
  column, and DuckDB 2.0.0 casts the BIGNUM 2^128 to the HUGEINT 0 with no
  error. The callers of toDuckValue are setConfigValue, defaultValue, the
  typed rows of table functions and COPY FROM, and the writer of a scalar
  function result. Use BigNum for a larger value.
  The default toField of the ToField class also calls toDuckValue. Thus a
  newtype over Integer with the default toField raises this error for each
  parameter value outside the HUGEINT range. An Integer parameter uses the
  rule of integerBinding above, for example a BIGNUM value for a parameter
  with no known type. Derive ToField with newtype deriving to keep that rule.

- Typed registrations. The DuckDBColumnType text of Integer and Natural stays
  BIGNUM, the type that holds each value: the element rule of a list
  parameter and the error texts use it. The declaration is HUGEINT for
  Integer and UHUGEINT for Natural, as for a field of a generic record (the
  DuckValue instances in "Database.DuckDB.Simple.Generic"). Thus a column, an
  argument, and a result of a typed registration have the type of the field.
  The ToDuckValue instances of a list, a SizedArray, and a Map give the
  declaration of the element as its target, so a list of Integer values in a
  typed row has HUGEINT elements. See Note [Type declarations].

- Arrays, lists, and maps. DuckDB casts each element of an ARRAY or a LIST
  value, and each key and value of a MAP value, to the declared type when it
  builds the value. The DuckDBColumnType of Integer, Natural, and BigNum is
  BIGNUM. For a target with an integer or DECIMAL element type, key type, or
  value type, withElementType declares that type in place of BIGNUM. Each
  Integer element is a HUGEINT or a UHUGEINT value, so DuckDB checks the range
  of the cast of each element. For example, [5, -6] for a UBIGINT[2] column
  raises an error. A SizedArray or a list of BigNum values also takes the element
  type of the target, but each element is a BIGNUM value. DuckDB casts each
  element with the casts from BIGNUM, so the result for BigNum elements does
  not improve. For each other target, and for a parameter with no known type,
  a list takes the type that its elements give (withValueType), as each scalar
  Integer and as a SQL list literal of the HUGEINT and BIGNUM values: [1, 2^64]
  gives HUGEINT[], and [5, 2^130] gives BIGNUM[]. An Integer has no fixed SQL
  type, so a list with no element, or with only NULL elements, takes the
  element type of a known target, and otherwise the type NULL, as [] and
  [NULL] in SQL. The test is on the column type name BIGNUM, so the rule also
  holds for BigNum elements and for each type whose column type name is
  BIGNUM, for example a newtype that derives its instances through Integer. A
  BigNum element is a BIGNUM value in each case.

Issue 41 of duckdb-haskell tracks the defects.
-}

-- | Bind an integer as a HUGEINT, a UHUGEINT, or a BIGNUM value. See
-- Note [Integer parameters].
integerBinding :: Integer -> FieldBinding
integerBinding value = case hugeIntInRange value of
  Just make -> valueBinding (show value) make
  Nothing -> targetValueBinding (show value) \ValueBuilder{builderFactory = factory} readTarget -> do
    target <- readTarget
    case target of
      Just (LogicalTypeScalar LogicalTypeIdUHugeInt) | value >= 0 && value < 1 `shiftL` 128 -> uhugeIntDuckValue factory value
      Just (LogicalTypeScalar dtype) | dtype `elem` integerTypes -> outOfRange (duckDBTypeToName dtype)
      Just (LogicalTypeDecimal width scale) -> outOfRange ("DECIMAL(" <> Text.pack (show width) <> "," <> Text.pack (show scale) <> ")")
      _ -> bigNumDuckValue factory (BigNum value)
  where
    outOfRange typeName = throwConversionError ("the value " <> Text.pack (show value) <> " is out of range for a parameter of the type " <> typeName)

-- | Construct a HUGEINT value for an integer from -2^127 to 2^127 - 1. Give
-- Nothing for each other integer. See Note [Integer parameters].
hugeIntInRange :: Integer -> Maybe (DuckDBV2FactoryHandle -> IO DuckDBV2ValueHandle)
hugeIntInRange value
  | value >= negate (1 `shiftL` 127) && value < 1 `shiftL` 127 = Just (`hugeIntDuckValue` value)
  | otherwise = Nothing

-- | The integer types, from TINYINT to UHUGEINT. See Note [Integer parameters].
integerTypes :: [LogicalTypeId]
integerTypes =
  [ LogicalTypeIdTinyInt
  , LogicalTypeIdSmallInt
  , LogicalTypeIdInteger
  , LogicalTypeIdBigInt
  , LogicalTypeIdHugeInt
  , LogicalTypeIdUTinyInt
  , LogicalTypeIdUSmallInt
  , LogicalTypeIdUInteger
  , LogicalTypeIdUBigInt
  , LogicalTypeIdUHugeInt
  ]

instance ToField UUID.UUID

instance ToField Word

instance ToField Word8

instance ToField Word16

instance ToField Word32

instance ToField Word64

instance ToField Double

instance ToField Float

instance ToField Text

instance ToField String

instance ToField BitString

-- | Bind a day as a DATE. A day outside the range of DuckDB, from -5877641-06-25
-- (5877642-06-25 BC) to 5881580-07-10 in the notation of "Data.Time", raises an
-- t'SQLError' of the type 'ErrorTypeConversion'. See Note [Time ranges].
instance ToField Day

-- | Bind a time of day as a TIME, or as a TIME_NS. See
-- Note [Nanosecond parameters]. A time of exactly 60 seconds (a leap second)
-- binds as the next second, as @make_time@ of DuckDB gives it: 23:59:60 binds as
-- 24:00:00. A later time, for example 23:59:60.5, raises an t'SQLError'. The
-- section \"Errors\" of the README gives its type. See Note [Time ranges].
instance ToField TimeOfDay where
  toField value =
    timeValueBinding (show value) (LogicalTypeScalar LogicalTypeIdTime) (FieldTime value) \builder -> timeOfDayDuckValue (builderFactory builder) value

-- | Bind a local time as a TIMESTAMP, or as a TIMESTAMP_NS. See
-- Note [Nanosecond parameters]. A time outside the range of DuckDB, from
-- -290308-12-22 00:00:00 (290309-12-22 BC) to 294247-01-10 04:00:54.775806 in
-- the notation of "Data.Time", raises an t'SQLError' of the type
-- 'ErrorTypeConversion'. For a TIMESTAMP_NS target, a time outside the range
-- of TIMESTAMP_NS can get another error, also a time outside the range of
-- DuckDB in a STRUCT, a TUPLE, or a UNION. See Note [Nanosecond parameters]. A
-- time of exactly 60 seconds (a leap second) binds as the next second. A later
-- time, for example 23:59:60.5, raises an t'SQLError'. The section
-- \"Errors\" of the README gives its type. See Note [Time ranges].
instance ToField LocalTime where
  toField value =
    timeValueBinding (show value) (LogicalTypeScalar LogicalTypeIdTimestamp) (FieldTimestamp (Finite value)) \builder -> localTimeDuckValue (builderFactory builder) value

-- | Bind the months, days, and microseconds as an INTERVAL. DuckDB keeps the
-- three fields.
instance ToField IntervalValue

-- | Bind the time and its offset in seconds as a TIMETZ. An offset outside
-- -57599 to 57599 seconds raises an t'SQLError' of the type
-- 'ErrorTypeConversion'. A time with digits below one
-- microsecond loses these digits, as in SQL text. A time of exactly 60 seconds
-- (a leap second) binds as the next second. A later time, for example
-- 23:59:60.5, raises an t'SQLError'. The section \"Errors\" of the README gives
-- its type. See Note [Time ranges].
instance ToField TimeWithZone

-- | Bind the value as a DECIMAL with its own width and scale. DuckDB casts it
-- to the type of the parameter. A width outside 1 to 38, or a scale larger than
-- the width, raises an t'SQLError' of the type 'ErrorInputInvalid'. An integer
-- that the width cannot hold raises an t'SQLError' of the type
-- 'ErrorTypeConversion'.
instance ToField DecimalValue where
  toField value = valueBinding (show value) (`decimalDuckValue` value)

-- | Bind the shape as @GEOMETRY@ with no CRS.
instance ToField G.Geometry

-- | Bind the WKB bytes as a GEOMETRY value with the CRS of the raw geometry.
-- DuckDB then casts the value to the type of the parameter. See
-- Note [Raw GEOMETRY parameters].
instance ToField RawGeometry where
  toField raw@RawGeometry{rawGeometryWKB, rawGeometryCRS} =
    builderValueBinding
      ("<geometry length=" <> show (BS.length rawGeometryWKB) <> ">")
      \builder -> rawGeometryDuckValue builder rawGeometryCRS raw

-- | Bind the payload as a VARIANT with the type cache of the connection.
instance ToField Variant where
  toField value =
    builderValueBinding (show value) \builder -> variantDuckValue builder (variantPayload value)

-- | Bind an instant as a TIMESTAMPTZ, or as a TIMESTAMPTZ_NS. An instant
-- outside the range of the t'LocalTime' instance, in UTC, raises an
-- t'SQLError' of the type 'ErrorTypeConversion'. For a TIMESTAMPTZ_NS target,
-- an instant outside the range of TIMESTAMPTZ_NS can get another error, also
-- an instant outside the range of DuckDB in a STRUCT, a TUPLE, or a UNION, as
-- for a t'LocalTime'. See Note [Nanosecond parameters]. A leap second of
-- exactly 60 seconds binds as the next second. A later time in a leap second,
-- for example 60.5 seconds, raises an t'SQLError'. The section \"Errors\" of
-- the README gives its type. See Note [Time ranges].
instance ToField UTCTime where
  toField value =
    timeValueBinding
      (show value)
      (LogicalTypeScalar LogicalTypeIdTimestampTz)
      (FieldTimestampTZ (Finite value))
      (\builder -> utcTimeDuckValue (builderFactory builder) value)

-- | Bind a day or an infinity as a DATE. A finite day has the range of the
-- 'Day' instance.
instance ToField (Unbounded Day)

-- | Bind a local time or an infinity as a TIMESTAMP, or a local time as a
-- TIMESTAMP_NS. See Note [Nanosecond parameters]. A finite time has the range
-- of the t'LocalTime' instance.
instance ToField (Unbounded LocalTime) where
  toField value =
    timeValueBinding (show value) (LogicalTypeScalar LogicalTypeIdTimestamp) (FieldTimestamp value) \builder -> localTimestampDuckValue (builderFactory builder) value

-- | Bind an instant or an infinity as a TIMESTAMPTZ, or an instant as a
-- TIMESTAMPTZ_NS. See Note [Nanosecond parameters]. A finite instant has the
-- range of the t'UTCTime' instance.
instance ToField (Unbounded UTCTime) where
  toField value =
    timeValueBinding
      (show value)
      (LogicalTypeScalar LogicalTypeIdTimestampTz)
      (FieldTimestampTZ value)
      (\builder -> utcTimestampDuckValue (builderFactory builder) value)

instance ToField BigNum where
  toField big@(BigNum n) = valueBinding (show n) (\factory -> bigNumDuckValue factory big)

instance ToField (StructValue FieldValue) where
  toField structVal =
    temporalValueBinding "<struct>" (structValueTypeRep structVal) (FieldStruct structVal) (`structValueDuckValue` structVal)

instance ToField (TupleValue FieldValue) where
  toField tuple =
    temporalValueBinding "<tuple>" (tupleValueTypeRep tuple) (FieldTuple tuple) (`tupleValueDuckValue` tuple)

instance ToField (UnionValue FieldValue) where
  toField unionVal =
    let label = Text.unpack (unionValueLabel unionVal)
    in  temporalValueBinding ("<union " <> label <> ">") (unionValueTypeRep unionVal) (FieldUnion unionVal) (`unionValueDuckValue` unionVal)

instance DuckDBColumnType BitString where
  duckdbColumnTypeFor _ = "BIT"

instance ToField BS.ByteString where
  toField bs =
    valueBinding
      ("<blob length=" <> show (BS.length bs) <> ">")
      (\factory -> toDuckValue factory bs)

-- | Bind a 'SizedArray' as an ARRAY of size @n@. Each element takes the
-- element type of the parameter as its target. See Note [Sized arrays].
instance (DuckDBColumnType a, ToField a) => ToField (SizedArray n a) where
  toField values =
    targetValueBinding
      ("<array length=" <> show (length values) <> ">")
      \builder readTarget -> do
        target <- readTarget
        let childTarget = collectionChildType target
        arrayDuckValue builder childTarget (bindElement builder childTarget) (elems (sizedArrayToArray values))

-- | Bind a list as a LIST value with the element rules of a 'SizedArray'. An empty
-- list binds as an empty LIST of the element type. When the element type has
-- no type name, an empty list takes the element type of the parameter, or the
-- type NULL for a parameter with no known type, as the SQL literal [] does.
-- See Note [Element types of a list]. 'String' keeps its own instance. A list
-- as a row is a row of parameters, so bind a list parameter as @Only [..]@.
instance {-# OVERLAPPABLE #-} (DuckDBColumnType a, ToField a) => ToField [a] where
  toField values =
    targetValueBinding
      ("<list length=" <> show (length values) <> ">")
      \builder readTarget -> do
        target <- readTarget
        let childTarget = collectionChildType target
        listDuckValue builder childTarget (bindElement builder childTarget) values

-- | Bind a map as a MAP value. The pairs go in ascending key order. The keys
-- and the values have the element rules of a list, so an empty map binds as an
-- empty MAP of the key type and the value type, or with the type NULL for a key
-- or value type with no type name and no type from the parameter. DuckDB
-- compares two MAP values pair by pair in their order.
instance (DuckDBColumnType k, DuckDBColumnType v, ToField k, ToField v) => ToField (Map.Map k v) where
  toField pairs =
    targetValueBinding
      ("<map size=" <> show (Map.size pairs) <> ">")
      \builder readTarget -> do
        (keyTarget, valueTarget) <- mapPartTypes <$> readTarget
        mapDuckValue
          (withElementType (Proxy @k) builder keyTarget)
          (withElementType (Proxy @v) builder valueTarget)
          builder
          (bindElement builder keyTarget)
          (bindElement builder valueTarget)
          (Map.toAscList pairs)

-- | Bind a dynamic value, for example one that a query read as a t'FieldValue'.
-- Each scalar constructor binds with the instance of its payload type, so it
-- has the DuckDB type of the constructor. 'FieldHugeInt' and 'FieldUHugeInt'
-- bind as HUGEINT and UHUGEINT. 'FieldNull' binds as NULL. 'FieldStruct',
-- 'FieldTuple', and 'FieldUnion' bind with their own types. 'FieldGeometry'
-- binds as a GEOMETRY with its CRS.
--
-- 'FieldList', 'FieldArray', and 'FieldMap' read the parameter type, and take
-- the element type that their elements give. See
-- Note [Element types of a list].
--
-- Some values lose their type:
--
-- * A 'FieldTimestamp' binds as a TIMESTAMP, or as a TIMESTAMP_NS when it has
--   digits below a microsecond and the parameter has the type TIMESTAMP_NS or
--   VARIANT, whatever the unit of the column it came from. The same holds for
--   'FieldTime' and 'FieldTimestampTZ'. In a 'FieldList' or a 'FieldMap' for a
--   VARIANT parameter, such a value gets the microsecond value. See
--   Note [Nanosecond parameters].
-- * A 'FieldText' binds as a VARCHAR with no collation, also when it came from
--   a JSON value.
-- * A value that came from a VARIANT binds as its payload.
-- * A 'FieldEnum' binds its label as a VARCHAR. DuckDB casts the text to the
--   ENUM type of a column.
--
-- A 'FieldList', a 'FieldArray', or a 'FieldMap' that is empty or has only
-- 'FieldNull' elements takes the element type of the parameter, or the type
-- NULL for a parameter with no known type, as a SQL list literal does.
--
-- An empty 'FieldArray' raises an t'SQLError' of the type 'ErrorInputInvalid',
-- because a DuckDB ARRAY has at least one element. Inside a 'FieldStruct', a
-- 'FieldTuple', or a 'FieldUnion', a 'FieldEnum' binds its label to the
-- declared ENUM type.
--
-- t'FieldValue' has no 'DuckDBColumnType' and no 'ToDuckValue' instance,
-- because no one DuckDB type holds each of its values.
instance ToField FieldValue where
  toField value = case value of
    FieldNull -> nullBinding "NULL"
    FieldInt8 v -> toField v
    FieldInt16 v -> toField v
    FieldInt32 v -> toField v
    FieldInt64 v -> toField v
    FieldWord8 v -> toField v
    FieldWord16 v -> toField v
    FieldWord32 v -> toField v
    FieldWord64 v -> toField v
    FieldUUID v -> toField v
    FieldFloat v -> toField v
    FieldDouble v -> toField v
    FieldText v -> toField v
    FieldBool v -> toField v
    FieldBlob v -> toField v
    FieldGeometry v -> toField v
    FieldDate v -> toField v
    FieldTime v -> toField v
    FieldTimestamp v -> toField v
    FieldInterval v -> toField v
    FieldHugeInt v -> valueBinding (show v) (`hugeIntDuckValue` v)
    FieldUHugeInt v -> valueBinding (show v) (`uhugeIntDuckValue` v)
    FieldDecimal v -> toField v
    FieldTimestampTZ v -> toField v
    FieldTimeTZ v -> toField v
    FieldBit v -> toField v
    FieldBigNum v -> toField v
    FieldEnum _ label -> toField label
    FieldArray items
      | Just failure <- arraySizeError (length items) -> valueBinding (show value) (const failure)
      | otherwise -> fieldValueSequence "FieldArray" c_duckdb_v2_value_create_array (elems items)
    FieldList items -> fieldValueSequence "FieldList" c_duckdb_v2_value_create_list items
    FieldMap pairs -> fieldValueMap pairs
    FieldStruct v -> toField v
    FieldTuple v -> toField v
    FieldUnion v -> toField v

-- | Bind the elements of a 'FieldList' or a 'FieldArray' with the element type
-- that they give. See Note [Element types of a list].
fieldValueSequence
  :: Text
  -> ( DuckDBV2FactoryHandle
       -> DuckDBV2LogicalTypeHandle
       -> ConstPtr DuckDBV2ValueHandle
       -> DuckDBIdx
       -> Ptr DuckDBV2ValueHandle
       -> Ptr DuckDBV2ErrorInfoHandle
       -> IO DuckDBV2Error
     )
  -> [FieldValue]
  -> FieldBinding
fieldValueSequence name create items =
  targetValueBinding ("<" <> Text.unpack name <> " length=" <> show (length items) <> ">") \builder readTarget -> do
    childTarget <- collectionChildType <$> readTarget
    sequenceDuckValue (withValueType builder childTarget) create builder (bindElement builder childTarget) items

-- | Bind the pairs of a 'FieldMap' in their order, with the key type and the
-- value type that they give. See Note [Element types of a list].
fieldValueMap :: [(FieldValue, FieldValue)] -> FieldBinding
fieldValueMap pairs =
  targetValueBinding ("<FieldMap size=" <> show (length pairs) <> ">") \builder readTarget -> do
    (keyTarget, valueTarget) <- mapPartTypes <$> readTarget
    mapDuckValue
      (withValueType builder keyTarget)
      (withValueType builder valueTarget)
      builder
      (bindElement builder keyTarget)
      (bindElement builder valueTarget)
      pairs

instance (ToField a) => ToField (Maybe a) where
  toField Nothing = nullBinding "Nothing"
  toField (Just value) =
    let binding = toField value
    in  binding
          { fieldBindingDisplay = "Just " <> renderFieldBinding binding
          }

instance DuckDBColumnType G.Geometry where
  duckdbColumnTypeFor _ = "GEOMETRY"

instance DuckDBColumnType Variant where
  duckdbColumnTypeFor _ = "VARIANT"

instance DuckDBColumnType Null where
  duckdbColumnTypeFor _ = "NULL"

instance DuckDBColumnType Bool where
  duckdbColumnTypeFor _ = "BOOLEAN"

instance DuckDBColumnType Int where
  duckdbColumnTypeFor _ = "BIGINT"

instance DuckDBColumnType Int8 where
  duckdbColumnTypeFor _ = "TINYINT"

instance DuckDBColumnType Int16 where
  duckdbColumnTypeFor _ = "SMALLINT"

instance DuckDBColumnType Int32 where
  duckdbColumnTypeFor _ = "INTEGER"

instance DuckDBColumnType Int64 where
  duckdbColumnTypeFor _ = "BIGINT"

instance DuckDBColumnType BigNum where
  duckdbColumnTypeFor _ = "BIGNUM"

instance DuckDBColumnType UUID.UUID where
  duckdbColumnTypeFor _ = "UUID"

-- | The text is BIGNUM, the type that holds each value. A typed registration
-- declares HUGEINT. See Note [Integer parameters].
instance DuckDBColumnType Integer where
  duckdbColumnTypeFor _ = "BIGNUM"
  duckdbTypeDeclaration _ = Right (StructuredType (LogicalTypeScalar LogicalTypeIdHugeInt))

-- | The text is BIGNUM, the type that holds each value. A typed registration
-- declares UHUGEINT. See Note [Integer parameters].
instance DuckDBColumnType Natural where
  duckdbColumnTypeFor _ = "BIGNUM"
  duckdbTypeDeclaration _ = Right (StructuredType (LogicalTypeScalar LogicalTypeIdUHugeInt))

instance DuckDBColumnType Word where
  duckdbColumnTypeFor _ = "UBIGINT"

instance DuckDBColumnType Word8 where
  duckdbColumnTypeFor _ = "UTINYINT"

instance DuckDBColumnType Word16 where
  duckdbColumnTypeFor _ = "USMALLINT"

instance DuckDBColumnType Word32 where
  duckdbColumnTypeFor _ = "UINTEGER"

instance DuckDBColumnType Word64 where
  duckdbColumnTypeFor _ = "UBIGINT"

instance DuckDBColumnType Double where
  duckdbColumnTypeFor _ = "DOUBLE"

instance DuckDBColumnType Float where
  duckdbColumnTypeFor _ = "FLOAT"

instance DuckDBColumnType Text where
  duckdbColumnTypeFor _ = "VARCHAR"

instance DuckDBColumnType String where
  duckdbColumnTypeFor _ = "VARCHAR"

instance DuckDBColumnType BS.ByteString where
  duckdbColumnTypeFor _ = "BLOB"

instance DuckDBColumnType Day where
  duckdbColumnTypeFor _ = "DATE"

instance DuckDBColumnType TimeOfDay where
  duckdbColumnTypeFor _ = "TIME"

instance DuckDBColumnType LocalTime where
  duckdbColumnTypeFor _ = "TIMESTAMP"

instance DuckDBColumnType IntervalValue where
  duckdbColumnTypeFor _ = "INTERVAL"

instance DuckDBColumnType TimeWithZone where
  duckdbColumnTypeFor _ = "TIMETZ"

instance DuckDBColumnType UTCTime where
  duckdbColumnTypeFor _ = "TIMESTAMPTZ"

instance DuckDBColumnType (Unbounded Day) where
  duckdbColumnTypeFor _ = "DATE"

instance DuckDBColumnType (Unbounded LocalTime) where
  duckdbColumnTypeFor _ = "TIMESTAMP"

instance DuckDBColumnType (Unbounded UTCTime) where
  duckdbColumnTypeFor _ = "TIMESTAMPTZ"

-- | No declaration. See Note [Type declarations].
instance DuckDBColumnType (StructValue FieldValue) where
  duckdbColumnTypeFor _ = "STRUCT"
  duckdbTypeDeclaration _ = Left (noStaticFieldTypes "StructValue")

-- | No declaration. See Note [Type declarations].
instance DuckDBColumnType (TupleValue FieldValue) where
  duckdbColumnTypeFor _ = "TUPLE"
  duckdbTypeDeclaration _ = Left (noStaticFieldTypes "TupleValue")

-- | No declaration. See Note [Type declarations].
instance DuckDBColumnType (UnionValue FieldValue) where
  duckdbColumnTypeFor _ = "UNION"
  duckdbTypeDeclaration _ = Left (noStaticFieldTypes "UnionValue")

instance (DuckDBColumnType a) => DuckDBColumnType (Maybe a) where
  duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy @a)
  duckdbTypeDeclaration _ = duckdbTypeDeclaration (Proxy @a)

-- | The type text is @T[n]@, and the declaration is the ARRAY type of size @n@.
-- See Note [Sized arrays].
instance (KnownNat n, DuckDBColumnType a) => DuckDBColumnType (SizedArray n a) where
  duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy @a) <> Text.pack ("[" <> show (natVal (Proxy @n)) <> "]")
  duckdbTypeDeclaration _ = case arraySizeReason size of
    Just reason -> Left ("a SizedArray of size " <> Text.pack (show size) <> " has no ARRAY type, because " <> reason)
    Nothing -> arrayDeclaration size <$> duckdbTypeDeclaration (Proxy @a)
    where
      size = natVal (Proxy @n)

{- Note [Sized arrays]
~~~~~~~~~~~~~~~~~~~~~~
A DuckDB ARRAY type has a size, for example BIGINT[3]. SizedArray n a is the
Haskell type of T[n]: its type text is T[n], and a typed registration declares
the ARRAY type of size n. A SizedArray holds an Array Int a with the bounds
(0, n - 1). sizedArray takes an Array with any bounds and stores its elements
with these bounds, so the derived Eq and Ord compare the elements only. It is
the one Haskell type of an ARRAY. For an ARRAY
of a size that varies, a list reads an ARRAY of each size, and DuckDB casts a
bound list to an ARRAY column of the same length. A FieldArray is an ARRAY
value that a program builds at run time.

- Binds. A SizedArray binds as an ARRAY of size n. Each element takes the
  element type of the parameter as its target, and the element type of the
  value follows Note [Element types of a list].
- Reads. A SizedArray reads an ARRAY and a LIST, as a list does, and the
  value must have n elements. The cast of DuckDB from a LIST or an ARRAY to
  T[n] gives the same value, or an error for another length, so this is a
  read by the rule of Note [Reads of another SQL type] in
  "Database.DuckDB.Simple.FromField". The check uses the length of the
  decoded value and not the column type, so it is the same for a LIST column,
  an ARRAY column of another size, and a VARIANT. A value of another length
  raises Incompatible, which names the column type and the SizedArray type.
- Sizes. A DuckDB ARRAY has 1 to 100000 elements (maxArraySize). DuckDB
  rejects the SQL type T[0] and a larger type, for example with "ARRAY type
  size must be at most 100000". The value API of DuckDB 2.0.0 does not check
  the largest size, so a parameter of a larger ARRAY would bind a value that
  the SQL of DuckDB cannot write. Thus the library checks both sizes, as an
  input check: the declaration of a SizedArray of another size gives Left,
  and the bind of a SizedArray or a FieldArray of another size raises an
  SQLError of the type ErrorInputInvalid (arraySizeError).
- Fields. A SizedArray field of a generic record declares the ARRAY of size
  n through its DuckValue instance, and reads as a SizedArray does.
-}

-- | Declare an ARRAY of a size with the declaration of its element type. See
-- Note [Sized arrays].
arrayDeclaration :: Integer -> TypeDeclaration -> TypeDeclaration
arrayDeclaration size (TypeExpression element) = TypeExpression (element <> Text.pack ("[" <> show size <> "]"))
arrayDeclaration size (StructuredType element) = StructuredType (LogicalTypeArray element (fromInteger size))

-- | See Note [Type declarations].
instance (DuckDBColumnType k, DuckDBColumnType v) => DuckDBColumnType (Map.Map k v) where
  duckdbColumnTypeFor _ = Text.pack "MAP(" <> duckdbColumnTypeFor (Proxy @k) <> Text.pack ", " <> duckdbColumnTypeFor (Proxy @v) <> Text.pack ")"
  duckdbTypeDeclaration _ = do
    key <- duckdbTypeDeclaration (Proxy @k)
    value <- duckdbTypeDeclaration (Proxy @v)
    mapDeclaration key value

-- | 'String' keeps its own instance. See Note [Type declarations].
instance {-# OVERLAPPABLE #-} (DuckDBColumnType a) => DuckDBColumnType [a] where
  duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy @a) <> Text.pack "[]"
  duckdbTypeDeclaration _ = listDeclaration <$> duckdbTypeDeclaration (Proxy @a)

nullBinding :: String -> FieldBinding
nullBinding repr = valueBinding repr nullDuckValue

nullDuckValue :: DuckDBV2FactoryHandle -> IO DuckDBV2ValueHandle
nullDuckValue factory =
  bracket
    (logicalTypeFromRep factory (LogicalTypeScalar LogicalTypeIdSQLNull))
    destroyLogicalType
    (\logical -> output (c_duckdb_v2_value_create_null factory logical))

boolDuckValue :: DuckDBV2FactoryHandle -> Bool -> IO DuckDBV2ValueHandle
boolDuckValue factory value = output (c_duckdb_v2_value_create_bool factory (if value then 1 else 0))

int8DuckValue :: DuckDBV2FactoryHandle -> Int8 -> IO DuckDBV2ValueHandle
int8DuckValue factory value = output (c_duckdb_v2_value_create_tinyint factory value)

int16DuckValue :: DuckDBV2FactoryHandle -> Int16 -> IO DuckDBV2ValueHandle
int16DuckValue factory value = output (c_duckdb_v2_value_create_smallint factory value)

int32DuckValue :: DuckDBV2FactoryHandle -> Int32 -> IO DuckDBV2ValueHandle
int32DuckValue factory value = output (c_duckdb_v2_value_create_int factory value)

int64DuckValue :: DuckDBV2FactoryHandle -> Int64 -> IO DuckDBV2ValueHandle
int64DuckValue factory value = output (c_duckdb_v2_value_create_bigint factory value)

uint64DuckValue :: DuckDBV2FactoryHandle -> Word64 -> IO DuckDBV2ValueHandle
uint64DuckValue factory value = output (c_duckdb_v2_value_create_ubigint factory value)

uint32DuckValue :: DuckDBV2FactoryHandle -> Word32 -> IO DuckDBV2ValueHandle
uint32DuckValue factory value = output (c_duckdb_v2_value_create_uint factory value)

uint16DuckValue :: DuckDBV2FactoryHandle -> Word16 -> IO DuckDBV2ValueHandle
uint16DuckValue factory value = output (c_duckdb_v2_value_create_usmallint factory value)

uint8DuckValue :: DuckDBV2FactoryHandle -> Word8 -> IO DuckDBV2ValueHandle
uint8DuckValue factory value = output (c_duckdb_v2_value_create_utinyint factory value)

doubleDuckValue :: DuckDBV2FactoryHandle -> Double -> IO DuckDBV2ValueHandle
doubleDuckValue factory value = output (c_duckdb_v2_value_create_double factory (CDouble value))

floatDuckValue :: DuckDBV2FactoryHandle -> Float -> IO DuckDBV2ValueHandle
floatDuckValue factory value = output (c_duckdb_v2_value_create_float factory (CFloat value))

textDuckValue :: DuckDBV2FactoryHandle -> Text -> IO DuckDBV2ValueHandle
textDuckValue factory value = withText value (\text -> output (c_duckdb_v2_value_create_varchar factory text))

stringDuckValue :: DuckDBV2FactoryHandle -> String -> IO DuckDBV2ValueHandle
stringDuckValue factory = textDuckValue factory . Text.pack

blobDuckValue :: DuckDBV2FactoryHandle -> BS.ByteString -> IO DuckDBV2ValueHandle
blobDuckValue factory value = withBytes value (\bytes -> output (c_duckdb_v2_value_create_blob factory bytes))

uuidDuckValue :: DuckDBV2FactoryHandle -> UUID.UUID -> IO DuckDBV2ValueHandle
uuidDuckValue factory uuid =
  let (upper, lower) = UUID.toWords64 uuid
      value = DuckDBV2HugeintT lower (fromIntegral (upper `xor` 0x8000000000000000))
  in  with value (\ptr -> output (c_duckdb_v2_value_create_uuid factory (ConstPtr ptr)))

bitDuckValue :: DuckDBV2FactoryHandle -> BitString -> IO DuckDBV2ValueHandle
bitDuckValue factory (BitString padding bits) = do
  when (BS.null bits || padding > 7) $
    throwInvalidArgument "BIT requires nonempty data and padding from 0 to 7"
  let nativePadding = complement ((1 `shiftL` (8 - fromIntegral padding)) - 1) :: Word8
      payload = BS.cons padding (BS.cons (BS.head bits .|. nativePadding) (BS.tail bits))
  withBytes payload (\bytes -> output (c_duckdb_v2_value_create_bit factory bytes))

bigNumDuckValue :: DuckDBV2FactoryHandle -> BigNum -> IO DuckDBV2ValueHandle
bigNumDuckValue factory (BigNum value) =
  withBigEndianMagnitude (abs value) \raw len -> alloca \lengthSlot -> do
    let encode ptr capacity = checked (c_duckdb_v2_bignum_encode (ConstPtr (castPtr raw)) (fromIntegral len) (fromBool (value < 0)) ptr capacity lengthSlot)
    encode nullPtr 0
    size <- peek lengthSlot
    allocaBytes (fromIntegral size) \storage -> do
      encode storage size
      payload <- BS.packCStringLen (castPtr storage, fromIntegral size)
      withBytes payload (\encoded -> output (c_duckdb_v2_value_create_bignum factory encoded))

-- | Write the unsigned big-endian magnitude of a non-negative number into a
-- buffer in linear time, and give the buffer and its length to the action. The
-- magnitude has no leading zero byte. Zero is one zero byte.
withBigEndianMagnitude :: Integer -> (Ptr Word8 -> Int -> IO a) -> IO a
withBigEndianMagnitude 0 action = with 0 \buffer -> action buffer 1
withBigEndianMagnitude magnitude action = allocaBytes size \buffer@(Ptr addr) -> integerToAddr magnitude addr 1# >> action buffer size
  where
    size = fromIntegral (integerLog2 magnitude `quot` 8) + 1

dayDuckValue :: DuckDBV2FactoryHandle -> Day -> IO DuckDBV2ValueHandle
dayDuckValue factory value = encodeDay value >>= (\encoded -> output (c_duckdb_v2_value_create_date factory encoded))

timeOfDayDuckValue :: DuckDBV2FactoryHandle -> TimeOfDay -> IO DuckDBV2ValueHandle
timeOfDayDuckValue factory value = encodeTimeOfDay value >>= (\encoded -> output (c_duckdb_v2_value_create_time factory encoded))

localTimeDuckValue :: DuckDBV2FactoryHandle -> LocalTime -> IO DuckDBV2ValueHandle
localTimeDuckValue factory value = encodeLocalTime value >>= (\encoded -> output (c_duckdb_v2_value_create_timestamp factory encoded))

utcTimeDuckValue :: DuckDBV2FactoryHandle -> UTCTime -> IO DuckDBV2ValueHandle
utcTimeDuckValue factory = utcTimestampDuckValue factory . Finite

dateDuckValue :: DuckDBV2FactoryHandle -> Date -> IO DuckDBV2ValueHandle
dateDuckValue factory value = encodeUnbounded encodeDay value >>= (\encoded -> output (c_duckdb_v2_value_create_date factory encoded))

localTimestampDuckValue :: DuckDBV2FactoryHandle -> LocalTimestamp -> IO DuckDBV2ValueHandle
localTimestampDuckValue factory value = encodeUnbounded (encodeTimestampUnits 1000000) value >>= (\encoded -> output (c_duckdb_v2_value_create_timestamp factory encoded))

utcTimestampDuckValue :: DuckDBV2FactoryHandle -> UTCTimestamp -> IO DuckDBV2ValueHandle
utcTimestampDuckValue factory value =
  encodeUnbounded (encodeTimestampUnits 1000000 . utcToLocalTime utc) value
    >>= (\encoded -> output (c_duckdb_v2_value_create_timestamp_tz factory encoded))

-- | Construct an ARRAY value. An array with no element or with more elements
-- than DuckDB permits raises t'SQLError'. See 'withElementType' for the element
-- type.
arrayDuckValue
  :: forall a
   . (DuckDBColumnType a)
  => ValueBuilder
  -> Maybe LogicalTypeRep
  -> (a -> IO DuckDBV2ValueHandle)
  -> [a]
  -> IO DuckDBV2ValueHandle
arrayDuckValue builder target elementValue values = do
  sequence_ (arraySizeError (length values))
  sequenceDuckValue (withElementType (Proxy @a) builder target) c_duckdb_v2_value_create_array builder elementValue values

-- | Give the input error of an ARRAY parameter of a size that DuckDB does not
-- have: no element, or more elements than DuckDB permits. A 'SizedArray' and a
-- 'FieldArray' raise this error. See Note [Sized arrays].
arraySizeError :: Int -> Maybe (IO a)
arraySizeError count = case arraySizeReason (toInteger count) of
  Nothing -> Nothing
  Just reason ->
    Just
      ( throwInvalidArgument case count of
          0 -> "an empty ARRAY parameter is not possible, because " <> reason <> "; bind an empty list for an empty LIST"
          _ -> "an ARRAY parameter of " <> Text.pack (show count) <> " elements is not possible, because " <> reason <> "; bind a list for a LIST"
      )

-- | Construct a LIST value. An empty list is an empty LIST of the element type.
-- See 'withElementType' for the element type.
listDuckValue
  :: forall a
   . (DuckDBColumnType a)
  => ValueBuilder
  -> Maybe LogicalTypeRep
  -> (a -> IO DuckDBV2ValueHandle)
  -> [a]
  -> IO DuckDBV2ValueHandle
listDuckValue builder target = sequenceDuckValue (withElementType (Proxy @a) builder target) c_duckdb_v2_value_create_list builder

-- | Construct a LIST or an ARRAY value with the native constructor. The first
-- argument gives the element type of the values to its action:
-- 'withElementType' for a Haskell element type, or 'withValueType'.
sequenceDuckValue
  :: ([DuckDBV2ValueHandle] -> (DuckDBV2LogicalTypeHandle -> IO DuckDBV2ValueHandle) -> IO DuckDBV2ValueHandle)
  -> ( DuckDBV2FactoryHandle
       -> DuckDBV2LogicalTypeHandle
       -> ConstPtr DuckDBV2ValueHandle
       -> DuckDBIdx
       -> Ptr DuckDBV2ValueHandle
       -> Ptr DuckDBV2ErrorInfoHandle
       -> IO DuckDBV2Error
     )
  -> ValueBuilder
  -> (a -> IO DuckDBV2ValueHandle)
  -> [a]
  -> IO DuckDBV2ValueHandle
sequenceDuckValue withType create ValueBuilder{builderFactory = factory} elementValue elements =
  withCreatedValues (map elementValue elements) \values ->
    withType values \elementType ->
      withArray values \ptr ->
        output (create factory elementType (ConstPtr ptr) (fromIntegral (length values)))

-- | Construct a MAP value from pairs in their order. The first two arguments
-- give the key type and the value type, as for 'sequenceDuckValue'.
mapDuckValue
  :: ([DuckDBV2ValueHandle] -> (DuckDBV2LogicalTypeHandle -> IO DuckDBV2ValueHandle) -> IO DuckDBV2ValueHandle)
  -> ([DuckDBV2ValueHandle] -> (DuckDBV2LogicalTypeHandle -> IO DuckDBV2ValueHandle) -> IO DuckDBV2ValueHandle)
  -> ValueBuilder
  -> (k -> IO DuckDBV2ValueHandle)
  -> (v -> IO DuckDBV2ValueHandle)
  -> [(k, v)]
  -> IO DuckDBV2ValueHandle
mapDuckValue withKeyType withMapValueType ValueBuilder{builderFactory = factory} keyValue valueValue pairs =
  withCreatedValues (map (keyValue . fst) pairs) \keys ->
    withCreatedValues (map (valueValue . snd) pairs) \values ->
      withKeyType keys \keyType ->
        withMapValueType values \valueType ->
          withArray keys \keyPtr -> withArray values \valuePtr ->
            output (c_duckdb_v2_value_create_map factory keyType valueType (ConstPtr keyPtr) (ConstPtr valuePtr) (fromIntegral (length pairs)))

{- Note [Element types of a list]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
withElementType and withValueType give the element type of a LIST or an ARRAY
value, and the key type or the value type of a MAP value, to the native
constructor:

- A scalar element type comes from the column type name of the element. Some
  elements take the element type of the target in place of that type: an
  Integer, a Natural, or a BigNum element for an integer or DECIMAL target (see
  Note [Integer parameters]), and a TimeOfDay, a LocalTime, or a UTCTime
  element for a nanosecond target (see Note [Nanosecond parameters]). For each
  other target, a list of Integer, Natural, or BigNum elements that is not
  empty takes the type that its elements give, as the elements below do.
- An element type with no recognized scalar type name uses withValueType to
  infer its type from values. Examples are a STRUCT, a TUPLE, a UNION, a generic
  record, a list, an array, and a FieldValue. A complete structured declaration
  supplies the type only when no value contributes a type, as described below.
  A typed NULL contributes its type, including aliases and collations.
  Only a NULL of the unaliased SQLNULL type has no type to contribute. The
  filter compares each NULL type with one temporary unaliased SQLNULL type.
  Native equality distinguishes aliases, including the alias "NULL". When
  each remaining element has the type of the first one, that type is the
  element type. The check uses native equality to keep aliases,
  and base type trees to distinguish collations that native equality ignores.
- When the types are different, but timestampTypeForTarget gives each of them
  the same nanosecond type for the target, that type is the element type. For
  example, a FieldTime with digits below a microsecond binds as a TIME_NS for a
  TIME_NS[] parameter, and one with no such digits binds as a TIME. DuckDB has
  no common type for TIME and TIME_NS, or for TIMESTAMPTZ and TIMESTAMPTZ_NS.
  This check reads full type trees only when a promotion changes a base type.
  The promoted trees must also agree on aliases on unchanged leaves.
- When these types are different, the library names no element type. The
  native constructor then gives the type of a SQL list literal of the elements
  (duckdb_v2.h: "The child type is the common type of the elements, resolved by
  the same rule a SQL list literal follows, and every element is cast to it; a
  set with no common type surfaces the cast error."). For example, a STRUCT(a
  INTEGER) and a STRUCT(b INTEGER) give STRUCT(a INTEGER, b INTEGER), with NULL
  for a field that an element does not have. A BIGINT field and a DOUBLE field
  give a DOUBLE field, which rounds a large BIGINT, as in SQL.
- Elements with no common type raise the SQLError of DuckDB. For a STRUCT(a
  INTEGER) and a STRUCT(a VARCHAR), it is "Cannot combine types" of the type
  ErrorQueryNotImplemented. The SQL list literal raises a Binder Error for the
  same elements, but array_value raises the same error as the constructor.
- The target does not change the type of these elements. In SQL, DuckDB also
  gives the list literal its type before it casts the list to the type of a
  column.
- A list with no element or only untyped NULL elements of this kind takes the
  element type of the target, when the parameter has a known type. DuckDB casts
  the list literal [] or [NULL, NULL] to the type of a column in the same way.
- For a parameter with no known type, the element type of a list with no
  element or only untyped NULL elements is the structured declaration of the
  element type, when it has one, for example a record through ViaDuckDB (see
  Note [Type declarations]). The two rules below apply to each other element
  type.
- For a parameter with no known type, a list with only untyped NULL elements
  names no element type. The native constructor then gives the type of the SQL
  list literal: "NULL"[] for a LIST, "NULL"[2] for an ARRAY of two NULL elements,
  and MAP(VARCHAR, "NULL") for a MAP of VARCHAR keys and NULL values.
- For a parameter with no known type, an empty list or map of this kind gets
  the element type NULL. The SQL literals [] and MAP {} have the types
  "NULL"[] and MAP("NULL", "NULL"). The native constructor gives no type to an
  empty set (it raises "cannot resolve the element type of an empty set"), so
  the library names the type NULL itself. A SizedArray or a FieldArray with no
  element raises an error before this point, because a DuckDB ARRAY has at
  least one element.
-}

-- | Give the element type of the values of a LIST, an ARRAY, or one part of a
-- MAP to the action. The target is the element type of the parameter. See
-- Note [Element types of a list].
withElementType
  :: forall a b
   . (DuckDBColumnType a)
  => Proxy a
  -> ValueBuilder
  -> Maybe LogicalTypeRep
  -> [DuckDBV2ValueHandle]
  -> (DuckDBV2LogicalTypeHandle -> IO b)
  -> IO b
withElementType proxy builder@ValueBuilder{builderType = makeType} target values action = case duckDBTypeFromName typeName of
  -- See Note [Integer parameters].
  Just LogicalTypeIdBigNum | not integerTarget -> withValueType builder target values action
  Just dtype -> do
    checkNanosecondElements (declaredType dtype) values
    bracket (makeType (declaredType dtype)) destroyLogicalType action
  Nothing -> withValueType builder elementTarget values action
  where
    typeName = duckdbColumnType proxy
    -- With no target, the structured declaration of a record is the element
    -- type of a list with no element that is not NULL. See
    -- Note [Element types of a list].
    elementTarget = maybe (declaredTarget proxy) Just target
    -- See Note [Integer parameters].
    integerTarget = case target of
      Just (LogicalTypeScalar dtype) -> dtype `elem` integerTypes
      Just LogicalTypeDecimal{} -> True
      _ -> False
    declaredType LogicalTypeIdBigNum | integerTarget, Just rep <- target = rep
    declaredType dtype = timestampTypeForTarget target (LogicalTypeScalar dtype)

-- | Raise the range error of the library for each TIMESTAMP or TIMESTAMPTZ
-- element that a TIMESTAMP_NS or TIMESTAMPTZ_NS element type cannot hold, before
-- the native constructor casts the element. An infinity value and a NULL value
-- pass, as in the cast. See Note [Nanosecond parameters].
checkNanosecondElements :: LogicalTypeRep -> [DuckDBV2ValueHandle] -> IO ()
checkNanosecondElements declared values = case declared of
  LogicalTypeScalar LogicalTypeIdTimestampNs -> forM_ values (check LogicalTypeIdTimestamp c_duckdb_v2_value_get_timestamp)
  LogicalTypeScalar LogicalTypeIdTimestampTzNs -> forM_ values (check LogicalTypeIdTimestampTz c_duckdb_v2_value_get_timestamp_tz)
  _ -> pure ()
  where
    check source get value = do
      isNull <- output (c_duckdb_v2_value_is_null value)
      -- See Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
      rep <- bracket (output (c_duckdb_v2_value_get_logical_type value)) destroyLogicalType logicalTypeToBaseRep
      when (isNull == 0 && rep == LogicalTypeScalar source) do
        micros <- output (get value)
        unless (micros == maxBound || micros == negate maxBound) $
          checkFiniteRange "TIMESTAMP" (minBound :: Int64) maxBound (1000 * toInteger micros)

-- | Give the element type that the values give to the action. The target is
-- the element type of the parameter. See Note [Element types of a list].
withValueType :: ValueBuilder -> Maybe LogicalTypeRep -> [DuckDBV2ValueHandle] -> (DuckDBV2LogicalTypeHandle -> IO b) -> IO b
withValueType ValueBuilder{builderFactory = factory, builderType = makeType} target values action = do
  -- Keep each value except an untyped NULL, in list order. The fold keeps a
  -- constant stack. See Note [Constant stack at safe calls] in
  -- "Database.DuckDB.Simple.Native".
  let keep plainNull kept value =
        output (c_duckdb_v2_value_is_null value) >>= \flag ->
          if flag == 0
            then pure (value : kept)
            else do
              untyped <-
                bracket (output (c_duckdb_v2_value_get_logical_type value)) destroyLogicalType \logical ->
                  (/= 0) <$> output (c_duckdb_v2_logical_type_is_equal logical plainNull)
              pure (if untyped then kept else value : kept)
  present <-
    bracket (makeType (LogicalTypeScalar LogicalTypeIdSQLNull)) destroyLogicalType \plainNull ->
      reverse <$> foldM (keep plainNull) [] values
  case present of
    value : rest -> bracket (output (c_duckdb_v2_value_get_logical_type value)) destroyLogicalType \logical -> do
      -- See Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
      expected <- logicalTypeToBaseRep logical
      let promoted = timestampTypeForTarget target expected
          sameTypes (same, samePromoted, changed) element =
            bracket (output (c_duckdb_v2_value_get_logical_type element)) destroyLogicalType \elementType -> do
              actual <- logicalTypeToBaseRep elementType
              sameNative <-
                if same && actual == expected
                  then (/= 0) <$> output (c_duckdb_v2_logical_type_is_equal logical elementType)
                  else pure False
              let actualPromoted = timestampTypeForTarget target actual
                  samePromoted' = samePromoted && actualPromoted == promoted
                  changed' = changed || actualPromoted /= actual
              sameNative `seq` samePromoted' `seq` changed' `seq` pure (sameNative, samePromoted', changed')
          promotedType native = timestampTypeForTarget target <$> logicalTypeToRep factory native
      (same, samePromoted, changed) <- foldM sameTypes (True, True, promoted /= expected) rest
      case (same, samePromoted, changed) of
        (True, _, _) -> action logical
        (False, True, True) -> do
          -- Preserve aliases on leaves that the promotion does not change.
          -- See Note [Element types of a list].
          fullPromoted <- promotedType logical
          sameFull <-
            foldM
              ( \matches element ->
                  if matches
                    then
                      bracket
                        (output (c_duckdb_v2_value_get_logical_type element))
                        destroyLogicalType
                        (fmap (== fullPromoted) . promotedType)
                    else pure False
              )
              True
              rest
          if sameFull then bracket (makeType fullPromoted) destroyLogicalType action else action nullHandle
        _ -> action nullHandle
    -- See Note [Element types of a list].
    [] -> case target of
      Just rep -> bracket (makeType rep) destroyLogicalType action
      Nothing
        | not (null values) -> action nullHandle
        | otherwise -> bracket (makeType (LogicalTypeScalar LogicalTypeIdSQLNull)) destroyLogicalType action

-- | Construct named fields. The native call copies the field values.
structValueDuckValue
  :: ValueBuilder -> StructValue FieldValue -> IO DuckDBV2ValueHandle
structValueDuckValue builder@ValueBuilder{builderFactory = factory} StructValue{structValueFields, structValueTypes} = do
  let fields = elems structValueFields
      types = elems structValueTypes
      names = map structFieldName fields
  when (names /= map structFieldName types) $
    throwInvalidArgument "struct value/type fields mismatch"
  withCreatedValues
    (zipWith (\field logical -> fieldValueWithTypeDuckValue builder (structFieldValue logical) (structFieldValue field)) fields types)
    \values ->
      withIdentifiers names \namePtr -> withArray values \valuePtr ->
        declaredCompositeValue builder (LogicalTypeStruct structValueTypes) $
          output (c_duckdb_v2_value_create_struct factory (ConstPtr namePtr) (ConstPtr valuePtr) (fromIntegral (length values)))

-- | Construct positional fields without inferring a TUPLE from field names.
tupleValueDuckValue
  :: ValueBuilder -> TupleValue FieldValue -> IO DuckDBV2ValueHandle
tupleValueDuckValue builder@ValueBuilder{builderFactory = factory} TupleValue{tupleValueFields, tupleValueTypes} = do
  let fields = elems tupleValueFields
      types = elems tupleValueTypes
  when (length fields /= length types) $
    throwInvalidArgument "tuple value/type fields mismatch"
  withCreatedValues (zipWith (fieldValueWithTypeDuckValue builder) types fields) \values ->
    withArray values \valuePtr ->
      declaredCompositeValue builder (LogicalTypeTuple tupleValueTypes) $
        output (c_duckdb_v2_value_create_tuple factory (ConstPtr valuePtr) (fromIntegral (length values)))

-- | Preserve declared field types through an owned vector. Scalar VARCHAR
-- values do not retain collations. Most alias values use their base type,
-- but their enclosing composite type keeps both. See
-- Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
declaredCompositeValue
  :: ValueBuilder
  -> LogicalTypeRep
  -> IO DuckDBV2ValueHandle
  -> IO DuckDBV2ValueHandle
declaredCompositeValue _ rep create | not (needsDeclaredType rep) = create
declaredCompositeValue ValueBuilder{builderFactory = factory, builderType = makeType} rep create = bracket create destroyValue \value ->
  bracket (makeType rep) destroyLogicalType \logical -> withOwnedVector factory logical \vector -> do
    checked (c_duckdb_v2_vector_set_size vector 1)
    checked (c_duckdb_v2_vector_set_value vector 0 value)
    output (c_duckdb_v2_vector_get_value vector 0)

-- | Test whether a type tree contains a VARCHAR collation or an alias.
needsDeclaredType :: LogicalTypeRep -> Bool
needsDeclaredType = containsType declared
  where
    declared (LogicalTypeCollatedVarchar _) = True
    declared LogicalTypeAlias{} = True
    declared _ = False

{- Note [UNION values]
~~~~~~~~~~~~~~~~~~~~~~
unionValueDuckValue builds a UNION value through a vector with one row (route
(a) below). It writes the tag and the member into the child vectors, and reads
the row as a value. The header documents the layout and the write: the children
of a UNION vector are "[0] = the tag with [1..N] = the members", and a write of
a value to a vector uses "the non-strict default cast". The write needs FLAT
child vectors. In DuckDB 2.0.0 the children of a new vector are FLAT, but the
header does not say this. See Note [Vector flatten] in
"Database.DuckDB.Simple.Materialize".

V2 has no constructor for a UNION value. A run with DuckDB 2.0.0 tried four
routes in eight cases: two BIGINT members with the tag of the first member, and
with the tag of the second member; a NULL payload of two BIGINT members; 255
INTEGER members; a member that is a UNION with a member name of the outer UNION;
a STRUCT member with a UTINYINT field and a VARCHAR field with the name of the
member; a collated VARCHAR member; and a GEOMETRY('OGC:CRS84') member.

- (a) The vector route. It gives the correct value in each of the eight cases,
  and it runs no cast that a user registers.
- (b) The cast of a STRUCT with a UTINYINT tag field and one field for each
  member, written to a one-row vector of the UNION type with the default cast,
  as defaultCast does. It gives the correct value in each of the eight cases,
  and it runs no cast that a user registers. The header does not document this
  cast.
- (c) The cast of the member value to the UNION type. The header calls it "the
  sanctioned way to build UNION and ENUM values". DuckDB chooses the member by
  the type of the value. Thus the cast is ambiguous when two members have the
  same type, as in a generic sum type whose constructors have equal fields, and
  for a member that is a UNION with a member name of the outer UNION. It fails
  in the first five cases. With the factory of a connection or of a callback
  context, the cast also runs the casts that a user registers.
- (c1) The cast of the member value to a UNION with only that member, and then
  the cast of that UNION to the full UNION. It gives the correct value for two
  members of one type. It fails for a member that is a UNION with a member name
  of the outer UNION ("The cast is ambiguous"), and for the STRUCT member, which
  DuckDB reads as the STRUCT form of the UNION.

Thus (a) is correct in each case and uses the layout and the write that the
header documents. DuckDB does not document two
facts: that the children of a new vector are FLAT, which (a) needs, and the cast
of a STRUCT to a UNION, which (b) needs. If a later DuckDB gives a new vector
other children, (a) can write a wrong value or fail. These cases bind a UNION
value and compare the value that DuckDB gives back, so they show such a change:
"a UNION value binds the member of its tag up to the member limit of DuckDB" and
"UNION NULL payload retains its member type" in ValueRegressionTests.hs, and
"round-trips struct and union parameters" in Spec.hs.
-}

-- | Build one UNION row through its tag and member vectors. See
-- Note [UNION values].
unionValueDuckValue
  :: ValueBuilder -> UnionValue FieldValue -> IO DuckDBV2ValueHandle
unionValueDuckValue builder@ValueBuilder{builderFactory = factory, builderType = makeType} UnionValue{unionValueIndex, unionValueLabel, unionValuePayload, unionValueMembers} = do
  let members = elems unionValueMembers
      index = fromIntegral unionValueIndex
  when (index >= length members) $ throwInvalidArgument "union value tag out of range"
  let UnionMemberType name rep = members !! index
  when (name /= unionValueLabel) $ throwInvalidArgument "union tag and member name mismatch"
  bracket (makeType (LogicalTypeUnion unionValueMembers)) destroyLogicalType \logical ->
    withOwnedVector factory logical \vector -> do
      checked (c_duckdb_v2_vector_set_size vector 1)
      tag <- output (c_duckdb_v2_vector_get_child vector 0)
      checked (c_duckdb_v2_vector_set_size tag 1)
      bracket (uint8DuckValue factory (fromIntegral index)) destroyValue \value ->
        checked (c_duckdb_v2_vector_set_value tag 0 value)
      forM_ [0 .. length members - 1] \memberIndex -> do
        member <- output (c_duckdb_v2_vector_get_child vector (fromIntegral memberIndex + 1))
        checked (c_duckdb_v2_vector_set_size member 1)
        if memberIndex == index
          then bracket (fieldValueWithTypeDuckValue builder rep unionValuePayload) destroyValue \value ->
            checked (c_duckdb_v2_vector_set_value member 0 value)
          else checked (c_duckdb_v2_vector_set_null member 0)
      output (c_duckdb_v2_vector_get_value vector 0)

{- Note [TIME alias values]
~~~~~~~~~~~~~~~~~~~~~~~~~~
DuckDB 2.0.0 rejects the default cast from TIME to a TIME alias. The public
API specifies that an alias keeps its base representation. TIME vector data
uses int64 microseconds since midnight, as encodeTimeOfDay does. Thus a checked
TIME payload can be written through vector_get_data_mutable without a cast.
vector_get_value preserves the declared alias. The owned vector has one FLAT
row, and its validity is set explicitly. NULL uses value_create_null instead.
-}

-- | Construct a value with explicit logical metadata, including typed NULL.
fieldValueWithTypeDuckValue
  :: ValueBuilder -> LogicalTypeRep -> FieldValue -> IO DuckDBV2ValueHandle
fieldValueWithTypeDuckValue ValueBuilder{builderFactory = factory, builderType = makeType} rep FieldNull = bracket (makeType rep) destroyLogicalType \logical ->
  output (c_duckdb_v2_value_create_null factory logical)
fieldValueWithTypeDuckValue builder@ValueBuilder{builderFactory = factory, builderType = makeType} rep value = case (rep, value) of
  (LogicalTypeAlias _ base, FieldTime time)
    | withoutAliases base == LogicalTypeScalar LogicalTypeIdTime -> do
        -- See Note [TIME alias values].
        micros <- encodeTimeOfDay time
        bracket (makeType rep) destroyLogicalType \logical -> withOwnedVector factory logical \vector -> do
          checked (c_duckdb_v2_vector_flatten vector)
          checked (c_duckdb_v2_vector_set_size vector 1)
          raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
          poke (castPtr raw) micros
          validity <- output (c_duckdb_v2_vector_flat_get_validity_mutable vector)
          peek validity >>= poke validity . (.|. 1)
          output (c_duckdb_v2_vector_get_value vector 0)
  -- A value of the base type. DuckDB casts it to the alias type. See
  -- Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
  (LogicalTypeAlias _ base, _) -> fieldValueWithTypeDuckValue builder base value
  (LogicalTypeScalar LogicalTypeIdVariant, _) -> variantDuckValue builder value
  (LogicalTypeScalar dtype, _) -> scalarFieldValueDuckValue factory dtype value
  (LogicalTypeCollatedVarchar _, FieldText text) -> textDuckValue factory text
  (LogicalTypeGeometry crs, FieldGeometry raw) -> rawGeometryDuckValue builder crs raw
  (LogicalTypeDecimal width scale, FieldDecimal decimal@DecimalValue{decimalWidth, decimalScale})
    | width == decimalWidth && scale == decimalScale -> decimalDuckValue factory decimal
  (LogicalTypeList child, FieldList values) -> sequenceValue c_duckdb_v2_value_create_list child values
  (LogicalTypeArray child size, FieldArray array)
    | fromIntegral (length (elems array)) == size -> do
        sequence_ (arraySizeError (length (elems array)))
        sequenceValue c_duckdb_v2_value_create_array child (elems array)
  (LogicalTypeMap keyRep valueRep, FieldMap pairs) ->
    bracket (makeType keyRep) destroyLogicalType \keyType ->
      bracket (makeType valueRep) destroyLogicalType \valueType ->
        withCreatedValues (map (fieldValueWithTypeDuckValue builder keyRep . fst) pairs) \keys ->
          withCreatedValues (map (fieldValueWithTypeDuckValue builder valueRep . snd) pairs) \values ->
            withArray keys \keyPtr -> withArray values \valuePtr ->
              output (c_duckdb_v2_value_create_map factory keyType valueType (ConstPtr keyPtr) (ConstPtr valuePtr) (fromIntegral (length pairs)))
  (LogicalTypeStruct _, FieldStruct struct)
    | sameBase (structValueTypeRep struct) -> structValueDuckValue builder struct
  (LogicalTypeTuple _, FieldTuple tuple)
    | sameBase (tupleValueTypeRep tuple) -> tupleValueDuckValue builder tuple
  (LogicalTypeUnion _, FieldUnion union)
    | sameBase (unionValueTypeRep union) -> unionValueDuckValue builder union
  (LogicalTypeEnum entries, FieldEnum _ label) -> enumDuckValue factory entries label
  _ -> typeMismatch rep value
  where
    -- See Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
    sameBase other = withoutAliases rep == withoutAliases other
    sequenceValue create child values = bracket (makeType child) destroyLogicalType \logical ->
      withCreatedValues (map (fieldValueWithTypeDuckValue builder child) values) \native -> withArray native \ptr ->
        output (create factory logical (ConstPtr ptr) (fromIntegral (length native)))

scalarFieldValueDuckValue :: DuckDBV2FactoryHandle -> LogicalTypeId -> FieldValue -> IO DuckDBV2ValueHandle
scalarFieldValueDuckValue factory dtype value = case (dtype, value) of
  (LogicalTypeIdBoolean, FieldBool b) -> boolDuckValue factory b
  (LogicalTypeIdTinyInt, FieldInt8 i) -> int8DuckValue factory i
  (LogicalTypeIdSmallInt, FieldInt16 i) -> int16DuckValue factory i
  (LogicalTypeIdInteger, FieldInt32 i) -> int32DuckValue factory i
  (LogicalTypeIdBigInt, FieldInt64 i) -> int64DuckValue factory i
  (LogicalTypeIdUTinyInt, FieldWord8 w) -> uint8DuckValue factory w
  (LogicalTypeIdUSmallInt, FieldWord16 w) -> uint16DuckValue factory w
  (LogicalTypeIdUInteger, FieldWord32 w) -> uint32DuckValue factory w
  (LogicalTypeIdUBigInt, FieldWord64 w) -> uint64DuckValue factory w
  (LogicalTypeIdFloat, FieldFloat f) -> floatDuckValue factory f
  (LogicalTypeIdDouble, FieldDouble d) -> doubleDuckValue factory d
  (LogicalTypeIdVarchar, FieldText t) -> textDuckValue factory t
  (LogicalTypeIdBlob, FieldBlob b) -> blobDuckValue factory b
  -- The type id has no CRS. See Note [Raw GEOMETRY parameters].
  (LogicalTypeIdGeometry, FieldGeometry raw) -> rawGeometryDuckValue (plainBuilder factory) Nothing raw
  (LogicalTypeIdUUID, FieldUUID u) -> uuidDuckValue factory u
  (LogicalTypeIdBit, FieldBit bits) -> bitDuckValue factory bits
  (LogicalTypeIdDate, FieldDate d) -> dateDuckValue factory d
  (LogicalTypeIdTime, FieldTime t) -> timeOfDayDuckValue factory t
  (LogicalTypeIdTimeNs, FieldTime t) ->
    timeOfDayUnits 1000000000 t >>= (\encoded -> output (c_duckdb_v2_value_create_time_ns factory encoded)) . fromInteger
  (LogicalTypeIdTimeTz, FieldTimeTZ tz) -> timeWithZoneDuckValue factory tz
  (LogicalTypeIdTimestamp, FieldTimestamp ts) -> localTimestampDuckValue factory ts
  (LogicalTypeIdTimestampSec, FieldTimestamp ts) ->
    encodeUnbounded (encodeTimestampUnits 1) ts >>= (\encoded -> output (c_duckdb_v2_value_create_timestamp_sec factory encoded))
  (LogicalTypeIdTimestampMs, FieldTimestamp ts) ->
    encodeUnbounded (encodeTimestampUnits 1000) ts >>= (\encoded -> output (c_duckdb_v2_value_create_timestamp_ms factory encoded))
  (LogicalTypeIdTimestampNs, FieldTimestamp ts) ->
    encodeUnbounded (encodeTimestampUnits 1000000000) ts >>= (\encoded -> output (c_duckdb_v2_value_create_timestamp_ns factory encoded))
  (LogicalTypeIdTimestampTz, FieldTimestampTZ ts) -> utcTimestampDuckValue factory ts
  (LogicalTypeIdTimestampTzNs, FieldTimestampTZ ts) -> utcTimestampNsDuckValue factory ts
  (LogicalTypeIdInterval, FieldInterval iv) -> intervalDuckValue factory iv
  (LogicalTypeIdHugeInt, FieldHugeInt i) -> hugeIntDuckValue factory i
  (LogicalTypeIdUHugeInt, FieldUHugeInt i) -> uhugeIntDuckValue factory i
  (LogicalTypeIdBigNum, FieldBigNum big) -> bigNumDuckValue factory big
  _ ->
    throwInvalidArgument
      ("unsupported scalar conversion for " <> logicalTypeSQL (LogicalTypeScalar dtype) <> " from a value of type " <> valueTypeName value)

-- | Construct an ENUM value by the default cast of the label to the ENUM type.
-- The label of a 'FieldEnum' gives the value, not its index: an index of
-- another dictionary would give another label with no error. A label that is
-- not in the dictionary raises an t'SQLError' of the type 'ErrorTypeConversion',
-- as the cast of DuckDB from VARCHAR does. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
enumDuckValue :: DuckDBV2FactoryHandle -> Array Int Text -> Text -> IO DuckDBV2ValueHandle
enumDuckValue factory entries label = do
  unless (label `elem` elems entries) $
    throwConversionError ("the ENUM label '" <> label <> "' is not in the dictionary of the type")
  bracket (logicalTypeFromRep factory (LogicalTypeEnum entries)) destroyLogicalType \logical ->
    bracket (textDuckValue factory label) destroyValue \value ->
      defaultCast factory value logical

-- | Cast a value to a type with the default cast of DuckDB
-- ('withDefaultCastVector' with one row). The caller destroys the result. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
defaultCast :: DuckDBV2FactoryHandle -> DuckDBV2ValueHandle -> DuckDBV2LogicalTypeHandle -> IO DuckDBV2ValueHandle
defaultCast factory value logical =
  withDefaultCastVector factory logical 1 (\_ write -> write value) \vector -> output (c_duckdb_v2_vector_get_value vector 0)

hugeIntDuckValue :: DuckDBV2FactoryHandle -> Integer -> IO DuckDBV2ValueHandle
hugeIntDuckValue factory value = integerToHugeInt value >>= \huge -> with huge (\ptr -> output (c_duckdb_v2_value_create_hugeint factory (ConstPtr ptr)))

uhugeIntDuckValue :: DuckDBV2FactoryHandle -> Integer -> IO DuckDBV2ValueHandle
uhugeIntDuckValue factory value = integerToUHugeInt value >>= \huge -> with huge (\ptr -> output (c_duckdb_v2_value_create_uhugeint factory (ConstPtr ptr)))

decimalDuckValue :: DuckDBV2FactoryHandle -> DecimalValue -> IO DuckDBV2ValueHandle
decimalDuckValue factory DecimalValue{decimalWidth, decimalScale, decimalInteger} = do
  when (decimalWidth < 1 || decimalWidth > 38 || decimalScale > decimalWidth) $
    throwInvalidArgument "invalid DECIMAL width or scale"
  when (abs decimalInteger >= 10 ^ decimalWidth) $ throwConversionError "DECIMAL value exceeds declared precision"
  huge <- integerToHugeInt decimalInteger
  with huge (\ptr -> output (c_duckdb_v2_value_create_decimal factory (ConstPtr ptr) decimalWidth decimalScale))

intervalDuckValue :: DuckDBV2FactoryHandle -> IntervalValue -> IO DuckDBV2ValueHandle
intervalDuckValue factory IntervalValue{intervalMonths, intervalDays, intervalMicros} =
  with
    (DuckDBV2IntervalT intervalMonths intervalDays intervalMicros)
    (\ptr -> output (c_duckdb_v2_value_create_interval factory (ConstPtr ptr)))

timeWithZoneDuckValue :: DuckDBV2FactoryHandle -> TimeWithZone -> IO DuckDBV2ValueHandle
timeWithZoneDuckValue factory TimeWithZone{timeWithZoneTime, timeWithZoneOffsetSeconds} = do
  micros <- timeOfDayUnits 1000000 timeWithZoneTime
  let offset = toInteger timeWithZoneOffsetSeconds
  when (abs offset > toInteger timeTzMaxOffsetSeconds) $
    throwConversionError
      ("the TIMETZ offset " <> Text.pack (show offset) <> " seconds is out of range; DuckDB stores an offset from -57599 to 57599 seconds")
  output
    ( c_duckdb_v2_value_create_time_tz
        factory
        ((fromIntegral micros `shiftL` timeTzOffsetBits) .|. fromIntegral (toInteger timeTzMaxOffsetSeconds - offset))
    )

integerToHugeInt :: Integer -> IO DuckDBV2HugeintT
integerToHugeInt value = do
  when (value < negate (1 `shiftL` 127) || value >= 1 `shiftL` 127) $ throwConversionError "HUGEINT value out of range"
  pure (DuckDBV2HugeintT (fromIntegral value) (fromIntegral (value `shiftR` 64)))

integerToUHugeInt :: Integer -> IO DuckDBV2UhugeintT
integerToUHugeInt value = do
  when (value < 0 || value >= 1 `shiftL` 128) $ throwConversionError "UHUGEINT value out of range"
  pure (DuckDBV2UhugeintT (fromIntegral value) (fromIntegral (value `shiftR` 64)))

-- | Borrow generated identifier views and reject embedded NUL bytes.
withIdentifiers :: [Text] -> (Ptr DuckDBV2IdentifierT -> IO a) -> IO a
withIdentifiers names action = do
  when (any (Text.any (== '\0')) names) $
    throwInvalidArgument "logical type name contains NUL"
  withIdentifierArray names action

-- | Raise the error of a value that does not fit its declared type. The text
-- names the type as DuckDB does. See Note [Type texts] in
-- "Database.DuckDB.Simple.LogicalRep".
typeMismatch :: LogicalTypeRep -> FieldValue -> IO a
typeMismatch expected actual = throwInvalidArgument ("cannot encode a value of type " <> valueTypeName actual <> " as " <> logicalTypeSQL expected)

-- | Construct a GEOMETRY value of the declared CRS from a raw geometry. See
-- Note [Raw GEOMETRY parameters], and Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
rawGeometryDuckValue :: ValueBuilder -> Maybe Text -> RawGeometry -> IO DuckDBV2ValueHandle
rawGeometryDuckValue ValueBuilder{builderFactory = factory, builderType = makeType} crs RawGeometry{rawGeometryWKB, rawGeometryCRS} = do
  when (rawGeometryCRS /= crs) $
    throwInvalidArgument ("a GEOMETRY value with " <> describeCRS rawGeometryCRS <> " cannot bind as a GEOMETRY with " <> describeCRS crs)
  bracket (makeType (LogicalTypeGeometry crs)) destroyLogicalType \logical ->
    bracket (blobDuckValue factory rawGeometryWKB) destroyValue \blob ->
      defaultCast factory blob logical
  where
    describeCRS = maybe "no CRS" (("the CRS " <>) . quotedCRS)

-- | Put a CRS between single quotes for an error message.
quotedCRS :: Text -> Text
quotedCRS crs = "'" <> crs <> "'"

{- Note [Raw GEOMETRY parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A raw geometry is a RawGeometry: WKB bytes and an optional CRS. A result
gives a raw geometry for a GEOMETRY value, and a FieldGeometry value for a
GEOMETRY value in a STRUCT, a TUPLE, a UNION, a LIST, an ARRAY, a MAP, or a
VARIANT. A user can also construct a raw geometry.

A top-level RawGeometry parameter (ToField RawGeometry) binds as a GEOMETRY
value with the CRS of the raw geometry. It uses rawGeometryDuckValue with
that CRS as the declared CRS, so the CRS check below always passes. DuckDB
then casts the value to the type of the parameter, as it casts a GEOMETRY
value in SQL text:

- A value without a CRS takes the CRS of a target that has a CRS.
- A target without a CRS removes the CRS of the value. No error occurs.
- A target with another CRS raises "Binder Error: Cannot cast GEOMETRY with
  CRS ... to GEOMETRY with different CRS ...". A comparison with = has the
  same rule.

RawGeometry has no DuckDBColumnType instance and no ToDuckValue instance.
Thus a SizedArray or a list of raw geometries and a typed row cannot hold one. Their
declared element type or column type would be GEOMETRY without a CRS, and
DuckDB would remove the CRS of each value.

A STRUCT, TUPLE, UNION, or Variant parameter binds each raw geometry that it
contains with this rule:

- The CRS of the value must be equal to the CRS of the declared GEOMETRY
  type. A value without a CRS agrees only with a type without a CRS. If
  the two are different, rawGeometryDuckValue raises an SQLError with the
  type ErrorInputInvalid.
- rawGeometryDuckValue makes a BLOB value from the WKB bytes. Then it casts
  the BLOB value to the declared GEOMETRY type with the default cast of
  DuckDB (defaultCast).
  The builder of the statement supplies a type with a configured CRS. See
  Note [CRS types of a connection] in "Database.DuckDB.Simple.TypeCache".
- A declared type that is only the type id GEOMETRY has no CRS. The binding
  uses the plain builder of the factory for it.
- A VARIANT accepts a raw geometry without a CRS. The payload is a GEOMETRY
  without a CRS. A raw geometry with a CRS in a VARIANT raises an SQLError
  with the type ErrorInputInvalid.
- The factory of an instance cannot construct a GEOMETRY type with a CRS.
  DuckDB raises "Binder Error: Cannot create GEOMETRY type with coordinate
  system without a connection". Thus toDuckValue with the factory of an
  instance raises this error for a raw geometry with a CRS. It also raises
  this error for a typed NULL with a CRS. The factory of a connection does
  not have this limit.

- Why the bytes do not change. DuckDB 2.0.0 stores a GEOMETRY value as WKB.
  The cast from BLOB to GEOMETRY reads the bytes with Geometry::FromBinary.
  ST_GeomFromWKB uses the same function (src/function/cast/geo_casts.cpp and
  src/function/scalar/geometry/geometry_functions.cpp). The cast from
  GEOMETRY to BLOB copies the stored bytes (Geometry::ToBinary in
  src/common/types/geometry.cpp). The result reader uses that cast
  (prepareGeometryReader in "Database.DuckDB.Simple.Materialize"). Thus the
  bytes of a result bind again with no change. The cast changes bytes that
  DuckDB does not write: it writes big-endian WKB and EWKB as little-endian
  ISO WKB. The cast rejects WKB that DuckDB cannot store, for example a
  collection with members of different Z/M layouts.

- Why a CRS mismatch is an error. A BLOB value has no CRS. The cast gives
  the CRS of the declared type to the value. Without the check, a value
  could get a different CRS or lose its CRS, and no error would occur.

- Why a top-level parameter has no such check. Its declared type is the type
  of its own CRS. The casts after the bind are the casts of SQL text, and the
  library adds no rule to them.

- Why a VARIANT accepts only a value without a CRS. A VARIANT keeps a
  GEOMETRY without its CRS. DuckDB removes the CRS when it casts a GEOMETRY
  with a CRS to VARIANT, and it gives no error.
-}

-- | Cast the payload to VARIANT with the default cast of DuckDB. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
variantDuckValue :: ValueBuilder -> FieldValue -> IO DuckDBV2ValueHandle
variantDuckValue builder@ValueBuilder{builderFactory = factory, builderType = makeType} value = do
  (rep, payload) <- variantPayloadType value
  bracket (makeType (LogicalTypeScalar LogicalTypeIdVariant)) destroyLogicalType \logical ->
    bracket (fieldValueWithTypeDuckValue builder rep payload) destroyValue \native ->
      defaultCast factory native logical

-- | Construct a UTC nanosecond value without losing sub-microsecond digits.
utcTimestampNsDuckValue :: DuckDBV2FactoryHandle -> UTCTimestamp -> IO DuckDBV2ValueHandle
utcTimestampNsDuckValue factory value =
  encodeUnbounded (encodeTimestampUnits 1000000000 . utcToLocalTime utc) value
    >>= (\encoded -> output (c_duckdb_v2_value_create_timestamp_tz_ns factory encoded))

-- | Cast the WKT text of the shape to GEOMETRY with the default cast of DuckDB.
-- See Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
instance ToDuckValue G.Geometry where
  toDuckValue factory geometry = do
    wkt <- either (throwInvalidArgument . Text.pack) pure (WKT.encodeWKT geometry)
    bracket (logicalTypeFromRep factory (LogicalTypeGeometry Nothing)) destroyLogicalType \logical ->
      bracket (textDuckValue factory wkt) destroyValue \value ->
        defaultCast factory value logical

instance ToDuckValue Variant where
  toDuckValue factory = variantDuckValue (plainBuilder factory) . variantPayload

-- | Choose the native type of a VARIANT payload. Containers get VARIANT
-- elements and fields. A time value with digits below a microsecond uses the
-- nanosecond type when it fits the range of that type (fitsNanoseconds). Each
-- other time value uses the microsecond type, as a VARIANT parameter does. See
-- Note [Nanosecond parameters].
-- The C API cannot construct a nonempty object whose keys are all empty.
-- DuckDB 2.0 treats that STRUCT type as a tuple and casts it to a VARIANT array.
-- A raw geometry without a CRS uses GEOMETRY. See Note [Raw GEOMETRY parameters].
-- A VARIANT does not keep a UNION, a MAP, an ENUM, or the CRS of a geometry, so
-- each of these payloads raises an error, at each depth.
variantPayloadType :: FieldValue -> IO (LogicalTypeRep, FieldValue)
variantPayloadType value = case value of
  FieldNull -> pure (variant, value)
  FieldBool{} -> scalar LogicalTypeIdBoolean
  FieldInt8{} -> scalar LogicalTypeIdTinyInt
  FieldInt16{} -> scalar LogicalTypeIdSmallInt
  FieldInt32{} -> scalar LogicalTypeIdInteger
  FieldInt64{} -> scalar LogicalTypeIdBigInt
  FieldWord8{} -> scalar LogicalTypeIdUTinyInt
  FieldWord16{} -> scalar LogicalTypeIdUSmallInt
  FieldWord32{} -> scalar LogicalTypeIdUInteger
  FieldWord64{} -> scalar LogicalTypeIdUBigInt
  FieldHugeInt{} -> scalar LogicalTypeIdHugeInt
  FieldUHugeInt{} -> scalar LogicalTypeIdUHugeInt
  FieldFloat{} -> scalar LogicalTypeIdFloat
  FieldDouble{} -> scalar LogicalTypeIdDouble
  FieldDecimal DecimalValue{decimalWidth, decimalScale} -> pure (LogicalTypeDecimal decimalWidth decimalScale, value)
  FieldText{} -> scalar LogicalTypeIdVarchar
  FieldBlob{} -> scalar LogicalTypeIdBlob
  FieldUUID{} -> scalar LogicalTypeIdUUID
  FieldDate{} -> scalar LogicalTypeIdDate
  FieldTime time
    | hasSubMicroseconds time -> scalar LogicalTypeIdTimeNs
    | otherwise -> scalar LogicalTypeIdTime
  FieldTimestamp (Finite LocalTime{localTimeOfDay})
    | hasSubMicroseconds localTimeOfDay && fitsNanoseconds value -> scalar LogicalTypeIdTimestampNs
  FieldTimestamp{} -> scalar LogicalTypeIdTimestamp
  FieldTimestampTZ (Finite instant)
    | hasSubMicroseconds (localTimeOfDay (utcToLocalTime utc instant)) && fitsNanoseconds value -> scalar LogicalTypeIdTimestampTzNs
  FieldTimestampTZ{} -> scalar LogicalTypeIdTimestampTz
  FieldTimeTZ{} -> scalar LogicalTypeIdTimeTz
  FieldInterval{} -> scalar LogicalTypeIdInterval
  FieldBigNum{} -> scalar LogicalTypeIdBigNum
  FieldBit{} -> scalar LogicalTypeIdBit
  FieldList{} -> pure (LogicalTypeList variant, value)
  FieldArray items -> pure (LogicalTypeList variant, FieldList (elems items))
  FieldTuple TupleValue{tupleValueFields} -> pure (LogicalTypeList variant, FieldList (elems tupleValueFields))
  FieldStruct structValue@StructValue{structValueTypes} -> do
    let names = map structFieldName (elems structValueTypes)
    when (Set.size (Set.fromList names) /= length names) $
      throwInvalidArgument "VARIANT objects need unique keys"
    when (names == [Text.empty]) $
      throwInvalidArgument "DuckDB's C API cannot construct a VARIANT object with only an empty key"
    let types = fmap (\field -> field{structFieldValue = variant}) structValueTypes
    pure (LogicalTypeStruct types, FieldStruct structValue{structValueTypes = types})
  FieldUnion{} -> throwInvalidArgument "a VARIANT keeps only the member value of a UNION, so a UNION value cannot bind as a VARIANT"
  FieldGeometry RawGeometry{rawGeometryCRS = Just crs} ->
    throwInvalidArgument
      ("a VARIANT does not keep the CRS of a GEOMETRY, so the GEOMETRY value with the CRS " <> quotedCRS crs <> " cannot bind as a VARIANT")
  FieldGeometry{} -> pure (LogicalTypeGeometry Nothing, value)
  FieldMap{} -> throwInvalidArgument "a VARIANT stores a MAP as a list of key and value objects, so a MAP value cannot bind as a VARIANT"
  FieldEnum{} -> throwInvalidArgument "a VARIANT stores an ENUM as its text, so an ENUM value cannot bind as a VARIANT"
  where
    variant = LogicalTypeScalar LogicalTypeIdVariant
    scalar dtype = pure (LogicalTypeScalar dtype, value)

instance ToDuckValue Null where
  toDuckValue factory _ = nullDuckValue factory

instance ToDuckValue Bool where
  toDuckValue = boolDuckValue

instance ToDuckValue Int where
  toDuckValue factory = int64DuckValue factory . fromIntegral

instance ToDuckValue Int8 where
  toDuckValue = int8DuckValue

instance ToDuckValue Int16 where
  toDuckValue = int16DuckValue

instance ToDuckValue Int32 where
  toDuckValue = int32DuckValue

instance ToDuckValue Int64 where
  toDuckValue = int64DuckValue

instance ToDuckValue BigNum where
  toDuckValue = bigNumDuckValue

instance ToDuckValue UUID.UUID where
  toDuckValue = uuidDuckValue

-- | A value from -2^127 to 2^127 - 1 gives a HUGEINT value. Each other value
-- raises an error. See Note [Integer parameters].
instance ToDuckValue Integer where
  toDuckValue factory value = case hugeIntInRange value of
    Just make -> make factory
    Nothing -> outsideRange "HUGEINT" value

-- | A value from 0 to 2^127 - 1 gives a HUGEINT value, and a value up to
-- 2^128 - 1 a UHUGEINT value. Each larger value raises an error. See
-- Note [Integer parameters].
instance ToDuckValue Natural where
  toDuckValue factory value = case hugeIntInRange (toInteger value) of
    Just make -> make factory
    Nothing
      | toInteger value < 1 `shiftL` 128 -> uhugeIntDuckValue factory (toInteger value)
      | otherwise -> outsideRange "UHUGEINT" (toInteger value)

-- | Raise the range error of 'toDuckValue' for an 'Integer' or a 'Natural'
-- outside the declared type. See Note [Integer parameters].
outsideRange :: Text -> Integer -> IO a
outsideRange typeName value =
  throwConversionError
    ("the value " <> Text.pack (show value) <> " is outside the range of " <> typeName <> ". Use BigNum for a larger value.")

instance ToDuckValue Word where
  toDuckValue factory = uint64DuckValue factory . fromIntegral

instance ToDuckValue Word16 where
  toDuckValue = uint16DuckValue

instance ToDuckValue Word32 where
  toDuckValue = uint32DuckValue

instance ToDuckValue Word64 where
  toDuckValue = uint64DuckValue

instance ToDuckValue Word8 where
  toDuckValue = uint8DuckValue

instance ToDuckValue Double where
  toDuckValue = doubleDuckValue

instance ToDuckValue Float where
  toDuckValue = floatDuckValue

instance ToDuckValue Text where
  toDuckValue = textDuckValue

instance ToDuckValue String where
  toDuckValue = stringDuckValue

instance ToDuckValue BS.ByteString where
  toDuckValue = blobDuckValue

instance ToDuckValue BitString where
  toDuckValue = bitDuckValue

instance ToDuckValue Day where
  toDuckValue = dayDuckValue

instance ToDuckValue TimeOfDay where
  toDuckValue = timeOfDayDuckValue

instance ToDuckValue LocalTime where
  toDuckValue = localTimeDuckValue

instance ToDuckValue IntervalValue where
  toDuckValue = intervalDuckValue

instance ToDuckValue TimeWithZone where
  toDuckValue = timeWithZoneDuckValue

instance ToDuckValue UTCTime where
  toDuckValue = utcTimeDuckValue

instance ToDuckValue (Unbounded Day) where
  toDuckValue = dateDuckValue

instance ToDuckValue (Unbounded LocalTime) where
  toDuckValue = localTimestampDuckValue

instance ToDuckValue (Unbounded UTCTime) where
  toDuckValue = utcTimestampDuckValue

instance ToDuckValue (StructValue FieldValue) where
  toDuckValue = structValueDuckValue . plainBuilder

instance ToDuckValue (TupleValue FieldValue) where
  toDuckValue = tupleValueDuckValue . plainBuilder

instance ToDuckValue (UnionValue FieldValue) where
  toDuckValue = unionValueDuckValue . plainBuilder

-- | Construct a MAP through the supplied factory with the key type of @k@ and
-- the value type of @v@. The pairs go in ascending key order. A structured
-- declaration of a part is its target. See Note [Type declarations].
instance (DuckDBColumnType k, DuckDBColumnType v, ToDuckValue k, ToDuckValue v) => ToDuckValue (Map.Map k v) where
  toDuckValue factory =
    mapDuckValue
      (withElementType (Proxy @k) builder (declaredTarget (Proxy @k)))
      (withElementType (Proxy @v) builder (declaredTarget (Proxy @v)))
      builder
      (toDuckValue factory)
      (toDuckValue factory)
      . Map.toAscList
    where
      builder = plainBuilder factory

-- | Construct an ARRAY of size @n@ through the supplied factory with the
-- element type of @a@. A structured declaration of @a@ is the target of the
-- elements. See Note [Sized arrays].
instance (KnownNat n, DuckDBColumnType a, ToDuckValue a) => ToDuckValue (SizedArray n a) where
  toDuckValue factory = arrayDuckValue (plainBuilder factory) (declaredTarget (Proxy @a)) (toDuckValue factory) . elems . sizedArrayToArray

-- | Construct a LIST through the supplied factory with the element type of @a@.
-- 'String' keeps its own instance. A structured declaration of @a@ is the
-- target of the elements. See Note [Type declarations].
instance {-# OVERLAPPABLE #-} (DuckDBColumnType a, ToDuckValue a) => ToDuckValue [a] where
  toDuckValue factory = listDuckValue (plainBuilder factory) (declaredTarget (Proxy @a)) (toDuckValue factory)

instance (ToDuckValue a) => ToDuckValue (Maybe a) where
  toDuckValue factory Nothing = nullDuckValue factory
  toDuckValue factory (Just value) = toDuckValue factory value

-- | Preserve infinity sentinels and validate finite values before narrowing.
encodeUnbounded :: (Integral b, Bounded b) => (a -> IO b) -> Unbounded a -> IO b
encodeUnbounded _ NegInfinity = pure (negate maxBound)
encodeUnbounded _ PosInfinity = pure maxBound
encodeUnbounded encode (Finite value) = encode value

-- | Encode finite dates as days from the Unix epoch. A day outside the range
-- of DuckDB raises an error. See Note [Time ranges].
encodeDay :: Day -> IO Int32
encodeDay day = do
  let days = diffDays day (fromGregorian 1970 1 1)
  when (days < -2147483646) $
    throwOutsideTimeRange (show day) "below the smallest DATE of DuckDB, -5877641-06-25"
  when (days > 2147483646) $
    throwOutsideTimeRange (show day) "above the largest DATE of DuckDB, 5881580-07-10"
  pure (fromInteger days)

-- | Encode a time of day at DuckDB microsecond precision.
encodeTimeOfDay :: TimeOfDay -> IO Int64
encodeTimeOfDay tod = fromInteger <$> timeOfDayUnits 1000000 tod

-- | Encode finite timestamps without native calendar conversions.
encodeLocalTime :: LocalTime -> IO Int64
encodeLocalTime = encodeTimestampUnits 1000000

-- | Encode finite timestamps in the requested number of units per second.
-- See Note [Time ranges].
encodeTimestampUnits :: Integer -> LocalTime -> IO Int64
encodeTimestampUnits units value@LocalTime{localTimeOfDay} = do
  -- timeOfDayUnits checks the time of day.
  _ <- timeOfDayUnits units localTimeOfDay
  checkTimestampRange units value
  let total = timestampUnits units value
  -- Only a nanosecond value reaches this check with a value outside the
  -- storage or at an infinity value. See Note [Time ranges].
  checkFiniteRange "TIMESTAMP" (minBound :: Int64) maxBound total
  pure (fromInteger total)

{- Note [Time ranges]
~~~~~~~~~~~~~~~~~~~~~
The storage of a DATE and of a TIMESTAMP can hold values that DuckDB 2.0.0
does not accept. The SQL literal of each type, and the casts to it, reject
these values:

- DATE: from -5877641-06-25 (5877642-06-25 BC) to 5881580-07-10, days
  -2147483646 to 2147483646 from the Unix epoch. The INT32 storage also holds
  day -2147483648.

- TIMESTAMP, TIMESTAMPTZ, TIMESTAMP_S, and TIMESTAMP_MS: from -290308-12-22
  00:00:00 (290309-12-22 BC) to 294247-01-10 04:00:54.775806, microseconds
  -9223372022400000000 to 2^63 - 2. The INT64 storage of a microsecond value
  also holds about four hours before that start, and the storage of a second
  or a millisecond value holds much more at each end.

A value outside these ranges breaks ordinary SQL in DuckDB 2.0.0. A cast of
such a TIMESTAMP to VARCHAR raises an INTERNAL error, and DuckDB then
invalidates the database. A cast of such a TIMESTAMP to TIMESTAMP_S stores the
first valid value with no error. A DATE before the range sorts before
-infinity, and a cast of it to TIMESTAMP fails. Thus encodeDay and
encodeTimestampUnits raise an SQLError of the type ErrorTypeConversion for
each finite value outside these ranges (throwOutsideTimeRange), before any
check of the storage, so a value outside the storage and a value at an
infinity value get this error too. The text names the value and the end of
the range in the notation of Data.Time, where the year -5877641 is 5877642 BC.
A nanosecond value has the range of its INT64 storage, and the literal of
TIMESTAMP_NS accepts that range, so it gets no check here. checkFiniteRange
then catches only a nanosecond value outside its storage or at an infinity
value.

A leap second gets no check. A TimeOfDay with 60 seconds binds as the next
second, as make_time(23, 59, 60) and make_timestamp(2016, 12, 31, 23, 59, 60)
of DuckDB give it. The SQL literal '23:59:60' raises an error.
-}

-- | Reject a second, millisecond, or microsecond timestamp outside the range
-- of DuckDB. See Note [Time ranges].
checkTimestampRange :: Integer -> LocalTime -> IO ()
checkTimestampRange units value
  | units > 1000000 = pure ()
  | micros < -9223372022400000000 =
      throwOutsideTimeRange (show value) ("below the smallest " <> name <> " of DuckDB, -290308-12-22 00:00:00")
  | micros > 9223372036854775806 = throwOutsideTimeRange (show value) ("above the largest " <> name <> " of DuckDB, " <> largest)
  | otherwise = pure ()
  where
    micros = timestampUnits 1000000 value
    (name, largest) = case units of
      1 -> ("TIMESTAMP_S", "294247-01-10 04:00:54")
      1000 -> ("TIMESTAMP_MS", "294247-01-10 04:00:54.775")
      _ -> ("TIMESTAMP", "294247-01-10 04:00:54.775806")

-- | Raise the error for a date or a timestamp outside the range of DuckDB. The
-- text names the Haskell value and the end of the range. See
-- Note [Time ranges].
throwOutsideTimeRange :: String -> Text -> IO a
throwOutsideTimeRange value bound = throwConversionError ("the value " <> Text.pack value <> " is " <> bound)

-- | Count the units from the Unix epoch to a timestamp, with the digits below
-- one unit cut toward the earlier time. The count does not check the time of
-- day.
timestampUnits :: Integer -> LocalTime -> Integer
timestampUnits units LocalTime{localDay, localTimeOfDay} =
  diffDays localDay (fromGregorian 1970 1 1) * 86400 * units
    + diffTimeToPicoseconds (timeOfDayToTime localTimeOfDay) `div` (1000000000000 `div` units)

-- | Check storage limits and the two DuckDB infinity sentinels.
checkFiniteRange :: (Integral a) => String -> a -> a -> Integer -> IO ()
checkFiniteRange label lower upper value =
  unless (inFiniteRange lower upper value) $
    throwConversionError (Text.pack label <> " value out of finite range")

-- | Check storage limits without accepting the two infinity sentinels.
inFiniteRange :: (Integral a) => a -> a -> Integer -> Bool
inFiniteRange lower upper value =
  value >= toInteger lower && value < toInteger upper && value /= negate (toInteger upper)

-- | Validate time components and convert to the requested units per second.
timeOfDayUnits :: Integer -> TimeOfDay -> IO Integer
timeOfDayUnits units tod@(TimeOfDay hours minutes seconds) = do
  -- A time of day that is not a TIME value is outside the range of the type: a
  -- field outside its own range, for example 23:61:00, and a time after
  -- 24:00:00 with valid fields, for example 24:00:01, 25:00:00, or 23:59:60.5.
  -- The two texts name the two cases.
  when (hours < 0 || minutes < 0 || minutes > 59 || seconds < 0 || seconds >= 61) $
    throwConversionError "invalid time of day"
  let picos = diffTimeToPicoseconds (timeOfDayToTime tod)
  when (hours > 24 || picos > 86400 * 1000000000000) $
    throwConversionError "time of day out of range"
  pure (picos `div` (1000000000000 `div` units))

-- | Transfer a newly constructed value to the statement on success.
bindDuckValue :: Statement -> DuckDBIdx -> IO DuckDBV2ValueHandle -> IO ()
bindDuckValue statement index make = mask_ do
  value <- make
  storeBoundValue statement index value `onException` destroyValue value
