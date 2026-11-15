{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.DuckDB.Simple.ToField
-- Description : Convert Haskell parameters into DuckDB bindable values.
--
-- The @ToField@ class mirrors the interface provided by @sqlite-simple@ while
-- delegating to the DuckDB C API under the hood.
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

import Control.Exception (bracket, mask_, onException, throwIO)
import Control.Monad (foldM, forM_, unless, when)
import Data.Array (Array, bounds, elems, listArray)
import Data.Bits (complement, shiftL, shiftR, xor, (.|.))
import qualified Data.ByteString as BS
import Data.Fixed (Pico)
import qualified Data.Geometry as G
import qualified Data.Geometry.WKT as WKT
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.List as List
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), TimeZone (..), timeOfDayToTime, timeZoneMinutes, utc, utcToLocalTime)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Element (timeTzMaxOffsetSeconds, timeTzOffsetBits)
import Database.DuckDB.Simple.FromField (
  BigNum (..),
  BitString (..),
  DecimalValue (..),
  FieldValue (..),
  IntervalValue (..),
  TimeWithZone (..),
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
  UnionMemberType (..),
  UnionValue (..),
  structValueTypeRep,
  tupleValueTypeRep,
  unionValueTypeRep,
 )
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Time (Date, LocalTimestamp, UTCTimestamp, Unbounded (..))
import Database.DuckDB.Simple.TypeCache (cachedLogicalType, typeCacheConnection)
import Database.DuckDB.Simple.Types (Null (..))
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeFromRep, logicalTypeToRep)
import Database.DuckDB.Simple.Variant (Variant (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CDouble (..), CFloat (..))
import Foreign.Marshal (fromBool)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import GHC.Exts (Ptr (..))
import GHC.Num.Integer (integerLog2, integerToAddr)
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
  --     Use 'ToField' to resolve arbitrary CRS labels from connection options.
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

-- | Preserve UTC nanoseconds where the prepared parameter requires them.
temporalValueBinding :: String -> LogicalTypeRep -> FieldValue -> (ValueBuilder -> IO DuckDBV2ValueHandle) -> FieldBinding
temporalValueBinding display source value makeDefault =
  targetValueBinding display \builder readTarget -> do
    target <- readTarget
    let precise = timestampTypeForTarget target source
    if precise == source
      then makeDefault builder
      else fieldValueWithTypeDuckValue builder precise (valueWithTypeMetadata precise value)

-- | Read the element type of a prepared LIST or ARRAY parameter.
collectionChildType :: Maybe LogicalTypeRep -> Maybe LogicalTypeRep
collectionChildType (Just (LogicalTypeList child)) = Just child
collectionChildType (Just (LogicalTypeArray child _)) = Just child
collectionChildType _ = Nothing

-- | Promote only UTC leaves that correspond to nanosecond parameter leaves.
timestampTypeForTarget :: Maybe LogicalTypeRep -> LogicalTypeRep -> LogicalTypeRep
timestampTypeForTarget target source = case (target, source) of
  (Just (LogicalTypeScalar LogicalTypeIdTimestampTzNs), LogicalTypeScalar LogicalTypeIdTimestampTz) ->
    LogicalTypeScalar LogicalTypeIdTimestampTzNs
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

-- | Types that map to a concrete DuckDB column type when used with @ToField@.
class DuckDBColumnType a where
  duckdbColumnTypeFor :: Proxy a -> Text

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
-- 'SQLError'. Bind a t'BigNum' to bind a BIGNUM value in each case. See
-- Note [Integer parameters].
instance ToField Integer where
  toField = integerBinding

-- | A value from 0 to 2^127 - 1 binds as a HUGEINT. A larger value binds as a
-- UHUGEINT for a UHUGEINT parameter that can hold it, and as a BIGNUM for a
-- parameter that has no known type or is not an integer or DECIMAL type. For
-- another integer or DECIMAL parameter, such a value raises 'SQLError'. Bind a
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
  SQLError with the type ErrorInputOutOfRange. Each other parameter, and a
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
  - An Array or a list of Integer values for a target with no known type, or
    for a target with an element type that is not an integer type or a DECIMAL
    type, has BIGNUM elements. A cast that DuckDB adds later can still have
    the defects above.
  - toDuckValue gives a BIGNUM value for a value outside the HUGEINT range.
    See the item about toDuckValue.

- toDuckValue. ToDuckValue Integer and ToDuckValue Natural use the same range
  rule (hugeIntInRange): a value from -2^127 to 2^127 - 1 gives a HUGEINT
  value. toDuckValue has no target type. Thus each other value gives a BIGNUM
  value, and a cast of it to an integer type can have the defects above. For
  example, setConfigValue with 2^128 + 5 for a UBIGINT option sets 5. The
  callers of toDuckValue are setConfigValue, defaultValue, the typed rows of
  table functions and COPY FROM, and the writer of a typed cast. The
  DuckDBColumnType of Integer and Natural stays BIGNUM, so a typed row declares
  a BIGNUM column for them. DuckDB casts each HUGEINT value to that BIGNUM
  column with no defect.

- Arrays and lists. DuckDB casts each element of an ARRAY or a LIST value to
  the declared element type when it builds the value. The DuckDBColumnType of
  Integer, Natural, and BigNum is BIGNUM. For an ARRAY or LIST target with an
  integer or DECIMAL element type, sequenceDuckValue declares that element
  type in place of BIGNUM. Each Integer element is a HUGEINT or a UHUGEINT
  value, so DuckDB checks the range of the cast of each element. For example,
  [5, -6] for a UBIGINT[2] column raises an error. An Array or a list of
  BigNum values also takes the element type of the target, but each element
  is a BIGNUM value. DuckDB casts each element with the casts from BIGNUM, so
  the result for BigNum elements does not improve.

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
    outOfRange typeName =
      throwIO
        SQLError
          { sqlErrorMessage = "duckdb-simple: the value " <> Text.pack (show value) <> " is out of range for a parameter of the type " <> typeName
          , sqlErrorType = Just ErrorInputOutOfRange
          , sqlErrorQuery = Nothing
          }

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

instance ToField Day

instance ToField TimeOfDay

instance ToField LocalTime

-- | Bind the months, days, and microseconds as an INTERVAL. DuckDB keeps the
-- three fields.
instance ToField IntervalValue

-- | Bind the time and its offset in minutes as a TIMETZ. The name and the
-- summer flag of the t'TimeZone' are not stored. An offset of 16 hours or more
-- raises an error.
instance ToField TimeWithZone

-- | Bind the value as a DECIMAL with its own width and scale. DuckDB casts it
-- to the type of the parameter. A width outside 1 to 38, a scale larger than
-- the width, or an integer that the width cannot hold raises an error.
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

instance ToField UTCTime where
  toField value =
    temporalValueBinding
      (show value)
      (LogicalTypeScalar LogicalTypeIdTimestampTz)
      (FieldTimestampTZ (Finite value))
      (\builder -> utcTimeDuckValue (builderFactory builder) value)

instance ToField (Unbounded Day)

instance ToField (Unbounded LocalTime)

instance ToField (Unbounded UTCTime) where
  toField value =
    temporalValueBinding
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

instance (DuckDBColumnType a, ToField a) => ToField (Array Int a) where
  toField arr =
    targetValueBinding
      ("<array length=" <> show (length (elems arr)) <> ">")
      \builder readTarget -> do
        target <- readTarget
        let childTarget = collectionChildType target
        arrayDuckValue builder childTarget (\value -> fieldBindingValue (toField value) builder (pure childTarget)) arr

-- | Bind a list as a LIST value with the element rules of an 'Array'. An empty
-- list binds as an empty LIST of the element type. 'String' keeps its own
-- instance. A list as a row is a row of parameters, so bind a list parameter
-- as @Only [..]@.
instance {-# OVERLAPPABLE #-} (DuckDBColumnType a, ToField a) => ToField [a] where
  toField values =
    targetValueBinding
      ("<list length=" <> show (length values) <> ">")
      \builder readTarget -> do
        target <- readTarget
        let childTarget = collectionChildType target
        listDuckValue builder childTarget (\value -> fieldBindingValue (toField value) builder (pure childTarget)) values

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

instance DuckDBColumnType Integer where
  duckdbColumnTypeFor _ = "BIGNUM"

instance DuckDBColumnType Natural where
  duckdbColumnTypeFor _ = "BIGNUM"

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
  duckdbColumnTypeFor _ = "TEXT"

instance DuckDBColumnType String where
  duckdbColumnTypeFor _ = "TEXT"

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

instance DuckDBColumnType (StructValue FieldValue) where
  duckdbColumnTypeFor _ = "STRUCT"

instance DuckDBColumnType (TupleValue FieldValue) where
  duckdbColumnTypeFor _ = "TUPLE"

instance DuckDBColumnType (UnionValue FieldValue) where
  duckdbColumnTypeFor _ = "UNION"

instance (DuckDBColumnType a) => DuckDBColumnType (Maybe a) where
  duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy :: Proxy a)

instance (DuckDBColumnType a) => DuckDBColumnType (Array Int a) where
  duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy :: Proxy a) <> Text.pack "[]"

-- | 'String' keeps its own instance.
instance {-# OVERLAPPABLE #-} (DuckDBColumnType a) => DuckDBColumnType [a] where
  duckdbColumnTypeFor _ = duckdbColumnTypeFor (Proxy :: Proxy a) <> Text.pack "[]"

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
    throwIO (userError "duckdb-simple: BIT requires nonempty data and padding from 0 to 7")
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

-- | Construct an ARRAY value. An empty array raises 'SQLError'. See
-- 'sequenceDuckValue' for the element type.
arrayDuckValue
  :: (DuckDBColumnType a)
  => ValueBuilder
  -> Maybe LogicalTypeRep
  -> (a -> IO DuckDBV2ValueHandle)
  -> Array Int a
  -> IO DuckDBV2ValueHandle
arrayDuckValue builder target elementValue array = do
  when (null (elems array)) $
    throwIO
      ( SQLError
          "duckdb-simple: an empty ARRAY parameter is not possible, because a DuckDB ARRAY has at least one element; bind an empty list for an empty LIST"
          Nothing
          Nothing
      )
  sequenceDuckValue c_duckdb_v2_value_create_array builder target elementValue (elems array)

-- | Construct a LIST value. An empty list is an empty LIST of the element type.
-- See 'sequenceDuckValue' for the element type.
listDuckValue
  :: (DuckDBColumnType a)
  => ValueBuilder
  -> Maybe LogicalTypeRep
  -> (a -> IO DuckDBV2ValueHandle)
  -> [a]
  -> IO DuckDBV2ValueHandle
listDuckValue = sequenceDuckValue c_duckdb_v2_value_create_list

-- | Construct a LIST or an ARRAY value with the native constructor. The target
-- is the element type of the parameter. A scalar element type comes from the
-- column type name of the element. When that type is BIGNUM and the target is
-- an integer type or a DECIMAL type, the value takes the target as its element
-- type. See Note [Integer parameters]. Each other element type comes from the
-- first element that is not NULL, and the other elements must have the same
-- type.
sequenceDuckValue
  :: forall a
   . (DuckDBColumnType a)
  => ( DuckDBV2FactoryHandle
       -> DuckDBV2LogicalTypeHandle
       -> ConstPtr DuckDBV2ValueHandle
       -> DuckDBIdx
       -> Ptr DuckDBV2ValueHandle
       -> Ptr DuckDBV2ErrorInfoHandle
       -> IO DuckDBV2Error
     )
  -> ValueBuilder
  -> Maybe LogicalTypeRep
  -> (a -> IO DuckDBV2ValueHandle)
  -> [a]
  -> IO DuckDBV2ValueHandle
sequenceDuckValue create ValueBuilder{builderFactory = factory, builderType = makeType} target elementValue elements =
  withCreatedValues (map elementValue elements) \values ->
    withElementType values \elementType ->
      withArray values \ptr ->
        output (create factory elementType (ConstPtr ptr) (fromIntegral (length values)))
  where
    typeName = duckdbColumnType (Proxy :: Proxy a)
    withElementType values action = case duckDBTypeFromName typeName of
      Just dtype -> bracket (makeType (declaredType dtype)) destroyLogicalType action
      Nothing -> do
        -- Keep the values that are not NULL, in list order. The fold keeps a
        -- constant stack. See Note [Constant stack at safe calls] in
        -- "Database.DuckDB.Simple.Native".
        let keep kept value = output (c_duckdb_v2_value_is_null value) >>= \flag -> if flag == 0 then pure (value : kept) else pure kept
        present <- reverse <$> foldM keep [] values
        case present of
          value : rest -> bracket (output (c_duckdb_v2_value_get_logical_type value)) destroyLogicalType \logical -> do
            expected <- logicalTypeToRep logical
            forM_ rest \element -> do
              actual <- bracket (output (c_duckdb_v2_value_get_logical_type element)) destroyLogicalType logicalTypeToRep
              when (actual /= expected) $ throwIO (userError "duckdb-simple: array elements have different logical types")
            action logical
          [] ->
            throwIO
              SQLError
                { sqlErrorMessage = "duckdb-simple: an empty or all-NULL array of " <> typeName <> " elements has no element type"
                , sqlErrorType = Nothing
                , sqlErrorQuery = Nothing
                }
    -- See Note [Integer parameters].
    declaredType LogicalTypeIdBigNum
      | Just rep@(LogicalTypeScalar dtype) <- target, dtype `elem` integerTypes = rep
      | Just rep@LogicalTypeDecimal{} <- target = rep
    declaredType dtype = timestampTypeForTarget target (LogicalTypeScalar dtype)

-- | Construct named fields. The native call copies the field values.
structValueDuckValue
  :: ValueBuilder -> StructValue FieldValue -> IO DuckDBV2ValueHandle
structValueDuckValue builder@ValueBuilder{builderFactory = factory} StructValue{structValueFields, structValueTypes} = do
  let fields = elems structValueFields
      types = elems structValueTypes
      names = map structFieldName fields
  when (names /= map structFieldName types) $
    throwIO (userError "duckdb-simple: struct value/type fields mismatch")
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
    throwIO (userError "duckdb-simple: tuple value/type fields mismatch")
  withCreatedValues (zipWith (fieldValueWithTypeDuckValue builder) types fields) \values ->
    withArray values \valuePtr ->
      declaredCompositeValue builder (LogicalTypeTuple tupleValueTypes) $
        output (c_duckdb_v2_value_create_tuple factory (ConstPtr valuePtr) (fromIntegral (length values)))

-- | Preserve declared field types through an owned vector. Scalar VARCHAR
-- values do not retain collations, but their enclosing composite type does.
declaredCompositeValue
  :: ValueBuilder
  -> LogicalTypeRep
  -> IO DuckDBV2ValueHandle
  -> IO DuckDBV2ValueHandle
declaredCompositeValue _ rep create | not (hasCollation rep) = create
declaredCompositeValue ValueBuilder{builderFactory = factory, builderType = makeType} rep create = bracket create destroyValue \value ->
  bracket (makeType rep) destroyLogicalType \logical -> withOwnedVector factory logical \vector -> do
    checked (c_duckdb_v2_vector_set_size vector 1)
    checked (c_duckdb_v2_vector_set_value vector 0 value)
    output (c_duckdb_v2_vector_get_value vector 0)

-- | Test whether a type tree contains a VARCHAR collation.
hasCollation :: LogicalTypeRep -> Bool
hasCollation rep = case rep of
  LogicalTypeCollatedVarchar _ -> True
  LogicalTypeList child -> hasCollation child
  LogicalTypeArray child _ -> hasCollation child
  LogicalTypeMap key value -> hasCollation key || hasCollation value
  LogicalTypeStruct fields -> any (hasCollation . structFieldValue) (elems fields)
  LogicalTypeTuple fields -> any hasCollation (elems fields)
  LogicalTypeUnion members -> any (hasCollation . unionMemberType) (elems members)
  _ -> False

-- | Build one UNION row through its public tag and member vectors. V2 has no
-- constructor for a UNION value. A cast of the member value to the UNION type
-- chooses the member by the type of the value, so it fails when two members have
-- the same type, as in a generic sum type whose constructors have equal fields.
-- The vectors keep the tag of the value.
unionValueDuckValue
  :: ValueBuilder -> UnionValue FieldValue -> IO DuckDBV2ValueHandle
unionValueDuckValue builder@ValueBuilder{builderFactory = factory, builderType = makeType} UnionValue{unionValueIndex, unionValueLabel, unionValuePayload, unionValueMembers} = do
  let members = elems unionValueMembers
      index = fromIntegral unionValueIndex
  when (index >= length members) $ throwIO (userError "duckdb-simple: union value tag out of range")
  let UnionMemberType name rep = members !! index
  when (name /= unionValueLabel) $ throwIO (userError "duckdb-simple: union tag and member name mismatch")
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

-- | Construct a value with explicit logical metadata, including typed NULL.
fieldValueWithTypeDuckValue
  :: ValueBuilder -> LogicalTypeRep -> FieldValue -> IO DuckDBV2ValueHandle
fieldValueWithTypeDuckValue ValueBuilder{builderFactory = factory, builderType = makeType} rep FieldNull = bracket (makeType rep) destroyLogicalType \logical ->
  output (c_duckdb_v2_value_create_null factory logical)
fieldValueWithTypeDuckValue builder@ValueBuilder{builderFactory = factory, builderType = makeType} rep value = case (rep, value) of
  (LogicalTypeScalar LogicalTypeIdVariant, _) -> variantDuckValue builder value
  (LogicalTypeScalar dtype, _) -> scalarFieldValueDuckValue factory dtype value
  (LogicalTypeCollatedVarchar _, FieldText text) -> textDuckValue factory text
  (LogicalTypeGeometry crs, FieldGeometry raw) -> rawGeometryDuckValue builder crs raw
  (LogicalTypeDecimal width scale, FieldDecimal decimal@DecimalValue{decimalWidth, decimalScale})
    | width == decimalWidth && scale == decimalScale -> decimalDuckValue factory decimal
  (LogicalTypeList child, FieldList values) -> sequenceValue c_duckdb_v2_value_create_list child values
  -- The generic ARRAY type declares the size 0. The value then gives the size.
  (LogicalTypeArray child size, FieldArray array)
    | size == 0 || fromIntegral (length (elems array)) == size -> sequenceValue c_duckdb_v2_value_create_array child (elems array)
  (LogicalTypeMap keyRep valueRep, FieldMap pairs) ->
    bracket (makeType keyRep) destroyLogicalType \keyType ->
      bracket (makeType valueRep) destroyLogicalType \valueType ->
        withCreatedValues (map (fieldValueWithTypeDuckValue builder keyRep . fst) pairs) \keys ->
          withCreatedValues (map (fieldValueWithTypeDuckValue builder valueRep . snd) pairs) \values ->
            withArray keys \keyPtr -> withArray values \valuePtr ->
              output (c_duckdb_v2_value_create_map factory keyType valueType (ConstPtr keyPtr) (ConstPtr valuePtr) (fromIntegral (length pairs)))
  (LogicalTypeStruct _, FieldStruct struct)
    | rep == structValueTypeRep struct -> structValueDuckValue builder struct
  (LogicalTypeTuple _, FieldTuple tuple)
    | rep == tupleValueTypeRep tuple -> tupleValueDuckValue builder tuple
  (LogicalTypeUnion _, FieldUnion union)
    | rep == unionValueTypeRep union -> unionValueDuckValue builder union
  (LogicalTypeEnum entries, FieldEnum index) -> enumDuckValue factory entries index
  _ -> typeMismatch (show rep) value
  where
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
    throwIO
      ( userError
          ( "duckdb-simple: unsupported scalar conversion for "
              <> show dtype
              <> " from "
              <> show value
          )
      )

-- | Construct an ENUM value by a cast of the entry text to the ENUM type. A
-- user cast from VARCHAR to that type replaces the cast. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
enumDuckValue :: DuckDBV2FactoryHandle -> Array Int Text -> Word32 -> IO DuckDBV2ValueHandle
enumDuckValue factory entries index = do
  let dictionary = elems entries
  when (toInteger index >= toInteger (length dictionary)) $ throwIO (userError "duckdb-simple: ENUM index out of range")
  bracket (logicalTypeFromRep factory (LogicalTypeEnum entries)) destroyLogicalType \logical ->
    bracket (textDuckValue factory (dictionary !! fromIntegral index)) destroyValue \value ->
      output (c_duckdb_v2_value_cast factory value logical)

hugeIntDuckValue :: DuckDBV2FactoryHandle -> Integer -> IO DuckDBV2ValueHandle
hugeIntDuckValue factory value = integerToHugeInt value >>= \huge -> with huge (\ptr -> output (c_duckdb_v2_value_create_hugeint factory (ConstPtr ptr)))

uhugeIntDuckValue :: DuckDBV2FactoryHandle -> Integer -> IO DuckDBV2ValueHandle
uhugeIntDuckValue factory value = integerToUHugeInt value >>= \huge -> with huge (\ptr -> output (c_duckdb_v2_value_create_uhugeint factory (ConstPtr ptr)))

decimalDuckValue :: DuckDBV2FactoryHandle -> DecimalValue -> IO DuckDBV2ValueHandle
decimalDuckValue factory DecimalValue{decimalWidth, decimalScale, decimalInteger} = do
  when (decimalWidth < 1 || decimalWidth > 38 || decimalScale > decimalWidth) $
    throwIO (userError "duckdb-simple: invalid DECIMAL width or scale")
  when (abs decimalInteger >= 10 ^ decimalWidth) $ throwIO (userError "duckdb-simple: DECIMAL value exceeds declared precision")
  huge <- integerToHugeInt decimalInteger
  with huge (\ptr -> output (c_duckdb_v2_value_create_decimal factory (ConstPtr ptr) decimalWidth decimalScale))

intervalDuckValue :: DuckDBV2FactoryHandle -> IntervalValue -> IO DuckDBV2ValueHandle
intervalDuckValue factory IntervalValue{intervalMonths, intervalDays, intervalMicros} =
  with
    (DuckDBV2IntervalT intervalMonths intervalDays intervalMicros)
    (\ptr -> output (c_duckdb_v2_value_create_interval factory (ConstPtr ptr)))

timeWithZoneDuckValue :: DuckDBV2FactoryHandle -> TimeWithZone -> IO DuckDBV2ValueHandle
timeWithZoneDuckValue factory TimeWithZone{timeWithZoneTime, timeWithZoneZone} = do
  micros <- timeOfDayUnits 1000000 timeWithZoneTime
  let offset = toInteger (timeZoneMinutes timeWithZoneZone) * 60
  when (abs offset > toInteger timeTzMaxOffsetSeconds) $ throwIO (userError "duckdb-simple: TIME WITH TIME ZONE offset out of range")
  output
    ( c_duckdb_v2_value_create_time_tz
        factory
        ((fromIntegral micros `shiftL` timeTzOffsetBits) .|. fromIntegral (toInteger timeTzMaxOffsetSeconds - offset))
    )

integerToHugeInt :: Integer -> IO DuckDBV2HugeintT
integerToHugeInt value = do
  when (value < negate (1 `shiftL` 127) || value >= 1 `shiftL` 127) $ throwIO (userError "duckdb-simple: HUGEINT value out of range")
  pure (DuckDBV2HugeintT (fromIntegral value) (fromIntegral (value `shiftR` 64)))

integerToUHugeInt :: Integer -> IO DuckDBV2UhugeintT
integerToUHugeInt value = do
  when (value < 0 || value >= 1 `shiftL` 128) $ throwIO (userError "duckdb-simple: UHUGEINT value out of range")
  pure (DuckDBV2UhugeintT (fromIntegral value) (fromIntegral (value `shiftR` 64)))

-- | Borrow generated identifier views and reject embedded NUL bytes.
withIdentifiers :: [Text] -> (Ptr DuckDBV2IdentifierT -> IO a) -> IO a
withIdentifiers names action = do
  when (any (Text.any (== '\0')) names) $
    throwIO (userError "duckdb-simple: logical type name contains NUL")
  withIdentifierArray names action

typeMismatch :: String -> FieldValue -> IO a
typeMismatch expected actual = throwIO (userError ("duckdb-simple: cannot encode " <> show actual <> " as " <> expected))

-- | Construct a GEOMETRY value of the declared CRS from a raw geometry. See
-- Note [Raw GEOMETRY parameters]. A user cast from BLOB to GEOMETRY replaces
-- the cast. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
rawGeometryDuckValue :: ValueBuilder -> Maybe Text -> RawGeometry -> IO DuckDBV2ValueHandle
rawGeometryDuckValue ValueBuilder{builderFactory = factory, builderType = makeType} crs RawGeometry{rawGeometryWKB, rawGeometryCRS} = do
  when (rawGeometryCRS /= crs) $
    throwIO
      SQLError
        { sqlErrorMessage =
            "duckdb-simple: a GEOMETRY value with " <> describeCRS rawGeometryCRS <> " cannot bind as a GEOMETRY with " <> describeCRS crs
        , sqlErrorType = Just ErrorInputInvalid
        , sqlErrorQuery = Nothing
        }
  bracket (makeType (LogicalTypeGeometry crs)) destroyLogicalType \logical ->
    bracket (blobDuckValue factory rawGeometryWKB) destroyValue \blob ->
      output (c_duckdb_v2_value_cast factory blob logical)
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
Thus an Array or a list of raw geometries and a typed row cannot hold one. Their
declared element type or column type would be GEOMETRY without a CRS, and
DuckDB would remove the CRS of each value.

A STRUCT, TUPLE, UNION, or Variant parameter binds each raw geometry that it
contains with this rule:

- The CRS of the value must be equal to the CRS of the declared GEOMETRY
  type. A value without a CRS agrees only with a type without a CRS. If
  the two are different, rawGeometryDuckValue raises an SQLError with the
  type ErrorInputInvalid.
- rawGeometryDuckValue makes a BLOB value from the WKB bytes. Then it casts
  the BLOB value to the declared GEOMETRY type with duckdb_v2_value_cast.
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

- Why earlier versions raised an error. DuckDB 1.5 does not have a cast from
  BLOB to GEOMETRY. Thus duckdb-simple 0.3 raised an error for each raw
  geometry parameter.
-}

-- | Cast the payload to VARIANT through the supplied native factory. A user
-- cast to VARIANT replaces the cast. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
variantDuckValue :: ValueBuilder -> FieldValue -> IO DuckDBV2ValueHandle
variantDuckValue builder@ValueBuilder{builderFactory = factory, builderType = makeType} value = do
  (rep, payload) <- variantPayloadType value
  bracket (makeType (LogicalTypeScalar LogicalTypeIdVariant)) destroyLogicalType \logical ->
    bracket (fieldValueWithTypeDuckValue builder rep payload) destroyValue \native ->
      output (c_duckdb_v2_value_cast factory native logical)

-- | Construct a UTC nanosecond value without losing sub-microsecond digits.
utcTimestampNsDuckValue :: DuckDBV2FactoryHandle -> UTCTimestamp -> IO DuckDBV2ValueHandle
utcTimestampNsDuckValue factory value =
  encodeUnbounded (encodeTimestampUnits 1000000000 . utcToLocalTime utc) value
    >>= (\encoded -> output (c_duckdb_v2_value_create_timestamp_tz_ns factory encoded))

-- | Cast the WKT text of the shape to GEOMETRY. A user cast from VARCHAR to
-- GEOMETRY replaces the cast. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize".
instance ToDuckValue G.Geometry where
  toDuckValue factory geometry = do
    wkt <- either (throwIO . userError) pure (WKT.encodeWKT geometry)
    bracket (logicalTypeFromRep factory (LogicalTypeGeometry Nothing)) destroyLogicalType \logical ->
      bracket (textDuckValue factory wkt) destroyValue \value ->
        output (c_duckdb_v2_value_cast factory value logical)

instance ToDuckValue Variant where
  toDuckValue factory = variantDuckValue (plainBuilder factory) . variantPayload

-- | Choose the native type of a VARIANT payload. Containers get VARIANT
-- elements and fields. Time values with sub-microsecond digits use nanosecond
-- types. Wide timestamps use milliseconds or seconds when these preserve the
-- value and fit the native range.
-- The C API cannot construct a nonempty object whose keys are all empty.
-- DuckDB 2.0 treats that STRUCT type as a tuple and casts it to a VARIANT array.
-- A raw geometry without a CRS uses GEOMETRY. A raw geometry with a CRS raises
-- an error. See Note [Raw GEOMETRY parameters].
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
    | hasNanos time -> scalar LogicalTypeIdTimeNs
    | otherwise -> scalar LogicalTypeIdTime
  FieldTimestamp (Finite LocalTime{localDay, localTimeOfDay})
    | hasNanos localTimeOfDay -> scalar LogicalTypeIdTimestampNs
    | inFiniteRange (minBound :: Int64) maxBound micros -> scalar LogicalTypeIdTimestamp
    | micros `rem` 1000 == 0 && inFiniteRange (minBound :: Int64) maxBound (micros `div` 1000) -> scalar LogicalTypeIdTimestampMs
    | micros `rem` 1000000 == 0 && inFiniteRange (minBound :: Int64) maxBound (micros `div` 1000000) -> scalar LogicalTypeIdTimestampSec
    where
      micros = diffDays localDay (fromGregorian 1970 1 1) * 86400 * 1000000 + diffTimeToPicoseconds (timeOfDayToTime localTimeOfDay) `div` 1000000
  FieldTimestamp{} -> scalar LogicalTypeIdTimestamp
  FieldTimestampTZ instant -> scalar (variantUtcTimestampType instant)
  FieldTimeTZ{} -> scalar LogicalTypeIdTimeTz
  FieldInterval{} -> scalar LogicalTypeIdInterval
  FieldBigNum{} -> scalar LogicalTypeIdBigNum
  FieldBit{} -> scalar LogicalTypeIdBit
  FieldList{} -> pure (LogicalTypeList variant, value)
  FieldArray items -> pure (LogicalTypeList variant, FieldList (elems items))
  FieldTuple TupleValue{tupleValueFields} -> pure (LogicalTypeList variant, FieldList (elems tupleValueFields))
  FieldStruct structValue@StructValue{structValueTypes} -> do
    let names = map structFieldName (elems structValueTypes)
    when (length names /= length (List.nub names)) $
      throwIO (userError "duckdb-simple: VARIANT objects need unique keys")
    when (not (null names) && all Text.null names) $
      throwIO (userError "duckdb-simple: DuckDB's C API cannot construct a VARIANT object with only an empty key")
    let types = fmap (\field -> field{structFieldValue = variant}) structValueTypes
    pure (LogicalTypeStruct types, FieldStruct structValue{structValueTypes = types})
  FieldUnion unionValue -> pure (unionValueTypeRep unionValue, value)
  FieldGeometry RawGeometry{rawGeometryCRS = Just crs} ->
    throwIO
      SQLError
        { sqlErrorMessage =
            "duckdb-simple: a VARIANT does not keep the CRS of a GEOMETRY, so the GEOMETRY value with the CRS "
              <> quotedCRS crs
              <> " cannot bind as a VARIANT"
        , sqlErrorType = Just ErrorInputInvalid
        , sqlErrorQuery = Nothing
        }
  FieldGeometry{} -> pure (LogicalTypeGeometry Nothing, value)
  FieldMap{} -> throwIO (userError "duckdb-simple: VARIANT payloads cannot contain MAP values")
  FieldEnum{} -> throwIO (userError "duckdb-simple: VARIANT payloads cannot contain ENUM values")
  where
    variant = LogicalTypeScalar LogicalTypeIdVariant
    scalar dtype = pure (LogicalTypeScalar dtype, value)
    hasNanos time = snd (properFraction (todSec time * 1000000) :: (Integer, Pico)) /= 0

-- | Choose nanosecond storage for a precise UTC VARIANT payload.
variantUtcTimestampType :: UTCTimestamp -> LogicalTypeId
variantUtcTimestampType (Finite instant)
  | snd (properFraction (todSec (localTimeOfDay (utcToLocalTime utc instant)) * 1000000) :: (Integer, Pico)) /= 0 = LogicalTypeIdTimestampTzNs
variantUtcTimestampType _ = LogicalTypeIdTimestampTz

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
-- gives a BIGNUM value. See Note [Integer parameters].
instance ToDuckValue Integer where
  toDuckValue factory value = case hugeIntInRange value of
    Just make -> make factory
    Nothing -> bigNumDuckValue factory (BigNum value)

-- | A value from 0 to 2^127 - 1 gives a HUGEINT value. Each larger value gives
-- a BIGNUM value. See Note [Integer parameters].
instance ToDuckValue Natural where
  toDuckValue factory = toDuckValue factory . toInteger

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

-- | Construct an array through the supplied factory with the element type of @a@.
instance (DuckDBColumnType a, ToDuckValue a) => ToDuckValue (Array Int a) where
  toDuckValue factory = arrayDuckValue (plainBuilder factory) Nothing (toDuckValue factory)

-- | Construct a LIST through the supplied factory with the element type of @a@.
-- 'String' keeps its own instance.
instance {-# OVERLAPPABLE #-} (DuckDBColumnType a, ToDuckValue a) => ToDuckValue [a] where
  toDuckValue factory = listDuckValue (plainBuilder factory) Nothing (toDuckValue factory)

instance (ToDuckValue a) => ToDuckValue (Maybe a) where
  toDuckValue factory Nothing = nullDuckValue factory
  toDuckValue factory (Just value) = toDuckValue factory value

-- | Preserve infinity sentinels and validate finite values before narrowing.
encodeUnbounded :: (Integral b, Bounded b) => (a -> IO b) -> Unbounded a -> IO b
encodeUnbounded _ NegInfinity = pure (negate maxBound)
encodeUnbounded _ PosInfinity = pure maxBound
encodeUnbounded encode (Finite value) = encode value

-- | Encode finite dates as days from the Unix epoch.
encodeDay :: Day -> IO Int32
encodeDay day = do
  let days = diffDays day (fromGregorian 1970 1 1)
  checkFiniteRange "DATE" (minBound :: Int32) maxBound days
  pure (fromInteger days)

-- | Encode a time of day at DuckDB microsecond precision.
encodeTimeOfDay :: TimeOfDay -> IO Int64
encodeTimeOfDay tod = fromInteger <$> timeOfDayUnits 1000000 tod

-- | Encode finite timestamps without native calendar conversions.
encodeLocalTime :: LocalTime -> IO Int64
encodeLocalTime = encodeTimestampUnits 1000000

-- | Encode finite timestamps in the requested number of units per second.
encodeTimestampUnits :: Integer -> LocalTime -> IO Int64
encodeTimestampUnits units LocalTime{localDay, localTimeOfDay} = do
  time <- timeOfDayUnits units localTimeOfDay
  let total = diffDays localDay (fromGregorian 1970 1 1) * 86400 * units + time
  checkFiniteRange "TIMESTAMP" (minBound :: Int64) maxBound total
  pure (fromInteger total)

-- | Check storage limits and the two DuckDB infinity sentinels.
checkFiniteRange :: (Integral a) => String -> a -> a -> Integer -> IO ()
checkFiniteRange label lower upper value =
  unless (inFiniteRange lower upper value) $
    throwIO (userError ("duckdb-simple: " <> label <> " value out of finite range"))

-- | Check storage limits without accepting the two infinity sentinels.
inFiniteRange :: (Integral a) => a -> a -> Integer -> Bool
inFiniteRange lower upper value =
  value >= toInteger lower && value < toInteger upper && value /= negate (toInteger upper)

-- | Validate time components and convert to the requested units per second.
timeOfDayUnits :: Integer -> TimeOfDay -> IO Integer
timeOfDayUnits units tod@(TimeOfDay hours minutes seconds) = do
  when
    (hours < 0 || hours > 24 || minutes < 0 || minutes > 59 || seconds < 0 || seconds >= 61 || (hours == 24 && (minutes /= 0 || seconds /= 0)))
    $ throwIO (userError "duckdb-simple: invalid time of day")
  let value = diffTimeToPicoseconds (timeOfDayToTime tod) `div` (1000000000000 `div` units)
  when (value > 86400 * units) $
    throwIO (userError "duckdb-simple: time of day out of range")
  pure value

-- | Transfer a newly constructed value to the statement on success.
bindDuckValue :: Statement -> DuckDBIdx -> IO DuckDBV2ValueHandle -> IO ()
bindDuckValue statement index make = mask_ do
  value <- make
  storeBoundValue statement index value `onException` destroyValue value
