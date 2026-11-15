{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : Database.DuckDB.Simple.LogicalRep
-- Description : Structured logical-type and value representations for DuckDB.
--
-- "Database.DuckDB.Simple.V2.LogicalRep" converts these types to and from native
-- logical types.
module Database.DuckDB.Simple.LogicalRep (
  -- * Structured value helpers
  StructField (..),
  StructValue (..),
  TupleValue (..),
  UnionMemberType (..),
  UnionValue (..),
  LogicalTypeRep (..),
  LogicalTypeId (..),
  TypeDeclaration (..),
  withoutAliases,
  aliasBase,
  structValueTypeRep,
  tupleValueTypeRep,
  unionValueTypeRep,
) where

import Data.Array (Array)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word16, Word64, Word8)
import Database.DuckDB.Simple.Native (LogicalTypeId (..))

-- | A Haskell description of a DuckDB logical type tree. See
-- Note [Type aliases] for 'LogicalTypeAlias'.
data LogicalTypeRep
  = LogicalTypeScalar LogicalTypeId
  | LogicalTypeDecimal !Word8 !Word8
  | LogicalTypeCollatedVarchar !Text
  | LogicalTypeGeometry !(Maybe Text)
  | LogicalTypeList LogicalTypeRep
  | LogicalTypeArray LogicalTypeRep !Word64
  | LogicalTypeMap LogicalTypeRep LogicalTypeRep
  | LogicalTypeStruct !(Array Int (StructField LogicalTypeRep))
  | LogicalTypeTuple !(Array Int LogicalTypeRep)
  | LogicalTypeUnion !(Array Int UnionMemberType)
  | LogicalTypeEnum !(Array Int Text)
  | LogicalTypeAlias !Text LogicalTypeRep
  deriving (Eq, Show)

{- Note [Type aliases]
~~~~~~~~~~~~~~~~~~~~~~
DuckDB can keep an alias on a type. The type then has the values and the casts
of its base type, and the type text of DuckDB gives the alias. In DuckDB 2.0.0
these types have an alias:

- JSON. It is VARCHAR with the alias JSON, also as a child type, for example
  the element type of JSON[].
- A type from 'Database.DuckDB.Simple.Registration.registerCustomType'.
- A type from duckdb_v2_factory_create_type_with_alias.

A column of a type from SQL @CREATE TYPE name AS base@ has the base type and no
alias. A column of an ENUM type from @CREATE TYPE name AS ENUM (...)@ also has
no name.

LogicalTypeAlias holds the alias and the base type. logicalTypeToRep in
"Database.DuckDB.Simple.V2.LogicalRep" makes it at each depth. The V2 API has
no function that gives only the alias. duckdb_v2_logical_type_get_name gives
the alias of a type, or the name of the type id for a type with no alias. Thus
a name that is not the name of the type id (logicalTypeIdName in
"Database.DuckDB.Simple.Native") is an alias. logicalTypeFromRep writes the
alias with duckdb_v2_factory_create_type_with_alias.

The base type decides each conversion of the library. Thus the binding of
parameters, and the readers of a DECIMAL and a GEOMETRY, use
logicalTypeToBaseRep. It reads no type name, so these paths make no native call
for an alias. These places keep the alias:

- A 'Database.DuckDB.Simple.Catalog.ColumnDescription'.
- The type fields of a STRUCT, TUPLE, or UNION value that a reader gives. The
  reader of a vector takes its tree from logicalTypeToRep, and it decides with
  aliasBase. Thus a value that the library reads and binds again keeps its
  type, for example STRUCT(k JSON).

Compare a type with an alias to another type with withoutAliases.
-}

-- | Replace each alias with its base type, at each depth. See
-- Note [Type aliases].
withoutAliases :: LogicalTypeRep -> LogicalTypeRep
withoutAliases = \case
  LogicalTypeAlias _ base -> withoutAliases base
  LogicalTypeList child -> LogicalTypeList (withoutAliases child)
  LogicalTypeArray child size -> LogicalTypeArray (withoutAliases child) size
  LogicalTypeMap key value -> LogicalTypeMap (withoutAliases key) (withoutAliases value)
  LogicalTypeStruct fields -> LogicalTypeStruct (fmap (\field -> field{structFieldValue = withoutAliases (structFieldValue field)}) fields)
  LogicalTypeTuple fields -> LogicalTypeTuple (fmap withoutAliases fields)
  LogicalTypeUnion members -> LogicalTypeUnion (fmap (\member -> member{unionMemberType = withoutAliases (unionMemberType member)}) members)
  rep -> rep

-- | Remove the aliases at the top of a type. The child types keep their
-- aliases. See Note [Type aliases].
aliasBase :: LogicalTypeRep -> LogicalTypeRep
aliasBase (LogicalTypeAlias _ base) = aliasBase base
aliasBase rep = rep

-- | A structured type or a SQL type expression. A 'TypeExpression' with the
-- name of a type from 'Database.DuckDB.Simple.Registration.registerCustomType'
-- keeps that name, so a cast to it is a cast to the custom type. A name from
-- SQL @CREATE TYPE name AS base@ is only another name for the base type, so a
-- cast to it is a cast to the base type. In a test with DuckDB 2.0.0, a
-- registered cast from BIGINT to a name for BIGINT did not run. The header of
-- DuckDB 2.0.0 says that a catalog type name is not kept, but DuckDB 2.0.0
-- keeps the name of a type from
-- 'Database.DuckDB.Simple.Registration.registerCustomType'.
data TypeDeclaration = StructuredType LogicalTypeRep | TypeExpression Text
  deriving (Eq, Show)

-- | A named field within a STRUCT-like value or type.
data StructField a = StructField
  { structFieldName :: !Text
  , structFieldValue :: !a
  }
  deriving (Eq, Show)

-- | A fully materialized STRUCT value together with its type metadata.
data StructValue a = StructValue
  { structValueFields :: !(Array Int (StructField a))
  , structValueTypes :: !(Array Int (StructField LogicalTypeRep))
  , structValueIndex :: !(Map Text Int)
  }
  deriving (Eq, Show)

-- | A positional TUPLE value with its field types. Array bounds are independent.
data TupleValue a = TupleValue
  { tupleValueFields :: !(Array Int a)
  , tupleValueTypes :: !(Array Int LogicalTypeRep)
  }
  deriving (Eq, Show)

-- | A named member within a UNION type.
data UnionMemberType = UnionMemberType
  { unionMemberName :: !Text
  , unionMemberType :: !LogicalTypeRep
  }
  deriving (Eq, Show)

-- | A fully materialized UNION value together with its member metadata.
data UnionValue a = UnionValue
  { unionValueIndex :: !Word16
  , unionValueLabel :: !Text
  , unionValuePayload :: !a
  , unionValueMembers :: !(Array Int UnionMemberType)
  }
  deriving (Eq, Show)

-- | Recover the logical STRUCT type corresponding to a @StructValue@.
structValueTypeRep :: StructValue a -> LogicalTypeRep
structValueTypeRep StructValue{structValueTypes} = LogicalTypeStruct structValueTypes

-- | Recover the logical TUPLE type of a positional value.
tupleValueTypeRep :: TupleValue a -> LogicalTypeRep
tupleValueTypeRep TupleValue{tupleValueTypes} = LogicalTypeTuple tupleValueTypes

-- | Recover the logical UNION type corresponding to a @UnionValue@.
unionValueTypeRep :: UnionValue a -> LogicalTypeRep
unionValueTypeRep UnionValue{unionValueMembers} = LogicalTypeUnion unionValueMembers
