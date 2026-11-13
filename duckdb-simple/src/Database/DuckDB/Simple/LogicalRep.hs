{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : Database.DuckDB.Simple.LogicalRep
Description : Structured logical-type and value representations for DuckDB.

"Database.DuckDB.Simple.V2.LogicalRep" converts these types to and from native
logical types.
-}
module Database.DuckDB.Simple.LogicalRep (
    -- * Structured value helpers
    StructField (..),
    StructValue (..),
    TupleValue (..),
    UnionMemberType (..),
    UnionValue (..),
    LogicalTypeRep (..),
    LogicalTypeId (..),
    structValueTypeRep,
    tupleValueTypeRep,
    unionValueTypeRep,
) where

import Data.Array (Array)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word16, Word64, Word8)
import Database.DuckDB.Simple.Native (LogicalTypeId (..))

-- | A Haskell description of a DuckDB logical type tree.
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
