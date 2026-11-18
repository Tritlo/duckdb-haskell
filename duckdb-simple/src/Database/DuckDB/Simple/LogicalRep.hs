{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
  listElementType,
  mapEntryTypes,
  logicalTypeSQL,
  structValueTypeRep,
  tupleValueTypeRep,
  unionValueTypeRep,
) where

import Data.Array (Array, elems)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word64, Word8)
import Database.DuckDB.Simple.Native (LogicalTypeId (..), logicalTypeIdName)

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
DuckDB can keep an alias on a type. The alias uses the value representation of
its base type, and the type text of DuckDB gives the alias. In DuckDB 2.0.0 these
types have an alias:

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
the alias of a type, or the name of the type id for a type with no alias. A
name that is not the name of the type id (logicalTypeIdName in
"Database.DuckDB.Simple.Native") is an alias. When the names are equal,
logicalTypeToRep makes a temporary copy with that alias and compares the two
native types. DuckDB includes the alias in type equality, so equal types mean
that the original type has the alias. This also distinguishes INTEGER from
INTEGER with the alias INTEGER, whose type texts are equal.

logicalTypeToRep borrows a factory for this comparison. The factory can be
from any instance. The comparison does not parse a type, run SQL, or call user
code. The temporary type is destroyed before the function returns.
logicalTypeFromRep writes the alias with
duckdb_v2_factory_create_type_with_alias.

The base type decides value conversion and vector layout. These operations use
logicalTypeToBaseRep, which reads no type name. Collection binding also compares
native type identity to keep aliases during element type inference. A temporal
promotion reads full type trees to keep aliases on unchanged leaves. See
Note [Element types of a list] in "Database.DuckDB.Simple.ToField".
These places keep the alias:

- A 'Database.DuckDB.Simple.Catalog.ColumnDescription'.
- The type fields of a STRUCT, TUPLE, or UNION value that a reader gives. The
  reader of a vector takes its tree from logicalTypeToRep, and it decides with
  aliasBase. A value that the library reads and binds again keeps its type when
  the native default cast supports its aliases, for example STRUCT(k JSON).

Alias metadata does not imply native cast support. DuckDB 2.0.0 cannot cast a
TIME value to an alias of TIME through the default cast. The library writes
TIME alias payloads through public vector storage. See Note [TIME alias values]
in "Database.DuckDB.Simple.ToField". LogicalTypeRep stores the alias
name and public type parameters. It does not store other C++ extension metadata.

Use withoutAliases to compare base types.
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

-- | The type of an element of a LIST or an ARRAY, from the type of its parent.
-- The function looks through an alias. Another parent type gives VARIANT: the
-- type of each stored value decides. See Note [Column types] in
-- "Database.DuckDB.Simple.FromField".
listElementType :: LogicalTypeRep -> LogicalTypeRep
listElementType parent = case aliasBase parent of
  LogicalTypeList element -> element
  LogicalTypeArray element _ -> element
  _ -> LogicalTypeScalar LogicalTypeIdVariant

-- | The key type and the value type of a MAP, from the type of its parent, as
-- 'listElementType' gives an element type.
mapEntryTypes :: LogicalTypeRep -> (LogicalTypeRep, LogicalTypeRep)
mapEntryTypes parent = case aliasBase parent of
  LogicalTypeMap key value -> (key, value)
  _ -> (LogicalTypeScalar LogicalTypeIdVariant, LogicalTypeScalar LogicalTypeIdVariant)

{- Note [Type texts]
~~~~~~~~~~~~~~~~~~~~
logicalTypeSQL gives the text of a type that duckdb_v2_logical_type_to_text of
DuckDB 2.0.0 gives, for example DECIMAL(18,3), BIGINT[], INTEGER[3],
MAP(VARCHAR, BIGINT), ENUM('sad', 'happy'), and GEOMETRY('OGC:CRS84'). An
alias gives its name, for example JSON. The function is pure, so an error of a
FromField instance can name the type with no native handle, and the text costs
nothing until an error renders.

These rules follow LogicalType::ToString of DuckDB 2.0.0:

- A scalar type has the name of its type id (logicalTypeIdName in
  "Database.DuckDB.Simple.Native"), and SQL NULL is "NULL" in double quotes. A
  VARCHAR with a collation is VARCHAR: the text has no collation.
- A label of an ENUM and a CRS are in single quotes, with each single quote
  doubled.
- A field name of a STRUCT and a member name of a UNION are in double quotes,
  with each double quote doubled, when the name needs quotes. A name needs
  quotes when it has a character that is not an ASCII letter, a digit, or an
  underscore, when it starts with a digit or an underscore followed by a digit,
  or when it is a keyword of DuckDB in any case (SQLIdentifier::ToString).
  The keywords are the 502 names of duckdb_keywords() of DuckDB 2.0.0.
- An empty STRUCT or TUPLE has only its type name, with no parentheses.

A test case compares the text with duckdb_v2_logical_type_to_text for one type of
each kind and for a STRUCT with each keyword as a field name.
-}

-- | The SQL text of a type, as DuckDB gives it, for example DECIMAL(18,3) or
-- JSON. See Note [Type texts].
logicalTypeSQL :: LogicalTypeRep -> Text
logicalTypeSQL = \case
  LogicalTypeScalar LogicalTypeIdSQLNull -> "\"NULL\""
  LogicalTypeScalar ident -> logicalTypeIdName ident
  LogicalTypeDecimal width scale -> "DECIMAL(" <> number width <> "," <> number scale <> ")"
  LogicalTypeCollatedVarchar _ -> "VARCHAR"
  LogicalTypeGeometry Nothing -> "GEOMETRY"
  LogicalTypeGeometry (Just crs) -> "GEOMETRY(" <> quoted '\'' crs <> ")"
  LogicalTypeList element -> logicalTypeSQL element <> "[]"
  LogicalTypeArray element size -> logicalTypeSQL element <> "[" <> number size <> "]"
  LogicalTypeMap key value -> "MAP(" <> logicalTypeSQL key <> ", " <> logicalTypeSQL value <> ")"
  LogicalTypeStruct fields
    | null (elems fields) -> "STRUCT"
    | otherwise -> "STRUCT(" <> commas [identifier name <> " " <> logicalTypeSQL rep | StructField name rep <- elems fields] <> ")"
  LogicalTypeTuple fields
    | null (elems fields) -> "TUPLE"
    | otherwise -> "TUPLE(" <> commas (map logicalTypeSQL (elems fields)) <> ")"
  LogicalTypeUnion members -> "UNION(" <> commas [identifier name <> " " <> logicalTypeSQL rep | UnionMemberType name rep <- elems members] <> ")"
  LogicalTypeEnum labels -> "ENUM(" <> commas (map (quoted '\'') (elems labels)) <> ")"
  LogicalTypeAlias name _ -> name
  where
    number :: (Show n) => n -> Text
    number = Text.pack . show
    commas = Text.intercalate ", "
    quoted mark text = Text.singleton mark <> Text.replace (Text.singleton mark) (Text.pack [mark, mark]) text <> Text.singleton mark
    identifier name
      | needsQuotes name = quoted '"' name
      | otherwise = name
    needsQuotes name =
      Text.null name
        || (Text.isPrefixOf "_" name && maybe False (isDigit . fst) (Text.uncons (Text.drop 1 name)))
        || not (and (zipWith plain [0 :: Int ..] (Text.unpack name)))
        || Set.member (Text.toLower name) duckDBKeywords
    plain index char = isAsciiLower char || isAsciiUpper char || char == '_' || (index > 0 && isDigit char)

-- | The keywords of DuckDB 2.0.0, from duckdb_keywords(). A field name that is
-- a keyword needs quotes. See Note [Type texts].
duckDBKeywords :: Set Text
duckDBKeywords =
  Set.fromList . Text.words $
    "abort absolute access action add admin after aggregate all also alter always analyse analyze and anti any \
    \ approx array as asc asof assertion assignment asymmetric at attach attribute authorization backward before \
    \ begin between bigint binary bit boolean both by cache call called cascade cascaded case cast catalog centuries \
    \ century chain char character characteristics check checkpoint class close cluster coalesce collate collation \
    \ column columns comment comments commit committed compression concurrently configuration conflict connect \
    \ connection constraint constraints content continue conversion copy cost create cross csv cube current cursor \
    \ cycle data database day days deallocate dec decade decades decimal declare default defaults deferrable \
    \ deferred definer delete delimiter delimiters depends desc describe destroy detach dictionary disable discard \
    \ disconnect distance distinct do document domain double drop each else enable encoding encrypted end enum error \
    \ escape event exact except exclude excluding exclusive execute exists explain export export_state extension \
    \ extensions external extract false family fetch filter first float following for force foreign forward freeze \
    \ from full function functions generated glob global grant granted group grouping grouping_id groups handler \
    \ having header hold hour hours identity if ignore ilike immediate immutable implicit import in include \
    \ including increment index indexes inherit inherits initially inline inner inout input insensitive insert \
    \ install instead int integer intersect interval into invoker is isnull isolation join json key label lambda \
    \ language large last lateral leading leakproof left level like limit listen load local location lock locked \
    \ logged macro map mapping match matched materialized maxvalue merge method microsecond microseconds millennia \
    \ millennium millisecond milliseconds minute minutes minvalue mode month months move name names national natural \
    \ nchar nearest new next no none not nothing notify notnull nowait null nullif nulls numeric object of off \
    \ offset oids old on only operator option options or order ordinality others out outer over overlaps overlay \
    \ overriding owned owner parallel parser partial partition partitioned passing password percent persistent pivot \
    \ pivot_longer pivot_wider placing plans policy position positional pragma preceding precision prepare prepared \
    \ preserve primary prior privileges procedural procedure program publication qualify quarter quarters quote \
    \ range read real reassign recheck recursive ref references referencing refresh register reindex relative \
    \ release rename repeatable replace replica reset resource resources respect restart restrict returning returns \
    \ revoke right role rollback rollup row rows rule sample savepoint schema schemas scope scroll search second \
    \ seconds secret secure security select semi sequence sequences serializable server session set setof sets share \
    \ show similar similarity simple skip smallint snapshot some sorted source sql stable standalone start statement \
    \ statistics stdin stdout storage stored strict strip struct subscription substring summarize symmetric sysid \
    \ system table tables tablesample tablespace target temp template temporary text then ties time timestamp to \
    \ trailing transaction transform treat trigger trim true truncate trusted try_cast tuple type types unbounded \
    \ uncommitted unencrypted union unique unknown unlisten unlogged unpack unpivot until update use user using \
    \ vacuum valid validate validator value values varchar variable variadic varying verbose version view views \
    \ virtual volatile week weeks when where whitespace window with within without work wrapper write xml \
    \ xmlattributes xmlconcat xmlelement xmlexists xmlforest xmlnamespaces xmlparse xmlpi xmlroot xmlserialize \
    \ xmltable year years yes zone"

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
