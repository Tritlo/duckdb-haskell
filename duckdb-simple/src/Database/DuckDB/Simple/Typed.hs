{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Database.DuckDB.Simple.Typed
-- Description : Read and write callback vectors as typed Haskell values.
--
-- Typed registrations use these helpers. An argument reader gives the rows of a
-- borrowed vector as fields with the names from 'argumentName'.
-- 'conversionMessage' describes the arguments that do not convert. 'TypedRow'
-- gives the column types and the column values of a row. The module also
-- converts a typed replacement function to a native replacement scan callback.
module Database.DuckDB.Simple.Typed (
  TypedRow (..),
  ColumnValue (..),
  argumentName,
  rowDeclarations,
  checkArgumentType,
  variantArgumentReason,
  prepareArgumentReader,
  conversionMessage,
  Replacement (..),
  replacementScanCallback,
) where

import Control.Exception (SomeException, bracket, fromException)
import Control.Monad (forM, when)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Element (containsType)
import Database.DuckDB.Simple.FromField (Field (..), ResultError (..))
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..))
import Database.DuckDB.Simple.Materialize (
  FallbackReaders (..),
  ReadFactory,
  lazyTypeRep,
  prepareGeometryReader,
  prepareVectorReader,
  rejectRows,
 )
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), ToDuckValue (..))
import Database.DuckDB.Simple.Types (Only (..))
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeToRep)
import Database.DuckDB.Simple.V2.Registration (
  ContextCallback,
  TypeDeclaration,
 )
import GHC.Generics

-- | A row with a fixed list of typed columns. A typed registration uses the
-- column types in the signature of the SQL object, and writes the column values
-- to DuckDB. t'Only', tuples of 2 to 7 columns, and @()@ are rows. A record with a
-- 'Generic' instance is a row when each field has a 'ToDuckValue' instance:
--
-- > data Point = Point {x :: Double, y :: Double} deriving (Generic)
-- > instance TypedRow Point
--
-- A @Maybe@ column can hold NULL. A row of arguments has the same rule as a row
-- of results: each column type needs a 'ToDuckValue' instance, because that
-- instance gives the SQL type of the column.
class TypedRow r where
  -- | The declaration of each column, from 'duckdbTypeDeclaration'. See
  -- Note [Type declarations] in "Database.DuckDB.Simple.ToField".
  rowColumnDeclarations :: Proxy r -> [Either Text TypeDeclaration]
  default rowColumnDeclarations :: (GTypedRow (Rep r)) => Proxy r -> [Either Text TypeDeclaration]
  rowColumnDeclarations _ = gRowColumnDeclarations (Proxy @(Rep r))

  -- | The value of each column.
  rowColumnValues :: r -> [ColumnValue]
  default rowColumnValues :: (Generic r, GTypedRow (Rep r)) => r -> [ColumnValue]
  rowColumnValues = gRowColumnValues . from

-- | The value of one column of a row. The action makes an owned native value
-- through a borrowed factory. The caller destroys the value.
newtype ColumnValue = ColumnValue (DuckDBV2FactoryHandle -> IO DuckDBV2ValueHandle)

-- | The generic implementation of 'TypedRow' for a type with one constructor.
class GTypedRow f where
  gRowColumnDeclarations :: Proxy f -> [Either Text TypeDeclaration]
  gRowColumnValues :: f p -> [ColumnValue]

instance GTypedRow U1 where
  gRowColumnDeclarations _ = []
  gRowColumnValues U1 = []

instance (GTypedRow a) => GTypedRow (M1 i c a) where
  gRowColumnDeclarations _ = gRowColumnDeclarations (Proxy @a)
  gRowColumnValues (M1 value) = gRowColumnValues value

instance (ToDuckValue a) => GTypedRow (K1 i a) where
  gRowColumnDeclarations _ = [duckdbTypeDeclaration (Proxy @a)]
  gRowColumnValues (K1 value) = [ColumnValue (`toDuckValue` value)]

instance (GTypedRow a, GTypedRow b) => GTypedRow (a :*: b) where
  gRowColumnDeclarations _ = gRowColumnDeclarations (Proxy @a) <> gRowColumnDeclarations (Proxy @b)
  gRowColumnValues (left :*: right) = gRowColumnValues left <> gRowColumnValues right

instance TypedRow ()

instance (ToDuckValue a) => TypedRow (Only a) where
  rowColumnDeclarations _ = [duckdbTypeDeclaration (Proxy @a)]
  rowColumnValues (Only value) = [ColumnValue (`toDuckValue` value)]

instance (ToDuckValue a, ToDuckValue b) => TypedRow (a, b)

instance (ToDuckValue a, ToDuckValue b, ToDuckValue c) => TypedRow (a, b, c)

instance (ToDuckValue a, ToDuckValue b, ToDuckValue c, ToDuckValue d) => TypedRow (a, b, c, d)

instance (ToDuckValue a, ToDuckValue b, ToDuckValue c, ToDuckValue d, ToDuckValue e) => TypedRow (a, b, c, d, e)

instance (ToDuckValue a, ToDuckValue b, ToDuckValue c, ToDuckValue d, ToDuckValue e, ToDuckValue f) => TypedRow (a, b, c, d, e, f)

instance (ToDuckValue a, ToDuckValue b, ToDuckValue c, ToDuckValue d, ToDuckValue e, ToDuckValue f, ToDuckValue g) => TypedRow (a, b, c, d, e, f, g)

-- | The name of the argument at an index, from 0: @arg0@, @arg1@, and so on.
argumentName :: Int -> Text
argumentName index = Text.pack ("arg" <> show index)

-- | Give the declaration of each column of a row, or raise the first 'Left' as
-- an @SQLError@ before a registration. The error names the registration and
-- the place of the column. The list of places has a name for each column. See
-- Note [Type declarations] in "Database.DuckDB.Simple.ToField".
rowDeclarations :: Text -> [Text] -> [Either Text TypeDeclaration] -> IO [TypeDeclaration]
rowDeclarations name places declarations =
  forM (zip places declarations) \(place, declaration) ->
    either (\reason -> throwInvalidArgument (Text.intercalate (Text.pack ": ") [name, place, reason])) pure declaration

-- | The reason in the error for a VARIANT argument. It names the limit and
-- what a user can write in its place. 'checkArgumentType' and the reader of a
-- pragma argument of the type ANY use it.
variantArgumentReason :: Text
variantArgumentReason =
  Text.pack
    "the library cannot read a VARIANT value in a callback; declare another type, such as VARCHAR, and cast the value in SQL, for example v::VARCHAR"

-- | Raise an error before a registration when the resolved type of an
-- argument is or contains VARIANT. A callback has no connection, so no argument
-- reader of a callback reads VARIANT ('prepareArgumentReader', and the readers
-- of a table function, a pragma, and the update function of a custom option).
-- Without this check the registration succeeds, a NULL value works, and each
-- value that is not NULL fails the query. When the first argument is False,
-- also raise the error for a type that is or contains GEOMETRY: the callbacks
-- of an aggregate function of an extension have no factory to read it. The
-- error names the registration and the place of the argument.
checkArgumentType :: Bool -> Text -> Text -> DuckDBV2LogicalTypeHandle -> IO ()
checkArgumentType readsGeometry name place logical = do
  rep <- logicalTypeToRep logical
  let refuse reason = throwInvalidArgument (Text.intercalate (Text.pack ": ") [name, place, reason])
  when (containsType isVariant rep) $
    refuse variantArgumentReason
  when (not readsGeometry && containsType isGeometry rep) $
    refuse (Text.pack "the type of an argument of an aggregate function of an extension cannot be or contain GEOMETRY")
  where
    isVariant = (== LogicalTypeScalar LogicalTypeIdVariant)
    isGeometry = \case
      LogicalTypeGeometry _ -> True
      _ -> False

-- | Prepare a reader for the rows of an argument vector. The second argument is
-- the index of the argument, from 0. The reader gives the field of a row, with
-- the name from 'argumentName'. A NULL row gives
-- 'Database.DuckDB.Simple.FromField.FieldNull', also a NULL VARIANT or GEOMETRY
-- row. A row that holds a VARIANT value raises an error. A GEOMETRY value reads
-- with the factory of the first argument, at each depth of the type. Without a
-- factory, a row that holds a GEOMETRY value raises an error. See
-- Note [User casts and the casts of the library] in
-- "Database.DuckDB.Simple.Materialize". The reader must not outlive the vector.
prepareArgumentReader :: Maybe ReadFactory -> Int -> DuckDBV2VectorHandle -> IO (Int -> IO Field)
prepareArgumentReader casts index vector = do
  let noVariant = rejectRows (throwInvalidArgument (Text.pack "the type of a typed argument cannot be or contain VARIANT"))
      noGeometry = rejectRows (throwInvalidArgument (Text.pack "a typed argument of this callback cannot be or contain GEOMETRY"))
  readValue <- prepareVectorReader (FallbackReaders noVariant (maybe noGeometry prepareGeometryReader casts)) vector
  -- See Note [Column types] in "Database.DuckDB.Simple.FromField".
  argumentType <- lazyTypeRep (output (c_duckdb_v2_vector_get_logical_type vector))
  pure \row -> (\value -> Field (argumentName index) index value argumentType) <$> readValue row

-- | Describe the arguments that 'Database.DuckDB.Simple.FromField.FromField' or
-- @FromRow@ cannot convert. The text names each argument with the name of its
-- field.
conversionMessage :: [SomeException] -> String
conversionMessage failures = intercalate "; " (map describe failures)
  where
    describe failure = case fromException failure of
      Just result@UnexpectedNull{} -> "argument " <> Text.unpack (errSQLField result) <> " is NULL, but " <> Text.unpack (errHaskellType result) <> " cannot hold NULL"
      Just result ->
        "cannot convert argument "
          <> Text.unpack (errSQLField result)
          <> " from "
          <> Text.unpack (errSQLType result)
          <> " to "
          <> Text.unpack (errHaskellType result)
          <> if Text.null (errMessage result) then "" else ": " <> Text.unpack (errMessage result)
      Nothing -> exceptionText failure

-- | What DuckDB reads in place of an unknown table. DuckDB reads the result
-- under the name of the table, unless the query gives an alias.
data Replacement
  = -- | Read the rows of a query. The text must contain exactly one SELECT
    --       statement. Other text fails the query with an SQL error.
    ReplaceWithQuery Text
  deriving (Eq, Show, Read)

-- | Convert a typed replacement function to a native replacement scan
-- callback. The callback reads the parts of the unknown name and gives them to the
-- function. 'Nothing' declines the name.
replacementScanCallback :: ([Text] -> IO (Maybe Replacement)) -> ContextCallback DuckDBV2ReplacementScanInfoHandle
replacementScanCallback replace info _ = do
  parts <- bracket (output (c_duckdb_v2_replacement_scan_get_name info)) (destroy c_duckdb_v2_qname_destroy) \name -> do
    count <- output (c_duckdb_v2_qname_get_part_count name)
    forM [0 .. fromIntegral count - 1 :: Int] \index -> output (c_duckdb_v2_qname_get_part name (fromIntegral index)) >>= readIdentifier
  replace parts >>= mapM_ \case
    ReplaceWithQuery sql -> withText sql (checked . c_duckdb_v2_replacement_scan_set_subquery info)
