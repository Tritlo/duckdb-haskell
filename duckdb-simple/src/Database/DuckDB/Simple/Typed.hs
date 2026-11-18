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
  positionalSignature,
  readBoundField,
  ArgumentReader,
  newArgumentReader,
  conversionMessage,
  Replacement (..),
  replacementScanCallback,
) where

import Control.Concurrent.MVar (modifyMVarMasked, newMVar)
import Control.Exception (SomeException, bracket, bracketOnError, finally, fromException, mask_)
import Control.Monad (forM, forM_)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (RegistrationTarget, targetCastFactory)
import Database.DuckDB.Simple.FromField (Field (..), ResultError (..))
import Database.DuckDB.Simple.Materialize (
  FallbackReaders (..),
  ReadFactory (InstanceFactory),
  createHelperInstance,
  materializeValue,
  prepareGeometryReader,
  prepareVectorReader,
  readCallbackVariant,
  readFactoryHandle,
  readInstanceVariant,
 )
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), ToDuckValue (..))
import Database.DuckDB.Simple.TypeScope (TypeScope, scopedTypeRep)
import Database.DuckDB.Simple.Types (Only (..))
import Database.DuckDB.Simple.V2.LogicalRep (logicalTypeToRep)
import Database.DuckDB.Simple.V2.Registration (
  ContextCallback,
  FunctionParameter (parameterKind),
  FunctionSignature (..),
  TypeDeclaration,
  parameter,
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

-- | Declare the positional arguments of a typed function. A table function
-- declares its result columns during bind and supplies no return type here.
positionalSignature :: [TypeDeclaration] -> Maybe TypeDeclaration -> FunctionSignature
positionalSignature types =
  FunctionSignature
    [ (parameter (argumentName index) dtype){parameterKind = DuckDBV2FunctionParameterKindPositionalOnly}
    | (index, dtype) <- zip [0 ..] types
    ]

-- | Copy one argument of a table function or pragma. Acquire its logical
-- type within the value's lifetime. The type scope owns it during conversion.
readBoundField :: TypeScope -> ReadFactory -> DuckDBV2FunctionBindInfoHandle -> Text -> DuckDBIdx -> IO Field
readBoundField scope factory info name index =
  bracket (output (c_duckdb_v2_function_bind_get_arg_value info index)) destroyValue \value ->
    Field name (fromIntegral index)
      <$> materializeValue readCallbackVariant factory value
      <*> scopedTypeRep scope (pure (readFactoryHandle factory)) (output (c_duckdb_v2_value_get_logical_type value))

{- Note [Function argument readers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A scalar or aggregate function belongs to the catalog. Its callbacks can
run after the registering connection closes. An extension has no managed
connection, and an aggregate update has no callback context.

A registration takes the host instance factory when available. An extension
opens a private instance on the first argument read or aggregate result
write that needs a factory. GEOMETRY inputs use its built-in cast to BLOB.
A VARIANT read always needs the private instance because its metadata query
can run while the caller holds a host instance lock. See Note [VARIANT reads inside callbacks]
in "Database.DuckDB.Simple.Materialize".

An MVar protects instance creation. Each VARIANT vector gets its own
connection and decoder state, so concurrent calls can decode independently.
The native callback owner releases the instance and its environment after
the last bound or executing function releases the callbacks.
-}

-- | Prepare a vector's fields within the current callback's type scope.
type ArgumentReader = TypeScope -> Int -> DuckDBV2VectorHandle -> IO (Int -> IO Field)

-- | Create the argument reader, the action to borrow its instance factory,
-- and the cleanup for one registration.
-- Native resources are acquired on demand. See Note [Function argument readers].
newArgumentReader :: RegistrationTarget -> IO (ArgumentReader, IO DuckDBV2FactoryHandle, IO ())
newArgumentReader target = do
  hostFactory <- targetCastFactory target
  cache <- newMVar Nothing
  let helper = modifyMVarMasked cache \case
        Just owned@(_, instance_) -> pure (Just owned, instance_)
        Nothing -> bracketOnError (output c_duckdb_v2_environment_create) (destroy c_duckdb_v2_environment_destroy) \env -> do
          instance_ <- createHelperInstance env
          pure (Just (env, instance_), instance_)
      close = mask_ do
        owned <- modifyMVarMasked cache (\state -> pure (Nothing, state))
        forM_ owned \(env, instance_) ->
          destroy c_duckdb_v2_instance_destroy instance_ `finally` destroy c_duckdb_v2_environment_destroy env
      factory = maybe (helper >>= output . c_duckdb_v2_instance_get_factory) pure hostFactory
      readers =
        FallbackReaders
          (\logical -> factory >>= \f -> logicalTypeToRep f logical)
          (\vector -> helper >>= \instance_ -> readInstanceVariant instance_ vector)
          (\vector -> factory >>= \f -> prepareGeometryReader (InstanceFactory f) vector)
      readArgument scope index vector = do
        readValue <- prepareVectorReader readers vector
        -- See Note [Scoped field types] in "Database.DuckDB.Simple.TypeScope".
        argumentType <- scopedTypeRep scope factory (output (c_duckdb_v2_vector_get_logical_type vector))
        pure \row -> (\value -> Field (argumentName index) index value argumentType) <$> readValue row
  pure (readArgument, factory, close)

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
