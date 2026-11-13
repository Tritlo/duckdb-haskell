{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Database.DuckDB.Simple.Typed
Description : Read and write callback vectors as typed Haskell values.

Typed registrations use these helpers. An argument reader converts the rows of
a borrowed vector with 'FromField'. 'TypedRow' gives the column types and the
column values of a row. The module also registers typed casts on a connection
or on a loading extension. It converts a typed replacement function to a native
replacement scan callback.
-}
module Database.DuckDB.Simple.Typed (
    TypedRow (..),
    ColumnValue (..),
    argumentName,
    prepareArgumentReader,
    conversionMessage,
    createCastOn,
    Replacement (..),
    replacementScanCallback,
) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, bracket, catch, displayException, fromException, throwIO, try)
import Control.Monad (forM, forM_, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Element (vectorElementType)
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), FromField (..), ResultError (..))
import Database.DuckDB.Simple.Materialize (prepareVectorReader)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.ToField (ToDuckValue (..), duckdbColumnType)
import Database.DuckDB.Simple.Types (Only (..))
import Database.DuckDB.Simple.V2.Registration (ContextCallback, RegistrationTarget, TypeDeclaration, registerCastWith, withRegistrationFactory, withTypeDeclaration)
import GHC.Generics

{- | A row with a fixed list of typed columns. A typed registration uses the
column types in the signature of the SQL object, and writes the column values
to DuckDB. 'Only', tuples of 2 to 7 columns, and @()@ are rows. A record with a
'Generic' instance is a row when each field has a 'ToDuckValue' instance:

> data Point = Point {x :: Double, y :: Double} deriving (Generic)
> instance TypedRow Point

A @Maybe@ column can hold NULL. A row of arguments has the same rule as a row
of results: each column type needs a 'ToDuckValue' instance, because that
instance gives the SQL type of the column.
-}
class TypedRow r where
    -- | The DuckDB type of each column, from 'duckdbColumnType'.
    rowColumnTypes :: Proxy r -> [Text]
    default rowColumnTypes :: (GTypedRow (Rep r)) => Proxy r -> [Text]
    rowColumnTypes _ = gRowColumnTypes (Proxy :: Proxy (Rep r))

    -- | The value of each column.
    rowColumnValues :: r -> [ColumnValue]
    default rowColumnValues :: (Generic r, GTypedRow (Rep r)) => r -> [ColumnValue]
    rowColumnValues = gRowColumnValues . from

{- | The value of one column of a row. The action makes an owned native value
through a borrowed factory. The caller destroys the value.
-}
newtype ColumnValue = ColumnValue (DuckDBV2FactoryHandle -> IO DuckDBV2ValueHandle)

-- | The generic implementation of 'TypedRow' for a type with one constructor.
class GTypedRow f where
    gRowColumnTypes :: Proxy f -> [Text]
    gRowColumnValues :: f p -> [ColumnValue]

instance GTypedRow U1 where
    gRowColumnTypes _ = []
    gRowColumnValues U1 = []

instance (GTypedRow a) => GTypedRow (M1 i c a) where
    gRowColumnTypes _ = gRowColumnTypes (Proxy :: Proxy a)
    gRowColumnValues (M1 value) = gRowColumnValues value

instance (ToDuckValue a) => GTypedRow (K1 i a) where
    gRowColumnTypes _ = [duckdbColumnType (Proxy :: Proxy a)]
    gRowColumnValues (K1 value) = [ColumnValue (`toDuckValue` value)]

instance (GTypedRow a, GTypedRow b) => GTypedRow (a :*: b) where
    gRowColumnTypes _ = gRowColumnTypes (Proxy :: Proxy a) <> gRowColumnTypes (Proxy :: Proxy b)
    gRowColumnValues (left :*: right) = gRowColumnValues left <> gRowColumnValues right

instance TypedRow ()

instance (ToDuckValue a) => TypedRow (Only a) where
    rowColumnTypes _ = [duckdbColumnType (Proxy :: Proxy a)]
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

{- | Prepare a reader for the rows of an argument vector. The first argument is
the index of the argument, from 0. The reader gives the field of a row, with
the name from 'argumentName'. A NULL row gives 'FieldNull'. A row of a type
that is or contains VARIANT or GEOMETRY raises an error. The reader must not
outlive the vector.
-}
prepareArgumentReader :: Int -> DuckDBV2VectorHandle -> IO (Int -> IO Field)
prepareArgumentReader index vector = do
    readValue <- prepareVectorReader (\_ -> pure \_ -> throwIO (userError "duckdb-simple: the type of a typed argument cannot be or contain VARIANT or GEOMETRY")) vector
    pure \row -> Field (argumentName index) index <$> readValue row

{- | Describe the arguments that 'FromField' or 'FromRow' cannot convert. The
text names each argument with the name of its field.
-}
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
        Nothing -> displayException failure

{- | Register an explicit typed cast on a connection or a loading extension.
'Database.DuckDB.Simple.Function.createCast' gives the rules for the rows.
-}
createCastOn :: (FromField a, ToDuckValue b) => RegistrationTarget -> TypeDeclaration -> TypeDeclaration -> (a -> Either Text b) -> IO ()
createCastOn target source destination convert =
    withRegistrationFactory target \factory ->
        registerCastWith
            target
            ( \function -> do
                withTypeDeclaration factory source $ checked . c_duckdb_v2_cast_function_set_source_type function
                withTypeDeclaration factory destination $ checked . c_duckdb_v2_cast_function_set_target_type function
            )
            (castRows convert)

{- | Convert the rows of one cast call. Write each row before an error leaves
the callback, because TRY_CAST keeps the output and ignores the error. A failed
row gets NULL. Then raise the error of the first failed row.
-}
castRows :: (FromField a, ToDuckValue b) => (a -> Either Text b) -> DuckDBV2CastFunctionExecInfoHandle -> DuckDBV2ContextHandle -> IO ()
castRows convert info context = withContextFactory context \factory -> do
    count <- fromIntegral <$> output (c_duckdb_v2_cast_function_exec_get_row_count info)
    readArgument <- output (c_duckdb_v2_cast_function_exec_get_input info) >>= prepareArgumentReader 0
    vector <- output (c_duckdb_v2_cast_function_exec_get_output info)
    -- The set operations check the row against the size. Flatten after the size changes.
    checked (c_duckdb_v2_vector_set_size vector (fromIntegral count))
    checked (c_duckdb_v2_vector_flatten vector)
    vectorType <- vectorElementType vector
    useText <- newIORef False
    let setNull row = checked (c_duckdb_v2_vector_set_null vector (fromIntegral row))
        castRow row =
            readArgument row >>= \case
                Field{fieldValue = FieldNull} -> setNull row
                field -> case fromField field of
                    Errors failures -> throwIO (userError ("duckdb-simple: " <> conversionMessage failures))
                    Ok value -> either (throwIO . userError . Text.unpack) (writeCastValue factory useText vectorType vector row) (convert value)
    -- Use a loop with a constant stack. The runtime reads the stack of the
    -- thread at each native call, so a deep stack makes each call slow.
    firstFailure <- newIORef Nothing
    forM_ [0 .. count - 1] \row ->
        -- Catch each exception, also an asynchronous one, and write NULL to the
        -- failed row. For a try cast, the header of
        -- duckdb_v2_cast_function_exec_get_mode says that "the engine discards
        -- the error and keeps whatever the callback left in the output". Thus
        -- the callback must write each row of the output vector before it
        -- returns.
        try (castRow row) >>= \case
            Right () -> pure ()
            Left (failure :: SomeException) -> do
                setNull row
                modifyIORef' firstFailure (<|> Just failure)
    readIORef firstFailure >>= mapM_ throwIO

{- | Write one value of a cast result. DuckDB casts the value to the type of
the vector with its default cast. DuckDB 2.0.0 has no default cast from a
BIGNUM, BIT, BLOB, DATE, TIME, TIMESTAMP, TIMESTAMPTZ, or UUID value to a custom
type with the same base type. It casts VARCHAR to such a type. When the default
cast fails for a value of the base type of the vector, write the text of the
value. That text comes from the default cast to VARCHAR, which does not read
the settings of the session. The flag then selects the text for the subsequent
values of the call, because each failed write costs one native error.
-}
writeCastValue :: (ToDuckValue b) => DuckDBV2FactoryHandle -> IORef Bool -> DuckDBV2LogicalTypeId -> DuckDBV2VectorHandle -> Int -> b -> IO ()
writeCastValue factory useText vectorType vector row value =
    bracket (toDuckValue factory value) destroyValue \native ->
        readIORef useText >>= \case
            True -> do
                -- A NULL value has no text. DuckDB writes it without a cast.
                isNull <- output (c_duckdb_v2_value_is_null native)
                if isNull /= 0 then write native else writeText native
            False ->
                write native `catch` \(failure :: SQLError) -> do
                    valueType <- bracket (output (c_duckdb_v2_value_get_logical_type native)) destroyLogicalType (output . c_duckdb_v2_logical_type_get_id)
                    when (valueType /= vectorType) $ throwIO failure
                    try (writeText native) >>= either (\(_ :: SQLError) -> throwIO failure) (\() -> writeIORef useText True)
  where
    write = checked . c_duckdb_v2_vector_set_value vector (fromIntegral row)
    writeText native = valueText native >>= \text -> bracket (toDuckValue factory text) destroyValue write

{- | What DuckDB reads in place of an unknown table. DuckDB reads the result
under the name of the table, unless the query gives an alias.
-}
data Replacement
    = {- | Read the rows of a query. The text must contain exactly one SELECT
      statement. Other text fails the query with an SQL error.
      -}
      ReplaceWithQuery Text
    deriving (Eq, Show, Read)

{- | Convert a typed replacement function to a native replacement scan
callback. The callback reads the parts of the unknown name and gives them to the
function. 'Nothing' declines the name.
-}
replacementScanCallback :: ([Text] -> IO (Maybe Replacement)) -> ContextCallback DuckDBV2ReplacementScanInfoHandle
replacementScanCallback replace info _ = do
    parts <- bracket (output (c_duckdb_v2_replacement_scan_get_name info)) (destroy c_duckdb_v2_qname_destroy) \name -> do
        count <- output (c_duckdb_v2_qname_get_part_count name)
        forM [0 .. fromIntegral count - 1 :: Int] \index -> output (c_duckdb_v2_qname_get_part name (fromIntegral index)) >>= readIdentifier
    replace parts >>= mapM_ \case
        ReplaceWithQuery sql -> withText sql (checked . c_duckdb_v2_replacement_scan_set_subquery info)
