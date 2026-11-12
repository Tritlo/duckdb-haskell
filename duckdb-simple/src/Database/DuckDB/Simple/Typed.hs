{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.Typed
Description : Read and write callback vectors as typed Haskell values.

Typed registrations use these helpers. An argument reader converts the rows of
a borrowed vector with 'FromField'. The module also registers typed casts on a
connection or on a loading extension.
-}
module Database.DuckDB.Simple.Typed (
    prepareArgumentReader,
    createCastOn,
) where

import Control.Exception (SomeException, bracket, catch, displayException, fromException, throwIO, try)
import Control.Monad (forM, when)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Element (vectorElementType)
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), FromField (..), ResultError (..))
import Database.DuckDB.Simple.Materialize (prepareVectorReader)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.Registration (RegistrationTarget, TypeDeclaration, registerCastWith, withRegistrationFactory, withTypeDeclaration)
import Database.DuckDB.Simple.ToField (ToDuckValue (..))

{- | Prepare a reader for the rows of an argument vector. The first argument is
the position of the argument, from 0. A NULL row gives 'Nothing'. A value that
'FromField' cannot convert raises an error. The error names the argument, the
SQL type, and the Haskell type. A VARIANT or GEOMETRY row raises an error. The
reader must not outlive the vector.
-}
prepareArgumentReader :: (FromField a) => Int -> DuckDBV2VectorHandle -> IO (Int -> IO (Maybe a))
prepareArgumentReader index vector = do
    readValue <- prepareVectorReader (\_ -> pure \_ -> throwIO (userError "duckdb-simple: a typed argument cannot be VARIANT or GEOMETRY")) vector
    pure \row ->
        readValue row >>= \case
            FieldNull -> pure Nothing
            value -> case fromField (Field (Text.pack ("arg" <> show index)) index value) of
                Ok converted -> pure (Just converted)
                Errors failures -> throwIO (userError (conversionMessage index failures))

-- | Describe the errors of an argument that 'FromField' cannot convert.
conversionMessage :: Int -> [SomeException] -> String
conversionMessage index failures =
    "duckdb-simple: cannot convert argument " <> show (index + 1) <> " " <> intercalate "; " (map describe failures)
  where
    describe failure = case fromException failure of
        Just result ->
            "from "
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
    let setNull row = checked (c_duckdb_v2_vector_set_null vector (fromIntegral row))
        castRow row =
            readArgument row
                >>= traverse (either (throwIO . userError . Text.unpack) pure . convert)
                >>= maybe (setNull row) (writeCastValue factory vectorType vector row)
    failures <- forM [0 .. count - 1] \row ->
        try (castRow row) >>= \case
            Right () -> pure Nothing
            Left (failure :: SomeException) -> Just failure <$ setNull row
    case catMaybes failures of
        failure : _ -> throwIO failure
        [] -> pure ()

{- | Write one value of a cast result. DuckDB casts the value to the type of
the vector with its default cast. DuckDB 2.0.0 has no default cast from a
BIGNUM, BIT, BLOB, DATE, TIME, TIMESTAMP, TIMESTAMPTZ, or UUID value to a custom
type with the same base type. It casts VARCHAR to such a type. When the default
cast fails for a value of the base type of the vector, write the text of the
value. That text comes from the default cast to VARCHAR, which does not read
the settings of the session.
-}
writeCastValue :: (ToDuckValue b) => DuckDBV2FactoryHandle -> DuckDBV2LogicalTypeId -> DuckDBV2VectorHandle -> Int -> b -> IO ()
writeCastValue factory vectorType vector row value =
    bracket (toDuckValue factory value) destroyValue \native ->
        write native `catch` \(failure :: SQLError) -> do
            valueType <- bracket (output (c_duckdb_v2_value_get_logical_type native)) destroyLogicalType (output . c_duckdb_v2_logical_type_get_id)
            when (valueType /= vectorType) $ throwIO failure
            text <- valueText native
            written <- try (bracket (toDuckValue factory text) destroyValue write)
            either (\(_ :: SQLError) -> throwIO failure) pure written
  where
    write = checked . c_duckdb_v2_vector_set_value vector (fromIntegral row)
