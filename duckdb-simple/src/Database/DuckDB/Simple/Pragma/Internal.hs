{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.DuckDB.Simple.Pragma.Internal
-- Description : Share the typed PRAGMA registration for connections and extensions.
--
-- 'registerPragmaFunctionOn' takes a registration target.
-- "Database.DuckDB.Simple.Pragma" gives the connection form.
module Database.DuckDB.Simple.Pragma.Internal (
  PragmaCallback (..),
  registerPragmaFunction,
  registerPragmaFunctionOn,
) where

import Control.Exception (bracket, throwIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.FromField (Field (..))
import Database.DuckDB.Simple.FromRow (FromRow (..), parseRow, rowErrorsToSqlError)
import Database.DuckDB.Simple.Internal (Connection)
import Database.DuckDB.Simple.Materialize (materializeValue, rejectRows)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import qualified Database.DuckDB.Simple.V2.Pragma as V2
import Database.DuckDB.Simple.V2.Registration (FunctionParameter, RegistrationTarget (..))
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray)

-- | The callback of a pragma. The argument @args@ is the row of the
-- arguments of the call. A query callback gives SQL. DuckDB runs that SQL in
-- place of the PRAGMA statement. An execution callback does an action and gives
-- no rows. An exception becomes an SQL error.
data PragmaCallback args
  = QueryPragma (args -> IO Text)
  | ExecPragma (args -> IO ())

-- | Register a pragma on a connection. The pragma has no return type.
--
-- The row has one field for each argument, in this order: the positional-only
-- parameters, the standard parameters, the arguments of the positional variadic
-- parameter, the named-only parameters, and the arguments of the named variadic
-- parameter. Parameters of one kind keep the order of the declaration. The
-- arguments of a variadic parameter keep the order of the call. Thus, for
-- parameters from 'Database.DuckDB.Simple.Registration.parameter', the row has
-- one field for each parameter, in the order of the declaration.
--
-- A call gives a standard parameter by position or by name, for example
-- @PRAGMA p(1, label = 'x')@. The field is at the position of the parameter in
-- both cases. When the call omits a parameter, the field has the default value of
-- the parameter. A NULL argument needs a 'Maybe' field. When the row does not
-- decode, the call fails with an SQL error that names the pragma.
--
-- > registerPragmaFunction conn "repeat_answer" [parameter "times" (TypeExpression "BIGINT")] $
-- >     QueryPragma \(Only (times :: Int64)) -> pure ("SELECT 42 FROM range(" <> Text.pack (show times) <> ")")
registerPragmaFunction :: (FromRow args) => Connection -> Text -> [FunctionParameter] -> PragmaCallback args -> IO ()
registerPragmaFunction = registerPragmaFunctionOn . ConnectionTarget

-- | Register a pragma on a connection or a loading extension.
-- 'registerPragmaFunction' gives the rules for the argument row.
registerPragmaFunctionOn :: (FromRow args) => RegistrationTarget -> Text -> [FunctionParameter] -> PragmaCallback args -> IO ()
registerPragmaFunctionOn target name parameters callback =
  V2.registerPragmaFunctionOn target name parameters case callback of
    QueryPragma action -> V2.QueryPragma \info context -> pragmaArguments name info context >>= action
    ExecPragma action -> V2.ExecPragma \info context -> pragmaArguments name info context >>= action

-- | Copy the arguments of a pragma call and decode them as one row. The first
-- argument is the name of the pragma for the error message.
pragmaArguments :: (FromRow args) => Text -> DuckDBV2FunctionBindInfoHandle -> DuckDBV2ContextHandle -> IO args
pragmaArguments name info context = withContextFactory context \factory -> do
  count <- allocaArray 4 \counts -> do
    checked (c_duckdb_v2_function_bind_get_arg_count info counts (advancePtr counts 1) (advancePtr counts 2) (advancePtr counts 3))
    sum <$> peekArray 4 counts
  fields <- mapM (argumentField factory) (indices count)
  case parseRow fromRow fields of
    Ok args -> pure args
    Errors failures -> do
      let message = sqlErrorMessage (rowErrorsToSqlError (Query name) failures)
      throwIO
        ( userError
            ( Text.unpack
                ("duckdb-simple: cannot decode the arguments of pragma " <> name <> ": " <> fromMaybe message (Text.stripPrefix "duckdb-simple: " message))
            )
        )
  where
    argumentField factory index = do
      argument <- output (c_duckdb_v2_function_bind_get_arg_name info index) >>= readIdentifier
      value <-
        bracket
          (output (c_duckdb_v2_function_bind_get_arg_value info index))
          (destroy c_duckdb_v2_value_destroy)
          (materializeValue noVariant factory)
      pure (Field argument (fromIntegral index) value)
    noVariant = rejectRows (throwIO (userError "duckdb-simple: the type of a pragma argument cannot be or contain VARIANT"))
