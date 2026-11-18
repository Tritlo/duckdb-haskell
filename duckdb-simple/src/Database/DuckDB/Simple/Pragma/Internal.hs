{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database.DuckDB.Simple.Pragma.Internal
-- Description : Share the typed PRAGMA registration for connections and extensions.
--
-- The cabal file lists this hidden module in @other-modules@.
-- 'registerPragmaFunctionOn' takes a registration target.
-- "Database.DuckDB.Simple.Pragma" exports the connection form.
-- "Database.DuckDB.Simple.Extension" exports the extension form.
module Database.DuckDB.Simple.Pragma.Internal (
  PragmaCallback (..),
  registerPragmaFunction,
  registerPragmaFunctionOn,
) where

import Control.Exception (throwIO)
import Control.Monad (forM, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (targetCastFactory)
import Database.DuckDB.Simple.Conversion (runConversion)
import Database.DuckDB.Simple.FromRow (FromRow (..), parseRow, rowErrorsToSqlError)
import Database.DuckDB.Simple.Internal (Connection)
import Database.DuckDB.Simple.Materialize (callbackReadFactory)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.TypeScope (TypeScope, withTypeScope)
import Database.DuckDB.Simple.Typed (readBoundField)
import qualified Database.DuckDB.Simple.V2.Pragma as V2
import Database.DuckDB.Simple.V2.Registration (
  FunctionParameter,
  RegistrationTarget (..),
 )
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
-- @PRAGMA p(1, label = \'x\')@. The field is at the position of the parameter in
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
registerPragmaFunctionOn target name parameters callback = do
  -- Check the name before the read of the state of the target, as each
  -- registration does. 'V2.registerPragmaFunctionOn' checks it again.
  when (Text.null name || Text.any (== '\0') name) $
    throwInvalidArgument "pragma function name must be nonempty and must not contain NUL"
  casts <- targetCastFactory target
  V2.registerPragmaFunctionOn target name parameters case callback of
    QueryPragma action -> V2.QueryPragma \info context -> withTypeScope \scope -> pragmaArguments scope casts name info context >>= action
    ExecPragma action -> V2.ExecPragma \info context -> withTypeScope \scope -> pragmaArguments scope casts name info context >>= action

-- | Copy the arguments of a pragma call and decode them as one row. The first
-- argument is the name of the pragma for the error message.
pragmaArguments
  :: (FromRow args) => TypeScope -> Maybe DuckDBV2FactoryHandle -> Text -> DuckDBV2FunctionBindInfoHandle -> DuckDBV2ContextHandle -> IO args
pragmaArguments scope casts name info context = withContextFactory context \contextFactory -> do
  let factory = callbackReadFactory casts contextFactory
  count <- allocaArray 4 \counts -> do
    checked (c_duckdb_v2_function_bind_get_arg_count info counts (advancePtr counts 1) (advancePtr counts 2) (advancePtr counts 3))
    sum <$> peekArray 4 counts
  fields <- forM (indices count) \index -> do
    argument <- output (c_duckdb_v2_function_bind_get_arg_name info index) >>= readIdentifier
    readBoundField scope factory info argument index
  converted <- runConversion (parseRow fromRow fields)
  case converted of
    Ok args -> pure args
    Errors failures -> do
      let message = sqlErrorMessage (rowErrorsToSqlError (Query name) failures)
      throwIO
        ( userError
            ( Text.unpack
                ("duckdb-simple: cannot decode the arguments of pragma " <> name <> ": " <> fromMaybe message (Text.stripPrefix "duckdb-simple: " message))
            )
        )
