{-# LANGUAGE BlockArguments #-}

-- |
-- Module      : Database.DuckDB.Simple.V2.Pragma
-- Description : Register PRAGMA functions with callbacks that get native handles.
--
-- The callbacks of this module get the borrowed bind info and context of the
-- call. "Database.DuckDB.Simple.Pragma" has the typed callbacks.
module Database.DuckDB.Simple.V2.Pragma (
  PragmaCallback (..),
  registerPragmaFunctionOn,
  registerPragmaFunctionWith,
) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (attachOpaque, runCallback, withCallbackResources, withTarget)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.V2.Registration (
  ContextCallback,
  FunctionParameter,
  FunctionSignature (..),
  RegistrationTarget (..),
  configureFunctionSignature,
  withRegistrationFactory,
 )
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)

-- | A query callback returns SQL to run in place of the PRAGMA statement.
-- An execution callback performs an action and returns no rows. Callback handles
-- are borrowed. Exceptions become SQL errors. DuckDB permits exactly one callback.
data PragmaCallback
  = QueryPragma (DuckDBV2FunctionBindInfoHandle -> DuckDBV2ContextHandle -> IO Text)
  | ExecPragma (ContextCallback DuckDBV2FunctionBindInfoHandle)

-- | Register a pragma on a connection or a loading extension. The name must not
-- be empty and must not contain NUL.
registerPragmaFunctionOn :: RegistrationTarget -> Text -> [FunctionParameter] -> PragmaCallback -> IO ()
registerPragmaFunctionOn target name parameters callback = do
  when (Text.null name || Text.any (== '\0') name) $
    throwInvalidArgument (Text.pack "pragma function name must be nonempty and must not contain NUL")
  withRegistrationFactory target \factory ->
    registerPragmaFunctionWith
      target
      ( \function -> do
          withIdentifier name (checked . c_duckdb_v2_pragma_function_set_name function)
          signature <- output (c_duckdb_v2_pragma_function_get_signature function)
          configureFunctionSignature factory (FunctionSignature parameters Nothing) signature
      )
      callback

-- | Configure a pragma on a connection or a loading extension.
-- The action borrows the builder and can set its name, signature, and documentation.
-- Do not set native callbacks or user data. The library owns the callback pointer.
-- Use Haskell closures to retain registration data.
registerPragmaFunctionWith :: RegistrationTarget -> (DuckDBV2PragmaFunctionHandle -> IO ()) -> PragmaCallback -> IO ()
registerPragmaFunctionWith target configure callback =
  withTarget
    target
    c_duckdb_v2_pragma_function_create
    c_duckdb_v2_pragma_function_destroy
    c_duckdb_v2_connection_register_pragma_function
    c_duckdb_v2_extension_register_pragma_function
    \function register ->
      withCallbackResources
        ( \allocate -> case callback of
            QueryPragma action -> do
              pointer <- allocate $ toFunPtr $ DuckDBV2PragmaFunctionQueryCallbackFn_Aux \info result context err ->
                runCallback err do
                  sql <- action info context
                  withText sql (checked . c_duckdb_v2_pragma_function_query_set_query result)
              pure (checked (c_duckdb_v2_pragma_function_set_query_callback function (DuckDBV2PragmaFunctionQueryCallbackFn pointer)))
            ExecPragma action -> do
              pointer <- allocate $ toFunPtr $ DuckDBV2PragmaFunctionExecCallbackFn_Aux \info context err -> runCallback err (action info context)
              pure (checked (c_duckdb_v2_pragma_function_set_exec_callback function (DuckDBV2PragmaFunctionExecCallbackFn pointer)))
        )
        (attachOpaque (c_duckdb_v2_pragma_function_set_user_data function))
        \install -> do
          configure function
          install
          register
