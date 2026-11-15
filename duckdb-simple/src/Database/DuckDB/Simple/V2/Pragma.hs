{-# LANGUAGE BlockArguments #-}

-- |
-- Module      : Database.DuckDB.Simple.V2.Pragma
-- Description : Register PRAGMA functions with callbacks that get native handles.
--
-- The callbacks of this module get the borrowed bind info and context of the
-- call. "Database.DuckDB.Simple.Pragma" has the typed callbacks.
--
-- Use this module to read the arguments of a call as native values, to set the documentation of a pragma,
-- or to register a pragma on an extension target. 'registerPragmaFunctionOn' declares the name and the
-- parameters. 'registerPragmaFunctionWith' gives the native builder to a configuration action. A pragma has
-- one callback: a 'QueryPragma' or an 'ExecPragma'.
--
-- The example uses the imports and the definitions of the module header of
-- "Database.DuckDB.Simple.V2.Registration".
module Database.DuckDB.Simple.V2.Pragma (
  PragmaCallback (..),
  registerPragmaFunctionOn,
  registerPragmaFunctionWith,
) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (attachOpaque, guardContext, install, runCallback, targetConfig, withCallbackResources, withTarget)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.V2.Registration (
  ContextCallback,
  FunctionParameter,
  FunctionSignature (..),
  RegistrationTarget (..),
  configureFunctionSignature,
  withRegistrationFactory,
 )

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
--
-- The example registers an execution pragma that counts its calls. The configuration action sets the name
-- and the description of the pragma.
--
-- > withConnection ":memory:" \connection -> do
-- >   calls <- newIORef (0 :: Int)
-- >   Pragma.registerPragmaFunctionWith
-- >     (R.ConnectionTarget connection)
-- >     ( \function -> do
-- >         R.withIdentifier "managed_exec" (R.checked . c_duckdb_v2_pragma_function_set_name function)
-- >         docs <- R.output (c_duckdb_v2_pragma_function_get_docs function)
-- >         Docs.setFunctionDocs docs Docs.defaultFunctionDocs{Docs.functionDescription = Just "Count calls."}
-- >     )
-- >     (Pragma.ExecPragma (\_ _ -> modifyIORef' calls (+ 1)))
-- >   _ <- execute_ connection "PRAGMA managed_exec"
-- >   readIORef calls >>= print -- 1
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
        (targetConfig target)
        ( \allocate -> case callback of
            QueryPragma action ->
              install
                allocate
                DuckDBV2PragmaFunctionQueryCallbackFn
                ( DuckDBV2PragmaFunctionQueryCallbackFn_Aux \info result context err ->
                    runCallback err do
                      sql <- action info context
                      withText sql (checked . c_duckdb_v2_pragma_function_query_set_query result)
                )
                (c_duckdb_v2_pragma_function_set_query_callback function)
            ExecPragma action ->
              install
                allocate
                DuckDBV2PragmaFunctionExecCallbackFn
                (DuckDBV2PragmaFunctionExecCallbackFn_Aux (guardContext action))
                (c_duckdb_v2_pragma_function_set_exec_callback function)
        )
        (attachOpaque (c_duckdb_v2_pragma_function_set_user_data function))
        \installCallback -> do
          configure function
          installCallback
          register
