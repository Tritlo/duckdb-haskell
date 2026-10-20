{-# LANGUAGE BlockArguments #-}

-- | Managed query and execution callbacks for native PRAGMA registration.
module Database.DuckDB.Simple.Pragma (
    PragmaCallback (..),
    registerPragmaFunction,
    registerPragmaFunctionWith,
) where

import Control.Exception (bracket)
import Data.Text (Text)
import Database.DuckDB.Simple.Callback (attachOpaque, runCallback, withCallbackResources)
import Database.DuckDB.Simple.Internal (Connection, withConnectionHandle)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Registration (ContextCallback, FunctionParameter, FunctionSignature (..), RegistrationTarget (..), configureFunctionSignature)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)

{- | A query callback returns SQL to run in place of the PRAGMA statement.
An execution callback performs an action and returns no rows. Callback handles
are borrowed. Exceptions become SQL errors. DuckDB permits exactly one callback.
-}
data PragmaCallback
    = QueryPragma (DuckDBV2FunctionBindInfoHandle -> DuckDBV2ContextHandle -> IO Text)
    | ExecPragma (ContextCallback DuckDBV2FunctionBindInfoHandle)

-- | Register a pragma with named, default, or variadic parameters and no return type.
registerPragmaFunction :: Connection -> Text -> [FunctionParameter] -> PragmaCallback -> IO ()
registerPragmaFunction connection name parameters callback =
    withConnectionHandle connection \native ->
        registerPragmaFunctionWith
            (ConnectionTarget connection)
            ( \function -> do
                withIdentifier name (checked . c_duckdb_v2_pragma_function_set_name function)
                signature <- output (c_duckdb_v2_pragma_function_get_signature function)
                configureFunctionSignature native (FunctionSignature parameters Nothing) signature
            )
            callback

{- | Configure a pragma on a connection or a loading extension.
The action borrows the builder and can set its name, signature, and documentation.
Do not set native callbacks or user data. The library owns the callback pointer.
Use Haskell closures to retain registration data.
-}
registerPragmaFunctionWith :: RegistrationTarget -> (DuckDBV2PragmaFunctionHandle -> IO ()) -> PragmaCallback -> IO ()
registerPragmaFunctionWith target configure callback = case target of
    ConnectionTarget connection -> withConnectionHandle connection \native ->
        withConnectionFactory native \factory ->
            build factory (c_duckdb_v2_connection_register_pragma_function native)
    ExtensionTarget extension context -> withContextFactory context \factory ->
        build factory (c_duckdb_v2_extension_register_pragma_function extension)
  where
    build factory register =
        bracket (output (c_duckdb_v2_pragma_function_create factory)) (destroy c_duckdb_v2_pragma_function_destroy) \function ->
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
                    checked (register function)
