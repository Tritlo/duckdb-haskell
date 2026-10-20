{-# LANGUAGE BlockArguments #-}

{- | Native scalar and table macro registration for a loading extension.
The current V2 API has no connection-scoped macro registration function.
Use SQL @CREATE MACRO@ for a connection.
-}
module Database.DuckDB.Simple.Macro (
    registerMacroFunction,
    registerMacroFunctionWith,
) where

import Control.Exception (bracket)
import Data.Text (Text)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Registration (FunctionParameter, FunctionSignature (..), configureFunctionSignatureWithFactory)

{- | Register a scalar expression or table SELECT as an extension macro.
Parameters must use the standard parameter kind. Use ANY for an untyped
parameter and 'Database.DuckDB.Simple.Registration.FactoryDefaultValue' for defaults.
DuckDB validates the body and signature. It resolves the body when the macro runs.
The extension and context handles must belong to the current load callback.
-}
registerMacroFunction ::
    DuckDBV2ExtensionHandle ->
    DuckDBV2ContextHandle ->
    Text ->
    DuckDBV2MacroFunctionType ->
    [FunctionParameter] ->
    Text ->
    IO ()
registerMacroFunction extension context name kind parameters body =
    withContextFactory context \factory ->
        registerMacroFunctionWith extension context kind \function -> do
            withIdentifier name (checked . c_duckdb_v2_macro_function_set_name function)
            withText body (checked . c_duckdb_v2_macro_function_set_body function)
            signature <- output (c_duckdb_v2_macro_function_get_signature function)
            configureFunctionSignatureWithFactory factory (FunctionSignature parameters Nothing) signature

{- | Configure and register a macro on the loading extension's database.
The configuration action borrows the builder. It can set the name, body,
signature, and documentation. DuckDB retains copies after this action returns.
-}
registerMacroFunctionWith ::
    DuckDBV2ExtensionHandle ->
    DuckDBV2ContextHandle ->
    DuckDBV2MacroFunctionType ->
    (DuckDBV2MacroFunctionHandle -> IO ()) ->
    IO ()
registerMacroFunctionWith extension context kind configure =
    withContextFactory context \factory ->
        bracket (output (c_duckdb_v2_macro_function_create factory kind)) (destroy c_duckdb_v2_macro_function_destroy) \function -> do
            configure function
            checked (c_duckdb_v2_extension_register_macro_function extension function)
