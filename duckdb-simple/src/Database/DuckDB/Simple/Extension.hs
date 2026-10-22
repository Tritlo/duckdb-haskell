{-# LANGUAGE BlockArguments #-}

{- | Entry support for a loadable V2 Haskell extension.
Build duckdb-ffi with its @extension@ flag. The C entrypoint must first call
@duckdb_haskell_extension_init_api@, then initialize the threaded GHC runtime,
then call the Haskell export. See @dev/extensions/@ in the repository.
-}
module Database.DuckDB.Simple.Extension (
    Extension,
    ExtensionEntry,
    runExtension,
    createFunction,
    createFunctionWithState,
    registerMacro,
    registerPragma,
    registerCustomOption,
    registerLogSink,
    withExtensionContext,
) where

import Data.Text (Text)
import Database.DuckDB.FFI.V2 (DuckDBV2ContextHandle, DuckDBV2ErrorInfoHandle, DuckDBV2ExtensionHandle)
import Database.DuckDB.Simple.Callback (runCallback)
import Database.DuckDB.Simple.Config (CustomOption)
import qualified Database.DuckDB.Simple.Config as Config
import Database.DuckDB.Simple.Function.Internal (Function, createFunctionOn, createFunctionWithStateOn)
import Database.DuckDB.Simple.Logging (LogEntry)
import qualified Database.DuckDB.Simple.Logging as Logging
import qualified Database.DuckDB.Simple.Macro as Macro
import Database.DuckDB.Simple.Native (DuckDBV2MacroFunctionType, c_duckdb_v2_pragma_function_get_signature, c_duckdb_v2_pragma_function_set_name, checked, output, withContextFactory, withIdentifier)
import Database.DuckDB.Simple.Pragma (PragmaCallback)
import qualified Database.DuckDB.Simple.Pragma as Pragma
import Database.DuckDB.Simple.Registration (FunctionParameter, FunctionSignature (..), RegistrationTarget (ExtensionTarget), configureFunctionSignatureWithFactory)
import Foreign.Ptr (Ptr)

{- | The borrowed registration target and context for one extension load.
Use this value only inside the action passed to 'runExtension'. Do not retain it
in registered callbacks or application state.
-}
data Extension = Extension DuckDBV2ExtensionHandle DuckDBV2ContextHandle

-- | The Haskell entrypoint called by the native load shim.
type ExtensionEntry = DuckDBV2ExtensionHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()

{- | Run registration and report exceptions through the borrowed load error slot.
The extension is valid only during this call. Registered functions own their
callback resources after this call returns.
-}
runExtension :: (Extension -> IO ()) -> ExtensionEntry
runExtension initialize extension context err =
    runCallback err (initialize (Extension extension context))

-- | Register a typed Haskell function with the loading DuckDB host.
createFunction :: (Function f) => Extension -> Text -> f -> IO ()
createFunction (Extension extension context) = createFunctionOn (ExtensionTarget extension context)

-- | Register a typed function with per-worker state owned by DuckDB.
createFunctionWithState :: (Function f) => Extension -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState (Extension extension context) = createFunctionWithStateOn (ExtensionTarget extension context)

-- | Register a scalar expression or a table SELECT on the loading extension.
registerMacro :: Extension -> Text -> DuckDBV2MacroFunctionType -> [FunctionParameter] -> Text -> IO ()
registerMacro (Extension extension context) = Macro.registerMacroFunction extension context

-- | Register a managed PRAGMA callback on the loading extension.
registerPragma :: Extension -> Text -> [FunctionParameter] -> PragmaCallback -> IO ()
registerPragma (Extension extension context) name parameters callback = withContextFactory context \factory ->
    Pragma.registerPragmaFunctionWith
        (ExtensionTarget extension context)
        ( \function -> do
            withIdentifier name (checked . c_duckdb_v2_pragma_function_set_name function)
            signature <- output (c_duckdb_v2_pragma_function_get_signature function)
            configureFunctionSignatureWithFactory factory (FunctionSignature parameters Nothing) signature
        )
        callback

{- | Register a typed option during extension initialization.
Use a factory default value. A connection default value requires a connection.
-}
registerCustomOption :: Extension -> CustomOption -> IO ()
registerCustomOption (Extension extension context) = Config.registerCustomOptionOn (ExtensionTarget extension context)

{- | Register a log sink with callback resources owned by DuckDB.
Callbacks can run concurrently. They must not log or change logging settings.
-}
registerLogSink :: Extension -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerLogSink (Extension extension context) = Logging.registerExtensionLogSink extension context

{- | Use the borrowed target and context for advanced managed registration.
Use this action for aggregate, table, COPY, and other registration APIs. Do not
retain or destroy the handles. They are valid only during 'runExtension'.
-}
withExtensionContext :: Extension -> (RegistrationTarget -> DuckDBV2ContextHandle -> IO a) -> IO a
withExtensionContext (Extension extension context) action = action (ExtensionTarget extension context) context
