{-# LANGUAGE BlockArguments #-}

{- | Entry support for a loadable V2 Haskell extension.
Enable the @extension@ flag of duckdb-ffi and of duckdb-simple. The C entry
point calls @duckdb_haskell_extension_load@ with the foreign export of an
'ExtensionEntry'. See @dev/extensions/@ in the repository.
-}
module Database.DuckDB.Simple.Extension (
    Extension,
    ExtensionEntry,
    runExtension,
    extensionTarget,
    createFunction,
    createFunctionWithState,
    createCast,
    registerReplacementScan,
    registerCustomType,
    registerMultiFileFunction,
    MacroFunctionType (..),
    registerMacroFunction,
    registerMacroFunctionWith,
    registerPragmaFunction,
    registerCustomOption,
    registerLogSink,
) where

import Control.Exception (bracket)
import Data.Text (Text)
import Database.DuckDB.Simple.Callback (runCallback)
import Database.DuckDB.Simple.Config (CustomOption)
import qualified Database.DuckDB.Simple.Config as Config
import Database.DuckDB.Simple.Copy (MultiFileOptions, registerMultiFileFunctionOn)
import Database.DuckDB.Simple.FromField (FromField)
import Database.DuckDB.Simple.Function.Internal (Function, createFunctionOn, createFunctionWithStateOn)
import Database.DuckDB.Simple.Logging (LogEntry)
import qualified Database.DuckDB.Simple.Logging as Logging
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Pragma (PragmaCallback)
import qualified Database.DuckDB.Simple.Pragma as Pragma
import Database.DuckDB.Simple.ToField (ToDuckValue)
import Database.DuckDB.Simple.Typed (Replacement, createCastOn, replacementScanCallback)
import Database.DuckDB.Simple.V2.Registration (FunctionParameter, FunctionSignature (..), RegistrationTarget (ExtensionTarget), TypeDeclaration, configureFunctionSignature, registerCustomTypeOn, registerReplacementScanOn)
import Foreign.Ptr (Ptr, castPtr)

{- | The borrowed registration target and context for one extension load.
Use this value only inside the action passed to 'runExtension'. Do not retain it
in registered callbacks or application state.
-}
data Extension = Extension DuckDBV2ExtensionHandle DuckDBV2ContextHandle

{- | The Haskell entry that @duckdb_haskell_extension_load@ calls. Its arguments
are the extension handle, the borrowed context, and the error slot of the load.
The plain pointers let a @foreign export@ use this type with no other import.
-}
type ExtensionEntry = Ptr () -> Ptr () -> Ptr () -> IO ()

{- | Run registration and report exceptions through the borrowed load error slot.
The extension is valid only during this call. Registered functions own their
callback resources after this call returns.
-}
runExtension :: (Extension -> IO ()) -> ExtensionEntry
runExtension initialize extension context err =
    runCallback (castPtr err) (initialize (Extension (DuckDBV2ExtensionHandle (castPtr extension)) (DuckDBV2ContextHandle (castPtr context))))

{- | The registration target of the loading extension. Pass it to the
registration functions that take a 'RegistrationTarget', for example for an
aggregate function, a table function, or a COPY function. The target is valid
only during 'runExtension'.
-}
extensionTarget :: Extension -> RegistrationTarget
extensionTarget (Extension extension context) = ExtensionTarget extension context

-- | Register a typed Haskell function with the loading DuckDB host.
createFunction :: (Function f) => Extension -> Text -> f -> IO ()
createFunction = createFunctionOn . extensionTarget

-- | Register a typed function with per-worker state owned by DuckDB.
createFunctionWithState :: (Function f) => Extension -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState = createFunctionWithStateOn . extensionTarget

{- | Register an explicit typed cast with the loading DuckDB host. The rules of
'Database.DuckDB.Simple.Function.createCast' apply.
-}
createCast :: (FromField a, ToDuckValue b) => Extension -> TypeDeclaration -> TypeDeclaration -> (a -> Either Text b) -> IO ()
createCast = createCastOn . extensionTarget

{- | Register a replacement scan with the loading DuckDB host. The scan belongs
to the instance. Each connection of the instance uses it until the instance
closes. The rules of
'Database.DuckDB.Simple.Registration.registerInstanceReplacementScan' apply.
-}
registerReplacementScan :: Extension -> ([Text] -> IO (Maybe Replacement)) -> IO ()
registerReplacementScan extension = registerReplacementScanOn (extensionTarget extension) . replacementScanCallback

{- | Register a custom type with the loading DuckDB host. The rules of
'Database.DuckDB.Simple.Registration.registerCustomType' apply.
-}
registerCustomType :: Extension -> Text -> TypeDeclaration -> IO ()
registerCustomType = registerCustomTypeOn . extensionTarget

{- | Register a multi-file table function with the loading DuckDB host. The
rules of 'Database.DuckDB.Simple.Copy.registerMultiFileFunction' apply.
-}
registerMultiFileFunction :: Extension -> Text -> Text -> MultiFileOptions -> IO ()
registerMultiFileFunction = registerMultiFileFunctionOn . extensionTarget

{- | Register a scalar expression or a table SELECT as a macro of the loading
extension. The V2 API registers a macro only for an extension. Use SQL
@CREATE MACRO@ on a connection. Parameters must use the standard parameter
kind. Use @StructuredType (LogicalTypeScalar DuckDBV2LogicalTypeIdAny)@ for an
untyped parameter and 'Database.DuckDB.Simple.V2.Registration.defaultValue' for a
default. DuckDB validates the body and the signature. It resolves the body
when the macro runs.
-}
registerMacroFunction :: Extension -> Text -> MacroFunctionType -> [FunctionParameter] -> Text -> IO ()
registerMacroFunction extension@(Extension _ context) name kind parameters body =
    withContextFactory context \factory ->
        registerMacroFunctionWith extension kind \function -> do
            withIdentifier name (checked . c_duckdb_v2_macro_function_set_name function)
            withText body (checked . c_duckdb_v2_macro_function_set_body function)
            signature <- output (c_duckdb_v2_macro_function_get_signature function)
            configureFunctionSignature factory (FunctionSignature parameters Nothing) signature

{- | Configure and register a macro of the loading extension. The configuration
action borrows the builder. It can set the name, the body, the signature, and
the documentation. DuckDB keeps copies after this action returns.
-}
registerMacroFunctionWith :: Extension -> MacroFunctionType -> (DuckDBV2MacroFunctionHandle -> IO ()) -> IO ()
registerMacroFunctionWith (Extension extension context) kind configure =
    withContextFactory context \factory ->
        bracket (output (c_duckdb_v2_macro_function_create factory (macroFunctionTypeToNative kind))) (destroy c_duckdb_v2_macro_function_destroy) \function -> do
            configure function
            checked (c_duckdb_v2_extension_register_macro_function extension function)

-- | Register a managed PRAGMA callback on the loading extension.
registerPragmaFunction :: Extension -> Text -> [FunctionParameter] -> PragmaCallback -> IO ()
registerPragmaFunction = Pragma.registerPragmaFunctionOn . extensionTarget

-- | Register a typed option during extension initialization.
registerCustomOption :: Extension -> CustomOption -> IO ()
registerCustomOption = Config.registerCustomOptionOn . extensionTarget

{- | Register a log sink with callback resources owned by DuckDB.
Callbacks can run concurrently. They must not log or change logging settings.
-}
registerLogSink :: Extension -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerLogSink = Logging.registerLogSinkOn . extensionTarget
