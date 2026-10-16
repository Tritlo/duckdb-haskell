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
    withExtensionContext,
) where

import Data.Text (Text)
import Database.DuckDB.FFI.V2 (DuckDBV2ContextHandle, DuckDBV2ErrorInfoHandle, DuckDBV2ExtensionHandle)
import Database.DuckDB.Simple.Callback (runCallback)
import Database.DuckDB.Simple.Function.Internal (Function, createExtensionFunction, createExtensionFunctionWithState)
import Database.DuckDB.Simple.Registration (RegistrationTarget (ExtensionTarget))
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
createFunction (Extension extension context) = createExtensionFunction extension context

-- | Register a typed function with per-worker state owned by DuckDB.
createFunctionWithState :: (Function f) => Extension -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState (Extension extension context) = createExtensionFunctionWithState extension context

{- | Use the borrowed target and context for advanced managed registration.
Use this action for aggregate, table, COPY, and other registration APIs. Do not
retain or destroy the handles. They are valid only during 'runExtension'.
-}
withExtensionContext :: Extension -> (RegistrationTarget -> DuckDBV2ContextHandle -> IO a) -> IO a
withExtensionContext (Extension extension context) action = action (ExtensionTarget extension) context
