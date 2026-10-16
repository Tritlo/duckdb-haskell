{- | Entry support for a loadable V2 Haskell extension.
Build duckdb-ffi with its @extension@ flag. The C entrypoint must first call
@duckdb_haskell_extension_init_api@, then initialize the threaded GHC runtime,
then call the Haskell export. See @dev/extensions/@ in the repository.
-}
module Database.DuckDB.Simple.Extension (
    runExtension,
) where

import Database.DuckDB.FFI.V2 (DuckDBV2ContextHandle, DuckDBV2ErrorInfoHandle, DuckDBV2ExtensionHandle)
import Database.DuckDB.Simple.Callback (runCallback)
import Database.DuckDB.Simple.Registration (RegistrationTarget (ExtensionTarget))
import Foreign.Ptr (Ptr)

{- | Run registration and report exceptions through the borrowed load error slot.
The target and context are valid only during this call. Do not retain or destroy
them. Callback registration owns its callback resources after this call returns.
-}
runExtension ::
    (RegistrationTarget -> DuckDBV2ContextHandle -> IO ()) ->
    DuckDBV2ExtensionHandle ->
    DuckDBV2ContextHandle ->
    Ptr DuckDBV2ErrorInfoHandle ->
    IO ()
runExtension initialize extension context err =
    runCallback err (initialize (ExtensionTarget extension) context)
