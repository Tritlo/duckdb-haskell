{-# LANGUAGE BlockArguments #-}

-- |
-- Module      : Database.DuckDB.Simple.Extension
-- Description : Entry point and typed registrations for a loadable Haskell extension.
--
-- A loadable extension is a shared library that DuckDB loads into its own process with @LOAD@. DuckDB calls the
-- entry point of the extension one time in each database instance. A second @LOAD@ of the same name in that
-- instance does not call it again.
-- The entry point registers SQL objects, and DuckDB keeps them in the database after the entry point returns. This
-- module gives the entry point and the typed registrations of "Database.DuckDB.Simple.Function" and of the other
-- typed modules for an extension.
--
-- An extension needs these items:
--
-- - The @extension@ flag of duckdb-ffi and of duckdb-simple. Enable the two flags together. A flag applies to the
--   complete build plan, so one project cannot build a DuckDB host and an extension. With the flag, the two
--   packages build only on Linux and macOS.
-- - A C entry point @NAME_init_c_api_v2@ that calls @duckdb_haskell_extension_load@ with the foreign export of an
--   'ExtensionEntry' and does nothing else. DuckDB finds the entry point from the file name @NAME.duckdb_extension@.
-- - The threaded runtime. Link the extension with @-threaded@. Otherwise the load fails with an error.
--
-- These steps build, package, and load an extension:
--
-- 1. Build the foreign library with Cabal in a project that enables the two flags, as
--    @dev\/extensions\/cabal.project@ and @dev\/extensions\/example\/duckdb-haskell-extension-example.cabal@ do.
-- 2. Append the DuckDB metadata to the library with @dev\/extensions\/Pack.hs@, as @dev\/extensions\/PACKAGING.md@
--    shows.
-- 3. Load the file with @LOAD@ in a DuckDB 2.0.0 process that accepts unsigned extensions, as
--    @dev\/extensions\/example\/test\/Main.hs@ does with @duckdb -unsigned@.
--
-- The Haskell side of a small extension, from @dev\/extensions\/example\/src\/Demo.hs@:
--
-- > {-# LANGUAGE ForeignFunctionInterface #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Demo where
-- >
-- > import Data.Int (Int64)
-- > import Data.Text (Text)
-- > import Database.DuckDB.Simple.Extension (Extension, ExtensionEntry)
-- > import qualified Database.DuckDB.Simple.Extension as Extension
-- >
-- > initialize :: ExtensionEntry
-- > initialize = Extension.runExtension register
-- >
-- > foreign export ccall "duckdb_haskell_demo_register" initialize :: ExtensionEntry
-- >
-- > register :: Extension -> IO ()
-- > register extension = do
-- >   Extension.createFunction extension "haskell_answer" (42 :: Int64)
-- >   Extension.createFunction extension "haskell_greet" (("Hello " <>) :: Text -> Text)
-- >   Extension.registerMacroFunction extension "haskell_table" Extension.MacroFunctionTypeTable [] "SELECT 42::BIGINT AS answer"
--
-- The C entry point of the same extension, from @dev\/extensions\/example\/cbits\/entry.c@. The packaged file must
-- be @haskell_demo.duckdb_extension@:
--
-- > #include "duckdb_haskell_extension.h"
-- >
-- > extern void duckdb_haskell_demo_register(void *extension, void *context, void *err);
-- >
-- > /* DuckDB derives this name from the file name of the packaged extension. */
-- > DUCKDB_EXTENSION_API void haskell_demo_init_c_api_v2(struct duckdb_v2_extension_input *input) {
-- >     duckdb_haskell_extension_load(input, duckdb_haskell_demo_register);
-- > }
--
-- The t'Extension' value is valid only while 'runExtension' runs. DuckDB gives a borrowed extension handle and a
-- borrowed context to the entry point. They are not valid after the entry point returns. Thus do not keep the
-- t'Extension' value, or the target of 'extensionTarget', in a registered callback or in other state. A registered
-- function keeps its callback resources after the entry point returns.
--
-- The README of the example gives these limits:
--
-- - The runtime has one capability (@-N1@). The callbacks of all DuckDB worker threads share it. You cannot change
--   the runtime options.
-- - Haskell extensions built with the same GHC share one runtime in the process, and the options of the first
--   loaded extension apply. Extensions built with different GHC versions in one process are not tested. The runtime
--   stays until the process ends. Do not call @hs_exit@.
-- - The process supports one DuckDB implementation.
-- - The extension loads only into the DuckDB release of the pinned header, 2.0.0.
-- - In extension mode, duckdb-ffi exposes only "Database.DuckDB.FFI.V2".
-- - A COPY TO function of an extension cannot read a VARIANT column. A replacement scan of an extension belongs to
--   the instance, not to one connection.
-- - The example links the GHC and package libraries dynamically. It loads only on a machine where its run paths
--   exist.
--
-- The complete example is in the directory @dev\/extensions\/example\/@ of the repository
-- <https://github.com/Tritlo/duckdb-haskell>. Its file @src\/Advanced.hs@ registers the other kinds of SQL objects.
-- @dev\/extensions\/README.md@ has the commands that build, package, test, and load it.
module Database.DuckDB.Simple.Extension (
  Extension,
  ExtensionEntry,
  runExtension,
  extensionTarget,
  createFunction,
  createFunctionWithState,
  registerReplacementScan,
  registerCustomType,
  registerMultiFileFunction,
  createTableFunction,
  createAggregate,
  MacroFunctionType (..),
  registerMacroFunction,
  registerPragmaFunction,
  registerCustomOption,
  registerLogSink,
  registerCopyToFunction,
  registerCopyFromFunction,
  registerCopyFunction,
) where

import Data.Text (Text)
import Database.DuckDB.Simple.Callback (runCallback)
import Database.DuckDB.Simple.Config.Internal (CustomOption)
import qualified Database.DuckDB.Simple.Config.Internal as Config
import Database.DuckDB.Simple.Copy.Internal (
  CopyBindInfo,
  CopyFinalizeInfo,
  CopyFromInfo,
  CopyInitInfo,
  CopySinkInfo,
  CopyTo,
  MultiFileOptions,
  registerMultiFileFunctionOn,
 )
import qualified Database.DuckDB.Simple.Copy.Internal as Copy
import Database.DuckDB.Simple.FromField (Field)
import Database.DuckDB.Simple.FromRow (FromRow)
import Database.DuckDB.Simple.Function.Internal (Function, FunctionResult, createFunctionOn, createFunctionWithStateOn)
import Database.DuckDB.Simple.Logging.Internal (LogEntry)
import qualified Database.DuckDB.Simple.Logging.Internal as Logging
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Pragma.Internal (PragmaCallback)
import qualified Database.DuckDB.Simple.Pragma.Internal as Pragma
import Database.DuckDB.Simple.Typed (Replacement, TypedRow, replacementScanCallback)
import Database.DuckDB.Simple.Typed.Aggregate (Aggregate, createAggregateOn)
import Database.DuckDB.Simple.Typed.Table (createTableFunctionOn)
import Database.DuckDB.Simple.V2.Registration (
  FunctionParameter,
  FunctionSignature (..),
  RegistrationTarget (ExtensionTarget),
  TypeDeclaration,
  configureFunctionSignature,
  registerCustomTypeOn,
  registerMacroFunctionWith,
  registerReplacementScanOn,
 )
import Foreign.Ptr (Ptr, castPtr)

-- | The borrowed registration target and context for one extension load.
-- Use this value only inside the action passed to 'runExtension'. Do not retain it
-- in registered callbacks or application state.
data Extension = Extension DuckDBV2ExtensionHandle DuckDBV2ContextHandle

-- | The Haskell entry that @duckdb_haskell_extension_load@ calls. Its arguments
-- are the extension handle, the borrowed context, and the error slot of the load.
-- The plain pointers let a @foreign export@ use this type with no other import.
type ExtensionEntry = Ptr () -> Ptr () -> Ptr () -> IO ()

-- | Run registration and report exceptions through the borrowed load error slot.
-- The extension is valid only during this call. Registered functions own their
-- callback resources after this call returns.
runExtension :: (Extension -> IO ()) -> ExtensionEntry
runExtension initialize extension context err =
  runCallback (castPtr err) (initialize (Extension (DuckDBV2ExtensionHandle (castPtr extension)) (DuckDBV2ContextHandle (castPtr context))))

-- | The registration target of the loading extension. Pass it to the
-- registration functions of the V2 modules, which take a 'RegistrationTarget'.
-- The typed functions of this module do not need it. The target is valid only
-- during 'runExtension'.
extensionTarget :: Extension -> RegistrationTarget
extensionTarget (Extension extension context) = ExtensionTarget extension context

-- | Register a typed Haskell function with the loading DuckDB host. The rules
-- of 'Database.DuckDB.Simple.Function.createFunction' apply.
createFunction :: (Function f) => Extension -> Text -> f -> IO ()
createFunction = createFunctionOn . extensionTarget

-- | Register a typed function with per-worker state owned by DuckDB. The rules
-- of 'Database.DuckDB.Simple.Function.createFunctionWithState' apply.
createFunctionWithState :: (Function f) => Extension -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState = createFunctionWithStateOn . extensionTarget

-- | Register a replacement scan with the loading DuckDB host. The scan belongs
-- to the instance. Each connection of the instance uses it until the instance
-- closes. No function removes a scan. DuckDB asks the scans of a connection
-- before the scans of the instance. It asks the scans of the instance in the
-- order of registration. The function can run on several connections at the same
-- time. 'Database.DuckDB.Simple.Registration.registerReplacementScan' gives the
-- rules for the function.
registerReplacementScan :: Extension -> ([Text] -> IO (Maybe Replacement)) -> IO ()
registerReplacementScan extension = registerReplacementScanOn (extensionTarget extension) . replacementScanCallback

-- | Register a custom type with the loading DuckDB host. The rules of
-- 'Database.DuckDB.Simple.Registration.registerCustomType' apply.
registerCustomType :: Extension -> Text -> TypeDeclaration -> IO ()
registerCustomType = registerCustomTypeOn . extensionTarget

-- | Register a multi-file table function with the loading DuckDB host. The
-- rules of 'Database.DuckDB.Simple.Copy.registerMultiFileFunction' apply.
registerMultiFileFunction :: Extension -> Text -> Text -> MultiFileOptions -> IO ()
registerMultiFileFunction = registerMultiFileFunctionOn . extensionTarget

-- | Register a typed table function with the loading DuckDB host. The rules of
-- 'Database.DuckDB.Simple.Function.createTableFunction' apply.
createTableFunction :: (FromRow params, TypedRow params, TypedRow row) => Extension -> Text -> [Text] -> (params -> IO [row]) -> IO ()
createTableFunction = createTableFunctionOn . extensionTarget

-- | Register an aggregate function with the loading DuckDB host. The rules of
-- 'Database.DuckDB.Simple.Function.createAggregate' apply.
createAggregate :: (FromRow args, TypedRow args, FunctionResult result) => Extension -> Text -> Aggregate state args result -> IO ()
createAggregate = createAggregateOn . extensionTarget

-- | Register a scalar expression or a table SELECT as a macro of the loading
-- extension. The V2 API registers a macro only for an extension. Use SQL
-- @CREATE MACRO@ on a connection. Parameters must use the standard parameter
-- kind. Use @StructuredType (LogicalTypeScalar LogicalTypeIdAny)@ for an
-- untyped parameter and 'Database.DuckDB.Simple.Registration.defaultValue' for a
-- default. DuckDB validates the body and the signature. It resolves the body
-- when the macro runs.
registerMacroFunction :: Extension -> Text -> MacroFunctionType -> [FunctionParameter] -> Text -> IO ()
registerMacroFunction extension@(Extension _ context) name kind parameters body =
  withContextFactory context \factory ->
    registerMacroFunctionWith (extensionTarget extension) kind \function -> do
      withIdentifier name (checked . c_duckdb_v2_macro_function_set_name function)
      withText body (checked . c_duckdb_v2_macro_function_set_body function)
      signature <- output (c_duckdb_v2_macro_function_get_signature function)
      configureFunctionSignature factory (FunctionSignature parameters Nothing) signature

-- | Register a typed PRAGMA callback on the loading extension. The rules of
-- 'Database.DuckDB.Simple.Pragma.registerPragmaFunction' apply.
registerPragmaFunction :: (FromRow args) => Extension -> Text -> [FunctionParameter] -> PragmaCallback args -> IO ()
registerPragmaFunction = Pragma.registerPragmaFunctionOn . extensionTarget

-- | Register a typed option during extension initialization. The rules of
-- 'Database.DuckDB.Simple.Config.registerCustomOption' apply.
registerCustomOption :: Extension -> CustomOption -> IO ()
registerCustomOption = Config.registerCustomOptionOn . extensionTarget

-- | Register a log sink with callback resources owned by DuckDB.
-- Callbacks can run concurrently. They must not log or change logging settings.
-- The rules of 'Database.DuckDB.Simple.Logging.registerLogSink' apply.
registerLogSink :: Extension -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerLogSink = Logging.registerLogSinkOn . extensionTarget

-- | Register row-oriented COPY TO callbacks with the loading DuckDB host. The
-- rules of 'Database.DuckDB.Simple.Copy.registerCopyToFunction' apply. The
-- callbacks cannot read a VARIANT column.
registerCopyToFunction
  :: Extension
  -> Text
  -> (CopyBindInfo -> IO bindState)
  -> (CopyInitInfo bindState -> IO globalState)
  -> (CopySinkInfo bindState globalState -> [[Field]] -> IO ())
  -> (CopyFinalizeInfo bindState globalState -> IO ())
  -> IO ()
registerCopyToFunction = Copy.registerCopyToFunctionOn . extensionTarget

-- | Register the COPY FROM side of a format with the loading DuckDB host. The
-- rules of 'Database.DuckDB.Simple.Copy.registerCopyFromFunction' apply.
registerCopyFromFunction :: (TypedRow row) => Extension -> Text -> (CopyFromInfo -> IO [row]) -> IO ()
registerCopyFromFunction = Copy.registerCopyFromFunctionOn . extensionTarget

-- | Register a format with a COPY TO side and a COPY FROM side with the
-- loading DuckDB host. The rules of
-- 'Database.DuckDB.Simple.Copy.registerCopyFunction' apply. The COPY TO side
-- cannot read a VARIANT column.
registerCopyFunction :: (TypedRow row) => Extension -> Text -> CopyTo bindState globalState -> (CopyFromInfo -> IO [row]) -> IO ()
registerCopyFunction = Copy.registerCopyFunctionOn . extensionTarget
