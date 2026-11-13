{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Demo
Description : Register typed Haskell functions with the loading DuckDB host.
-}
module Demo where

import qualified Advanced
import Control.Exception (throwIO)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Database.DuckDB.Simple.Config as Config
import Database.DuckDB.Simple.Extension (Extension, ExtensionEntry)
import qualified Database.DuckDB.Simple.Extension as Extension
import Database.DuckDB.Simple.LogicalRep (LogicalTypeId (LogicalTypeIdAny), LogicalTypeRep (LogicalTypeScalar))
import qualified Database.DuckDB.Simple.Pragma as Pragma
import qualified Database.DuckDB.Simple.V2.Registration as Registration

-- | Export the registration action called by the C load entrypoint.
initialize :: ExtensionEntry
initialize = Extension.runExtension register

foreign export ccall "duckdb_haskell_demo_register" initialize :: ExtensionEntry

{- | Register typed callbacks, macros, a PRAGMA, a custom option, and a log sink.
Then register the functions with native callbacks and the typed cast in
"Advanced".
-}
register :: Extension -> IO ()
register extension = do
    logEntries <- newIORef (0 :: Int64)
    Extension.createFunction extension "haskell_answer" (42 :: Int64)
    Extension.createFunction extension "haskell_add_one" addOne
    Extension.createFunction extension "haskell_greet" (("Hello " <>) :: Text -> Text)
    Extension.createFunction extension "haskell_fail" (throwIO (userError "Haskell callback failure") :: IO Int64)
    Extension.registerMacroFunction extension "haskell_twice" Extension.MacroFunctionTypeScalar [Registration.parameter "value" (Registration.StructuredType (LogicalTypeScalar LogicalTypeIdAny))] "value + value"
    Extension.registerMacroFunction extension "haskell_table" Extension.MacroFunctionTypeTable [] "SELECT 42::BIGINT AS answer"
    Extension.registerCustomOption extension $
        (Config.customOption "haskell_multiplier" (Registration.TypeExpression "BIGINT"))
            { Config.customOptionDefault = Just (Registration.defaultValue (7 :: Int64))
            }
    Extension.registerPragmaFunction extension "haskell_demo" [] $
        Pragma.QueryPragma (\() -> pure "SELECT 'pragma', current_setting('haskell_multiplier')::BIGINT")
    Extension.registerLogSink extension "haskell_sink" (\_ -> atomicModifyIORef' logEntries (\count -> (count + 1, ()))) (pure ())
    Extension.createFunction extension "haskell_log_entries" (readIORef logEntries)
    Advanced.register extension

-- | Increment a BIGINT argument. Keep NULL values and report overflow.
addOne :: Maybe Int64 -> IO (Maybe Int64)
addOne Nothing = pure Nothing
addOne (Just value)
    | value == maxBound = throwIO (userError "Haskell BIGINT overflow")
    | otherwise = pure (Just (value + 1))
