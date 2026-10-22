{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Register typed Haskell functions with the loading DuckDB host.
module Demo where

import Control.Exception (throwIO)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import Database.DuckDB.FFI.V2 (DuckDBV2ContextHandle (..), DuckDBV2ExtensionHandle (..), c_duckdb_v2_value_create_bigint, pattern DuckDBV2LogicalTypeIdAny, pattern DuckDBV2MacroFunctionTypeScalar, pattern DuckDBV2MacroFunctionTypeTable)
import qualified Database.DuckDB.Simple.Config as Config
import Database.DuckDB.Simple.Extension (Extension, ExtensionEntry)
import qualified Database.DuckDB.Simple.Extension as Extension
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (LogicalTypeScalar))
import qualified Database.DuckDB.Simple.Pragma as Pragma
import qualified Database.DuckDB.Simple.Registration as Registration

-- | Export the registration action called by the C load entrypoint.
initialize :: ExtensionEntry
initialize = Extension.runExtension register

foreign export ccall "duckdb_haskell_demo_register" initialize :: ExtensionEntry

-- | Register typed callbacks, macros, a PRAGMA, a custom option, and a log sink.
register :: Extension -> IO ()
register extension = do
    logEntries <- newIORef (0 :: Int64)
    Extension.createFunction extension "haskell_answer" (42 :: Int64)
    Extension.createFunction extension "haskell_add_one" addOne
    Extension.createFunction extension "haskell_greet" (("Hello " <>) :: Text -> Text)
    Extension.createFunction extension "haskell_fail" (throwIO (userError "Haskell callback failure") :: IO Int64)
    Extension.registerMacro extension "haskell_twice" DuckDBV2MacroFunctionTypeScalar [Registration.parameter "value" (Registration.StructuredType (LogicalTypeScalar DuckDBV2LogicalTypeIdAny))] "value + value"
    Extension.registerMacro extension "haskell_table" DuckDBV2MacroFunctionTypeTable [] "SELECT 42::BIGINT AS answer"
    Extension.registerCustomOption extension $
        (Config.customOption "haskell_multiplier" (Registration.TypeExpression "BIGINT"))
            { Config.customOptionDefault = Just (Registration.FactoryDefaultValue (\factory -> Registration.output (c_duckdb_v2_value_create_bigint factory 7)))
            }
    Extension.registerPragma extension "haskell_demo" [] $
        Pragma.QueryPragma (\_ _ -> pure "SELECT 'pragma', current_setting('haskell_multiplier')::BIGINT")
    Extension.registerLogSink extension "haskell_sink" (\_ -> atomicModifyIORef' logEntries (\count -> (count + 1, ()))) (pure ())
    Extension.createFunction extension "haskell_log_entries" (readIORef logEntries)

-- | Increment a BIGINT argument. Keep NULL values and report overflow.
addOne :: Maybe Int64 -> IO (Maybe Int64)
addOne Nothing = pure Nothing
addOne (Just value)
    | value == maxBound = throwIO (userError "Haskell BIGINT overflow")
    | otherwise = pure (Just (value + 1))
