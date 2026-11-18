{-# LANGUAGE BlockArguments #-}

-- |
-- Module      : Database.DuckDB.Simple.V2.Logging
-- Description : Write log messages through the context of a native callback.
--
-- The function of this module gets the borrowed context of a native callback.
-- "Database.DuckDB.Simple.Logging" has the log sinks and the functions that write
-- messages for a connection or an instance.
--
-- The example uses the imports of the module header of "Database.DuckDB.Simple.V2.Registration".
module Database.DuckDB.Simple.V2.Logging (
  logMessage,
) where

import Data.Text (Text)
import Database.DuckDB.Simple.Native

-- | Emit a message from a native callback's borrowed context.
-- A log sink gets the first text as 'Database.DuckDB.Simple.Logging.logEntryType' and the second text as
-- 'Database.DuckDB.Simple.Logging.logEntryMessage'. For example, in the exec callback of a scalar function:
--
-- > V2Logging.logMessage context Logging.LogLevelWarning "HaskellContext" "from a callback"
logMessage :: DuckDBV2ContextHandle -> LogLevel -> Text -> Text -> IO ()
logMessage context level category message =
  withText category \kind -> withText message \body ->
    checked (c_duckdb_v2_context_log context (logLevelToNative level) kind body)
