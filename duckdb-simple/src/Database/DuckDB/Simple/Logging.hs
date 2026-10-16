{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | V2 callback logging and the retained log-storage compatibility boundary.
module Database.DuckDB.Simple.Logging (
    LogEntry (..),
    registerLogStorage,
    logMessage,
) where

import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.DuckDB.Simple.Internal (Connection, withConnectionHandle)
import Database.DuckDB.Simple.Native

-- | A log event accepted by the earlier log-storage interface.
data LogEntry = LogEntry
    { logEntryTimestamp :: !(Maybe UTCTime)
    , logEntryLevel :: !Text
    , logEntryType :: !Text
    , logEntryMessage :: !Text
    }
    deriving (Eq, Show)

{- | Report that custom log storage is unavailable in the pinned V2 client API.
Use DuckDB's SQL logging configuration and tables to collect native logs.
-}
registerLogStorage :: Connection -> Text -> (LogEntry -> IO ()) -> IO ()
registerLogStorage conn _ _ = withConnectionHandle conn \_ ->
    throwIO (SQLError "duckdb-simple: custom log-storage registration is unavailable in DuckDB V2" (Just DuckDBV2ErrorApi) Nothing)

-- | Emit a message from a native callback's borrowed context.
logMessage :: DuckDBV2ContextHandle -> DuckDBV2LogLevel -> Text -> Text -> IO ()
logMessage context level category message =
    withText category \kind -> withText message \body ->
        checked (c_duckdb_v2_context_log context level kind body)
