-- | V2 log sinks with managed callbacks and copied log entries.
module Database.DuckDB.Simple.Logging (
    LogEntry (..),
    LogLevel (..),
    LogScope (..),
    registerLogStorage,
    registerLogSink,
    registerInstanceLogSink,
    logConnectionMessage,
    logInstanceMessage,
) where

import Database.DuckDB.Simple.Logging.Internal (
    LogEntry (..),
    LogLevel (..),
    LogScope (..),
    logConnectionMessage,
    logInstanceMessage,
    registerInstanceLogSink,
    registerLogSink,
    registerLogStorage,
 )
