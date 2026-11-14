{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Database.DuckDB.Simple.Logging.Internal
-- Description : Share the log sink registration for connections and extensions.
--
-- 'registerLogSinkOn' takes a registration target.
-- "Database.DuckDB.Simple.Logging" gives the connection and instance forms.
-- "Database.DuckDB.Simple.Extension" gives the extension form.
module Database.DuckDB.Simple.Logging.Internal (
  LogEntry (..),
  LogLevel (..),
  LogScope (..),
  registerLogStorage,
  registerLogSink,
  registerInstanceLogSink,
  registerLogSinkOn,
  logConnectionMessage,
  logInstanceMessage,
) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Database.DuckDB.Simple.Callback (RegistrationTarget (..), attachOpaque, runCallback, withCallbackResourcesFinally)
import Database.DuckDB.Simple.Internal (Connection, Instance, withConnectionHandle, withDatabaseHandle, withInstanceHandle)
import Database.DuckDB.Simple.Native
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)

-- | A copied log event. Native context identifiers are absent outside their scope.
data LogEntry = LogEntry
  { logEntryTimestamp :: Maybe UTCTime
  , logEntryLevel :: Text
  , logEntryType :: Text
  , logEntryMessage :: Text
  , logEntrySeverity :: LogLevel
  , logEntryScope :: LogScope
  , logEntryConnectionId :: Maybe Word64
  , logEntryTransactionId :: Maybe Word64
  , logEntryQueryId :: Maybe Word64
  , logEntryThreadId :: Maybe Word64
  }
  deriving (Eq, Show)

-- | Register a log sink without a flush action.
-- Select the sink through DuckDB's logging configuration to receive entries.
registerLogStorage :: Connection -> Text -> (LogEntry -> IO ()) -> IO ()
registerLogStorage conn name write = registerLogSink conn name write (pure ())

-- | Register an instance log sink through a live connection.
-- Callbacks can run concurrently on native threads. They must not log or change
-- logging settings. Native logging discards callback errors after containment.
-- The flush action also runs when DuckDB releases the sink. If the registration
-- fails after the name check, the flush action runs one time.
registerLogSink :: Connection -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerLogSink conn name write flush = withDatabaseHandle conn \instance_ ->
  withInstanceFactory instance_ \factory ->
    registerSink factory (checked . c_duckdb_v2_instance_register_log_sink instance_) name write flush

-- | Register a log sink on an explicitly managed instance. If the registration
-- fails after the name check, the flush action runs one time.
registerInstanceLogSink :: Instance -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerInstanceLogSink instance_ name write flush = withInstanceHandle instance_ \native ->
  withInstanceFactory native \factory ->
    registerSink factory (checked . c_duckdb_v2_instance_register_log_sink native) name write flush

-- | Register a log sink on a connection or a loading extension.
registerLogSinkOn :: RegistrationTarget -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerLogSinkOn target name write flush = case target of
  ConnectionTarget conn -> registerLogSink conn name write flush
  ExtensionTarget extension context -> withContextFactory context \factory ->
    registerSink factory (checked . c_duckdb_v2_extension_register_log_sink extension) name write flush

-- | Transfer callback ownership to the native sink and release its builder.
registerSink :: DuckDBV2FactoryHandle -> (DuckDBV2LogSinkHandle -> IO ()) -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerSink factory register name write flush = do
  when (Text.null name || Text.any (== '\0') name) $ throwInvalidArgument "log sink name must be nonempty and must not contain NUL"
  bracket (output (c_duckdb_v2_log_sink_create factory)) (destroy c_duckdb_v2_log_sink_destroy) \sink ->
    withCallbackResourcesFinally
      flush
      ( \allocate -> do
          writer <- allocate $ toFunPtr $ DuckDBV2LogSinkWriteCallbackFn_Aux \info _ timestamp level scope (ConstPtr kind) (ConstPtr message) err -> runCallback err do
            category <- peek kind >>= readText
            body <- peek message >>= readText
            connection <- optionalId (c_duckdb_v2_log_sink_write_info_get_connection_id info)
            transaction <- optionalId (c_duckdb_v2_log_sink_write_info_get_transaction_id info)
            query <- optionalId (c_duckdb_v2_log_sink_write_info_get_query_id info)
            thread <- optionalId (c_duckdb_v2_log_sink_write_info_get_thread_id info)
            severity <- logLevelFromNative level
            origin <- logScopeFromNative scope
            let time = posixSecondsToUTCTime (fromRational (toInteger timestamp % 1000000))
            write (LogEntry (Just time) (levelName severity) category body severity origin connection transaction query thread)
          flusher <- allocate $ toFunPtr $ DuckDBV2LogSinkFlushCallbackFn_Aux \_ _ err -> runCallback err flush
          pure (DuckDBV2LogSinkWriteCallbackFn writer, DuckDBV2LogSinkFlushCallbackFn flusher)
      )
      (attachOpaque (c_duckdb_v2_log_sink_set_user_data sink))
      \(writer, flusher) -> do
        withIdentifier name $ checked . c_duckdb_v2_log_sink_set_name sink
        checked (c_duckdb_v2_log_sink_set_write_callback sink writer)
        checked (c_duckdb_v2_log_sink_set_flush_callback sink flusher)
        register sink

-- | Copy an optional identifier from a borrowed log-entry context.
optionalId :: (Ptr DuckDBIdx -> Ptr CBool -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO (Maybe Word64)
optionalId getter = alloca \present -> do
  value <- output (\slot -> getter slot present)
  exists <- peek present
  pure (if exists == 0 then Nothing else Just (fromIntegral value))

-- | Retain the textual severity names of the log-storage interface.
levelName :: LogLevel -> Text
levelName = \case
  LogLevelTrace -> "TRACE"
  LogLevelDebug -> "DEBUG"
  LogLevelInfo -> "INFO"
  LogLevelWarning -> "WARNING"
  LogLevelError -> "ERROR"
  LogLevelFatal -> "FATAL"

-- | Emit a connection-scoped message through its current logging configuration.
logConnectionMessage :: Connection -> LogLevel -> Text -> Text -> IO ()
logConnectionMessage conn level category message = withConnectionHandle conn \native ->
  withText category \kind -> withText message \body ->
    checked (c_duckdb_v2_connection_log native (logLevelToNative level) kind body)

-- | Emit a database-scoped message on an explicit instance.
logInstanceMessage :: Instance -> LogLevel -> Text -> Text -> IO ()
logInstanceMessage instance_ level category message = withInstanceHandle instance_ \native ->
  withText category \kind -> withText message \body ->
    checked (c_duckdb_v2_instance_log native (logLevelToNative level) kind body)
