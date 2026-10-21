{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | V2 log sinks with managed callbacks and copied log entries.
module Database.DuckDB.Simple.Logging (
    LogEntry (..),
    registerLogStorage,
    registerLogSink,
    registerInstanceLogSink,
    registerExtensionLogSink,
    logMessage,
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
import Database.DuckDB.Simple.Callback (attachOpaque, runCallback, withCallbackResourcesFinally)
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
    { logEntryTimestamp :: !(Maybe UTCTime)
    , logEntryLevel :: !Text
    , logEntryType :: !Text
    , logEntryMessage :: !Text
    , logEntrySeverity :: !DuckDBV2LogLevel
    , logEntryScope :: !DuckDBV2LogScope
    , logEntryConnectionId :: !(Maybe Word64)
    , logEntryTransactionId :: !(Maybe Word64)
    , logEntryQueryId :: !(Maybe Word64)
    , logEntryThreadId :: !(Maybe Word64)
    }
    deriving (Eq, Show)

{- | Register a log sink without a flush action.
Select the sink through DuckDB's logging configuration to receive entries.
-}
registerLogStorage :: Connection -> Text -> (LogEntry -> IO ()) -> IO ()
registerLogStorage conn name write = registerLogSink conn name write (pure ())

{- | Register an instance log sink through a live connection.
Callbacks can run concurrently on native threads. They must not log or change
logging settings. Native logging discards callback errors after containment.
The flush action also runs when DuckDB releases the sink.
-}
registerLogSink :: Connection -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerLogSink conn name write flush = withDatabaseHandle conn \instance_ ->
    withInstanceFactory instance_ \factory ->
        registerSink factory (checked . c_duckdb_v2_instance_register_log_sink instance_) name write flush

-- | Register a log sink on an explicitly managed instance.
registerInstanceLogSink :: Instance -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerInstanceLogSink instance_ name write flush = withInstanceHandle instance_ \native ->
    withInstanceFactory native \factory ->
        registerSink factory (checked . c_duckdb_v2_instance_register_log_sink native) name write flush

-- | Register a log sink during extension initialization. Both handles are borrowed.
registerExtensionLogSink :: DuckDBV2ExtensionHandle -> DuckDBV2ContextHandle -> Text -> (LogEntry -> IO ()) -> IO () -> IO ()
registerExtensionLogSink extension context name write flush = withContextFactory context \factory ->
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
                    let time = posixSecondsToUTCTime (fromRational (toInteger timestamp % 1000000))
                    write (LogEntry (Just time) (levelName level) category body level scope connection transaction query thread)
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
levelName :: DuckDBV2LogLevel -> Text
levelName level
    | level == DuckDBV2LogLevelTrace = "TRACE"
    | level == DuckDBV2LogLevelDebug = "DEBUG"
    | level == DuckDBV2LogLevelInfo = "INFO"
    | level == DuckDBV2LogLevelWarning = "WARNING"
    | level == DuckDBV2LogLevelError = "ERROR"
    | level == DuckDBV2LogLevelFatal = "FATAL"
    | otherwise = Text.pack (show level)

-- | Emit a message from a native callback's borrowed context.
logMessage :: DuckDBV2ContextHandle -> DuckDBV2LogLevel -> Text -> Text -> IO ()
logMessage context level category message =
    withText category \kind -> withText message \body ->
        checked (c_duckdb_v2_context_log context level kind body)

-- | Emit a connection-scoped message through its current logging configuration.
logConnectionMessage :: Connection -> DuckDBV2LogLevel -> Text -> Text -> IO ()
logConnectionMessage conn level category message = withConnectionHandle conn \native ->
    withText category \kind -> withText message \body ->
        checked (c_duckdb_v2_connection_log native level kind body)

-- | Emit a database-scoped message on an explicit instance.
logInstanceMessage :: Instance -> DuckDBV2LogLevel -> Text -> Text -> IO ()
logInstanceMessage instance_ level category message = withInstanceHandle instance_ \native ->
    withText category \kind -> withText message \body ->
        checked (c_duckdb_v2_instance_log native level kind body)
