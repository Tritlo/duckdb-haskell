{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : Database.DuckDB.Simple.Logging
Description : High-level wrappers for DuckDB custom log storage.
-}
module Database.DuckDB.Simple.Logging (
    LogEntry (..),
    registerLogStorage,
) where

import Control.Exception (bracket, mask_)
import Control.Monad (when)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Callback (ignoreCallbackExceptions, withCallbackResources)
import Database.DuckDB.Simple.Internal (Connection, peekUtf8CString, throwRegistrationError, withDatabaseHandle)
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, poke)

-- | A single log event delivered through DuckDB's log-storage callback.
data LogEntry = LogEntry
    { logEntryTimestamp :: !(Maybe UTCTime)
    , logEntryLevel :: !Text
    , logEntryType :: !Text
    , logEntryMessage :: !Text
    }
    deriving (Eq, Show)

-- | Register a custom log storage callback on the database behind a connection.
registerLogStorage :: Connection -> Text -> (LogEntry -> IO ()) -> IO ()
registerLogStorage conn name callback = do
    when (Text.null name || Text.any (== '\0') name) $
        throwRegistrationError "invalid log storage name"
    bracket c_duckdb_create_log_storage destroyLogStorage \storage -> do
        when (storage == nullPtr) $ throwRegistrationError "allocate log storage"
        withCallbackResources
            (\allocate -> allocate (mkWriteLogEntryCallback (logStorageHandler callback)))
            (c_duckdb_log_storage_set_extra_data storage)
            \writeCb -> do
                TextForeign.withCString name $ c_duckdb_log_storage_set_name storage
                c_duckdb_log_storage_set_write_log_entry storage writeCb
                withDatabaseHandle conn \db -> mask_ do
                    rc <- c_duckdb_register_log_storage db storage
                    -- DuckDB consumes extra data on duplicate-name failure.
                    -- Clear the wrapper to prevent a second destruction.
                    c_duckdb_log_storage_set_extra_data storage nullPtr nullFunPtr
                    when (rc /= DuckDBSuccess) $ throwRegistrationError "register log storage"

logStorageHandler ::
    (LogEntry -> IO ()) ->
    Ptr () ->
    Ptr DuckDBTimestamp ->
    CString ->
    CString ->
    CString ->
    IO ()
logStorageHandler callback _ timestampPtr levelPtr logTypePtr messagePtr =
    ignoreCallbackExceptions do
        entry <- do
            logEntryTimestamp <- readTimestamp timestampPtr
            logEntryLevel <- readCStringText levelPtr
            logEntryType <- readCStringText logTypePtr
            logEntryMessage <- readCStringText messagePtr
            pure LogEntry{logEntryTimestamp, logEntryLevel, logEntryType, logEntryMessage}
        callback entry

readTimestamp :: Ptr DuckDBTimestamp -> IO (Maybe UTCTime)
readTimestamp ptr
    | ptr == nullPtr = pure Nothing
    | otherwise = do
        DuckDBTimestamp micros <- peek ptr
        pure (Just (posixSecondsToUTCTime (fromRational (toInteger micros % 1000000))))

readCStringText :: CString -> IO Text
readCStringText ptr
    | ptr == nullPtr = pure Text.empty
    | otherwise = peekUtf8CString ptr

destroyLogStorage :: DuckDBLogStorage -> IO ()
destroyLogStorage storage =
    alloca \ptr -> poke ptr storage >> c_duckdb_destroy_log_storage ptr

foreign import ccall "wrapper"
    mkWriteLogEntryCallback ::
        (Ptr () -> Ptr DuckDBTimestamp -> CString -> CString -> CString -> IO ()) ->
        IO DuckDBLoggerWriteLogEntryFun
