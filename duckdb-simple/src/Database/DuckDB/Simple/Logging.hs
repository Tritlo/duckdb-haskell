{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database.DuckDB.Simple.Logging (
    LogEntry (..),
    registerLogStorage,
) where

import Control.Exception (bracket, onException, throwIO)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Internal (Connection, SQLError (..), withDatabaseHandle)
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peek, poke)

data LogEntry = LogEntry
    { logEntryTimestamp :: !(Maybe UTCTime)
    , logEntryLevel :: !Text
    , logEntryType :: !Text
    , logEntryMessage :: !Text
    }
    deriving (Eq, Show)

registerLogStorage :: Connection -> Text -> (LogEntry -> IO ()) -> IO ()
registerLogStorage conn name callback = do
    writeCb <- mkWriteLogEntryCallback (logStorageHandler callback)
    callbackStable <- newStablePtr writeCb
    deleteCb <- mkDeleteCallback releaseWriteLogCallback
    let release = freeHaskellFunPtr writeCb >> freeStablePtr callbackStable >> freeHaskellFunPtr deleteCb
    bracket c_duckdb_create_log_storage destroyLogStorage \storage ->
        (`onException` release) $ do
            TextForeign.withCString name \cName ->
                c_duckdb_log_storage_set_name storage cName
            c_duckdb_log_storage_set_write_log_entry storage writeCb
            c_duckdb_log_storage_set_extra_data storage (castStablePtrToPtr callbackStable) deleteCb
            withDatabaseHandle conn \db -> do
                rc <- c_duckdb_register_log_storage db storage
                if rc == DuckDBSuccess
                    then pure ()
                    else throwRegistrationError "register log storage"

logStorageHandler ::
    (LogEntry -> IO ()) ->
    Ptr () ->
    Ptr DuckDBTimestamp ->
    CString ->
    CString ->
    CString ->
    IO ()
logStorageHandler callback _ timestampPtr levelPtr logTypePtr messagePtr = do
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
    | otherwise = Text.pack <$> peekCString ptr

releaseWriteLogCallback :: Ptr () -> IO ()
releaseWriteLogCallback rawPtr =
    if rawPtr == nullPtr
        then pure ()
        else do
            let stablePtr = castPtrToStablePtr rawPtr :: StablePtr DuckDBLoggerWriteLogEntryFun
            callback <- deRefStablePtr stablePtr
            freeHaskellFunPtr callback
            freeStablePtr stablePtr

destroyLogStorage :: DuckDBLogStorage -> IO ()
destroyLogStorage storage =
    alloca \ptr -> poke ptr storage >> c_duckdb_destroy_log_storage ptr

throwRegistrationError :: String -> IO a
throwRegistrationError label =
    throwIO
        SQLError
            { sqlErrorMessage = Text.pack ("duckdb-simple: " <> label <> " failed")
            , sqlErrorType = Nothing
            , sqlErrorQuery = Nothing
            }

foreign import ccall "wrapper"
    mkWriteLogEntryCallback ::
        (Ptr () -> Ptr DuckDBTimestamp -> CString -> CString -> CString -> IO ()) ->
        IO DuckDBLoggerWriteLogEntryFun

foreign import ccall "wrapper"
    mkDeleteCallback :: (Ptr () -> IO ()) -> IO DuckDBDeleteCallback
