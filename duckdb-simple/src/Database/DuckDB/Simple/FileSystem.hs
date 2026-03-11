{-# LANGUAGE BlockArguments #-}

module Database.DuckDB.Simple.FileSystem (
    withFileHandle,
    readFileHandleChunk,
    writeFileHandleBytes,
    fileHandleTell,
    fileHandleSize,
    fileHandleSeek,
    fileHandleSync,
) where

import Control.Exception (bracket, throwIO)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Internal (Connection, SQLError (..), withConnectionHandle)
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek, poke)

withFileHandle :: Connection -> FilePath -> [DuckDBFileFlag] -> (DuckDBFileHandle -> IO a) -> IO a
withFileHandle conn path flags action =
    withFileSystem conn \fs ->
        bracket
            c_duckdb_create_file_open_options
            destroyFileOpenOptions
            \opts -> do
                mapM_ (\flag -> expectState "set file-open flag" (c_duckdb_file_open_options_set_flag opts flag 1)) flags
                withCString path \cPath ->
                    alloca \filePtr -> do
                        rc <- c_duckdb_file_system_open fs cPath opts filePtr
                        if rc /= DuckDBSuccess
                            then throwFileSystemError fs path
                            else do
                                handle <- peek filePtr
                                bracket (pure handle) destroyFileHandle action

readFileHandleChunk :: DuckDBFileHandle -> Int64 -> IO BS.ByteString
readFileHandleChunk handle requested
    | requested <= 0 = pure BS.empty
    | otherwise = do
        raw <- mallocBytes (fromIntegral requested)
        bytesRead <- c_duckdb_file_handle_read handle raw requested
        if bytesRead < 0
            then free raw >> throwFileHandleError handle (Text.pack "read failed")
            else do
                bs <- BS.packCStringLen (castPtr raw, fromIntegral bytesRead)
                free raw
                pure bs

writeFileHandleBytes :: DuckDBFileHandle -> BS.ByteString -> IO Int64
writeFileHandleBytes handle bytes =
    BS.useAsCStringLen bytes \(ptr, len) -> do
        written <- c_duckdb_file_handle_write handle (castPtr ptr) (fromIntegral len)
        if written < 0
            then throwFileHandleError handle (Text.pack "write failed")
            else pure written

fileHandleTell :: DuckDBFileHandle -> IO Int64
fileHandleTell handle = do
    pos <- c_duckdb_file_handle_tell handle
    if pos < 0 then throwFileHandleError handle (Text.pack "tell failed") else pure pos

fileHandleSize :: DuckDBFileHandle -> IO Int64
fileHandleSize handle = do
    size <- c_duckdb_file_handle_size handle
    if size < 0 then throwFileHandleError handle (Text.pack "size failed") else pure size

fileHandleSeek :: DuckDBFileHandle -> Int64 -> IO ()
fileHandleSeek handle pos = do
    rc <- c_duckdb_file_handle_seek handle pos
    if rc == DuckDBSuccess
        then pure ()
        else throwFileHandleError handle (Text.pack "seek failed")

fileHandleSync :: DuckDBFileHandle -> IO ()
fileHandleSync handle = do
    rc <- c_duckdb_file_handle_sync handle
    if rc == DuckDBSuccess
        then pure ()
        else throwFileHandleError handle (Text.pack "sync failed")

withFileSystem :: Connection -> (DuckDBFileSystem -> IO a) -> IO a
withFileSystem conn action =
    withClientContext conn \ctx ->
        bracket
            (c_duckdb_client_context_get_file_system ctx)
            destroyFileSystem
            action

withClientContext :: Connection -> (DuckDBClientContext -> IO a) -> IO a
withClientContext conn action =
    withConnectionHandle conn \connPtr ->
        alloca \ctxPtr -> do
            c_duckdb_connection_get_client_context connPtr ctxPtr
            ctx <- peek ctxPtr
            bracket (pure ctx) destroyClientContext action

destroyClientContext :: DuckDBClientContext -> IO ()
destroyClientContext ctx =
    alloca \ptr -> poke ptr ctx >> c_duckdb_destroy_client_context ptr

destroyFileSystem :: DuckDBFileSystem -> IO ()
destroyFileSystem fs =
    alloca \ptr -> poke ptr fs >> c_duckdb_destroy_file_system ptr

destroyFileOpenOptions :: DuckDBFileOpenOptions -> IO ()
destroyFileOpenOptions opts =
    alloca \ptr -> poke ptr opts >> c_duckdb_destroy_file_open_options ptr

destroyFileHandle :: DuckDBFileHandle -> IO ()
destroyFileHandle handle =
    alloca \ptr -> poke ptr handle >> c_duckdb_destroy_file_handle ptr

throwFileSystemError :: DuckDBFileSystem -> FilePath -> IO a
throwFileSystemError fs path = do
    err <- c_duckdb_file_system_error_data fs
    throwErrorData err (Text.concat [Text.pack "duckdb-simple: failed to open file ", Text.pack path])

throwFileHandleError :: DuckDBFileHandle -> Text -> IO a
throwFileHandleError handle fallback = do
    err <- c_duckdb_file_handle_error_data handle
    throwErrorData err fallback

throwErrorData :: DuckDBErrorData -> Text -> IO a
throwErrorData err fallback =
    bracket (pure err) destroyErrorData \errData -> do
        msgPtr <- c_duckdb_error_data_message errData
        errType <- c_duckdb_error_data_error_type errData
        message <-
            if msgPtr == nullPtr
                then pure fallback
                else Text.pack <$> peekCString msgPtr
        throwIO
            SQLError
                { sqlErrorMessage = message
                , sqlErrorType = Just errType
                , sqlErrorQuery = Nothing
                }

destroyErrorData :: DuckDBErrorData -> IO ()
destroyErrorData err =
    alloca \ptr -> poke ptr err >> c_duckdb_destroy_error_data ptr

expectState :: String -> IO DuckDBState -> IO ()
expectState label action = do
    rc <- action
    if rc == DuckDBSuccess
        then pure ()
        else
            throwIO $
                SQLError
                    { sqlErrorMessage = Text.pack ("duckdb-simple: " <> label <> " failed")
                    , sqlErrorType = Nothing
                    , sqlErrorQuery = Nothing
                    }
