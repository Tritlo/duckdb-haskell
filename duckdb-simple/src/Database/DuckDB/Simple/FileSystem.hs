{-# LANGUAGE BlockArguments #-}

{- |
Module      : Database.DuckDB.Simple.FileSystem
Description : High-level wrappers around DuckDB's file-system API.
-}
module Database.DuckDB.Simple.FileSystem (
    withFileHandle,
    readFileHandleChunk,
    writeFileHandleBytes,
    fileHandleTell,
    fileHandleSize,
    fileHandleSeek,
    fileHandleSync,
) where

import Control.Exception (bracket, mask_, throwIO)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Internal (Connection, SQLError (..), peekUtf8CString, withClientContext)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)

-- | Open a file through DuckDB's file-system layer for the duration of an action.
withFileHandle :: Connection -> FilePath -> [DuckDBFileFlag] -> (DuckDBFileHandle -> IO a) -> IO a
withFileHandle conn path flags action
    | '\0' `elem` path = throwIO (SQLError (Text.pack "duckdb-simple: file path contains NUL") Nothing Nothing)
    | otherwise =
        withFileSystem conn \fs ->
            bracket
                c_duckdb_create_file_open_options
                destroyFileOpenOptions
                \opts -> do
                    whenNull opts "allocate file-open options"
                    mapM_ (\flag -> expectState "set file-open flag" (c_duckdb_file_open_options_set_flag opts flag 1)) flags
                    TextForeign.withCString (Text.pack path) \cPath ->
                        bracket
                            ( alloca \filePtr -> do
                                poke filePtr nullPtr
                                rc <- c_duckdb_file_system_open fs cPath opts filePtr
                                if rc /= DuckDBSuccess
                                    then throwFileSystemError fs path
                                    else do
                                        handle <- peek filePtr
                                        whenNull handle "allocate file handle"
                                        pure handle
                            )
                            destroyFileHandle
                            action

-- | Read up to the requested number of bytes from a file handle.
readFileHandleChunk :: DuckDBFileHandle -> Int64 -> IO BS.ByteString
readFileHandleChunk handle requested
    | requested <= 0 = pure BS.empty
    | toInteger requested > toInteger (maxBound :: Int) =
        throwIO (SQLError (Text.pack "duckdb-simple: file read size exceeds Int range") Nothing Nothing)
    | otherwise =
        bracket (mallocBytes (fromIntegral requested)) free \raw -> do
            bytesRead <- c_duckdb_file_handle_read handle raw requested
            if bytesRead < 0
                then throwFileHandleError handle (Text.pack "read failed")
                else
                    if bytesRead > requested
                        then throwFileHandleError handle (Text.pack "read size exceeds buffer size")
                        else BS.packCStringLen (castPtr raw, fromIntegral bytesRead)

-- | Write an entire bytestring to a file handle.
writeFileHandleBytes :: DuckDBFileHandle -> BS.ByteString -> IO Int64
writeFileHandleBytes handle bytes =
    BS.useAsCStringLen bytes \(ptr, len) -> do
        written <- c_duckdb_file_handle_write handle (castPtr ptr) (fromIntegral len)
        if written < 0
            then throwFileHandleError handle (Text.pack "write failed")
            else pure written

-- | Return the current file position.
fileHandleTell :: DuckDBFileHandle -> IO Int64
fileHandleTell handle = do
    pos <- c_duckdb_file_handle_tell handle
    if pos < 0 then throwFileHandleError handle (Text.pack "tell failed") else pure pos

-- | Return the current file size in bytes.
fileHandleSize :: DuckDBFileHandle -> IO Int64
fileHandleSize handle = do
    size <- c_duckdb_file_handle_size handle
    if size < 0 then throwFileHandleError handle (Text.pack "size failed") else pure size

-- | Seek to an absolute byte offset.
fileHandleSeek :: DuckDBFileHandle -> Int64 -> IO ()
fileHandleSeek handle pos = do
    rc <- c_duckdb_file_handle_seek handle pos
    if rc == DuckDBSuccess
        then pure ()
        else throwFileHandleError handle (Text.pack "seek failed")

-- | Flush file-handle writes to stable storage.
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
            (\fs -> whenNull fs "allocate file system" >> action fs)

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
throwFileSystemError fs path = mask_ do
    err <- c_duckdb_file_system_error_data fs
    throwErrorData err (Text.concat [Text.pack "duckdb-simple: failed to open file ", Text.pack path])

throwFileHandleError :: DuckDBFileHandle -> Text -> IO a
throwFileHandleError handle fallback = mask_ do
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
                else peekUtf8CString msgPtr
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

-- | Reject a null file-system handle before use.
whenNull :: Ptr a -> String -> IO ()
whenNull ptr label =
    if ptr == nullPtr then expectState label (pure DuckDBError) else pure ()
