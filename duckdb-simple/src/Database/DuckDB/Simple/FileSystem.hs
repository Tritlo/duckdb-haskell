{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.Coerce (Coercible, coerce)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Internal (Connection, SQLError (..), peekUtf8CString, withClientContext)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)

-- | Open a file through DuckDB's file-system layer for the duration of an action.
withFileHandle :: Connection -> FilePath -> [Duckdb_file_flag] -> (Duckdb_file_handle -> IO a) -> IO a
withFileHandle conn path flags action
    | '\0' `elem` path = throwIO (SQLError (Text.pack "duckdb-simple: file path contains NUL") Nothing Nothing)
    | otherwise =
        withFileSystem conn \fs ->
            bracket
                duckdb_create_file_open_options
                destroyFileOpenOptions
                \opts -> do
                    whenNull opts "allocate file-open options"
                    mapM_ (\flag -> expectState "set file-open flag" (duckdb_file_open_options_set_flag opts flag 1)) flags
                    TextForeign.withCString (Text.pack path) \cPath ->
                        bracket
                            ( alloca \filePtr -> do
                                poke filePtr (Duckdb_file_handle nullPtr)
                                rc <- duckdb_file_system_open fs (ConstPtr cPath) opts filePtr
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
readFileHandleChunk :: Duckdb_file_handle -> Int64 -> IO BS.ByteString
readFileHandleChunk handle requested
    | requested <= 0 = pure BS.empty
    | toInteger requested > toInteger (maxBound :: Int) =
        throwIO (SQLError (Text.pack "duckdb-simple: file read size exceeds Int range") Nothing Nothing)
    | otherwise =
        bracket (mallocBytes (fromIntegral requested)) free \raw -> do
            bytesRead <- duckdb_file_handle_read handle raw requested
            if bytesRead < 0
                then throwFileHandleError handle (Text.pack "read failed")
                else
                    if bytesRead > requested
                        then throwFileHandleError handle (Text.pack "read size exceeds buffer size")
                        else BS.packCStringLen (castPtr raw, fromIntegral bytesRead)

-- | Write an entire bytestring to a file handle.
writeFileHandleBytes :: Duckdb_file_handle -> BS.ByteString -> IO Int64
writeFileHandleBytes handle bytes =
    BS.useAsCStringLen bytes \(ptr, len) -> do
        written <- duckdb_file_handle_write handle (ConstPtr (castPtr ptr)) (fromIntegral len)
        if written < 0
            then throwFileHandleError handle (Text.pack "write failed")
            else pure written

-- | Return the current file position.
fileHandleTell :: Duckdb_file_handle -> IO Int64
fileHandleTell handle = do
    pos <- duckdb_file_handle_tell handle
    if pos < 0 then throwFileHandleError handle (Text.pack "tell failed") else pure pos

-- | Return the current file size in bytes.
fileHandleSize :: Duckdb_file_handle -> IO Int64
fileHandleSize handle = do
    size <- duckdb_file_handle_size handle
    if size < 0 then throwFileHandleError handle (Text.pack "size failed") else pure size

-- | Seek to an absolute byte offset.
fileHandleSeek :: Duckdb_file_handle -> Int64 -> IO ()
fileHandleSeek handle pos = do
    rc <- duckdb_file_handle_seek handle pos
    if rc == DuckDBSuccess
        then pure ()
        else throwFileHandleError handle (Text.pack "seek failed")

-- | Flush file-handle writes to stable storage.
fileHandleSync :: Duckdb_file_handle -> IO ()
fileHandleSync handle = do
    rc <- duckdb_file_handle_sync handle
    if rc == DuckDBSuccess
        then pure ()
        else throwFileHandleError handle (Text.pack "sync failed")

withFileSystem :: Connection -> (Duckdb_file_system -> IO a) -> IO a
withFileSystem conn action =
    withClientContext conn \ctx ->
        bracket
            (duckdb_client_context_get_file_system ctx)
            destroyFileSystem
            (\fs -> whenNull fs "allocate file system" >> action fs)

destroyFileSystem :: Duckdb_file_system -> IO ()
destroyFileSystem fs =
    alloca \ptr -> poke ptr fs >> duckdb_destroy_file_system ptr

destroyFileOpenOptions :: Duckdb_file_open_options -> IO ()
destroyFileOpenOptions opts =
    alloca \ptr -> poke ptr opts >> duckdb_destroy_file_open_options ptr

destroyFileHandle :: Duckdb_file_handle -> IO ()
destroyFileHandle handle =
    alloca \ptr -> poke ptr handle >> duckdb_destroy_file_handle ptr

throwFileSystemError :: Duckdb_file_system -> FilePath -> IO a
throwFileSystemError fs path = mask_ do
    err <- duckdb_file_system_error_data fs
    throwErrorData err (Text.concat [Text.pack "duckdb-simple: failed to open file ", Text.pack path])

throwFileHandleError :: Duckdb_file_handle -> Text -> IO a
throwFileHandleError handle fallback = mask_ do
    err <- duckdb_file_handle_error_data handle
    throwErrorData err fallback

throwErrorData :: Duckdb_error_data -> Text -> IO a
throwErrorData err fallback =
    bracket (pure err) destroyErrorData \errData -> do
        msgPtr <- duckdb_error_data_message errData
        errType <- duckdb_error_data_error_type errData
        message <-
            if msgPtr == ConstPtr nullPtr
                then pure fallback
                else peekUtf8CString msgPtr
        throwIO
            SQLError
                { sqlErrorMessage = message
                , sqlErrorType = Just errType
                , sqlErrorQuery = Nothing
                }

destroyErrorData :: Duckdb_error_data -> IO ()
destroyErrorData err =
    alloca \ptr -> poke ptr err >> duckdb_destroy_error_data ptr

expectState :: String -> IO Duckdb_state -> IO ()
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
whenNull :: (Coercible p (Ptr ())) => p -> String -> IO ()
whenNull ptr label =
    if coerce ptr == (nullPtr :: Ptr ()) then expectState label (pure DuckDBError) else pure ()
