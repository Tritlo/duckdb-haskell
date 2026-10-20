{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Checked file operations through the V2 file system.
module Database.DuckDB.Simple.FileSystem (
    withFileHandle,
    readFileHandleChunk,
    writeFileHandleBytes,
    readFileHandleAt,
    writeFileHandleAt,
    fileHandleTell,
    fileHandleSize,
    fileHandleSeek,
    fileHandleSync,
    pathExists,
    pathIsRemote,
    expandPath,
    globPaths,
    movePath,
    removePath,
) where

import Control.Exception (bracket, throwIO)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Callback (runCallback)
import Database.DuckDB.Simple.Internal (Connection, withConnectionHandle)
import Database.DuckDB.Simple.Native
import Foreign.C.ConstPtr (ConstPtr (..), unConstPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)

-- | Open a file for one action. Release its handle on every exit path.
withFileHandle :: Connection -> FilePath -> [DuckDBV2FileFlag] -> (DuckDBV2FileHandle -> IO a) -> IO a
withFileHandle conn path flags action
    | '\0' `elem` path = fileError "file path contains NUL"
    | otherwise = withConnectionHandle conn \handle -> do
        fs <- output (c_duckdb_v2_connection_get_file_system handle)
        bracket (output (c_duckdb_v2_file_open_options_create fs)) (destroy c_duckdb_v2_file_open_options_destroy) \options -> do
            mapM_ (checked . c_duckdb_v2_file_open_options_set_flag options) flags
            withText (Text.pack path) \name ->
                bracket (output (c_duckdb_v2_file_system_open fs name options)) (destroy c_duckdb_v2_file_destroy) action

-- | Read up to the requested byte count and advance the file position.
readFileHandleChunk :: DuckDBV2FileHandle -> Int64 -> IO BS.ByteString
readFileHandleChunk handle requested
    | requested <= 0 = pure BS.empty
    | toInteger requested > toInteger (maxBound :: Int) = fileError "file read size exceeds Int range"
    | otherwise = allocaBytes (fromIntegral requested) \buffer -> do
        count <- output (c_duckdb_v2_file_read handle (castPtr buffer) (fromIntegral requested))
        if toInteger count > toInteger requested
            then fileError "native file read exceeds buffer size"
            else BS.packCStringLen (castPtr buffer, fromIntegral count)

-- | Write bytes and return the byte count. Advance the file position.
writeFileHandleBytes :: DuckDBV2FileHandle -> BS.ByteString -> IO Int64
writeFileHandleBytes handle bytes = BS.useAsCStringLen bytes \(ptr, count) ->
    fromIntegral <$> output (c_duckdb_v2_file_write handle (ConstPtr (castPtr ptr)) (fromIntegral count))

-- | Read exactly the requested bytes without changing the file position.
readFileHandleAt :: DuckDBV2FileHandle -> Int64 -> Int64 -> IO BS.ByteString
readFileHandleAt handle offset requested
    | offset < 0 = fileError "file offset is negative"
    | requested < 0 = fileError "file read size is negative"
    | toInteger requested > toInteger (maxBound :: Int) = fileError "file read size exceeds Int range"
    | otherwise = allocaBytes (fromIntegral requested) \buffer -> do
        checked (c_duckdb_v2_file_read_at handle (castPtr buffer) (fromIntegral requested) (fromIntegral offset))
        BS.packCStringLen (castPtr buffer, fromIntegral requested)

-- | Write all bytes at an offset without changing the file position.
writeFileHandleAt :: DuckDBV2FileHandle -> Int64 -> BS.ByteString -> IO ()
writeFileHandleAt handle offset bytes
    | offset < 0 = fileError "file offset is negative"
    | otherwise = BS.useAsCStringLen bytes \(ptr, count) ->
        checked (c_duckdb_v2_file_write_at handle (ConstPtr (castPtr ptr)) (fromIntegral count) (fromIntegral offset))

-- | Return the current byte position.
fileHandleTell :: DuckDBV2FileHandle -> IO Int64
fileHandleTell handle = output (c_duckdb_v2_file_tell handle) >>= checkedPosition

-- | Return the file size in bytes.
fileHandleSize :: DuckDBV2FileHandle -> IO Int64
fileHandleSize handle = output (c_duckdb_v2_file_size handle) >>= checkedPosition

-- | Set the absolute byte position.
fileHandleSeek :: DuckDBV2FileHandle -> Int64 -> IO ()
fileHandleSeek handle position
    | position < 0 = fileError "file offset is negative"
    | otherwise = checked (c_duckdb_v2_file_seek handle (fromIntegral position))

-- | Flush pending file writes to storage.
fileHandleSync :: DuckDBV2FileHandle -> IO ()
fileHandleSync handle = checked (c_duckdb_v2_file_sync handle)

-- | Test whether a file or directory exists in DuckDB's file system.
pathExists :: Connection -> FilePath -> IO Bool
pathExists conn path = withPath conn path \fs name ->
    (/= 0) <$> output (c_duckdb_v2_file_system_path_exists fs name)

-- | Test whether DuckDB routes a path to a remote file system.
pathIsRemote :: Connection -> FilePath -> IO Bool
pathIsRemote conn path = withPath conn path \fs name ->
    (/= 0) <$> output (c_duckdb_v2_file_system_path_is_remote fs name)

-- | Expand a path with DuckDB's file-system rules.
expandPath :: Connection -> FilePath -> IO FilePath
expandPath conn path = withPath conn path \fs name -> do
    pieces <- newIORef []
    bracket
        ( toFunPtr
            ( DuckDBV2TextSinkFn_Aux
                ( \text _ err -> runCallback err do
                    value <- peek (unConstPtr text) >>= readText
                    modifyIORef' pieces (value :)
                )
            )
        )
        freeHaskellFunPtr
        \callback -> checked (c_duckdb_v2_file_system_path_expand fs name (DuckDBV2TextSinkFn callback) nullPtr)
    Text.unpack . Text.concat . reverse <$> readIORef pieces

-- | Collect glob matches in the order supplied by DuckDB.
globPaths :: Connection -> FilePath -> IO [FilePath]
globPaths conn pattern_ = withPath conn pattern_ \fs name -> do
    paths <- newIORef []
    bracket
        ( toFunPtr
            ( DuckDBV2FileSystemPathGlobCallbackFn_Aux
                ( \path _ err -> runCallback err do
                    value <- peek (unConstPtr path) >>= readText
                    modifyIORef' paths (Text.unpack value :)
                )
            )
        )
        freeHaskellFunPtr
        \callback -> checked (c_duckdb_v2_file_system_path_glob fs name (DuckDBV2FileSystemPathGlobCallbackFn callback) nullPtr)
    reverse <$> readIORef paths

-- | Move a path through DuckDB's file system.
movePath :: Connection -> FilePath -> FilePath -> IO ()
movePath conn source target
    | '\0' `elem` target = fileError "file path contains NUL"
    | otherwise = withPath conn source \fs name -> withText (Text.pack target) \destination ->
        checked (c_duckdb_v2_file_system_path_move fs name destination)

-- | Remove a file. Return whether DuckDB removed it.
removePath :: Connection -> FilePath -> IO Bool
removePath conn path = withPath conn path \fs name ->
    (/= 0) <$> output (c_duckdb_v2_file_system_path_remove fs name)

-- | Borrow the connection's file system and a checked path.
withPath :: Connection -> FilePath -> (DuckDBV2FileSystemHandle -> ConstPtr DuckDBV2Str -> IO a) -> IO a
withPath conn path action
    | '\0' `elem` path = fileError "file path contains NUL"
    | otherwise = withConnectionHandle conn \handle -> do
        fs <- output (c_duckdb_v2_connection_get_file_system handle)
        withText (Text.pack path) (action fs)

-- | Check the public signed range before converting a native byte position.
checkedPosition :: DuckDBIdx -> IO Int64
checkedPosition value
    | toInteger value > toInteger (maxBound :: Int64) = fileError "file position exceeds Int64 range"
    | otherwise = pure (fromIntegral value)

-- | Report an invalid high-level file argument.
fileError :: Text.Text -> IO a
fileError message = throwIO (SQLError ("duckdb-simple: " <> message) Nothing Nothing)
