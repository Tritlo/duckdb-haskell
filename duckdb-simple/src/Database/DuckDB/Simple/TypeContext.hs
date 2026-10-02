{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Construct logical types that need DuckDB's SQL binder.
module Database.DuckDB.Simple.TypeContext (
    withTypeConnection,
    queryLogicalType,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Internal (destroyValue, peekUtf8CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke, sizeOf)

{- | Use the caller's connection when one is available. Standalone value and
type constructors use a temporary database and connection. These temporary
handles are closed before this function returns.
-}
withTypeConnection :: Maybe DuckDBConnection -> (DuckDBConnection -> IO a) -> IO a
withTypeConnection (Just connection) action = action connection
withTypeConnection Nothing action =
    alloca \database ->
        bracket
            (poke database nullPtr >> c_duckdb_open nullPtr database)
            (const (c_duckdb_close database))
            \opened -> do
                when (opened /= DuckDBSuccess) (throwIO (userError "duckdb-simple: cannot open type construction database"))
                db <- peek database
                alloca \connection ->
                    bracket
                        (poke connection nullPtr >> c_duckdb_connect db connection)
                        (const (c_duckdb_disconnect connection))
                        \connected -> do
                            when (connected /= DuckDBSuccess) (throwIO (userError "duckdb-simple: cannot connect to type construction database"))
                            peek connection >>= action

{- | Obtain an owned column type from a constant query. The optional text
parameter is bound by length. The caller must destroy the returned type.
-}
queryLogicalType :: DuckDBConnection -> Text -> Maybe Text -> IO DuckDBLogicalType
queryLogicalType connection sql parameter =
    BS.useAsCString (Text.encodeUtf8 sql) \sqlPtr ->
        alloca \statementPtr ->
            bracket
                (poke statementPtr nullPtr >> c_duckdb_prepare connection sqlPtr statementPtr)
                (const (c_duckdb_destroy_prepare statementPtr))
                \prepared -> do
                    statement <- peek statementPtr
                    when (prepared /= DuckDBSuccess) do
                        errorPtr <- c_duckdb_prepare_error statement
                        message <- if errorPtr == nullPtr then pure "prepare failed" else peekUtf8CString errorPtr
                        throwIO (userError ("duckdb-simple: type query: " <> Text.unpack message))
                    case parameter of
                        Nothing -> pure ()
                        Just value ->
                            BS.useAsCStringLen (Text.encodeUtf8 value) \(ptr, len) ->
                                bracket (c_duckdb_create_varchar_length ptr (fromIntegral len)) destroyValue \native -> do
                                    when (native == nullPtr) (throwIO (userError "duckdb-simple: cannot create type query parameter"))
                                    bound <- c_duckdb_bind_value statement 1 native
                                    when (bound /= DuckDBSuccess) (throwIO (userError "duckdb-simple: cannot bind type query parameter"))
                    alloca \result ->
                        bracket
                            (fillBytes result 0 (sizeOf (undefined :: DuckDBResult)) >> c_duckdb_execute_prepared statement result)
                            (const (c_duckdb_destroy_result result))
                            \executed -> do
                                when (executed /= DuckDBSuccess) do
                                    errorPtr <- c_duckdb_result_error result
                                    message <- if errorPtr == nullPtr then pure "execution failed" else peekUtf8CString errorPtr
                                    throwIO (userError ("duckdb-simple: type query: " <> Text.unpack message))
                                logical <- c_duckdb_column_logical_type result 0
                                when (logical == nullPtr) (throwIO (userError "duckdb-simple: type query returned no type"))
                                pure logical
