{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Native type construction that needs SQL metadata.
module Database.DuckDB.Simple.TypeContext (
    logicalTypeForConnection,
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
import Database.DuckDB.Simple.LogicalRep.Internal (LogicalTypeRep (..), logicalTypeFromRepWith)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)

-- | Construct an owned type with the binding connection's SQL metadata.
logicalTypeForConnection :: DuckDBConnection -> LogicalTypeRep -> IO DuckDBLogicalType
logicalTypeForConnection connection = logicalTypeFromRepWith resolve
  where
    resolve (LogicalTypeGeometry (Just crs)) =
        queryLogicalType connection "SELECT system.main.ST_SetCRS('POINT EMPTY'::GEOMETRY, ?)" (Just crs)
    resolve _ = throwIO (userError "duckdb-simple: unsupported SQL logical type")

{- | Obtain an owned column type from a constant query. The optional text
parameter is bound by length. The caller must destroy the returned type.
-}
queryLogicalType :: DuckDBConnection -> Text -> Maybe Text -> IO DuckDBLogicalType
queryLogicalType connection sql parameter = case parameter of
    Nothing -> query Nothing
    Just value ->
        BS.useAsCStringLen (Text.encodeUtf8 value) \(ptr, len) ->
            bracket (c_duckdb_create_varchar_length ptr (fromIntegral len)) destroyValue \native -> do
                when (native == nullPtr) (throwIO (userError "duckdb-simple: cannot create type query parameter"))
                query (Just native)
  where
    query parameterValue = withQueryResult connection sql parameterValue \result -> do
        logical <- c_duckdb_column_logical_type result 0
        when (logical == nullPtr) (throwIO (userError "duckdb-simple: type query returned no type"))
        pure logical

-- | Keep a prepared statement, optional borrowed parameter, and result scoped.
withQueryResult :: DuckDBConnection -> Text -> Maybe DuckDBValue -> (Ptr DuckDBResult -> IO a) -> IO a
withQueryResult connection sql parameter action =
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
                        throwIO (userError ("duckdb-simple: construction query: " <> Text.unpack message))
                    case parameter of
                        Nothing -> pure ()
                        Just native -> do
                            bound <- c_duckdb_bind_value statement 1 native
                            when (bound /= DuckDBSuccess) (throwIO (userError "duckdb-simple: cannot bind construction query parameter"))
                    alloca \result ->
                        bracket
                            (fillBytes result 0 (sizeOf (undefined :: DuckDBResult)) >> c_duckdb_execute_prepared statement result)
                            (const (c_duckdb_destroy_result result))
                            \executed -> do
                                when (executed /= DuckDBSuccess) do
                                    errorPtr <- c_duckdb_result_error result
                                    message <- if errorPtr == nullPtr then pure "execution failed" else peekUtf8CString errorPtr
                                    throwIO (userError ("duckdb-simple: construction query: " <> Text.unpack message))
                                action result
