{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Database.DuckDB.Simple.Appender
Description : High-performance appender API
-}
module Database.DuckDB.Simple.Appender
  ( withTableAppender
  , withTableAppenderExt
  , withQueryAppender
  , appendTableRow
  , tableSchema
  , createTableQuery
  , module Database.DuckDB.Simple.Appender.Generic
  )
  where

import Database.DuckDB.FFI    (c_duckdb_appender_error_data, c_duckdb_error_data_message, DuckDBAppender,      DuckDBLogicalType,      DuckDBState,      pattern DuckDBSuccess,      pattern DuckDBError,      c_duckdb_appender_create,      c_duckdb_appender_create_ext,      c_duckdb_appender_create_query,      c_duckdb_appender_destroy, c_duckdb_appender_begin_row, c_duckdb_appender_end_row, c_duckdb_appender_flush, c_duckdb_appender_close )
import Data.Text.Foreign (withCString)
import Data.Text (Text)
import Foreign (Ptr, nullPtr, Storable (peek))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Control.Exception (finally, throwIO)
import Control.Monad (when)
import Database.DuckDB.Simple.Internal (withConnectionHandle, Connection, SQLError(..), Query(..))
import GHC.Stack (HasCallStack)
import Data.Data (Proxy)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Appender.Generic
import Foreign.C (peekCString)

type TableName = Text

withTableAppender :: Connection -> TableName -> (DuckDBAppender -> IO a) -> IO a
withTableAppender conn tableName action = withConnectionHandle conn $ \conn' ->
    withCString tableName $ \tablePtr ->
        withAppenderAcquire
            (c_duckdb_appender_create conn' nullPtr tablePtr)
            action

withTableAppenderExt :: Connection -> TableName -> (DuckDBAppender -> IO a) -> IO a
withTableAppenderExt conn tableName action = withConnectionHandle conn $ \conn' ->
    withCString tableName $ \tablePtr ->
        withAppenderAcquire
            (c_duckdb_appender_create_ext conn' nullPtr nullPtr tablePtr)
            action

withQueryAppender :: Connection -> TableName -> [DuckDBLogicalType] -> (DuckDBAppender -> IO a) -> IO a
withQueryAppender conn query types action = withConnectionHandle conn $ \conn' ->
    withCString query $ \queryPtr ->
        withArray types $ \typeArray ->
            withAppenderAcquire
                (c_duckdb_appender_create_query conn' queryPtr (fromIntegral (length types)) typeArray nullPtr nullPtr)
                action

withAppenderAcquire :: (Ptr DuckDBAppender -> IO DuckDBState) -> (DuckDBAppender -> IO a) -> IO a
withAppenderAcquire acquire action =
    alloca $ \appPtr -> do
        state <- acquire appPtr
        when (state /= DuckDBSuccess) $ throwIO (userError "duckdb-simple: could not acquire appender")
        case state of
          DuckDBSuccess -> pure ()
          DuckDBError -> throwIO (userError "withAppenderAcquire")
        app <- peek appPtr
        let release = do
              assertSuccess app $ c_duckdb_appender_destroy appPtr
            flushAndClose = do
                assertSuccess app $ c_duckdb_appender_flush app
                assertSuccess app $ c_duckdb_appender_close app
        (action app <* flushAndClose) `finally` release

appendTableRow :: (HasCallStack, AppendTableRow a) => DuckDBAppender -> a -> IO ()
appendTableRow app row = do
    assertSuccess app $ c_duckdb_appender_begin_row app
    assertSuccess app $ appendDuckRow app row
    assertSuccess app $ c_duckdb_appender_end_row app

assertSuccess :: (HasCallStack) => DuckDBAppender -> IO DuckDBState -> IO ()
assertSuccess app f = f >>= \case
  DuckDBSuccess -> pure ()
  _errorStatus -> do
        err <- c_duckdb_appender_error_data app >>= c_duckdb_error_data_message >>= peekCString
        throwIO $ SQLError("duckdb-simple: appender error" <> Text.pack err) Nothing Nothing -- FIXME: proper error handling
{-# INLINE assertSuccess #-}

createTableQuery :: AppendTableRow a => Text -> Proxy a -> Query
createTableQuery nme pxy = Query $ "CREATE TABLE \"" <> nme <> "\" (" <> tableSchema pxy <> ")" -- FIXME: unsafe

tableSchema :: (AppendTableRow a) => Proxy a -> Text
tableSchema pxy = Text.intercalate ", \n" ["\"" <> structFieldName <> "\" " <> renderDuckTypeName structFieldValue | (structFieldName, structFieldValue) <- appendDuckRowSchema pxy]
