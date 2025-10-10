{-# LANGUAGE BlockArguments #-}

module Utils
  ( withDatabase
  , withConnection
  , withResult
  , withResultCString
  , withValue
  ) where

import Control.Exception (bracket)
import Database.DuckDB.FFI
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import Test.Tasty.HUnit ((@?=))

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase action =
  withCString ":memory:" \path ->
    alloca \dbPtr -> do
      c_duckdb_open path dbPtr >>= (@?= DuckDBSuccess)
      db <- peek dbPtr
      result <- action db
      c_duckdb_close dbPtr
      pure result

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db action =
  bracket acquire release action
  where
    acquire =
      alloca \connPtr -> do
        c_duckdb_connect db connPtr >>= (@?= DuckDBSuccess)
        peek connPtr
    release conn =
      alloca \connPtr -> do
        poke connPtr conn
        c_duckdb_disconnect connPtr

withResult :: DuckDBConnection -> String -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn sql action =
  withCString sql \sqlPtr -> withResultCString conn sqlPtr action

withResultCString :: DuckDBConnection -> CString -> (Ptr DuckDBResult -> IO a) -> IO a
withResultCString conn sql action =
  alloca \resPtr -> do
    c_duckdb_query conn sql resPtr >>= (@?= DuckDBSuccess)
    result <- action resPtr
    c_duckdb_destroy_result resPtr
    pure result

withValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO a
withValue acquire action =
  bracket acquire destroy action
  where
    destroy value =
      alloca \ptr -> do
        poke ptr value
        c_duckdb_destroy_value ptr
