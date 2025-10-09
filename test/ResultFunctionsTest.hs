{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module ResultFunctionsTest (tests) where

import Control.Monad (forM_, when)
import Database.DuckDB.FFI
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Result Functions"
    [ chunkIntrospection
    ]

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase f =
  withCString ":memory:" \path ->
    alloca \dbPtr -> do
      state <- c_duckdb_open path dbPtr
      state @?= DuckDBSuccess
      db <- peek dbPtr
      result <- f db
      c_duckdb_close dbPtr
      pure result

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db f =
  alloca \connPtr -> do
    st <- c_duckdb_connect db connPtr
    st @?= DuckDBSuccess
    conn <- peek connPtr
    result <- f conn
    c_duckdb_disconnect connPtr
    pure result

chunkIntrospection :: TestTree
chunkIntrospection =
  testCase "result chunk information and streaming state" $
    withDatabase \db ->
      withConnection db \conn -> do
        let createSQL = "CREATE TABLE items(id INTEGER);"
            insertSQL =
              "INSERT INTO items VALUES (1), (2), (3), (4), (5);"
        forM_ [createSQL, insertSQL] \sql ->
          withCString sql \cSql ->
            alloca \resPtr -> do
              st <- c_duckdb_query conn cSql resPtr
              st @?= DuckDBSuccess
              c_duckdb_destroy_result resPtr

        withCString "SELECT * FROM items" \selectSQL ->
          alloca \resPtr -> do
            st <- c_duckdb_query conn selectSQL resPtr
            st @?= DuckDBSuccess

            chunkCount <- c_duckdb_result_chunk_count resPtr
            assertBool "chunk count should be positive" (chunkCount > 0)

            returnType <- c_duckdb_result_return_type resPtr
            returnType @?= DuckDBResultTypeQueryResult

            streamingFlag <- c_duckdb_result_is_streaming resPtr
            streamingFlag @?= CBool 0

            -- Retrieve first chunk if available
            when (chunkCount > 0) $ do
              chunk0 <- c_duckdb_result_get_chunk resPtr 0
              assertBool "first chunk should not be null" (chunk0 /= nullPtr)
              -- Requesting beyond the available chunk count should return null
              chunkInvalid <- c_duckdb_result_get_chunk resPtr chunkCount
              assertBool "out-of-range chunk should be null" (chunkInvalid == nullPtr)

            c_duckdb_destroy_result resPtr
