{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module ResultFunctionsTest (tests) where

import Control.Monad (forM_, when)
import Data.Coerce (coerce)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Result Functions"
        [ chunkIntrospection
        ]

chunkIntrospection :: TestTree
chunkIntrospection =
    testCase "result chunk information and streaming state" $
        withDatabase \db ->
            withConnection db \conn -> do
                let createSQL = "CREATE TABLE items(id INTEGER);"
                    insertSQL =
                        "INSERT INTO items VALUES (1), (2), (3), (4), (5);"
                forM_ [createSQL, insertSQL] \sql ->
                    withConstCString sql \cSql ->
                        alloca \resPtr -> do
                            st <- c_duckdb_query conn cSql resPtr
                            st @?= DuckDBSuccess
                            c_duckdb_destroy_result resPtr

                withConstCString "SELECT * FROM items" \selectSQL ->
                    alloca \resPtr -> do
                        st <- c_duckdb_query conn selectSQL resPtr
                        st @?= DuckDBSuccess

                        chunkCount <- (peek resPtr >>= \rawValue -> c_duckdb_result_chunk_count rawValue)
                        assertBool "chunk count should be positive" (chunkCount > 0)

                        returnType <- (peek resPtr >>= \rawValue -> c_duckdb_result_return_type rawValue)
                        returnType @?= DUCKDB_RESULT_TYPE_QUERY_RESULT

                        streamingFlag <- (peek resPtr >>= \rawValue -> c_duckdb_result_is_streaming rawValue)
                        streamingFlag @?= CBool 0

                        -- Retrieve first chunk if available
                        when (chunkCount > 0) $ do
                            chunk0 <- (peek resPtr >>= \rawValue -> c_duckdb_result_get_chunk rawValue 0)
                            assertBool "first chunk should not be null" (chunk0 /= (coerce (nullPtr :: Ptr Void)))
                            -- Requesting beyond the available chunk count should return null
                            chunkInvalid <- (peek resPtr >>= \rawValue -> c_duckdb_result_get_chunk rawValue chunkCount)
                            assertBool "out-of-range chunk should be null" (chunkInvalid == (coerce (nullPtr :: Ptr Void)))

                        c_duckdb_destroy_result resPtr
