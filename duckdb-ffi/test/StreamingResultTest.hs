{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module StreamingResultTest (tests) where

import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..), CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase, withResult)

tests :: TestTree
tests =
    testGroup
        "Streaming Result Interface"
        [ streamingFetchConsumesAllChunks
        , materializedFetchChunkExhaustsResult
        ]

streamingFetchConsumesAllChunks :: TestTree
streamingFetchConsumesAllChunks =
    testCase "stream_fetch_chunk yields data until exhaustion" $
        withDatabase \db ->
            withConnection db \conn -> do
                setupTable conn "streaming_table" 6

                withConstCString "SELECT id FROM streaming_table ORDER BY id" \querySql ->
                    alloca \stmtPtr -> do
                        prepareOk <- c_duckdb_prepare conn querySql stmtPtr
                        prepareOk @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "prepared statement should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        alloca \pendingPtr -> do
                            stPending <- c_duckdb_pending_prepared_streaming stmt pendingPtr
                            stPending @?= DuckDBSuccess
                            pending <- peek pendingPtr
                            assertBool "pending result should not be null" (pending /= (coerce (nullPtr :: Ptr Void)))

                            void (c_duckdb_pending_execute_task pending)

                            alloca \resPtr -> do
                                execState <- c_duckdb_execute_pending pending resPtr
                                execState @?= DuckDBSuccess

                                streamingFlag <- (peek resPtr >>= \rawValue -> c_duckdb_result_is_streaming rawValue)
                                streamingFlag @?= CBool 1

                                totalRows <- consumeStreamingChunks resPtr 0
                                totalRows @?= 6

                                c_duckdb_destroy_result resPtr

                            c_duckdb_destroy_pending pendingPtr

                        c_duckdb_destroy_prepare stmtPtr

materializedFetchChunkExhaustsResult :: TestTree
materializedFetchChunkExhaustsResult =
    testCase "fetch_chunk provides chunk and then null for materialized result" $
        withDatabase \db ->
            withConnection db \conn -> do
                setupTable conn "materialized_table" 4

                withResult conn "SELECT id FROM materialized_table ORDER BY id" \resPtr -> do
                    chunk <- (peek resPtr >>= \rawValue -> c_duckdb_fetch_chunk rawValue)
                    assertBool "first fetch_chunk should yield a chunk" (chunk /= (coerce (nullPtr :: Ptr Void)))

                    chunkSize <- c_duckdb_data_chunk_get_size chunk
                    assertBool "materialized chunk should have rows" (chunkSize > 0)

                    destroyChunk chunk

                    chunkNext <- (peek resPtr >>= \rawValue -> c_duckdb_fetch_chunk rawValue)
                    chunkNext @?= (coerce (nullPtr :: Ptr Void))

-- helpers ------------------------------------------------------------------

consumeStreamingChunks :: Ptr DuckDBResult -> Int64 -> IO Int64
consumeStreamingChunks resPtr acc = do
    chunk <- (peek resPtr >>= \rawValue -> c_duckdb_stream_fetch_chunk rawValue)
    if chunk == (coerce (nullPtr :: Ptr Void))
        then pure acc
        else do
            chunkSize <- c_duckdb_data_chunk_get_size chunk
            assertBool "streaming chunk should not be empty" (chunkSize > 0)
            destroyChunk chunk
            consumeStreamingChunks resPtr (acc + fromIntegral chunkSize)

setupTable :: DuckDBConnection -> String -> Int -> IO ()
setupTable conn tableName totalRows = do
    withConstCString ("CREATE TABLE " <> tableName <> " (id INTEGER);") $ \createSql ->
        execStatement conn createSql
    let values = intercalate ", " ["(" <> show i <> ")" | i <- [1 .. totalRows]]
        insertSql = "INSERT INTO " <> tableName <> " VALUES " <> values <> ";"
    withConstCString insertSql $ \insertCStr ->
        execStatement conn insertCStr

execStatement :: DuckDBConnection -> (ConstPtr CChar) -> IO ()
execStatement conn sql =
    alloca \resPtr -> do
        st <- c_duckdb_query conn sql resPtr
        st @?= DuckDBSuccess
        c_duckdb_destroy_result resPtr

destroyChunk :: DuckDBDataChunk -> IO ()
destroyChunk chunk =
    alloca \ptr -> do
        poke ptr chunk
        c_duckdb_destroy_data_chunk ptr
