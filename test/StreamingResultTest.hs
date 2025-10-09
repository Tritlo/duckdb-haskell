{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module StreamingResultTest (tests) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.List (intercalate)
import Database.DuckDB.FFI
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

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

        withCString "SELECT id FROM streaming_table ORDER BY id" \querySql ->
          alloca \stmtPtr -> do
            prepareOk <- c_duckdb_prepare conn querySql stmtPtr
            prepareOk @?= DuckDBSuccess
            stmt <- peek stmtPtr
            assertBool "prepared statement should not be null" (stmt /= nullPtr)

            alloca \pendingPtr -> do
              stPending <- c_duckdb_pending_prepared_streaming stmt pendingPtr
              stPending @?= DuckDBSuccess
              pending <- peek pendingPtr
              assertBool "pending result should not be null" (pending /= nullPtr)

              void (c_duckdb_pending_execute_task pending)

              alloca \resPtr -> do
                execState <- c_duckdb_execute_pending pending resPtr
                execState @?= DuckDBSuccess

                streamingFlag <- c_duckdb_result_is_streaming resPtr
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
          chunk <- c_duckdb_fetch_chunk resPtr
          assertBool "first fetch_chunk should yield a chunk" (chunk /= nullPtr)

          chunkSize <- c_duckdb_data_chunk_get_size chunk
          assertBool "materialized chunk should have rows" (chunkSize > 0)

          destroyChunk chunk

          chunkNext <- c_duckdb_fetch_chunk resPtr
          chunkNext @?= nullPtr

-- helpers ------------------------------------------------------------------

consumeStreamingChunks :: Ptr DuckDBResult -> Int64 -> IO Int64
consumeStreamingChunks resPtr acc = do
  chunk <- c_duckdb_stream_fetch_chunk resPtr
  if chunk == nullPtr
    then pure acc
    else do
      chunkSize <- c_duckdb_data_chunk_get_size chunk
      assertBool "streaming chunk should not be empty" (chunkSize > 0)
      destroyChunk chunk
      consumeStreamingChunks resPtr (acc + fromIntegral chunkSize)

setupTable :: DuckDBConnection -> String -> Int -> IO ()
setupTable conn tableName totalRows = do
  withCString ("CREATE TABLE " <> tableName <> " (id INTEGER);") \createSql ->
    execStatement conn createSql
  let values = intercalate ", " [ "(" <> show i <> ")" | i <- [1 .. totalRows] ]
      insertSql = "INSERT INTO " <> tableName <> " VALUES " <> values <> ";"
  withCString insertSql \insertCStr ->
    execStatement conn insertCStr

execStatement :: DuckDBConnection -> CString -> IO ()
execStatement conn sql =
  alloca \resPtr -> do
    st <- c_duckdb_query conn sql resPtr
    st @?= DuckDBSuccess
    c_duckdb_destroy_result resPtr

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase action =
  withCString ":memory:" \path ->
    alloca \dbPtr -> do
      st <- c_duckdb_open path dbPtr
      st @?= DuckDBSuccess
      db <- peek dbPtr
      result <- action db
      c_duckdb_close dbPtr
      pure result

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db action =
  alloca \connPtr -> do
    st <- c_duckdb_connect db connPtr
    st @?= DuckDBSuccess
    conn <- peek connPtr
    result <- action conn
    c_duckdb_disconnect connPtr
    pure result

withResult :: DuckDBConnection -> String -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn sql action =
  withCString sql \sqlPtr ->
    alloca \resPtr -> do
      st <- c_duckdb_query conn sqlPtr resPtr
      st @?= DuckDBSuccess
      result <- action resPtr
      c_duckdb_destroy_result resPtr
      pure result

destroyChunk :: DuckDBDataChunk -> IO ()
destroyChunk chunk =
  alloca \ptr -> do
    poke ptr chunk
    c_duckdb_destroy_data_chunk ptr
