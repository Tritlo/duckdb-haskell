{-# LANGUAGE BlockArguments #-}

module PendingResultTest (tests) where

import Control.Monad (forM_, when)
import Data.Int (Int32, Int64)
import Data.Maybe (isNothing)
import Database.DuckDB.FFI
import Database.DuckDB.FFI.Deprecated
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Pending Result Interface"
        [ pendingPreparedRoundtrip
        , pendingPreparedStreamingRoundtrip
        , pendingPreparedReportsError
        ]

withChunk :: DuckDBDataChunk -> (Ptr DuckDBDataChunk -> IO ()) -> IO ()
withChunk chunk action =
    alloca \chunkPtr -> do
        poke chunkPtr chunk
        action chunkPtr

pendingErrorMessage :: DuckDBPendingResult -> IO (Maybe String)
pendingErrorMessage pending = do
    errPtr <- c_duckdb_pending_error pending
    if errPtr == nullPtr
        then pure Nothing
        else Just <$> peekCString errPtr

assertPendingState :: DuckDBPendingState -> IO ()
assertPendingState state =
    let valid =
            state == DuckDBPendingResultReady
                || state == DuckDBPendingResultNotReady
                || state == DuckDBPendingError
                || state == DuckDBPendingNoTasksAvailable
     in assertBool "unexpected pending state" valid

pendingPreparedRoundtrip :: TestTree
pendingPreparedRoundtrip =
    testCase "pending_prepared executes query to completion" $
        withDatabase \db ->
            withConnection db \conn -> do
                forM_
                    [ "CREATE TABLE pending_numbers(val INTEGER);"
                    , "INSERT INTO pending_numbers VALUES (1), (2), (3);"
                    ]
                    \sql ->
                        withCString sql \cSql ->
                            alloca \resPtr -> do
                                st <- c_duckdb_query conn cSql resPtr
                                st @?= DuckDBSuccess
                                c_duckdb_destroy_result resPtr

                withCString "SELECT SUM(val) FROM pending_numbers;" \querySql ->
                    alloca \stmtPtr -> do
                        st <- c_duckdb_prepare conn querySql stmtPtr
                        st @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "prepared statement should not be null" (stmt /= nullPtr)

                        alloca \pendingPtr -> do
                            stPending <- c_duckdb_pending_prepared stmt pendingPtr
                            stPending @?= DuckDBSuccess
                            pending <- peek pendingPtr
                            assertBool "pending result should not be null" (pending /= nullPtr)

                            stateBefore <- c_duckdb_pending_execute_check_state pending
                            assertPendingState stateBefore
                            _ <- c_duckdb_pending_execution_is_finished stateBefore

                            taskState <- c_duckdb_pending_execute_task pending
                            assertPendingState taskState
                            _ <- c_duckdb_pending_execution_is_finished taskState

                            alloca \resPtr -> do
                                execState <- c_duckdb_execute_pending pending resPtr
                                execState @?= DuckDBSuccess

                                resultFinished <- c_duckdb_pending_execute_check_state pending
                                assertPendingState resultFinished

                                rowCount <- c_duckdb_row_count resPtr
                                rowCount @?= 1
                                total <- c_duckdb_value_int64 resPtr 0 0
                                (total :: Int64) @?= 6

                                c_duckdb_destroy_result resPtr

                            errMsg <- pendingErrorMessage pending
                            assertBool "no error expected for successful pending execution" (isNothing errMsg)

                            c_duckdb_destroy_pending pendingPtr

                        c_duckdb_destroy_prepare stmtPtr

pendingPreparedStreamingRoundtrip :: TestTree
pendingPreparedStreamingRoundtrip =
    testCase "pending_prepared_streaming yields streaming duckdb_result" $
        withDatabase \db ->
            withConnection db \conn -> do
                forM_
                    [ "CREATE TABLE pending_stream(id INTEGER);"
                    , "INSERT INTO pending_stream VALUES (1), (2), (3), (4);"
                    ]
                    \sql ->
                        withCString sql \cSql ->
                            alloca \resPtr -> do
                                st <- c_duckdb_query conn cSql resPtr
                                st @?= DuckDBSuccess
                                c_duckdb_destroy_result resPtr

                withCString "SELECT * FROM pending_stream ORDER BY id;" \querySql ->
                    alloca \stmtPtr -> do
                        st <- c_duckdb_prepare conn querySql stmtPtr
                        st @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "prepared statement should not be null" (stmt /= nullPtr)

                        alloca \pendingPtr -> do
                            stPending <- c_duckdb_pending_prepared_streaming stmt pendingPtr
                            stPending @?= DuckDBSuccess
                            pending <- peek pendingPtr
                            assertBool "pending result should not be null" (pending /= nullPtr)

                            stateBefore <- c_duckdb_pending_execute_check_state pending
                            assertPendingState stateBefore
                            _ <- c_duckdb_pending_execution_is_finished stateBefore

                            taskState <- c_duckdb_pending_execute_task pending
                            assertPendingState taskState
                            _ <- c_duckdb_pending_execution_is_finished taskState

                            alloca \resPtr -> do
                                execState <- c_duckdb_execute_pending pending resPtr
                                execState @?= DuckDBSuccess

                                streamingFlag <- c_duckdb_result_is_streaming resPtr
                                streamingFlag @?= CBool 1

                                chunk <- c_duckdb_stream_fetch_chunk resPtr
                                assertBool "streaming fetch should yield a chunk" (chunk /= nullPtr)

                                chunkSize <- c_duckdb_data_chunk_get_size chunk
                                assertBool "streamed chunk should have rows" (chunkSize > 0)

                                withChunk chunk c_duckdb_destroy_data_chunk
                                c_duckdb_destroy_result resPtr

                            errMsg <- pendingErrorMessage pending
                            assertBool "no error expected for successful streaming execution" (isNothing errMsg)

                            c_duckdb_destroy_pending pendingPtr

                        c_duckdb_destroy_prepare stmtPtr

pendingPreparedReportsError :: TestTree
pendingPreparedReportsError =
    testCase "pending execution surfaces failure details" $
        withDatabase \db ->
            withConnection db \conn -> do
                forM_
                    [ "CREATE TABLE pending_unique(val INTEGER PRIMARY KEY);"
                    , "INSERT INTO pending_unique VALUES (1);"
                    ]
                    \sql ->
                        withCString sql \cSql ->
                            alloca \resPtr -> do
                                st <- c_duckdb_query conn cSql resPtr
                                st @?= DuckDBSuccess
                                c_duckdb_destroy_result resPtr

                withCString "INSERT INTO pending_unique VALUES (?);" \insertSql ->
                    alloca \stmtPtr -> do
                        st <- c_duckdb_prepare conn insertSql stmtPtr
                        st @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "prepared insert statement should not be null" (stmt /= nullPtr)

                        c_duckdb_bind_int32 stmt 1 (1 :: Int32) >>= (@?= DuckDBSuccess)

                        alloca \pendingPtr -> do
                            stPending <- c_duckdb_pending_prepared stmt pendingPtr
                            stPending @?= DuckDBSuccess
                            pending <- peek pendingPtr
                            assertBool "pending result should not be null" (pending /= nullPtr)

                            _ <- c_duckdb_pending_execute_check_state pending
                            taskState <- c_duckdb_pending_execute_task pending
                            assertPendingState taskState

                            alloca \resPtr -> do
                                execState <- c_duckdb_execute_pending pending resPtr
                                execState @?= DuckDBError

                            errMsg <- pendingErrorMessage pending
                            when (maybe False null errMsg) $
                                assertFailure "pending error message should not be empty when present"

                            c_duckdb_destroy_pending pendingPtr

                        c_duckdb_destroy_prepare stmtPtr
