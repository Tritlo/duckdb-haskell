{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module PendingResultTest (tests) where

import Control.Monad (forM_, when)
import Data.Coerce (coerce)
import Data.Int (Int32, Int64)
import Data.Maybe (isNothing)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase)

tests :: TestTree
tests =
    testGroup
        "Pending Result Interface"
        [ pendingPreparedRoundtrip
        , pendingPreparedStreamingRoundtrip
        , pendingPreparedReportsError
        ]

withChunk :: Duckdb_data_chunk -> (Ptr Duckdb_data_chunk -> IO ()) -> IO ()
withChunk chunk action =
    alloca \chunkPtr -> do
        poke chunkPtr chunk
        action chunkPtr

pendingErrorMessage :: Duckdb_pending_result -> IO (Maybe String)
pendingErrorMessage pending = do
    errPtr <- duckdb_pending_error pending
    if errPtr == (coerce (nullPtr :: Ptr Void))
        then pure Nothing
        else Just <$> (peekCString . coerce) errPtr

assertPendingState :: Duckdb_pending_state -> IO ()
assertPendingState state =
    let valid =
            state == DUCKDB_PENDING_RESULT_READY
                || state == DUCKDB_PENDING_RESULT_NOT_READY
                || state == DUCKDB_PENDING_ERROR
                || state == DUCKDB_PENDING_NO_TASKS_AVAILABLE
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
                        withConstCString sql \cSql ->
                            alloca \resPtr -> do
                                st <- duckdb_query conn cSql resPtr
                                st @?= DuckDBSuccess
                                duckdb_destroy_result resPtr

                withConstCString "SELECT SUM(val) FROM pending_numbers;" \querySql ->
                    alloca \stmtPtr -> do
                        st <- duckdb_prepare conn querySql stmtPtr
                        st @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "prepared statement should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        alloca \pendingPtr -> do
                            stPending <- duckdb_pending_prepared stmt pendingPtr
                            stPending @?= DuckDBSuccess
                            pending <- peek pendingPtr
                            assertBool "pending result should not be null" (pending /= (coerce (nullPtr :: Ptr Void)))

                            stateBefore <- duckdb_pending_execute_check_state pending
                            assertPendingState stateBefore
                            _ <- duckdb_pending_execution_is_finished stateBefore

                            taskState <- duckdb_pending_execute_task pending
                            assertPendingState taskState
                            _ <- duckdb_pending_execution_is_finished taskState

                            alloca \resPtr -> do
                                execState <- duckdb_execute_pending pending resPtr
                                execState @?= DuckDBSuccess

                                resultFinished <- duckdb_pending_execute_check_state pending
                                assertPendingState resultFinished

                                rowCount <- duckdb_row_count resPtr
                                rowCount @?= 1
                                total <- duckdb_value_int64 resPtr 0 0
                                (total :: Int64) @?= 6

                                duckdb_destroy_result resPtr

                            errMsg <- pendingErrorMessage pending
                            assertBool "no error expected for successful pending execution" (isNothing errMsg)

                            duckdb_destroy_pending pendingPtr

                        duckdb_destroy_prepare stmtPtr

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
                        withConstCString sql \cSql ->
                            alloca \resPtr -> do
                                st <- duckdb_query conn cSql resPtr
                                st @?= DuckDBSuccess
                                duckdb_destroy_result resPtr

                withConstCString "SELECT * FROM pending_stream ORDER BY id;" \querySql ->
                    alloca \stmtPtr -> do
                        st <- duckdb_prepare conn querySql stmtPtr
                        st @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "prepared statement should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        alloca \pendingPtr -> do
                            stPending <- duckdb_pending_prepared_streaming stmt pendingPtr
                            stPending @?= DuckDBSuccess
                            pending <- peek pendingPtr
                            assertBool "pending result should not be null" (pending /= (coerce (nullPtr :: Ptr Void)))

                            stateBefore <- duckdb_pending_execute_check_state pending
                            assertPendingState stateBefore
                            _ <- duckdb_pending_execution_is_finished stateBefore

                            taskState <- duckdb_pending_execute_task pending
                            assertPendingState taskState
                            _ <- duckdb_pending_execution_is_finished taskState

                            alloca \resPtr -> do
                                execState <- duckdb_execute_pending pending resPtr
                                execState @?= DuckDBSuccess

                                streamingFlag <- (peek resPtr >>= \rawValue -> duckdb_result_is_streaming rawValue)
                                streamingFlag @?= CBool 1

                                chunk <- (peek resPtr >>= \rawValue -> duckdb_stream_fetch_chunk rawValue)
                                assertBool "streaming fetch should yield a chunk" (chunk /= (coerce (nullPtr :: Ptr Void)))

                                chunkSize <- duckdb_data_chunk_get_size chunk
                                assertBool "streamed chunk should have rows" (chunkSize > 0)

                                withChunk chunk duckdb_destroy_data_chunk
                                duckdb_destroy_result resPtr

                            errMsg <- pendingErrorMessage pending
                            assertBool "no error expected for successful streaming execution" (isNothing errMsg)

                            duckdb_destroy_pending pendingPtr

                        duckdb_destroy_prepare stmtPtr

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
                        withConstCString sql \cSql ->
                            alloca \resPtr -> do
                                st <- duckdb_query conn cSql resPtr
                                st @?= DuckDBSuccess
                                duckdb_destroy_result resPtr

                withConstCString "INSERT INTO pending_unique VALUES (?);" \insertSql ->
                    alloca \stmtPtr -> do
                        st <- duckdb_prepare conn insertSql stmtPtr
                        st @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "prepared insert statement should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        duckdb_bind_int32 stmt 1 (1 :: Int32) >>= (@?= DuckDBSuccess)

                        alloca \pendingPtr -> do
                            stPending <- duckdb_pending_prepared stmt pendingPtr
                            stPending @?= DuckDBSuccess
                            pending <- peek pendingPtr
                            assertBool "pending result should not be null" (pending /= (coerce (nullPtr :: Ptr Void)))

                            _ <- duckdb_pending_execute_check_state pending
                            taskState <- duckdb_pending_execute_task pending
                            assertPendingState taskState

                            alloca \resPtr -> do
                                execState <- duckdb_execute_pending pending resPtr
                                execState @?= DuckDBError

                            errMsg <- pendingErrorMessage pending
                            when (maybe False null errMsg) $
                                assertFailure "pending error message should not be empty when present"

                            duckdb_destroy_pending pendingPtr

                        duckdb_destroy_prepare stmtPtr
