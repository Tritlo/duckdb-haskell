{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module ThreadingTest (tests) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..), CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withConstCString)

tests :: TestTree
tests =
    testGroup
        "Threading Information"
        [ executeTasksCompletesPendingQuery
        , taskStateControlsExecutionLifecycle
        ]

{- | Open a database that has no worker threads. Without this, the
worker threads of DuckDB start a pending query immediately, and the
test races them. With one thread, only 'duckdb_execute_tasks' can
make progress, so the checks are deterministic.
-}
withSingleThreadedDatabase :: (Duckdb_database -> IO a) -> IO a
withSingleThreadedDatabase action =
    withConstCString ":memory:" \path ->
        alloca \configPtr -> do
            duckdb_create_config configPtr >>= (@?= DuckDBSuccess)
            config <- peek configPtr
            withConstCString "threads" \flag ->
                withConstCString "1" \value ->
                    duckdb_set_config config flag value >>= (@?= DuckDBSuccess)
            alloca \dbPtr -> do
                st <- duckdb_open_ext path dbPtr config (coerce (nullPtr :: Ptr Void))
                duckdb_destroy_config configPtr
                st @?= DuckDBSuccess
                db <- peek dbPtr
                result <- action db
                duckdb_close dbPtr
                pure result

executeTasksCompletesPendingQuery :: TestTree
executeTasksCompletesPendingQuery =
    testCase "execute_tasks drives pending query to completion" $
        withSingleThreadedDatabase \db ->
            withConnection db \conn -> do
                setupAggTable conn

                withConstCString "SELECT SUM(val) FROM threading_numbers;" \querySql ->
                    alloca \stmtPtr -> do
                        stPrepare <- duckdb_prepare conn querySql stmtPtr
                        stPrepare @?= DuckDBSuccess
                        stmt <- peek stmtPtr
                        assertBool "prepared statement should not be null" (stmt /= (coerce (nullPtr :: Ptr Void)))

                        alloca \pendingPtr -> do
                            stPending <- duckdb_pending_prepared stmt pendingPtr
                            stPending @?= DuckDBSuccess
                            pending <- peek pendingPtr
                            assertBool "pending result should not be null" (pending /= (coerce (nullPtr :: Ptr Void)))

                            finishedBefore <- duckdb_execution_is_finished conn
                            finishedBefore @?= CBool 0

                            driveTasks db conn

                            finishedAfter <- duckdb_execution_is_finished conn
                            finishedAfter @?= CBool 1

                            alloca \resPtr -> do
                                stExec <- duckdb_execute_pending pending resPtr
                                stExec @?= DuckDBSuccess
                                sumVal <- duckdb_value_int64 resPtr 0 0
                                (sumVal :: Int64) @?= 15
                                duckdb_destroy_result resPtr

                            duckdb_destroy_pending pendingPtr

                        duckdb_destroy_prepare stmtPtr

taskStateControlsExecutionLifecycle :: TestTree
taskStateControlsExecutionLifecycle =
    testCase "task state executes batches and finishes on request" $
        withSingleThreadedDatabase \db ->
            withConnection db \conn -> do
                setupAggTable conn

                taskState <- duckdb_create_task_state db
                assertBool "task state should not be null" (taskState /= (coerce (nullPtr :: Ptr Void)))

                doneVar <- newEmptyMVar
                _ <- forkFinally (duckdb_execute_tasks_state taskState) (const (putMVar doneVar ()))

                executed <- duckdb_execute_n_tasks_state taskState 0
                assertBool "execute_n_tasks_state should not report negative work" (executed >= 0)

                isFinishedBefore <- duckdb_task_state_is_finished taskState
                isFinishedBefore @?= CBool 0

                duckdb_finish_execution taskState

                isFinishedAfter <- duckdb_task_state_is_finished taskState
                isFinishedAfter @?= CBool 1

                takeMVar doneVar
                duckdb_destroy_task_state taskState

setupAggTable :: Duckdb_connection -> IO ()
setupAggTable conn = do
    withConstCString "CREATE TABLE threading_numbers(val INTEGER);" $ \createSql ->
        execStatement conn createSql
    withConstCString "INSERT INTO threading_numbers VALUES (1), (2), (3), (4), (5);" $ \insertSql ->
        execStatement conn insertSql

driveTasks :: Duckdb_database -> Duckdb_connection -> IO ()
driveTasks db conn = go 0
  where
    go :: Int -> IO ()
    go attempts
        | attempts > 10 = assertFailure "execute_tasks did not finish query within expected iterations"
        | otherwise = do
            duckdb_execute_tasks db 1000
            finished <- duckdb_execution_is_finished conn
            if finished == CBool 1
                then pure ()
                else go (attempts + 1)

execStatement :: Duckdb_connection -> (ConstPtr CChar) -> IO ()
execStatement conn sql =
    alloca \resPtr -> do
        st <- duckdb_query conn sql resPtr
        st @?= DuckDBSuccess
        duckdb_destroy_result resPtr
