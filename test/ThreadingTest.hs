{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module ThreadingTest (tests) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Data.Int (Int64)
import Database.DuckDB.FFI
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Threading Information"
    [ executeTasksCompletesPendingQuery
    , taskStateControlsExecutionLifecycle
    ]

executeTasksCompletesPendingQuery :: TestTree
executeTasksCompletesPendingQuery =
  testCase "execute_tasks drives pending query to completion" $
    withDatabase \db ->
      withConnection db \conn -> do
        setupAggTable conn

        withCString "SELECT SUM(val) FROM threading_numbers;" \querySql ->
          alloca \stmtPtr -> do
            stPrepare <- c_duckdb_prepare conn querySql stmtPtr
            stPrepare @?= DuckDBSuccess
            stmt <- peek stmtPtr
            assertBool "prepared statement should not be null" (stmt /= nullPtr)

            alloca \pendingPtr -> do
              stPending <- c_duckdb_pending_prepared stmt pendingPtr
              stPending @?= DuckDBSuccess
              pending <- peek pendingPtr
              assertBool "pending result should not be null" (pending /= nullPtr)

              finishedBefore <- c_duckdb_execution_is_finished conn
              finishedBefore @?= CBool 0

              driveTasks db conn

              finishedAfter <- c_duckdb_execution_is_finished conn
              finishedAfter @?= CBool 1

              alloca \resPtr -> do
                stExec <- c_duckdb_execute_pending pending resPtr
                stExec @?= DuckDBSuccess
                sumVal <- c_duckdb_value_int64 resPtr 0 0
                (sumVal :: Int64) @?= 15
                c_duckdb_destroy_result resPtr

              c_duckdb_destroy_pending pendingPtr

            c_duckdb_destroy_prepare stmtPtr

taskStateControlsExecutionLifecycle :: TestTree
taskStateControlsExecutionLifecycle =
  testCase "task state executes batches and finishes on request" $
    withDatabase \db ->
      withConnection db \conn -> do
        setupAggTable conn

        taskState <- c_duckdb_create_task_state db
        assertBool "task state should not be null" (taskState /= nullPtr)

        doneVar <- newEmptyMVar
        _ <- forkFinally (c_duckdb_execute_tasks_state taskState) (const (putMVar doneVar ()))

        executed <- c_duckdb_execute_n_tasks_state taskState 0
        assertBool "execute_n_tasks_state should not report negative work" (executed >= 0)

        isFinishedBefore <- c_duckdb_task_state_is_finished taskState
        isFinishedBefore @?= CBool 0

        c_duckdb_finish_execution taskState

        isFinishedAfter <- c_duckdb_task_state_is_finished taskState
        isFinishedAfter @?= CBool 1

        takeMVar doneVar
        c_duckdb_destroy_task_state taskState

setupAggTable :: DuckDBConnection -> IO ()
setupAggTable conn = do
  withCString "CREATE TABLE threading_numbers(val INTEGER);" \createSql ->
    execStatement conn createSql
  withCString "INSERT INTO threading_numbers VALUES (1), (2), (3), (4), (5);" \insertSql ->
    execStatement conn insertSql

driveTasks :: DuckDBDatabase -> DuckDBConnection -> IO ()
driveTasks db conn = go 0
  where
    go attempts
      | attempts > 10 = assertFailure "execute_tasks did not finish query within expected iterations"
      | otherwise = do
          c_duckdb_execute_tasks db 1000
          finished <- c_duckdb_execution_is_finished conn
          if finished == CBool 1
            then pure ()
            else go (attempts + 1)

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
