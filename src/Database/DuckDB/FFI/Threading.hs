module Database.DuckDB.FFI.Threading (
  c_duckdb_execute_tasks,
  c_duckdb_create_task_state,
  c_duckdb_execute_tasks_state,
  c_duckdb_execute_n_tasks_state,
  c_duckdb_finish_execution,
  c_duckdb_task_state_is_finished,
  c_duckdb_destroy_task_state,
  c_duckdb_execution_is_finished
) where

import Database.DuckDB.FFI.Types
import Foreign.C.Types (CBool (..))

-- | Execute DuckDB tasks on this thread.
--
-- Will return after @max_tasks@ have been executed, or if there are no more
-- tasks present.
--
-- Parameters:
-- * @database@: The database object to execute tasks for
-- * @max_tasks@: The maximum amount of tasks to execute
foreign import ccall safe "duckdb_execute_tasks"
  c_duckdb_execute_tasks :: DuckDBDatabase -> DuckDBIdx -> IO ()

-- | Creates a task state that can be used with duckdb_execute_tasks_state to
-- execute tasks until @duckdb_finish_execution@ is called on the state.
--
-- @duckdb_destroy_state@ must be called on the result.
--
-- Parameters:
-- * @database@: The database object to create the task state for
--
-- Returns The task state that can be used with duckdb_execute_tasks_state.
foreign import ccall unsafe "duckdb_create_task_state"
  c_duckdb_create_task_state :: DuckDBDatabase -> IO DuckDBTaskState

-- | Execute DuckDB tasks on this thread.
--
-- The thread will keep on executing tasks forever, until duckdb_finish_execution
-- is called on the state. Multiple threads can share the same duckdb_task_state.
--
-- Parameters:
-- * @state@: The task state of the executor
foreign import ccall safe "duckdb_execute_tasks_state"
  c_duckdb_execute_tasks_state :: DuckDBTaskState -> IO ()

-- | Execute DuckDB tasks on this thread.
--
-- The thread will keep on executing tasks until either duckdb_finish_execution
-- is called on the state, max_tasks tasks have been executed or there are no
-- more tasks to be executed.
--
-- Multiple threads can share the same duckdb_task_state.
--
-- Parameters:
-- * @state@: The task state of the executor
-- * @max_tasks@: The maximum amount of tasks to execute
--
-- Returns The amount of tasks that have actually been executed
foreign import ccall safe "duckdb_execute_n_tasks_state"
  c_duckdb_execute_n_tasks_state :: DuckDBTaskState -> DuckDBIdx -> IO DuckDBIdx

-- | Finish execution on a specific task.
--
-- Parameters:
-- * @state@: The task state to finish execution
foreign import ccall unsafe "duckdb_finish_execution"
  c_duckdb_finish_execution :: DuckDBTaskState -> IO ()

-- | Check if the provided duckdb_task_state has finished execution
--
-- Parameters:
-- * @state@: The task state to inspect
--
-- Returns Whether or not duckdb_finish_execution has been called on the task
-- state
foreign import ccall unsafe "duckdb_task_state_is_finished"
  c_duckdb_task_state_is_finished :: DuckDBTaskState -> IO CBool

-- | Destroys the task state returned from duckdb_create_task_state.
--
-- Note that this should not be called while there is an active
-- duckdb_execute_tasks_state running on the task state.
--
-- Parameters:
-- * @state@: The task state to clean up
foreign import ccall unsafe "duckdb_destroy_task_state"
  c_duckdb_destroy_task_state :: DuckDBTaskState -> IO ()

-- | Returns true if the execution of the current query is finished.
--
-- Parameters:
-- * @con@: The connection on which to check
foreign import ccall unsafe "duckdb_execution_is_finished"
  c_duckdb_execution_is_finished :: DuckDBConnection -> IO CBool
