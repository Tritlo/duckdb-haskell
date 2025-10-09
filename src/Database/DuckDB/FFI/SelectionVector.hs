module Database.DuckDB.FFI.SelectionVector (
  c_duckdb_create_selection_vector,
  c_duckdb_destroy_selection_vector,
  c_duckdb_selection_vector_get_data_ptr
) where

import Database.DuckDB.FFI.Types
import Foreign.Ptr (Ptr)

-- | Creates a new selection vector of size @size@. Must be destroyed with
-- @duckdb_destroy_selection_vector@.
--
-- Parameters:
-- * @size@: The size of the selection vector.
--
-- Returns The selection vector.
foreign import ccall unsafe "duckdb_create_selection_vector"
  c_duckdb_create_selection_vector :: DuckDBIdx -> IO DuckDBSelectionVector

-- | Destroys the selection vector and de-allocates its memory.
--
-- Parameters:
-- * @sel@: The selection vector.
foreign import ccall unsafe "duckdb_destroy_selection_vector"
  c_duckdb_destroy_selection_vector :: DuckDBSelectionVector -> IO ()

-- | Access the data pointer of a selection vector.
--
-- Parameters:
-- * @sel@: The selection vector.
--
-- Returns The data pointer.
foreign import ccall unsafe "duckdb_selection_vector_get_data_ptr"
  c_duckdb_selection_vector_get_data_ptr :: DuckDBSelectionVector -> IO (Ptr DuckDBSel)
