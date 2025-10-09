module Database.DuckDB.FFI.DataChunk (
  c_duckdb_create_data_chunk,
  c_duckdb_destroy_data_chunk,
  c_duckdb_data_chunk_reset,
  c_duckdb_data_chunk_get_column_count,
  c_duckdb_data_chunk_get_vector,
  c_duckdb_data_chunk_get_size,
  c_duckdb_data_chunk_set_size
) where

import Database.DuckDB.FFI.Types
import Foreign.Ptr (Ptr)

-- | Creates an empty data chunk with the specified column types. The result must
-- be destroyed with @duckdb_destroy_data_chunk@.
--
-- Parameters:
-- * @types@: An array of column types. Column types can not contain ANY and
--   INVALID types.
-- * @column_count@: The number of columns.
--
-- Returns The data chunk.
foreign import ccall unsafe "duckdb_create_data_chunk"
  c_duckdb_create_data_chunk :: Ptr DuckDBLogicalType -> DuckDBIdx -> IO DuckDBDataChunk

-- | Destroys the data chunk and de-allocates all memory allocated for that chunk.
--
-- Parameters:
-- * @chunk@: The data chunk to destroy.
foreign import ccall unsafe "duckdb_destroy_data_chunk"
  c_duckdb_destroy_data_chunk :: Ptr DuckDBDataChunk -> IO ()

-- | Resets a data chunk, clearing the validity masks and setting the cardinality
-- of the data chunk to 0. After calling this method, you must call
-- @duckdb_vector_get_validity@ and @duckdb_vector_get_data@ to obtain current
-- data and validity pointers
--
-- Parameters:
-- * @chunk@: The data chunk to reset.
foreign import ccall unsafe "duckdb_data_chunk_reset"
  c_duckdb_data_chunk_reset :: DuckDBDataChunk -> IO ()

-- | Retrieves the number of columns in a data chunk.
--
-- Parameters:
-- * @chunk@: The data chunk to get the data from
--
-- Returns The number of columns in the data chunk
foreign import ccall unsafe "duckdb_data_chunk_get_column_count"
  c_duckdb_data_chunk_get_column_count :: DuckDBDataChunk -> IO DuckDBIdx

-- | Retrieves the vector at the specified column index in the data chunk.
--
-- The pointer to the vector is valid for as long as the chunk is alive. It does
-- NOT need to be destroyed.
--
-- Parameters:
-- * @chunk@: The data chunk to get the data from
--
-- Returns The vector
foreign import ccall unsafe "duckdb_data_chunk_get_vector"
  c_duckdb_data_chunk_get_vector :: DuckDBDataChunk -> DuckDBIdx -> IO DuckDBVector

-- | Retrieves the current number of tuples in a data chunk.
--
-- Parameters:
-- * @chunk@: The data chunk to get the data from
--
-- Returns The number of tuples in the data chunk
foreign import ccall unsafe "duckdb_data_chunk_get_size"
  c_duckdb_data_chunk_get_size :: DuckDBDataChunk -> IO DuckDBIdx

-- | Sets the current number of tuples in a data chunk.
--
-- Parameters:
-- * @chunk@: The data chunk to set the size in
-- * @size@: The number of tuples in the data chunk
foreign import ccall unsafe "duckdb_data_chunk_set_size"
  c_duckdb_data_chunk_set_size :: DuckDBDataChunk -> DuckDBIdx -> IO ()
