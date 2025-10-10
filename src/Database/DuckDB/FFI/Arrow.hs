{-# LANGUAGE RecordWildCards #-}

module Database.DuckDB.FFI.Arrow (
  c_duckdb_to_arrow_schema,
  c_duckdb_data_chunk_to_arrow,
  c_duckdb_schema_from_arrow,
  c_duckdb_data_chunk_from_arrow,
  c_duckdb_destroy_arrow_converted_schema,
  c_duckdb_query_arrow,
  c_duckdb_query_arrow_schema,
  c_duckdb_prepared_arrow_schema,
  c_duckdb_result_arrow_array,
  c_duckdb_query_arrow_array,
  c_duckdb_arrow_column_count,
  c_duckdb_arrow_row_count,
  c_duckdb_arrow_rows_changed,
  c_duckdb_query_arrow_error,
  c_duckdb_destroy_arrow,
  c_duckdb_destroy_arrow_stream,
  c_duckdb_execute_prepared_arrow,
  c_duckdb_arrow_scan,
  c_duckdb_arrow_array_scan,
) where

import Data.Int (Int64)
import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (Storable (..), peekByteOff, pokeByteOff, sizeOf)

-- | Transforms a DuckDB Schema into an Arrow Schema
--
-- Parameters:
-- * @arrow_options@: The Arrow settings used to produce arrow.
-- * @types@: The DuckDB logical types for each column in the schema.
-- * @names@: The names for each column in the schema.
-- * @column_count@: The number of columns that exist in the schema.
-- * @out_schema@: The resulting arrow schema. Must be destroyed with
--   @out_schema->release(out_schema)@.
--
-- Returns The error data. Must be destroyed with @duckdb_destroy_error_data@.
foreign import ccall unsafe "duckdb_to_arrow_schema"
  c_duckdb_to_arrow_schema :: DuckDBArrowOptions -> Ptr DuckDBLogicalType -> Ptr CString -> DuckDBIdx -> Ptr ArrowSchema -> IO DuckDBErrorData

-- | Transforms a DuckDB data chunk into an Arrow array.
--
-- Parameters:
-- * @arrow_options@: The Arrow settings used to produce arrow.
-- * @chunk@: The DuckDB data chunk to convert.
-- * @out_arrow_array@: The output Arrow structure that will hold the converted
--   data. Must be released with @out_arrow_array->release(out_arrow_array)@
--
-- Returns The error data. Must be destroyed with @duckdb_destroy_error_data@.
foreign import ccall unsafe "duckdb_data_chunk_to_arrow"
  c_duckdb_data_chunk_to_arrow :: DuckDBArrowOptions -> DuckDBDataChunk -> Ptr ArrowArray -> IO DuckDBErrorData

-- | Transforms an Arrow Schema into a DuckDB Schema.
--
-- Parameters:
-- * @connection@: The connection to get the transformation settings from.
-- * @schema@: The input Arrow schema. Must be released with
--   @schema->release(schema)@.
-- * @out_types@: The Arrow converted schema with extra information about the
--   arrow types. Must be destroyed with @duckdb_destroy_arrow_converted_schema@.
--
-- Returns The error data. Must be destroyed with @duckdb_destroy_error_data@.
foreign import ccall unsafe "duckdb_schema_from_arrow"
  c_duckdb_schema_from_arrow :: DuckDBConnection -> Ptr ArrowSchema -> Ptr DuckDBArrowConvertedSchema -> IO DuckDBErrorData

-- | Transforms an Arrow array into a DuckDB data chunk. The data chunk will retain
-- ownership of the underlying Arrow data.
--
-- Parameters:
-- * @connection@: The connection to get the transformation settings from.
-- * @arrow_array@: The input Arrow array. Data ownership is passed on to
--   DuckDB's DataChunk, the underlying object does not need to be released and
--   won't have ownership of the data.
-- * @converted_schema@: The Arrow converted schema with extra information about
--   the arrow types.
-- * @out_chunk@: The resulting DuckDB data chunk. Must be destroyed by
--   duckdb_destroy_data_chunk.
--
-- Returns The error data. Must be destroyed with @duckdb_destroy_error_data@.
foreign import ccall unsafe "duckdb_data_chunk_from_arrow"
  c_duckdb_data_chunk_from_arrow :: DuckDBConnection -> Ptr ArrowArray -> DuckDBArrowConvertedSchema -> Ptr DuckDBDataChunk -> IO DuckDBErrorData

-- | Destroys the arrow converted schema and de-allocates all memory allocated for
-- that arrow converted schema.
--
-- Parameters:
-- * @arrow_converted_schema@: The arrow converted schema to destroy.
foreign import ccall unsafe "duckdb_destroy_arrow_converted_schema"
  c_duckdb_destroy_arrow_converted_schema :: Ptr DuckDBArrowConvertedSchema -> IO ()

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Executes a SQL query within a connection and stores the full (materialized)
-- result in an arrow structure. If the query fails to execute, DuckDBError is
-- returned and the error message can be retrieved by calling
-- @duckdb_query_arrow_error@.
--
-- Note that after running @duckdb_query_arrow@, @duckdb_destroy_arrow@ must be
-- called on the result object even if the query fails, otherwise the error
-- stored within the result will not be freed correctly.
--
-- Parameters:
-- * @connection@: The connection to perform the query in.
-- * @query@: The SQL query to run.
-- * @out_result@: The query result.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall safe "duckdb_query_arrow"
  c_duckdb_query_arrow :: DuckDBConnection -> CString -> Ptr DuckDBArrow -> IO DuckDBState

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Fetch the internal arrow schema from the arrow result. Remember to call
-- release on the respective ArrowSchema object.
--
-- Parameters:
-- * @result@: The result to fetch the schema from.
-- * @out_schema@: The output schema.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_query_arrow_schema"
  c_duckdb_query_arrow_schema :: DuckDBArrow -> Ptr DuckDBArrowSchema -> IO DuckDBState

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Fetch the internal arrow schema from the prepared statement. Remember to call
-- release on the respective ArrowSchema object.
--
-- Parameters:
-- * @prepared@: The prepared statement to fetch the schema from.
-- * @out_schema@: The output schema.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_prepared_arrow_schema"
  c_duckdb_prepared_arrow_schema :: DuckDBPreparedStatement -> Ptr DuckDBArrowSchema -> IO DuckDBState

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Convert a data chunk into an arrow struct array. Remember to call release on
-- the respective ArrowArray object.
--
-- Parameters:
-- * @result@: The result object the data chunk have been fetched from.
-- * @chunk@: The data chunk to convert.
-- * @out_array@: The output array.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_result_arrow_array@ but mirror the DuckDB C API semantics of
-- @duckdb_result_arrow_array@.
foreign import ccall unsafe "wrapped_duckdb_result_arrow_array"
  c_duckdb_result_arrow_array :: Ptr DuckDBResult -> DuckDBDataChunk -> Ptr DuckDBArrowArray -> IO ()

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Fetch an internal arrow struct array from the arrow result. Remember to call
-- release on the respective ArrowArray object.
--
-- This function can be called multiple time to get next chunks, which will free
-- the previous out_array. So consume the out_array before calling this function
-- again.
--
-- Parameters:
-- * @result@: The result to fetch the array from.
-- * @out_array@: The output array.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall safe "duckdb_query_arrow_array"
  c_duckdb_query_arrow_array :: DuckDBArrow -> Ptr DuckDBArrowArray -> IO DuckDBState

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns the number of columns present in the arrow result object.
--
-- Parameters:
-- * @result@: The result object.
--
-- Returns The number of columns present in the result object.
foreign import ccall unsafe "duckdb_arrow_column_count"
  c_duckdb_arrow_column_count :: DuckDBArrow -> IO DuckDBIdx

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns the number of rows present in the arrow result object.
--
-- Parameters:
-- * @result@: The result object.
--
-- Returns The number of rows present in the result object.
foreign import ccall unsafe "duckdb_arrow_row_count"
  c_duckdb_arrow_row_count :: DuckDBArrow -> IO DuckDBIdx

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns the number of rows changed by the query stored in the arrow result.
-- This is relevant only for INSERT/UPDATE/DELETE queries. For other queries the
-- rows_changed will be 0.
--
-- Parameters:
-- * @result@: The result object.
--
-- Returns The number of rows changed.
foreign import ccall unsafe "duckdb_arrow_rows_changed"
  c_duckdb_arrow_rows_changed :: DuckDBArrow -> IO DuckDBIdx

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns the error message contained within the result. The error is only set
-- if @duckdb_query_arrow@ returns @DuckDBError@.
--
-- The error message should not be freed. It will be de-allocated when
-- @duckdb_destroy_arrow@ is called.
--
-- Parameters:
-- * @result@: The result object to fetch the error from.
--
-- Returns The error of the result.
foreign import ccall unsafe "duckdb_query_arrow_error"
  c_duckdb_query_arrow_error :: DuckDBArrow -> IO CString

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Closes the result and de-allocates all memory allocated for the arrow result.
--
-- Parameters:
-- * @result@: The result to destroy.
foreign import ccall unsafe "duckdb_destroy_arrow"
  c_duckdb_destroy_arrow :: Ptr DuckDBArrow -> IO ()

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Releases the arrow array stream and de-allocates its memory.
--
-- Parameters:
-- * @stream_p@: The arrow array stream to destroy.
foreign import ccall unsafe "duckdb_destroy_arrow_stream"
  c_duckdb_destroy_arrow_stream :: Ptr DuckDBArrowStream -> IO ()

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Executes the prepared statement with the given bound parameters, and returns
-- an arrow query result. Note that after running
-- @duckdb_execute_prepared_arrow@, @duckdb_destroy_arrow@ must be called on the
-- result object.
--
-- Parameters:
-- * @prepared_statement@: The prepared statement to execute.
-- * @out_result@: The query result.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall safe "duckdb_execute_prepared_arrow"
  c_duckdb_execute_prepared_arrow :: DuckDBPreparedStatement -> Ptr DuckDBArrow -> IO DuckDBState

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Scans the Arrow stream and creates a view with the given name.
--
-- Parameters:
-- * @connection@: The connection on which to execute the scan.
-- * @table_name@: Name of the temporary view to create.
-- * @arrow@: Arrow stream wrapper.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall safe "duckdb_arrow_scan"
  c_duckdb_arrow_scan :: DuckDBConnection -> CString -> DuckDBArrowStream -> IO DuckDBState

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Scans the Arrow array and creates a view with the given name. Note that after
-- running @duckdb_arrow_array_scan@, @duckdb_destroy_arrow_stream@ must be
-- called on the out stream.
--
-- Parameters:
-- * @connection@: The connection on which to execute the scan.
-- * @table_name@: Name of the temporary view to create.
-- * @arrow_schema@: Arrow schema wrapper.
-- * @arrow_array@: Arrow array wrapper.
-- * @out_stream@: Output array stream that wraps around the passed schema, for
--   releasing/deleting once done.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall safe "duckdb_arrow_array_scan"
  c_duckdb_arrow_array_scan :: DuckDBConnection -> CString -> DuckDBArrowSchema -> DuckDBArrowArray -> Ptr DuckDBArrowStream -> IO DuckDBState
