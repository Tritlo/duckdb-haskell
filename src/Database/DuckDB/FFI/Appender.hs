module Database.DuckDB.FFI.Appender (
  c_duckdb_appender_create,
  c_duckdb_appender_create_ext,
  c_duckdb_appender_create_query,
  c_duckdb_appender_column_count,
  c_duckdb_appender_column_type,
  c_duckdb_appender_error,
  c_duckdb_appender_error_data,
  c_duckdb_appender_flush,
  c_duckdb_appender_close,
  c_duckdb_appender_destroy,
  c_duckdb_appender_add_column,
  c_duckdb_appender_clear_columns,
  c_duckdb_appender_begin_row,
  c_duckdb_appender_end_row,
  c_duckdb_append_default,
  c_duckdb_append_default_to_chunk,
  c_duckdb_append_bool,
  c_duckdb_append_int8,
  c_duckdb_append_int16,
  c_duckdb_append_int32,
  c_duckdb_append_int64,
  c_duckdb_append_hugeint,
  c_duckdb_append_uint8,
  c_duckdb_append_uint16,
  c_duckdb_append_uint32,
  c_duckdb_append_uint64,
  c_duckdb_append_uhugeint,
  c_duckdb_append_float,
  c_duckdb_append_double,
  c_duckdb_append_date,
  c_duckdb_append_time,
  c_duckdb_append_timestamp,
  c_duckdb_append_interval,
  c_duckdb_append_varchar,
  c_duckdb_append_varchar_length,
  c_duckdb_append_blob,
  c_duckdb_append_null,
  c_duckdb_append_value,
  c_duckdb_append_data_chunk
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CDouble (..), CFloat (..), CInt (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr)

-- | Creates an appender object.
--
-- Note that the object must be destroyed with @duckdb_appender_destroy@.
--
-- Parameters:
-- * @connection@: The connection context to create the appender in.
-- * @schema@: The schema of the table to append to, or @nullptr@ for the default
--   schema.
-- * @table@: The table name to append to.
-- * @out_appender@: The resulting appender object.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_appender_create"
  c_duckdb_appender_create :: DuckDBConnection -> CString -> CString -> Ptr DuckDBAppender -> IO DuckDBState

-- | Creates an appender object.
--
-- Note that the object must be destroyed with @duckdb_appender_destroy@.
--
-- Parameters:
-- * @connection@: The connection context to create the appender in.
-- * @catalog@: The catalog of the table to append to, or @nullptr@ for the
--   default catalog.
-- * @schema@: The schema of the table to append to, or @nullptr@ for the default
--   schema.
-- * @table@: The table name to append to.
-- * @out_appender@: The resulting appender object.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_appender_create_ext"
  c_duckdb_appender_create_ext :: DuckDBConnection -> CString -> CString -> CString -> Ptr DuckDBAppender -> IO DuckDBState

-- | Creates an appender object that executes the given query with any data
-- appended to it.
--
-- Note that the object must be destroyed with @duckdb_appender_destroy@.
--
-- Parameters:
-- * @connection@: The connection context to create the appender in.
-- * @query@: The query to execute, can be an INSERT, DELETE, UPDATE or MERGE
--   INTO statement.
-- * @column_count@: The number of columns to append.
-- * @types@: The types of the columns to append.
-- * @table_name@: (optionally) the table name used to refer to the appended
--   data, defaults to "appended_data".
-- * @column_names@: (optionally) the list of column names, defaults to "col1",
--   "col2", ...
-- * @out_appender@: The resulting appender object.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_appender_create_query"
  c_duckdb_appender_create_query :: DuckDBConnection -> CString -> DuckDBIdx -> Ptr DuckDBLogicalType -> CString -> Ptr CString -> Ptr DuckDBAppender -> IO DuckDBState

-- | Returns the number of columns that belong to the appender. If there is no
-- active column list, then this equals the table's physical columns.
--
-- Parameters:
-- * @appender@: The appender to get the column count from.
--
-- Returns The number of columns in the data chunks.
foreign import ccall unsafe "duckdb_appender_column_count"
  c_duckdb_appender_column_count :: DuckDBAppender -> IO DuckDBIdx

-- | Returns the type of the column at the specified index. This is either a type
-- in the active column list, or the same type as a column in the receiving
-- table.
--
-- Note: The resulting type must be destroyed with @duckdb_destroy_logical_type@.
--
-- Parameters:
-- * @appender@: The appender to get the column type from.
-- * @col_idx@: The index of the column to get the type of.
--
-- Returns The @duckdb_logical_type@ of the column.
foreign import ccall unsafe "duckdb_appender_column_type"
  c_duckdb_appender_column_type :: DuckDBAppender -> DuckDBIdx -> IO DuckDBLogicalType

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release. Use duckdb_appender_error_data instead.
--
-- Returns the error message associated with the appender. If the appender has no
-- error message, this returns @nullptr@ instead.
--
-- The error message should not be freed. It will be de-allocated when
-- @duckdb_appender_destroy@ is called.
--
-- Parameters:
-- * @appender@: The appender to get the error from.
--
-- Returns The error message, or @nullptr@ if there is none.
foreign import ccall unsafe "duckdb_appender_error"
  c_duckdb_appender_error :: DuckDBAppender -> IO CString

-- | Returns the error data associated with the appender. Must be destroyed with
-- duckdb_destroy_error_data.
--
-- Parameters:
-- * @appender@: The appender to get the error data from.
--
-- Returns The error data.
foreign import ccall unsafe "duckdb_appender_error_data"
  c_duckdb_appender_error_data :: DuckDBAppender -> IO DuckDBErrorData

-- | Flush the appender to the table, forcing the cache of the appender to be
-- cleared. If flushing the data triggers a constraint violation or any other
-- error, then all data is invalidated, and this function returns DuckDBError. It
-- is not possible to append more values. Call duckdb_appender_error_data to
-- obtain the error data followed by duckdb_appender_destroy to destroy the
-- invalidated appender.
--
-- Parameters:
-- * @appender@: The appender to flush.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_appender_flush"
  c_duckdb_appender_flush :: DuckDBAppender -> IO DuckDBState

-- | Closes the appender by flushing all intermediate states and closing it for
-- further appends. If flushing the data triggers a constraint violation or any
-- other error, then all data is invalidated, and this function returns
-- DuckDBError. Call duckdb_appender_error_data to obtain the error data followed
-- by duckdb_appender_destroy to destroy the invalidated appender.
--
-- Parameters:
-- * @appender@: The appender to flush and close.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_appender_close"
  c_duckdb_appender_close :: DuckDBAppender -> IO DuckDBState

-- | Closes the appender by flushing all intermediate states to the table and
-- destroying it. By destroying it, this function de-allocates all memory
-- associated with the appender. If flushing the data triggers a constraint
-- violation, then all data is invalidated, and this function returns
-- DuckDBError. Due to the destruction of the appender, it is no longer possible
-- to obtain the specific error message with duckdb_appender_error. Therefore,
-- call duckdb_appender_close before destroying the appender, if you need
-- insights into the specific error.
--
-- Parameters:
-- * @appender@: The appender to flush, close and destroy.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_appender_destroy"
  c_duckdb_appender_destroy :: Ptr DuckDBAppender -> IO DuckDBState

-- | Appends a column to the active column list of the appender. Immediately
-- flushes all previous data.
--
-- The active column list specifies all columns that are expected when flushing
-- the data. Any non-active columns are filled with their default values, or
-- NULL.
--
-- Parameters:
-- * @appender@: The appender to add the column to.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_appender_add_column"
  c_duckdb_appender_add_column :: DuckDBAppender -> CString -> IO DuckDBState

-- | Removes all columns from the active column list of the appender, resetting the
-- appender to treat all columns as active. Immediately flushes all previous
-- data.
--
-- Parameters:
-- * @appender@: The appender to clear the columns from.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_appender_clear_columns"
  c_duckdb_appender_clear_columns :: DuckDBAppender -> IO DuckDBState

-- | A nop function, provided for backwards compatibility reasons. Does nothing.
-- Only @duckdb_appender_end_row@ is required.
foreign import ccall unsafe "duckdb_appender_begin_row"
  c_duckdb_appender_begin_row :: DuckDBAppender -> IO DuckDBState

-- | Finish the current row of appends. After end_row is called, the next row can
-- be appended.
--
-- Parameters:
-- * @appender@: The appender.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_appender_end_row"
  c_duckdb_appender_end_row :: DuckDBAppender -> IO DuckDBState

-- | Append a DEFAULT value (NULL if DEFAULT not available for column) to the
-- appender.
foreign import ccall unsafe "duckdb_append_default"
  c_duckdb_append_default :: DuckDBAppender -> IO DuckDBState

-- | Append a DEFAULT value, at the specified row and column, (NULL if DEFAULT not
-- available for column) to the chunk created from the specified appender. The
-- default value of the column must be a constant value. Non-deterministic
-- expressions like nextval(@seq@) or random() are not supported.
--
-- Parameters:
-- * @appender@: The appender to get the default value from.
-- * @chunk@: The data chunk to append the default value to.
-- * @col@: The chunk column index to append the default value to.
-- * @row@: The chunk row index to append the default value to.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_append_default_to_chunk"
  c_duckdb_append_default_to_chunk :: DuckDBAppender -> DuckDBDataChunk -> DuckDBIdx -> DuckDBIdx -> IO DuckDBState

-- | Append a bool value to the appender.
foreign import ccall unsafe "duckdb_append_bool"
  c_duckdb_append_bool :: DuckDBAppender -> CBool -> IO DuckDBState

-- | Append an int8_t value to the appender.
foreign import ccall unsafe "duckdb_append_int8"
  c_duckdb_append_int8 :: DuckDBAppender -> Int8 -> IO DuckDBState

-- | Append an int16_t value to the appender.
foreign import ccall unsafe "duckdb_append_int16"
  c_duckdb_append_int16 :: DuckDBAppender -> Int16 -> IO DuckDBState

-- | Append an int32_t value to the appender.
foreign import ccall unsafe "duckdb_append_int32"
  c_duckdb_append_int32 :: DuckDBAppender -> Int32 -> IO DuckDBState

-- | Append an int64_t value to the appender.
foreign import ccall unsafe "duckdb_append_int64"
  c_duckdb_append_int64 :: DuckDBAppender -> Int64 -> IO DuckDBState

-- | Append a duckdb_hugeint value to the appender.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_append_hugeint@
-- but mirror the DuckDB C API semantics of @duckdb_append_hugeint@.
foreign import ccall unsafe "wrapped_duckdb_append_hugeint"
  c_duckdb_append_hugeint :: DuckDBAppender -> Ptr DuckDBHugeInt -> IO DuckDBState

-- | Append a uint8_t value to the appender.
foreign import ccall unsafe "duckdb_append_uint8"
  c_duckdb_append_uint8 :: DuckDBAppender -> Word8 -> IO DuckDBState

-- | Append a uint16_t value to the appender.
foreign import ccall unsafe "duckdb_append_uint16"
  c_duckdb_append_uint16 :: DuckDBAppender -> Word16 -> IO DuckDBState

-- | Append a uint32_t value to the appender.
foreign import ccall unsafe "duckdb_append_uint32"
  c_duckdb_append_uint32 :: DuckDBAppender -> Word32 -> IO DuckDBState

-- | Append a uint64_t value to the appender.
foreign import ccall unsafe "duckdb_append_uint64"
  c_duckdb_append_uint64 :: DuckDBAppender -> Word64 -> IO DuckDBState

-- | Append a duckdb_uhugeint value to the appender.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_append_uhugeint@
-- but mirror the DuckDB C API semantics of @duckdb_append_uhugeint@.
foreign import ccall unsafe "wrapped_duckdb_append_uhugeint"
  c_duckdb_append_uhugeint :: DuckDBAppender -> Ptr DuckDBUHugeInt -> IO DuckDBState

-- | Append a float value to the appender.
foreign import ccall unsafe "duckdb_append_float"
  c_duckdb_append_float :: DuckDBAppender -> CFloat -> IO DuckDBState

-- | Append a double value to the appender.
foreign import ccall unsafe "duckdb_append_double"
  c_duckdb_append_double :: DuckDBAppender -> CDouble -> IO DuckDBState

-- | Append a duckdb_date value to the appender.
foreign import ccall unsafe "duckdb_append_date"
  c_duckdb_append_date :: DuckDBAppender -> DuckDBDate -> IO DuckDBState

-- | Append a duckdb_time value to the appender.
foreign import ccall unsafe "duckdb_append_time"
  c_duckdb_append_time :: DuckDBAppender -> DuckDBTime -> IO DuckDBState

-- | Append a duckdb_timestamp value to the appender.
foreign import ccall unsafe "duckdb_append_timestamp"
  c_duckdb_append_timestamp :: DuckDBAppender -> DuckDBTimestamp -> IO DuckDBState

-- | Append a duckdb_interval value to the appender.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_append_interval@
-- but mirror the DuckDB C API semantics of @duckdb_append_interval@.
foreign import ccall unsafe "wrapped_duckdb_append_interval"
  c_duckdb_append_interval :: DuckDBAppender -> Ptr DuckDBInterval -> IO DuckDBState

-- | Append a varchar value to the appender.
foreign import ccall unsafe "duckdb_append_varchar"
  c_duckdb_append_varchar :: DuckDBAppender -> CString -> IO DuckDBState

-- | Append a varchar value to the appender.
foreign import ccall unsafe "duckdb_append_varchar_length"
  c_duckdb_append_varchar_length :: DuckDBAppender -> CString -> DuckDBIdx -> IO DuckDBState

-- | Append a blob value to the appender.
foreign import ccall unsafe "duckdb_append_blob"
  c_duckdb_append_blob :: DuckDBAppender -> Ptr () -> DuckDBIdx -> IO DuckDBState

-- | Append a NULL value to the appender (of any type).
foreign import ccall unsafe "duckdb_append_null"
  c_duckdb_append_null :: DuckDBAppender -> IO DuckDBState

-- | Append a duckdb_value to the appender.
foreign import ccall unsafe "duckdb_append_value"
  c_duckdb_append_value :: DuckDBAppender -> DuckDBValue -> IO DuckDBState

-- | Appends a pre-filled data chunk to the specified appender. Attempts casting,
-- if the data chunk types do not match the active appender types.
--
-- Parameters:
-- * @appender@: The appender to append to.
-- * @chunk@: The data chunk to append.
--
-- Returns @DuckDBSuccess@ on success or @DuckDBError@ on failure.
foreign import ccall unsafe "duckdb_append_data_chunk"
  c_duckdb_append_data_chunk :: DuckDBAppender -> DuckDBDataChunk -> IO DuckDBState
