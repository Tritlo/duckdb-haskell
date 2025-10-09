module Database.DuckDB.FFI.Helpers (
  c_duckdb_malloc,
  c_duckdb_free,
  c_duckdb_vector_size,
  c_duckdb_string_is_inlined,
  c_duckdb_string_t_length,
  c_duckdb_string_t_data,
  c_duckdb_from_date,
  c_duckdb_to_date,
  c_duckdb_is_finite_date,
  c_duckdb_from_time,
  c_duckdb_create_time_tz,
  c_duckdb_from_time_tz,
  c_duckdb_to_time,
  c_duckdb_from_timestamp,
  c_duckdb_to_timestamp,
  c_duckdb_is_finite_timestamp,
  c_duckdb_is_finite_timestamp_s,
  c_duckdb_is_finite_timestamp_ms,
  c_duckdb_is_finite_timestamp_ns,
  c_duckdb_hugeint_to_double,
  c_duckdb_double_to_hugeint,
  c_duckdb_uhugeint_to_double,
  c_duckdb_double_to_uhugeint,
  c_duckdb_double_to_decimal,
  c_duckdb_decimal_to_double
) where

import Data.Int (Int32, Int64)
import Data.Word (Word32, Word8)
import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CDouble (..), CSize (..))
import Foreign.Ptr (Ptr)

-- | Allocate @size@ bytes of memory using the duckdb internal malloc function. Any
-- memory allocated in this manner should be freed using @duckdb_free@.
--
-- Parameters:
-- * @size@: The number of bytes to allocate.
--
-- Returns A pointer to the allocated memory region.
foreign import ccall unsafe "duckdb_malloc"
  c_duckdb_malloc :: CSize -> IO (Ptr ())

-- | Free a value returned from @duckdb_malloc@, @duckdb_value_varchar@,
-- @duckdb_value_blob@, or @duckdb_value_string@.
--
-- Parameters:
-- * @ptr@: The memory region to de-allocate.
foreign import ccall unsafe "duckdb_free"
  c_duckdb_free :: Ptr () -> IO ()

-- | The internal vector size used by DuckDB. This is the amount of tuples that
-- will fit into a data chunk created by @duckdb_create_data_chunk@.
--
-- Returns The vector size.
foreign import ccall unsafe "duckdb_vector_size"
  c_duckdb_vector_size :: IO DuckDBIdx

-- | Whether or not the duckdb_string_t value is inlined. This means that the data
-- of the string does not have a separate allocation.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_string_is_inlined@ but mirror the DuckDB C API semantics of
-- @duckdb_string_is_inlined@.
foreign import ccall unsafe "wrapped_duckdb_string_is_inlined"
  c_duckdb_string_is_inlined :: Ptr DuckDBStringT -> IO CBool

-- | Get the string length of a string_t
--
-- Parameters:
-- * @string@: The string to get the length of.
--
-- Returns The length.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_string_t_length@
-- but mirror the DuckDB C API semantics of @duckdb_string_t_length@.
foreign import ccall unsafe "wrapped_duckdb_string_t_length"
  c_duckdb_string_t_length :: Ptr DuckDBStringT -> IO Word32

-- | Get a pointer to the string data of a string_t
--
-- Parameters:
-- * @string@: The string to get the pointer to.
--
-- Returns The pointer.
foreign import ccall unsafe "duckdb_string_t_data"
  c_duckdb_string_t_data :: Ptr DuckDBStringT -> IO CString

-- | Decompose a @duckdb_date@ object into year, month and date (stored as
-- @duckdb_date_struct@).
--
-- Parameters:
-- * @date@: The date object, as obtained from a @DUCKDB_TYPE_DATE@ column.
--
-- Returns The @duckdb_date_struct@ with the decomposed elements.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_from_date@ but
-- mirror the DuckDB C API semantics of @duckdb_from_date@.
foreign import ccall unsafe "wrapped_duckdb_from_date"
  c_duckdb_from_date :: DuckDBDate -> Ptr DuckDBDateStruct -> IO ()

-- | Re-compose a @duckdb_date@ from year, month and date (@duckdb_date_struct@).
--
-- Parameters:
-- * @date@: The year, month and date stored in a @duckdb_date_struct@.
--
-- Returns The @duckdb_date@ element.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_to_date@ but
-- mirror the DuckDB C API semantics of @duckdb_to_date@.
foreign import ccall unsafe "wrapped_duckdb_to_date"
  c_duckdb_to_date :: Ptr DuckDBDateStruct -> IO DuckDBDate

-- | Test a @duckdb_date@ to see if it is a finite value.
--
-- Parameters:
-- * @date@: The date object, as obtained from a @DUCKDB_TYPE_DATE@ column.
--
-- Returns True if the date is finite, false if it is ±infinity.
foreign import ccall unsafe "duckdb_is_finite_date"
  c_duckdb_is_finite_date :: DuckDBDate -> IO CBool

-- | Decompose a @duckdb_time@ object into hour, minute, second and microsecond
-- (stored as @duckdb_time_struct@).
--
-- Parameters:
-- * @time@: The time object, as obtained from a @DUCKDB_TYPE_TIME@ column.
--
-- Returns The @duckdb_time_struct@ with the decomposed elements.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_from_time@ but
-- mirror the DuckDB C API semantics of @duckdb_from_time@.
foreign import ccall unsafe "wrapped_duckdb_from_time"
  c_duckdb_from_time :: DuckDBTime -> Ptr DuckDBTimeStruct -> IO ()

-- | Create a @duckdb_time_tz@ object from micros and a timezone offset.
--
-- Parameters:
-- * @micros@: The microsecond component of the time.
-- * @offset@: The timezone offset component of the time.
--
-- Returns The @duckdb_time_tz@ element.
foreign import ccall unsafe "duckdb_create_time_tz"
  c_duckdb_create_time_tz :: Int64 -> Int32 -> IO DuckDBTimeTz

-- | Decompose a TIME_TZ objects into micros and a timezone offset.
--
-- Use @duckdb_from_time@ to further decompose the micros into hour, minute,
-- second and microsecond.
--
-- Parameters:
-- * @micros@: The time object, as obtained from a @DUCKDB_TYPE_TIME_TZ@ column.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_from_time_tz@
-- but mirror the DuckDB C API semantics of @duckdb_from_time_tz@.
foreign import ccall unsafe "wrapped_duckdb_from_time_tz"
  c_duckdb_from_time_tz :: DuckDBTimeTz -> Ptr DuckDBTimeTzStruct -> IO ()

-- | Re-compose a @duckdb_time@ from hour, minute, second and microsecond
-- (@duckdb_time_struct@).
--
-- Parameters:
-- * @time@: The hour, minute, second and microsecond in a @duckdb_time_struct@.
--
-- Returns The @duckdb_time@ element.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_to_time@ but
-- mirror the DuckDB C API semantics of @duckdb_to_time@.
foreign import ccall unsafe "wrapped_duckdb_to_time"
  c_duckdb_to_time :: Ptr DuckDBTimeStruct -> IO DuckDBTime

-- | Decompose a @duckdb_timestamp@ object into a @duckdb_timestamp_struct@.
--
-- Parameters:
-- * @ts@: The ts object, as obtained from a @DUCKDB_TYPE_TIMESTAMP@ column.
--
-- Returns The @duckdb_timestamp_struct@ with the decomposed elements.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_from_timestamp@
-- but mirror the DuckDB C API semantics of @duckdb_from_timestamp@.
foreign import ccall unsafe "wrapped_duckdb_from_timestamp"
  c_duckdb_from_timestamp :: DuckDBTimestamp -> Ptr DuckDBTimestampStruct -> IO ()

-- | Re-compose a @duckdb_timestamp@ from a duckdb_timestamp_struct.
--
-- Parameters:
-- * @ts@: The de-composed elements in a @duckdb_timestamp_struct@.
--
-- Returns The @duckdb_timestamp@ element.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_to_timestamp@
-- but mirror the DuckDB C API semantics of @duckdb_to_timestamp@.
foreign import ccall unsafe "wrapped_duckdb_to_timestamp"
  c_duckdb_to_timestamp :: Ptr DuckDBTimestampStruct -> IO DuckDBTimestamp

-- | Test a @duckdb_timestamp@ to see if it is a finite value.
--
-- Parameters:
-- * @ts@: The duckdb_timestamp object, as obtained from a
--   @DUCKDB_TYPE_TIMESTAMP@ column.
--
-- Returns True if the timestamp is finite, false if it is ±infinity.
foreign import ccall unsafe "duckdb_is_finite_timestamp"
  c_duckdb_is_finite_timestamp :: DuckDBTimestamp -> IO CBool

-- | Test a @duckdb_timestamp_s@ to see if it is a finite value.
--
-- Parameters:
-- * @ts@: The duckdb_timestamp_s object, as obtained from a
--   @DUCKDB_TYPE_TIMESTAMP_S@ column.
--
-- Returns True if the timestamp is finite, false if it is ±infinity.
foreign import ccall unsafe "duckdb_is_finite_timestamp_s"
  c_duckdb_is_finite_timestamp_s :: DuckDBTimestampS -> IO CBool

-- | Test a @duckdb_timestamp_ms@ to see if it is a finite value.
--
-- Parameters:
-- * @ts@: The duckdb_timestamp_ms object, as obtained from a
--   @DUCKDB_TYPE_TIMESTAMP_MS@ column.
--
-- Returns True if the timestamp is finite, false if it is ±infinity.
foreign import ccall unsafe "duckdb_is_finite_timestamp_ms"
  c_duckdb_is_finite_timestamp_ms :: DuckDBTimestampMs -> IO CBool

-- | Test a @duckdb_timestamp_ns@ to see if it is a finite value.
--
-- Parameters:
-- * @ts@: The duckdb_timestamp_ns object, as obtained from a
--   @DUCKDB_TYPE_TIMESTAMP_NS@ column.
--
-- Returns True if the timestamp is finite, false if it is ±infinity.
foreign import ccall unsafe "duckdb_is_finite_timestamp_ns"
  c_duckdb_is_finite_timestamp_ns :: DuckDBTimestampNs -> IO CBool

-- | Converts a duckdb_hugeint object (as obtained from a @DUCKDB_TYPE_HUGEINT@
-- column) into a double.
--
-- Parameters:
-- * @val@: The hugeint value.
--
-- Returns The converted @double@ element.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_hugeint_to_double@ but mirror the DuckDB C API semantics of
-- @duckdb_hugeint_to_double@.
foreign import ccall unsafe "wrapped_duckdb_hugeint_to_double"
  c_duckdb_hugeint_to_double :: Ptr DuckDBHugeInt -> IO CDouble

-- | Converts a double value to a duckdb_hugeint object.
--
-- If the conversion fails because the double value is too big the result will be
-- 0.
--
-- Parameters:
-- * @val@: The double value.
--
-- Returns The converted @duckdb_hugeint@ element.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_double_to_hugeint@ but mirror the DuckDB C API semantics of
-- @duckdb_double_to_hugeint@.
foreign import ccall unsafe "wrapped_duckdb_double_to_hugeint"
  c_duckdb_double_to_hugeint :: CDouble -> Ptr DuckDBHugeInt -> IO ()

-- | Converts a duckdb_uhugeint object (as obtained from a @DUCKDB_TYPE_UHUGEINT@
-- column) into a double.
--
-- Parameters:
-- * @val@: The uhugeint value.
--
-- Returns The converted @double@ element.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_uhugeint_to_double@ but mirror the DuckDB C API semantics of
-- @duckdb_uhugeint_to_double@.
foreign import ccall unsafe "wrapped_duckdb_uhugeint_to_double"
  c_duckdb_uhugeint_to_double :: Ptr DuckDBUHugeInt -> IO CDouble

-- | Converts a double value to a duckdb_uhugeint object.
--
-- If the conversion fails because the double value is too big the result will be
-- 0.
--
-- Parameters:
-- * @val@: The double value.
--
-- Returns The converted @duckdb_uhugeint@ element.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_double_to_uhugeint@ but mirror the DuckDB C API semantics of
-- @duckdb_double_to_uhugeint@.
foreign import ccall unsafe "wrapped_duckdb_double_to_uhugeint"
  c_duckdb_double_to_uhugeint :: CDouble -> Ptr DuckDBUHugeInt -> IO ()

-- | Converts a double value to a duckdb_decimal object.
--
-- If the conversion fails because the double value is too big, or the
-- width/scale are invalid the result will be 0.
--
-- Parameters:
-- * @val@: The double value.
--
-- Returns The converted @duckdb_decimal@ element.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_double_to_decimal@ but mirror the DuckDB C API semantics of
-- @duckdb_double_to_decimal@.
foreign import ccall unsafe "wrapped_duckdb_double_to_decimal"
  c_duckdb_double_to_decimal :: CDouble -> Word8 -> Word8 -> Ptr DuckDBDecimal -> IO ()

-- | Converts a duckdb_decimal object (as obtained from a @DUCKDB_TYPE_DECIMAL@
-- column) into a double.
--
-- Parameters:
-- * @val@: The decimal value.
--
-- Returns The converted @double@ element.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_decimal_to_double@ but mirror the DuckDB C API semantics of
-- @duckdb_decimal_to_double@.
foreign import ccall unsafe "wrapped_duckdb_decimal_to_double"
  c_duckdb_decimal_to_double :: Ptr DuckDBDecimal -> IO CDouble
