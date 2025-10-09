module Database.DuckDB.FFI.SafeFetch (
  c_duckdb_value_boolean,
  c_duckdb_value_int8,
  c_duckdb_value_int16,
  c_duckdb_value_int32,
  c_duckdb_value_int64,
  c_duckdb_value_hugeint,
  c_duckdb_value_uhugeint,
  c_duckdb_value_decimal,
  c_duckdb_value_uint8,
  c_duckdb_value_uint16,
  c_duckdb_value_uint32,
  c_duckdb_value_uint64,
  c_duckdb_value_float,
  c_duckdb_value_double,
  c_duckdb_value_date,
  c_duckdb_value_time,
  c_duckdb_value_timestamp,
  c_duckdb_value_interval,
  c_duckdb_value_varchar,
  c_duckdb_value_string,
  c_duckdb_value_varchar_internal,
  c_duckdb_value_string_internal,
  c_duckdb_value_blob,
  c_duckdb_value_is_null
) where

import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CDouble (..), CFloat (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr)

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The boolean value at the specified location, or false if the value
-- cannot be converted.
foreign import ccall safe "duckdb_value_boolean"
  c_duckdb_value_boolean :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO CBool

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The int8_t value at the specified location, or 0 if the value cannot
-- be converted.
foreign import ccall unsafe "duckdb_value_int8"
  c_duckdb_value_int8 :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Int8

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The int16_t value at the specified location, or 0 if the value cannot
-- be converted.
foreign import ccall unsafe "duckdb_value_int16"
  c_duckdb_value_int16 :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Int16

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The int32_t value at the specified location, or 0 if the value cannot
-- be converted.
foreign import ccall safe "duckdb_value_int32"
  c_duckdb_value_int32 :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Int32

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The int64_t value at the specified location, or 0 if the value cannot
-- be converted.
foreign import ccall safe "duckdb_value_int64"
  c_duckdb_value_int64 :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Int64

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The duckdb_hugeint value at the specified location, or 0 if the value
-- cannot be converted.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_value_hugeint@
-- but mirror the DuckDB C API semantics of @duckdb_value_hugeint@.
foreign import ccall unsafe "wrapped_duckdb_value_hugeint"
  c_duckdb_value_hugeint :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> Ptr DuckDBHugeInt -> IO ()

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The duckdb_uhugeint value at the specified location, or 0 if the value
-- cannot be converted.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_value_uhugeint@
-- but mirror the DuckDB C API semantics of @duckdb_value_uhugeint@.
foreign import ccall unsafe "wrapped_duckdb_value_uhugeint"
  c_duckdb_value_uhugeint :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> Ptr DuckDBUHugeInt -> IO ()

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The duckdb_decimal value at the specified location, or 0 if the value
-- cannot be converted.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_value_decimal@
-- but mirror the DuckDB C API semantics of @duckdb_value_decimal@.
foreign import ccall unsafe "wrapped_duckdb_value_decimal"
  c_duckdb_value_decimal :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> Ptr DuckDBDecimal -> IO ()

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The uint8_t value at the specified location, or 0 if the value cannot
-- be converted.
foreign import ccall unsafe "duckdb_value_uint8"
  c_duckdb_value_uint8 :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Word8

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The uint16_t value at the specified location, or 0 if the value cannot
-- be converted.
foreign import ccall unsafe "duckdb_value_uint16"
  c_duckdb_value_uint16 :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Word16

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The uint32_t value at the specified location, or 0 if the value cannot
-- be converted.
foreign import ccall unsafe "duckdb_value_uint32"
  c_duckdb_value_uint32 :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Word32

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The uint64_t value at the specified location, or 0 if the value cannot
-- be converted.
foreign import ccall unsafe "duckdb_value_uint64"
  c_duckdb_value_uint64 :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Word64

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The float value at the specified location, or 0 if the value cannot be
-- converted.
foreign import ccall unsafe "duckdb_value_float"
  c_duckdb_value_float :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO CFloat

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The double value at the specified location, or 0 if the value cannot
-- be converted.
foreign import ccall unsafe "duckdb_value_double"
  c_duckdb_value_double :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO CDouble

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The duckdb_date value at the specified location, or 0 if the value
-- cannot be converted.
foreign import ccall unsafe "duckdb_value_date"
  c_duckdb_value_date :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO DuckDBDate

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The duckdb_time value at the specified location, or 0 if the value
-- cannot be converted.
foreign import ccall unsafe "duckdb_value_time"
  c_duckdb_value_time :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO DuckDBTime

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The duckdb_timestamp value at the specified location, or 0 if the
-- value cannot be converted.
foreign import ccall unsafe "duckdb_value_timestamp"
  c_duckdb_value_timestamp :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO DuckDBTimestamp

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The duckdb_interval value at the specified location, or 0 if the value
-- cannot be converted.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_value_interval@
-- but mirror the DuckDB C API semantics of @duckdb_value_interval@.
foreign import ccall unsafe "wrapped_duckdb_value_interval"
  c_duckdb_value_interval :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> Ptr DuckDBInterval -> IO ()

-- | > Deprecated This method has been deprecated. Use duckdb_value_string instead.
-- This function does not work correctly if the string contains null bytes.
--
-- Returns The text value at the specified location as a null-terminated string,
-- or nullptr if the value cannot be converted. The result must be freed with
-- @duckdb_free@.
foreign import ccall safe "duckdb_value_varchar"
  c_duckdb_value_varchar :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO CString

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- No support for nested types, and for other complex types. The resulting field
-- "string.data" must be freed with @duckdb_free.@
--
-- Returns The string value at the specified location. Attempts to cast the
-- result value to string.
--
-- These bindings call the wrapper symbol @wrapped_duckdb_value_string@
-- but mirror the DuckDB C API semantics of @duckdb_value_string@.
foreign import ccall unsafe "wrapped_duckdb_value_string"
  c_duckdb_value_string :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> Ptr DuckDBString -> IO ()

-- | > Deprecated This method has been deprecated. Use duckdb_value_string_internal
-- instead. This function does not work correctly if the string contains null
-- bytes.
--
-- Returns The char* value at the specified location. ONLY works on VARCHAR
-- columns and does not auto-cast. If the column is NOT a VARCHAR column this
-- function will return NULL. The result must NOT be freed.
foreign import ccall unsafe "duckdb_value_varchar_internal"
  c_duckdb_value_varchar_internal :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO CString

-- | > Deprecated This method has been deprecated. Use duckdb_value_string_internal
-- instead. This function does not work correctly if the string contains null
-- bytes.
--
-- Returns The char* value at the specified location. ONLY works on VARCHAR
-- columns and does not auto-cast. If the column is NOT a VARCHAR column this
-- function will return NULL. The result must NOT be freed.
--
-- These bindings call the wrapper symbol
-- @wrapped_duckdb_value_string_internal@ but mirror the DuckDB C API semantics of
-- @duckdb_value_string_internal@.
foreign import ccall unsafe "wrapped_duckdb_value_string_internal"
  c_duckdb_value_string_internal :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> Ptr DuckDBString -> IO ()

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns The duckdb_blob value at the specified location. Returns a blob with
-- blob.data set to nullptr if the value cannot be converted. The resulting field
-- "blob.data" must be freed with @duckdb_free.@
--
-- These bindings call the wrapper symbol @wrapped_duckdb_value_blob@ but
-- mirror the DuckDB C API semantics of @duckdb_value_blob@.
foreign import ccall unsafe "wrapped_duckdb_value_blob"
  c_duckdb_value_blob :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> Ptr DuckDBBlob -> IO ()

-- | > Warning Deprecation notice. This method is scheduled for removal in a future
-- release.
--
-- Returns Returns true if the value at the specified index is NULL, and false
-- otherwise.
foreign import ccall safe "duckdb_value_is_null"
  c_duckdb_value_is_null :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO CBool
