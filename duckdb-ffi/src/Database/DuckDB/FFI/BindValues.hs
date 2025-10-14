module Database.DuckDB.FFI.BindValues (
    c_duckdb_bind_value,
    c_duckdb_bind_parameter_index,
    c_duckdb_bind_boolean,
    c_duckdb_bind_int8,
    c_duckdb_bind_int16,
    c_duckdb_bind_int32,
    c_duckdb_bind_int64,
    c_duckdb_bind_hugeint,
    c_duckdb_bind_uhugeint,
    c_duckdb_bind_decimal,
    c_duckdb_bind_uint8,
    c_duckdb_bind_uint16,
    c_duckdb_bind_uint32,
    c_duckdb_bind_uint64,
    c_duckdb_bind_float,
    c_duckdb_bind_double,
    c_duckdb_bind_date,
    c_duckdb_bind_time,
    c_duckdb_bind_timestamp,
    c_duckdb_bind_timestamp_tz,
    c_duckdb_bind_interval,
    c_duckdb_bind_varchar,
    c_duckdb_bind_varchar_length,
    c_duckdb_bind_blob,
    c_duckdb_bind_null,
) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CDouble (..), CFloat (..), CInt (..))
import Foreign.Ptr (Ptr)

-- | Binds a value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_value"
    c_duckdb_bind_value :: DuckDBPreparedStatement -> DuckDBIdx -> DuckDBValue -> IO DuckDBState

{- | Retrieve the index of the parameter for the prepared statement, identified by
name
-}
foreign import ccall safe "duckdb_bind_parameter_index"
    c_duckdb_bind_parameter_index :: DuckDBPreparedStatement -> Ptr DuckDBIdx -> CString -> IO DuckDBState

-- | Binds a bool value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_boolean"
    c_duckdb_bind_boolean :: DuckDBPreparedStatement -> DuckDBIdx -> CBool -> IO DuckDBState

-- | Binds an int8_t value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_int8"
    c_duckdb_bind_int8 :: DuckDBPreparedStatement -> DuckDBIdx -> Int8 -> IO DuckDBState

-- | Binds an int16_t value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_int16"
    c_duckdb_bind_int16 :: DuckDBPreparedStatement -> DuckDBIdx -> Int16 -> IO DuckDBState

-- | Binds an int32_t value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_int32"
    c_duckdb_bind_int32 :: DuckDBPreparedStatement -> DuckDBIdx -> Int32 -> IO DuckDBState

-- | Binds an int64_t value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_int64"
    c_duckdb_bind_int64 :: DuckDBPreparedStatement -> DuckDBIdx -> Int64 -> IO DuckDBState

{- | Binds a duckdb_hugeint value to the prepared statement at the specified index.

These bindings call the wrapper symbol @wrapped_duckdb_bind_hugeint@
but mirror the DuckDB C API semantics of @duckdb_bind_hugeint@.
-}
foreign import ccall safe "wrapped_duckdb_bind_hugeint"
    c_duckdb_bind_hugeint :: DuckDBPreparedStatement -> DuckDBIdx -> Ptr DuckDBHugeInt -> IO DuckDBState

{- | Binds a duckdb_uhugeint value to the prepared statement at the specified
index.

These bindings call the wrapper symbol @wrapped_duckdb_bind_uhugeint@
but mirror the DuckDB C API semantics of @duckdb_bind_uhugeint@.
-}
foreign import ccall safe "wrapped_duckdb_bind_uhugeint"
    c_duckdb_bind_uhugeint :: DuckDBPreparedStatement -> DuckDBIdx -> Ptr DuckDBUHugeInt -> IO DuckDBState

{- | Binds a duckdb_decimal value to the prepared statement at the specified index.

These bindings call the wrapper symbol @wrapped_duckdb_bind_decimal@
but mirror the DuckDB C API semantics of @duckdb_bind_decimal@.
-}
foreign import ccall safe "wrapped_duckdb_bind_decimal"
    c_duckdb_bind_decimal :: DuckDBPreparedStatement -> DuckDBIdx -> Ptr DuckDBDecimal -> IO DuckDBState

-- | Binds a uint8_t value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_uint8"
    c_duckdb_bind_uint8 :: DuckDBPreparedStatement -> DuckDBIdx -> Word8 -> IO DuckDBState

-- | Binds a uint16_t value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_uint16"
    c_duckdb_bind_uint16 :: DuckDBPreparedStatement -> DuckDBIdx -> Word16 -> IO DuckDBState

-- | Binds a uint32_t value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_uint32"
    c_duckdb_bind_uint32 :: DuckDBPreparedStatement -> DuckDBIdx -> Word32 -> IO DuckDBState

-- | Binds a uint64_t value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_uint64"
    c_duckdb_bind_uint64 :: DuckDBPreparedStatement -> DuckDBIdx -> Word64 -> IO DuckDBState

-- | Binds a float value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_float"
    c_duckdb_bind_float :: DuckDBPreparedStatement -> DuckDBIdx -> CFloat -> IO DuckDBState

-- | Binds a double value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_double"
    c_duckdb_bind_double :: DuckDBPreparedStatement -> DuckDBIdx -> CDouble -> IO DuckDBState

-- | Binds a duckdb_date value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_date"
    c_duckdb_bind_date :: DuckDBPreparedStatement -> DuckDBIdx -> DuckDBDate -> IO DuckDBState

-- | Binds a duckdb_time value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_time"
    c_duckdb_bind_time :: DuckDBPreparedStatement -> DuckDBIdx -> DuckDBTime -> IO DuckDBState

{- | Binds a duckdb_timestamp value to the prepared statement at the specified
index.
-}
foreign import ccall safe "duckdb_bind_timestamp"
    c_duckdb_bind_timestamp :: DuckDBPreparedStatement -> DuckDBIdx -> DuckDBTimestamp -> IO DuckDBState

{- | Binds a duckdb_timestamp value to the prepared statement at the specified
index.
-}
foreign import ccall safe "duckdb_bind_timestamp_tz"
    c_duckdb_bind_timestamp_tz :: DuckDBPreparedStatement -> DuckDBIdx -> DuckDBTimestamp -> IO DuckDBState

{- | Binds a duckdb_interval value to the prepared statement at the specified
index.

These bindings call the wrapper symbol @wrapped_duckdb_bind_interval@
but mirror the DuckDB C API semantics of @duckdb_bind_interval@.
-}
foreign import ccall safe "wrapped_duckdb_bind_interval"
    c_duckdb_bind_interval :: DuckDBPreparedStatement -> DuckDBIdx -> Ptr DuckDBInterval -> IO DuckDBState

{- | Binds a null-terminated varchar value to the prepared statement at the
specified index.
-}
foreign import ccall safe "duckdb_bind_varchar"
    c_duckdb_bind_varchar :: DuckDBPreparedStatement -> DuckDBIdx -> CString -> IO DuckDBState

-- | Binds a varchar value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_varchar_length"
    c_duckdb_bind_varchar_length :: DuckDBPreparedStatement -> DuckDBIdx -> CString -> DuckDBIdx -> IO DuckDBState

-- | Binds a blob value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_blob"
    c_duckdb_bind_blob :: DuckDBPreparedStatement -> DuckDBIdx -> Ptr () -> DuckDBIdx -> IO DuckDBState

-- | Binds a NULL value to the prepared statement at the specified index.
foreign import ccall safe "duckdb_bind_null"
    c_duckdb_bind_null :: DuckDBPreparedStatement -> DuckDBIdx -> IO DuckDBState
