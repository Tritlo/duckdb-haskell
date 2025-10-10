module Database.DuckDB.FFI.ValueInterface (
    c_duckdb_destroy_value,
    c_duckdb_create_varchar,
    c_duckdb_create_varchar_length,
    c_duckdb_create_bool,
    c_duckdb_create_int8,
    c_duckdb_create_uint8,
    c_duckdb_create_int16,
    c_duckdb_create_uint16,
    c_duckdb_create_int32,
    c_duckdb_create_uint32,
    c_duckdb_create_uint64,
    c_duckdb_create_int64,
    c_duckdb_create_hugeint,
    c_duckdb_create_uhugeint,
    c_duckdb_create_bignum,
    c_duckdb_create_decimal,
    c_duckdb_create_float,
    c_duckdb_create_double,
    c_duckdb_create_date,
    c_duckdb_create_time,
    c_duckdb_create_time_ns,
    c_duckdb_create_time_tz_value,
    c_duckdb_create_timestamp,
    c_duckdb_create_timestamp_tz,
    c_duckdb_create_timestamp_s,
    c_duckdb_create_timestamp_ms,
    c_duckdb_create_timestamp_ns,
    c_duckdb_create_interval,
    c_duckdb_create_blob,
    c_duckdb_create_bit,
    c_duckdb_create_uuid,
    c_duckdb_get_bool,
    c_duckdb_get_int8,
    c_duckdb_get_uint8,
    c_duckdb_get_int16,
    c_duckdb_get_uint16,
    c_duckdb_get_int32,
    c_duckdb_get_uint32,
    c_duckdb_get_int64,
    c_duckdb_get_uint64,
    c_duckdb_get_hugeint,
    c_duckdb_get_uhugeint,
    c_duckdb_get_bignum,
    c_duckdb_get_decimal,
    c_duckdb_get_float,
    c_duckdb_get_double,
    c_duckdb_get_date,
    c_duckdb_get_time,
    c_duckdb_get_time_ns,
    c_duckdb_get_time_tz,
    c_duckdb_get_timestamp,
    c_duckdb_get_timestamp_tz,
    c_duckdb_get_timestamp_s,
    c_duckdb_get_timestamp_ms,
    c_duckdb_get_timestamp_ns,
    c_duckdb_get_interval,
    c_duckdb_get_value_type,
    c_duckdb_get_blob,
    c_duckdb_get_bit,
    c_duckdb_get_uuid,
    c_duckdb_get_varchar,
    c_duckdb_create_struct_value,
    c_duckdb_create_list_value,
    c_duckdb_create_array_value,
    c_duckdb_create_map_value,
    c_duckdb_create_union_value,
    c_duckdb_get_map_size,
    c_duckdb_get_map_key,
    c_duckdb_get_map_value,
    c_duckdb_is_null_value,
    c_duckdb_create_null_value,
    c_duckdb_get_list_size,
    c_duckdb_get_list_child,
    c_duckdb_create_enum_value,
    c_duckdb_get_enum_value,
    c_duckdb_get_struct_child,
    c_duckdb_value_to_string,
) where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI.Types
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CDouble (..), CFloat (..))
import Foreign.Ptr (Ptr)

{- | Destroys the value and de-allocates all memory allocated for that type.

Parameters:
* @value@: The value to destroy.
-}
foreign import ccall unsafe "duckdb_destroy_value"
    c_duckdb_destroy_value :: Ptr DuckDBValue -> IO ()

{- | Creates a value from a null-terminated string

Parameters:
* @text@: The null-terminated string

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_varchar"
    c_duckdb_create_varchar :: CString -> IO DuckDBValue

{- | Creates a value from a string

Parameters:
* @text@: The text
* @length@: The length of the text

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_varchar_length"
    c_duckdb_create_varchar_length :: CString -> DuckDBIdx -> IO DuckDBValue

{- | Creates a value from a boolean

Parameters:
* @input@: The boolean value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_bool"
    c_duckdb_create_bool :: CBool -> IO DuckDBValue

{- | Creates a value from an int8_t (a tinyint)

Parameters:
* @input@: The tinyint value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_int8"
    c_duckdb_create_int8 :: Int8 -> IO DuckDBValue

{- | Creates a value from a uint8_t (a utinyint)

Parameters:
* @input@: The utinyint value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_uint8"
    c_duckdb_create_uint8 :: Word8 -> IO DuckDBValue

{- | Creates a value from an int16_t (a smallint)

Parameters:
* @input@: The smallint value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_int16"
    c_duckdb_create_int16 :: Int16 -> IO DuckDBValue

{- | Creates a value from a uint16_t (a usmallint)

Parameters:
* @input@: The usmallint value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_uint16"
    c_duckdb_create_uint16 :: Word16 -> IO DuckDBValue

{- | Creates a value from an int32_t (an integer)

Parameters:
* @input@: The integer value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_int32"
    c_duckdb_create_int32 :: Int32 -> IO DuckDBValue

{- | Creates a value from a uint32_t (a uinteger)

Parameters:
* @input@: The uinteger value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_uint32"
    c_duckdb_create_uint32 :: Word32 -> IO DuckDBValue

{- | Creates a value from a uint64_t (a ubigint)

Parameters:
* @input@: The ubigint value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_uint64"
    c_duckdb_create_uint64 :: Word64 -> IO DuckDBValue

{- | Creates a value from an int64

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_int64"
    c_duckdb_create_int64 :: Int64 -> IO DuckDBValue

{- | Creates a value from a hugeint

Parameters:
* @input@: The hugeint value

Returns The value. This must be destroyed with @duckdb_destroy_value@.

These bindings call the wrapper symbol @wrapped_duckdb_create_hugeint@
but mirror the DuckDB C API semantics of @duckdb_create_hugeint@.
-}
foreign import ccall unsafe "wrapped_duckdb_create_hugeint"
    c_duckdb_create_hugeint :: Ptr DuckDBHugeInt -> IO DuckDBValue

{- | Creates a value from a uhugeint

Parameters:
* @input@: The uhugeint value

Returns The value. This must be destroyed with @duckdb_destroy_value@.

These bindings call the wrapper symbol @wrapped_duckdb_create_uhugeint@
but mirror the DuckDB C API semantics of @duckdb_create_uhugeint@.
-}
foreign import ccall unsafe "wrapped_duckdb_create_uhugeint"
    c_duckdb_create_uhugeint :: Ptr DuckDBUHugeInt -> IO DuckDBValue

{- | Creates a BIGNUM value from a duckdb_bignum

Parameters:
* @input@: The duckdb_bignum value

Returns The value. This must be destroyed with @duckdb_destroy_value@.

These bindings call the wrapper symbol @wrapped_duckdb_create_bignum@
but mirror the DuckDB C API semantics of @duckdb_create_bignum@.
-}
foreign import ccall unsafe "wrapped_duckdb_create_bignum"
    c_duckdb_create_bignum :: Ptr DuckDBBignum -> IO DuckDBValue

{- | Creates a DECIMAL value from a duckdb_decimal

Parameters:
* @input@: The duckdb_decimal value

Returns The value. This must be destroyed with @duckdb_destroy_value@.

These bindings call the wrapper symbol @wrapped_duckdb_create_decimal@
but mirror the DuckDB C API semantics of @duckdb_create_decimal@.
-}
foreign import ccall unsafe "wrapped_duckdb_create_decimal"
    c_duckdb_create_decimal :: Ptr DuckDBDecimal -> IO DuckDBValue

{- | Creates a value from a float

Parameters:
* @input@: The float value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_float"
    c_duckdb_create_float :: CFloat -> IO DuckDBValue

{- | Creates a value from a double

Parameters:
* @input@: The double value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_double"
    c_duckdb_create_double :: CDouble -> IO DuckDBValue

{- | Creates a value from a date

Parameters:
* @input@: The date value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_date"
    c_duckdb_create_date :: DuckDBDate -> IO DuckDBValue

{- | Creates a value from a time

Parameters:
* @input@: The time value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_time"
    c_duckdb_create_time :: DuckDBTime -> IO DuckDBValue

{- | Creates a value from a time_ns

Parameters:
* @input@: The time value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_time_ns"
    c_duckdb_create_time_ns :: DuckDBTimeNs -> IO DuckDBValue

{- | Creates a value from a time_tz. Not to be confused with
@duckdb_create_time_tz@, which creates a duckdb_time_tz_t.

Parameters:
* @value@: The time_tz value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_time_tz_value"
    c_duckdb_create_time_tz_value :: DuckDBTimeTz -> IO DuckDBValue

{- | Creates a TIMESTAMP value from a duckdb_timestamp

Parameters:
* @input@: The duckdb_timestamp value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_timestamp"
    c_duckdb_create_timestamp :: DuckDBTimestamp -> IO DuckDBValue

{- | Creates a TIMESTAMP_TZ value from a duckdb_timestamp

Parameters:
* @input@: The duckdb_timestamp value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_timestamp_tz"
    c_duckdb_create_timestamp_tz :: DuckDBTimestamp -> IO DuckDBValue

{- | Creates a TIMESTAMP_S value from a duckdb_timestamp_s

Parameters:
* @input@: The duckdb_timestamp_s value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_timestamp_s"
    c_duckdb_create_timestamp_s :: DuckDBTimestampS -> IO DuckDBValue

{- | Creates a TIMESTAMP_MS value from a duckdb_timestamp_ms

Parameters:
* @input@: The duckdb_timestamp_ms value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_timestamp_ms"
    c_duckdb_create_timestamp_ms :: DuckDBTimestampMs -> IO DuckDBValue

{- | Creates a TIMESTAMP_NS value from a duckdb_timestamp_ns

Parameters:
* @input@: The duckdb_timestamp_ns value

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_timestamp_ns"
    c_duckdb_create_timestamp_ns :: DuckDBTimestampNs -> IO DuckDBValue

{- | Creates a value from an interval

Parameters:
* @input@: The interval value

Returns The value. This must be destroyed with @duckdb_destroy_value@.

These bindings call the wrapper symbol @wrapped_duckdb_create_interval@
but mirror the DuckDB C API semantics of @duckdb_create_interval@.
-}
foreign import ccall unsafe "wrapped_duckdb_create_interval"
    c_duckdb_create_interval :: Ptr DuckDBInterval -> IO DuckDBValue

{- | Creates a value from a blob

Parameters:
* @data@: The blob data
* @length@: The length of the blob data

Returns The value. This must be destroyed with @duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_blob"
    c_duckdb_create_blob :: Ptr Word8 -> DuckDBIdx -> IO DuckDBValue

{- | Creates a BIT value from a duckdb_bit

Parameters:
* @input@: The duckdb_bit value

Returns The value. This must be destroyed with @duckdb_destroy_value@.

These bindings call the wrapper symbol @wrapped_duckdb_create_bit@ but
mirror the DuckDB C API semantics of @duckdb_create_bit@.
-}
foreign import ccall unsafe "wrapped_duckdb_create_bit"
    c_duckdb_create_bit :: Ptr DuckDBBit -> IO DuckDBValue

{- | Creates a UUID value from a uhugeint

Parameters:
* @input@: The duckdb_uhugeint containing the UUID

Returns The value. This must be destroyed with @duckdb_destroy_value@.

These bindings call the wrapper symbol @wrapped_duckdb_create_uuid@ but
mirror the DuckDB C API semantics of @duckdb_create_uuid@.
-}
foreign import ccall unsafe "wrapped_duckdb_create_uuid"
    c_duckdb_create_uuid :: Ptr DuckDBUHugeInt -> IO DuckDBValue

{- | Returns the boolean value of the given value.

Parameters:
* @val@: A duckdb_value containing a boolean

Returns A boolean, or false if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_bool"
    c_duckdb_get_bool :: DuckDBValue -> IO CBool

{- | Returns the int8_t value of the given value.

Parameters:
* @val@: A duckdb_value containing a tinyint

Returns A int8_t, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_int8"
    c_duckdb_get_int8 :: DuckDBValue -> IO Int8

{- | Returns the uint8_t value of the given value.

Parameters:
* @val@: A duckdb_value containing a utinyint

Returns A uint8_t, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_uint8"
    c_duckdb_get_uint8 :: DuckDBValue -> IO Word8

{- | Returns the int16_t value of the given value.

Parameters:
* @val@: A duckdb_value containing a smallint

Returns A int16_t, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_int16"
    c_duckdb_get_int16 :: DuckDBValue -> IO Int16

{- | Returns the uint16_t value of the given value.

Parameters:
* @val@: A duckdb_value containing a usmallint

Returns A uint16_t, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_uint16"
    c_duckdb_get_uint16 :: DuckDBValue -> IO Word16

{- | Returns the int32_t value of the given value.

Parameters:
* @val@: A duckdb_value containing an integer

Returns A int32_t, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_int32"
    c_duckdb_get_int32 :: DuckDBValue -> IO Int32

{- | Returns the uint32_t value of the given value.

Parameters:
* @val@: A duckdb_value containing a uinteger

Returns A uint32_t, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_uint32"
    c_duckdb_get_uint32 :: DuckDBValue -> IO Word32

{- | Returns the int64_t value of the given value.

Parameters:
* @val@: A duckdb_value containing a bigint

Returns A int64_t, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_int64"
    c_duckdb_get_int64 :: DuckDBValue -> IO Int64

{- | Returns the uint64_t value of the given value.

Parameters:
* @val@: A duckdb_value containing a ubigint

Returns A uint64_t, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_uint64"
    c_duckdb_get_uint64 :: DuckDBValue -> IO Word64

{- | Returns the hugeint value of the given value.

Parameters:
* @val@: A duckdb_value containing a hugeint

Returns A duckdb_hugeint, or MinValue if the value cannot be converted

These bindings call the wrapper symbol @wrapped_duckdb_get_hugeint@ but
mirror the DuckDB C API semantics of @duckdb_get_hugeint@.
-}
foreign import ccall unsafe "wrapped_duckdb_get_hugeint"
    c_duckdb_get_hugeint :: DuckDBValue -> Ptr DuckDBHugeInt -> IO ()

{- | Returns the uhugeint value of the given value.

Parameters:
* @val@: A duckdb_value containing a uhugeint

Returns A duckdb_uhugeint, or MinValue if the value cannot be converted

These bindings call the wrapper symbol @wrapped_duckdb_get_uhugeint@
but mirror the DuckDB C API semantics of @duckdb_get_uhugeint@.
-}
foreign import ccall unsafe "wrapped_duckdb_get_uhugeint"
    c_duckdb_get_uhugeint :: DuckDBValue -> Ptr DuckDBUHugeInt -> IO ()

{- | Returns the duckdb_bignum value of the given value. The @data@ field must be
destroyed with @duckdb_free@.

Parameters:
* @val@: A duckdb_value containing a BIGNUM

Returns A duckdb_bignum. The @data@ field must be destroyed with
@duckdb_free@.

These bindings call the wrapper symbol @wrapped_duckdb_get_bignum@ but
mirror the DuckDB C API semantics of @duckdb_get_bignum@.
-}
foreign import ccall unsafe "wrapped_duckdb_get_bignum"
    c_duckdb_get_bignum :: DuckDBValue -> Ptr DuckDBBignum -> IO ()

{- | Returns the duckdb_decimal value of the given value.

Parameters:
* @val@: A duckdb_value containing a DECIMAL

Returns A duckdb_decimal, or MinValue if the value cannot be converted

These bindings call the wrapper symbol @wrapped_duckdb_get_decimal@ but
mirror the DuckDB C API semantics of @duckdb_get_decimal@.
-}
foreign import ccall unsafe "wrapped_duckdb_get_decimal"
    c_duckdb_get_decimal :: DuckDBValue -> Ptr DuckDBDecimal -> IO ()

{- | Returns the float value of the given value.

Parameters:
* @val@: A duckdb_value containing a float

Returns A float, or NAN if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_float"
    c_duckdb_get_float :: DuckDBValue -> IO CFloat

{- | Returns the double value of the given value.

Parameters:
* @val@: A duckdb_value containing a double

Returns A double, or NAN if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_double"
    c_duckdb_get_double :: DuckDBValue -> IO CDouble

{- | Returns the date value of the given value.

Parameters:
* @val@: A duckdb_value containing a date

Returns A duckdb_date, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_date"
    c_duckdb_get_date :: DuckDBValue -> IO DuckDBDate

{- | Returns the time value of the given value.

Parameters:
* @val@: A duckdb_value containing a time

Returns A duckdb_time, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_time"
    c_duckdb_get_time :: DuckDBValue -> IO DuckDBTime

{- | Returns the time_ns value of the given value.

Parameters:
* @val@: A duckdb_value containing a time_ns

Returns A duckdb_time_ns, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_time_ns"
    c_duckdb_get_time_ns :: DuckDBValue -> IO DuckDBTimeNs

{- | Returns the time_tz value of the given value.

Parameters:
* @val@: A duckdb_value containing a time_tz

Returns A duckdb_time_tz, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_time_tz"
    c_duckdb_get_time_tz :: DuckDBValue -> IO DuckDBTimeTz

{- | Returns the TIMESTAMP value of the given value.

Parameters:
* @val@: A duckdb_value containing a TIMESTAMP

Returns A duckdb_timestamp, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_timestamp"
    c_duckdb_get_timestamp :: DuckDBValue -> IO DuckDBTimestamp

{- | Returns the TIMESTAMP_TZ value of the given value.

Parameters:
* @val@: A duckdb_value containing a TIMESTAMP_TZ

Returns A duckdb_timestamp, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_timestamp_tz"
    c_duckdb_get_timestamp_tz :: DuckDBValue -> IO DuckDBTimestamp

{- | Returns the duckdb_timestamp_s value of the given value.

Parameters:
* @val@: A duckdb_value containing a TIMESTAMP_S

Returns A duckdb_timestamp_s, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_timestamp_s"
    c_duckdb_get_timestamp_s :: DuckDBValue -> IO DuckDBTimestampS

{- | Returns the duckdb_timestamp_ms value of the given value.

Parameters:
* @val@: A duckdb_value containing a TIMESTAMP_MS

Returns A duckdb_timestamp_ms, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_timestamp_ms"
    c_duckdb_get_timestamp_ms :: DuckDBValue -> IO DuckDBTimestampMs

{- | Returns the duckdb_timestamp_ns value of the given value.

Parameters:
* @val@: A duckdb_value containing a TIMESTAMP_NS

Returns A duckdb_timestamp_ns, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_timestamp_ns"
    c_duckdb_get_timestamp_ns :: DuckDBValue -> IO DuckDBTimestampNs

{- | Returns the interval value of the given value.

Parameters:
* @val@: A duckdb_value containing a interval

Returns A duckdb_interval, or MinValue if the value cannot be converted

These bindings call the wrapper symbol @wrapped_duckdb_get_interval@
but mirror the DuckDB C API semantics of @duckdb_get_interval@.
-}
foreign import ccall unsafe "wrapped_duckdb_get_interval"
    c_duckdb_get_interval :: DuckDBValue -> Ptr DuckDBInterval -> IO ()

{- | Returns the type of the given value. The type is valid as long as the value is
not destroyed. The type itself must not be destroyed.

Parameters:
* @val@: A duckdb_value

Returns A duckdb_logical_type.
-}
foreign import ccall unsafe "duckdb_get_value_type"
    c_duckdb_get_value_type :: DuckDBValue -> IO DuckDBLogicalType

{- | Returns the blob value of the given value.

Parameters:
* @val@: A duckdb_value containing a blob

Returns A duckdb_blob

These bindings call the wrapper symbol @wrapped_duckdb_get_blob@ but
mirror the DuckDB C API semantics of @duckdb_get_blob@.
-}
foreign import ccall unsafe "wrapped_duckdb_get_blob"
    c_duckdb_get_blob :: DuckDBValue -> Ptr DuckDBBlob -> IO ()

{- | Returns the duckdb_bit value of the given value. The @data@ field must be
destroyed with @duckdb_free@.

Parameters:
* @val@: A duckdb_value containing a BIT

Returns A duckdb_bit

These bindings call the wrapper symbol @wrapped_duckdb_get_bit@ but
mirror the DuckDB C API semantics of @duckdb_get_bit@.
-}
foreign import ccall unsafe "wrapped_duckdb_get_bit"
    c_duckdb_get_bit :: DuckDBValue -> Ptr DuckDBBit -> IO ()

{- | Returns a duckdb_uhugeint representing the UUID value of the given value.

Parameters:
* @val@: A duckdb_value containing a UUID

Returns A duckdb_uhugeint representing the UUID value

These bindings call the wrapper symbol @wrapped_duckdb_get_uuid@ but
mirror the DuckDB C API semantics of @duckdb_get_uuid@.
-}
foreign import ccall unsafe "wrapped_duckdb_get_uuid"
    c_duckdb_get_uuid :: DuckDBValue -> Ptr DuckDBUHugeInt -> IO ()

{- | Obtains a string representation of the given value. The result must be
destroyed with @duckdb_free@.

Parameters:
* @value@: The value

Returns The string value. This must be destroyed with @duckdb_free@.
-}
foreign import ccall unsafe "duckdb_get_varchar"
    c_duckdb_get_varchar :: DuckDBValue -> IO CString

{- | Creates a struct value from a type and an array of values. Must be destroyed
with @duckdb_destroy_value@.

Parameters:
* @type@: The type of the struct
* @values@: The values for the struct fields

Returns The struct value, or nullptr, if any child type is @DUCKDB_TYPE_ANY@
or @DUCKDB_TYPE_INVALID@.
-}
foreign import ccall unsafe "duckdb_create_struct_value"
    c_duckdb_create_struct_value :: DuckDBLogicalType -> Ptr DuckDBValue -> IO DuckDBValue

{- | Creates a list value from a child (element) type and an array of values of
length @value_count@. Must be destroyed with @duckdb_destroy_value@.

Parameters:
* @type@: The type of the list
* @values@: The values for the list
* @value_count@: The number of values in the list

Returns The list value, or nullptr, if the child type is @DUCKDB_TYPE_ANY@ or
@DUCKDB_TYPE_INVALID@.
-}
foreign import ccall unsafe "duckdb_create_list_value"
    c_duckdb_create_list_value :: DuckDBLogicalType -> Ptr DuckDBValue -> DuckDBIdx -> IO DuckDBValue

{- | Creates an array value from a child (element) type and an array of values of
length @value_count@. Must be destroyed with @duckdb_destroy_value@.

Parameters:
* @type@: The type of the array
* @values@: The values for the array
* @value_count@: The number of values in the array

Returns The array value, or nullptr, if the child type is @DUCKDB_TYPE_ANY@ or
@DUCKDB_TYPE_INVALID@.
-}
foreign import ccall unsafe "duckdb_create_array_value"
    c_duckdb_create_array_value :: DuckDBLogicalType -> Ptr DuckDBValue -> DuckDBIdx -> IO DuckDBValue

{- | Creates a map value from a map type and two arrays, one for the keys and one
for the values, each of length @entry_count@. Must be destroyed with
@duckdb_destroy_value@.

Parameters:
* @map_type@: The map type
* @keys@: The keys of the map
* @values@: The values of the map
* @entry_count@: The number of entrys (key-value pairs) in the map

Returns The map value, or nullptr, if the parameters are invalid.
-}
foreign import ccall unsafe "duckdb_create_map_value"
    c_duckdb_create_map_value :: DuckDBLogicalType -> Ptr DuckDBValue -> Ptr DuckDBValue -> DuckDBIdx -> IO DuckDBValue

{- | Creates a union value from a union type, a tag index, and a value. Must be
destroyed with @duckdb_destroy_value@.

Parameters:
* @union_type@: The union type
* @tag_index@: The index of the tag of the union
* @value@: The value of the union for that tag

Returns The union value, or nullptr, if the parameters are invalid.
-}
foreign import ccall unsafe "duckdb_create_union_value"
    c_duckdb_create_union_value :: DuckDBLogicalType -> DuckDBIdx -> DuckDBValue -> IO DuckDBValue

{- | Returns the number of elements in a MAP value.

Parameters:
* @value@: The MAP value.

Returns The number of elements in the map.
-}
foreign import ccall unsafe "duckdb_get_map_size"
    c_duckdb_get_map_size :: DuckDBValue -> IO DuckDBIdx

{- | Returns the MAP key at index as a duckdb_value.

Parameters:
* @value@: The MAP value.
* @index@: The index of the key.

Returns The key as a duckdb_value.
-}
foreign import ccall unsafe "duckdb_get_map_key"
    c_duckdb_get_map_key :: DuckDBValue -> DuckDBIdx -> IO DuckDBValue

{- | Returns the MAP value at index as a duckdb_value.

Parameters:
* @value@: The MAP value.
* @index@: The index of the value.

Returns The value as a duckdb_value.
-}
foreign import ccall unsafe "duckdb_get_map_value"
    c_duckdb_get_map_value :: DuckDBValue -> DuckDBIdx -> IO DuckDBValue

{- | Returns whether the value's type is SQLNULL or not.

Parameters:
* @value@: The value to check.

Returns True, if the value's type is SQLNULL, otherwise false.
-}
foreign import ccall unsafe "duckdb_is_null_value"
    c_duckdb_is_null_value :: DuckDBValue -> IO CBool

{- | Creates a value of type SQLNULL.

Returns The duckdb_value representing SQLNULL. This must be destroyed with
@duckdb_destroy_value@.
-}
foreign import ccall unsafe "duckdb_create_null_value"
    c_duckdb_create_null_value :: IO DuckDBValue

{- | Returns the number of elements in a LIST value.

Parameters:
* @value@: The LIST value.

Returns The number of elements in the list.
-}
foreign import ccall unsafe "duckdb_get_list_size"
    c_duckdb_get_list_size :: DuckDBValue -> IO DuckDBIdx

{- | Returns the LIST child at index as a duckdb_value.

Parameters:
* @value@: The LIST value.
* @index@: The index of the child.

Returns The child as a duckdb_value.
-}
foreign import ccall unsafe "duckdb_get_list_child"
    c_duckdb_get_list_child :: DuckDBValue -> DuckDBIdx -> IO DuckDBValue

{- | Creates an enum value from a type and a value. Must be destroyed with
@duckdb_destroy_value@.

Parameters:
* @type@: The type of the enum
* @value@: The value for the enum

Returns The enum value, or nullptr.
-}
foreign import ccall unsafe "duckdb_create_enum_value"
    c_duckdb_create_enum_value :: DuckDBLogicalType -> Word64 -> IO DuckDBValue

{- | Returns the enum value of the given value.

Parameters:
* @value@: A duckdb_value containing an enum

Returns A uint64_t, or MinValue if the value cannot be converted
-}
foreign import ccall unsafe "duckdb_get_enum_value"
    c_duckdb_get_enum_value :: DuckDBValue -> IO Word64

{- | Returns the STRUCT child at index as a duckdb_value.

Parameters:
* @value@: The STRUCT value.
* @index@: The index of the child.

Returns The child as a duckdb_value.
-}
foreign import ccall unsafe "duckdb_get_struct_child"
    c_duckdb_get_struct_child :: DuckDBValue -> DuckDBIdx -> IO DuckDBValue

{- | Returns the SQL string representation of the given value.

Parameters:
* @value@: A duckdb_value.

Returns The SQL string representation as a null-terminated string. The result
must be freed with @duckdb_free@.
-}
foreign import ccall unsafe "duckdb_value_to_string"
    c_duckdb_value_to_string :: DuckDBValue -> IO CString
