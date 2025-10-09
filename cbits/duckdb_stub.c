#include "duckdb.h"
#include <stdint.h>

// duckdb_query_progress returns a struct by value; expose a pointer-based version
// so the Haskell FFI can marshal the result and so we can gracefully handle NULL.
void wrapped_duckdb_query_progress(duckdb_connection connection, duckdb_query_progress_type *out_progress) {
  if (!out_progress) {
    return;
  }
  *out_progress = duckdb_query_progress(connection);
}

// duckdb_result_statement_type consumes a duckdb_result by value; this wrapper lets
// us pass a pointer from Haskell and returns a safe default when NULL is supplied.
duckdb_statement_type wrapped_duckdb_result_statement_type(const duckdb_result *result) {
  if (!result) {
    return DUCKDB_STATEMENT_TYPE_INVALID;
  }
  return duckdb_result_statement_type(*result);
}

// duckdb_result_return_type also expects the result struct by value; wrap it so we
// can call it from Haskell while avoiding undefined behaviour on NULL.
duckdb_result_type wrapped_duckdb_result_return_type(const duckdb_result *result) {
  if (!result) {
    return DUCKDB_RESULT_TYPE_INVALID;
  }
  return duckdb_result_return_type(*result);
}

// duckdb_result_get_arrow_options mutates the result structure; we pass the pointer
// through after checking for NULL and casting away const for the FFI surface.
duckdb_arrow_options wrapped_duckdb_result_get_arrow_options(const duckdb_result *result) {
  if (!result) {
    return NULL;
  }
  return duckdb_result_get_arrow_options((duckdb_result *)result);
}

// duckdb_result_get_chunk takes the result by value; we provide a pointer-friendly
// wrapper that also protects against accidental NULL dereferences.
duckdb_data_chunk wrapped_duckdb_result_get_chunk(const duckdb_result *result, idx_t chunk_index) {
  if (!result) {
    return NULL;
  }
  return duckdb_result_get_chunk(*result, chunk_index);
}

// duckdb_result_chunk_count takes a duckdb_result by value; mirror it with pointer
// semantics for the Haskell FFI.
idx_t wrapped_duckdb_result_chunk_count(const duckdb_result *result) {
  if (!result) {
    return 0;
  }
  return duckdb_result_chunk_count(*result);
}

// duckdb_result_is_streaming takes the result by value; add a pointer-based shim
// so we can pass NULL safely from the managed side.
bool wrapped_duckdb_result_is_streaming(const duckdb_result *result) {
  if (!result) {
    return false;
  }
  return duckdb_result_is_streaming(*result);
}

// duckdb_value_hugeint returns a struct by value; expose an out-parameter variant
// with NULL guards for easier use from Haskell.
void wrapped_duckdb_value_hugeint(duckdb_result *result, idx_t col, idx_t row, duckdb_hugeint *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_value_hugeint(result, col, row);
}

// duckdb_value_uhugeint returns a struct by value; wrap it to write into caller-
// supplied storage and to tolerate NULL pointers.
void wrapped_duckdb_value_uhugeint(duckdb_result *result, idx_t col, idx_t row, duckdb_uhugeint *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_value_uhugeint(result, col, row);
}

// duckdb_value_decimal returns a struct by value; provide an out-parameter version
// so we can marshal it easily.
void wrapped_duckdb_value_decimal(duckdb_result *result, idx_t col, idx_t row, duckdb_decimal *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_value_decimal(result, col, row);
}

// duckdb_value_string returns a struct by value; expose a wrapper that fills a
// duckdb_string for the caller if the output pointer is non-NULL.
void wrapped_duckdb_value_string(duckdb_result *result, idx_t col, idx_t row, duckdb_string *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_value_string(result, col, row);
}

// duckdb_value_string_internal also returns a struct by value; same rationale as
// wrapped_duckdb_value_string.
void wrapped_duckdb_value_string_internal(duckdb_result *result, idx_t col, idx_t row, duckdb_string *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_value_string_internal(result, col, row);
}

// duckdb_value_blob returns a struct by value; copy into caller storage and guard
// against NULL in the wrapper.
void wrapped_duckdb_value_blob(duckdb_result *result, idx_t col, idx_t row, duckdb_blob *out_blob) {
  if (!out_blob) {
    return;
  }
  *out_blob = duckdb_value_blob(result, col, row);
}

// duckdb_value_interval returns a struct by value; expose pointer-friendly variant
// for the managed bindings.
void wrapped_duckdb_value_interval(duckdb_result *result, idx_t col, idx_t row, duckdb_interval *out_interval) {
  if (!out_interval) {
    return;
  }
  *out_interval = duckdb_value_interval(result, col, row);
}

// duckdb_string_is_inlined expects a struct by value; we pass a pointer and guard
// against NULL before delegating.
bool wrapped_duckdb_string_is_inlined(const duckdb_string_t *string) {
  if (!string) {
    return false;
  }
  return duckdb_string_is_inlined(*string);
}

// duckdb_string_t_length reads from duckdb_string_t by value; provide a pointer
// based wrapper that handles NULL safely.
uint32_t wrapped_duckdb_string_t_length(const duckdb_string_t *string) {
  if (!string) {
    return 0;
  }
  return duckdb_string_t_length(*string);
}

// duckdb_result_arrow_array modifies the arrow array; we add NULL checks so FFI
// callers can omit either pointer without crashing the process.
void wrapped_duckdb_result_arrow_array(const duckdb_result *result, duckdb_data_chunk chunk, duckdb_arrow_array *out_array) {
  if (!result || !out_array) {
    return;
  }
  duckdb_result_arrow_array(*result, chunk, out_array);
}

// duckdb_stream_fetch_chunk takes duckdb_result by value; provide a pointer form
// so we can handle NULLs and avoid struct copies in the FFI.
duckdb_data_chunk wrapped_duckdb_stream_fetch_chunk(const duckdb_result *result) {
  if (!result) {
    return NULL;
  }
  return duckdb_stream_fetch_chunk(*result);
}

// duckdb_fetch_chunk also expects duckdb_result by value; same pointer-friendly wrap
// as above.
duckdb_data_chunk wrapped_duckdb_fetch_chunk(const duckdb_result *result) {
  if (!result) {
    return NULL;
  }
  return duckdb_fetch_chunk(*result);
}

// duckdb_bind_interval consumes duckdb_interval by value; we copy from the pointer
// so Haskell can pass NULL and avoid struct marshalling complexities.
duckdb_state wrapped_duckdb_bind_interval(duckdb_prepared_statement statement, idx_t param_idx, const duckdb_interval *val) {
  duckdb_interval interval_value = {0};
  if (val) {
    interval_value = *val;
  }
  return duckdb_bind_interval(statement, param_idx, interval_value);
}

// duckdb_bind_hugeint requires a struct by value; wrap to accept a pointer and
// provide a zero-initialised default when NULL.
duckdb_state wrapped_duckdb_bind_hugeint(duckdb_prepared_statement statement, idx_t param_idx, const duckdb_hugeint *val) {
  duckdb_hugeint hugeint_value = {0};
  if (val) {
    hugeint_value = *val;
  }
  return duckdb_bind_hugeint(statement, param_idx, hugeint_value);
}

// duckdb_bind_uhugeint requires a struct by value; same pointer-based shim as for
// duckdb_bind_hugeint.
duckdb_state wrapped_duckdb_bind_uhugeint(duckdb_prepared_statement statement, idx_t param_idx, const duckdb_uhugeint *val) {
  duckdb_uhugeint uhugeint_value = {0};
  if (val) {
    uhugeint_value = *val;
  }
  return duckdb_bind_uhugeint(statement, param_idx, uhugeint_value);
}

// duckdb_bind_decimal consumes duckdb_decimal by value; wrap it so we can accept a
// pointer and default to zero when NULL arrives from Haskell.
duckdb_state wrapped_duckdb_bind_decimal(duckdb_prepared_statement statement, idx_t param_idx, const duckdb_decimal *val) {
  duckdb_decimal decimal_value = {0};
  if (val) {
    decimal_value = *val;
  }
  return duckdb_bind_decimal(statement, param_idx, decimal_value);
}

// duckdb_create_hugeint takes the struct by value; the wrapper copies from the
// user pointer so Haskell can choose to pass NULL.
duckdb_value wrapped_duckdb_create_hugeint(const duckdb_hugeint *input) {
  duckdb_hugeint value = {0};
  if (input) {
    value = *input;
  }
  return duckdb_create_hugeint(value);
}

// duckdb_create_uhugeint takes the struct by value; wrap to accept optional pointer
// input from the FFI.
duckdb_value wrapped_duckdb_create_uhugeint(const duckdb_uhugeint *input) {
  duckdb_uhugeint value = {0};
  if (input) {
    value = *input;
  }
  return duckdb_create_uhugeint(value);
}

// duckdb_create_bignum consumes duckdb_bignum by value; copy from pointer so the
// managed caller can omit it.
duckdb_value wrapped_duckdb_create_bignum(const duckdb_bignum *input) {
  duckdb_bignum value = {0};
  if (input) {
    value = *input;
  }
  return duckdb_create_bignum(value);
}

// duckdb_create_decimal expects duckdb_decimal by value; pointer wrapper allows us
// to default the argument and plays nicely with the FFI.
duckdb_value wrapped_duckdb_create_decimal(const duckdb_decimal *input) {
  duckdb_decimal value = {0};
  if (input) {
    value = *input;
  }
  return duckdb_create_decimal(value);
}

// duckdb_create_interval expects duckdb_interval by value; copy from pointer so
// the managed caller can pass NULL and avoid struct marshalling.
duckdb_value wrapped_duckdb_create_interval(const duckdb_interval *input) {
  duckdb_interval value = {0};
  if (input) {
    value = *input;
  }
  return duckdb_create_interval(value);
}

// duckdb_create_bit requires duckdb_bit by value; copy from pointer for ease of use
// on the Haskell side.
duckdb_value wrapped_duckdb_create_bit(const duckdb_bit *input) {
  duckdb_bit value = {0};
  if (input) {
    value = *input;
  }
  return duckdb_create_bit(value);
}

// duckdb_create_uuid expects a duckdb_uhugeint by value; wrap it so NULL pointers
// are handled and the FFI can reuse the helper.
duckdb_value wrapped_duckdb_create_uuid(const duckdb_uhugeint *input) {
  duckdb_uhugeint value = {0};
  if (input) {
    value = *input;
  }
  return duckdb_create_uuid(value);
}

// duckdb_append_hugeint consumes a struct by value; copy from pointer before
// delegating to avoid marshalling issues from Haskell.
duckdb_state wrapped_duckdb_append_hugeint(duckdb_appender appender, const duckdb_hugeint *value) {
  duckdb_hugeint tmp = {0};
  if (value) {
    tmp = *value;
  }
  return duckdb_append_hugeint(appender, tmp);
}

// duckdb_append_uhugeint also needs the value struct by copy; pointer wrapper keeps
// the FFI simple and guards NULL.
duckdb_state wrapped_duckdb_append_uhugeint(duckdb_appender appender, const duckdb_uhugeint *value) {
  duckdb_uhugeint tmp = {0};
  if (value) {
    tmp = *value;
  }
  return duckdb_append_uhugeint(appender, tmp);
}

// duckdb_append_interval expects a duckdb_interval by value; use a pointer wrapper
// to appease the Haskell FFI and handle absent values.
duckdb_state wrapped_duckdb_append_interval(duckdb_appender appender, const duckdb_interval *value) {
  duckdb_interval tmp = {0};
  if (value) {
    tmp = *value;
  }
  return duckdb_append_interval(appender, tmp);
}

// duckdb_get_hugeint returns a struct by value; write the result into caller
// storage to avoid struct-return ABI issues in Haskell.
void wrapped_duckdb_get_hugeint(duckdb_value val, duckdb_hugeint *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_get_hugeint(val);
}

// duckdb_get_uhugeint returns a struct by value; same pointer-based extraction as
// wrapped_duckdb_get_hugeint.
void wrapped_duckdb_get_uhugeint(duckdb_value val, duckdb_uhugeint *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_get_uhugeint(val);
}

// duckdb_get_bignum returns a struct by value; write the result into an out pointer
// for the managed caller.
void wrapped_duckdb_get_bignum(duckdb_value val, duckdb_bignum *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_get_bignum(val);
}

// duckdb_get_decimal returns a struct by value; provide the same pointer-based
// access pattern as the other get wrappers.
void wrapped_duckdb_get_decimal(duckdb_value val, duckdb_decimal *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_get_decimal(val);
}

// duckdb_get_interval returns a struct by value; wrap it to write into caller
// memory.
void wrapped_duckdb_get_interval(duckdb_value val, duckdb_interval *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_get_interval(val);
}

// duckdb_get_blob returns duckdb_blob by value; expose an out-parameter wrapper for
// the FFI.
void wrapped_duckdb_get_blob(duckdb_value val, duckdb_blob *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_get_blob(val);
}

// duckdb_get_bit returns duckdb_bit by value; wrap it to fill caller-provided
// storage.
void wrapped_duckdb_get_bit(duckdb_value val, duckdb_bit *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_get_bit(val);
}

// duckdb_get_uuid returns duckdb_uhugeint by value; write into an out pointer for
// the managed binding.
void wrapped_duckdb_get_uuid(duckdb_value val, duckdb_uhugeint *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_get_uuid(val);
}

// duckdb_hugeint_to_double consumes its argument by value; copy from pointer so we
// can call it from Haskell without struct marshalling.
double wrapped_duckdb_hugeint_to_double(const duckdb_hugeint *value) {
  duckdb_hugeint tmp = {0};
  if (value) {
    tmp = *value;
  }
  return duckdb_hugeint_to_double(tmp);
}

// duckdb_double_to_hugeint returns a struct by value; provide an out pointer for
// the managed side.
void wrapped_duckdb_double_to_hugeint(double input, duckdb_hugeint *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_double_to_hugeint(input);
}

// duckdb_double_to_decimal returns duckdb_decimal by value; write into caller
// storage instead.
void wrapped_duckdb_double_to_decimal(double input, uint8_t width, uint8_t scale, duckdb_decimal *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_double_to_decimal(input, width, scale);
}

// duckdb_decimal_to_double consumes its argument by value; copy from pointer so
// the FFI can pass nullable inputs.
double wrapped_duckdb_decimal_to_double(const duckdb_decimal *value) {
  duckdb_decimal tmp = {0};
  if (value) {
    tmp = *value;
  }
  return duckdb_decimal_to_double(tmp);
}

// duckdb_from_date returns a struct by value; emit the result through an
// out-parameter to avoid struct-return plumbing on the Haskell side.
void wrapped_duckdb_from_date(duckdb_date date, duckdb_date_struct *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_from_date(date);
}

// duckdb_to_date expects a struct by value; copy from pointer so NULL can be passed
// and so the ABI stays simple.
duckdb_date wrapped_duckdb_to_date(const duckdb_date_struct *input) {
  duckdb_date_struct tmp = {0};
  if (input) {
    tmp = *input;
  }
  return duckdb_to_date(tmp);
}

// duckdb_from_time returns a struct by value; wrap to emit via an out pointer.
void wrapped_duckdb_from_time(duckdb_time time, duckdb_time_struct *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_from_time(time);
}

// duckdb_from_time_tz returns a struct by value; provide pointer-based output.
void wrapped_duckdb_from_time_tz(duckdb_time_tz micros, duckdb_time_tz_struct *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_from_time_tz(micros);
}

// duckdb_to_time expects a struct by value; copy from pointer for the bindings.
duckdb_time wrapped_duckdb_to_time(const duckdb_time_struct *input) {
  duckdb_time_struct tmp = {0};
  if (input) {
    tmp = *input;
  }
  return duckdb_to_time(tmp);
}

// duckdb_from_timestamp returns a struct by value; write into caller-provided
// storage instead.
void wrapped_duckdb_from_timestamp(duckdb_timestamp ts, duckdb_timestamp_struct *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_from_timestamp(ts);
}

// duckdb_to_timestamp expects a struct by value; copy from pointer before calling.
duckdb_timestamp wrapped_duckdb_to_timestamp(const duckdb_timestamp_struct *input) {
  duckdb_timestamp_struct tmp = {0};
  if (input) {
    tmp = *input;
  }
  return duckdb_to_timestamp(tmp);
}

// duckdb_uhugeint_to_double consumes its argument by value; copy from pointer to
// make it callable from Haskell.
double wrapped_duckdb_uhugeint_to_double(const duckdb_uhugeint *value) {
  duckdb_uhugeint tmp = {0};
  if (value) {
    tmp = *value;
  }
  return duckdb_uhugeint_to_double(tmp);
}

// duckdb_double_to_uhugeint returns a struct by value; write the result into caller
// storage instead of relying on struct-return ABI support.
void wrapped_duckdb_double_to_uhugeint(double input, duckdb_uhugeint *out_value) {
  if (!out_value) {
    return;
  }
  *out_value = duckdb_double_to_uhugeint(input);
}
