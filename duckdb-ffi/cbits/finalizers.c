/* C finalizers for owned DuckDB handles.
 *
 * A Haskell ForeignPtr takes the address of a C function as its finalizer. A
 * loadable extension calls DuckDB through the function table of its host, and
 * it has no DuckDB symbol, so Haskell code cannot take the address of a DuckDB
 * function. Each function here calls DuckDB through the same path as each other
 * binding: directly in a program, and through the function table in an
 * extension. The functions have default visibility, so that other libraries of
 * an extension can take their address. */
#ifndef DUCKDB_V2_API_ALLOW_UNSTABLE
#define DUCKDB_V2_API_ALLOW_UNSTABLE 1
#endif
#include "duckdb_v2.h"

#if defined(__GNUC__) || defined(__clang__)
#define DUCKDB_FFI_EXPORT __attribute__((visibility("default")))
#else
#define DUCKDB_FFI_EXPORT
#endif

/* Destroy an owned logical type. The destruction of a valid handle does not
 * fail, so the function ignores the result code. */
DUCKDB_FFI_EXPORT void duckdb_ffi_destroy_logical_type(duckdb_v2_logical_type_handle *type) {
    (void)duckdb_v2_logical_type_destroy(type);
}

/* Destroy an owned schema. The function ignores the result code, as above. */
DUCKDB_FFI_EXPORT void duckdb_ffi_destroy_schema(duckdb_v2_schema_handle *schema) {
    (void)duckdb_v2_schema_destroy(schema);
}
