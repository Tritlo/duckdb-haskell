#ifndef DUCKDB_HASKELL_EXTENSION_H
#define DUCKDB_HASKELL_EXTENSION_H

/* Include this header before the generated V2 C wrappers. */
#define DUCKDB_V2_API_ALLOW_UNSTABLE 1
#include "duckdb_extension_v2.h"

DUCKDB_EXTENSION_EXTERN_C_GUARD_OPEN

/* Keep the dispatch table private to this duckdb-ffi library. */
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC visibility push(hidden)
#endif
DUCKDB_EXTENSION_EXTERN
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC visibility pop
#endif

/* Negotiate and copy the host table before starting Haskell. */
DUCKDB_EXTENSION_API int duckdb_haskell_extension_init_api(struct duckdb_v2_extension_input *input);

DUCKDB_EXTENSION_EXTERN_C_GUARD_CLOSE

#endif
