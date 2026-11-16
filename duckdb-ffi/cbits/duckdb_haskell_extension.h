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

/* The Haskell registration action of an extension. It receives the extension
 * handle, the borrowed context, and the error slot of the load. */
typedef void (*duckdb_haskell_extension_entry)(void *extension, void *context, void *err);

/* Load a Haskell extension. Call this function from the C entry point
 * NAME_init_c_api_v2 with the foreign export of the registration action. It
 * negotiates and copies the host table, starts the threaded GHC runtime once
 * for the process, and calls the entry. It reports a failure through
 * input->err. Only this function is for use outside duckdb-ffi. The table is
 * hidden in the duckdb-ffi library, so other C code cannot call DuckDB through
 * the redirected names. */
DUCKDB_EXTENSION_API void duckdb_haskell_extension_load(struct duckdb_v2_extension_input *input,
                                                        duckdb_haskell_extension_entry entry);

DUCKDB_EXTENSION_EXTERN_C_GUARD_CLOSE

#endif
