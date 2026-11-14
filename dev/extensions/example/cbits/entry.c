#include "duckdb_haskell_extension.h"

extern void duckdb_haskell_demo_register(void *extension, void *context, void *err);

/* DuckDB derives this name from the file name of the packaged extension. */
DUCKDB_EXTENSION_API void haskell_demo_init_c_api_v2(struct duckdb_v2_extension_input *input) {
    duckdb_haskell_extension_load(input, duckdb_haskell_demo_register);
}
