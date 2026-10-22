#include "duckdb_haskell_extension.h"
#include "Rts.h"
#include <pthread.h>

extern void duckdb_haskell_demo_register(HsPtr extension, HsPtr context, HsPtr err);

static pthread_once_t runtime_once = PTHREAD_ONCE_INIT;

static void start_runtime(void) {
    int argc = 1;
    static char name[] = "haskell_demo";
    static char *arguments[] = {name, NULL};
    char **argv = arguments;
    RtsConfig config = defaultRtsConfig;
    config.rts_hs_main = HS_BOOL_FALSE;
    config.rts_opts_enabled = RtsOptsIgnoreAll;
    config.rts_opts = "-N1 --install-signal-handlers=no";
    hs_init_ghc(&argc, &argv, config);
}

/* Keep the runtime alive while DuckDB owns Haskell callbacks and state. */
DUCKDB_EXTENSION_API void haskell_demo_init_c_api_v2(struct duckdb_v2_extension_input *input) {
    if (!duckdb_haskell_extension_init_api(input)) {
        return;
    }
    pthread_once(&runtime_once, start_runtime);
    duckdb_haskell_demo_register(input->extension, input->context, input->err);
}
