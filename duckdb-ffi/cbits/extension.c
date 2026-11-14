#include "duckdb_haskell_extension.h"
#include "Rts.h"
#include <pthread.h>

DUCKDB_EXTENSION_GLOBAL

static pthread_mutex_t api_mutex = PTHREAD_MUTEX_INITIALIZER;
static int api_ready = 0;
static pthread_once_t runtime_once = PTHREAD_ONCE_INIT;

/* Negotiate and copy the host table before starting Haskell. */
static int init_api(struct duckdb_v2_extension_input *input) {
    /* DuckDB owns this temporary table. Do not retain its address. */
    const duckdb_ext_api_v2 *api = input->get_api(input->extension, DUCKDB_EXTENSION_API_VERSION_STRING);
    if (!api) {
        /* DuckDB has recorded the negotiation error. Leave input->err alone. */
        return 0;
    }

    /* Publish once. Later loads must not write over active callback readers. */
    pthread_mutex_lock(&api_mutex);
    if (!api_ready) {
        duckdb_ext_api = *api;
        api_ready = 1;
    }
    pthread_mutex_unlock(&api_mutex);
    return 1;
}

/* Use private arguments, ignore GHCRTS, and keep the signal handlers of the host. */
static void start_runtime(void) {
    int argc = 1;
    static char name[] = "duckdb_haskell_extension";
    static char *arguments[] = {name, NULL};
    char **argv = arguments;
    RtsConfig config = defaultRtsConfig;
    config.rts_hs_main = HS_BOOL_FALSE;
    config.rts_opts_enabled = RtsOptsIgnoreAll;
    config.rts_opts = "-N1 --install-signal-handlers=no";
    hs_init_ghc(&argc, &argv, config);
}

void duckdb_haskell_extension_load(struct duckdb_v2_extension_input *input, duckdb_haskell_extension_entry entry) {
    if (!init_api(input)) {
        return;
    }
    if (!rtsSupportsBoundThreads()) {
        /* The -N1 option ends the process when the runtime has no thread support. */
        static const char message[] = "A Haskell extension requires the threaded GHC runtime. Link it with -threaded.";
        duckdb_v2_str text = {message, sizeof(message) - 1};
        duckdb_v2_error_info_set_code(*input->err, DUCKDB_V2_ERROR_API);
        duckdb_v2_error_info_set_text(*input->err, &text);
        return;
    }
    /* Keep the runtime alive while DuckDB owns Haskell callbacks and state. */
    pthread_once(&runtime_once, start_runtime);
    entry(input->extension, input->context, input->err);
}
