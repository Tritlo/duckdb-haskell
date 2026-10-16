#include "duckdb_haskell_extension.h"
#include <pthread.h>

DUCKDB_EXTENSION_GLOBAL

static pthread_mutex_t api_mutex = PTHREAD_MUTEX_INITIALIZER;
static int api_ready = 0;

int duckdb_haskell_extension_init_api(struct duckdb_v2_extension_input *input) {
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
