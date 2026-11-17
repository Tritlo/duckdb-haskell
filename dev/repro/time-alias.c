#define DUCKDB_V2_API_ALLOW_UNSTABLE 1
#include "duckdb_v2.h"
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

#define OK(call) assert((call) == 0)

int main(void) {
    duckdb_v2_environment_handle env = NULL;
    duckdb_v2_instance_handle instance = NULL;
    duckdb_v2_factory_handle factory = NULL;
    duckdb_v2_logical_type_handle base = NULL, alias = NULL, actual = NULL;
    duckdb_v2_data_chunk_handle chunk = NULL;
    duckdb_v2_vector_handle vector = NULL;
    duckdb_v2_value_handle input = NULL, output = NULL, casted = NULL;
    duckdb_v2_identifier_t name = {"clock_alias", 11};
    int64_t micros = 45296123456LL, result = -1;
    bool equal = false;
    void *raw = NULL;

    OK(duckdb_v2_environment_create(&env, NULL));
    OK(duckdb_v2_instance_create(env, &instance, NULL));
    OK(duckdb_v2_instance_get_factory(instance, &factory, NULL));
    OK(duckdb_v2_factory_create_type_from_id(factory,
        DUCKDB_V2_LOGICAL_TYPE_ID_TIME, NULL, NULL, 0, &base, NULL));
    OK(duckdb_v2_factory_create_type_with_alias(factory, base, &name, &alias, NULL));
    OK(duckdb_v2_data_chunk_create(factory, &alias, 1, &chunk, NULL));
    OK(duckdb_v2_data_chunk_get_vector(chunk, 0, &vector, NULL));
    OK(duckdb_v2_vector_flatten(vector, NULL));
    OK(duckdb_v2_vector_set_size(vector, 1, NULL));
    OK(duckdb_v2_value_create_time(factory, micros, &input, NULL));

    DUCKDB_V2_ERROR setter = duckdb_v2_vector_set_value(vector, 0, input, NULL);
    DUCKDB_V2_ERROR cast = duckdb_v2_value_cast(factory, input, alias, &casted, NULL);
    printf("TIME -> clock_alias: vector_set_value=%d value_cast=%d\n", setter, cast);
    assert(setter == DUCKDB_V2_ERROR_INPUT_INVALID);
    assert(cast == DUCKDB_V2_ERROR_INPUT_INVALID);

    OK(duckdb_v2_vector_get_data_mutable(vector, &raw, NULL));
    ((int64_t *)raw)[0] = micros;
    uint64_t *validity = NULL;
    OK(duckdb_v2_vector_flat_get_validity_mutable(vector, &validity, NULL));
    validity[0] |= 1;
    OK(duckdb_v2_vector_get_value(vector, 0, &output, NULL));
    OK(duckdb_v2_value_get_time(output, &result, NULL));
    OK(duckdb_v2_value_get_logical_type(output, &actual, NULL));
    OK(duckdb_v2_logical_type_is_equal(alias, actual, &equal, NULL));
    printf("public buffer: microseconds=%" PRId64 " alias_equal=%d\n", result, equal);
    assert(result == micros && equal);

    duckdb_v2_logical_type_destroy(&actual);
    duckdb_v2_value_destroy(&output);
    duckdb_v2_value_destroy(&casted);
    duckdb_v2_value_destroy(&input);
    duckdb_v2_data_chunk_destroy(&chunk);
    duckdb_v2_logical_type_destroy(&alias);
    duckdb_v2_logical_type_destroy(&base);
    duckdb_v2_instance_destroy(&instance);
    duckdb_v2_environment_destroy(&env);
    return 0;
}
