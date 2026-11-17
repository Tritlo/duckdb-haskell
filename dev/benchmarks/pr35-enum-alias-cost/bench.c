#define DUCKDB_V2_API_ALLOW_UNSTABLE 1
#include "duckdb_v2.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define CHECK(call) do { if ((call) != 0) { fprintf(stderr, "failed: %s\n", #call); exit(1); } } while (0)

static double now(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec + t.tv_nsec / 1e9;
}

static duckdb_v2_logical_type_handle make_enum(duckdb_v2_factory_handle factory, size_t n) {
    char *sql = malloc(n * 32 + 16);
    if (!sql) exit(1);
    size_t offset = (size_t)sprintf(sql, "ENUM(");
    for (size_t i = 0; i < n; ++i) offset += (size_t)sprintf(sql + offset, "%s'label%zu'", i ? "," : "", i);
    offset += (size_t)sprintf(sql + offset, ")");
    duckdb_v2_logical_type_handle result = NULL;
    CHECK(duckdb_v2_factory_create_type_from_text(factory, &(duckdb_v2_str){sql, offset}, &result, NULL));
    free(sql);
    return result;
}

int main(void) {
    duckdb_v2_environment_handle env = NULL;
    duckdb_v2_instance_handle instance = NULL;
    duckdb_v2_factory_handle factory = NULL;
    CHECK(duckdb_v2_environment_create(&env, NULL));
    CHECK(duckdb_v2_instance_create(env, &instance, NULL));
    CHECK(duckdb_v2_instance_get_factory(instance, &factory, NULL));
    const size_t sizes[] = {1, 1000, 100000};
    puts("labels,kind,operation,sample,iterations,seconds,ns_per_operation");
    for (size_t s = 0; s < 3; ++s) {
        size_t n = sizes[s], iterations = n == 100000 ? 100 : 10000;
        duckdb_v2_logical_type_handle types[3] = {make_enum(factory, n), NULL, NULL};
        duckdb_v2_identifier_t canonical = {"ENUM", 4}, named = {"named", 5};
        CHECK(duckdb_v2_factory_create_type_with_alias(factory, types[0], &canonical, &types[1], NULL));
        CHECK(duckdb_v2_factory_create_type_with_alias(factory, types[0], &named, &types[2], NULL));
        const char *kinds[] = {"plain", "canonical_alias", "named_alias"};
        for (size_t kind = 0; kind < 3; ++kind) {
            duckdb_v2_identifier_t name;
            CHECK(duckdb_v2_logical_type_get_name(types[kind], &name, NULL));
            if (name.len != (kind == 2 ? 5 : 4)) exit(2);
            for (int op = 0; op < 3; ++op) {
                for (int sample = 0; sample < 5; ++sample) {
                    double start = now();
                    for (size_t i = 0; i < iterations; ++i) {
                        duckdb_v2_logical_type_handle copy = NULL;
                        if (op == 0) {
                            duckdb_v2_identifier_t actual_name;
                            bool has_alias = true;
                            CHECK(duckdb_v2_logical_type_get_name(types[kind], &actual_name, NULL));
                            if (actual_name.len == canonical.len && !memcmp(actual_name.ptr, canonical.ptr, canonical.len)) {
                                CHECK(duckdb_v2_factory_create_type_with_alias(factory, types[kind], &canonical, &copy, NULL));
                                CHECK(duckdb_v2_logical_type_is_equal(types[kind], copy, &has_alias, NULL));
                            }
                            if (has_alias != (kind != 0)) exit(3);
                        } else if (op == 1) {
                            idx_t length;
                            CHECK(duckdb_v2_logical_type_to_text(types[kind], NULL, 0, &length, NULL));
                            if (length != (kind == 1 ? 4 : kind == 2 ? 5 : n == 1 ? 14 : n == 1000 ? 11894 : 1388894)) exit(4);
                        } else {
                            CHECK(duckdb_v2_logical_type_copy(types[kind], &copy, NULL));
                        }
                        if (copy) CHECK(duckdb_v2_logical_type_destroy(&copy));
                    }
                    double elapsed = now() - start;
                    printf("%zu,%s,%s,%d,%zu,%.9f,%.2f\n", n, kinds[kind], op == 0 ? "exact_discriminator" : op == 1 ? "text_length" : "copy", sample + 1, iterations, elapsed, elapsed * 1e9 / iterations);
                    fflush(stdout);
                }
            }
        }
        for (size_t i = 0; i < 3; ++i) CHECK(duckdb_v2_logical_type_destroy(&types[i]));
    }
    CHECK(duckdb_v2_instance_destroy(&instance));
    CHECK(duckdb_v2_environment_destroy(&env));
    return 0;
}
