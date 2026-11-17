# Exact ENUM alias detection

The public V2 name getter maps an absent alias and the canonical-name alias to
the same name. The exact discriminator creates a temporary canonical-name
alias and compares native equality. The driver checks that complete operation,
including the name branch, for plain, canonical-aliased, and named-aliased ENUMs.

Five-sample native medians with the pinned DuckDB 2.0.0 library:

| Labels | Plain discriminator | Canonical alias | Named alias | Plain shared copy | Plain text sizing |
| --- | ---: | ---: | ---: | ---: | ---: |
| 1 | 140 ns | 40 ns | 6 ns | 12 ns | 50 ns |
| 1,000 | 57.6 us | 40 ns | 6 ns | 12 ns | 28.0 us |
| 100,000 | 9.68 ms | 41 ns | 6 ns | 12 ns | 2.95 ms |

A plain ENUM rebuilds its label lookup map when the alias changes. An ENUM
already aliased as `ENUM` shares its metadata because the alias does not change.
A named alias takes the name branch. The public type copy shares the metadata.

Text sizing still renders the full plain dictionary. It is a diagnostic,
not an exact replacement: scalar texts can be equal with or without an alias,
and extension modifiers can append text to aliases. Type ids and parameters
also do not expose alias presence. Reconstructing an ENUM from its labels still
builds a lookup map. Common-type and cast APIs specify cast behavior, not an
alias-presence predicate. The public `factory_create_type_with_alias` rejects
an empty alias name, so clearing the alias is not a fast public substitute.
Retained API handles cannot be used as V2 handles.

No production change was made. The smallest proposed upstream change is
`duckdb_v2_logical_type_has_alias(type, out_has_alias, err)`, which can call the
existing native `LogicalType::HasAlias`. The name getter can then give the alias
text. This needs an API declaration, schema and export integration where
required, generated declarations, and upstream tests. It has not been built or
benchmarked. A future Haskell adoption needs a native pin update and existing
alias, scope, cancellation, and leak regressions.

`bench.c` builds fixtures outside each measured interval. It destroys temporary
types inside each iteration, then destroys fixtures before their factory owner.
`samples.csv` has 135 raw rows. `summary.json` gives the median and range of each
cell. `provenance.json` records hashes, dynamic dependencies, and the capped run.
These native timings do not measure Haskell snapshots, ordinary label decoding,
total query latency, or cancellation latency. Earlier full-snapshot measurements
were not repeated.

Run from the repository root in the pinned shell. Set `native_lib` to the pinned
DuckDB library directory. Serialize the run through `heavy-management/bin/capped`
with a 2G memory cap and a task-specific `HEAVY_PEAKS` file.

```sh
cc -O2 -Wall -Wextra -Werror -Iduckdb-ffi/cbits \
  dev/benchmarks/pr35-enum-alias-cost/bench.c \
  -L"$native_lib" -Wl,-rpath,"$native_lib" -lduckdb -o /tmp/enum-alias-bench
/tmp/enum-alias-bench > /tmp/enum-alias-samples.csv
```

The successful run used 409 MiB peak under its 2G cap. No Haskell files changed,
so the Haskell suite and formatters were not run. CONTRIBUTING.md requires the
full build, suite, and leak test before a future implementation is submitted.
