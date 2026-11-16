# duckdb-simple V2 backend

This document describes the contracts, design, and native limits of the
`duckdb-simple` backend on the generated V2 client API.

## Overview

The backend uses only V2 handles. It does not convert retained API handles into
V2 handles. Upstream marks the complete V2 API as unstable, so the backend
requires exactly the DuckDB release of the pinned headers. Creating an
environment compares the library version with the header version and raises
`SQLError` on a mismatch.

The public API includes shared environments and instances, attachment
management, option discovery, tokenization, advanced function signatures,
aggregate, table, and cast registration, COPY FROM and COPY TO operations,
custom types, replacement scans, file operations, callback logging, log sinks,
custom options, macros, pragmas, query metrics, function documentation, and
Arrow integration. Operations that V2 does not provide raise `SQLError` or are
absent.

## Contracts

- `open` owns a private environment and instance. Explicit instances can share
  connections. Close children before their native owners.
- `execute` drains V2 results so side effects complete before it returns.
- Ad-hoc helpers parse all statements before execution. A syntax error prevents
  earlier statements from running. The retained native query API can execute
  earlier statements before reporting a later syntax error.
- Default cursors materialize native chunks before row delivery. Explicit
  streaming retains the one-live-result restriction of a V2 connection.
- Bound values remain owned by the statement until replacement, clear, or close.
  Value construction borrows a factory from its live connection or callback.
  Prepared statements accept valid non-cacheable SQL.
- Copy borrowed schemas, strings, and diagnostics before releasing their owners.
  Apply selection vectors before reading physical vector cells.
- Catch all exceptions at native callback boundaries. DuckDB owns registered
  callback resources until it releases the registration.
- Preserve nanosecond timestamps, infinities, nested values, GEOMETRY WKB, and
  CRS metadata. Bind and decode VARIANT values without lossy text conversions.
  Use public SQL metadata and native casts when direct V2 access is incomplete.
  Keep helper queries off the caller's active connection.

## Design notes

The pinned V2 API has no structural VARIANT accessor. The decoder uses batched
SQL type metadata and native casts. A native cast to a STRUCT with VARIANT
fields extracts the fields of the objects that have the same keys. The STRUCT
cast path excludes empty keys, keys with NUL bytes, and keys equal after ASCII
case folding. For those objects, an extraction query binds the exact keys.
The decoder does not cast objects to MAP values. Object keys use DuckDB's
native lexicographic order. Arrays retain element order. Native factories
construct VARIANT types directly.

GEOMETRY has no committed vector layout. The reader casts each GEOMETRY vector
to BLOB with the factory of the instance, which has only the built-in casts,
and copies the WKB rows.

A connection makes the CRS names in `connectionGeometryCRS` available to
nested parameters, including typed NULLs and empty collections. When the
connection opens, it constructs each name directly. DuckDB keeps recognized
names and complete definitions. For each other name, the connection reads a
SQL-derived type with `ST_SetCRS` before the caller can run a statement. The
connection caches each type, and a bind copies it. A default connection runs no
query. Helper queries for VARIANT do not interrupt the caller's active stream.
The backend does not read private payload layouts or convert handles between
the retained and V2 APIs.

`Database.DuckDB.Simple.Streaming` provides V2 row and Arrow streaming. The
advanced opaque equality callback remains available through the raw API.

## Native limitations

The pinned native API has these limits. The generated bindings expose the
available API. They do not add native symbols.

- Prepared expanding statements, including dynamic PIVOT, cannot have parameters
  or require a cacheable plan. Their schema becomes available during execution.
  The backend fetches before reading schema, including Arrow results.
- V2 cannot register a custom file system. Native log-sink registration is
  available. Log callbacks can run concurrently and must not log or change the
  logging configuration. Native logging contains and discards callback errors.
- DuckDB 2.0 rejects an ARRAY type with zero elements, including in SQL. Empty
  LIST values remain supported. The binding must not change one type into the other.
- Native GEOMETRY collections require one Z/M layout for all members, including
  empty members. Mixed-layout WKB remains valid for the standalone codec, but
  native import raises `SQLError`. Do not promote or remove coordinates.
- Bare expression statements use the CLI's shell grammar extension. Client SQL
  uses an explicit `SELECT`.
- Unconfigured GEOMETRY CRS names use direct V2 type construction. This requires
  a recognized CRS or a complete definition. Configure custom names through
  `connectionGeometryCRS` to preserve them through the SQL-derived type cache.
  Do not silently drop CRS metadata when `ignore_unknown_crs` is enabled.
- `CatalogEntryType` has no collation kind. `pragma_collations()` gives the
  name of each collation, but not its catalog and its schema. Table, view,
  index, sequence, and type lookups use SQL metadata.
  These lookups accept a top-level schema identifier. A dot in that identifier
  is literal. V2 table descriptions accept at most three qualified name parts,
  so they cannot describe tables in nested schemas.

## Validation

After a backend change, run the full suites, the leak suite, the DataFrame
suite, and Haddock. Run Valgrind with `--fair-sched=yes` for the V2 tests.
Check callback exceptions, async cancellation, selection vectors, shared
lifetimes, registration cleanup, and materialized cursor behavior.

## Measured performance

The 2026-11-16 comparison uses V1 bindings at `08feb05d`, the V2 baseline at
`9f85ab5a`, and the selected V2 implementation at `6949a6b3`. Text, GEOMETRY,
and BIGNUM collection reads benefit from V2 in these cases. Parameter queries
and VARIANT reads remain more costly than V1. Results vary by workload.

The [raw samples](benchmarks/pr35-release.csv) contain 483 measurements:
23 cases, three implementations, and seven runs after one warm-up. The
[metadata](benchmarks/pr35-release.json) records source commits, executable
hashes, case order, and the protocol. All implementations use the same
[harness](https://github.com/Tritlo/duckdb-haskell/blob/6949a6b30e4daba22e2c68f9941b6d69c7061306/duckdb-simple/bench/Main.hs), SHA256
`2085cafc2e1695281a6bf8114cb220139b33306260d83535446173dd1905cedd`, GHC 9.14.1
at `-O2`, and the same Nix DuckDB 2.0.0 library. Runtime options are
`+RTS -N1 -T`; DuckDB uses `threads=1`.

Each process was pinned to CPU 23 on a WSL2 host with an AMD Ryzen 9 7950X.
The host also ran concurrent compilation. One-minute load ranged from 6.55 to
7.33 across 24 allowed CPUs. CPU pinning did not establish exclusive use.
Treat small elapsed-time differences as provisional.

Elapsed time includes query preparation, binding, execution, conversion, and
checksum forcing. Connection and fixture setup, warm-up, and explicit major
GCs are outside the timed interval. Normal GC during an action is included.
All samples passed the harness checks. The tables give medians. Collection
counts are elements in one result row. Parameter counts are singleton queries.
Other counts are result rows.

| Workload | Count | V1 ms | V2 baseline ms | Selected V2 ms |
| --- | ---: | ---: | ---: | ---: |
| eager | 100,000 | 45.756 | 38.634 | 41.061 |
| fold | 100,000 | 8.600 | 9.610 | 9.828 |
| scalar | 100,000 | 44.597 | 51.584 | 55.487 |
| text | 100,000 | 620.210 | 84.084 | 87.379 |
| timestamp | 100,000 | 287.126 | 310.615 | 275.354 |
| parameters | 1,000 | 325.477 | 387.012 | 392.733 |
| timestamp-parameters | 1,000 | 389.707 | 443.006 | 454.599 |
| variant | 10,000 | 58.518 | 79.509 | 98.947 |
| variant-parameters | 1,000 | 417.499 | 877.543 | 805.890 |
| variant-objects | 2,000 | 26.045 | 76.709 | 73.069 |
| variant-list | 20,000 | 100.370 | 311.236 | 284.775 |
| geometry | 10,000 | 54.564 | 2.541 | 3.407 |
| geometry-parameters | 1,000 | 476.783 | 576.630 | 643.475 |
| geometry-typed | 10,000 | 57.632 | 7.485 | 8.472 |
| geometry-typed-parameters | 1,000 | 332.383 | 431.178 | 421.274 |
| list-bigint | 20,000 | 4.719 | 5.775 | 4.364 |
| list-bignum | 20,000 | 114.497 | 70.427 | 12.029 |
| array-bignum | 20,000 | 117.397 | 69.721 | 21.065 |
| map-bignum | 20,000 | 54.162 | 41.027 | 26.457 |

Allocated MB means 1,000,000 bytes of Haskell allocation. Each value is a
`GHC.Stats.allocated_bytes` delta around the action and the following major GC.
It includes small measurement overhead. It excludes native allocation and RSS.
Counts match the elapsed-time table.

| Workload | V1 MB | V2 baseline MB | Selected V2 MB |
| --- | ---: | ---: | ---: |
| eager | 46.47 | 45.03 | 45.03 |
| fold | 96.86 | 97.81 | 97.81 |
| scalar | 86.54 | 84.62 | 84.62 |
| text | 92.05 | 76.22 | 76.22 |
| timestamp | 540.98 | 539.54 | 539.54 |
| parameters | 8.73 | 24.33 | 24.34 |
| timestamp-parameters | 13.76 | 27.77 | 27.79 |
| variant | 16.42 | 32.58 | 35.03 |
| variant-parameters | 18.99 | 65.55 | 60.11 |
| variant-objects | 7.90 | 24.58 | 25.88 |
| variant-list | 1620.42 | 63.77 | 68.55 |
| geometry | 7.58 | 7.08 | 7.08 |
| geometry-parameters | 11.44 | 35.91 | 35.92 |
| geometry-typed | 26.14 | 25.64 | 25.64 |
| geometry-typed-parameters | 18.69 | 41.99 | 41.97 |
| list-bigint | 14.14 | 14.15 | 15.74 |
| list-bignum | 32.96 | 30.46 | 32.05 |
| array-bignum | 34.08 | 33.03 | 33.18 |
| map-bignum | 56.61 | 54.11 | 54.89 |

VARIANT metadata reads use borrowed result vectors before their chunks are
released. VARIANT types use the native type-name operation. Singleton INT64
and UINT64 casts use native value getters.

The scalar VARIANT median rose from 79.509 to 98.947 ms in this matrix. The
separate confirmation below did not reproduce that increase. Parameter,
object, and nested-list VARIANT medians fell by about 5–9%. These shared-host
measurements do not establish stable gains of that size. Parameter allocation fell, while object and nested-list
allocation rose. The selected decoder still uses helper SQL for exact scalar
tags and object keys, batched by nesting level. It preserves native tags and existing
payload evaluation. The changes do not remove helper execution or per-query preparation.

### Scalar VARIANT confirmation

A separate [confirmation](benchmarks/pr35-variant-confirmation.csv) used three
processes per implementation, with one warm-up and seven samples per process.
Each action read 10,000 scalar VARIANT rows. The same harness, compiler, native
library, affinity, and thread settings applied. One-minute host load was
1.35–1.40. The table gives pooled medians across 21 samples per implementation;
process medians and execution order are in the
[metadata](benchmarks/pr35-release.json).

| Implementation | Median ms | Allocated MB |
| --- | ---: | ---: |
| V2 baseline | 51.259 | 32.58 |
| Cleanup control | 49.751 | 33.15 |
| Before collection folds | 43.067 | 33.13 |
| Selected V2 | 43.627 | 35.03 |

The earlier timing increase did not reproduce. This comparison does not identify
its cause. Selected Haskell allocation remains higher than baseline. Separate
whole-process Callgrind runs at 1,000 rows report 854,648,092 instructions for
the baseline and 791,665,132 for selected V2, a 7.37% reduction. Each includes
startup, fixture setup, one warm-up, and seven runs. The metadata records all
four instruction profiles.

### Larger collections and isolated phase measurements

The same final matrix includes 100,000-element collections:

| Collection | V1 ms | Baseline ms | Selected ms | V1 MB | Baseline MB | Selected MB |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| list-bigint | 26.355 | 27.472 | 25.850 | 72.44 | 72.45 | 80.41 |
| list-bignum | 578.233 | 320.372 | 83.618 | 174.49 | 154.05 | 162.01 |
| array-bignum | 597.594 | 343.121 | 87.137 | 180.09 | 166.85 | 167.61 |
| map-bignum | 312.916 | 236.842 | 180.235 | 294.66 | 274.22 | 278.02 |

These measurements show finite-size scaling. They do not establish an
asymptotic complexity bound. The collection folds keep a constant stack at
safe foreign calls and preserve native read order. They allocate additional
Haskell list cells.

A separate [paired phase](benchmarks/pr35-collection-phase.csv) compares the
collection folds with the preceding implementation. Both sides include the
other three optimizations and the cleanup fixes. Reproduce the later arm at
`6949a6b3`. For the earlier arm, use that commit with `6386befa` reverted.
Keep the harness, compiler, and native library fixed. Each case has 100,000
elements and seven samples:

| Collection | Before ms | After ms | Before MB | After MB |
| --- | ---: | ---: | ---: | ---: |
| list-bignum | 283.978 | 56.215 | 154.05 | 162.01 |
| array-bignum | 295.232 | 57.824 | 166.85 | 167.61 |
| map-bignum | 170.063 | 115.609 | 274.22 | 278.02 |

In that phase, BIGNUM list and ARRAY reads take about one fifth of the earlier
time. MAP time falls by about 32%. Haskell allocation rises. These are separate
phase measurements; the final matrix above ran under higher host load.

For a 2,000-element BIGNUM list, a paired Callgrind process reports total
instructions falling from 757,816,390 to 353,277,317. Self instructions in
`threadPaused` fall from 426,850,641 to 21,128,614. This is consistent with
reduced stack scanning for that workload. Each profile includes startup, fixture setup,
one warm-up, and seven runs. It does not measure only the timed action.
The [metadata](benchmarks/pr35-release.json) records the profile sources and
counts. Inclusive native call costs overlap and cannot be added as disjoint
shares of elapsed time.

For a 100,000-element nested VARIANT list, peak RSS was 441,728 KiB for the
cleanup control and 383,868 KiB for the selected implementation. There was
one process per implementation. These are whole-process peaks, including
Haskell and native memory, setup, warm-up, and all runs. They do not isolate
native allocation.

The original-to-metadata phase also includes the `acquireItems` cleanup fix.
Use isolated controls when attributing its costs. Nested-list controls at
2,000 and 20,000 elements showed about 56 additional allocated bytes per
element in those cases. This is not a general allocation rule.

`executeMany` keeps each needed parameter type for one call. Concurrent DDL
does not refresh that cached type for later rows. Use a new call after changing
a target type. This contract avoids repeated type binding within a call.

Release validation passed 793 high-level tests, 97 retained-API FFI tests, and
74 V2 FFI tests, plus the leak and non-threaded suites. One Valgrind process
ran 11 focused tests. It reported no errors, no definitely or indirectly lost
bytes, 384 possibly lost bytes, and 4,623,380 still reachable bytes. The combined
stack passed 817 high-level tests, the same 97 and 74 FFI tests, the leak and
non-threaded suites, and 19 extension tests. Source/archive checks and four
generated hash checks per worktree passed. Public Haddock indices were present.
These counts describe regression coverage for the reviewed implementations.
