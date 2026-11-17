# VARIANT profile protocol

The baseline is commit `e4d9c8434db0e50260e3f5ce6ff967606d9d4967`.
The new measurements compare the V2 baseline and candidate. Retained V1 was
not rerun. The historical V1/V2 matrix uses DuckDB 2.0.0.
The 2026-11-16 wall-clock comparisons in `dev/v2-migration.md` are historical.
They do not measure this baseline.

`pr35-variant-profile.hs` constructs a table before the warm-up.
It then reads 10,000 rows seven times with DuckDB `threads=1` and GHC
`+RTS -N1 -T`. Each action checks the row count and the exact `FieldValue`
of every row. Connection setup, table construction, warm-up, and explicit
major collections are outside the timed interval. Ordinary collection during
the action is included. The allocation delta includes the final major
collection so the RTS has recorded the action's allocations.

The shapes are BIGINT scalars, two-element arrays, two-field objects, objects
with empty, embedded NUL, and case-sensitive keys, and mixed scalars, arrays, nested objects,
and SQL NULL. The driver checks integer constructors as well as payloads.
The regression suite covers the other exact scalar types, embedded NUL keys,
empty containers, VARIANT NULL, active streams, and registered user casts. The cancellation
suite checks native execution and streaming cancellation; it does not directly
exercise an interrupted VARIANT helper query. A benchmark does not replace these checks.

Build and run in the pinned `dev/nix/shell.nix` environment. Record the compiler,
native library path and SHA256, executable and driver SHA256, Cabal plan,
source commit, options, CPU affinity, and host load with the raw samples.
Run each build, benchmark, and test under `heavy-management/bin/capped` with a
memory cap of at most 16 GiB. Run one such job at a time across worktrees.

```sh
cabal build duckdb-simple:bench:duckdb-simple-bench \
  duckdb-simple:test:duckdb-simple-test --offline --enable-optimization=2 --disable-documentation -j1
GHC_ENVIRONMENT=- cabal exec -- ghc -O2 -threaded -rtsopts -Wall -Werror \
  -hide-all-packages -package base \
  -package-id duckdb-simple-1.0.0.0-inplace -package-id text-2.1.3-e2b1 \
  dev/benchmarks/pr35-variant-profile.hs -outputdir /tmp/variant-profile \
  -o /tmp/variant-profile/probe
/tmp/variant-profile/probe scalar 10000 7 +RTS -N1 -T
```

Repeat the final command for `array`, `object`, `unusual`, and `mixed`.
Use Callgrind on smaller checked runs to attribute instructions to SQL
metadata execution, statement parsing and preparation, native casts, and
Haskell decoding. Whole-process counts include setup and shutdown. Compare
matched row counts and separate this scope from the warm wall-clock actions.
Inclusive instruction counts overlap and must not be added.

Select a change after the profile. Keep the helper connection and built-in
instance casts. Use public APIs. Do not read private VARIANT payload layouts.
Do not add a cache without a measured need and a stated memory bound.

## Baseline result

The [samples](pr35-variant-baseline.csv) contain 35 checked actions, with
350,000 rows in total. The [metadata](pr35-variant-baseline.json) records the
actual plan, native library, executable hashes, and instruction attribution.
The build used 1,507 MiB at its peak. The measured process started at load
averages 1.43, 0.86, and 0.70. CPU affinity covered all 24 allowed CPUs.
No exclusive CPU reservation was made.

| Shape | Median ms | Median allocated bytes |
| --- | ---: | ---: |
| Scalar | 42.669 | 36,924,992 |
| Array | 399.730 | 131,930,328 |
| Object | 426.751 | 172,097,840 |
| Unusual keys | 1,503.923 | 280,296,248 |
| Mixed | 289.004 | 110,190,296 |

A scalar Callgrind run read 3,000 rows twice, including the warm-up.
Its whole-process count was 643,415,499 instructions. Native inclusive counts
were 149,940,554 for statement execution, 93,371,250 for value extraction,
and 34,624,070 for value casts. These include setup and overlap. They do not
form a partition of decoder costs. The profile does not separate metadata
execution from the outer query or fixture execution. Repeated native planning
is a candidate for a focused follow-up measurement.

Callgrind reported a `brk segment overflow` warning. The process completed
with all value checks. Its inferred Haskell call graph has cycles that make
inclusive Haskell totals exceed the process total. Those totals cannot support
a performance claim. Exclusive `threadPaused` cost was 17,263,226 instructions
(2.68% of the whole process). Haskell allocation is measured separately in the
native runs above.

The baseline has no library source changes. A candidate is to replace the existing parsed
metadata statement with one prepared statement. The public API can report
whether that plan is reusable. Measure that property and the resulting cost
before retaining the change. The unusual-key extraction statement binds
constant keys, so it remains outside this candidate.

## Metadata plan check

The [plan probe](pr35-variant-plan.hs) prepares the exact metadata SQL through
the public API. Its native `prepared_statement_reuses_plan` flag was true
before binding and after each binding and execution. List lengths were
0, 1, 2, 2, 4,096, 1, and 0. Values included NULL, integers, text, arrays,
objects, and empty and case-sensitive object keys. Each result matched a fresh
statement with the same parameters. Each execution reached end of result.
The process peaked at 430 MiB under a 2 GiB cap.

The existing decoder retains a parsed statement to avoid repeated parsing.
The source gives no reason to require a new bind and plan for every metadata
execution. This probe establishes that the engine can reuse this metadata
plan. It does not yet measure a changed decoder.

## Prepared metadata result

The selected change retains one prepared metadata statement in the existing
slot. It releases the parsed AST after preparation. The extraction statement
and cast types keep their existing behavior. The helper connection and the
instance factory remain the same. A failed preparation leaves the slot empty.
The cache owns a new prepared statement before temporary handles are closed.

The [alternating samples](pr35-variant-prepared.csv) contain 70 actions and
700,000 checked rows. Each process performs one warm-up and one measured
action. The first implementation alternates in each round. The
[metadata](pr35-variant-prepared.json) records the order and executable hashes.
The compiler, driver, dependency IDs, package flags, and native library match.
Only generated output paths differ between the saved Cabal plans. The
registered library paths point to the `-O2` build in both compilations.
The load average was 0.53 at the start and 0.95 at the end.

| Shape | Baseline median ms | Prepared median ms | Change |
| --- | ---: | ---: | ---: |
| Scalar | 44.550 | 38.045 | -14.6% |
| Array | 416.581 | 366.521 | -12.0% |
| Object | 444.274 | 397.035 | -10.6% |
| Unusual keys | 1,502.167 | 1,537.410 | +2.3% |
| Mixed | 298.056 | 285.068 | -4.4% |

The unusual-key result has no clear improvement. Four of its seven paired
rounds favor the candidate. These timings do not establish a regression or
an improvement for that case. Haskell allocation is nearly unchanged in all
five shapes. The change removes native planning work.

The matched scalar Callgrind whole-process count fell from 643,415,499 to
538,647,893 instructions (16.3%). Native inclusive statement execution changed
from 149,940,554 instructions to 2,136,742 instructions, plus 15,215,660 for
prepared execution. Those counts overlap other categories. They include
fixture setup and the outer query. The same Callgrind limitations apply to
both profiles. No separate additive metadata/cast/allocation breakdown is
claimed.

The existing VARIANT regressions now check failed metadata preparation,
subsequent recovery, and repeated heterogeneous and empty inputs. They cancel
materialized and streaming folds after the first decoded row, then check a
heterogeneous fold on the same connection. This exercises reuse after
post-metadata cancellation. It does not interrupt a native helper query.

To reproduce the alternating run, preserve the baseline executable before
building the candidate with the same options. Run the comparison under the
pinned shell and one capped scope:

```sh
python3 dev/benchmarks/pr35-variant-compare.py \
  /absolute/path/to/baseline /absolute/path/to/candidate /tmp/variant-comparison
```

## Validation scope

The ordinary checks passed: 835 high-level tests, 97 retained-API tests,
74 V2 tests, the non-threaded check, and the default leak suite with ten
batches. The repository requires the full ordinary suite. The existing
failure-injection fixtures now use unusual-key extraction after metadata has
been prepared. The extraction query still binds on each execution. Tests
require the reader error and check subsequent reuse; they do not replace an
error assertion with a success assertion. Metadata log assertions account for
preparation and execution.

Prepared-plan reuse changes binding-time configuration sensitivity. A lower
GLOBAL expression-depth limit can leave a prepared metadata plan usable.
Initial preparation, native rebinds, and unusual-key extraction can still fail
at their own bind points. The README and decoder Note describe this behavior.
The runtime candidate was held fixed during the fixture updates.

Five focused cases passed under Valgrind: failed preparation and recovery,
materialized and streaming cancellation followed by cache reuse, repeated
key layouts, and later extraction failure followed by reuse. Valgrind reported
zero errors and zero definite, indirect, or possible lost bytes. It reported
4,621,964 reachable bytes in 30 blocks. Peak scoped memory was 1,773 MiB.
The sustained `all 100` leak run was not repeated. Fourmolu checked all touched
Haskell files, and cabal-gild checked both package files.

The implementation is commit `12992f47`. Its exact runtime source SHA256 is
recorded with the comparison metadata. The measured runtime source stayed
unchanged during the regression-fixture updates.
