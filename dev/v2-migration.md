# duckdb-simple V2 migration

This document records the implementation scope, contracts, and validation.
The branch is `feature/duckdb-simple-v2`. Its pull request targets
`spike/hs-bindgen-2.0` (PR #34).

## Scope

Move the default `duckdb-simple` backend to the generated V2 client API.
Preserve query, binding, callback, Arrow, and cleanup behavior where the pinned
V2 API provides the required operations. Add public support for its available
client facilities. Report unavailable operations explicitly. Keep generated
bindings outside Git. Release source archives must build without LLVM or the
generator.

The public additions include shared environments and instances, attachment
management, option discovery, tokenization, advanced function signatures,
aggregate/table/cast registration, COPY FROM and advanced COPY TO operations,
custom types, replacement scans, file operations, callback logging, and Arrow
integration. The October 20 API also provides log sinks, custom options, macros,
pragmas, query metrics, and function documentation. Custom file-system
registration remains absent from V2.
Use native V2 errors and ownership rules. Do not convert retained API handles
into V2 handles.

## Contracts

- `open` owns a private environment and instance. Explicit instances can share
  connections. Close children before their native owners.
- `execute` drains V2 results so side effects complete before it returns.
- Default cursors materialize native chunks before row delivery. Explicit
  streaming retains the one-live-result restriction of a V2 connection.
- Bound values remain owned by the statement until replacement, clear, or close.
  Value construction borrows a factory from its live connection or callback.
  Prepared statements accept valid
  non-cacheable SQL.
- Copy borrowed schemas, strings, and diagnostics before releasing their owners.
  Apply selection vectors before reading physical vector cells.
- Catch all exceptions at native callback boundaries. DuckDB owns registered
  callback resources until it releases the registration.
- Preserve nanosecond timestamps, infinities, nested values, GEOMETRY WKB, and
  CRS metadata. Bind and decode VARIANT values without lossy text conversions.
  Use public SQL metadata and native casts when direct V2 access is incomplete.
  Keep helper queries off the caller's active connection.

## Implementation and validation

The current source pin is `d54cbbe3df17e8e5ce68312f483b4155836112aa` from
2026-10-20. Both client modules regenerate with the existing naming policy.
The retained API has 548 functions and 508 ABI assertions. V2 has 620 functions
and 642 ABI assertions. All V2 functions are marked unstable. Runtime validation
of this update is in progress. The checks below describe the earlier pin.

The core, conversion, scalar callback, Arrow, registration, and COPY backends
use V2. The branch is based on PR #34 at `28bacb3`.

Local checks on 2026-10-16 used Nix GHC 9.14.1 on Linux x86_64 and the pinned
`2.0.0-dev0` native library:

- All 608 functional tests pass: 486 high-level, 97 retained-API, 19 raw V2,
  and six DataFrame tests. The high-level total includes registration, COPY,
  automatic VARIANT decoding, and configured custom CRS fixtures.
- The default leak suite passes with stable thread counts and RSS inside its
  32 MB allowance. It includes 1,000 callback cycles, 700 cancellations, and
  1,000 new-type cycles.
- All seven registration and five COPY cases pass under Valgrind with
  `--fair-sched=yes`. Both runs report zero errors and no definite, indirect,
  or possible lost bytes. A separate 15-case Valgrind run covers VARIANT scalar
  precision, nested values, COPY callbacks, configured CRS, active streams, and
  cursor-error cleanup. It reports the same zero-error and leak results.
- Both packages build with `-Wall -Werror`, and Haddock builds. Generated
  declarations still produce unresolved-link and ambiguity warnings.
- Regeneration matches all four stored checksums. The coverage audit verifies
  548 retained and 582 V2 functions and all callbacks. Generated files stay
  outside Git.
- Both extracted source packages install without LLVM or hs-bindgen. Separate
  static and dynamic Haskell consumers pass with the loader environment unset.
  They exercise both raw APIs, shared instances, typed registration, COPY,
  automatic nested VARIANT decoding, and configured custom CRS names with
  quotes and Unicode. VARIANT checks retain integer and decimal precision,
  interior NUL bytes, NULLs, array order, and the native object-key order.
  All installed headers and the license match the package inputs. Source
  archives include generated bindings and exclude maintainer tools and the
  removed streaming module. The installed package does not expose that module.
  Installed source digests match both final archives. All 103 archived files
  match the frozen checkout.
- Fourmolu and cabal-gild checks pass. The release script passes nine isolated
  mock cases after the tool move. No Hackage upload ran.

The private VARIANT payload parser and its separate suite are removed.
VARIANT decoding uses batched SQL type metadata and native casts. Object keys
use DuckDB's native lexicographic order. Arrays retain element order. A helper
connection also caches SQL-derived VARIANT and configured GEOMETRY types.
Configured custom CRS names remain available in nested parameters, including
typed NULLs and empty collections. The helper does not interrupt the caller's
active stream. The backend does not read private payload layouts or convert
handles between the retained and V2 APIs.

`Database.DuckDB.Simple.Streaming` provides V2 row and Arrow streaming.
The former `Database.DuckDB.Simple.Deprecated.Streaming` module is removed.

The platform matrix remains for CI. Quack runtime tests need local extension
fixtures. Managed extension targets compile, but loading a Haskell shared
library as a native extension remains separate packaging work. The advanced
opaque equality callback remains available through the raw API.

## Native limitations

These are limits of the pinned native API. They are not missing generated
bindings. The migration uses the current API. It does not add native symbols.

- Prepared expanding statements, including dynamic PIVOT, cannot have parameters
  or require a cacheable plan. Their schema becomes available during execution.
  The backend fetches before reading schema, including Arrow results.
- V2 cannot register a custom file system. Native log-sink registration is
  available. Log callbacks can run concurrently and must not log or change the
  logging configuration. Native logging contains and discards callback errors.
- V2 rejects fixed ARRAY types with zero elements. Empty LIST values remain
  supported. The binding must not change one type into the other.
- Native GEOMETRY collections require one Z/M layout for all members, including
  empty members. Mixed-layout WKB remains valid for the standalone codec, but
  native import raises `SQLError`. Do not promote or remove coordinates.
- Bare expression statements use the CLI's shell grammar extension. Client SQL
  uses an explicit `SELECT`.
- Unconfigured GEOMETRY CRS names use direct V2 type construction. This requires
  a recognized CRS or a complete definition. Configure custom names through
  `connectionGeometryCRS` to preserve them through the SQL-derived type cache.
  Do not silently drop CRS metadata when `ignore_unknown_crs` is enabled.
- V2 metadata cannot perform scoped collation lookup. That operation raises
  `SQLError`. Table, view, index, sequence, and type lookups use SQL metadata.

## Validation commands

Run generation and coverage checks, then build all components in
`dev/nix/shell.nix`. Run all existing integration, DataFrame, and leak
suites. Check callback exceptions, async cancellation, selection vectors,
shared lifetimes, registration cleanup, and materialized cursor behavior.
Run Valgrind with `--fair-sched=yes` for V2 tests. Run both formatters and
Haddock. Verify extracted source archives and installed consumers without the
generator. Record native preview limitations separately from missing bindings.

Use `GHC_ENVIRONMENT=- cabal exec --with-compiler=ghc
--builddir=dist-newstyle-bindgen -- ghc ...` for isolated compiler probes inside
the Nix shell. The default global package environment can contain stale units.
