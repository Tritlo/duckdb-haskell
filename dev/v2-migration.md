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
integration. Custom file-system and log-storage registration are absent from V2.
Use native V2 errors and ownership rules. Do not convert retained API handles
into V2 handles.

## Contracts

- `open` owns a private environment and instance. Explicit instances can share
  connections. Close children before their native owners.
- `execute` drains V2 results so side effects complete before it returns.
- Default cursors materialize native chunks before row delivery. Explicit
  streaming retains the one-live-result restriction of a V2 connection.
- Bound values remain owned by the statement until replacement, clear, or close.
  Value construction receives a connection. Prepared statements accept valid
  non-cacheable SQL.
- Copy borrowed schemas, strings, and diagnostics before releasing their owners.
  Apply selection vectors before reading physical vector cells.
- Catch all exceptions at native callback boundaries. DuckDB owns registered
  callback resources until it releases the registration.
- Preserve nanosecond timestamps, infinities, nested values, GEOMETRY WKB, and
  CRS metadata. Bind VARIANT values and use explicit SQL casts to read them.
  Do not substitute lossy text conversions.

## Implementation and validation

The core, conversion, scalar callback, Arrow, registration, and COPY backends
use V2. The branch is based on PR #34 at `28bacb3`.

Local checks on 2026-10-16 used Nix GHC 9.14.1 on Linux x86_64 and the pinned
`2.0.0-dev0` native library:

- All 484 high-level, 97 retained-API, 19 raw V2, and six DataFrame tests pass.
  The high-level total includes the registration and COPY fixtures.
- The default leak suite passes with stable thread counts and RSS inside its
  32 MB allowance. It includes 1,000 callback cycles, 700 cancellations, and
  1,000 new-type cycles.
- All seven registration and five COPY cases pass under Valgrind with
  `--fair-sched=yes`. Both runs report zero errors and no definite, indirect,
  or possible lost bytes.
- Both packages build with `-Wall -Werror`, and Haddock builds. Generated
  declarations still produce unresolved-link and ambiguity warnings.
- Regeneration matches all four stored checksums. The coverage audit verifies
  548 retained and 582 V2 functions and all callbacks. Generated files stay
  outside Git.
- Both extracted source packages install without LLVM or hs-bindgen. Separate
  static and dynamic Haskell consumers pass with the loader environment unset.
  They exercise both raw APIs, shared instances, typed registration, and COPY.
  All installed headers and the license match the package inputs. Source
  archives include generated bindings and exclude maintainer tools.
- Fourmolu and cabal-gild checks pass. The release script passes nine isolated
  mock cases after the tool move. No Hackage upload ran.

The unused private VARIANT payload parser and its separate suite are removed.
The V2 boundary retains SQL NULL handling and explicit unsupported errors.

The platform matrix remains for CI. Quack runtime tests need local extension
fixtures. Managed extension targets compile, but loading a Haskell shared
library as a native extension remains separate packaging work. The advanced
opaque equality callback remains available through the raw API.

## Native limitations

These are limits of the pinned native API. They are not missing generated
bindings. The migration uses the current API. It does not add native symbols.

- The pinned prepared API cannot plan some expanding statements, including
  dynamic PIVOT. Direct `query_` and `execute_` use native statement execution
  and support these statements.
- Non-NULL VARIANT results cannot expose their structural payload through V2.
  Use an explicit SQL cast or field extraction to select supported result types.
  Binding VARIANT values and reading SQL NULL VARIANT values remain supported.
- V2 cannot register custom log storage or a custom file system. Log-storage
  registration raises `SQLError`. Native callback logging remains available.
- V2 rejects fixed ARRAY types with zero elements. Empty LIST values remain
  supported. The binding must not change one type into the other.
- Direct V2 GEOMETRY type construction requires a recognized CRS or a complete
  definition. An unknown name raises an error. Do not silently drop CRS metadata
  when `ignore_unknown_crs` is enabled.
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
