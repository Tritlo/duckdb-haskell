# duckdb-simple V2 migration

This document records the scope, contracts, and native limits of the V2
backend.

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
custom types, replacement scans, file operations, callback logging, log sinks,
custom options, macros, pragmas, query metrics, function documentation, and
Arrow integration. V2 has no custom file-system registration.
Use native V2 errors and ownership rules. Do not convert retained API handles
into V2 handles.

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
SQL type metadata, native extraction, and casts. Object extraction binds exact
keys, including empty strings and interior NUL bytes. It does not cast objects
to MAP values. Object keys use DuckDB's native lexicographic order. Arrays
retain element order. Native factories construct VARIANT types directly.

A helper connection caches SQL-derived GEOMETRY types for the CRS names in
`connectionGeometryCRS`. These names remain available in nested parameters,
including typed NULLs and empty collections. Helper queries do not interrupt
the caller's active stream. The backend does not read private payload layouts
or convert handles between the retained and V2 APIs.

`Database.DuckDB.Simple.Streaming` provides V2 row and Arrow streaming. The
advanced opaque equality callback remains available through the raw API.

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
- V2 metadata cannot look up collations, so `CatalogEntryType` has no
  collation kind. Table, view, index, sequence, and type lookups use SQL
  metadata.
  These lookups accept a top-level schema identifier. A dot in that identifier
  is literal. V2 table descriptions accept at most three qualified name parts,
  so they cannot describe tables in nested schemas.

## Validation commands

Run generation and coverage checks, then build all components in
`dev/nix/shell.nix`. Run all existing integration, DataFrame, and leak
suites. Check callback exceptions, async cancellation, selection vectors,
shared lifetimes, registration cleanup, and materialized cursor behavior.
Run Valgrind with `--fair-sched=yes` for V2 tests. Run both formatters and
Haddock. Verify extracted source archives and installed consumers without the
generator. Record native API limitations separately from missing bindings.

Use `GHC_ENVIRONMENT=- cabal exec --with-compiler=ghc
--builddir=dist-newstyle-bindgen -- ghc ...` for isolated compiler probes inside
the Nix shell. The default global package environment can contain stale units.
