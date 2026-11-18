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

Each scalar or aggregate registration opens its own private helper instance
on demand and reuses it across calls. Each VARIANT vector uses a separate
connection and decoder state. DuckDB releases the helper when it releases the
last reference to the callbacks. See Note [Function argument readers] in
`Database.DuckDB.Simple.Typed`.

GEOMETRY has no committed vector layout. The reader casts each GEOMETRY vector
to BLOB with the factory of the instance, which has only the built-in casts,
and copies the WKB rows. Extension scalar and aggregate functions use the
private instance factory for this cast and for type metadata. An aggregate
result encoder can borrow the same factory. Other extension
callbacks write each row to a BLOB vector with `duckdb_v2_vector_set_value`
and the callback context. This operation uses the built-in default cast.

Parameters construct recognized GEOMETRY CRS names and full definitions
through the V2 API. For an unknown custom label, they read a SQL-derived type
with `ST_SetCRS` on a helper connection when the parameter needs it. This
includes nested types, typed NULLs, empty collections, and inactive UNION
members. There is no permanent cache of custom CRS types or connection option
for a CRS list. Blank labels and labels with NUL fail at parameter use.
Helper queries for CRS types and VARIANT do not interrupt the caller's active
stream. The backend does not read private payload layouts or convert handles
between the retained and V2 APIs.

`Database.DuckDB.Simple.Streaming` provides V2 row and Arrow streaming. The
advanced opaque equality callback remains available through the raw API.
[The extension recipe](extensions/README.md) builds and loads a Haskell
extension.

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
- Typed registrations and direct `logicalTypeFromRep` calls require a
  recognized GEOMETRY CRS or a complete definition. Query reads retain custom
  CRS labels; plain GEOMETRY callback arguments lose their label through the
  cast to the declared type. Reject native type construction that drops CRS
  metadata under `ignore_unknown_crs`.
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
