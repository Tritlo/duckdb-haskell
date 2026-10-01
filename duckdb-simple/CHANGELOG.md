# Changelog

## 0.2.0.0

### Query execution and resource lifetime

- Add scoped Arrow batch export through `foldArrow` and `foldArrow_`. The
  callback borrows each array and its schema; the fold releases them on every
  exit path. This uses the supported schema/chunk conversion API and retains
  a materialized native result while the fold runs.

- Previously, cursors decoded rows with prepare-time column types. Parameter
  binding or schema rebinding could change those types and cause truncated
  values or invalid memory access. Cursors now read types from the executed
  result. This addresses the streaming failures in #18.
- Cursors use the supported materialized execution API. DuckDB 1.5 provides
  native streaming only through deprecated entry points, which the default
  interface does not use. Folds decode one row at a time, but native result
  memory still depends on the result size. Fetch failures now produce SQL
  errors; previously, they were indistinguishable from end of input.
- Previously, reading past EOF could execute the statement again, including
  INSERT statements. Cursors now remain exhausted until an explicit reset.
  Clearing bindings also destroys any active result.
- Previously, exceptions during result decoding could skip native destruction.
  Results, chunks, and nested logical types now have exception-safe cleanup.
  Cursor state records chunk ownership before decoding and clears it before
  destruction, so cancellation cannot cause a leak or a second destruction.
- Previously, GC could finalize a connection or statement during its last
  native call. The binding now keeps the Haskell owner alive until the call
  returns. Reads through a closed parent connection fail before native access.
- Previously, streaming rejected STRUCT and UNION columns even though the
  shared decoder supported them. Eager queries and cursors now use the same
  row decoder, including nested collections and NULLs.
- Previously, a rollback failure could replace the exception from the user's
  transaction. The original exception is now preserved. A failed commit also
  attempts rollback.

### Callbacks and native helpers

- Previously, failed scalar, COPY, or logging registration could free callback
  resources twice. Registration now transfers ownership once and releases
  resources acquired before a failure. Static destructors avoid allocating
  a separate destructor callback for each registration.
- Previously, callback closures or query state could remain live on a long-lived
  connection after replacement or execution. Cleanup now releases scalar
  worker state, COPY state, and replaced closures before connection close.
- Previously, exceptions during callback initialization or error formatting
  could escape into native code. Scalar and COPY callbacks now report SQL
  errors. Logging callbacks contain exceptions because their API has no error
  channel. This includes asynchronous exceptions raised inside a callback.
- Previously, DuckDB could skip a scalar callback when an argument was NULL.
  Callbacks now receive those arguments. Use `Maybe` to accept NULL; a
  non-nullable Haskell argument produces a conversion error.
- Previously, Word and Word64 scalar results used signed BIGINT storage and
  could overflow. They now use UBIGINT. Float callbacks preserve NaN, infinity,
  and negative zero without depending on compiler optimization rules.
- Catalog, configuration, and filesystem helpers now bracket native allocations
  on failure paths. File reads reject sizes that cannot fit a Haskell buffer.
  Unsupported catalog entry kinds fail before the native lookup.

### Value conversion

- Previously, temporal infinity could abort the process or decode as an
  unrelated finite value. `Database.DuckDB.Simple.Time` now provides `Unbounded`,
  `Date`, `LocalTimestamp`, and `UTCTimestamp` to read and bind both infinities.
  Ordinary `Day`, `LocalTime`, and `UTCTime` report a conversion error for
  infinity. Floating-point NaN and infinities remain supported.
- `FieldDate`, `FieldTimestamp`, and `FieldTimestampTZ` now hold `Unbounded`
  payloads. Wrap existing finite payloads in `Finite`. Custom `FromField`
  instances can inspect infinity before conversion. Generic composites and
  nested collections preserve infinity and NULL separately.
- Previously, binding dates outside native storage limits could abort in C++.
  Finite date/time conversions now use checked epoch arithmetic and reject
  out-of-range inputs with Haskell exceptions.
- Previously, TIMESTAMP_S and TIMESTAMP_MS decoding multiplied Int64 values
  into microseconds and could overflow. Each timestamp family now retains its
  own units. Composite TIMESTAMP_S/MS/NS and TIME_NS values can be rebound
  without losing units, nanoseconds, or typed NULLs.
- Previously, UTCTime parameters had SQL type TIMESTAMP and could change their
  meaning under a non-UTC session timezone. They now have type TIMESTAMPTZ.
- Previously, Float parameters used DOUBLE, which hid a missing REAL decoder.
  Float parameters now use FLOAT, and REAL results decode to Float or Double.
  NaN, infinity, and negative zero remain supported. Narrowing a finite Double
  that exceeds Float's range now fails instead of producing infinity.
- Previously, Int8 and unsigned narrowing conversions could wrap out-of-range
  values. They now report conversion errors. Intermediate signed and unsigned
  values remain 64-bit until the target bounds have been checked.
- Previously, text parameters were terminated at an embedded NUL. Text and
  String parameters now pass their UTF-8 byte length and preserve NULs. SQL,
  native names, paths, and configuration strings reject NUL to prevent silent
  truncation. Native names and error messages are decoded as UTF-8.
- DECIMAL values retain their exact integer representation. Invalid width,
  scale, or magnitude now fails before native construction. Invalid ENUM
  indexes, UNION tags, and composite constructors also produce controlled errors.
- Previously, BIT padding could give incorrect SQL bit counts. Padding now
  follows DuckDB's representation; unsupported empty or malformed inputs fail
  before native use.
- Previously, generic records and sums decoded by position. They now match
  field and member names and reject incompatible schemas. NULL non-nullable
  products report a conversion failure instead of reaching a partial `error`.
- Previously, a NULL UNION payload could lose its declared member type. Typed
  NULL payloads now retain that type through native construction.
- TIMETZ offsets containing seconds now fail when decoding to Haskell's
  minute-based TimeZone. Invalid clock components and offsets fail before
  binding instead of being rounded or narrowed silently.

### Testing and compatibility

- Add crash reproductions for #18, real native ownership tests, and sustained
  checks for long-lived connections, callback release, and cancellation.
  Property tests now include embedded NUL rather than filtering it out.
- Add repeatable benchmarks with checked results. Linux CI checks callback
  and cancellation workloads under Valgrind.
- Raise the minimum native DuckDB version to 1.5.3.
- Use GHC 9.14.1 by default. Test the latest stable patch release in each GHC
  series from 9.6 to 9.14.

## 0.1.5.2
- Fix a connection leak: `close` and the connection finalizer built the close action but then discarded it, so the DuckDB connection and database handles stayed open. Every leaked database instance also kept its own DuckDB thread pool alive. (Reported by @winitzki, see #15.)
- Fix the same defect in `closeStatement`, which discarded the action that destroys the prepared statement. (Fixed by @bgamari in #15.)
- Add a `duckdb-simple-leak-test` test suite that runs many open/close cycles and fails if the thread count or the resident set size of the process grows.

## 0.1.5.1
- Re-export the `RowParser` data constructor from `Database.DuckDB.Simple.FromRow`, restoring the API that `0.1.5.0` unintentionally broke (see #6). Downstream packages such as `beam-duckdb` rely on this constructor. (Sorry!)

## 0.1.5.0
- Raise the supported DuckDB runtime baseline to `1.5.0+` via the `duckdb-ffi-1.5` dependency line.
- Add DuckDB 1.5 high-level wrappers for startup config inspection and connection setup, catalog lookup, file-system handles, copy-function registration, and custom log storage registration.
- Extend scalar-function support with `createFunctionWithState`, allowing thread-local per-worker execution state backed by the new DuckDB 1.5 scalar init API.
- Deduplicate shared helpers (`withClientContext`, `destroyValue`, `destroyLogicalType`, etc.) into `Internal.hs`.
- Update the test suite for DuckDB 1.5 behavior, including `TIME_NS` decoding and new wrapper coverage for copy/logging/config/catalog/file-system helpers.

## 0.1.2.3
- Move DuckDBColumnType requirement for ToField to DuckValue

## 0.1.2.2
- Add support for reading and writing arrays.
- Added `Database.DuckDB.Simple.Generic` with GHC generics helpers (`GToField`/`GFromField`) for encoding records as DuckDB STRUCTs and sum types as UNIONs, plus a `ViaDuckDB` newtype for convenient @DerivingVia@ support.
- Added `DuckValue` instances for `[]`, `Array Int a`, and `Map k v` to enable lists, arrays, and maps as fields within generically-encoded structs and unions.
- Documented struct/union generic support; added richer tests covering database round-trips and deriving-via examples.
- Support decoding and binding STRUCT and UNION values via new `StructValue`/`UnionValue` helpers and corresponding `FromField`/`ToField` instances.

## 0.1.2.0
- Added LIST/MAP coverage note: LIST columns decode into Haskell lists and MAP columns into strict `Map k v`, with matching parameter bindings via `ToField`.
- Taught `FromField` to interpret `FieldBigNum` as `Integer`/`Natural` and added matching `ToField` instances for `BigNum`, `Integer`, and `Natural` so BIGNUM parameters round-trip without truncation.
- Added `duckdbColumnType` helper and `DuckDBColumnType` class, exposing the DuckDB column type associated with each `ToField` instance.
- Fixed UUID decoding by undoing DuckDB’s upper-word bias and added a UUID round-trip regression test.
- Fixed BIT encoding and decoding

## 0.1.1.2
- Broadened `FieldValue` and `FromField` coverage to handle all DuckDB scalar types, including intervals, HUGEINT/UHUGEINT, decimals (with metadata), time/timestamp with additional precisions, timezone-aware values, bit strings, bignums, and enums.
- Fixed enum decoding for both query results and scalar-function inputs by honouring the logical type’s underlying storage width (uint8/uint16/uint32).
- Ensured decimal vectors read accurate width/scale metadata when materializing results or invoking UDFs.
- Added unsigned `ToField` bindings that route through DuckDB’s native uint creators and exposed `FromField Word`.
- Expanded the test suite with regressions covering unsigned round-trips, huge integers, intervals, decimals, and timezone-aware values.

## 0.1.0.0
- Initial release
