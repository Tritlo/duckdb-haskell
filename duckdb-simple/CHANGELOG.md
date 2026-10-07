# Changelog

## Unreleased

These changes target the 0.3.0.0 release. Do not release review branches separately.

- Use native DuckDB 1.5.6 by default. Keep native support for DuckDB >= 1.5.3
  and < 1.6. Test native versions 1.5.3 through 1.5.6.
- Require `duckdb-ffi >= 1.5.6.0` for the updated native bindings.

- Decode VARIANT results to the `FieldValue` of the stored value, so existing
  `FromField` instances read them. Objects decode to `FieldStruct` values with
  VARIANT fields, in entry order. The decoder checks DuckDB's private 1.5
  format and payload bounds, and rejects cyclic child references. Previously,
  valid values with 128 nested containers raised an error. The decoder now
  accepts deep acyclic values without a library depth limit.
- Add `Variant`, a `FieldValue` that binds as a VARIANT, and `variantObject`,
  which builds an object payload. Scalars keep their native type. Object
  parameters reject duplicate, empty, and NUL-containing keys because the
  native constructors cannot represent all of these names.
  `Variant` has no `ToDuckValue` instance, and `logicalTypeFromRep` raises an
  error for VARIANT.
- Read the VARIANT type and GEOMETRY types for a list of CRS definitions,
  because the C API cannot create them. A connection reads them with one
  query on a separate connection, the first time a parameter needs one.
  Parameters use these types. Add `ConnectionOptions`, `openWithOptions`, and
  `withConnectionWithOptions` to set the CRS list, which defaults to
  `OGC:CRS84`.
- Array parameters use the element's `ToField` instance. Elements require
  `ToField` and `DuckDBColumnType`. Add a `ToDuckValue` instance for arrays
  whose elements have `ToDuckValue`. It does not need a connection.
- Array parameters of STRUCT values, UNION values, generic records, and
  arrays bind. They take the element type of the first element that is not
  NULL. An empty or all-NULL array of these elements raises an error. Scalar
  elements keep the type of their column type name.
- A GEOMETRY payload decodes to `FieldGeometry` with raw WKB. Import those
  bytes with `ST_GeomFromWKB(?)::VARIANT`.
- Add parameter and result instances for `Data.Geometry.Geometry` from
  `geometry-simple`. It provides decoded shapes, unboxed coordinate vectors,
  and runtime coordinate layouts. This type does not store CRS metadata.
  `RawGeometry` retains WKB and CRS without decoding coordinates. Existing
  `ByteString` results still return WKB. Empty points remain distinct from SQL NULL.
- Use `geometry-simple >= 0.1.1.0` for pure geometry types and codecs.
  Structured parameters use its WKT writer and DuckDB's native cast.
  `RawGeometry` has no parameter instance. Import its WKB with
  `ST_GeomFromWKB` and apply its CRS with `ST_SetCRS`. This preserves mixed
  layouts, dimensional empties, and native NaN payloads without WKT conversion.
- Add `FieldGeometry` and `LogicalTypeGeometry` to the public value and type
  representations. Update exhaustive matches when upgrading.
  Nested result decoding retains raw WKB and CRS. Generic parameters with
  non-NULL `FieldGeometry` values raise an error instead of converting their
  bytes through WKT. Construct those nested values with explicit SQL import.
- Keep CRS metadata in this package. The standalone shape has no CRS.
  Both forms own their memory and remain usable after the connection closes.
  CRS text can hold an identifier, a custom name, or a full WKT2/PROJJSON
  definition.
- Composite parameters keep GEOMETRY CRS metadata for the CRS definitions
  that the connection read. Other CRS definitions bind without a CRS, and
  `logicalTypeFromRep` creates `GEOMETRY` with no CRS. Insert the value into a
  column with a CRS, or cast it in SQL, to apply the CRS.
- Read STRUCT and UNION type metadata once for each result vector instead of
  once for each row. Prepare their direct child readers at the same time.
  A local benchmark with a CRS-tagged GEOMETRY field ran about ten times faster.
  LIST, ARRAY, and MAP decoders can still read child metadata for each parent row.

## 0.2.0.0

### Query execution and resource lifetime

- Add `Database.DuckDB.Simple.Deprecated.Streaming` for callers that need native
  streaming. It provides row folds, cursors, and Arrow folds with a deprecation
  warning. Both execution modes share decoding and resource cleanup. Native
  chunk fetching is interruptible, including cleanup of a chunk fetched just
  before cancellation. Default APIs continue to use materialized execution.
- Previously, a long native query could defer Ctrl-C until execution finished.
  Query preparation and execution now run in a worker so the caller can receive
  asynchronous exceptions. Cancellation interrupts DuckDB and waits for the
  worker before releasing its resources. Prompt cancellation requires the
  threaded RTS. Native code and Haskell callbacks must return before cleanup
  can finish.
- Add scoped Arrow batch export through `foldArrow` and `foldArrow_`. The
  callback receives a separate schema and array for each batch. Consumers may
  release or move them under the Arrow C Data Interface. The fold releases
  remaining contents on every exit path. This uses the supported schema/chunk
  conversion API and retains a materialized native result while the fold runs.
- The initial Arrow fold shared one borrowed schema across callbacks. Consumers
  such as `dataframe-arrow-bridge` release that schema when they import a batch,
  which made subsequent batches unusable. Each batch now has its own schema.
  Moved root objects remain usable after query and connection close; their
  consumer must release them.

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

- Add an optional DataFrame integration suite with `-fdataframe-tests`. It
  imports several Arrow batches through `dataframe-arrow-bridge` and checks
  values, NULLs, column order, consumer failures, and use after connection close.
  CI runs it on Linux and macOS. The ordinary suite also checks consumption,
  ownership transfer, and cleanup after a consumer has released its objects.
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
