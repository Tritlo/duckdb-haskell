# duckdb-simple Notes

Document failed experiments, unexpected behaviour, and other project learnings
here whenever the test suite uncovers an issue. Keeping this log current helps
us understand past decisions and avoid repeating mistakes.

## 2024-10-10 — Initial scaffolding build

- Observed a `ghc-9.6.6` panic (`Don't understand library name duckdb`) when
  the library depended on `duckdb-ffi`. The panic occurred while building the
  empty scaffolding because GHC tried to resolve the DuckDB C library even
  though we are not yet using the bindings.
- Immediate fix (before upstream fix landed): drop the `duckdb-ffi` dependency
  for the scaffolding phase so the initial build could pass.
- Update: the underlying “Don't understand library name duckdb” linker issue
  has now been fixed upstream, so the dependency can return once we start
  wiring connections in Phase 1.

## 2024-10-10 — Phase 1 statement preparation bug

- Initial attempt to turn queries into C strings used
  `ByteString.Unsafe.unsafeUseAsCString`. DuckDB reported syntax errors with
  stray characters (e.g. `SELECT 1)`), revealing the temporary buffer was not
  consistently null-terminated for our use case.
- Switched to `Data.Text.Foreign.withCString`, which always allocates a
  properly terminated buffer and resolved the parser failures in the test
  suite.

## 2024-10-10 — duckdb-ffi setup fix

- While reconfiguring the test suite, `duckdb-ffi/Setup.hs` failed to compile
  because `installVendoredDuckDB'` was invoked without the required verbosity
  argument. Added the missing parameter so the vendored download step builds
  reliably under GHC 9.6.

## 2024-10-10 — DuckDB SQL limitations discovered in Phase 4

- The DuckDB engine currently rejects statements that mix positional (`?`)
  and named (`$param`/`:param`) placeholders in the same query with a parser
  error (“Mixing named and positional parameters is not supported yet”).
  Updating the statement tests to account for this limitation keeps the suite
  green while documenting the behaviour.
- DuckDB also reports a syntax error when executing `SAVEPOINT ...`, so the
  planned `withSavepoint` helper cannot rely on native savepoints. We need to
  gate the helper or provide a graceful fallback/error message in the public
  API before enabling the test coverage.

## 2024-10-11 — DuckDB parameter metadata quirks

- `duckdb_parameter_name` returns auto-generated numeric names (e.g. `"1"`)
  for positional placeholders. Treat these values as “unnamed” when deciding
  whether to raise `FormatError`, otherwise legitimate positional queries would
  be rejected.
- Queries with genuine named placeholders still surface their textual name,
  so the validation logic can differentiate between the two cases.

## 2024-10-11 — Phase 5 observations

- `bindNamed` still requires callers to supply every placeholder explicitly; consider accepting sparse bindings once DuckDB exposes richer metadata.
- Error reporting for mixed positional/named parameters still relies on DuckDB’s runtime error message; revisit if the FFI exposes better introspection.


## 2024-10-11 — Phase 5.5 (RowParser) observations

- Swapping the direct `[Field] -> Either ResultError` conversion for a RowParser/Ok pipeline significantly improves diagnostics and unlocks generic `FromRow` instances. The conversion glue now needs to translate parser failures back into meaningful `ResultError` values; make sure new parser combinators surface the column index so the resulting `SQLError` stays actionable.
- DuckDB does not surface column metadata about unused trailing fields, so when the parser advances past the expected column count we synthesise `ColumnCountMismatch`. Keep an eye on this once streaming APIs arrive—chunked decoding must maintain the same invariant.
- The test suite now depends on `transformers`; ensure downstream projects importing duckdb-simple bring that dependency in if they rely on the new RowParser combinators directly.

## 2025-10-11  Functions

- Added `Database.DuckDB.Simple.Function` to expose DuckDB scalar function registration with a `sqlite-simple`-style `Function` typeclass and `createFunction`/`deleteFunction` helpers.
- Callback implementation decodes argument vectors into existing `FromField` values and materialises results back into DuckDB vectors for `BOOL`, `INTEGER`, `DOUBLE`, and `VARCHAR` (including `Maybe` support and zero-argument functions).
- Registration stores Haskell callbacks via DuckDB extra-info with lifetime handling; errors propagate as `SQLError` instances and are surfaced inside DuckDB through `duckdb_scalar_function_set_error`.
- Added integration tests covering pure, nullable, and `IO`-based functions plus the `deleteFunction` helper; README now documents the new module alongside example usage.
- `deleteFunction` attempts a `DROP FUNCTION IF EXISTS`; DuckDB flags C API
  registrations as internal, so the operation reports an `SQLError` explaining
  the limitation instead of removing the function.

## 2024-10-11 — Phase 6 documentation notes

- Expanded the README with resource-management guidance, RowParser usage, and a
  roadmap so new adopters understand current scope vs. future work.
- Verified that every newly exported helper (`RowParser`, `field`, `fieldWith`,
  `numFieldsRemaining`) carries Haddock documentation to keep the public API
  discoverable.

## 2024-10-12 — Phase 7 metadata & ergonomics

- DuckDB exposes column metadata through both prepared-statement and result
  APIs; `columnCount`/`columnName` now wrap the prepared-statement path, but
  callers should expect zero-based indexing and `SQLError` when requesting an
  out-of-bounds column (DuckDB returns a null pointer rather than an error
  code).
- The new connection-level `rowsChanged` helper tracks the most recent mutation
  by persisting counts in an `IORef`. DuckDB still lacks a portable
  `last_insert_rowid`, so the README points users to SQL `RETURNING` instead.
- Reintroducing the `(:.)` combinator required enabling `TypeOperators` across
  `Types`, `FromRow`, and `ToRow`; remember to pull that language pragma into
  any future modules that reference the operator directly.
## 2024-10-12 — Streaming result integration learnings

- DuckDB’s streaming API (`duckdb_execute_prepared_streaming` +
  `duckdb_stream_fetch_chunk`) returns chunk pointers that must be destroyed
  with `duckdb_destroy_data_chunk` once exhausted. Forgetting to free the last
  chunk leaked memory in early experiments; the final implementation always
  releases the active chunk before fetching the next one and ensures statement
  teardown resets any lingering stream.
- Streaming chunks expose raw vectors rather than column metadata names. We now
  capture column descriptors up-front via the prepared-statement metadata so
  streamed `Field` values mirror the eager query path (names, indices, types).
- Streaming chunk decoders now cover bool/int/float/text as well as the new
  temporal/blob cases. List/struct/vector families still fall back to a
  descriptive `SQLError` until Phase 9 follow-up work lands.
- `nextRow` shares the streaming cursor with the new `fold*` helpers. Resetting
  the prepared statement bindings or closing the statement tears down the
  stream so eager queries can run afterwards without seeing the stale result.
- Added regression tests that fold thousands of rows, exercise parameterised
  folds, and iterate with `nextRow` to guard against future refactors breaking
  the chunk lifecycle.

## 2024-10-12 — Extended type coverage

- Added `FieldValue` constructors and `FromField`/`ToField` instances for DuckDB
  temporal types (`DATE`, `TIME`, `TIMESTAMP`). Values map to `Day`,
  `TimeOfDay`, `LocalTime`, and `UTCTime` so application code can lean on the
  standard `Data.Time` APIs.
- Binary columns now surface as strict `ByteString` both eagerly and through
  streaming folds. The BLOB helpers copy out the underlying buffer and release
  the DuckDB allocation immediately to avoid lifetime pitfalls.
- Dedicated UUID round-tripping remains on the backlog; for now UUID columns
  surface as text so callers can opt into parsing once the mapping stabilises.
- Updated the README to document the new mappings and tightened the tests with
  round-trip coverage for temporals and blobs to catch future regressions.
