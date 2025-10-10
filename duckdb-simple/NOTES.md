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
