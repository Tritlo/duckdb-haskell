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
