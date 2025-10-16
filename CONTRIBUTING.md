# Contribution Guidelines


## Project Structure & Module Organization
`duckdb-ffi/src` mirrors DuckDB’s C API, with accompanying C shims in `duckdb-ffi/cbits` and tests in `duckdb-ffi/test`. `duckdb-simple/src` layers ergonomic helpers over the FFI surface; integration and property tests live in `duckdb-simple/test`.

## Testing guidelines

Make sure tests touch all new code paths, and round-trip through the database when required.
All tests should pass, but feel free to use `expectFailure` to document a bug.

## Code style

Code should be formatted with `fourmolu` before being committed.
We try to stick to the ghc naming convention: values are in `snake_case`, functions in `camelCase`.

## Native Dependencies & Configuration
Ensure a compatible `libduckdb` shared library is present before building or running tests; the bindings target DuckDB 1.4-series.
