# duckdb-haskell
## duckdb-ffi

This library provides an FFI for the [DuckDB](https://duckdb.org)
[C API](https://duckdb.org/docs/api/c/overview).

Generated from `duckdb.h`, and includes a test suite that touches all functions from the FFI.

Tested with DuckDB 1.4.0, 1.4.1.

Note that you must also install `libduckdb`, via

https://duckdb.org/install/?platform=linux&environment=c&architecture=x86_64.

Note that installing `duckdb` itself is not enough.

## duckdb-simple
A library in the style of `sqlite-simple` and `postgresql-simple`, including `FieldParser`, `RowParser` etc.
Also allows registering Haskell functions as scalar functions in `duckdb`, similar to `sqlite-simple`.
