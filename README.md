# duckdb-ffi

This library provides an FFI for the [DuckDB](https://duckdb.org)
[C API](https://duckdb.org/docs/api/c/overview).

Generated from `duckdb.h`, and includes a test suite that touches all
functions from the FFI.

Currently supports DuckDB 1.4.0.

Note that you must also install `libduckdb`, via

https://duckdb.org/install/?platform=linux&environment=c&architecture=x86_64.

Note that installing `duckdb` itself is not enough.

If you are looking for a mid-level API, have a look at `duckdb-simple`,
a library similar to `sqlite-simple`.