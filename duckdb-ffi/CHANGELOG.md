# Changelog

## 1.5.0.0
- Upgrade the vendored DuckDB C header and build metadata to DuckDB 1.5.0, making `duckdb-ffi` a DuckDB `1.5.0+` binding set.
- Add raw FFI coverage for new DuckDB 1.5 API areas including custom config options, scalar function init/state hooks, copy functions, file-system handles, catalog inspection, logging, and new helper/appender/vector APIs.
- Extend the aggregate re-export surface with the new 1.5 modules: `Catalog`, `CopyFunctions`, `FileSystem`, and `Logging`.
- Fix the long-string helper regression by switching the test coverage to length-aware `duckdb_string_t` decoding.

## 1.4.1.4
- Simplify Setup.hs to use defaultMain, fixing builds in Nix and other
  environments where the custom library search logic failed.

## 0.1.4.2
- Move deprecated FFI functions to the Deprecated module.

## 0.1.4.1
- Initial release
