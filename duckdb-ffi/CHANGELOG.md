# Changelog

## 1.5.3.0

- Deprecated native modules now emit a Haskell deprecation warning. The bindings
  remain available for applications that need the legacy API.

- Previously, temporal foreign imports treated single-field C structs as scalar
  arguments and return values. C adapters now perform those conversions with
  the native calling convention. The typed Haskell signatures retain every
  native field, including DATE days and packed TIME_TZ bits.
- Previously, deprecated Arrow handles were treated as wrappers containing an
  internal pointer. The helpers now use the actual Arrow object address and
  clear only its release callback after a move. This prevents corruption of
  schema fields, array metadata, and stream callbacks. Tests use real DuckDB
  objects and verify that moved buffers remain valid.
- Download the native library to the user cache and verify the archive's SHA256 checksum.
- Use an existing native library when the user or Nix supplies it.
- Track native library selection through the `systemlib` flag, `extra-lib-dirs`, and the `--duckdb-install-dir` configure option.
- Raise the minimum native DuckDB version to 1.5.3.
- Use GHC 9.14.1 by default. Test the latest stable patch release in each GHC series from 9.6 to 9.14.

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
