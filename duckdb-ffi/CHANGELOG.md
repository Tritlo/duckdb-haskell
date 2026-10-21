# Changelog

## 2.0.0.0 (unreleased)

- Add opt-in `extension` mode for loadable V2 extensions on Linux and macOS.
  Use the host function table instead of linking another DuckDB library.
  This mode exposes only the V2 module and requires entrypoint initialization.
- Target DuckDB 2.0. The package includes both C API generations without a
  preview flag. Use the earlier package release with DuckDB 1.5.
- Pin both client headers to the final DuckDB 2.0.0 tag, commit `d54cbbe3df`.
  Refresh factories, configuration handles, explicit registration
  targets, callbacks, and scope enums with the current V2 API.
- Expose all 620 V2 functions. The entire V2 API is currently unstable and
  requires the matching native snapshot. The retained API has 548 functions.
- Add nanosecond timezone timestamp values and the data-corruption error tag.
- Generate both C APIs with hs-bindgen 1.0. Keep generated Haskell and C
  artefacts outside Git. Generate them locally and in CI.
- Expose one generated module per API. Remove the exposed Deprecated module
  and V2 Functions and Types submodules. Use native typedef newtypes, const
  pointers, and generated callback conversions.
- Check complete function and callback coverage and 1,150 native ABI assertions.
- Install verified DuckDB 2.0.0 shared-library archives on glibc Linux and macOS.
  Preserve their symlinks and versioned library files.
- Install both client headers and the DuckDB license. Keep these
  headers available when the Cabal build directory is outside the source tree.
- Test v2 aggregate, table, cast, COPY, vector, filesystem, and logging behavior.

## 1.5.6.0

- Use the official DuckDB 1.5.6 header and native download. Keep native support
  for DuckDB >= 1.5.3 and < 1.6.
- Add the COPY_DATABASE, UPDATE_EXTENSIONS, and MERGE_INTO statement tags.
- Document that invalid DECIMAL metadata can terminate native DuckDB 1.5.3.
  Test the nonfatal constructor behavior only on native 1.5.4 and later.
- Add the GEOMETRY and VARIANT type tags and the geometry CRS accessor. The
  returned CRS string is owned by the caller and must be freed with `duckdb_free`.
- Document that `duckdb_create_logical_type` returns an incomplete VARIANT
  descriptor. Obtain a complete descriptor from a query result before inspecting
  its children.

## 1.5.3.0

- Export invokers for Arrow schema/array release and all four stream callbacks.
  Applications can now call the function pointers exposed by the Arrow types
  without repeating foreign declarations from the test suite.
- Export the corresponding `wrapArrow...` constructors for Haskell callbacks.
  Add `releaseArrowSchema`, `releaseArrowArray`, and `releaseArrowStream` to
  release initialized objects only when their release callback is non-null.
  These helpers mask asynchronous exceptions during release. Callers still
  own the outer struct storage and any callback pointers they allocate.

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
