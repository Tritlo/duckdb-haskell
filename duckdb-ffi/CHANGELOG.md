# Changelog

## 2.0.0.0

- Target DuckDB 2.0. The retained API supports DuckDB 2.0.x releases. Use the
  earlier package release with DuckDB 1.5.
- Generate the bindings from the DuckDB 2.0.0 client headers with hs-bindgen
  1.0. `Database.DuckDB.FFI` contains all 548 functions of the retained API.
  Remove `Database.DuckDB.FFI.Deprecated`. Its functions are in
  `Database.DuckDB.FFI`. See MIGRATION.md for the changed types and names.
- Add `Database.DuckDB.FFI.V2` with the 620 functions of the new C API.
  Upstream marks this API as unstable, so it requires exactly DuckDB 2.0.0.
- DuckDB 2.0.0 plans casts between some pairs of types as casts that cannot
  fail. This includes registered casts, for example from INTEGER to VARCHAR. If
  a registered cast reports an error for such a pair with an Invalid Input, Out
  of Range, or Conversion code, or with a code that DuckDB does not map, for
  example `DUCKDB_V2_ERROR_API`, DuckDB gives an INTERNAL error. During query
  execution, that error invalidates the database. A V1 cast cannot
  choose the code of its error. A V2 cast can report
  `DUCKDB_V2_ERROR_TYPE_MISMATCH`. The other codes that keep their type and the
  database, and that abort an explicit transaction under each transaction
  invalidation policy, are the codes with the prefix `DUCKDB_V2_ERROR_` and
  these names: IO_GENERAL, IO_HTTP, INPUT_OBJECT_SIZE, RESOURCE_OUT_OF_MEMORY,
  RESOURCE_DEPENDENCY, RESOURCE_MISSING_EXTENSION, RESOURCE_AUTOLOAD,
  TYPE_UNKNOWN, TYPE_INVALID, TYPE_DECIMAL, TYPE_DIVIDE_BY_ZERO, QUERY_SYNTAX,
  QUERY_PLANNER, QUERY_OPTIMIZER, QUERY_EXPRESSION, QUERY_EXECUTOR,
  QUERY_SCHEDULER, QUERY_NOT_IMPLEMENTED, QUERY_PARAMETER_NOT_RESOLVED,
  DATABASE_CONSTRAINT, DATABASE_INDEX, DATABASE_SEQUENCE, DATABASE_STATISTICS,
  DATABASE_SERIALIZATION, DATABASE_TRANSACTION, CONFIGURATION_SETTINGS,
  CONFIGURATION_INVALID, RUNTIME_INTERRUPT, and RUNTIME_NULL_POINTER.
- Add nanosecond timezone timestamps and the data-corruption and
  resource-in-use error tags.
- Remove the Arrow callback constructors, wrappers, and release helpers. Use
  `toFunPtr` and `fromFunPtr` from hs-bindgen-runtime.
- Deprecated native functions no longer have `DEPRECATED` pragmas.
- Download the DuckDB 2.0.0 shared-library archives on glibc Linux and macOS.
  Keep their symlinks and versioned library files.
- Install both client headers, `duckdb_arrow.h`, and the DuckDB license with
  the library.
- Reject platforms other than Linux and macOS on x86_64 and aarch64 at compile
  time.

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
