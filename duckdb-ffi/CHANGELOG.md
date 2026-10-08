# Changelog

## 1.5.6.0

- Generate the complete raw API from the DuckDB and Arrow headers with
  hs-bindgen 1.0. Remove handwritten raw modules, C ABI shims, and the old
  binding generator. Keep all 158 earlier type names and all 546 `c_duckdb_*`
  function names directly in `Database.DuckDB.FFI`. Enum constants use C names.
- Use existing hs-bindgen type specifications and `RenameTerm` configuration
  through a maintainer Haskell driver. Retained names use generated representations.
- Include generated source and 508 C ABI assertions in release archives.
  Keep these files out of Git. Generate both files with the maintainer script
  and pinned Nix toolchain before checkout builds. Commit their SHA256 checksums
  in `generated.sha256`. CI and the release script generate and verify both
  files against this manifest. The release script completes this check before
  it prepares documentation and source archives. Consumers need only `base`,
  `hs-bindgen-runtime`, a C compiler, and DuckDB. Compare the four supported
  native targets and check generated layouts during each package build.
- Keep native handle pointees opaque. Do not generate readable instances
  for the placeholder structs in the C header.
- Move Arrow ownership helpers to `duckdb-simple`. Raw calls use direct struct
  arguments/results and do not preserve the old C shims' NULL defaults.

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
