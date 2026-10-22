# duckdb-haskell

`duckdb-ffi` 2.0 and `duckdb-simple` 0.5 target DuckDB 2.0.
See [the pinned API, feature coverage, and build commands](docs/duckdb-2.0.md).
Use the earlier package releases with DuckDB 1.5.

## duckdb-ffi

Available on [Hackage](https://hackage.haskell.org/package/duckdb-ffi).
`duckdb-ffi` provides low-level Haskell bindings to the
[DuckDB](https://duckdb.org) [C API](https://duckdb.org/docs/api/c/overview).
The package generates bindings from the pinned DuckDB 2.0 client headers with
`hs-bindgen` 1.0. It provides `Database.DuckDB.FFI` for the retained C API and
`Database.DuckDB.FFI.V2` for the new C API.

### Highlights

- Covers connections, prepared statements, result sets, vectors, logical types,
  appenders, and Arrow integration.
- Generates native types, layouts, callbacks, and calling-convention adapters.
- Includes integration tests for the native bindings.
- Covers all 548 retained and 620 V2 functions in the pinned 2.0.0 release.
- `Database.DuckDB.FFI` supports DuckDB 2.0.x releases. CI tests DuckDB 2.0.0.
- `Database.DuckDB.FFI.V2` requires exactly DuckDB 2.0.0. Upstream marks the
  complete V2 API as unstable: "The C API v2 is not yet stable. Its interface
  may change in upcoming releases." A later DuckDB release can require a new
  `duckdb-ffi` release for V2.

### Generate the bindings

Run this command before building a repository checkout:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
```

The command checks the output against `duckdb-ffi/generated.sha256` and writes
the Haskell modules and C ABI assertions to the ignored `duckdb-ffi/generated/`
directory. CI generates the same artefacts before building. Git tracks the
generator, pinned headers, and checksums.

`dev/release.sh` generates and verifies both APIs, then builds and uploads the
Hackage archives. Those archives include the generated sources. Package users
need `hs-bindgen-runtime` and a matching native library, but no LLVM or hs-bindgen.
See [dev/README.md](dev/README.md) for the maintainer tools.

For a local build with the pinned native release:

```sh
nix-shell dev/nix/shell.nix --run 'cabal build all --with-compiler=ghc -fsystemlib'
```

### Native library installation

The automatic installer supports glibc Linux (x86_64 and aarch64) and macOS.
It downloads the verified DuckDB 2.0.0 release into
`$XDG_CACHE_HOME/duckdb-haskell/2.0.0` (usually
`~/.cache/duckdb-haskell/2.0.0`). It verifies SHA256 before extraction.
It requires `curl`, `tar`, and `sha256sum` (`shasum` on macOS).
The installer uses upstream `duckdb-shared-libs-PLATFORM.tar.gz` archives.
It keeps shared-library symlinks and their versioned targets together.
Keep the installed native directory available while linked applications run.

`duckdb-ffi/native.sha256` pins the release archives and their checksums.
The Haskell package version can differ from the native library version.

As in [Hasktorch](https://github.com/hasktorch/hasktorch/blob/master/libtorch-ffi/Setup.hs), you can choose the installation directory:

```sh
cabal build all --configure-option="--duckdb-install-dir=$PWD/.duckdb"
```

Setup appends the version and platform, for example `.duckdb/2.0.0/linux-amd64`.
Use an absolute path. Cabal tracks this option and reconfigures the package
when it changes. You can also set it in `cabal.project.local`:

```cabal
package duckdb-ffi
  configure-options: --duckdb-install-dir=/absolute/path/to/native-libraries
```

This option applies to automatic installation. Supplied libraries take precedence.

To use an existing library, configure `duckdb-ffi` in `cabal.project`:

```cabal
package duckdb-ffi
  flags: +systemlib
  extra-lib-dirs: /absolute/path/to/duckdb
```

Omit `extra-lib-dirs` if the linker can already find the system library.
Use absolute paths for supplied directories. Cabal tracks the flag and paths
when deciding whether to reuse an installed package. Setup records supplied
directories in the runtime library search path on Linux and macOS.

When building this repository, you can also pass `-fsystemlib` and
`--extra-lib-dirs=/absolute/path/to/duckdb` on the command line.
Supplying `extra-lib-dirs` also skips the download without the flag.

The supplied library must be a DuckDB 2.0.x release. The V2 API, and
`duckdb-simple` 0.5, which uses it, require exactly DuckDB 2.0.0. In Nix builds and shells, Setup uses Cabal and Nix to
find the library and skips the download.

`duckdb-ffi` builds on Linux and macOS on x86_64 and aarch64. The generated ABI
checks reject other platforms. The automatic download supports glibc Linux and
macOS. On other Linux systems, supply the library with `-fsystemlib`.

For raw API changes and earlier upgrade notes, see
[MIGRATION.md](MIGRATION.md).

## duckdb-simple

Available on [Hackage](https://hackage.haskell.org/package/duckdb-simple).
`duckdb-simple` builds on `duckdb-ffi` to provide an interface in the style of
`sqlite-simple` and `postgresql-simple`. It uses `ToField` for parameter binding
and `FromField` for result conversion.

### Highlights

- Manage connections and statements with `withConnection` and `withStatement`.
  Run SQL with `execute` and `query`, or process rows with `fold`, `foldNamed`,
  `fold_`, and `nextRow`.
- Decode signed and unsigned integers, HUGEINT/UHUGEINT, decimals, intervals,
  date and time values, enums, bit strings, blobs, and bignums.
- Support for positional (`?`) and named (`$name`) parameters, with detailed
  diagnostics when placeholders and bindings disagree.
- Map product types to query results and parameter lists with `FromRow` and
  `ToRow`, including generic deriving.
- Scalar function registration via `Database.DuckDB.Simple.Function`, allowing
  Haskell code (including `Maybe`-returning and `IO` actions) to be invoked
  directly from SQL expressions.
- Transaction helper `withTransaction` and statement metadata utilities
  `columnCount` and `columnName`.

See [duckdb-simple/README.md](duckdb-simple/README.md) for a step-by-step guide,
extended examples, and notes on streaming behaviour and user-defined
functions.

## geometry-simple

`geometry-simple` provides pure geometry types and checked WKB/WKT codecs. It
uses unboxed coordinate vectors and supports XY, XYZ, XYM, XYZM, empty points,
and collections. It needs no DuckDB or other native library. `duckdb-simple`
provides parameter and result instances for its `Geometry` type. Use
`RawGeometry` for WKB bytes with CRS metadata. See
[geometry-simple](https://github.com/Tritlo/geometry-simple) for its API and examples.

## Supported compilers

CI tests these GHC releases:
9.6.7, 9.8.4, 9.10.3, 9.12.4, and 9.14.1.
The project and Docker use GHC 9.14.1 by default.
Use `cabal build all --with-compiler=ghc-VERSION` to select another compiler.

CI uses the pinned DuckDB 2.0.0 release archives on Linux and macOS ARM64.
Two jobs use the automatic download. Docker builds and tests both packages
with the same release. Nix builds the pinned source and tests both packages.
The tests check the loaded native version. With GHC 9.14.1, CI also installs
`duckdb-ffi` and runs a separate program with normal and dynamic Haskell linking.
