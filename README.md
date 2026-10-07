# duckdb-haskell

`duckdb-ffi` 2.0 and `duckdb-simple` 0.4 target DuckDB 2.0.
See [the API snapshot, feature coverage, build commands, and release checklist](docs/duckdb-2.0.md).
The package version selects the supported native API. No preview flag is required.
Use the earlier package releases with DuckDB 1.5.

## duckdb-ffi

Available on [Hackage](https://hackage.haskell.org/package/duckdb-ffi).
`duckdb-ffi` provides low-level Haskell bindings to the
[DuckDB](https://duckdb.org) [C API](https://duckdb.org/docs/api/c/overview).
The package includes the official `duckdb.h` header and C wrappers for the
Haskell FFI.

### Highlights

- Covers connections, prepared statements, result sets, vectors, logical types,
  appenders, and Arrow integration.
- Groups bindings into modules under `Database.DuckDB.FFI.*`.
- Includes integration tests for the native bindings.
- Provides both C API generations for DuckDB 2.0. V2 shares common value and
  Arrow layouts with the retained v1 bindings.

### Native library installation

Supply the matching DuckDB 2.0 library while the final release archives are
unavailable. Build the pinned source with `scripts/build-duckdb-preview.sh`.
The build commands are in [docs/duckdb-2.0.md](docs/duckdb-2.0.md).
Setup fails before downloading if final native checksums have not been recorded.
It cannot fall back to DuckDB 1.5.

The automatic installer supports glibc Linux (x86_64 and aarch64) and macOS.
After the final archives are verified and pinned, it downloads DuckDB 2.0.0 into
`$XDG_CACHE_HOME/duckdb-haskell/2.0.0` (usually
`~/.cache/duckdb-haskell/2.0.0`). It verifies SHA256 before extraction.
It requires `curl`, `unzip`, and `sha256sum` (`shasum` on macOS).
Keep the installed native directory available while linked applications run.

The download version and checksums are pinned together in `Setup.hs`.
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

The supplied library must be DuckDB 2.0 and match the bundled headers. In Nix builds and
shells, Setup skips the download and leaves library discovery to Cabal and
the Nix build environment. Cross builds, musl Linux, and other platforms must
supply the target library explicitly.

For historical upgrading notes from DuckDB 1.4 to 1.5, see
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

CI builds the pinned DuckDB 2.0 source on Linux and macOS ARM64. Docker
builds and tests both packages. Nix does the same with its own DuckDB library.
The tests check the loaded native version. With GHC 9.14.1, CI also installs
`duckdb-ffi` and runs a separate program with normal and dynamic Haskell linking.
