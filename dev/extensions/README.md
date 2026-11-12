# Loadable Haskell extensions

This directory contains a loadable Haskell example for the pinned DuckDB 2.0.0
release. The example is not part of either Hackage package. The reusable
`Database.DuckDB.Simple.Extension` helper and extension-mode FFI headers are
part of the packages.

## Design

The separate Cabal project enables the `extension` flags on `duckdb-ffi` and
`duckdb-simple`. Enable both flags together. A flag applies to the complete
build plan, so one project cannot build a DuckDB host and an extension. The
FFI mode exposes only `Database.DuckDB.FFI.V2`. It skips
native-library installation and does not link `libduckdb`. A forced C include
uses DuckDB's official function-table macros in the existing hs-bindgen C
wrappers. The generated bindings remain unchanged and outside Git.

The C entry point of an extension calls `duckdb_haskell_extension_load` and
does nothing else. See [entry.c](example/cbits/entry.c). The function
negotiates the table before Haskell starts. It copies the borrowed table into
the FFI library and publishes it once under a mutex. This mode supports one
DuckDB implementation per process. Generated C wrappers, the table, and
managed callback helpers have hidden visibility. The load function and the
demo's native entrypoint remain exported.
Extension mode omits interface unfoldings. This keeps calls to private C
wrappers inside the FFI shared library when a client uses optimization.

The load function then initializes the threaded RTS once. It uses private
arguments, ignores `GHCRTS`, and disables the RTS signal handlers. The load
fails with an error when the extension is not linked with `-threaded`. The
function keeps the RTS alive for the process lifetime. Do not call `hs_exit`
after registration or database close.
DuckDB can still own Haskell callbacks, state, and destructor addresses.

The RTS runs with `-N1`, so callbacks from all DuckDB worker threads share one
Haskell capability. Users cannot change the RTS options. Haskell extensions
built with the same GHC share one RTS, and the options of the first loaded
extension apply. Extensions built with different GHC versions in one process
are not tested. If the RTS cannot start, the process ends.

`Extension.runExtension` passes an opaque `Extension` to the registration action.
`Extension.createFunction` and `Extension.createFunctionWithState` use the same
typed argument and result conversions as the connection API.
`Extension.extensionTarget` gives the registration target for the functions
that take a `RegistrationTarget`, such as the aggregate, table, and COPY
registration functions. The target must not escape the entrypoint.
Initialization and callback exceptions become native errors. Managed
registration retains callback resources after the entrypoint returns.

`Extension.registerMacroFunction`, `registerPragmaFunction`,
`registerCustomOption`, and `registerLogSink` use the same opaque extension
value. Macro signatures and custom-option defaults use `defaultValue`, as on a
connection.
Upstream marks the complete V2 API as unstable. An extension uses the
`C_STRUCT_UNSTABLE` function table, which DuckDB gives only to an extension
whose footer names the exact running release. `Pack.hs` writes the version of
the pinned V2 header. A later DuckDB release needs a `duckdb-ffi` release with
its headers. A rebuild against the same headers does not help.

The example registers typed scalar functions, scalar and table macros, a
PRAGMA, a typed custom option, and a log sink. They exercise native results,
batch inputs, NULLs, Unicode text, overflow, and callback errors. The example
also registers an aggregate function, a table function with projection
pushdown, a custom type with a cast, a replacement scan, a COPY format, and a
multi-file reader. Together, the registrations use all twelve V2 extension
registration calls. The entrypoint is
`haskell_demo_init_c_api_v2`. DuckDB derives the entrypoint name from the
file name, so the packaged file must be `haskell_demo.duckdb_extension`.

Registration code uses ordinary Haskell values and functions:

```haskell
register extension = do
    Extension.createFunction extension "answer" (42 :: Int64)
    Extension.createFunction extension "add" ((+) :: Int64 -> Int64 -> Int64)
```

See [Demo.hs](example/src/Demo.hs) for the complete entrypoint. Application
functions do not need native handles, vectors, or allocation calls.
[Advanced.hs](example/src/Advanced.hs) uses the registration functions that
take `Extension.extensionTarget`. Their callbacks use native handles and
vectors.

## Build and load

Use the pinned GHC 9.14.1 environment.

Generate ignored API artifacts if they are absent:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
```

Build only the example in its separate project and build directory. Use a fresh
build directory after a native header change. When the source pin changes,
clean an existing directory before the build. Cabal can retain C objects from
the previous function table:

```sh
(cd dev/extensions && cabal clean --builddir=../../dist-newstyle-extensions)
```

```sh
nix-shell dev/nix/shell.nix --run '(cd dev/extensions && \
  GHC_ENVIRONMENT=- cabal build --project-file=cabal.project \
  --builddir=../../dist-newstyle-extensions --with-compiler=ghc \
  duckdb-haskell-extension-example:flib:haskell_demo)'
```

Locate the library after the build. `list-bin` does not check that it exists:

```sh
nix-shell dev/nix/shell.nix --run '(cd dev/extensions && \
  GHC_ENVIRONMENT=- cabal list-bin --project-file=cabal.project \
  --builddir=../../dist-newstyle-extensions --with-compiler=ghc \
  duckdb-haskell-extension-example:flib:haskell_demo)'
```

Fetch the verified native CLI and shared library for the pinned release:

```sh
dev/fetch-duckdb.sh /tmp/duckdb-extension-native
```

Read the target platform before packaging:

```sh
/tmp/duckdb-extension-native/native/duckdb -csv -noheader -c 'PRAGMA platform;'
```

Use the exact platform printed by the CLI. `Pack.hs` reads the DuckDB version
from `duckdb-ffi/cbits/duckdb_v2.h`, so run it from the repository root. See
[PACKAGING.md](PACKAGING.md).

On Linux, Nix's GHC can require a newer glibc than the host CLI uses.
`nix-shell` does not replace the downloaded CLI's ELF interpreter. In that
case, create this wrapper and use its path for `DUCKDB_CLI` in the test command.
Run it inside the same Nix shell as the extension build:

```sh
cat > /tmp/duckdb-extension-native/native/duckdb-nix <<'SH'
#!/usr/bin/env bash
set -eu
loader=$(cat "$NIX_CC/nix-support/dynamic-linker")
libc_dir=$(dirname "$loader")
cxx_dir=$(dirname "$(cc -print-file-name=libstdc++.so.6)")
exec "$loader" --library-path "$libc_dir:$cxx_dir" "$(dirname "$0")/duckdb" "$@"
SH
chmod +x /tmp/duckdb-extension-native/native/duckdb-nix
```

Replace the library path and platform in the packaging command:

```sh
mkdir -p dev/extensions/dist
nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- runghc \
  dev/extensions/Pack.hs /absolute/path/to/libhaskell_demo.so \
  --output dev/extensions/dist/haskell_demo.duckdb_extension \
  --platform linux_amd64 --extension-version 0.1.0'
```

Run the test suite in the same project and build directory. `DUCKDB_CLI`
names the CLI, and `DUCKDB_HASKELL_DEMO_EXTENSION` names the packaged
extension. The suite fails when a variable is missing. Use absolute paths,
because `cabal test` runs the suite in the package directory:

```sh
nix-shell dev/nix/shell.nix --run '(cd dev/extensions && \
  DUCKDB_CLI=/tmp/duckdb-extension-native/native/duckdb \
  DUCKDB_HASKELL_DEMO_EXTENSION="$PWD/dist/haskell_demo.duckdb_extension" \
  GHC_ENVIRONMENT=- cabal test --project-file=cabal.project \
  --builddir=../../dist-newstyle-extensions --with-compiler=ghc \
  --test-show-details=direct duckdb-haskell-extension-example:test:haskell-demo-test)'
```

To load the packaged extension by hand, start the CLI with `-unsigned`. The
packaged file has no signature:

```sh
/tmp/duckdb-extension-native/native/duckdb -unsigned \
  -c "LOAD 'dev/extensions/dist/haskell_demo.duckdb_extension'; SELECT haskell_answer();"
```

If you created the `duckdb-nix` wrapper, use it here too, inside the Nix shell.
The plain CLI cannot load an extension that requires the newer glibc.

Each test case starts a plain native CLI process, loads the packaged
extension, and compares the CSV rows. Each registration kind has a case that
uses the registered object in SQL. The cases also check scalar results, 10,000
input rows, NULLs, and overflow. Error cases make a Haskell callback fail in the
scalar functions, the aggregate update and combine, the table function bind,
the cast, the replacement scan, COPY TO, COPY FROM, and the file readers. After
each error, a valid statement runs in the same process. `TRY_CAST` keeps the
NULL that the cast callback writes before it reports the error. One case runs
a second `LOAD` in the same process. DuckDB records the extension name at the
first `LOAD` and skips a later `LOAD` of the same name, so the entrypoint runs
once in each process.
The suite does not link DuckDB. Each CLI process must finish in 60 seconds,
including process exit and output collection. A timeout kills and reaps the
CLI child.

The CI matrix builds the example, packs it, and runs the test suite on Linux
and macOS with GHC 9.14.1 at optimization level 1. Each job uses a fresh
extension build directory and the matching native CLI.

## Deployment

The example targets Linux and macOS and links GHC and package libraries
dynamically. Its run paths point into the Cabal build directory, the Cabal
store, and the GHC or Nix installation. The packaged extension loads only on a
machine where those paths exist, and `cabal clean` breaks it. Two extensions
built in different projects can use the same `-inplace` library names. The
loader then reuses the first library, so do not load two such extensions in
one process. This example does not cover relocatable bundling, Windows, or
loading into another Haskell executable.

References: [Cabal foreign libraries](https://cabal.readthedocs.io/en/3.18/cabal-package-description-file.html#foreign-libraries),
[GHC runtime initialization](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/ffi.html#using-your-own-main),
and [shared-library deployment](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/shared_libs.html#finding-shared-libraries-at-runtime).
