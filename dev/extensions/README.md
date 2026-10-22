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

`duckdb_haskell_extension_init_api` negotiates the table before Haskell starts.
It copies the borrowed table into the FFI library and publishes it once under
a mutex. This mode supports one DuckDB implementation per process. Generated
C wrappers, the table, and managed callback helpers have hidden visibility.
The initializer and the demo's native entrypoint remain exported.
Extension mode omits interface unfoldings. This keeps calls to private C
wrappers inside the FFI shared library when a client uses optimization.

The demo initializes the threaded RTS once. It uses private arguments, ignores
`GHCRTS`, and disables the RTS signal handlers. It keeps the RTS alive for the
process lifetime. Do not call `hs_exit` after registration or database close.
DuckDB can still own Haskell callbacks, state, and destructor addresses.

The RTS runs with `-N1`, so callbacks from all DuckDB worker threads share one
Haskell capability. Users cannot change the RTS options. Haskell extensions
built with the same GHC share one RTS, and the options of the first loaded
extension apply. Extensions built with different GHC versions in one process
are not tested. If the RTS cannot start, the process ends.

`Extension.runExtension` passes an opaque `Extension` to the registration action.
`Extension.createFunction` and `Extension.createFunctionWithState` use the same
typed argument and result conversions as the connection API. The scoped
`Extension.withExtensionContext` helper gives advanced registration code access
to its borrowed target and context. These handles must not escape the entrypoint.
Initialization and callback exceptions become native errors. Managed
registration retains callback resources after the entrypoint returns.

`Extension.registerMacro`, `registerPragma`, `registerCustomOption`, and
`registerLogSink` use the same opaque extension value. Macro signatures and
custom-option defaults use `FactoryDefaultValue` during initialization.
Upstream marks the complete V2 API as unstable. An extension uses the
`C_STRUCT_UNSTABLE` function table, which DuckDB gives only to an extension
whose footer names the exact running release. `Pack.hs` writes the version of
the pinned V2 header. A later DuckDB release needs a `duckdb-ffi` release with
its headers. A rebuild against the same headers does not help.

The example registers typed scalar functions, scalar and table macros, a
PRAGMA, a typed custom option, and a log sink. They exercise native results,
batch inputs, NULLs, Unicode text, overflow, and callback errors. The entrypoint is
`haskell_demo_init_c_api_v2`. DuckDB derives the entrypoint name from the
file name, so the packaged file must be `haskell_demo.duckdb_extension`.

Registration code uses ordinary Haskell values and functions:

```haskell
register extension = do
    Extension.createFunction extension "answer" (42 :: Int64)
    Extension.createFunction extension "add" ((+) :: Int64 -> Int64 -> Int64)
```

See [Demo.hs](example/src/Demo.hs) for the complete entrypoint. GHC requires
the two native handle constructors in scope at its foreign export. Application
functions do not need native handles, vectors, or allocation calls.

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
case, create this wrapper and use its path for `--duckdb` in the smoke command.
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

nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- runghc \
  dev/extensions/Smoke.hs \
  --duckdb /tmp/duckdb-extension-native/native/duckdb \
  dev/extensions/dist/haskell_demo.duckdb_extension'
```

The smoke script starts two plain native CLI processes. It checks a second
`LOAD` of the loaded extension, which DuckDB skips, scalar results, 10,000
input rows, NULLs, callback errors, overflow, and connection reuse after
errors. It also checks both macro kinds, PRAGMA results before and after an
option update, and that the Haskell log sink receives entries. The entrypoint
runs once in each process.
It does not use a Haskell host executable. The timeout includes process exit
and output collection. A timeout kills and reaps the CLI child.

The CI matrix builds and loads the example on Linux and macOS with
GHC 9.14.1 at optimization level 1. Each job uses a fresh extension build
directory and the matching native CLI.

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
