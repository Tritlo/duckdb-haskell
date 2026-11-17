# Package the Haskell demo extension

`Pack.hs` copies a built foreign library and appends unsigned DuckDB extension
metadata. It runs with `runghc` and the standard GHC packages. It does not build
the library or change the input file.

Supply the exact library path from the build output. Name the output
`NAME.duckdb_extension`, where `NAME_init_c_api_v2` is the load entrypoint.
DuckDB uses the file name to find it. For the demo, use
`haskell_demo.duckdb_extension`. The input library can keep its Cabal build
file name. Run the script from the repository root.

```sh
nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- runghc \
  dev/extensions/Pack.hs /absolute/path/to/libhaskell_demo.so \
  --output /absolute/path/to/haskell_demo.duckdb_extension \
  --platform linux_amd64 \
  --extension-version 0.1.0'
```

Set `--platform` to the exact DuckDB platform that matches the library. For
example, a native Apple Silicon library can use `osx_arm64`. Linux platform
labels can include ABI suffixes. The script does not infer a platform from the
host or check the library's CPU architecture. It checks the platform's operating
system family against the library format.

The script accepts 64-bit ELF shared objects on Linux and thin 64-bit Mach-O
libraries or bundles on macOS. It rejects Windows DLLs, static archives, and
universal Mach-O files.

The footer uses the `C_STRUCT_UNSTABLE` ABI. DuckDB loads it only into the
release named in its version field. The script reads that version from the
`DUCKDB_V2_API_VERSION_*` definitions in `duckdb-ffi/cbits/duckdb_v2.h`. For the
pinned headers, the field is `v2.0.0`. The version is not an option, because
the function table of another release can differ.

Each metadata field must contain at most 32 ASCII bytes. Required fields must
be nonempty and must not contain NUL. The script rejects input that already has
a DuckDB metadata footer. It requires different input and output paths and
refuses to overwrite any existing output. Remove an old output explicitly
before packaging it again.

The 534-byte suffix follows the pinned DuckDB
[`scripts/append_metadata.cmake`](https://github.com/duckdb/duckdb/blob/d54cbbe3df17e8e5ce68312f483b4155836112aa/scripts/append_metadata.cmake):
a 22-byte custom-section prefix, eight reversed 32-byte metadata fields, and a
256-byte zero signature. The fields record metadata format `4`, platform,
DuckDB version, extension version, ABI type, and three empty reserved fields.
The zero signature does not authenticate the library. Native loading requires
DuckDB's unsigned-extension option.

The packer does not verify exported symbols or native dependency resolution.

## Ubuntu 24.04 directory bundle

`build-linux.sh` builds and verifies a directory bundle for Linux x86_64 on
Ubuntu 24.04 or later. The supported host is the pinned native DuckDB 2.0.0 CLI
with one Haskell extension. Haskell host executables and multiple independently
built Haskell extensions are not supported. This path does not target macOS or
Windows. The Docker client can run on Windows; the output is a Linux extension.

Generate and verify the four ignored binding source files first. Then run from
the repository root with Docker available:

```sh
bash dev/extensions/build-linux.sh /absolute/new-output-directory
```

Set `DOCKER` to the Docker client executable when it is not named `docker`.
The script streams source into a pinned Ubuntu container. It does not mount the
build tree or use Nix libraries. It installs checksum-verified GHC 9.14.1 and
Cabal 3.18.1.0 release archives. The build container has a 12 GiB memory limit,
no swap, and two CPUs. Cabal uses one build job. The verification container has
a 3 GiB memory limit, no swap, and one CPU. Run the complete command through the
serial heavy-job wrapper when that wrapper is required by the host.

The output contains `bundle/haskell_demo.duckdb_extension`, `bundle/lib/`,
`bundle/manifest.json`, and `bundle/licenses/`. Move the complete `bundle`
directory together. Load its extension with the native CLI and `-unsigned`.
The manifest records dependency hashes, SONAMEs, required symbol versions,
toolchain versions, the base image, native archive checksums, and license
provenance. Boot-package and bundled libffi notices come from the matching
checksum-verified GHC source archive. Native copyright files include their
referenced common-license texts. License source archives retain the dependency
notices and sources.
The output also contains verification fixtures and build metadata. These are
not part of the deployable bundle.

The packer copies non-glibc dependencies. It rejects unresolved dependencies,
different libraries with the same name, non-x86_64 ELF objects, native DuckDB
imports, missing license provenance, and glibc requirements newer than 2.39.
Each bundled library uses a relative `$ORIGIN` search path. The extension uses
`$ORIGIN/lib`. The script changes the raw ELF files before `Pack.hs` appends the
exact-release footer. It does not bundle glibc or its loader.

A fresh Ubuntu container receives only the bundle and verification tools. The
script moves the bundle to a path with spaces, checks hashes and dependency
resolution, checks rejection of incompatible footer versions and platforms,
and runs the existing extension integration suite. The compiler, Cabal store,
source tree, and original build directory are absent from this container.

On failure, the script retains its task containers and prints their exact cleanup
command. Successful runs remove their task containers.
