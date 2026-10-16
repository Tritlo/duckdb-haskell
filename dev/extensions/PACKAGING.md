# Package the Haskell demo extension

`Pack.hs` copies a built foreign library and appends unsigned DuckDB extension
metadata. It runs with `runghc` and the standard GHC packages. It does not build
the library or change the input file.

Supply the exact library path from the build output. Keep the output basename
`haskell_demo.duckdb_extension`. DuckDB uses that name to find the demo's load
entrypoint. The input library can retain its Cabal build filename.

```sh
nix-shell dev/nix/shell.nix --run 'GHC_ENVIRONMENT=- runghc \
  dev/extensions/Pack.hs /absolute/path/to/libhaskell_demo.so \
  --output /absolute/path/to/haskell_demo.duckdb_extension \
  --platform linux_amd64 \
  --duckdb-version 13d1d8f56a \
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

The footer uses the `C_STRUCT_UNSTABLE` ABI. Its DuckDB version field must match
the native loader's version exactly. For the pinned development source, that
value is the ten-character source ID `13d1d8f56a`. It is not the reported
`v2.0.0-dev0` library version. Release builds use a normalized version tag such
as `v2.0.0`. Read the target runtime's `PRAGMA version` and `PRAGMA platform`
results before packaging for another build. The script records the supplied
values without version normalization or runtime inspection.

Each metadata field must contain at most 32 ASCII bytes. Required fields must
be nonempty and must not contain NUL. The script rejects input that already has
a DuckDB metadata footer. It requires different input and output paths and
refuses to overwrite any existing output. Remove an old output explicitly
before packaging it again.

The 534-byte suffix follows the pinned DuckDB
[`scripts/append_metadata.cmake`](https://github.com/duckdb/duckdb/blob/13d1d8f56a82b3413055819eb90d0b0e68b5c847/scripts/append_metadata.cmake):
a 22-byte custom-section prefix, eight reversed 32-byte metadata fields, and a
256-byte zero signature. The fields record metadata format `4`, platform,
DuckDB version, extension version, ABI type, and three empty reserved fields.
The zero signature does not authenticate the library. Native loading requires
DuckDB's unsigned-extension option.

The generated suffix matches the pinned official CMake writer byte for byte.
The check used a temporary library fixture and verified that the input did not
change. It also checked invalid fields, existing outputs, repeated metadata,
and supported native headers. The packer does not verify exported symbols or
native dependency resolution.
