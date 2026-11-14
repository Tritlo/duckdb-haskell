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
