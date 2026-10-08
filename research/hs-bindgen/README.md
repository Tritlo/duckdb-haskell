# DuckDB 1.5.6 with hs-bindgen

This spike starts from `main` at `e75bc39`.
It uses hs-bindgen 1.0.0.0 and the pinned DuckDB 1.5.6 header.
It does not change either production package.

## Replacement target

Generate the raw API from `duckdb.h` and `duckdb_arrow.h`.
Adapt `duckdb-simple` to the generated types and functions.
Keep its ownership rules and value conversion code.
Do not preserve the old raw FFI signatures.

The inspection run generates all 546 native functions.
The complete generated API compiles and links with GHC 9.14.1.
The executable passes its native checks against DuckDB 1.5.6 on Linux x86_64.
Valgrind reports zero memory errors and zero definite or indirect lost bytes.
It reports 416 possibly lost bytes and 4,766,943 reachable bytes at process exit.
This is feasibility evidence.
The production packages have not been migrated.

The current FFI has this source inventory:

| Component | Count |
| --- | ---: |
| Haskell modules | 44 |
| Haskell source lines | 8,482 |
| Bound native DuckDB functions | 546 |
| Foreign imports | 564 |
| `Types.hs` lines | 1,841 |
| Manual `Storable` instances | 22 |
| C helper functions | 94 |
| C helper source lines | 714 |
| Native installer source lines | 134 |

The 546 native functions comprise 458 direct imports and 88 ABI wrappers.
The other six C helpers inspect or clear deprecated Arrow release callbacks.
The remaining 12 foreign imports construct or invoke Arrow callbacks.

`duckdb-simple` uses 23 functions behind the ABI wrappers.
The inspected calls supply valid records, output storage, or vector elements.
They do not depend on the wrappers' NULL defaults.
It does not call the six deprecated Arrow C helpers.
Thus a new raw API can remove the complete handwritten `duckdb_stub.c`.
Applications that use the old raw API must change their calls.

## Work that remains handwritten

Keep the native installer in `Setup.hs`.
hs-bindgen does not install or select the DuckDB library.

Keep result and chunk ownership, cancellation, and borrowed-buffer lifetimes.
Keep callback ownership transfer, exception containment, and `StablePtr` cleanup.
Keep Arrow release and move rules.
Keep timestamp, UUID, DECIMAL, BIT, BIGNUM, and nested-value conversion.
These rules belong in `duckdb-simple`.
They are not C declarations that hs-bindgen can infer.

The two static callback destructors in `Callback.hs` still need their Haskell
`foreign export` declarations.
The six deprecated Arrow C helpers can be omitted with the old raw API.
If they remain public, implement them in Haskell with the generated Arrow fields.

## Adaptation scope

21 of the 28 `duckdb-simple` source modules import FFI directly.
They reference 203 native functions, 60 raw types, 53 enum patterns, and 24
record selectors.
Most changes concern names, handle constructors, const pointers, and records.
Struct conversion is concentrated in `Element.hs` and `ToField.hs`.
Use the generated `duckdb_string_t` layout instead of its hardcoded 16-byte size.
Preserve the lifetime state machines in `Internal.hs` and `Result.hs`.
Preserve the callback rules in `Callback.hs`, `Function.hs`, `Copy.hs`, and
`Logging.hs`.

Some public types expose the raw API.
These include `SQLError`, `LogicalTypeRep`, `CopyBindInfo`, filesystem functions,
and Arrow callbacks.
An adaptation must update those types too.

## Build approach

`DuckDB.hs` uses Template Haskell to generate safe imports during compilation.
It also generates the C wrappers for structs passed or returned by value.
No handwritten C source participates in this spike.
The generator tracks the included headers with `addDependentFile`.
This avoids the header-change limitation of hs-bindgen's literate mode.

Generate for each build platform.
Generated layouts are specific to the target ABI.
Do not use one Linux-generated file as the source for all supported platforms.
The `generate.sh` script produces a readable copy for inspection only.
The executable compiles `DuckDB.hs` directly.

GHC 9.14 needs bound overrides for `debruijn` and `skew-list`.
The project limits these overrides to their `base` bounds.
The Nix shell uses GHC 9.14.1 and LLVM/Clang 21.1.8.
Use the GHC and C compiler from the same shell.
Mixing the host GHC with Nix's newer glibc fails during Template Haskell loading.

Default field prefixes cause a collision between the DECIMAL `width` and
`scale` fields and the `duckdb_decimal_width` and `duckdb_decimal_scale` functions.
Use `OmitFieldPrefixes`, `DuplicateRecordFields`, and `NoFieldSelectors`.
The initial generation omitted eight functions because of this collision.
The corrected run generates all 546 native functions.
It also generates the VARCHAR union, Arrow records, and callback conversions.
The inspection output has 49,497 lines with Doxygen comments.
These lines are generated artifacts.
The three untranslated macros are `DUCKDB_C_API`, `DUCKDB_EXTENSION_API`, and
`DUCKDB_DEPRECATED`.
They contain compiler attributes.
Clang still applies those attributes when it parses the declarations.
No native function is missing after generation.

## Run the spike

Run these commands from `research/hs-bindgen`.
Supply the directory that contains the DuckDB 1.5.6 shared library.
The executable checks the runtime version.

```sh
cat > cabal.project.local <<'EOF'
package hs-bindgen-spike
  extra-lib-dirs: /absolute/path/to/duckdb/lib
  ghc-options: -optl-Wl,-rpath,/absolute/path/to/duckdb/lib
EOF
nix-shell shell.nix --run 'cabal run hs-bindgen-spike'
```

Cabal builds hs-bindgen from Hackage.
Clang and Doxygen come from the pinned Nix shell.
The probe uses no `duckdb-ffi` library and no handwritten C wrappers.

The native checks cover these operations:

- Open and close a database and connection.
- Bind and execute a prepared statement.
- Stream 5,000 ordered rows across multiple chunks.
- Pass and return date and DECIMAL structs by value.
- Read inline and allocated VARCHAR values with the generated union layout.
- Construct a Haskell callback pointer and invoke it through a SQL function.
- Export an Arrow array, read its data, and invoke its release callback.
- Destroy query results after native errors.

Cancellation, callback failure, registration ownership transfer, the complete
`duckdb-simple` suite, and other platforms remain outside this probe.
The existing integration and leak suites must pass after a production migration.

## Proposed migration

1. Keep `duckdb-ffi` as the package that installs the native library and generates
   the raw API. Replace its raw modules with a header-driven module.
2. Adapt `duckdb-simple` to the generated names and representations. Keep the
   existing ownership and conversion rules. Move Arrow ownership helpers there.
3. Remove the handwritten C shims and the old binding generator. Update the
   existing FFI and integration tests to the new raw API.
4. Run the existing compiler, native-version, platform, and leak checks. Keep
   generation in the build so each platform obtains its own layouts.

The handwritten FFI maintenance target becomes the header pin, generation
configuration, and native installer. A new C declaration does not need a
matching handwritten Haskell declaration or C ABI shim.

## Sources

- [hs-bindgen 1.0 announcement](https://well-typed.com/blog/2026/10/hs-bindgen-official-release/)
- [hs-bindgen 1.0 package](https://hackage.haskell.org/package/hs-bindgen-1.0.0.0)
- [Invocation manual](https://github.com/well-typed/hs-bindgen/blob/main/manual/low-level/usage/invocation.md)
- [Portability manual](https://github.com/well-typed/hs-bindgen/blob/main/manual/low-level/usage/portability.md)
