# Maintainer tools

These files support development and release work. Hackage packages do not
include them. Package source archives contain the generated bindings, C ABI
checks, client headers, and upstream license.

- `bindings/` generates both client APIs and checks header provenance, API
  coverage, and ABI changes. These tools remain useful after the final release.
- `release.sh` generates and checks the bindings, then builds and uploads
  authorized Hackage candidates or releases.
- `build-ghc-matrix.sh` runs local Docker builds. It writes logs to `ci-logs/`.
- `nix/` contains pinned development, generation, and CI environments.
- `preview/` builds the pinned development runtime. See its README for the
  removal condition.
- `diagnostics/` contains a native probe for the deprecated Arrow API. Set
  `DUCKDB_LIB_PATH` to the library under test.
- `docs/` contains the C API documentation snapshot and its fetch tool. Run the
  fetch tool with `uv run --with requests --with beautifulsoup4 dev/docs/fetch.py`.

Generate and audit bindings from a checkout:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
uv run dev/bindings/duckdb-api.py --check
```

Package users do not need these tools, Nix, LLVM, or hs-bindgen.

CI caches the generated files by generator, toolchain, headers, manifest, and
expected checksums. An exact cache hit skips Nix setup and generation. Every
run checks the file checksums, header provenance, and API coverage before it
uploads the generated artifact. A cache miss runs the pinned generator.
