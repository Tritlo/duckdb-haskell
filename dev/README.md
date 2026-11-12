# Maintainer tools

These files support development and release work. Hackage packages do not
include them. Package source archives contain the generated bindings, C ABI
checks, client headers, and upstream license.

- `bindings/` generates both client APIs and their ABI checks.
- `update-duckdb.sh` pins a DuckDB release tag. It updates the client headers,
  the upstream license, and all native digests.
- `release.sh` generates and checks the bindings, then builds and uploads
  authorized Hackage candidates or releases.
- `build-ghc-matrix.sh` runs local Docker builds. Generate the bindings first.
  It writes logs to `ci-logs/`.
- `nix/` contains pinned development, generation, and CI environments.
- `fetch-duckdb.sh` downloads the release library from
  `duckdb-ffi/native.sha256` for local, CI, and Docker validation. It verifies
  the archive checksum and compares the release headers with the vendored
  headers.

Generate and check the bindings from a checkout:

```sh
nix-shell dev/nix/generate.nix --run 'dev/bindings/generate-bindings.sh --check'
```

Package users do not need these tools, Nix, LLVM, or hs-bindgen.

CI caches the generated files by generator, toolchain, headers, and expected
checksums. An exact cache hit skips Nix setup and generation. Every run checks
the header digests and the generated checksums before it uploads the generated
artifact. A cache miss runs the pinned generator.
