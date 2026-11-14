# Maintainer tools

These files support development and release work. Hackage packages do not
include them. Package source archives contain the generated bindings, C ABI
checks, client headers, and upstream license.

- `bindings/` generates both client APIs and their ABI checks.
- `update-duckdb.sh` pins a DuckDB release tag. It updates the client and
  extension headers, the upstream license, and all native digests.
- `release.sh` generates and checks the bindings, then builds and uploads
  authorized Hackage candidates or releases.
- `build-ghc-matrix.sh` runs local Docker builds. Generate the bindings first.
  It writes logs to `ci-logs/`.
- `nix/` contains pinned development, generation, and CI environments.
- `v2-migration.md` describes the V2 backend contracts and native limits.
- `extensions/` prepares a loadable Haskell extension for the pinned release.
  Its packer uses `runghc` with GHC's bundled packages. Its test suite
  starts the DuckDB CLI with the packaged extension.
- `fetch-duckdb.sh` downloads the release library and CLI from
  `duckdb-ffi/native.sha256` for local, CI, and Docker validation. It verifies
  the archive checksums and compares the release headers with the vendored
  headers.

See [CONTRIBUTING.md](../CONTRIBUTING.md) for the generation command and the
release steps.

CI caches the generated files by generator, toolchain, headers, and expected
checksums. An exact cache hit skips Nix setup and generation. Every run checks
the header digests and the generated checksums before it uploads the generated
artifact. A cache miss runs the pinned generator.
