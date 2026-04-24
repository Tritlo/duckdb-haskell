# duckdb-haskell
## duckdb-ffi

Available on [Hackage](https://hackage.haskell.org/package/duckdb-ffi).
`duckdb-ffi` exposes comprehensive Haskell bindings to the
[DuckDB](https://duckdb.org) [C API](https://duckdb.org/docs/api/c/overview)
by generating FFI shims directly from the official `duckdb.h` header.

### Highlights

- Mirrors the public C API surface, covering connections, prepared statements,
  result sets, vectors, logical types, appenders, Arrow integration, and more.
- Provides both raw imports and curated modules under `Database.DuckDB.FFI.*`
  so you can pick between granular or aggregate re-exports.
- Ships with an exhaustive test suite that exercises every binding to catch
  signature drift when DuckDB evolves.
- Validated against DuckDB 1.5.0+ releases.

### Installation

`cabal build` automatically downloads the platform-specific `libduckdb` shared
library from the [DuckDB GitHub releases](https://github.com/duckdb/duckdb/releases)
and caches it under `~/.cache/duckdb/<version>/<platform>/` (XDG cache).
No manual installation is required on Linux (x86_64) or macOS.

The following environment variables customise this behaviour:

* `DUCKDB_VERSION`: (default `1.5.0`) DuckDB release to download
* `DUCKDB_HOME`: (`~/.cache/duckdb`) Override the cache root directory
* `DUCKDB_SKIP_DOWNLOAD`: (default *(unset)*) Set to any value to skip the download and link against a system-installed `libduckdb` instead.

NB: Nix builds are detected via a heuristic `NIX_BUILD_TOP` and the download is skipped; `libduckdb` must be provided by the Nix environment in that case (the current flake does not do this).

`duckdb-ffi` is ideal when you need precise control over DuckDB’s C API, want to
interoperate with other native components, or plan to build higher-level
abstractions.

For upgrading notes from DuckDB 1.4-based releases to the new 1.5 line, see
[MIGRATION.md](MIGRATION.md).

## duckdb-simple

Available on [Hackage](https://hackage.haskell.org/package/duckdb-simple).
`duckdb-simple` builds on `duckdb-ffi` to provide a high-level interface in the
style of `sqlite-simple`/`postgresql-simple`, combining ergonomic helpers with
typeclass-driven parameter binding and row decoding.

### Highlights

- Connection and statement helpers (`withConnection`, `withStatement`,
  `execute`, `query`, etc.) plus streaming primitives (`fold`, `foldNamed`,
  `fold_`, `nextRow`) that expose DuckDB’s chunked result processing without
  materialising entire result sets.
- Comprehensive scalar type coverage out of the box: signed/unsigned integers,
  HUGEINT/UHUGEINT, decimals (with correct width/scale), intervals, precise and
  timezone-aware temporals, enums, bit strings, blobs, and bignums. Standard
  `ToField`/`FromField` instances make round-trips seamless.
- Support for positional (`?`) and named (`$name`) parameters, with detailed
  diagnostics when placeholders and bindings disagree.
- `FromRow`/`ToRow` combinators—complete with generic deriving—for mapping
  product types to query results or parameter lists.
- Scalar function registration via `Database.DuckDB.Simple.Function`, allowing
  Haskell code (including `Maybe`-returning and `IO` actions) to be invoked
  directly from SQL expressions.
- Transaction helpers (`withTransaction`, `withSavepoint`) and metadata
  utilities (`columnCount`, `columnName`, `rowsChanged`).

See [duckdb-simple/README.md](duckdb-simple/README.md) for a step-by-step guide,
extended examples, and notes on streaming behaviour and user-defined
functions.
