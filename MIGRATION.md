# Migration Guide: DuckDB 1.4 to 1.5

This guide covers the changes needed when upgrading this repository from the
DuckDB 1.4 line to the DuckDB 1.5 line.

The short version is:

- `duckdb-ffi` now targets DuckDB `1.5.0+`.
- `duckdb-simple` now depends on `duckdb-ffi-1.5`.
- Existing 1.4 bindings continue to work, but the runtime `libduckdb` you load
  must be 1.5 or newer.
- New 1.5 functionality is exposed through additive modules and helpers; no
  wholesale rewrite is required.

## Who Needs to Change What

If you use `duckdb-ffi` directly:

- Rebuild and relink against DuckDB `1.5.0+`.
- Update any packaging, Nix, CI, Docker, or deployment config that still pulls
  a 1.4 `libduckdb`.
- If you want the new 1.5 APIs, import the new raw modules and bind against the
  new opaque handle types and callbacks.

If you use `duckdb-simple`:

- Rebuild against `duckdb-simple-0.1.3.0` and DuckDB `1.5.0+`.
- Existing query/statement code should continue to work unchanged.
- New 1.5 helpers are available from dedicated modules instead of being folded
  into the core query API.

## Runtime Compatibility

The biggest practical change is the runtime baseline.

Before:

- The packages were validated against DuckDB 1.4.x.

Now:

- `duckdb-ffi-1.5.0.0` and `duckdb-simple-0.1.3.0` require a DuckDB 1.5
  shared library at runtime.

If your executable still finds a 1.4 shared library first, you will see symbol
lookup failures for new 1.5 APIs such as config, catalog, or logging symbols.

Typical places to update:

- `LD_LIBRARY_PATH`
- Docker images
- CI build images
- Nix derivations
- system packages
- Cabal `extra-lib-dirs`

Example test invocation:

```bash
LD_LIBRARY_PATH=/path/to/duckdb-1.5 \
  cabal test --extra-lib-dirs=/path/to/duckdb-1.5
```

## `duckdb-ffi` Migration

### Header and Symbol Surface

The vendored `duckdb.h` now matches DuckDB 1.5.0.

The 1.5 change is additive for the C API surface used here:

- existing 1.4 function imports remain valid
- new 1.5 functions, enums, handles, and callbacks are now available

This means most direct FFI users do not need to rewrite existing code. Instead:

- keep existing bindings for legacy 1.4 APIs
- opt into the new 1.5 APIs where needed

### New Raw Modules

The new 1.5 areas are exposed through these modules:

- `Database.DuckDB.FFI.Catalog`
- `Database.DuckDB.FFI.CopyFunctions`
- `Database.DuckDB.FFI.FileSystem`
- `Database.DuckDB.FFI.Logging`

Use these when adopting new 1.5 functionality rather than trying to infer the
symbols manually from the header.

### New Types and Callbacks

DuckDB 1.5 introduces new opaque handles and enums that may affect your own FFI
layer if you were maintaining local bindings. In this repository they are now
provided centrally, including:

- file flags
- config option scopes
- catalog entry types
- copy-function handles and callback types
- file-system and file-handle handles
- catalog and catalog-entry handles
- log-storage handles
- scalar function init callbacks

If you had local downstream bindings for any of these, delete them and import
the shared versions from `Database.DuckDB.FFI.Types`.

### Error-Handling Guidance

DuckDB 1.5 adds more `duckdb_error_data`-style paths.

You do not need to rewrite all existing code to use them immediately.

Recommended approach:

- leave old bindings that already use legacy error accessors in place
- prefer the structured 1.5 error-data interface when binding new APIs

That is the policy used in this repository.

## `duckdb-simple` Migration

### Existing Query Code

Most `duckdb-simple` code should not need source changes.

The following remain source-compatible:

- `open`, `close`, `withConnection`
- prepared statements and named parameters
- `execute`, `query`, `fold`, `nextRow`
- `ToField`/`FromField`-based row and parameter handling
- existing scalar function registration with `createFunction`

### New Modules

DuckDB 1.5 features are exposed in additive modules:

- `Database.DuckDB.Simple.Config`
- `Database.DuckDB.Simple.Catalog`
- `Database.DuckDB.Simple.FileSystem`
- `Database.DuckDB.Simple.Copy`
- `Database.DuckDB.Simple.Logging`

Import them only where needed. The main `Database.DuckDB.Simple` module stays
focused on the core query interface.

### Opening with Config

If you previously created connections only with:

```haskell
open ":memory:"
```

you can keep doing that.

If you want startup config at open time, switch to:

```haskell
openWithConfig ":memory:" [("threads", "1")]
```

or:

```haskell
withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn -> ...
```

### Stateful Scalar Functions

DuckDB 1.5 adds scalar-function init/state hooks. In `duckdb-simple`, that is
surfaced as `createFunctionWithState`.

Use `createFunction` when:

- your scalar function is pure
- or all state can live in global `IORef`/`MVar` values that you manage

Use `createFunctionWithState` when:

- you want per-worker thread-local state
- you want state initialized once per worker thread, not once per row

Example:

```haskell
createFunctionWithState conn "hs_counter" (newIORef (0 :: Int)) $ \ref -> do
  atomicModifyIORef' ref $ \n ->
    let next = n + 1
     in (next, next)
```

This is an additive feature. Existing `createFunction` users do not need to
rewrite working code.

### Copy Functions

DuckDB 1.5 adds C APIs for custom `COPY` integrations. `duckdb-simple` now
exposes `registerCopyToFunction` in `Database.DuckDB.Simple.Copy`.

Use it when you want to receive rows emitted by:

```sql
COPY (SELECT ...) TO 'path' (FORMAT your_format)
```

The wrapper separates the callback phases:

- bind
- global init
- sink
- finalize

This mirrors DuckDB’s ownership model closely on purpose. If you had no custom
COPY integration before, nothing changes for normal SQL `COPY` usage.

### Logging

DuckDB 1.5 adds custom log storage registration. `duckdb-simple` now exposes
`registerLogStorage` in `Database.DuckDB.Simple.Logging`.

This is optional advanced functionality. Existing applications do not need to
change anything unless they want to capture DuckDB log events.

## Behavior Changes Worth Noting

### `TIME_NS` Decoding

The test suite was updated because DuckDB 1.5 now successfully decodes
`TIME_NS`, where older expectations treated that as unsupported.

For most users this is a strict improvement:

- code that already handled `FieldTime` continues to work
- tests that expected a failure for `TIME_NS` should be updated

### `duckdb_string_t` Handling

DuckDB 1.5 made it more obvious that `duckdb_string_t_data` must be consumed
with its explicit length, not by assuming NUL termination.

If you have downstream helper code using:

```haskell
peekCString ...
```

on `duckdb_string_t_data`, switch to a length-aware read such as:

```haskell
peekCStringLen ...
```

using `duckdb_string_t_length`.

## Suggested Upgrade Steps

1. Upgrade the Haskell packages to:
   - `duckdb-ffi-1.5.0.0`
   - `duckdb-simple-0.1.3.0`
2. Upgrade the native DuckDB shared library to `1.5.0+`.
3. Run your test suite with the 1.5 shared library explicitly selected.
4. Update any tests that expected old 1.4 behavior, especially around
   `TIME_NS` or string helper assumptions.
5. Adopt the new modules only where you need 1.5-specific features.

## Repository Notes

Relevant release notes live in:

- [duckdb-ffi/CHANGELOG.md](duckdb-ffi/CHANGELOG.md)
- [duckdb-simple/CHANGELOG.md](duckdb-simple/CHANGELOG.md)

If you are upgrading code in this repository itself, use those together with
this guide.
