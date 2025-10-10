## duckdb-simple Plan

A small, ergonomic “simple” interface for DuckDB modeled after the familiar simple-DB style. The initial scope focuses on opening/closing a database, executing parameterized statements, querying rows into Haskell types, and the FromRow/ToRow ecosystem. We deliberately keep the first iteration tight and skip advanced DuckDB features.

We will use the existing FFI in `duckdb-ffi/src/Database/DuckDB/FFI.hs` as the sole binding surface. The reference copy of sqlite-simple under `reference/sqlite-simple` guides API shape and module layout (naming adapted for DuckDB).

### Goals and Scope (v0)

- Provide a compact public API with:
  - `open`, `close`, `withConnection`
  - `execute`, `execute_`, `executeMany`, `executeNamed`
  - `query`, `query_`, `queryNamed`, and `queryWith` variants
  - `FromRow`, `ToRow`, `FromField`, `ToField`, plus `Only`, `Query`, and `(:.)`
  - Minimal statement API: `openStatement`, `closeStatement`, `withStatement`, `bind`, `bindNamed`
  - Basic transaction helpers: `withTransaction`, `withImmediateTransaction`, `withExclusiveTransaction`, `withSavepoint`
- Keep behavior and ergonomics close to the simple pattern: positional params (`?`), named params (`:name`), typed conversion via `FromRow`.
- Internals backed by `Database.DuckDB.FFI.*` only; no direct C or other libs.
- Skip the QQ module.

Non-goals for v0 (defer):
- Arrow, appender, table/aggregate/replacement scans, custom scalar functions.
- Full type coverage (we start with a pragmatic core set).
- High-performance chunked streaming and vectors (we’ll add a phase using `ResultFunctions` and `DataChunk` later).
- Connection pooling and advanced configuration APIs.

### Public API Surface (initial)

- Module `Database.DuckDB.Simple` (primary entry):
  - Types: `Query(..)`, `Connection(..)` (opaque), `Statement(..)` (opaque), `Only(..)`, `(:.)(..)`, `NamedParam(..)` (`:=`), `ColumnIndex(..)`
  - Typeclasses: `ToRow(..)`, `FromRow(..)`
  - Connections: `open :: String -> IO Connection`, `close :: Connection -> IO ()`, `withConnection :: String -> (Connection -> IO a) -> IO a`
  - Exec (no result rows): `execute`, `execute_`, `executeMany`, `executeNamed`
  - Query (with result rows): `query`, `query_`, `queryNamed`, `queryWith`, `queryWith_`
  - Fold/streaming (materialized first in v0; true streaming in v1): `fold`, `fold_`, `foldNamed`
  - Prepared statements (low level): `openStatement`, `closeStatement`, `withStatement`, `bind`, `bindNamed`
  - Metadata helpers: `columnName`, `columnCount`
  - Transactions: `withTransaction`, `withImmediateTransaction`, `withExclusiveTransaction`, `withSavepoint`
  - Effects counters: `changes` (rows changed in last exec). `totalChanges` and `lastInsertRowId` are not directly exposed by DuckDB; we’ll omit or emulate carefully (see “Open Questions”).
  - Exceptions: `FormatError`, `ResultError`, and basic `SQLError` wrapper for prepare/execute errors.

- Module `Database.DuckDB.Simple.Types`:
  - `Query`, `Only`, `(:.)`, `Null` (if useful for `Maybe`/nullable semantics)

- Module `Database.DuckDB.Simple.Internal`:
  - `Connection` newtype storing both `DuckDBDatabase` and `DuckDBConnection` handles
  - `Statement` wrapper for `DuckDBPreparedStatement`
  - `RowParser`, `Field` metadata, and internal machinery (adapted from the reference) used by `FromRow`

- Module `Database.DuckDB.Simple.FromField` and `ToField`:
  - Define conversion classes and core instances for `Int[8/16/32/64]`, `Word[8/16/32/64]`, `Int`, `Integer`, `Double`, `Float`, `Bool`, `Text`, `String`, `ByteString`, lazy variants, and `Maybe a`
  - Time/Date types can be added after core primitives

- Module `Database.DuckDB.Simple.FromRow` and `ToRow`:
  - Expose tuple/list instances and generic derivation (as in the reference), built atop `FromField`/`ToField`

- Module `Database.DuckDB.Simple.Ok`:
  - The lightweight `Ok` error-accumulator used by `RowParser`, same as the reference

### Implementation Plan

We’ll implement in phases to keep risk low and tests incremental.

#### Phase Acceptance Criteria

For a phase to be accepted we must:

- Add or extend an automated test suite that exercises every newly exported function (and significant new datatype behavior) introduced in that phase.
- Write Haddock documentation for every exported module, type, class, and function touched in that phase.
- If the test suite fails at any point, capture the failure mode and takeaways in `duckdb-simple/NOTES.md` before proceeding.
- Format every Haskell source touched in the phase with `fourmolu -q -i <file>.hs` before submitting.

These requirements apply cumulatively as we progress; later phases cannot ship until all earlier commitments remain satisfied.

#### Phase 0 — Scaffolding and build setup
- Add `duckdb-simple.cabal` and hook into `cabal.project`.
- Create module skeletons listed above with minimal exports.
- Add internal re-exports and aliases aligned with the reference layout.

#### Phase 1 — Connections and statements
- Connection management using FFI:
  - Open: `c_duckdb_open` + `c_duckdb_connect`
  - Close: `c_duckdb_disconnect` + `c_duckdb_close`
  - `withConnection` via `bracket`.
- Prepared statement lifecycle:
  - Create: `c_duckdb_prepare`
  - Destroy: `c_duckdb_destroy_prepare`
  - Clear bindings between reuses: `c_duckdb_clear_bindings`
  - Named parameter index: `c_duckdb_bind_parameter_index`
- Result lifecycle:
  - Materialized execution: `c_duckdb_query` (for `query_` and simple cases)
  - For prepared execution: `c_duckdb_execute_prepared`
  - Destroy results: `c_duckdb_destroy_result`

#### Phase 2 — Parameters and ToField/ToRow
- Define `ToField` returning an internal parameter representation. Two viable approaches:
  1) Use per-type binders: `c_duckdb_bind_int64`, `c_duckdb_bind_double`, `c_duckdb_bind_varchar`, `c_duckdb_bind_blob`, `c_duckdb_bind_null`, etc. (fastest, minimal allocations)
  2) Use value objects: Build a `DuckDBValue` via `c_duckdb_create_*` and bind using `c_duckdb_bind_value`, then `c_duckdb_destroy_value`. (simpler logic; acceptable for v0)
- For v0 we will use approach (2) for simplicity. We can replace internals with (1) later without changing the public API.
- Implement `ToRow` by mapping containers/tuples/lists into a flat `[Param]`, then a binder that walks and binds by index (1-based) to the prepared statement.
- Add `NamedParam` (`:=`) support by resolving indices with `c_duckdb_bind_parameter_index`.
- Raise `FormatError` when placeholder counts mismatch or unexpected named parameters appear.

#### Phase 3 — Results and FromField/FromRow
- v0 materialized reads:
  - For `query_`: `c_duckdb_query` returns a `DuckDBResult` — use column/row counts via `c_duckdb_column_count` and `c_duckdb_row_count`.
  - Extract cell values with safe fetch functions: `c_duckdb_value_is_null`, `c_duckdb_value_boolean`, `c_duckdb_value_int64`, `c_duckdb_value_double`, `c_duckdb_value_varchar`/`c_duckdb_value_string`, `c_duckdb_value_blob`, etc. Convert them into a local SQLData-like sum, then run `RowParser` over that row.
  - Always call `c_duckdb_destroy_result`.
- For prepared queries: after `c_duckdb_execute_prepared`, read like above.
- Implement `FromField` in terms of a `Field` containing the per-cell SQLData plus column index, closely mirroring the reference behavior and error messages.
- Implement `FromRow` and `RowParser` machinery (generic derivation, tuple instances) as in the reference.

#### Phase 4 — Query helpers, transactions, and counters
- `execute`, `execute_`, `executeMany`, `executeNamed` built on `withStatement` + `bind` + stepping via `c_duckdb_execute_prepared` (materialize and discard rows), and expose affected-row count via `c_duckdb_rows_changed`.
- `query`, `query_`, `queryNamed`, `queryWith`, `queryWith_` materialize results and convert to requested `FromRow`.
- Transactions implemented by executing `BEGIN`, `COMMIT`, `ROLLBACK` variants with `execute_`. Helpers: `withTransaction`, `withImmediateTransaction`, `withExclusiveTransaction`, `withSavepoint` mirror the reference.
- `changes` returns `c_duckdb_rows_changed` of the last result for `execute*` operations. DuckDB does not expose `totalChanges` or `lastInsertRowId` directly; we defer or emulate carefully (see below).

#### Phase 5 — Errors and exceptions
- Wrap prepare and execution failures:
  - Prepare: `c_duckdb_prepare_error` on failure (convert `CString` to `Text` and raise an error value)
  - Execute/query: `c_duckdb_result_error` + `c_duckdb_result_error_type` on `DuckDBError`
- `FormatError` for placeholder mismatches and named-parameter issues.
- `ResultError` mirrors reference error constructors for conversions (`Incompatible`, `UnexpectedNull`, `ConversionFailed`).

#### Phase 6 — Documentation and examples
- README with short examples for `open/close`, `execute`, `query` using tuples and records with `FromRow`.
- Note resource management guarantees: every `DuckDBResult`, `DuckDBPreparedStatement`, and `DuckDBValue` is destroyed.
- Document mapping subset and future work.

### Future Work (v1+)

- Chunked/streaming results for large datasets:
  - Use `c_duckdb_execute_prepared_streaming` and `c_duckdb_result_get_chunk` with `DuckDBDataChunk` + `DuckDBVector` to drive `fold` without materializing all rows.
  - Provide a fast-path `FromField` for primitive vectors to reduce overhead.
- Switch `ToField` binding path to per-type binders (`BindValues`) for performance.
- Time/date types (map to `c_duckdb_value_timestamp`, `c_duckdb_value_date`, etc.) with parsers compatible with the reference’s Time module.
- Add `columnName`/`columnCount` wrappers using `c_duckdb_column_name`/`c_duckdb_column_count` on results and prepared statements.
- Optional query logging hook (lightweight trace analog implemented in Haskell).
- Investigate `totalChanges` and `lastInsertRowId` equivalents:
  - `rows_changed` exists for last statement (`c_duckdb_rows_changed`).
  - For “last insert id”: DuckDB does not have a direct API; we can recommend `RETURNING` clauses or sequences, or consider an extension helper.
- Add tests adapted from `reference/sqlite-simple/test` for the covered API surface.

### Key FFI Touchpoints

Use only functions re-exported by `Database.DuckDB.FFI`:
- Open/connect: `c_duckdb_open`, `c_duckdb_close`, `c_duckdb_connect`, `c_duckdb_disconnect`
- Prepare/execute: `c_duckdb_prepare`, `c_duckdb_destroy_prepare`, `c_duckdb_execute_prepared`
- Bindings: `c_duckdb_bind_value` (v0) and/or `c_duckdb_bind_*` family; `c_duckdb_bind_parameter_index`
- Direct query: `c_duckdb_query`
- Results: `c_duckdb_destroy_result`, `c_duckdb_column_count`, `c_duckdb_row_count`, `c_duckdb_rows_changed`, `c_duckdb_result_error`, `c_duckdb_result_error_type`
- Safe fetch (for v0 cell-by-cell): `c_duckdb_value_is_null`, `c_duckdb_value_boolean`, `c_duckdb_value_int64`, `c_duckdb_value_double`, `c_duckdb_value_varchar`/`c_duckdb_value_string`, `c_duckdb_value_blob`, etc.
- Values (for parameter creation when using `bind_value`): `c_duckdb_create_*`, `c_duckdb_destroy_value`

### Deliverables Checklist

- Build scaffolding and empty modules
- Connection + simple exec/query working against DuckDB
- `ToField`/`FromField`, `ToRow`/`FromRow` with core instances
- Prepared statements + positional and named params
- Error types and propagation
- Transactions helpers
- Minimal docs + examples
- Tests ported/adapted from reference for covered features

### Open Questions / Decisions

- `lastInsertRowId`/`totalChanges` parity: no direct primitives in the FFI; we will omit in v0 and document alternatives (`RETURNING`, sequences), adding later if DuckDB exposes equivalents.
- Fold/streaming: v0 may materialize then fold for simplicity; v1 will switch to chunked reads.
- Exact surface parity vs. pragmatic subset: we intentionally keep a smaller initial surface to ship early and iterate.

---

This plan intentionally mirrors the structure and feel of the reference project while grounding all implementation details in the DuckDB FFI we already ship. It should enable a quick v0 that’s useful for many applications, with a clear path to performance and feature expansion.
