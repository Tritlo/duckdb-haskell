# duckdb-simple Roadmap (October 2024)

## Current Status Summary

- **Core API**: `Database.DuckDB.Simple` exposes connections, prepared
  statements, `execute*` and `query*` helpers, transaction wrappers, and
  user-defined scalar functions. Positional and named parameter binding now run
  preflight checks and surface `FormatError` on mismatches.
- **Row decoding**: The library uses a `RowParser`/`Ok` pipeline mirroring
  `sqlite-simple`, supports generic `FromRow` derivation, and re-exports
  parsing combinators (`field`, `fieldWith`, `numFieldsRemaining`).
- **Types & diagnostics**: `FormatError`, `SQLError`, and `ResultError` carry
  detailed messages. `FieldBinding` stores printable parameter values, which
  appear in error logs.
- **Tests**: Tasty suite covers connections, statements, parameter validation,
  RowParser behaviour, transactions, and scalar UDFs.
- **DuckDB caveats**: Mixing positional/named placeholders is rejected by
  DuckDB itself; savepoints are not yet supported and are surfaced as user-facing
  errors. DuckDB provides row-count metadata but lacks a direct
  `lastInsertRowId`.

## Phase 6 (Documentation) – ✅ Done
- Expand README with RowParser usage, resource-management guidance, and a
  roadmap for future work.
- Ensure all newly exported symbols have Haddock descriptions.

---

## Phase 7 — Metadata & Ergonomics

Focus: surface readily available metadata and polish basic ergonomics.

- Expose `columnCount` and `columnName` for prepared statements/results using
  `c_duckdb_prepared_statement_column_*` and `c_duckdb_column_*`.
- Publish `rowsChanged` as part of the public API; document the absence of
  `lastInsertRowId` and recommend `RETURNING` instead.
- Add `(:.)` to `Types` plus corresponding `FromRow`/`ToRow` support.
- Update README/Haddocks with examples for the new helpers.
- Extend the test suite to cover metadata calls and `(:.)` round-trips.

## Phase 8 — Streaming & Folding

Focus: introduce scalable result consumption while respecting DuckDB’s capabilities.

- Implement `fold`, `fold_`, and `foldNamed` atop DuckDB’s chunked result API
  (`c_duckdb_execute_prepared` + `c_duckdb_stream_fetch_chunk`).
- Provide `nextRow`/`nextRowWith` for cursor-style iteration.
- Ensure resource management handles chunk lifetimes; document eager vs streaming
  trade-offs.
- Add tests comparing eager queries and folds over large datasets.

## Phase 9 — Extended Type Coverage

Focus: broaden `FromField`/`ToField` beyond core numerics/text.

- Add support for DuckDB temporal types (date, time, timestamp) and map to
  appropriate Haskell types (`Data.Time.*`).
- Handle boolean and binary large objects more explicitly; defer UUID coverage until follow-up.
- Document any DuckDB-specific parsing/encoding quirks.
- Expand tests using DuckDB’s built-in literals to verify round-trips.

## Phase 10 — Advanced Transactions & Savepoints

Focus: align transaction helpers with DuckDB’s evolving capabilities.

- Detect whether DuckDB build supports savepoints; conditionally enable
  `withSavepoint` instead of eagerly failing.
- If future DuckDB releases distinguish deferred/immediate/exclusive semantics,
  wire them through and update docs/tests.
- Add regression tests covering nested transactions and savepoint behaviour.

## Phase 11 — Documentation & Cabal Polish

Focus: polish release packaging and guidance.

- Update README with streaming examples, temporal types, and transaction caveats.
- Enable `-Wall` and capture warnings; add `tested-with` metadata and repository
  links pointing to this project.
- Publish a CHANGELOG summarising major milestones.
- Review Haddock warnings (e.g. unresolved links) and resolve or document them.

---

## Future Ideas (Post-Phase 11)

- Table/arrow/appender APIs from DuckDB FFI for bulk ingest.
- Optional logging/tracing hooks around query execution.
- Integration with connection pools or resource abstractions (outside core
  library if needed).
