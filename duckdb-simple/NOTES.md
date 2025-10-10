# Development Notes

- Added `Database.DuckDB.Simple.Function` to expose DuckDB scalar function registration with a `sqlite-simple`-style `Function` typeclass and `createFunction`/`deleteFunction` helpers.
- Callback implementation decodes argument vectors into existing `FromField` values and materialises results back into DuckDB vectors for `BOOL`, `INTEGER`, `DOUBLE`, and `VARCHAR` (including `Maybe` support and zero-argument functions).
- Registration stores Haskell callbacks via DuckDB extra-info with lifetime handling; errors propagate as `SQLError` instances and are surfaced inside DuckDB through `duckdb_scalar_function_set_error`.
- Added integration tests covering pure, nullable, and `IO`-based functions plus the `deleteFunction` helper; README now documents the new module alongside example usage.
- `deleteFunction` attempts a `DROP FUNCTION IF EXISTS`; DuckDB flags C API
  registrations as internal, so the operation reports an `SQLError` explaining
  the limitation instead of removing the function.
