# duckdb-simple

`duckdb-simple` provides a high-level Haskell interface to DuckDB inspired by
the ergonomics of [`sqlite-simple`](https://hackage.haskell.org/package/sqlite-simple).
It builds on the low-level bindings exposed by [`duckdb-ffi`](../duckdb-ffi) and
provides a small, focused API for opening connections, running queries, binding
parameters, and decoding typed results.

## Getting Started

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Database.DuckDB.Simple
import Database.DuckDB.Simple.Types (Only (..))

main :: IO ()
main =
  withConnection ":memory:" \conn -> do
    _ <- execute_ conn "CREATE TABLE items (id INTEGER, name TEXT)"
    _ <- execute conn "INSERT INTO items VALUES (?, ?)" (1 :: Int, "banana" :: String)
    rows <- query_ conn "SELECT id, name FROM items ORDER BY id"
    mapM_ print (rows :: [(Int, String)])
```

### Key Modules

- `Database.DuckDB.Simple` – primary API: connections, statements, execution,
  queries, and error handling.
- `Database.DuckDB.Simple.ToField` / `ToRow` – typeclasses for preparing
  parameters that can be passed to `execute`/`query`.
- `Database.DuckDB.Simple.FromField` / `FromRow` – typeclasses for decoding
  query results returned by `query`/`query_`.
- `Database.DuckDB.Simple.Types` – common utility types (`Query`, `Null`,
  `Only`, `SQLError`).

## Querying Data

```haskell
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Types (Only (..))

fetchNames :: Connection -> IO [Maybe String]
fetchNames conn = do
  _ <- execute_ conn "CREATE TABLE names (value TEXT)"
  _ <- executeMany conn "INSERT INTO names VALUES (?)"
    [Only (Just "Alice"), Only (Nothing :: Maybe String)]
  fmap fromOnly <$> query_ conn "SELECT value FROM names ORDER BY value IS NULL, value"
```

The execution helpers return the number of affected rows (`Int`) so callers can
assert on data changes when needed.

## Named Parameters

duckdb-simple supports both positional (`?`) and named parameters. Named
parameters are bound with the `(:=)` helper exported from
`Database.DuckDB.Simple.ToField`.

```haskell
import Database.DuckDB.Simple
import Database.DuckDB.Simple (NamedParam ((:=)))

insertNamed :: Connection -> IO Int
insertNamed conn =
  executeNamed conn
    "INSERT INTO events VALUES ($kind, $payload)"
    ["$kind" := ("metric" :: String), "$payload" := ("ok" :: String)]
```

DuckDB currently does not allow mixing positional and named placeholders within
the same SQL statement; the library preserves DuckDB’s error message in that
situation. Savepoints are also unavailable in DuckDB at the moment, so
`withSavepoint` throws an `SQLError` detailing the limitation.

## Tests

The test suite is built with [tasty](https://hackage.haskell.org/package/tasty)
and covers connection management, statement lifecycle, parameter binding, and
query execution.

```
cabal test duckdb-simple-test --test-show-details=direct
```
