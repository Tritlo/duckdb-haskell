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
    execute_ conn "CREATE TABLE items (id INTEGER, name TEXT)"
    execute conn "INSERT INTO items VALUES (?, ?)" (1 :: Int, "banana" :: String)
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
  execute_ conn "CREATE TABLE names (value TEXT)"
  executeMany conn "INSERT INTO names VALUES (?)"
    [Only (Just "Alice"), Only (Nothing :: Maybe String)]
  fmap fromOnly <$> query_ conn "SELECT value FROM names ORDER BY value IS NULL, value"
```

## Tests

The test suite is built with [tasty](https://hackage.haskell.org/package/tasty)
and covers connection management, statement lifecycle, parameter binding, and
query execution.

```
cabal test duckdb-simple-test --test-show-details=direct
```

