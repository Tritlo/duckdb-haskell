{- |
Module      : Database.DuckDB.Simple.Pragma
Description : Register PRAGMA functions with typed Haskell callbacks.

A callback gets the arguments of the call as one row, decoded with
'Database.DuckDB.Simple.FromRow.FromRow'.
"Database.DuckDB.Simple.V2.Pragma" has the callbacks that get native handles.
-}
module Database.DuckDB.Simple.Pragma (
    PragmaCallback (..),
    registerPragmaFunction,
) where

import Database.DuckDB.Simple.Pragma.Internal (PragmaCallback (..), registerPragmaFunction)
