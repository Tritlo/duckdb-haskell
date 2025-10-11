{- |
Module      : Database.DuckDB.Simple.Types
Description : Shared data types for the duckdb-simple public surface.

The datatypes in this module are intentionally kept lightweight.  The main
`Database.DuckDB.Simple` module exposes the types without their constructors so
that callers interact with them through the high-level API.  The actual
definitions live in 'Database.DuckDB.Simple.Internal'.
-}
module Database.DuckDB.Simple.Types (
    Connection,
    Statement,
    Query (..),
    SQLError (..),
    FormatError (..),
    Null (..),
    Only (..),
) where

import Control.Exception (Exception)
import Data.Text (Text)

import Database.DuckDB.Simple.Internal (
    Connection,
    Query (..),
    SQLError (..),
    Statement,
 )

-- | Placeholder representing SQL @NULL@.
data Null = Null
    deriving (Eq, Ord, Show, Read)

-- | Wrapper used for single-column rows.
newtype Only a = Only {fromOnly :: a}
    deriving (Eq, Ord, Show, Read)

-- | Raised when parameter formatting fails before a statement is executed.
data FormatError = FormatError
    { formatErrorMessage :: !Text
    -- ^ Human-readable description of the mismatch (e.g. parameter counts).
    , formatErrorQuery :: !Query
    -- ^ Query that triggered the formatting failure.
    , formatErrorParams :: ![String]
    -- ^ Rendered parameter values supplied by the caller (used for diagnostics).
    }
    deriving (Eq, Show)

instance Exception FormatError
