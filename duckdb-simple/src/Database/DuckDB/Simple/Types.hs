{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Database.DuckDB.Simple.Types
-- Description : Shared data types for the duckdb-simple public surface.
--
-- This module defines 'Null', 'Only', @(:.)@, and 'FormatError'. It re-exports
-- 'Connection', 'Statement', 'Query', 'SQLError', and 'ErrorType' from
-- @Database.DuckDB.Simple.Internal@. This module and @Database.DuckDB.Simple@
-- export 'Connection' and 'Statement' without their constructors. They export
-- the other types with their constructors.
module Database.DuckDB.Simple.Types (
  Connection,
  Statement,
  Query (..),
  SQLError (..),
  ErrorType (..),
  FormatError (..),
  Null (..),
  Only (..),
  (:.) (..),
) where

import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), FromField (..), ResultError (..), returnError)
import Database.DuckDB.Simple.Internal (
  Connection,
  ErrorType (..),
  Query (..),
  SQLError (..),
  Statement,
 )
import Database.DuckDB.Simple.Ok (Ok (..))

-- | Placeholder representing SQL @NULL@.
data Null = Null
  deriving (Eq, Ord, Show, Read)

-- | This instance is in this module because @Internal@ imports @FromField@.
-- An import of this module from @FromField@ would cause an import cycle.
instance FromField Null where
  fromField f = case fieldValue f of
    FieldNull -> Ok Null
    _ -> returnError Incompatible f (Text.pack "expected NULL")

-- | Wrapper used for single-column rows.
newtype Only a = Only {fromOnly :: a}
  deriving (Eq, Ord, Show, Read)

-- | Convenience product type for combining @FromRow@ and @ToRow@ instances.
data h :. t = h :. t
  deriving (Eq, Ord, Show, Read)

infixr 3 :.

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
