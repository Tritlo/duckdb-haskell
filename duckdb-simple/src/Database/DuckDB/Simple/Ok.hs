{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Database.DuckDB.Simple.Ok
-- Description : Accumulate errors from row parsing.
--
-- The @Ok@ type mirrors the helper used by @sqlite-simple@. It behaves like
-- @Either [SomeException]@. Its @Alternative@ instance keeps the errors when
-- both alternatives fail. @Database.DuckDB.Simple.Conversion.runConversion@
-- returns it after conversion effects and diagnostics have completed.
module Database.DuckDB.Simple.Ok (
  Ok (..),
  ManyErrors (..),
) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception, SomeException, toException)
import Control.Monad (MonadPlus (..))
import qualified Control.Monad.Fail as Fail

-- | A value or a list of errors.
data Ok a
  = Errors [SomeException]
  | Ok !a
  deriving stock (Show)
  deriving stock (Functor)

-- | Two failures are considered equal regardless of their payload.
instance (Eq a) => Eq (Ok a) where
  Errors _ == Errors _ = True
  Ok a == Ok b = a == b
  _ == _ = False

instance Applicative Ok where
  pure = Ok
  Ok f <*> Ok a = Ok (f a)
  Errors es <*> Ok _ = Errors es
  Ok _ <*> Errors es = Errors es
  Errors es1 <*> Errors es2 = Errors (es1 <> es2)

instance Alternative Ok where
  empty = Errors []
  Ok x <|> _ = Ok x
  Errors _ <|> Ok y = Ok y
  Errors xs <|> Errors ys = Errors (xs <> ys)

instance Monad Ok where
  Ok a >>= f = f a
  Errors es >>= _ = Errors es

instance MonadPlus Ok where
  mzero = empty
  mplus = (<|>)

instance Fail.MonadFail Ok where
  fail msg = Errors [toException (userError msg)]

-- | Store multiple exceptions in one exception.
newtype ManyErrors = ManyErrors [SomeException]
  deriving stock (Show)

instance Exception ManyErrors
