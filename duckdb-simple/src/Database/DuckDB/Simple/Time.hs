{-# LANGUAGE DeriveFunctor #-}

-- | Dates and timestamps that can represent DuckDB's infinities.
module Database.DuckDB.Simple.Time (
    Unbounded (..),
    Date,
    LocalTimestamp,
    UTCTimestamp,
) where

import Data.Time (Day, LocalTime, UTCTime)

-- | A finite value or either infinity, as in @postgresql-simple@.
data Unbounded a
    = NegInfinity
    | Finite !a
    | PosInfinity
    deriving (Eq, Ord, Show, Read, Functor)

-- | A DuckDB DATE, including infinity.
type Date = Unbounded Day

-- | A timestamp without a time zone, including infinity.
type LocalTimestamp = Unbounded LocalTime

-- | A timestamp with a time zone, including infinity.
type UTCTimestamp = Unbounded UTCTime
