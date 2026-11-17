{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Data.Int (Int64)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue (..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (allocated_bytes), getRTSStats)
import System.Mem (performMajorGC)
import Text.Printf (printf)

-- | Measure NULL collection binding with a checked row count.
main :: IO ()
main = withConnectionWithConfig ":memory:" [("threads", "1"), ("async_threads", "0")] \conn -> do
  let cases =
        [ ("untyped_nulls", FieldList (replicate 1000 FieldNull), 0)
        , ("mixed_nulls", FieldList (concat (replicate 500 [FieldNull, FieldInt64 7])), 500)
        ]
      run (name, payload, nonNull) = do
        rows <- query conn "SELECT count(*)::BIGINT, count(x)::BIGINT FROM unnest(?) t(x)" (Only payload) :: IO [(Int64, Int64)]
        unless (rows == [(1000, nonNull)]) (fail (name <> show rows))
  forM_ cases run
  putStrLn "case,round,repetitions,real_ms,allocated_bytes"
  forM_ [1 .. 5 :: Int] \n -> forM_ (if odd n then cases else reverse cases) \c@(name, _, _) -> do
    performMajorGC
    before <- getRTSStats
    start <- getMonotonicTimeNSec
    forM_ [1 .. 10 :: Int] (const (run c))
    end <- getMonotonicTimeNSec
    performMajorGC
    after <- getRTSStats
    printf "%s,%d,10,%.3f,%d\n" name n (fromIntegral (end - start) / 1000000 :: Double) (allocated_bytes after - allocated_bytes before)
