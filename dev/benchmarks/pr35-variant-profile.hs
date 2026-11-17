{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Measure checked VARIANT reads after fixture construction and one warm-up.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless, void)
import Data.Int (Int64)
import qualified Data.Text as Text
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Variant (Variant (..), variantObject)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (allocated_bytes), getRTSStats, getRTSStatsEnabled)
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import Text.Printf (printf)

-- | Run one shape with an exact value check on every row.
main :: IO ()
main = do
  [shape, countText, runsText] <- getArgs
  let count = read countText :: Int64
      runs = read runsText :: Int
      (expression, expected) = fixture shape
      table = Query ("CREATE TABLE fixture AS SELECT " <> expression <> " AS v FROM range(" <> Text.pack countText <> ") t(i)")
  enabled <- getRTSStatsEnabled
  unless enabled (fail "Run with +RTS -N1 -T")
  withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn -> do
    void (execute_ conn table)
    let action = do
          rows <- query_ conn "SELECT v FROM fixture" :: IO [Only Variant]
          let go !index [] = unless (index == count) (fail "row count mismatch") >> pure index
              go !index (Only (Variant value) : rest)
                | value == expected index = go (index + 1) rest
                | otherwise = fail ("row mismatch: " <> show (index, value, expected index))
          go 0 rows
    _ <- action >>= evaluate
    putStrLn "shape,rows,run,milliseconds,allocated_bytes,checked_rows"
    forM_ [1 .. runs] $ \run -> do
      performMajorGC
      before <- getRTSStats
      start <- getMonotonicTimeNSec
      checked <- action
      end <- getMonotonicTimeNSec
      performMajorGC
      after <- getRTSStats
      printf
        "%s,%d,%d,%.3f,%d,%d\n"
        shape
        count
        run
        (fromIntegral (end - start) / 1000000 :: Double)
        (allocated_bytes after - allocated_bytes before)
        checked

-- | Supply native SQL and an independent expected value for each row.
fixture :: String -> (Text.Text, Int64 -> FieldValue)
fixture shape = case shape of
  "scalar" -> ("i::BIGINT::VARIANT", FieldInt64)
  "array" -> ("[i, i + 1]::VARIANT", \i -> FieldList [FieldInt64 i, FieldInt64 (i + 1)])
  "object" -> ("{'a': i, 'b': i + 1}::VARIANT", \i -> variantObject [("a", FieldInt64 i), ("b", FieldInt64 (i + 1))])
  "unusual" ->
    ( "json_object('', i, 'A', i + 1, 'a', i + 2, 'a' || chr(0) || 'b', i + 3)::VARIANT"
    , \i ->
        variantObject
          [ ("", FieldWord64 (fromIntegral i))
          , ("A", FieldWord64 (fromIntegral (i + 1)))
          , ("a", FieldWord64 (fromIntegral (i + 2)))
          , ("a\0b", FieldWord64 (fromIntegral (i + 3)))
          ]
    )
  "mixed" ->
    ( "CASE i % 4 WHEN 0 THEN i::BIGINT::VARIANT WHEN 1 THEN [i, i + 1]::VARIANT WHEN 2 THEN {'a': i, 'b': [i + 1]}::VARIANT ELSE NULL::VARIANT END"
    , \i -> case i `mod` 4 of
        0 -> FieldInt64 i
        1 -> FieldList [FieldInt64 i, FieldInt64 (i + 1)]
        2 -> variantObject [("a", FieldInt64 i), ("b", FieldList [FieldInt64 (i + 1)])]
        _ -> FieldNull
    )
  _ -> error "Expected scalar, array, object, unusual, or mixed"
