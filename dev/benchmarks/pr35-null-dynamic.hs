{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Data.Array (elems, listArray)
import Data.Int (Int64)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue (..), StructField (..), StructValue (..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (allocated_bytes), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import Text.Printf (printf)

-- | Measure non-NULL composite binding with checked types and sums.
main :: IO ()
main = do
  enabled <- getRTSStatsEnabled
  unless enabled (fail "Use +RTS -T -N1")
  withConnectionWithConfig ":memory:" [("threads", "1"), ("async_threads", "0")] \conn -> do
    parsed <- query_ conn "SELECT {'k':i::BIGINT} FROM range(1000) t(i) ORDER BY i" :: IO [Only (StructValue FieldValue)]
    let structs = [value | Only value <- parsed]
        values = map FieldStruct structs
        array = FieldArray (listArray (0, 999) values)
        pairs = FieldMap (zip (map FieldInt64 [0 .. 999]) values)
        expectedSum = 499500 :: Int64
        collectionSql = "SELECT typeof(v), len(v)::BIGINT, (SELECT sum(x.k)::BIGINT FROM unnest(v) t(x)) FROM (SELECT ? AS v)"
        mapSql =
          "SELECT typeof(v), len(map_keys(v))::BIGINT, (SELECT sum(x.k)::BIGINT FROM unnest(map_values(v)) t(x)), (SELECT sum(x)::BIGINT FROM unnest(map_keys(v)) t(x)) FROM (SELECT ? AS v)"
        checkCollection :: (ToRow p) => FieldValue -> p -> IO ()
        checkCollection dtype payload = do
          result <- query conn collectionSql payload :: IO [(FieldValue, Int64, Int64)]
          unless (result == [(dtype, 1000, expectedSum)]) (fail ("collection mismatch: " <> show result))
        checkMap = do
          result <- query conn mapSql (Only pairs) :: IO [(FieldValue, Int64, Int64, Int64)]
          unless (result == [(FieldText "MAP(BIGINT, STRUCT(k BIGINT))", 1000, expectedSum, expectedSum)]) (fail ("map mismatch: " <> show result))
        cases =
          [ ("struct_list", checkCollection (FieldText "STRUCT(k BIGINT)[]") (Only structs))
          , ("struct_array", checkCollection (FieldText "STRUCT(k BIGINT)[1000]") (Only array))
          , ("struct_map", checkMap)
          ]
    unless (length structs == 1000) (fail "fixture length")
    forM_ (zip [0 .. 999 :: Int64] structs) \(index, value) -> do
      unless (elems (structValueFields value) == [StructField "k" (FieldInt64 index)]) (fail "fixture payload/order")
      unless (length (elems (structValueTypes value)) == 1) (fail "fixture metadata")
    forM_ cases (\(_, action) -> action)
    putStrLn "case,round,elements,repetitions,real_ms,allocated_bytes,checksum_per_query"
    forM_ [1 .. 3 :: Int] \roundNumber -> do
      let rotate n xs = drop n xs <> take n xs
      forM_ (rotate ((roundNumber - 1) `mod` length cases) cases) \(name, action) -> do
        performMajorGC
        before <- getRTSStats
        start <- getMonotonicTimeNSec
        forM_ [1 .. 10 :: Int] (const action)
        end <- getMonotonicTimeNSec
        performMajorGC
        after <- getRTSStats
        printf
          "%s,%d,1000,10,%.3f,%d,%d\n"
          (name :: String)
          roundNumber
          (fromIntegral (end - start) / 1000000 :: Double)
          (allocated_bytes after - allocated_bytes before)
          expectedSum
