{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Main
-- Description : Repeatable end-to-end benchmarks with checked results.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.Array (Array, elems, listArray, (!))
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import qualified Data.Geometry as G
import Data.Int (Int64)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue (..), StructField (..), StructValue (..))
import Database.DuckDB.Simple.Geometry (RawGeometry (..))
import Database.DuckDB.Simple.Variant (Variant (..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (allocated_bytes), getRTSStats, getRTSStatsEnabled)
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import Text.Printf (printf)

-- | Run one workload seven times after a warm-up run.
main :: IO ()
main = do
  statsEnabled <- getRTSStatsEnabled
  unless statsEnabled (fail "Run the benchmark with +RTS -T")
  args <- getArgs
  let workload = case args of name : _ -> name; _ -> "eager"
      count = case args of _ : n : _ -> read n; _ -> 100000 :: Int64
      expected = count * (count - 1) `div` 2
      collectionQuery expression = Query ("SELECT " <> expression <> " FROM range(" <> Text.pack (show count) <> ") t(i)")
      objectKeys = listArray (0, 63) ["k" <> Text.pack (show index) | index <- [0 .. 63 :: Int]]
  withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn -> do
    createFunction conn "bench_identity" (id :: Int64 -> Int64)
    [Only geometry] <- query_ conn "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
    let typedGeometry = G.PointGeometry (G.PointXY (G.XY 1 2))
    let sql = Query ("SELECT i FROM range(" <> Text.pack (show count) <> ") t(i)")
        action = case workload of
          "eager" -> do
            rows <- query_ conn sql
            evaluate (List.foldl' (\acc (Only n) -> acc + n) 0 rows)
          "fold" -> fold_ conn sql 0 (\acc (Only n) -> pure (acc + n))
          "scalar" -> do
            rows <- query_ conn (Query ("SELECT bench_identity(i) FROM range(" <> Text.pack (show count) <> ") t(i)"))
            evaluate (List.foldl' (\acc (Only n) -> acc + n) 0 rows)
          "text" -> do
            rows <- query_ conn (Query ("SELECT repeat('duckdb λ text', 4) FROM range(" <> Text.pack (show count) <> ")"))
            evaluate (List.foldl' (\acc (Only value) -> acc + fromIntegral (Text.length value)) 0 rows)
          "timestamp" -> do
            rows <- query_ conn (Query ("SELECT TIMESTAMP '2000-01-01' + i * INTERVAL 1 SECOND FROM range(" <> Text.pack (show count) <> ") t(i)"))
            evaluate (List.foldl' (\acc (Only (value :: LocalTime)) -> acc + floor (utcTimeToPOSIXSeconds (localTimeToUTC utc value))) 0 rows)
          "parameters" -> do
            repeatChecksum count $ do
              [Only value] <- query conn "SELECT ?::BIGINT" (Only (1 :: Int64))
              pure value
          "timestamp-parameters" -> do
            repeatChecksum count $ do
              [Only value] <- query conn "SELECT 1::BIGINT WHERE ? IS NOT NULL" (Only (posixSecondsToUTCTime 0))
              pure value
          "list-bigint" -> do
            [Only values] <- query_ conn (collectionQuery "list(i::BIGINT)") :: IO [Only [Int64]]
            collectionChecksum count values
          "list-bignum" -> do
            [Only values] <- query_ conn (collectionQuery "list(i::BIGNUM)") :: IO [Only [Integer]]
            collectionChecksum count values
          "array-bignum" -> do
            [Only values] <-
              query_ conn (collectionQuery ("list(i::BIGNUM)::BIGNUM[" <> Text.pack (show count) <> "]")) :: IO [Only (Array Int Integer)]
            collectionChecksum count values
          "map-bignum" -> do
            [Only values] <- query_ conn (collectionQuery "map(list(i::BIGINT), list(i::BIGNUM))") :: IO [Only (Map.Map Int64 Integer)]
            collectionChecksum count values
          "variant" -> do
            rows <- query_ conn (Query ("SELECT i::VARIANT FROM range(" <> Text.pack (show count) <> ") t(i)"))
            evaluate (List.foldl' (\acc (Only value) -> acc + variantNumber value) 0 rows)
          "variant-list" -> do
            [Only (Variant value)] <- query_ conn (collectionQuery "list(i)::VARIANT")
            case value of
              FieldList values -> collectionChecksum count (map (variantNumber . Variant) values)
              _ -> fail "unexpected VARIANT list benchmark shape"
          "variant-parameters" -> do
            repeatChecksum count $ do
              [Only value] <- query conn "SELECT ?" (Only (Variant (FieldInt64 1)))
              pure (variantNumber value)
          -- Objects with 64 different key sets.
          "variant-objects" -> do
            rows <- query_ conn (Query ("SELECT json_object('k' || (i % 64)::VARCHAR, i)::VARIANT FROM range(" <> Text.pack (show count) <> ") t(i)"))
            collectionChecksum count (map (\(Only value) -> variantObjectNumber objectKeys value) rows)
          "geometry" -> do
            rows <- query_ conn (Query ("SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84') FROM range(" <> Text.pack (show count) <> ")"))
            evaluate (sum [fromIntegral (BS.length (rawGeometryWKB value)) | Only value <- rows])
          "geometry-parameters" -> do
            repeatChecksum count $ do
              [Only value] <-
                query conn "SELECT system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?)" (rawGeometryWKB geometry, rawGeometryCRS geometry)
              pure (fromIntegral (BS.length (rawGeometryWKB value)))
          "geometry-typed" -> do
            rows <- query_ conn (Query ("SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84') FROM range(" <> Text.pack (show count) <> ")"))
            evaluate (sum [pointChecksum value | Only value <- rows])
          "geometry-typed-parameters" -> do
            repeatChecksum count $ do
              [Only value] <- query conn "SELECT ?" (Only typedGeometry)
              pure (pointChecksum value)
          _ ->
            fail
              "Expected eager, fold, scalar, text, timestamp, timestamp-parameters, parameters, list-bigint, list-bignum, array-bignum, map-bignum, variant, variant-list, variant-parameters, variant-objects, geometry, geometry-parameters, geometry-typed, or geometry-typed-parameters"
        expectedResult = case workload of
          "parameters" -> count
          "timestamp-parameters" -> count
          "variant-parameters" -> count
          "geometry" -> count * 21
          "geometry-parameters" -> count * 21
          "geometry-typed" -> count * 3
          "geometry-typed-parameters" -> count * 3
          "text" -> count * fromIntegral (Text.length (Text.replicate 4 "duckdb λ text"))
          "timestamp" -> count * 946684800 + expected
          _ -> expected
        check actual = unless (actual == expectedResult) (fail "benchmark result mismatch")
    action >>= check
    putStrLn "workload,rows,run,milliseconds,allocated_bytes,checksum"
    forM_ [1 .. 7 :: Int] $ \run -> do
      performMajorGC
      before <- getRTSStats
      start <- getMonotonicTimeNSec
      result <- action >>= evaluate
      end <- getMonotonicTimeNSec
      performMajorGC
      after <- getRTSStats
      check result
      printf
        "%s,%d,%d,%.3f,%d,%d\n"
        workload
        count
        run
        (fromIntegral (end - start) / 1000000 :: Double)
        (allocated_bytes after - allocated_bytes before)
        result

-- | Repeat an action and add its results with a constant stack.
repeatChecksum :: Int64 -> IO Int64 -> IO Int64
repeatChecksum count action = go count 0
  where
    go remaining !total
      | remaining <= 0 = pure total
      | otherwise = do
          value <- action
          go (remaining - 1) (total + value)

-- | Check the element count and force the checksum of a collection.
collectionChecksum :: (Foldable f, Integral a) => Int64 -> f a -> IO Int64
collectionChecksum count values = do
  let (!actualCount, !checksum) = Foldable.foldl' (\(!size, !total) value -> (size + 1, total + fromIntegral value)) (0, 0) values
  unless (actualCount == count) (fail "benchmark collection size mismatch")
  pure checksum

-- | Read the integer in a scalar VARIANT.
variantNumber :: Variant -> Int64
variantNumber (Variant (FieldInt64 value)) = value
variantNumber (Variant (FieldWord64 value))
  | value <= fromIntegral (maxBound :: Int64) = fromIntegral value
variantNumber value = error ("unexpected VARIANT benchmark value: " <> show value)

-- | Check one object field and its key, then read its integer payload.
variantObjectNumber :: Array Int Text.Text -> Variant -> Int64
variantObjectNumber keys (Variant (FieldStruct value)) = case elems (structValueFields value) of
  [StructField key payload]
    | let number = variantNumber (Variant payload)
    , key == keys ! fromIntegral (number `mod` 64) ->
        number
  _ -> error "unexpected VARIANT object benchmark fields"
variantObjectNumber _ _ = error "unexpected VARIANT object benchmark shape"

-- | Force coordinate decoding and check the point shape.
pointChecksum :: G.Geometry -> Int64
pointChecksum (G.PointGeometry (G.PointXY (G.XY x y))) = round (x + y)
pointChecksum value = error ("unexpected geometry benchmark value: " <> show value)
