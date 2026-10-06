{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Repeatable end-to-end benchmarks with checked results.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM, unless)
import qualified Data.ByteString as BS
import qualified Data.Geometry as G
import Data.Int (Int64)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Geometry (RawGeometry (..))
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import Text.Printf (printf)

-- | Run one workload seven times after a warm-up run.
main :: IO ()
main = do
    args <- getArgs
    let workload = case args of name : _ -> name; _ -> "eager"
        count = case args of _ : n : _ -> read n; _ -> 100000 :: Int64
        expected = count * (count - 1) `div` 2
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
                    rows <- replicateM (fromIntegral count) (query conn "SELECT ?::BIGINT" (Only (1 :: Int64)))
                    evaluate (sum [n | [Only n] <- rows])
                "geometry" -> do
                    rows <- query_ conn (Query ("SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84') FROM range(" <> Text.pack (show count) <> ")"))
                    evaluate (sum [fromIntegral (BS.length (rawGeometryWKB value)) | Only value <- rows])
                "geometry-parameters" -> do
                    rows <- replicateM (fromIntegral count) (query conn "SELECT ?" (Only geometry))
                    evaluate (sum [fromIntegral (BS.length (rawGeometryWKB value)) | [Only value] <- rows])
                "geometry-typed" -> do
                    rows <- query_ conn (Query ("SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84') FROM range(" <> Text.pack (show count) <> ")"))
                    evaluate (sum [pointChecksum value | Only value <- rows])
                "geometry-typed-parameters" -> do
                    rows <- replicateM (fromIntegral count) (query conn "SELECT ?" (Only typedGeometry))
                    evaluate (sum [pointChecksum value | [Only value] <- rows])
                _ -> fail "Expected eager, fold, scalar, text, timestamp, parameters, geometry, geometry-parameters, geometry-typed, or geometry-typed-parameters"
            expectedResult = case workload of
                "parameters" -> count
                "geometry" -> count * 21
                "geometry-parameters" -> count * 21
                "geometry-typed" -> count * 3
                "geometry-typed-parameters" -> count * 3
                "text" -> count * fromIntegral (Text.length (Text.replicate 4 "duckdb λ text"))
                "timestamp" -> count * 946684800 + expected
                _ -> expected
            check actual = unless (actual == expectedResult) (fail "benchmark result mismatch")
        action >>= check
        putStrLn "workload,rows,run,milliseconds,checksum"
        forM_ [1 .. 7 :: Int] $ \run -> do
            performMajorGC
            start <- getMonotonicTimeNSec
            result <- action
            end <- getMonotonicTimeNSec
            check result
            printf "%s,%d,%d,%.3f,%d\n" workload count run (fromIntegral (end - start) / 1000000 :: Double) result

-- | Force coordinate decoding and check the point shape.
pointChecksum :: G.Geometry -> Int64
pointChecksum (G.PointGeometry (G.PointXY (G.XY x y))) = round (x + y)
pointChecksum value = error ("unexpected geometry benchmark value: " <> show value)
