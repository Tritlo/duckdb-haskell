{-# LANGUAGE OverloadedStrings #-}

-- | Performance benchmarks for duckdb-simple
module Main (main) where

import Criterion.Main
import Database.DuckDB.Simple
import qualified Data.Text as Text
import Data.Int (Int64)
import Data.Word (Word64)
import Control.Monad (void, forM_)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))

main :: IO ()
main = defaultMain
    [ connectionBenchmarks
    , queryBenchmarks
    , insertBenchmarks
    , batchBenchmarks
    , typeBenchmarks
    , streamingBenchmarks
    , transactionBenchmarks
    ]

-- Connection benchmarks
connectionBenchmarks :: Benchmark
connectionBenchmarks = bgroup "connection"
    [ bench "open-close" $ nfIO $ do
        conn <- open ":memory:"
        close conn
    , bench "withConnection" $ nfIO $
        withConnection ":memory:" $ \_ -> pure ()
    ]

-- Query benchmarks
queryBenchmarks :: Benchmark
queryBenchmarks = env setupSimpleTable $ \conn -> bgroup "queries"
    [ bench "query_ simple select" $ nfIO $
        query_ conn "SELECT 1" :: IO [Only Int]
    , bench "query_ with 10 rows" $ nfIO $
        query_ conn "SELECT n FROM simple WHERE n <= 10" :: IO [Only Int]
    , bench "query_ with 100 rows" $ nfIO $
        query_ conn "SELECT n FROM simple WHERE n <= 100" :: IO [Only Int]
    , bench "query_ with 1000 rows" $ nfIO $
        query_ conn "SELECT n FROM simple WHERE n <= 1000" :: IO [Only Int]
    , bench "query with parameters" $ nfIO $
        query conn "SELECT n FROM simple WHERE n > ?" (Only (500 :: Int)) :: IO [Only Int]
    , bench "queryNamed" $ nfIO $
        queryNamed conn "SELECT n FROM simple WHERE n > $limit" ["$limit" := (500 :: Int)] :: IO [Only Int]
    , bench "query multiple columns" $ nfIO $
        query_ conn "SELECT n, n * 2, n * 3 FROM simple WHERE n <= 100" :: IO [(Int, Int, Int)]
    ]

-- Insert benchmarks
insertBenchmarks :: Benchmark
insertBenchmarks = bgroup "insert"
    [ bench "execute single row" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            execute conn "INSERT INTO test VALUES (?)" (Only (42 :: Int))
    , bench "execute_ without parameters" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            execute_ conn "INSERT INTO test VALUES (42)"
    , bench "executeNamed single row" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            executeNamed conn "INSERT INTO test VALUES ($x)" ["$x" := (42 :: Int)]
    ]

-- Batch insert benchmarks
batchBenchmarks :: Benchmark
batchBenchmarks = bgroup "batch"
    [ bench "executeMany 10 rows" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            executeMany conn "INSERT INTO test VALUES (?)" (fmap Only [1..10 :: Int])
    , bench "executeMany 100 rows" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            executeMany conn "INSERT INTO test VALUES (?)" (fmap Only [1..100 :: Int])
    , bench "executeMany 1000 rows" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            executeMany conn "INSERT INTO test VALUES (?)" (fmap Only [1..1000 :: Int])
    , bench "executeMany multi-column 100 rows" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (a INTEGER, b TEXT, c DOUBLE)"
            executeMany conn "INSERT INTO test VALUES (?, ?, ?)"
                [(i :: Int, Text.pack $ "text" ++ show i, fromIntegral i * 1.5 :: Double) | i <- [1..100]]
    ]

-- Type conversion benchmarks
typeBenchmarks :: Benchmark
typeBenchmarks = env setupTypesTable $ \conn -> bgroup "types"
    [ bench "decode integers" $ nfIO $
        query_ conn "SELECT int_col FROM types LIMIT 100" :: IO [Only Int]
    , bench "decode Int64" $ nfIO $
        query_ conn "SELECT bigint_col FROM types LIMIT 100" :: IO [Only Int64]
    , bench "decode Word64" $ nfIO $
        query_ conn "SELECT ubigint_col FROM types LIMIT 100" :: IO [Only Word64]
    , bench "decode Double" $ nfIO $
        query_ conn "SELECT double_col FROM types LIMIT 100" :: IO [Only Double]
    , bench "decode Text" $ nfIO $
        query_ conn "SELECT text_col FROM types LIMIT 100" :: IO [Only Text.Text]
    , bench "decode Bool" $ nfIO $
        query_ conn "SELECT bool_col FROM types LIMIT 100" :: IO [Only Bool]
    , bench "decode timestamp" $ nfIO $
        query_ conn "SELECT ts_col FROM types LIMIT 100" :: IO [Only LocalTime]
    , bench "decode Maybe (all Just)" $ nfIO $
        query_ conn "SELECT int_col FROM types LIMIT 100" :: IO [Only (Maybe Int)]
    , bench "decode Maybe (mixed NULL)" $ nfIO $
        query_ conn "SELECT nullable_col FROM types LIMIT 100" :: IO [Only (Maybe Int)]
    , bench "decode mixed tuple" $ nfIO $
        query_ conn "SELECT int_col, text_col, double_col FROM types LIMIT 100" :: IO [(Int, Text.Text, Double)]
    ]

-- Streaming benchmarks
streamingBenchmarks :: Benchmark
streamingBenchmarks = env setupLargeTable $ \conn -> bgroup "streaming"
    [ bench "fold_ 1000 rows" $ nfIO $
        fold_ conn "SELECT n FROM large WHERE n <= 1000" (0 :: Int) $ \acc (Only n) ->
            pure (acc + n)
    , bench "fold_ 10000 rows" $ nfIO $
        fold_ conn "SELECT n FROM large WHERE n <= 10000" (0 :: Int) $ \acc (Only n) ->
            pure (acc + n)
    , bench "query_ 1000 rows (eager)" $ nfIO $
        query_ conn "SELECT n FROM large WHERE n <= 1000" :: IO [Only Int]
    , bench "query_ 10000 rows (eager)" $ nfIO $
        query_ conn "SELECT n FROM large WHERE n <= 10000" :: IO [Only Int]
    , bench "fold with parameters" $ nfIO $
        fold conn "SELECT n FROM large WHERE n > ? AND n <= ?"
            (Only (5000 :: Int, 6000 :: Int)) (0 :: Int) $ \acc (Only n) ->
            pure (acc + n)
    , bench "nextRow iteration 100 rows" $ nfIO $
        withStatement conn "SELECT n FROM large WHERE n <= 100" $ \stmt -> do
            let loop acc = do
                    row <- nextRow stmt :: IO (Maybe (Only Int))
                    case row of
                        Nothing -> pure acc
                        Just (Only n) -> loop (acc + n)
            loop (0 :: Int)
    ]

-- Transaction benchmarks
transactionBenchmarks :: Benchmark
transactionBenchmarks = bgroup "transactions"
    [ bench "transaction overhead (single insert)" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            withTransaction conn $
                execute conn "INSERT INTO test VALUES (?)" (Only (42 :: Int))
    , bench "transaction with 10 inserts" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            withTransaction conn $
                forM_ [1..10 :: Int] $ \i ->
                    execute conn "INSERT INTO test VALUES (?)" (Only i)
    , bench "transaction with 100 inserts" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            withTransaction conn $
                forM_ [1..100 :: Int] $ \i ->
                    execute conn "INSERT INTO test VALUES (?)" (Only i)
    , bench "no transaction (10 inserts)" $ nfIO $
        withConnection ":memory:" $ \conn -> do
            void $ execute_ conn "CREATE TABLE test (x INTEGER)"
            forM_ [1..10 :: Int] $ \i ->
                execute conn "INSERT INTO test VALUES (?)" (Only i)
    ]

-- Setup functions

setupSimpleTable :: IO Connection
setupSimpleTable = do
    conn <- open ":memory:"
    void $ execute_ conn "CREATE TABLE simple (n INTEGER)"
    void $ executeMany conn "INSERT INTO simple VALUES (?)" (fmap Only [1..10000 :: Int])
    pure conn

setupTypesTable :: IO Connection
setupTypesTable = do
    conn <- open ":memory:"
    void $ execute_ conn $ Query $ Text.unlines
        [ "CREATE TABLE types ("
        , "  int_col INTEGER,"
        , "  bigint_col BIGINT,"
        , "  ubigint_col UBIGINT,"
        , "  double_col DOUBLE,"
        , "  text_col TEXT,"
        , "  bool_col BOOLEAN,"
        , "  ts_col TIMESTAMP,"
        , "  nullable_col INTEGER"
        , ")"
        ]
    let sampleTime = LocalTime (fromGregorian 2024 10 12) (TimeOfDay 14 30 15)
    void $ executeMany conn
        "INSERT INTO types VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        [ ( i :: Int
          , fromIntegral i :: Int64
          , fromIntegral (i * 2) :: Word64
          , fromIntegral i * 1.5 :: Double
          , Text.pack $ "text" ++ show i
          , even i
          , sampleTime
          , if i `mod` 3 == 0 then Nothing else Just i :: Maybe Int
          ) | i <- [1..1000]
        ]
    pure conn

setupLargeTable :: IO Connection
setupLargeTable = do
    conn <- open ":memory:"
    void $ execute_ conn "CREATE TABLE large (n INTEGER)"
    void $ executeMany conn "INSERT INTO large VALUES (?)" (fmap Only [1..50000 :: Int])
    pure conn
