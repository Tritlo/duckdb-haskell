{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Regression check for leaked DuckDB handles.

The handles live in C memory, so the GHC heap statistics cannot see them.
Each leaked database instance keeps its own DuckDB thread pool alive, thus
this check compares the number of operating-system threads and the resident
set size of the process before and after many open\/close cycles.  Both
values are process-global, thus the check has its own test suite and does
not share a process with the other tests.

The check reads @\/proc\/self\/status@.  On a system without @\/proc@ the
check reports a skip and exits with success.
-}
module Main (main) where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM_, when)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Database.DuckDB.Simple
import System.Exit (exitFailure)

-- | The process-global resource counters that a leaked handle increases.
data Usage = Usage
    { usageThreads :: Int
    , usageRssKb :: Int
    }
    deriving (Show)

-- | The number of open\/close cycles that the check runs.
cycles :: Int
cycles = 32

-- | The largest thread growth that is not a leak.
threadSlack :: Int
threadSlack = 8

-- | The largest resident-set growth, in kB, that is not a leak.
rssSlackKb :: Int
rssSlackKb = 32 * 1024

{- | Read the current resource counters.  The result is 'Nothing' if the system
has no @\/proc\/self\/status@.
-}
readUsage :: IO (Maybe Usage)
readUsage = do
    result <- try (readFile "/proc/self/status") :: IO (Either IOException String)
    case result of
        Left _ -> pure Nothing
        Right contents -> do
            _ <- evaluate (length contents)
            pure (Usage <$> statusField "Threads" contents <*> statusField "VmRSS" contents)
  where
    statusField name contents =
        listToMaybe
            [ value
            | line <- lines contents
            , (name <> ":") `isPrefixOf` line
            , (value, _) <- reads (drop (length name + 1) line)
            ]

-- | Open a connection, use a statement, then close both.
openCloseCycle :: IO ()
openCloseCycle = do
    conn <- open ":memory:"
    stmt <- openStatement conn "SELECT 42"
    closeStatement stmt
    _ <- query_ conn "SELECT 42" :: IO [Only Int64]
    close conn

main :: IO ()
main = do
    -- The first cycle also does the one-time initialization, which must not
    -- count as growth.
    openCloseCycle
    before <- readUsage
    case before of
        Nothing -> putStrLn "duckdb-simple leak check: /proc/self/status is unavailable; skipped."
        Just baseline -> do
            forM_ [1 .. cycles] \_ -> openCloseCycle
            after <- readUsage
            case after of
                Nothing -> putStrLn "duckdb-simple leak check: /proc/self/status disappeared; skipped."
                Just final -> report baseline final

-- | Print both measurements and fail if either one grew too much.
report :: Usage -> Usage -> IO ()
report before after = do
    putStrLn $ "cycles: " <> show cycles
    putStrLn $
        "threads: "
            <> show (usageThreads before)
            <> " -> "
            <> show (usageThreads after)
            <> " (slack "
            <> show threadSlack
            <> ")"
    putStrLn $
        "rss kB: "
            <> show (usageRssKb before)
            <> " -> "
            <> show (usageRssKb after)
            <> " (slack "
            <> show rssSlackKb
            <> ")"
    when (threadGrowth > threadSlack || rssGrowth > rssSlackKb) do
        putStrLn "FAIL: open/close leaks DuckDB resources"
        exitFailure
  where
    threadGrowth = usageThreads after - usageThreads before
    rssGrowth = usageRssKb after - usageRssKb before
