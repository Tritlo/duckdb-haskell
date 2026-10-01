{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{- | Regression check for leaked DuckDB handles.

The handles live in C memory, so the GHC heap statistics cannot see them.
The open/close check counts native threads and resident memory. The sustained
checks keep one connection open through repeated queries, failures, cursor
resets, callback replacement, and cancellation. Weak references check callback
state release independently of resident memory.

Each check runs in this dedicated process. On systems without
@/proc/self/status@, the native resource counters are unavailable. The
functional checks and callback collection checks still run.
-}
module Main (main) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Exception (AsyncException (ThreadKilled), IOException, SomeException, evaluate, fromException, try)
import Control.Monad (forM_, replicateM, unless, void, when)
import Data.IORef (atomicModifyIORef', atomicWriteIORef, mkWeakIORef, newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, listToMaybe)
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Copy as Copy
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Database.DuckDB.Simple.FromField (FieldValue)
import qualified Database.DuckDB.Simple.Logging as Logging
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.Mem (performMajorGC)
import System.Mem.Weak (deRefWeak)
import Text.Read (readMaybe)

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
    -- VARIANT decoding fails after DuckDB allocates the materialized result.
    -- The result and connection must still be destroyed.
    rejected <- try (query_ conn "SELECT i::VARIANT FROM range(100000) t(i)") :: IO (Either SomeException [Only FieldValue])
    case rejected of
        Left _ -> pure ()
        Right _ -> fail "expected unsupported VARIANT conversion"
    close conn

main :: IO ()
main = do
    args <- getArgs
    let (mode, batches) = case args of
            [] -> ("all", 10)
            [name] -> (name, 10)
            [name, n] | Just count <- readMaybe n, count > 0 -> (name, count)
            _ -> ("invalid", 0)
    unless (mode `elem` ["all", "open-close", "long-lived", "callbacks", "cancel", "decode-failure"]) $
        fail "Expected [all|open-close|long-lived|callbacks|cancel|decode-failure] [positive batch count]"
    when (mode `elem` ["all", "open-close"]) checkOpenClose
    when (mode `elem` ["all", "long-lived"]) (checkLongLived batches)
    when (mode `elem` ["all", "callbacks"]) (checkCallbacks batches)
    when (mode `elem` ["all", "cancel"]) (checkCancellation batches)
    when (mode == "decode-failure") checkDecodeFailure

-- | Check native results and database handles across connection lifetimes.
checkOpenClose :: IO ()
checkOpenClose = do
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
                Just final -> report ("open/close, " <> show cycles <> " cycles") baseline final

-- | Reuse one connection and statement through successful and failed operations.
checkLongLived :: Int -> IO ()
checkLongLived batches =
    withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
        withStatement conn "SELECT ?::BIGINT FROM range(10000)" \stmt -> do
            let cycleQuery = do
                    rows <- query_ conn "SELECT sum(i)::BIGINT FROM range(10000) t(i)"
                    unless (rows == [Only (49995000 :: Int64)]) (fail "wrong query result")
                    expectFailure (query_ conn "SELECT i::VARIANT FROM range(10000) t(i)" :: IO [Only FieldValue])
                    expectFailure (query_ conn "SELECT CAST('bad' AS BIGINT)" :: IO [Only Int64])
                    bind stmt [toField (42 :: Int64)]
                    nextRow stmt >>= \row -> unless (row == Just (Only (42 :: Int64))) (fail "wrong cursor result")
                    clearStatementBindings stmt
                    expectFailure (nextRow stmt :: IO (Maybe (Only Int64)))
                batch = forM_ [1 .. 100 :: Int] (const cycleQuery)
            batch
            performMajorGC
            before <- readUsage
            forM_ [1 .. batches] \n -> do
                batch
                performMajorGC
                after <- readUsage
                reportOptional ("open connection, " <> show (n * 100) <> " cycles") before after

-- | Isolate failed result cleanup for comparison with earlier package versions.
checkDecodeFailure :: IO ()
checkDecodeFailure =
    withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
        let rejected = expectFailure (query_ conn "SELECT i::VARIANT FROM range(100000) t(i)" :: IO [Only FieldValue])
        rejected
        performMajorGC
        before <- readUsage
        forM_ [1 .. cycles] (const rejected)
        performMajorGC
        after <- readUsage
        reportOptional ("open connection, " <> show cycles <> " decode failures") before after

-- | Check that callback closures and query state are released before close.
checkCallbacks :: Int -> IO ()
checkCallbacks batches =
    withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
        Logging.registerLogStorage conn "leak_log" (\_ -> pure ())
        copyStates <- newIORef []
        let newCopyState = do
                state <- newIORef (7 :: Int64)
                weak <- mkWeakIORef state (pure ())
                atomicModifyIORef' copyStates (\old -> (weak : old, ()))
                pure state
        Copy.registerCopyToFunction conn "leak_copy" (\_ -> newCopyState) (\_ -> newCopyState) (\_ _ -> pure ()) (\_ -> pure ())
        Copy.registerCopyToFunction conn "leak_copy_failure" (\_ -> newCopyState) (\_ -> fail "COPY init failure" :: IO ()) (\_ _ -> pure ()) (\_ -> pure ())
        let cycleCallback = do
                retained <- newIORef (42 :: Int64)
                weak <- mkWeakIORef retained (pure ())
                createFunction conn "leak_scalar" (readIORef retained)
                query_ conn "SELECT leak_scalar()" >>= \rows -> unless (rows == [Only (42 :: Int64)]) (fail "wrong callback result")
                states <- newIORef []
                createFunctionWithState
                    conn
                    "leak_state"
                    ( do
                        state <- newIORef (7 :: Int64)
                        stateWeak <- mkWeakIORef state (pure ())
                        -- The initializer returns each weak reference to the test.
                        atomicModifyIORef' states (\old -> (stateWeak : old, ()))
                        pure state
                    )
                    readIORef
                query_ conn "SELECT leak_state()" >>= \rows -> unless (rows == [Only (7 :: Int64)]) (fail "wrong state result")
                rejectedLog <- newIORef (0 :: Int64)
                logWeak <- mkWeakIORef rejectedLog (pure ())
                expectFailure (Logging.registerLogStorage conn "leak_log" (\_ -> void (readIORef rejectedLog)))
                void (execute_ conn "COPY (SELECT i FROM range(100) t(i)) TO 'unused' (FORMAT leak_copy)")
                expectFailure (execute_ conn "COPY (SELECT 1) TO 'unused' (FORMAT leak_copy_failure)")
                copyRefs <- atomicModifyIORef' copyStates (\refs -> ([], refs))
                stateRefs <- readIORef states
                pure (weak : logWeak : stateRefs <> copyRefs)
            batch = concat <$> replicateM 100 cycleCallback
            checkReleased refs = do
                -- Replace the last closure, then run a new transaction.
                createFunction conn "leak_scalar" (0 :: Int64)
                createFunction conn "leak_state" (0 :: Int64)
                void (query_ conn "SELECT 1" :: IO [Only Int64])
                performMajorGC
                performMajorGC
                live <- length . catMaybes <$> mapM deRefWeak refs
                unless (live == 0) (fail (show live <> " callback values remain live after replacement"))
        batch >>= checkReleased
        before <- readUsage
        forM_ [1 .. batches] \n -> do
            batch >>= checkReleased
            after <- readUsage
            reportOptional ("open connection, " <> show (n * 100) <> " callback cycles") before after

-- | Require an error without retaining the exception or its native resources.
expectFailure :: IO a -> IO ()
expectFailure action = do
    outcome <- try (void action) :: IO (Either SomeException ())
    case outcome of
        Left _ -> pure ()
        Right () -> fail "expected operation to fail"

-- | Reuse a connection after cancelling native execution and row decoding.
checkCancellation :: Int -> IO ()
checkCancellation batches =
    withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
        void (execute_ conn "SET streaming_buffer_size = '64KB'")
        let cancel action = do
                started <- newEmptyMVar
                done <- newEmptyMVar
                let signal = void (tryPutMVar started ())
                tid <- forkFinally (action signal) (\outcome -> putMVar done outcome >> signal)
                takeMVar started
                threadDelay 1000
                killThread tid
                outcome <- takeMVar done
                case outcome of
                    Left err | fromException err == Just ThreadKilled -> pure ()
                    Left err -> fail ("unexpected cancellation error: " <> show err)
                    Right () -> fail "query completed before cancellation"
                rows <- query_ conn "SELECT 42"
                unless (rows == [Only (42 :: Int64)]) (fail "connection failed after cancellation")
            batch = forM_ [1 .. 10 :: Int] \_ -> do
                cancel \signal -> do
                    createFunction conn "leak_cancel_started" (signal >> pure (1 :: Int64))
                    void
                        ( query_
                            conn
                            "WITH started AS MATERIALIZED (SELECT leak_cancel_started() AS seed) \
                            \SELECT sum(sin((a.i + b.j + started.seed)::DOUBLE)) \
                            \FROM started, range(1000000) a(i), range(1000000) b(j)" ::
                            IO [Only Double]
                        )
                cancel \signal -> do
                    signal
                    void (query_ conn "SELECT {'x': i, 'values': [i, i + 1]} FROM range(100000) t(i)" :: IO [Only FieldValue])
                cancel \signal -> do
                    blocked <- newEmptyMVar
                    -- Keep the first chunk live until the worker is cancelled.
                    void (fold_ conn "SELECT {'x': i, 'values': [i, NULL]} FROM range(100000) t(i)" (0 :: Int64) (\n (Only (_ :: FieldValue)) -> signal >> takeMVar blocked >> pure (n + 1)))
                forM_ [False, True] \arrow -> cancel \signal -> do
                    filtering <- newIORef False
                    createFunction conn "leak_stream_filter" \(_ :: Int64) -> do
                        active <- readIORef filtering
                        when active signal
                        pure active
                    let sql = "SELECT i FROM range(1000000000000) t(i) WHERE NOT leak_stream_filter(i)"
                        delivered = atomicWriteIORef filtering True
                    if arrow
                        then Streaming.foldArrow_ conn sql () (\() _ _ -> delivered)
                        else Streaming.fold_ conn sql () (\() (Only (_ :: Int64)) -> delivered)
        batch
        performMajorGC
        before <- readUsage
        forM_ [1 .. batches] \n -> do
            batch
            performMajorGC
            after <- readUsage
            reportOptional ("open connection, " <> show (n * 50) <> " cancellations") before after

-- | Report native counters when the operating system provides them.
reportOptional :: String -> Maybe Usage -> Maybe Usage -> IO ()
reportOptional label (Just before) (Just after) = report label before after
reportOptional label _ _ = putStrLn (label <> ": resource counters unavailable; functional checks passed")

-- | Print both measurements and fail if either one grew too much.
report :: String -> Usage -> Usage -> IO ()
report label before after = do
    checkRss <- (/= Just "0") <$> lookupEnv "DUCKDB_LEAK_RSS_CHECK"
    putStrLn label
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
    unless checkRss (putStrLn "RSS assertion disabled for native memory instrumentation")
    when (threadGrowth > threadSlack || (checkRss && rssGrowth > rssSlackKb)) do
        putStrLn ("FAIL: " <> label <> " retains DuckDB resources")
        exitFailure
  where
    threadGrowth = usageThreads after - usageThreads before
    rssGrowth = usageRssKb after - usageRssKb before
