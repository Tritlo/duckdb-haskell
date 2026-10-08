{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Cancellation during native execution must finish before handles are released.
module CancellationTests (cancellationTests) where

import Control.Concurrent (MVar, ThreadId, forkIOWithUnmask, newEmptyMVar, putMVar, readMVar, rtsSupportsBoundThreads, threadDelay, throwTo, tryPutMVar, tryReadMVar)
import Control.Exception (AsyncException (..), SomeException, bracket, fromException, mask_, try)
import Control.Monad (unless, void)
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import Data.Int (Int64)
import Database.DuckDB.FFI.Compat (c_duckdb_interrupt, c_duckdb_query)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Arrow (foldArrow_)
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Database.DuckDB.Simple.Internal (withConnectionHandle, withQueryCString, withResult)
import GHC.Conc (BlockReason (..), ThreadStatus (..), threadStatus)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Exercise each native execution entry point and two cancellation races.
cancellationTests :: TestTree
cancellationTests =
    testGroup "native cancellation" $
        [ testCase name $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
            assertBool "cancellation requires the threaded runtime" rtsSupportsBoundThreads
            entered <- newEmptyMVar
            createFunction conn "query_started" (signal entered >> pure (1 :: Int64))
            withCaller conn (pure ()) (run conn longQuery) \caller done -> do
                await "native query did not start" (readMVar entered)
                (_, sent) <- startCaller (throwTo caller UserInterrupt)
                outcome <- await "native query did not cancel" (readMVar done)
                assertAsync [UserInterrupt] outcome
                await "interrupt sender did not finish" (readMVar sent) >>= assertSucceeded
            assertReusable conn
        | (name, run) <-
            [ ("query_", \conn sql -> void (query_ conn sql :: IO [Only Double]))
            , ("query with a prepared statement", \conn sql -> void (query conn sql () :: IO [Only Double]))
            , ("execute_", \conn sql -> void (execute_ conn sql))
            , ("execute with a prepared statement", \conn sql -> void (execute conn sql ()))
            , ("statement cursor", \conn sql -> withStatement conn sql \stmt -> void (nextRow stmt :: IO (Maybe (Only Double))))
            , ("fold_", \conn sql -> void (fold_ conn sql (0 :: Double) (\acc (Only value) -> pure (acc + value))))
            , ("Arrow fold", \conn sql -> foldArrow_ conn sql () (\() _ _ -> pure ()))
            ]
        ]
            <> [ testCase name $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
                    void (execute_ conn "SET streaming_buffer_size = '64KB'")
                    filtering <- newIORef False
                    entered <- newEmptyMVar
                    createFunction conn "discard_remaining" \(_ :: Int64) -> do
                        active <- readIORef filtering
                        if active then signal entered >> pure True else pure False
                    let sql = "SELECT i FROM range(1000000000000) t(i) WHERE NOT discard_remaining(i)"
                    withCaller conn (pure ()) (run conn sql (atomicWriteIORef filtering True)) \caller done -> do
                        await "fetch did not start after the first delivered batch" (readMVar entered)
                        (_, sent) <- startCaller (throwTo caller UserInterrupt)
                        await "streaming fetch did not cancel" (readMVar done) >>= assertAsync [UserInterrupt]
                        await "interrupt sender did not finish" (readMVar sent) >>= assertSucceeded
                    assertReusable conn
               | (name, run) <-
                    [ ("cancel a native row fetch", \conn sql delivered -> Streaming.fold_ conn sql () (\() (Only (_ :: Int64)) -> delivered))
                    , ("cancel a native Arrow fetch", \conn sql delivered -> Streaming.foldArrow_ conn sql () (\() _ _ -> delivered))
                    ,
                        ( "mixed cursor entry points retain interruptible fetching"
                        , \conn sql delivered -> withStatement conn sql \stmt -> do
                            Streaming.nextRow stmt >>= (@?= Just (Only (0 :: Int64)))
                            delivered
                            let consume =
                                    nextRow stmt >>= \case
                                        Nothing -> pure ()
                                        Just (Only (_ :: Int64)) -> consume
                            consume
                        )
                    ]
               ]
            <> [ testCase "cancellation before native entry is not cleared by startup" $
                    withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
                        queued <- newEmptyMVar
                        begin <- newEmptyMVar
                        createFunction conn "query_started" (pure 1 :: IO Int64)
                        let action = withConnectionHandle conn \handle ->
                                withQueryCString longQuery \sql ->
                                    withResult
                                        conn
                                        longQuery
                                        (\result -> signal queued >> readMVar begin >> c_duckdb_query handle sql result)
                                        (const (pure ()))
                        withCaller conn (signal begin) action \caller done -> do
                            await "query worker did not reach its entry gate" (readMVar queued)
                            (_, sent) <- startCaller (throwTo caller UserInterrupt)
                            await "first interrupt was not delivered" (readMVar sent) >>= assertSucceeded
                            awaitCleanup caller
                            signal begin
                            await "startup cleared the cancellation" (readMVar done) >>= assertAsync [UserInterrupt]
                        assertReusable conn
               , testCase "a second cancellation waits for a blocked callback to return" $
                    withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
                        entered <- newEmptyMVar
                        release <- newEmptyMVar
                        createFunction conn "blocked_callback" (signal entered >> readMVar release >> pure (1 :: Int64))
                        withCaller conn (signal release) (void (query_ conn "SELECT blocked_callback()" :: IO [Only Int64])) \caller done -> do
                            await "callback did not start" (readMVar entered)
                            (_, firstSent) <- startCaller (throwTo caller UserInterrupt)
                            await "first interrupt was not delivered" (readMVar firstSent) >>= assertSucceeded
                            secondAttempted <- newEmptyMVar
                            (_, secondSent) <- startCaller (signal secondAttempted >> throwTo caller ThreadKilled)
                            await "second interrupt sender did not start" (readMVar secondAttempted)
                            premature <- timeout 50000 (readMVar secondSent)
                            case premature of
                                Nothing -> pure ()
                                Just _ -> assertFailure "second interrupt escaped cleanup"
                            tryReadMVar done >>= \case
                                Nothing -> pure ()
                                Just _ -> assertFailure "query returned while its callback was still running"
                            signal release
                            await "callback cancellation did not finish" (readMVar done) >>= assertAsync [UserInterrupt, ThreadKilled]
                            await "second interrupt sender did not finish" (readMVar secondSent) >>= assertSucceeded
                        assertReusable conn
               ]

-- | Run one volatile callback before a CPU query that cannot finish in the test window.
longQuery :: Query
longQuery =
    "WITH started AS MATERIALIZED (SELECT query_started() AS seed) \
    \SELECT sum(sin((a.i + b.j + started.seed)::DOUBLE)) \
    \FROM started, range(1000000) a(i), range(1000000) b(j)"

-- | Publish a thread's outcome while asynchronous exceptions are masked.
startCaller :: IO () -> IO (ThreadId, MVar (Either SomeException ()))
startCaller action = mask_ do
    done <- newEmptyMVar
    caller <- forkIOWithUnmask \unmask -> try (unmask action) >>= putMVar done
    pure (caller, done)

-- | Release gates and interrupt unfinished SQL before the connection can close.
withCaller :: Connection -> IO () -> IO () -> (ThreadId -> MVar (Either SomeException ()) -> IO a) -> IO a
withCaller conn release action use =
    bracket (startCaller action) cleanup (uncurry use)
  where
    cleanup (_, done) = do
        release
        await "emergency query cleanup did not finish" (stop done)
    stop done = do
        finished <- tryReadMVar done
        case finished of
            Just _ -> pure ()
            Nothing -> do
                withConnectionHandle conn c_duckdb_interrupt
                threadDelay 10000
                stop done

-- | Wait until the cancelled caller has reached its interrupt-and-join loop.
awaitCleanup :: ThreadId -> IO ()
awaitCleanup caller = awaitCondition "caller did not enter cancellation cleanup" do
    threadStatus caller >>= \case
        ThreadBlocked BlockedOnForeignCall -> pure False
        ThreadBlocked _ -> pure True
        ThreadFinished -> assertFailure "caller finished before the native worker" >> pure False
        ThreadDied -> assertFailure "caller died before publishing its result" >> pure False
        ThreadRunning -> pure False

-- | Bound waits in the test thread, independently of the cancelled caller.
await :: String -> IO a -> IO a
await message action = timeout 5000000 action >>= maybe (assertFailure message >> fail message) pure

-- | Wait for a thread state without relying on an arbitrary scheduling delay.
awaitCondition :: String -> IO Bool -> IO ()
awaitCondition message condition = await message loop
  where
    loop = do
        ready <- condition
        unless ready (threadDelay 1000 >> loop)

-- | Open a test gate once; cleanup can safely call this again.
signal :: MVar () -> IO ()
signal gate = void (tryPutMVar gate ())

-- | Require the requested asynchronous exception to reach the caller.
assertAsync :: [AsyncException] -> Either SomeException () -> Assertion
assertAsync expected = \case
    Left err -> case fromException err of
        Just actual -> assertBool ("unexpected asynchronous exception: " <> show actual) (actual `elem` expected)
        Nothing -> assertFailure ("expected asynchronous cancellation, got " <> show err)
    Right () -> assertFailure "query completed instead of reporting cancellation"

-- | Check that an interrupt sender completed normally.
assertSucceeded :: Either SomeException () -> Assertion
assertSucceeded = either (assertFailure . show) pure

-- | The interrupted native query must have released its connection state.
assertReusable :: Connection -> Assertion
assertReusable conn = do
    rows <- query_ conn "SELECT 42" :: IO [Only Int64]
    rows @?= [Only 42]
