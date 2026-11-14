{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : CancellationTests
-- Description : Cancellation during native execution must finish before handles are released, and a callback must not wait without a bound for its own connection.
module CancellationTests (cancellationTests, callbackLockTests) where

import Control.Concurrent (
  MVar,
  ThreadId,
  forkIOWithUnmask,
  myThreadId,
  newEmptyMVar,
  putMVar,
  readMVar,
  rtsSupportsBoundThreads,
  threadDelay,
  throwTo,
  tryPutMVar,
  tryReadMVar,
 )
import Control.Exception (AsyncException (..), ErrorCall (..), SomeException, bracket, fromException, mask_, throwIO, try)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Arrow (foldArrow_)
import Database.DuckDB.Simple.Internal (
  c_duckdb_v2_connection_interrupt,
  c_duckdb_v2_result_drain,
  checked,
  output,
  runInterruptibleQuery,
  withConnectionHandle,
  withResult,
 )
import qualified Database.DuckDB.Simple.Streaming as Streaming
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
               let action = withStatement conn longQuery \stmt ->
                     withResult stmt \result -> runInterruptibleQuery conn do
                       signal queued
                       readMVar begin
                       void (output (c_duckdb_v2_result_drain result))
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

-- | A callback that runs a statement on the connection that runs the callback
-- waits for the lock of that connection. Each case sets a bound of 0.2 s.
callbackLockTests :: TestTree
callbackLockTests =
  testGroup
    "a callback that waits for a connection lock"
    [ testCase "a callback that runs a statement on its own connection in query_ fails at the bound" $
        withBoundedConnections "1" testLockWait \conn _ -> do
          (threads, errors) <- registerStatementCallback conn conn
          outcome <- boundedOuterCall 30000000 threads (void (query_ conn "SELECT own_statement(i) FROM range(3) t(i)" :: IO [Only Int64]))
          assertBoundError outcome
          assertInnerErrors errors
          assertReusable conn
    , testCase "a callback that runs a statement on its own connection in a later stream fetch fails at the bound" $
        withBoundedConnections "1" testLockWait \conn _ -> do
          (threads, errors) <- registerStatementCallback conn conn
          rows <- newIORef (0 :: Int)
          void (execute_ conn "SET streaming_buffer_size = '16KB'")
          outcome <- boundedOuterCall 30000000 threads $ withStatement conn laterFetchQuery \stmt -> do
            let consume =
                  Streaming.nextRow stmt >>= \case
                    Nothing -> pure ()
                    Just (Only (_ :: Int64)) -> modifyIORef' rows (+ 1) >> consume
            consume
          assertBoundError outcome
          assertInnerErrors errors
          readIORef rows >>= \count -> assertBool "the callback ran before the stream delivered a row" (count > 0)
          assertReusable conn
    , testCase "a callback that runs a statement on its own connection in a streaming fold fails at the bound" $
        withBoundedConnections "1" testLockWait \conn _ -> do
          (threads, errors) <- registerStatementCallback conn conn
          rows <- newIORef (0 :: Int)
          void (execute_ conn "SET streaming_buffer_size = '16KB'")
          outcome <- boundedOuterCall 30000000 threads $ Streaming.fold_ conn laterFetchQuery () \() (Only (_ :: Int64)) -> modifyIORef' rows (+ 1)
          assertBoundError outcome
          assertInnerErrors errors
          readIORef rows >>= \count -> assertBool "the callback ran before the fold received a row" (count > 0)
          assertReusable conn
    , testCase "callbacks on 4 DuckDB threads that run statements on their own connection each fail at the bound" $
        withBoundedConnections "4" testLockWait \conn _ -> do
          void (execute_ conn "CREATE TABLE callback_rows AS SELECT i FROM range(1000000) t(i)")
          (threads, errors) <- registerStatementCallback conn conn
          outcome <- boundedOuterCall 30000000 threads (void (query_ conn "SELECT sum(own_statement(i)) FROM callback_rows" :: IO [Only Integer]))
          assertBoundError outcome
          assertInnerErrors errors
          assertReusable conn
    , testCase "callbacks on 4 DuckDB threads can share a second connection" $
        withBoundedConnections "4" (Just defaultCallbackLockWait) \conn other -> do
          void (execute_ conn "CREATE TABLE callback_rows AS SELECT i FROM range(1000000) t(i)")
          threads <- newIORef []
          createFunction conn "other_statement" \(i :: Int64) -> do
            when (i `mod` 200 == 0) do
              myThreadId >>= \self -> atomicModifyIORef' threads (\known -> (self : known, ()))
              query_ other "SELECT 1" >>= (@?= [Only (1 :: Int64)])
            pure (i `mod` 7)
          result <- newIORef []
          outcome <- boundedOuterCall 120000000 threads (query_ conn "SELECT sum(other_statement(i)) FROM callback_rows" >>= atomicWriteIORef result)
          either (assertFailure . show) pure outcome
          readIORef result >>= (@?= [Only (sum [i `mod` 7 | i <- [0 .. 999999 :: Integer]])])
          assertReusable other
    , testCase "the callback lock wait rejects a negative bound and accepts no bound" $ do
        negative <- try (openWithOptions ":memory:" defaultConnectionOptions{connectionCallbackLockWait = Just (-1)})
        case negative of
          Left (err :: SQLError) -> assertBool ("unexpected message: " <> show err) ("connectionCallbackLockWait" `Text.isInfixOf` sqlErrorMessage err)
          Right conn -> close conn >> assertFailure "a negative bound opened a connection"
        withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionCallbackLockWait = Nothing} assertReusable
    ]

-- | The bound of the cases in microseconds: 0.2 s.
testLockWait :: Maybe Int
testLockWait = Just 200000

-- | Run an action with two connections of one instance, each with the bound.
withBoundedConnections :: Text -> Maybe Int -> (Connection -> Connection -> IO a) -> IO a
withBoundedConnections threads bound action =
  withEnvironment \environment -> withInstance environment [("threads", threads)] \inst -> do
    attachDatabase inst ":memory:" "callback_lock" []
    setDefaultDatabase inst "callback_lock"
    let options = defaultConnectionOptions{connectionCallbackLockWait = bound}
    bracket (connectWithOptions inst options) close \conn ->
      bracket (connectWithOptions inst options) close (action conn)

-- | Register @own_statement@ on a connection. It runs a statement on the target
-- connection, records its thread and the error of the statement, and raises the error.
registerStatementCallback :: Connection -> Connection -> IO (IORef [ThreadId], IORef [SQLError])
registerStatementCallback conn target = do
  threads <- newIORef []
  errors <- newIORef []
  createFunction conn "own_statement" \(i :: Int64) -> do
    myThreadId >>= \self -> atomicModifyIORef' threads (\known -> (self : known, ()))
    try (query_ target "SELECT 1" :: IO [Only Int64]) >>= \case
      Left err -> atomicModifyIORef' errors (\known -> (err : known, ())) >> throwIO err
      Right _ -> pure i
  pure (threads, errors)

-- | The stream delivers 500000 rows before DuckDB computes a row that calls the
-- callback. With a small stream buffer, a later fetch computes that row.
laterFetchQuery :: Query
laterFetchQuery = "SELECT CASE WHEN i >= 500000 AND i % 100000 = 0 THEN own_statement(i) ELSE i END FROM range(1000000) t(i)"

-- | Run an outer call in its own thread, and wait for it at most the test
-- bound. When that bound ends, throw to each callback thread in the list. Then
-- the outer call returns, and the case fails and does not hang.
boundedOuterCall :: Int -> IORef [ThreadId] -> IO () -> IO (Either SomeException ())
boundedOuterCall micros threads action = do
  (_, done) <- startCaller action
  timeout micros (readMVar done) >>= \case
    Just outcome -> pure outcome
    Nothing -> do
      readIORef threads >>= mapM_ (`throwTo` ErrorCall "test bound: release the callback")
      _ <- await "the outer call did not return after the release of its callbacks" (readMVar done)
      assertFailure "the outer call did not return within the test bound"

-- | Require the error of a callback that waited for the bound. DuckDB reports
-- the error of the callback with its own error type and the text of the error.
assertBoundError :: Either SomeException () -> Assertion
assertBoundError = \case
  Left err -> case fromException err of
    Just (failure :: SQLError) -> do
      let message = sqlErrorMessage failure
      assertBool
        ("the rule is missing: " <> show failure)
        ("A callback must not run a statement on the connection that runs the callback" `Text.isInfixOf` message)
      assertBool ("the bound is missing: " <> show failure) ("waited 0.2 s" `Text.isInfixOf` message)
    Nothing -> assertFailure ("expected an SQLError, got " <> show err)
  Right () -> assertFailure "the outer call returned without the error of the bound"

-- | Require that each statement of a callback failed with the busy error type.
assertInnerErrors :: IORef [SQLError] -> Assertion
assertInnerErrors errors = do
  known <- readIORef errors
  assertBool "no statement of a callback failed" (not (null known))
  map sqlErrorType known @?= map (const (Just ErrorResourceInUse)) known

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
          withConnectionHandle conn (\raw -> checked (c_duckdb_v2_connection_interrupt raw))
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
