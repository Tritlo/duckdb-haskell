{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Main
-- Description : Regression check for leaked DuckDB handles.
--
-- The handles live in C memory, so the GHC heap statistics cannot see them. The
-- open/close check counts native threads and resident memory. The sustained
-- checks keep one connection open through repeated queries, failures, cursor
-- resets, callback replacement, and cancellation. Weak references check callback
-- state release independently of resident memory.
--
-- Each check runs in this dedicated process. On systems without
-- @/proc/self/status@, the native resource counters are unavailable. The
-- functional checks and callback collection checks still run.
module Main (main) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Exception (
  AsyncException (ThreadKilled),
  BlockedIndefinitelyOnMVar (..),
  IOException,
  SomeException,
  evaluate,
  finally,
  fromException,
  throwIO,
  try,
 )
import Control.Monad (forM, forM_, replicateM, replicateM_, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.Geometry as G
import Data.IORef (atomicModifyIORef', atomicWriteIORef, mkWeakIORef, newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isPrefixOf, minimumBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Arrow as Arrow
import qualified Database.DuckDB.Simple.Copy as Copy
import Database.DuckDB.Simple.FromField (BigNum (..), DecimalValue (..), FieldValue (..), StructValue, UnionValue)
import Database.DuckDB.Simple.Geometry (RawGeometry (..), fromRawGeometry)
import qualified Database.DuckDB.Simple.Logging as Logging
import Database.DuckDB.Simple.Registration (TypeDeclaration (..), registerCustomType)
import qualified Database.DuckDB.Simple.Streaming as Streaming
import qualified Database.DuckDB.Simple.V2.Registration as R
import Database.DuckDB.Simple.Variant (Variant (..), variantObject)
import GHC.Weak.Finalize (getFinalizerExceptionHandler, setFinalizerExceptionHandler)
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

-- | Read the current resource counters.  The result is 'Nothing' if the system
-- has no @\/proc\/self\/status@.
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
  values <- query conn "SELECT ?" (Only (Variant (FieldInt64 42)))
  unless (values == [Only (Variant (FieldInt64 42))]) (fail "wrong VARIANT payload")
  -- The reader of a VARIANT value fails while it reads the chunk. See
  -- breakVariantReads. The result and connection must still be destroyed.
  breakVariantReads conn
  rejected <- try (query_ conn rejectedQuery) :: IO (Either SomeException [Only FieldValue])
  case rejected of
    Left _ -> pure ()
    Right _ -> fail "expected the failure of the VARIANT reader"
  close conn

main :: IO ()
main = do
  args <- getArgs
  let (mode, batches) = case args of
        [] -> ("all", 10)
        [name] -> (name, 10)
        [name, n] | Just count <- readMaybe n, count > 0 -> (name, count)
        _ -> ("invalid", 0)
  unless (mode `elem` ["all", "open-close", "dropped-handles", "long-lived", "callbacks", "cancel", "decode-failure", "new-types", "typed"]) $
    fail "Expected [all|open-close|dropped-handles|long-lived|callbacks|cancel|decode-failure|new-types|typed] [positive batch count]"
  when (mode `elem` ["all", "open-close"]) checkOpenClose
  when (mode `elem` ["all", "dropped-handles"]) checkDroppedHandles
  when (mode `elem` ["all", "long-lived"]) (checkLongLived batches)
  when (mode `elem` ["all", "callbacks"]) (checkCallbacks batches)
  when (mode `elem` ["all", "cancel"]) (checkCancellation batches)
  when (mode == "decode-failure") checkDecodeFailure
  when (mode `elem` ["all", "new-types"]) (checkNewTypes batches)
  when (mode `elem` ["all", "typed"]) (checkTyped batches)

-- | Drop a connection and one of its statements without closing them. The
-- statement is younger than the connection, so the collector can run the
-- finalizer of the connection first. Cleanup must still release the database,
-- and no finalizer may wait for a finalizer that runs after it.
checkDroppedHandles :: IO ()
checkDroppedHandles = do
  blocked <- newIORef (0 :: Int)
  previous <- getFinalizerExceptionHandler
  let record err = do
        case fromException err of
          Just BlockedIndefinitelyOnMVar -> atomicModifyIORef' blocked (\count -> (count + 1, ()))
          Nothing -> pure ()
        previous err
      -- The database owns the callback, and the callback retains the token.
      allocate before after = do
        token <- newIORef (42 :: Int64)
        weak <- mkWeakIORef token (pure ())
        conn <- open ":memory:"
        createFunction conn "dropped_token" (readIORef token)
        -- Decoding a VARIANT opens the helper that the finalizer must release.
        values <- query conn "SELECT ?" (Only (Variant (FieldInt64 42)))
        unless (values == [Only (Variant (FieldInt64 42))]) (fail "wrong VARIANT payload before the drop")
        replicateM_ before performMajorGC
        stmt <- openStatement conn "SELECT dropped_token()"
        replicateM_ after performMajorGC
        row <- nextRow stmt
        unless (row == Just (Only (42 :: Int64))) (fail "wrong row before the drop")
        pure weak
      released weak attempts
        | attempts == (0 :: Int) = fail "a dropped connection kept its database open"
        | otherwise = do
            performMajorGC
            token <- deRefWeak weak
            when (isJust token) (threadDelay 10000 >> released weak (attempts - 1))
  flip finally (setFinalizerExceptionHandler previous) do
    setFinalizerExceptionHandler record
    forM_ [(before, after) | before <- [0 .. 3 :: Int], after <- [0, 1 :: Int]] \(before, after) ->
      allocate before after >>= \weak -> released weak 500
    count <- readIORef blocked
    unless (count == 0) (fail ("a finalizer waited for a later finalizer " <> show count <> " time(s)"))

-- | Import raw WKB and release native values after failed nested construction.
checkNewTypes :: Int -> IO ()
checkNewTypes batches =
  withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
    [Only geometry] <- query_ conn "SELECT 'POINT ZM (1 2 3 4)'::GEOMETRY('OGC:CRS84')" :: IO [Only RawGeometry]
    typed <- either fail pure (fromRawGeometry geometry) :: IO G.Geometry
    -- These values come from DuckDB, so that they have the native types.
    [Only collated] <- query_ conn "SELECT {'text': 'A' COLLATE nocase, 'items': ['B' COLLATE nocase]}" :: IO [Only (StructValue FieldValue)]
    [Only union] <- query_ conn "SELECT union_value(t := 'λ')::UNION(n BIGINT, t VARCHAR)" :: IO [Only (UnionValue FieldValue)]
    let emptyKey = Variant (variantObject [("", FieldWord64 1), ("n", FieldWord64 2)])
        foldedKeys = Variant (variantObject [("A", FieldWord64 1), ("a", FieldWord64 2), ("b", FieldWord64 3)])
        -- The encoded value of this number has more than 100 bytes.
        large = 2 ^ (1000 :: Int) + 12345 :: Integer
        roundTrip :: (ToField a, FromField a, Eq a) => String -> a -> IO ()
        roundTrip label value = query conn "SELECT ?" (Only value) >>= \rows -> unless (rows == [Only value]) (fail (label <> " binding failed"))
    let variant = Variant (FieldList [FieldInt64 42, FieldList [FieldNull, FieldText "before\0after"]])
        custom = geometry{rawGeometryCRS = Just "local' íslenska λ"}
        batch = forM_ [1 .. 100 :: Int] \_ -> do
          roundTrip "custom CRS" custom
          expectFailure (query conn "SELECT ?" (Only geometry{rawGeometryCRS = Just " "}) :: IO [Only RawGeometry])
          rows <-
            query
              conn
              "SELECT ?, system.main.ST_SetCRS(system.main.ST_GeomFromWKB(?), ?), ?"
              (variant, rawGeometryWKB geometry, rawGeometryCRS geometry, typed)
          unless (rows == [(variant, geometry, typed)]) (fail "new type round trip failed")
          expectFailure (query conn "SELECT ?, system.main.ST_GeomFromWKB(?)" (typed, BS.pack [1, 1, 0, 0, 0]) :: IO [(G.Geometry, RawGeometry)])
          expectFailure (query conn "SELECT system.main.ST_GeomFromWKB(?)" (Only (rawGeometryWKB geometry)) :: IO [Only Int64])
          expectFailure
            (query conn "SELECT ?" (Only (Variant (FieldList [variantPayload variant, FieldDecimal (DecimalValue 0 0 0)]))) :: IO [Only Variant])
          expectFailure (query conn "SELECT ?" (Only variant) :: IO [Only Int64])
          -- STRUCT field names cannot be an empty key or two keys that differ
          -- only in case. Thus these objects use the extraction query. The two
          -- key counts replace the kept extraction statement.
          objects <- query_ conn "SELECT json_object('', 1, 'n', 2)::VARIANT, json_object('A', 1, 'a', 2, 'b', 3)::VARIANT"
          unless (objects == [(emptyKey, foldedKeys)]) (fail "VARIANT object read failed")
          roundTrip "collated STRUCT" collated
          roundTrip "UNION" union
          roundTrip "MAP" (Map.fromList [(1 :: Int64, [10, 11 :: Int64]), (2, [])])
          roundTrip "LIST" [[1, 2], [3 :: Int64]]
          query conn "SELECT ?::VARCHAR" (Only (BigNum large)) >>= \text ->
            unless (text == [Only (show large)]) (fail "BIGNUM binding failed")
    batch
    performMajorGC
    before <- readUsage
    forM_ [1 .. batches] \n -> do
      batch
      performMajorGC
      after <- readUsage
      reportOptional ("open connection, " <> show (n * 100) <> " new type cycles") before after

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
  withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
    breakVariantReads conn
    withStatement conn "SELECT ?::BIGINT FROM range(10000)" \stmt -> do
      let cycleQuery = do
            rows <- query_ conn "SELECT sum(i)::BIGINT FROM range(10000) t(i)"
            unless (rows == [Only (49995000 :: Int64)]) (fail "wrong query result")
            expectFailure
              (query_ conn "SELECT {'xs': [i, i + 1, i + 2, i + 3], 'bad': 42::BIGINT::VARIANT} FROM range(10000) t(i)" :: IO [Only FieldValue])
            expectFailure (query_ conn "SELECT CAST('bad' AS BIGINT)" :: IO [Only Int64])
            bind stmt [toField (42 :: Int64)]
            nextRow stmt >>= \row -> unless (row == Just (Only (42 :: Int64))) (fail "wrong cursor result")
            clearStatementBindings stmt
            expectFailure (nextRow stmt :: IO (Maybe (Only Int64)))
            -- Close a statement while its cursor holds the other chunks of a
            -- materialized result, and then while its cursor holds a native stream.
            withStatement conn "SELECT i FROM range(100000) t(i)" \cursor -> void (nextRow cursor :: IO (Maybe (Only Int64)))
            withStatement conn "SELECT i FROM range(100000) t(i)" \cursor -> void (Streaming.nextRow cursor :: IO (Maybe (Only Int64)))
            -- The step fails at the first batch of a materialized Arrow fold.
            -- The fold must release each batch that it did not deliver.
            expectFailure (Arrow.foldArrow_ conn "SELECT i FROM range(100000) t(i)" () (\() _ _ -> fail "step"))
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
    breakVariantReads conn
    let rejected = expectFailure (query_ conn rejectedQuery :: IO [Only FieldValue])
    rejected
    performMajorGC
    before <- readUsage
    forM_ [1 .. cycles] (const rejected)
    performMajorGC
    after <- readUsage
    reportOptional ("open connection, " <> show cycles <> " failed VARIANT reads") before after

-- | Check that callback closures and query state are released before close.
checkCallbacks :: Int -> IO ()
checkCallbacks batches =
  withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
    copyStates <- newIORef []
    let newCopyState = do
          state <- newIORef (7 :: Int64)
          weak <- mkWeakIORef state (pure ())
          atomicModifyIORef' copyStates (\old -> (weak : old, ()))
          pure state
    Copy.registerCopyToFunction conn "leak_copy" (\_ -> newCopyState) (\_ -> newCopyState) (\_ _ -> pure ()) (\_ -> pure ())
    Copy.registerCopyToFunction
      conn
      "leak_copy_failure"
      (\_ -> newCopyState)
      (\_ -> fail "COPY init failure" :: IO ())
      (\_ _ -> pure ())
      (\_ -> pure ())
    -- Register leak_log once, so that the registration in each cycle fails and
    -- the test can check that the rejected callback is released.
    Logging.registerLogStorage conn "leak_log" (\_ -> pure ())
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

-- | Make VARIANT metadata preparation and object extraction fail. A prepared
-- metadata plan can survive a lower expression depth. 'rejectedQuery' uses an
-- unusual key, so its extraction query still binds on each execution and fails
-- at depth 3. Set the caller's SESSION depth first, so its control statements
-- can still bind after the GLOBAL depth changes.
breakVariantReads :: Connection -> IO ()
breakVariantReads conn = do
  void (execute_ conn "SET SESSION max_expression_depth = 1000")
  void (execute_ conn "SET GLOBAL max_expression_depth = 3")

-- | A query whose result fails in the VARIANT reader after 'breakVariantReads'.
rejectedQuery :: Query
rejectedQuery = "SELECT {'xs': [i, i + 1, i + 2, i + 3], 'bad': json_object('', i)::VARIANT} FROM range(100000) t(i)"

-- | Require an error without retaining the exception or its native resources.
expectFailure :: IO a -> IO ()
expectFailure action = do
  outcome <- try (void action) :: IO (Either SomeException ())
  case outcome of
    Left _ -> pure ()
    Right () -> fail "expected operation to fail"

-- | Reuse a connection after cancelling native execution and row decoding.
--
-- Each batch reports its resident memory and fails if the thread count grew
-- too much. The rule for the resident memory comes after the last batch. The
-- resident memory rises in the first batches, and later single samples go up
-- and down. A leak never returns memory, so it raises each later sample, also
-- the lowest sample. A single high sample does not raise the lowest sample.
-- Thus the rule compares the lowest sample of the second half of the batches
-- with the lowest sample of batch 3 to the half. Batches 1 and 2 are not in the
-- rule, because the memory can still rise in them. The rule needs at least 6
-- batches. With fewer batches, the check does not compare the memory.
checkCancellation :: Int -> IO ()
checkCancellation batches =
  withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
    void (execute_ conn "SET streaming_buffer_size = '64KB'")
    let run mayFinish action = do
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
            Right ()
              | mayFinish -> pure ()
              | otherwise -> fail "query completed before cancellation"
          rows <- query_ conn "SELECT 42"
          unless (rows == [Only (42 :: Int64)]) (fail "connection failed after cancellation")
        -- These queries block or run until the thread is killed.
        cancel = run False
        -- These finite queries can finish before the interrupt reaches them.
        interrupt = run True
        batch = forM_ [1 .. 10 :: Int] \_ -> do
          cancel \signal -> do
            createFunction conn "leak_cancel_started" (signal >> pure (1 :: Int64))
            void
              ( query_
                  conn
                  "WITH started AS MATERIALIZED (SELECT leak_cancel_started() AS seed) \
                  \SELECT sum(sin((a.i + b.j + started.seed)::DOUBLE)) \
                  \FROM started, range(1000000) a(i), range(1000000) b(j)"
                  :: IO [Only Double]
              )
          interrupt \signal -> do
            signal
            void (query_ conn "SELECT {'x': i, 'values': [i, i + 1]} FROM range(100000) t(i)" :: IO [Only FieldValue])
          cancel \signal -> do
            blocked <- newEmptyMVar
            -- Keep the first chunk live until the worker is cancelled.
            void
              ( fold_
                  conn
                  "SELECT {'x': i, 'values': [i, NULL]} FROM range(100000) t(i)"
                  (0 :: Int64)
                  (\n (Only (_ :: FieldValue)) -> signal >> takeMVar blocked >> pure (n + 1))
              )
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
          interrupt \signal -> do
            signal
            void (query_ conn "SELECT {'x': i, 'values': [i, i + 1]}::VARIANT FROM range(100000) t(i)" :: IO [Only FieldValue])
          cancel \signal -> do
            blocked <- newEmptyMVar
            void
              ( fold_
                  conn
                  "SELECT 'POINT (1 2)'::GEOMETRY('OGC:CRS84') FROM range(100000)"
                  (0 :: Int64)
                  (\n (Only (_ :: RawGeometry)) -> signal >> takeMVar blocked >> pure (n + 1))
              )
    batch
    performMajorGC
    before <- readUsage
    samples <- forM [1 .. batches] \n -> do
      batch
      performMajorGC
      after <- readUsage
      let label = "open connection, " <> show (n * 70) <> " cancellation attempts"
      case (before, after) of
        (Just first, Just current) -> reportWith False label first current
        _ -> reportOptional label before after
      pure after
    case sequence samples of
      Just usages | batches >= 6 -> do
        let half = batches `div` 2
            (early, late) = splitAt half usages
            lowest = minimumBy (comparing usageRssKb)
            label = "open connection, lowest rss of cancellation batches 3 to " <> show half <> " -> batches " <> show (half + 1) <> " to " <> show batches
        report label (lowest (drop 2 early)) (lowest late)
      _ -> pure ()

-- | Report native counters when the operating system provides them.
reportOptional :: String -> Maybe Usage -> Maybe Usage -> IO ()
reportOptional label (Just before) (Just after) = report label before after
reportOptional label _ _ = putStrLn (label <> ": resource counters unavailable; functional checks passed")

-- | Print both measurements and fail if either one grew too much.
report :: String -> Usage -> Usage -> IO ()
report = reportWith True

-- | Print both measurements. Fail if the thread count grew too much. If the
-- first argument is 'True', also fail if the resident memory grew too much.
reportWith :: Bool -> String -> Usage -> Usage -> IO ()
reportWith rssRule label before after = do
  enabled <- (/= Just "0") <$> lookupEnv "DUCKDB_LEAK_RSS_CHECK"
  let checkRss = enabled && rssRule
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
  unless enabled (putStrLn "RSS assertion disabled for native memory instrumentation")
  when (threadGrowth > threadSlack || (checkRss && rssGrowth > rssSlackKb)) do
    putStrLn ("FAIL: " <> label <> " retains DuckDB resources")
    exitFailure
  where
    threadGrowth = usageThreads after - usageThreads before
    rssGrowth = usageRssKb after - usageRssKb before

-- | Run the typed registrations through the public API on one connection: a
-- scalar function with VARCHAR and BIGINT results over 5000 rows, a grouped
-- aggregate with VARCHAR results, a typed table function, and a cast that
-- fails.
-- Each VARCHAR value is longer than 12 bytes, so DuckDB stores it outside the
-- string slot.
checkTyped :: Int -> IO ()
checkTyped batches =
  withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
    let label prefix n = Text.pack (prefix <> "-" <> show n <> "-with-a-long-suffix")
        numbers = [0 .. 4999] :: [Int64]
        labelLength prefix = sum (map (fromIntegral . Text.length . label prefix) numbers) :: Int64
        groups = [(group, label "total" (sum [n | n <- numbers, n `mod` 50 == group])) | group <- [0 .. 49]]
    createFunction conn "leak_text" (label "row" :: Int64 -> Text)
    createFunction conn "leak_double" ((* 2) :: Int64 -> Int64)
    createAggregate
      conn
      "leak_total"
      Aggregate
        { aggregateInitial = 0 :: Int64
        , aggregateStep = \total (Only n) -> pure (total + n)
        , aggregateCombine = \a b -> pure (a + b)
        , aggregateResult = pure . label "total"
        }
    createTableFunction conn "leak_rows" ["n", "label"] \(Only count) ->
      pure [(n, label "row" n) | n <- [0 .. count - 1 :: Int64]]
    registerCustomType conn "leak_tag" (TypeExpression "VARCHAR")
    -- Each cycle fails the cast, so that the error path of the callback runs.
    R.registerCast conn (TypeExpression "BIGINT") (TypeExpression "leak_tag") Nothing \_ _ -> throwIO (userError "leak cast refused the value")
    let cycleTyped = do
          scalar <- query_ conn "SELECT sum(length(leak_text(i)))::BIGINT, sum(leak_double(i))::BIGINT FROM range(5000) t(i)"
          unless (scalar == [(labelLength "row", 2 * sum numbers)]) (fail "wrong typed scalar result")
          grouped <- query_ conn "SELECT i % 50 AS g, leak_total(i) FROM range(5000) t(i) GROUP BY g ORDER BY g"
          unless (grouped == groups) (fail "wrong typed aggregate result")
          table <- query_ conn "SELECT count(*), sum(n)::BIGINT, sum(length(label))::BIGINT FROM leak_rows(5000)"
          unless (table == [(5000 :: Int64, sum numbers, labelLength "row")]) (fail "wrong typed table function result")
          cast <- try (query_ conn "SELECT CAST(i AS leak_tag) FROM range(5000) t(i)" :: IO [Only Text])
          case cast of
            Left (err :: SQLError) | sqlErrorType err == Just ErrorTypeMismatch -> pure ()
            _ -> fail "wrong cast result"
        batch = forM_ [1 .. 20 :: Int] (const cycleTyped)
    batch
    performMajorGC
    before <- readUsage
    forM_ [1 .. batches] \n -> do
      batch
      performMajorGC
      after <- readUsage
      reportOptional ("open connection, " <> show (n * 20) <> " typed registration cycles") before after
