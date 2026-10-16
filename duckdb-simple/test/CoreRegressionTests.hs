{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Regression tests for result metadata, cursor state, and handle lifetime.
module CoreRegressionTests (coreRegressionTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, finally, throwIO, try)
import Control.Monad (forM_, replicateM_, void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.Internal (DuckDBV2ResultHandle (..), Statement (statementStream), StatementStream (statementStreamResult), StatementStreamState (..), nullHandle)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.Mem (performMajorGC)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Exercise public operations against an in-memory database.
coreRegressionTests :: TestTree
coreRegressionTests =
    testGroup
        "core regressions"
        [ testCase "stream metadata follows parameter types" $
            withConnection ":memory:" $ \conn -> do
                rows <- fold conn "SELECT ?" (Only (42 :: Int64)) [] collect
                rows @?= [Only (42 :: Int64)]
        , testCase "cursor uses the supported materialized execution API" $
            withConnection ":memory:" $ \conn ->
                withStatement conn "SELECT i FROM range(10000) t(i)" $ \stmt -> do
                    void (nextRow stmt :: IO (Maybe (Only Int64)))
                    state <- readIORef (statementStream stmt)
                    case state of
                        StatementStreamActive stream -> do
                            assertBool "expected a released execution cursor" (statementStreamResult stream == nullHandle)
                        _ -> assertFailure "expected an active cursor"
        , testCase "stream metadata preserves 64-bit values" $
            withConnection ":memory:" $ \conn -> do
                rows <- fold conn "SELECT coalesce(?, 1) FROM range(3)" (Only (5000000000 :: Int64)) [] collect
                rows @?= replicate 3 (Only (5000000000 :: Int64))
        , testCase "stream numeric to Text conversion uses result metadata" $
            withConnection ":memory:" $ \conn -> do
                rows <- fold conn "SELECT coalesce(?, 'x')" (Only (100 :: Int64)) [] collect
                rows @?= [Only ("100" :: Text)]
        , testCase "stream metadata follows schema rebinding" $
            withConnection ":memory:" $ \conn -> do
                void $ execute_ conn "CREATE TABLE rebind_probe(x VARCHAR)"
                withStatement conn "SELECT x FROM rebind_probe" $ \stmt -> do
                    void $ execute_ conn "DROP TABLE rebind_probe"
                    void $ execute_ conn "CREATE TABLE rebind_probe(x BIGINT)"
                    void $ execute_ conn "INSERT INTO rebind_probe VALUES (5000000000)"
                    nextRow stmt >>= (@?= Just (Only (5000000000 :: Int64)))
        , testCase "EOF remains exhausted" $
            withConnection ":memory:" $ \conn ->
                withStatement conn "SELECT 42" $ \stmt -> do
                    nextRow stmt >>= (@?= Just (Only (42 :: Int64)))
                    replicateM_ 3 $ nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
        , testCase "DML cursor executes only once" $
            withConnection ":memory:" $ \conn -> do
                void $ execute_ conn "CREATE TABLE once_probe(x INTEGER)"
                withStatement conn "INSERT INTO once_probe VALUES (1)" $ \stmt ->
                    replicateM_ 3 $ void (nextRow stmt :: IO (Maybe (Only Int64)))
                query_ conn "SELECT count(*) FROM once_probe" >>= (@?= [Only (1 :: Int64)])
        , testCase "rebinding resets an exhausted cursor" $
            withConnection ":memory:" $ \conn ->
                withStatement conn "SELECT ?::BIGINT" $ \stmt -> do
                    bind stmt [toField (1 :: Int64)]
                    nextRow stmt >>= (@?= Just (Only (1 :: Int64)))
                    nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
                    bind stmt [toField (2 :: Int64)]
                    nextRow stmt >>= (@?= Just (Only (2 :: Int64)))
        , testCase "clear bindings invalidates an active cursor" $
            withConnection ":memory:" $ \conn ->
                withStatement conn "SELECT ?::BIGINT FROM range(3)" $ \stmt -> do
                    bind stmt [toField (7 :: Int64)]
                    void (nextRow stmt :: IO (Maybe (Only Int64)))
                    clearStatementBindings stmt
                    assertThrows (nextRow stmt :: IO (Maybe (Only Int64)))
        , testCase "fetch errors are not EOF" $
            withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn ->
                assertThrows (fold_ conn "SELECT CASE WHEN i = 5000 THEN error('late failure') ELSE i END FROM range(10000) t(i)" (0 :: Int64) (\acc (Only n) -> pure (acc + n)))
        , testCase "composite cursors preserve values after chunk cleanup" $
            withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn ->
                forM_
                    [ "SELECT {'n': i, 'text': 'λ' || i::VARCHAR} FROM range(5000) t(i)"
                    , "SELECT CASE WHEN i % 2 = 0 THEN union_value(n := i)::UNION(n BIGINT, text VARCHAR) ELSE union_value(text := i::VARCHAR)::UNION(n BIGINT, text VARCHAR) END FROM range(5000) t(i)"
                    , "SELECT CASE WHEN i % 3 = 0 THEN NULL ELSE {'n': i, 'list': [i, NULL], 'union': union_value(v := i)} END FROM range(5000) t(i)"
                    ]
                    $ \sql -> do
                        eager <- query_ conn sql :: IO [Only FieldValue]
                        streamed <- reverse <$> fold_ conn sql [] (\acc row -> pure (row : acc))
                        streamed @?= eager
        , testCase "composite cursor failures release the result" $
            withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn -> do
                let sql = "SELECT {'n': i, 'values': [i, NULL]} FROM range(5000) t(i)"
                assertThrows (fold_ conn sql () (\() (Only (_ :: Int64)) -> pure ()))
                assertThrows $ fold_ conn sql (0 :: Int) $ \n (Only (_ :: FieldValue)) ->
                    if n == 2050 then throwIO (userError "composite fold failure") else pure (n + 1)
                query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
        , testCase "closed connection rejects active cursor reads" $ do
            conn <- open ":memory:"
            stmt <- openStatement conn "SELECT i FROM range(3) t(i)"
            void (nextRow stmt :: IO (Maybe (Only Int64)))
            close conn
            assertThrows (nextRow stmt :: IO (Maybe (Only Int64)))
            closeStatement stmt
        , testCase "NUL in SQL is rejected" $
            withConnection ":memory:" $ \conn ->
                assertThrows (query_ conn "SELECT 1\0 + 2" :: IO [Only Int64])
        , testCase "NUL in database path is rejected" $
            assertThrows (withConnection ":memory:\0suffix" (const (pure ())))
        , testCase "NUL in configuration value is rejected" $
            assertThrows (withConnectionWithConfig ":memory:" [("threads", "1\0suffix")] (const (pure ())))
        , testCase "NUL in parameter name is rejected" $
            withConnection ":memory:" $ \conn ->
                withStatement conn "SELECT $value" $ \stmt ->
                    assertThrows (namedParameterIndex stmt "value\0suffix")
        , testCase "duplicate named bindings are rejected" $
            withConnection ":memory:" $ \conn ->
                withStatement conn "SELECT $foo + $bar" $ \stmt ->
                    forM_ ["$foo", "FOO"] $ \duplicate -> do
                        result <- try (bindNamed stmt ["foo" := (1 :: Int64), duplicate := (2 :: Int64)])
                        case result of
                            Left (_ :: FormatError) -> pure ()
                            Right () -> assertFailure "expected duplicate binding error before execution"
        , testCase "named bindings retain distinct Unicode names" $
            withConnection ":memory:" $ \conn ->
                (queryNamed conn "SELECT $Straße::BIGINT, $STRASSE::BIGINT" ["Straße" := (1 :: Int64), "STRASSE" := (2 :: Int64)] :: IO [(Int64, Int64)])
                    >>= (@?= [(1, 2)])
        , testCase "transaction preserves the original exception if rollback fails" $
            withConnection ":memory:" $ \conn -> do
                result <- try $ withTransaction conn $ do
                    void $ execute_ conn "ROLLBACK"
                    throwIO (userError "original transaction failure")
                case result of
                    Left (err :: IOError) -> assertBool "original exception" ("original transaction failure" `isInfixOf` show err)
                    Right () -> assertFailure "expected transaction failure"
        , testCase "materialization reports a late error before the first callback" $
            withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn -> do
                calls <- newIORef (0 :: Int)
                assertThrows $ fold_ conn "SELECT CASE WHEN i = 5000 THEN error('late failure') ELSE i END FROM range(10000) t(i)" () $ \() (Only (_ :: Int64)) ->
                    modifyIORef' calls (+ 1)
                readIORef calls >>= (@?= 0)
        , testCase "a materialized callback can query the same connection" $
            withConnection ":memory:" $ \conn -> do
                count <- fold_ conn "SELECT i FROM range(3) t(i)" (0 :: Int) $ \acc (Only (_ :: Int64)) -> do
                    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
                    pure (acc + 1)
                count @?= 3
        , testCase "ad hoc SQL executes statement-expanded groups" $
            withConnection ":memory:" $ \conn -> do
                void (execute_ conn "CREATE TABLE pivot_probe(category VARCHAR, yr INTEGER, amount BIGINT); INSERT INTO pivot_probe VALUES ('a',2020,1),('a',2021,2)")
                query_ conn "PIVOT pivot_probe ON yr USING sum(amount)" >>= (@?= [("a" :: Text, 1 :: Int64, 2 :: Int64)])
                void (execute_ conn "ALTER TABLE pivot_probe ADD COLUMN extra DOUBLE DEFAULT random()")
                query_ conn "SELECT count(*) FROM pivot_probe WHERE extra IS NOT NULL" >>= (@?= [Only (2 :: Int64)])
        , testCase "connections share an explicit instance" $
            withEnvironment $ \environment -> withInstance environment [] $ \instance_ -> do
                attachDatabase instance_ ":memory:" "shared" [] True
                withConnectionOn instance_ $ \first -> withConnectionOn instance_ $ \second -> do
                    void (execute_ first "CREATE TABLE shared_probe(x BIGINT); INSERT INTO shared_probe VALUES (42)")
                    query_ second "SELECT x FROM shared_probe" >>= (@?= [Only (42 :: Int64)])
        , testCase "environment close releases live cursors before their parents" $ do
            environment <- createEnvironment
            instance_ <- createInstance environment []
            attachDatabase instance_ ":memory:" "shared" [] True
            conn <- connect instance_
            stmt <- openStatement conn "SELECT i FROM range(5000) t(i)"
            nextRow stmt >>= (@?= Just (Only (0 :: Int64)))
            replicateM_ 20 (void (openStatement conn "SELECT 42"))
            performMajorGC
            closeEnvironment environment
            assertThrows (nextRow stmt :: IO (Maybe (Only Int64)))
            assertThrows (query_ conn "SELECT 42" :: IO [Only Int64])
            closeStatement stmt
            close conn
            closeInstance instance_
            closeEnvironment environment
        , testCase "GC releases an unclosed convenience connection" $ do
            directory <- getTemporaryDirectory
            (path, file) <- openBinaryTempFile directory "duckdb-v2-gc.duckdb"
            hClose file
            removeFile path
            let allocate = do
                    conn <- openWithConfig path [("threads", "1")]
                    void (execute_ conn "CREATE TABLE gc_probe AS SELECT 42::BIGINT AS x")
                wait = do
                    performMajorGC
                    reopened <- try $ withConnectionWithConfig path [("threads", "1")] $ \conn ->
                        query_ conn "SELECT x FROM gc_probe" >>= (@?= [Only (42 :: Int64)])
                    case reopened of
                        Right () -> pure ()
                        Left (_ :: SQLError) -> threadDelay 10000 >> wait
            flip finally (removeFile path) $ do
                allocate
                released <- timeout 5000000 wait
                released @?= Just ()
        , testCase "GC during the last connection use keeps native owners alive" $ do
            -- A bracket cleanup would retain conn and hide early finalization.
            conn <- open ":memory:"
            createFunction conn "gc_identity" (\(n :: Int64) -> performMajorGC >> pure n)
            rows <- query_ conn "SELECT gc_identity(i) FROM range(10) t(i)"
            rows @?= map Only ([0 .. 9] :: [Int64])
        ]
  where
    collect acc row = pure (acc <> [row])

-- | Require an exception without constraining its concrete type.
assertThrows :: IO a -> Assertion
assertThrows action = do
    result <- try (void action)
    case result of
        Left (_ :: SomeException) -> pure ()
        Right () -> assertFailure "expected an exception"
