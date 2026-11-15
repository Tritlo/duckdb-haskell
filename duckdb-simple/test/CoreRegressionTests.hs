{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : CoreRegressionTests
-- Description : Regression tests for result metadata, cursor state, and handle lifetime.
module CoreRegressionTests (coreRegressionTests) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Exception (IOException, SomeException, bracket, finally, throwIO, try)
import Control.Monad (forM_, replicateM, replicateM_, void, when)
import Data.Either (partitionEithers)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.DuckDB.FFI.V2 as Raw
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Catalog as Catalog
import qualified Database.DuckDB.Simple.Config as Config
import qualified Database.DuckDB.Simple.Copy as Copy
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.Internal (
  DuckDBV2ResultHandle (..),
  Statement (statementStream),
  StatementStream (statementStreamResult),
  StatementStreamState (..),
  checked,
  executePreparedResult,
  nullHandle,
  output,
  withCollectionStatement,
  withConnectionFactory,
  withConnectionHandle,
 )
import qualified Database.DuckDB.Simple.Internal as Internal
import qualified Database.DuckDB.Simple.Logging as Logging
import Database.DuckDB.Simple.Pragma (PragmaCallback (..), registerPragmaFunction)
import Database.DuckDB.Simple.Registration (
  Replacement (..),
  TypeDeclaration (..),
  parameter,
  registerInstanceReplacementScan,
  registerReplacementScan,
 )
import qualified Database.DuckDB.Simple.Streaming as Streaming
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (withArray)
import System.Directory (canonicalizePath, createDirectory, getTemporaryDirectory, listDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import System.Mem (performMajorGC)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestUtils (assertInputError, assertReusable, assertSqlErrorType, raiseCapabilities)

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
    , testCase "reset metadata follows an altered result schema" $
        withConnection ":memory:" $ \conn -> do
          void $ execute_ conn "CREATE TABLE metadata_rebind(x BIGINT)"
          withStatement conn "SELECT * FROM metadata_rebind" $ \stmt -> do
            columnCount stmt >>= (@?= 1)
            void $ execute_ conn "ALTER TABLE metadata_rebind ADD COLUMN y VARCHAR"
            clearStatementBindings stmt
            columnCount stmt >>= (@?= 2)
            columnName stmt 1 >>= (@?= "y")
    , testCase "EOF remains exhausted" $
        withConnection ":memory:" $ \conn ->
          withStatement conn "SELECT 42" $ \stmt -> do
            nextRow stmt >>= (@?= Just (Only (42 :: Int64)))
            replicateM_ 3 $ nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
    , testCase "DML cursor executes only once" $
        withConnection ":memory:" $ \conn -> do
          void $ execute_ conn "CREATE TABLE once_probe(x INTEGER)"
          withStatement conn "INSERT INTO once_probe VALUES (1)" $ \stmt -> do
            nextRow stmt >>= (@?= Just (Only (1 :: Int64)))
            replicateM_ 3 $ nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
          query_ conn "SELECT count(*) FROM once_probe" >>= (@?= [Only (1 :: Int64)])
    , testCase "queries and folds give the changed-row count of a DML statement" $
        withConnection ":memory:" $ \conn -> do
          void (execute_ conn "CREATE TABLE changed_rows(value BIGINT)")
          (query_ conn "INSERT INTO changed_rows VALUES (1),(2)" :: IO [Only Int64]) >>= (@?= [Only 2])
          (query conn "INSERT INTO changed_rows VALUES (?)" (Only (3 :: Int64)) :: IO [Only Int64]) >>= (@?= [Only 1])
          (queryNamed conn "UPDATE changed_rows SET value=value+10 WHERE value<=$limit" ["limit" := (2 :: Int64)] :: IO [Only Int64])
            >>= (@?= [Only 2])
          (query_ conn "DELETE FROM changed_rows WHERE false" :: IO [Only Int64]) >>= (@?= [Only 0])
          fold_ conn "INSERT INTO changed_rows VALUES (4)" [] collect >>= (@?= [Only (1 :: Int64)])
          fold conn "INSERT INTO changed_rows VALUES (?)" (Only (5 :: Int64)) [] collect >>= (@?= [Only (1 :: Int64)])
          foldNamed conn "UPDATE changed_rows SET value=value+10 WHERE value<=$limit" ["limit" := (4 :: Int64)] [] collect
            >>= (@?= [Only (2 :: Int64)])
          query_ conn "SELECT value FROM changed_rows ORDER BY value" >>= (@?= map Only [5, 11, 12, 13, 14 :: Int64])
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
            assertSqlErrorType ErrorInputInvalid (nextRow stmt :: IO (Maybe (Only Int64)))
    , testCase "fetch errors are not EOF" $
        withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn ->
          assertSqlErrorType
            ErrorInputInvalid
            ( fold_
                conn
                "SELECT CASE WHEN i = 5000 THEN error('late failure') ELSE i END FROM range(10000) t(i)"
                (0 :: Int64)
                (\acc (Only n) -> pure (acc + n))
            )
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
    , -- The case of the leak test for a statement that closes with a live cursor
      -- checks the release of the chunks.
      testCase "composite fold failures raise their error and leave the connection usable" $
        withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn -> do
          let sql = "SELECT {'n': i, 'values': [i, NULL]} FROM range(5000) t(i)"
          -- A STRUCT does not convert to Int64. The conversion error names the
          -- Haskell type and the query.
          conversion <- try (fold_ conn sql () (\() (Only (_ :: Int64)) -> pure ()))
          case conversion of
            Left err -> do
              assertBool (Text.unpack (sqlErrorMessage err)) ("Int64" `Text.isInfixOf` sqlErrorMessage err)
              sqlErrorQuery err @?= Just sql
            Right () -> assertFailure "expected the conversion error"
          stepFailure <- try $ fold_ conn sql (0 :: Int) $ \n (Only (_ :: FieldValue)) ->
            if n == 2050 then throwIO (userError "composite fold failure") else pure (n + 1)
          case stepFailure of
            Left (err :: IOException) -> show err @?= "user error (composite fold failure)"
            Right count -> assertFailure ("expected the step failure, got " <> show count)
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
          assertInputError (query_ conn "SELECT 1\0 + 2" :: IO [Only Int64])
    , testCase "NUL in database path is rejected" $
        assertInputError (withConnection ":memory:\0suffix" (const (pure ())))
    , testCase "NUL in configuration value is rejected" $
        assertInputError (withConnectionWithConfig ":memory:" [("threads", "1\0suffix")] (const (pure ())))
    , testCase "a column index out of bounds raises the input error with the query" $
        withConnection ":memory:" $ \conn ->
          withStatement conn "SELECT 1 AS a" $ \stmt ->
            forM_ [-1, 1] $ \index -> do
              assertInputError (columnName stmt index)
              result <- try (columnName stmt index)
              case result of
                Left (err :: SQLError) -> sqlErrorQuery err @?= Just "SELECT 1 AS a"
                Right name -> assertFailure ("expected the column index error, got " <> show name)
    , testCase "NUL in parameter name is rejected" $
        withConnection ":memory:" $ \conn ->
          withStatement conn "SELECT $value" $ \stmt ->
            assertInputError (namedParameterIndex stmt "value\0suffix")
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
    , testCase "numeric named bindings retain sparse and reordered parameter names" $
        withConnection ":memory:" $ \conn -> do
          (queryNamed conn "SELECT $3::BIGINT" ["3" := (33 :: Int64)] :: IO [Only Int64]) >>= (@?= [Only 33])
          (queryNamed conn "SELECT $3::BIGINT, $1::BIGINT, $3::BIGINT" ["1" := (11 :: Int64), "3" := (33 :: Int64)] :: IO [(Int64, Int64, Int64)])
            >>= (@?= [(33, 11, 33)])
          (query conn "SELECT ?::BIGINT, ?::BIGINT" ((1 :: Int64), (2 :: Int64)) :: IO [(Int64, Int64)]) >>= (@?= [(1, 2)])
    , testCase "statements bind several thousand parameters by position and by name" $
        withConnection ":memory:" $ \conn -> do
          let count = 3000 :: Int
              values = [1 .. fromIntegral count] :: [Int64]
              listSum items = Query ("SELECT list_sum([" <> Text.intercalate "," items <> "])")
              name index = "p" <> Text.pack (show index)
          (query conn (listSum (replicate count "?::BIGINT")) values :: IO [Only Int64]) >>= (@?= [Only (sum values)])
          ( queryNamed
              conn
              (listSum ["$" <> name index <> "::BIGINT" | index <- [1 .. count]])
              [name index := value | (index, value) <- zip [1 :: Int ..] values]
              :: IO [Only Int64]
            )
            >>= (@?= [Only (sum values)])
          -- The last parameter decides whether the names are the positions 1 to n.
          (query conn "SELECT $2::BIGINT, $1::BIGINT, $3::BIGINT" (1 :: Int64, 2 :: Int64, 3 :: Int64) :: IO [(Int64, Int64, Int64)])
            >>= (@?= [(2, 1, 3)])
          (queryNamed conn "SELECT $1::BIGINT, $3::BIGINT" ["1" := (11 :: Int64), "3" := (33 :: Int64)] :: IO [(Int64, Int64)]) >>= (@?= [(11, 33)])
          assertThrows (query conn "SELECT $1::BIGINT, $3::BIGINT" (11 :: Int64, 33 :: Int64) :: IO [(Int64, Int64)])
    , testCase "transaction preserves the original exception if rollback fails" $
        withConnection ":memory:" $ \conn -> do
          result <- try $ withTransaction conn $ do
            void $ execute_ conn "ROLLBACK"
            throwIO (userError "original transaction failure")
          case result of
            Left (err :: IOError) -> assertBool "original exception" ("original transaction failure" `isInfixOf` show err)
            Right () -> assertFailure "expected transaction failure"
    , testCase "a callback exception of each kind fails its statement in a transaction and leaves no rows" $
        mapM_ runFailingCallback failingCallbacks
    , testCase "materialization reports a late error before the first callback" $
        withConnectionWithConfig ":memory:" [("threads", "1")] $ \conn -> do
          calls <- newIORef (0 :: Int)
          assertSqlErrorType ErrorInputInvalid $ fold_ conn "SELECT CASE WHEN i = 5000 THEN error('late failure') ELSE i END FROM range(10000) t(i)" () $ \() (Only (_ :: Int64)) ->
            modifyIORef' calls (+ 1)
          readIORef calls >>= (@?= 0)
    , testCase "a materialized callback can query the same connection" $
        withConnection ":memory:" $ \conn -> do
          count <- fold_ conn "SELECT i FROM range(3) t(i)" (0 :: Int) $ \acc (Only (_ :: Int64)) -> do
            query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
            pure (acc + 1)
          count @?= 3
    , testCase "threads share a connection for default queries" $
        do
          -- See Note [Capabilities of the tests] in "TestUtils".
          raiseCapabilities 4
          withConnection ":memory:" $ \conn -> do
            void (execute_ conn "CREATE TABLE shared_rows(worker BIGINT, i BIGINT)")
            start <- newEmptyMVar
            completed <- replicateM 8 newEmptyMVar
            forM_ (zip [1 :: Int64 ..] completed) $ \(worker, result) ->
              void $ flip forkFinally (putMVar result) $ do
                readMVar start
                forM_ [1 .. 50 :: Int64] $ \i -> do
                  query conn "SELECT ? + ?" (worker, i) >>= (@?= [Only (worker + i)])
                  execute conn "INSERT INTO shared_rows VALUES (?, ?)" (worker, i) >>= (@?= 1)
                  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
                  -- The threads share the VARIANT decoder state of the connection.
                  query conn "SELECT ?::VARIANT, [?, ?]::VARIANT" (worker + i, worker, i) >>= (@?= [(worker + i, [worker, i])])
                  fold_ conn "SELECT i FROM range(3) t(i)" (0 :: Int64) (\acc (Only n) -> pure (acc + n)) >>= (@?= 3)
                  execute_ conn "UPDATE shared_rows SET i = i WHERE worker = 0" >>= (@?= 0)
            putMVar start ()
            outcomes <- mapM takeMVar completed
            case partitionEithers outcomes of
              ([], _) -> pure ()
              (failure : _, _) -> throwIO failure
            query_ conn "SELECT count(*) FROM shared_rows" >>= (@?= [Only (400 :: Int64)])
    , testCase "ad hoc SQL executes statement-expanded groups" $
        withConnection ":memory:" $ \conn -> do
          void
            ( execute_
                conn
                "CREATE TABLE pivot_probe(category VARCHAR, yr INTEGER, amount BIGINT); INSERT INTO pivot_probe VALUES ('a',2020,1),('a',2021,2)"
            )
          query_ conn "PIVOT pivot_probe ON yr USING sum(amount)" >>= (@?= [("a" :: Text, 1 :: Int64, 2 :: Int64)])
          void (execute_ conn "ALTER TABLE pivot_probe ADD COLUMN extra DOUBLE DEFAULT random()")
          query_ conn "SELECT count(*) FROM pivot_probe WHERE extra IS NOT NULL" >>= (@?= [Only (2 :: Int64)])
    , testCase "a prepared PIVOT refreshes its result schema on each execution" $
        withConnection ":memory:" $ \conn -> do
          void
            ( execute_
                conn
                "CREATE TABLE prepared_pivot(category VARCHAR, yr INTEGER, amount BIGINT); INSERT INTO prepared_pivot VALUES ('a',2020,1),('a',2021,2)"
            )
          withStatement conn "PIVOT prepared_pivot ON yr USING sum(amount)" $ \stmt -> do
            nextRow stmt >>= (@?= Just ("a" :: Text, 1 :: Int64, 2 :: Int64))
            nextRow stmt >>= (@?= (Nothing :: Maybe (Text, Int64, Int64)))
            void (execute_ conn "INSERT INTO prepared_pivot VALUES ('a',2022,3)")
            clearStatementBindings stmt
            nextRow stmt >>= (@?= Just ("a" :: Text, 1 :: Int64, 2 :: Int64, 3 :: Int64))
            nextRow stmt >>= (@?= (Nothing :: Maybe (Text, Int64, Int64, Int64)))
    , testCase "a scoped collection statement retains named input and parameter metadata" $
        withConnection ":memory:" $ \conn ->
          withConnectionHandle conn $ \native -> withConnectionFactory native $ \factory ->
            withStatement conn "SELECT 42::BIGINT" $ \source ->
              bracket (executePreparedResult source) (Internal.destroy Raw.c_duckdb_v2_result_destroy) $ \result ->
                bracket (output (Raw.c_duckdb_v2_result_fetch_chunk result)) (Internal.destroy Raw.c_duckdb_v2_data_chunk_destroy) $ \chunk -> do
                  bracket (output (Raw.c_duckdb_v2_result_fetch_chunk result)) (Internal.destroy Raw.c_duckdb_v2_data_chunk_destroy) $ \end ->
                    end @?= nullHandle
                  vector <- output (Raw.c_duckdb_v2_data_chunk_get_vector chunk 0)
                  bracket (output (Raw.c_duckdb_v2_vector_get_logical_type vector)) (Internal.destroy Raw.c_duckdb_v2_logical_type_destroy) $ \logical ->
                    withArray [logical] $ \types ->
                      bracket
                        (output (Raw.c_duckdb_v2_column_data_collection_create factory (ConstPtr types) 1))
                        (Internal.destroy Raw.c_duckdb_v2_column_data_collection_destroy)
                        $ \collection -> do
                          let checkRows :: Maybe (Only Int64) -> IO ()
                              checkRows expected = withCollectionStatement conn "SELECT payload + ?::BIGINT FROM input_rows" [("input_rows", collection, ["payload"])] $ \stmt -> do
                                bind stmt [toField (1 :: Int64)]
                                nextRow stmt >>= (@?= expected)
                                nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
                          checkRows Nothing
                          bracket
                            (output (Raw.c_duckdb_v2_column_data_collection_append_state_create collection))
                            (Internal.destroy Raw.c_duckdb_v2_column_data_collection_append_state_destroy)
                            $ \appendState ->
                              checked (Raw.c_duckdb_v2_column_data_collection_append collection appendState chunk)
                          checkRows (Just (Only (43 :: Int64)))
    , testCase "connections share an explicit instance" $
        withEnvironment $ \environment -> withInstance environment [] $ \instance_ -> do
          attachDatabase instance_ ":memory:" "shared" []
          setDefaultDatabase instance_ "shared"
          withConnectionOn instance_ $ \first -> withConnectionOn instance_ $ \second -> do
            void (execute_ first "CREATE TABLE shared_probe(x BIGINT); INSERT INTO shared_probe VALUES (42)")
            query_ second "SELECT x FROM shared_probe" >>= (@?= [Only (42 :: Int64)])
    , testCase "a detached database is not reachable" $
        withEnvironment $ \environment -> withInstance environment [] $ \instance_ -> do
          attachDatabase instance_ ":memory:" "shared" []
          setDefaultDatabase instance_ "shared"
          attachDatabase instance_ ":memory:" "side" []
          withConnectionOn instance_ $ \conn -> do
            void (execute_ conn "CREATE TABLE side.probe(x BIGINT); INSERT INTO side.probe VALUES (7)")
            query_ conn "SELECT x FROM side.probe" >>= (@?= [Only (7 :: Int64)])
          detachDatabase instance_ "side"
          withConnectionOn instance_ $ \conn -> do
            query_ conn "SELECT count(*) FROM duckdb_databases() WHERE database_name = 'side'" >>= (@?= [Only (0 :: Int64)])
            assertSqlErrorType ErrorDatabaseCatalog (query_ conn "SELECT x FROM side.probe" :: IO [Only Int64])
            query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
          -- The native detach call gives this type. The SQL statement DETACH gives
          -- a Binder Error.
          assertSqlErrorType ErrorInputInvalid (detachDatabase instance_ "side")
    , testCase "NUL in attach options and aliases is rejected" $
        withEnvironment $ \environment -> withInstance environment [] $ \instance_ -> do
          attachDatabase instance_ ":memory:" "shared" []
          setDefaultDatabase instance_ "shared"
          forM_
            [ attachDatabase instance_ ":memory:" "side" [("read_only\0suffix", "false")]
            , attachDatabase instance_ ":memory:" "side" [("read_only", "false\0suffix")]
            , detachDatabase instance_ "shared\0suffix"
            , setDefaultDatabase instance_ "shared\0suffix"
            ]
            $ \action -> do
              result <- try action
              case result of
                Left (err :: SQLError) -> do
                  sqlErrorType err @?= Just ErrorInputInvalid
                  assertBool "NUL diagnostic" ("contains NUL" `Text.isInfixOf` sqlErrorMessage err)
                Right () -> assertFailure "expected a NUL error"
          withConnectionOn instance_ $ \conn ->
            query_ conn "SELECT current_database(), count(*) FILTER (WHERE database_name = 'side') FROM duckdb_databases()"
              >>= (@?= [("shared" :: Text, 0 :: Int64)])
    , testCase "parent close releases concurrently created children" $
        do
          -- See Note [Capabilities of the tests] in "TestUtils".
          raiseCapabilities 2
          withEnvironment $ \environment -> withInstance environment [("threads", "1"), ("async_threads", "1")] $ \instance_ ->
            withConcurrentChildren 64 (connect instance_) close $ \connections -> do
              closeInstance instance_
              forM_ connections $ \connection -> do
                state <- readIORef (Internal.connectionState connection)
                case state of
                  Internal.ConnectionClosed -> pure ()
                  Internal.ConnectionOpen{} -> assertFailure "parent close missed a connection"
                assertThrows (query_ connection "SELECT 42" :: IO [Only Int64])
          withEnvironment $ \environment ->
            withConcurrentChildren 16 (createInstance environment [("threads", "1"), ("async_threads", "1")]) closeInstance $ \instances -> do
              closeEnvironment environment
              forM_ instances $ \instance_ -> do
                state <- readIORef (Internal.instanceState instance_)
                case state of
                  Internal.InstanceClosed -> pure ()
                  Internal.InstanceOpen{} -> assertFailure "parent close missed an instance"
                assertThrows (connect instance_)
    , testCase "startup configuration sets the whitelist before disabling external access" $ do
        path <- canonicalizePath "/tmp/duckdb-whitelist-probe"
        withConnectionWithConfig ":memory:" [("enable_external_access", "false"), ("allowed_paths", "['/tmp/duckdb-whitelist-probe']")] $ \conn ->
          query conn "SELECT current_setting('enable_external_access'), list_contains(current_setting('allowed_paths'), ?)" (Only path)
            >>= (@?= [(False, True)])
    , testCase "environment close ends the live cursors of its connections" $ do
        environment <- createEnvironment
        instance_ <- createInstance environment []
        attachDatabase instance_ ":memory:" "shared" []
        setDefaultDatabase instance_ "shared"
        conn <- connect instance_
        stmt <- openStatement conn "SELECT i FROM range(5000) t(i)"
        nextRow stmt >>= (@?= Just (Only (0 :: Int64)))
        replicateM_ 20 (void (openStatement conn "SELECT 42"))
        performMajorGC
        closeEnvironment environment
        cursor <- readIORef (statementStream stmt)
        case cursor of
          StatementStreamActive _ -> assertFailure "the environment close left the cursor of the statement active"
          _ -> pure ()
        assertThrows (nextRow stmt :: IO (Maybe (Only Int64)))
        assertThrows (query_ conn "SELECT 42" :: IO [Only Int64])
        closeStatement stmt
        close conn
        closeInstance instance_
        closeEnvironment environment
    , testCase "a refused environment close keeps the environment open" $ do
        environment <- createEnvironment
        state <- readIORef (Internal.environmentState environment)
        raw <- case state of
          Internal.EnvironmentOpen raw _ -> pure raw
          Internal.EnvironmentClosed -> assertFailure "a new environment is closed"
        -- The library does not track this instance, so it does not close it.
        untracked <- output (Raw.c_duckdb_v2_instance_create raw)
        refused <- try (closeEnvironment environment)
        case refused of
          Left (err :: SQLError) -> sqlErrorType err @?= Just ErrorResourceInUse
          Right () -> assertFailure "expected a refused close"
        -- Keep this instance open. The second close must find it in the child registry.
        instance_ <- createInstance environment []
        withConnectionOn instance_ $ \conn -> query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
        Internal.destroy Raw.c_duckdb_v2_instance_destroy untracked
        closeEnvironment environment
        assertThrows (connect instance_)
        closed <- try (createInstance environment [])
        case closed of
          Left (err :: SQLError) -> sqlErrorMessage err @?= "duckdb-simple: environment is closed"
          Right _ -> assertFailure "expected a closed environment"
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
          released <- timeout 60000000 wait
          released @?= Just ()
    , testCase "GC during the last connection use keeps native owners alive" $ do
        -- A bracket cleanup would retain conn and hide early finalization.
        conn <- open ":memory:"
        createFunction conn "gc_identity" (\(n :: Int64) -> performMajorGC >> pure n)
        rows <- query_ conn "SELECT gc_identity(i) FROM range(10) t(i)"
        rows @?= map Only ([0 .. 9] :: [Int64])
    , testCase "a prepare of no statement or of two statements raises its error" $ withConnection ":memory:" $ \conn ->
        forM_
          [ ("", "duckdb-simple: SQL query contains no statement")
          , ("-- a comment only", "duckdb-simple: SQL query contains no statement")
          , ("SELECT 1; SELECT 2", "duckdb-simple: prepared SQL contains more than one statement")
          ]
          $ \(sql, message) -> do
            outcome <- try (openStatement conn (Query sql))
            case outcome of
              Left err -> do
                sqlErrorMessage err @?= message
                sqlErrorQuery err @?= Just (Query sql)
              Right stmt -> closeStatement stmt >> assertFailure ("expected an error for " <> show sql)
    , testCase "named and indexed statement access raises the errors of the library" $
        withConnection ":memory:" $ \conn -> withStatement conn "SELECT $value AS v" $ \stmt -> do
          nulName <- try (bindNamed stmt ["value\0x" := (1 :: Int64)])
          case nulName of
            Left err -> do
              sqlErrorMessage err @?= "duckdb-simple: parameter name contains NUL"
              sqlErrorQuery err @?= Just "SELECT $value AS v"
            Right () -> assertFailure "expected the NUL error"
          tooMany <- try (bindNamed stmt ["value" := (1 :: Int64), "other" := (2 :: Int64)])
          case tooMany of
            Left err -> formatErrorMessage err @?= "duckdb-simple: SQL query contains 1 named parameter(s), but 2 argument(s) were supplied"
            Right () -> assertFailure "expected the count error"
          negativeIndex <- try (columnName stmt (-1))
          case negativeIndex of
            Left err -> sqlErrorMessage err @?= "duckdb-simple: column index -1 out of bounds"
            Right name -> assertFailure ("expected the index error, got " <> show name)
          -- The statement still works after the errors.
          bindNamed stmt ["value" := (7 :: Int64)]
          nextRow stmt >>= (@?= Just (Only (7 :: Int64)))
    , testCase "connection options with NUL raise an error before the connection opens" $
        withEnvironment $ \environment -> withInstance environment [] $ \instance_ -> do
          attachDatabase instance_ ":memory:" "shared" []
          setDefaultDatabase instance_ "shared"
          outcome <- try (connectWithOptions instance_ defaultConnectionOptions{connectionConfig = [("threads\0x", "1")]})
          case outcome of
            Left err -> sqlErrorMessage err @?= "duckdb-simple: configuration contains NUL"
            Right conn -> close conn >> assertFailure "expected the NUL error"
    , testCase "statements without a query result give no row through each read function" $ withConnection ":memory:" $ \conn -> do
        let prepend rows (Only (value :: Int64)) = pure (value : rows)
        (query_ conn "CREATE TABLE no_rows_a (x BIGINT)" :: IO [Only Int64]) >>= (@?= [])
        fold_ conn "CREATE TABLE no_rows_b (x BIGINT)" [] prepend >>= (@?= [])
        withStatement conn "CREATE TABLE no_rows_c (x BIGINT)" nextRow >>= (@?= (Nothing :: Maybe (Only Int64)))
        Streaming.fold_ conn "CREATE TABLE no_rows_d (x BIGINT)" [] prepend >>= (@?= [])
        withStatement conn "CREATE TABLE no_rows_e (x BIGINT)" Streaming.nextRow >>= (@?= (Nothing :: Maybe (Only Int64)))
        -- DuckDB ran each statement one time.
        query_ conn "SELECT table_name FROM duckdb_tables() WHERE table_name LIKE 'no_rows_%' ORDER BY table_name"
          >>= (@?= map Only ["no_rows_a", "no_rows_b", "no_rows_c", "no_rows_d", "no_rows_e" :: Text])
    , testCase "a rejected runtime option raises the error of DuckDB and leaves the environment closable" $ do
        environment <- createEnvironment
        outcome <- try (createInstance environment [("logging_level", "no_such_level")])
        case outcome of
          Left err ->
            assertBool
              (Text.unpack (sqlErrorMessage err))
              ("unrecognized value \"no_such_level\" for enum \"LogLevel\"" `Text.isInfixOf` sqlErrorMessage err)
          Right instance_ -> closeInstance instance_ >> assertFailure "expected the error of DuckDB for logging_level"
        -- No instance remains, so DuckDB destroys the environment.
        closeEnvironment environment
    , testCase "a description of a missing table raises the catalog error of DuckDB" $ withConnection ":memory:" $ \conn -> do
        outcome <- try (Catalog.describeTable conn "no_such_table")
        case outcome of
          Left err -> assertBool (Text.unpack (sqlErrorMessage err)) ("Table with name no_such_table does not exist" `Text.isInfixOf` sqlErrorMessage err)
          Right description -> assertFailure ("expected the catalog error, got " <> show (Catalog.tableDescriptionName description))
        query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    , testCase "an attach in a missing directory raises the IO error of DuckDB" $
        withEnvironment $ \environment -> withInstance environment [] $ \instance_ -> do
          outcome <- try (attachDatabase instance_ "/nonexistent-duckdb-simple/attached.db" "missing" [])
          case outcome of
            Left err ->
              assertBool
                (Text.unpack (sqlErrorMessage err))
                ("Cannot open file \"/nonexistent-duckdb-simple/attached.db\": No such file or directory" `Text.isInfixOf` sqlErrorMessage err)
            Right () -> assertFailure "expected the attach error"
          -- The instance attaches another database after the failure.
          attachDatabase instance_ ":memory:" "present" []
          setDefaultDatabase instance_ "present"
          withConnectionOn instance_ $ \conn -> query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    ]
  where
    collect acc row = pure (acc <> [row])

-- | Join all creation workers and release every acquired child after the action.
withConcurrentChildren :: Int -> IO a -> (a -> IO ()) -> ([a] -> IO ()) -> IO ()
withConcurrentChildren count acquire release action =
  bracket createChildren (mapM_ release) action
  where
    createChildren = do
      start <- newEmptyMVar
      completed <- replicateM count newEmptyMVar
      forM_ completed $ \result ->
        void $ forkFinally (readMVar start >> acquire) (putMVar result)
      putMVar start ()
      outcomes <- mapM takeMVar completed
      case partitionEithers outcomes of
        ([], children) -> pure children
        (failure : _, children) -> mapM_ release children >> throwIO failure

-- | Require an exception without constraining its concrete type.
assertThrows :: IO a -> Assertion
assertThrows action = do
  result <- try (void action)
  case result of
    Left (_ :: SomeException) -> pure ()
    Right () -> assertFailure "expected an exception"

-- | A callback of one kind and phase that raises an exception while DuckDB runs
-- a statement over more than one chunk, and that statement.
data FailingCallback = FailingCallback
  { failingKind :: String
  , failingLabel :: String
  , failingThreads :: Int
  , failingSetup :: FailingContext -> IO ()
  , failingStatement :: FailingContext -> IO ()
  , failingText :: Maybe Text.Text
  -- ^ A text of the error of the statement, or 'Nothing' for a statement that
  --   does not fail, because DuckDB ignores the error of a log sink. The
  --   callback of such a row must count its calls in 'contextCalls'.
  }

-- | The connections and the directory of one row of 'failingCallbacks'.
data FailingContext = FailingContext
  { contextConnection :: Connection
  , contextOther :: Connection
  , contextInstance :: Instance
  , contextDirectory :: FilePath
  , contextCalls :: IORef Int
  }

-- | Run a statement on the connection of the row.
runIn :: FailingContext -> Query -> IO ()
runIn context sql = void (execute_ (contextConnection context) sql)

-- | Raise the exception of a row.
failWith :: String -> IO a
failWith text = throwIO (userError text)

-- | A sum whose step, combine, or result raises an exception.
failingSum :: (Int64 -> Bool) -> Bool -> Bool -> String -> Aggregate Int64 (Only Int64) Int64
failingSum failStep failCombine failResult text =
  Aggregate
    { aggregateInitial = 0
    , aggregateStep = \total (Only value) -> if failStep value then failWith text else pure (total + value)
    , aggregateCombine = \a b -> if failCombine then failWith text else pure (a + b)
    , aggregateResult = \total -> if failResult then failWith text else pure total
    }

-- | A table function of the pairs (n, f) for n from 0 to the count, whose column
-- f raises an exception at one row.
failingPairs :: Connection -> Text.Text -> Int64 -> String -> IO ()
failingPairs conn name row text =
  createTableFunction conn name ["n", "f"] \(Only count) ->
    pure [(n, if n == row then error text else n) | n <- [0 .. count - 1 :: Int64]]

-- | Each callback kind and phase in which a function of the user can fail. The
-- statements run over 5000 rows: two full chunks of 2048 rows come before row
-- 3000. Each kind runs one time more at 4 threads.
failingCallbacks :: [FailingCallback]
failingCallbacks = rows <> [row{failingThreads = 4} | row <- firstOfEachKind rows, failingThreads row == 1]
  where
    firstOfEachKind = foldr (\row kept -> row : filter ((/= failingKind row) . failingKind) kept) []
    insertRows sql context = runIn context ("INSERT INTO failing_rows " <> sql)
    countCall context = atomicModifyIORef' (contextCalls context) \count -> (count + 1, ())
    rows =
      [ FailingCallback
          "scalar"
          "scalar function with an IO result"
          1
          ( \context ->
              createFunction (contextConnection context) "failing_io" \(x :: Int64) ->
                (when (x == 3000) (failWith "failing scalar io") >> pure x :: IO Int64)
          )
          (insertRows "SELECT failing_io(i) FROM range(5000) t(i)")
          (Just "failing scalar io")
      , FailingCallback
          "scalar"
          "scalar function with a pure result"
          1
          (\context -> createFunction (contextConnection context) "failing_pure" \(x :: Int64) -> if x == 3000 then error "failing scalar pure" else x)
          (insertRows "SELECT failing_pure(i) FROM range(5000) t(i)")
          (Just "failing scalar pure")
      , FailingCallback
          "stateful scalar"
          "stateful scalar function call"
          1
          ( \context ->
              createFunctionWithState (contextConnection context) "failing_call" (pure ()) \() (x :: Int64) ->
                (when (x == 3000) (failWith "failing stateful call") >> pure x :: IO Int64)
          )
          (insertRows "SELECT failing_call(i) FROM range(5000) t(i)")
          (Just "failing stateful call")
      , FailingCallback
          "stateful scalar"
          "stateful scalar function init"
          1
          (\context -> createFunctionWithState (contextConnection context) "failing_init" (failWith "failing stateful init") \() (x :: Int64) -> x)
          (insertRows "SELECT failing_init(i) FROM range(5000) t(i)")
          (Just "failing stateful init")
      , FailingCallback
          "aggregate"
          "aggregate update with DISTINCT"
          1
          (\context -> createAggregate (contextConnection context) "failing_distinct" (failingSum (== 3000) False False "failing distinct update"))
          (insertRows "SELECT failing_distinct(DISTINCT i) FROM range(5000) t(i)")
          (Just "failing distinct update")
      , FailingCallback
          "aggregate"
          "aggregate update with FILTER"
          1
          (\context -> createAggregate (contextConnection context) "failing_filter" (failingSum (== 3000) False False "failing filter update"))
          (insertRows "SELECT failing_filter(i) FILTER (WHERE i >= 0) FROM range(5000) t(i)")
          (Just "failing filter update")
      , FailingCallback
          "aggregate"
          "aggregate update as a WINDOW aggregate"
          1
          (\context -> createAggregate (contextConnection context) "failing_window" (failingSum (== 3000) False False "failing window update"))
          (insertRows "SELECT failing_window(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) FROM range(5000) t(i)")
          (Just "failing window update")
      , FailingCallback
          "aggregate"
          "aggregate combine of a plain aggregate"
          1
          (\context -> createAggregate (contextConnection context) "failing_combine" (failingSum (const False) True False "failing plain combine"))
          (insertRows "SELECT failing_combine(i) FROM range(5000) t(i)")
          (Just "failing plain combine")
      , -- A grouped aggregate combines only the partial states of several
        -- threads, and DuckDB scans one row group of 122880 rows with one
        -- thread. Thus this row needs two row groups and 4 threads.
        FailingCallback
          "aggregate"
          "aggregate combine of a grouped aggregate"
          4
          ( \context -> do
              runIn context "CREATE TABLE failing_groups AS SELECT i FROM range(130000) t(i)"
              createAggregate (contextConnection context) "failing_grouped" (failingSum (const False) True False "failing grouped combine")
          )
          (insertRows "SELECT failing_grouped(i) FROM failing_groups GROUP BY i % 1000")
          (Just "failing grouped combine")
      , FailingCallback
          "aggregate"
          "aggregate finalize as a WINDOW aggregate"
          1
          (\context -> createAggregate (contextConnection context) "failing_result" (failingSum (const False) False True "failing window finalize"))
          (insertRows "SELECT failing_result(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) FROM range(5000) t(i)")
          (Just "failing window finalize")
      , FailingCallback
          "table function"
          "table function scan after rows, failing column only"
          1
          (\context -> failingPairs (contextConnection context) "failing_later" 3000 "failing scan at row 3000")
          (insertRows "SELECT f FROM failing_later(5000)")
          (Just "failing scan at row 3000")
      , FailingCallback
          "table function"
          "table function scan after rows, all columns"
          1
          (\context -> failingPairs (contextConnection context) "failing_later" 3000 "failing scan at row 3000")
          (\context -> runIn context "INSERT INTO failing_pairs SELECT * FROM failing_later(5000)")
          (Just "failing scan at row 3000")
      , FailingCallback
          "table function"
          "table function scan at row 0, failing column only"
          1
          (\context -> failingPairs (contextConnection context) "failing_first" 0 "failing scan at row 0")
          (insertRows "SELECT f FROM failing_first(5000)")
          (Just "failing scan at row 0")
      , FailingCallback
          "table function"
          "table function scan at row 0, all columns"
          1
          (\context -> failingPairs (contextConnection context) "failing_first" 0 "failing scan at row 0")
          (\context -> runIn context "INSERT INTO failing_pairs SELECT * FROM failing_first(5000)")
          (Just "failing scan at row 0")
      , FailingCallback
          "table function"
          "table function init"
          1
          ( \context ->
              createTableFunction (contextConnection context) "failing_start" ["v"] \(Only (_ :: Int64)) ->
                failWith "failing table init" :: IO [Only Int64]
          )
          (insertRows "SELECT v FROM failing_start(5000)")
          (Just "failing table init")
      , -- The bind callback runs no function of the user. A NULL for a
        -- parameter that cannot hold NULL fails there.
        FailingCallback
          "table function"
          "table function bind"
          1
          (\context -> createTableFunction (contextConnection context) "failing_bind" ["v"] \(Only (count :: Int64)) -> pure [Only count])
          (insertRows "SELECT v FROM failing_bind(NULL::BIGINT)")
          (Just "failing_bind: argument arg0 is NULL, but Int64 cannot hold NULL")
      , FailingCallback
          "table function"
          "table function init with a failing query on another connection"
          1
          ( \context ->
              createTableFunction (contextConnection context) "failing_inner" ["v"] \(Only (_ :: Int64)) ->
                query_ (contextOther context) "SELECT 'abc'::INTEGER" :: IO [Only Int64]
          )
          (insertRows "SELECT v FROM failing_inner(5000)")
          (Just "Conversion Error: Could not convert string 'abc' to INT32")
      , FailingCallback
          "pragma"
          "pragma in the call form"
          1
          ( \context ->
              registerPragmaFunction
                (contextConnection context)
                "failing_pragma"
                [parameter "amount" (TypeExpression "BIGINT")]
                (ExecPragma \(Only (_ :: Int64)) -> failWith "failing pragma call")
          )
          (\context -> runIn context "PRAGMA failing_pragma(1)")
          (Just "failing pragma call")
      , FailingCallback
          "custom option"
          "custom option update"
          1
          ( \context ->
              Config.registerCustomOption
                (contextConnection context)
                (Config.customOption "failing_option" (TypeExpression "BIGINT")){Config.customOptionUpdate = Just \_ -> failWith "failing option update"}
          )
          (\context -> runIn context "SET failing_option = 1")
          (Just "failing option update")
      , FailingCallback
          "replacement scan"
          "replacement scan on a connection"
          1
          ( \context -> registerReplacementScan (contextConnection context) \parts ->
              if parts == ["failing_missing"] then failWith "failing replacement scan" else pure Nothing
          )
          (insertRows "SELECT * FROM failing_missing")
          (Just "failing replacement scan")
      , FailingCallback
          "replacement scan"
          "replacement scan on an instance"
          1
          ( \context -> registerInstanceReplacementScan (contextInstance context) \parts ->
              if parts == ["failing_missing"] then failWith "failing instance replacement scan" else pure (Nothing :: Maybe Replacement)
          )
          (insertRows "SELECT * FROM failing_missing")
          (Just "failing instance replacement scan")
      , FailingCallback
          "COPY TO"
          "COPY TO sink after rows"
          1
          ( \context -> do
              written <- newIORef (0 :: Int)
              Copy.registerCopyToFunction
                (contextConnection context)
                "failing_copy"
                (\_ -> pure ())
                (\info -> writeFile (Copy.copyInitFilePath info) "" >> pure (Copy.copyInitFilePath info))
                ( \info chunk -> do
                    appendFile (Copy.copySinkGlobalState info) (concatMap (const "row\n") chunk)
                    total <- atomicModifyIORef' written \count -> (count + length chunk, count + length chunk)
                    when (total > 3000) (failWith "failing COPY TO sink")
                )
                (\_ -> pure ())
          )
          ( \context ->
              void
                ( execute
                    (contextConnection context)
                    "COPY (SELECT i FROM range(5000) t(i)) TO ? (FORMAT failing_copy)"
                    (Only (Text.pack (contextDirectory context </> "out.txt")))
                )
          )
          (Just "failing COPY TO sink")
      , FailingCallback
          "COPY FROM"
          "COPY FROM after rows"
          1
          ( \context -> do
              writeFile (contextDirectory context </> "in.txt") ""
              Copy.registerCopyFromFunction (contextConnection context) "failing_from" \_ ->
                pure [Only (if i == 3000 then error "failing COPY FROM row" else i) | i <- [0 .. 4999 :: Int64]]
          )
          ( \context ->
              void
                ( execute
                    (contextConnection context)
                    "COPY failing_rows FROM ? (FORMAT failing_from)"
                    (Only (Text.pack (contextDirectory context </> "in.txt")))
                )
          )
          (Just "failing COPY FROM row")
      , FailingCallback
          "multi-file function"
          "multi-file function at its second file"
          1
          ( \context -> do
              forM_ [1 :: Int .. 3] \file -> writeFile (contextDirectory context </> ("part" <> show file <> ".txt")) ""
              createTableFunction (contextConnection context) "failing_single" ["v"] \(Only (path :: Text.Text)) -> do
                when ("part2.txt" `Text.isSuffixOf` path) (failWith "failing second file")
                pure [Only i | i <- [0 .. 2999 :: Int64]]
              Copy.registerMultiFileFunction (contextConnection context) "failing_files" "failing_single" Copy.defaultMultiFileOptions
          )
          ( \context ->
              void
                ( execute
                    (contextConnection context)
                    "INSERT INTO failing_rows SELECT v FROM failing_files(?)"
                    (Only (Text.pack (contextDirectory context </> "part*.txt")))
                )
          )
          (Just "failing second file")
      , FailingCallback
          "log sink"
          "log sink write"
          1
          ( \context -> Logging.registerLogSink (contextConnection context) "failing_log" (\_ -> countCall context >> failWith "failing log write") (pure ())
          )
          ( \context -> runIn context "CALL enable_logging(); SET logging_storage = 'failing_log'; SELECT write_log('failing'); SET logging_storage = 'memory'"
          )
          Nothing
      , FailingCallback
          "log sink"
          "log sink flush"
          1
          ( \context -> Logging.registerLogSink (contextConnection context) "failing_log" (\_ -> pure ()) (countCall context >> failWith "failing log flush")
          )
          ( \context -> runIn context "CALL enable_logging(); SET logging_storage = 'failing_log'; SELECT write_log('failing'); SET logging_storage = 'memory'"
          )
          Nothing
      ]

-- | Run one row of 'failingCallbacks' in its own instance and directory, inside
-- an explicit transaction that has written a marker row. Require the error type
-- and the text of the callback, then COMMIT, then a usable connection and a
-- usable new connection of the instance, no row of the statement, no marker
-- row, and no new file. A log sink row requires no error and a call of the
-- sink, and keeps the marker. A new connection runs a query before the COMMIT
-- too. The same connection runs its query only after the COMMIT, because
-- DuckDB refuses each other statement in a transaction after a failed
-- statement.
runFailingCallback :: FailingCallback -> Assertion
runFailingCallback row = bracket createRowDirectory removePathForcibly \directory ->
  withEnvironment \environment -> withInstance environment [("threads", Text.pack (show (failingThreads row)))] \instance' ->
    withConnection ":memory:" \other -> do
      attachDatabase instance' ":memory:" "failing" []
      setDefaultDatabase instance' "failing"
      withConnectionOn instance' \conn -> do
        calls <- newIORef 0
        let context = FailingContext conn other instance' directory calls
            label = failingLabel row <> " at " <> show (failingThreads row) <> " threads"
        mapM_
          (void . execute_ conn)
          ["CREATE TABLE failing_rows (v BIGINT)", "CREATE TABLE failing_pairs (n BIGINT, f BIGINT)", "CREATE TABLE failing_marker (m INTEGER)"]
        failingSetup row context
        before <- listDirectory directory
        void (execute_ conn "BEGIN")
        void (execute_ conn "INSERT INTO failing_marker VALUES (1)")
        outcome <- try (failingStatement row context)
        case (failingText row, outcome) of
          (Just text, Left err) -> do
            assertEqual label (Just ErrorInputInvalid) (sqlErrorType err)
            assertBool (label <> ": " <> show (sqlErrorMessage err)) (text `Text.isInfixOf` sqlErrorMessage err)
          (Just _, Right ()) -> assertFailure (label <> ": the statement did not fail")
          (Nothing, Left err) -> assertFailure (label <> ": " <> show err)
          (Nothing, Right ()) -> readIORef calls >>= assertBool (label <> ": the callback ran") . (> 0)
        withConnectionOn instance' assertReusable
        void (execute_ conn "COMMIT")
        assertReusable conn
        withConnectionOn instance' assertReusable
        let marker = maybe 1 (const 0) (failingText row) :: Int64
        query_ conn "SELECT (SELECT count(*) FROM failing_rows), (SELECT count(*) FROM failing_pairs), (SELECT count(*) FROM failing_marker)"
          >>= assertEqual (label <> ": rows after COMMIT") [(0 :: Int64, 0 :: Int64, marker)]
        listDirectory directory >>= assertEqual (label <> ": files after the statement") before
  where
    createRowDirectory = do
      root <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile root "duckdb-failing-callback"
      hClose handle
      removeFile path
      createDirectory path
      pure path
