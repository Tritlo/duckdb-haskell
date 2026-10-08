{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Check the explicit native streaming API and its cursor ownership.
module StreamingTests (nativeStreamingTests) where

import Control.Exception (IOException, throwIO, try)
import Control.Monad (forM_, replicateM_, void, when)
import Data.IORef (atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.FFI (duckdb_result_is_streaming, duckdb_vector_size)
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Database.DuckDB.Simple.FromField (FieldValue)
import Database.DuckDB.Simple.Internal (Statement (statementStream), StatementStream (statementStreamResult), StatementStreamState (..))
import Foreign.Storable (peek)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Exercise native streaming, resets, metadata, and cleanup across chunks.
nativeStreamingTests :: TestTree
nativeStreamingTests =
    testGroup
        "explicit native streaming"
        [ testCase "nextRow starts a native streaming result" $ withDb \conn ->
            withStatement conn "SELECT i FROM range(10000) t(i)" \stmt -> do
                Streaming.nextRow stmt >>= (@?= Just (Only (0 :: Int64)))
                state <- readIORef (statementStream stmt)
                case state of
                    StatementStreamActive stream -> do
                        flag <- (peek (statementStreamResult stream) >>= \rawValue -> duckdb_result_is_streaming rawValue)
                        assertBool "expected a native streaming result" (flag /= 0)
                    _ -> assertFailure "expected an active cursor"
        , testCase "default execution remains materialized when cursor entry points change" $ withDb \conn ->
            withStatement conn "SELECT i FROM range(10000) t(i)" \stmt -> do
                nextRow stmt >>= (@?= Just (Only (0 :: Int64)))
                Streaming.nextRow stmt >>= (@?= Just (Only (1 :: Int64)))
                state <- readIORef (statementStream stmt)
                case state of
                    StatementStreamActive stream -> do
                        flag <- (peek (statementStreamResult stream) >>= \rawValue -> duckdb_result_is_streaming rawValue)
                        flag @?= 0
                    _ -> assertFailure "expected an active cursor"
        , testCase "nested values survive chunk cleanup" $ withDb \conn ->
            forM_
                [ "SELECT CASE WHEN i % 7 = 0 THEN NULL ELSE {'n': i, 'text': 'λ' || i::VARCHAR, 'list': [i, NULL], 'union': union_value(value := i)} END FROM range(5000) t(i)"
                , "SELECT [{'n': i, 'text': 'λ' || i::VARCHAR}, NULL] FROM range(5000) t(i)"
                , "SELECT map([i], [{'values': [i, NULL], 'union': union_value(value := i)}]) FROM range(5000) t(i)"
                , "SELECT CASE WHEN i % 2 = 0 THEN union_value(n := i)::UNION(n BIGINT, text VARCHAR) ELSE union_value(text := i::VARCHAR)::UNION(n BIGINT, text VARCHAR) END FROM range(5000) t(i)"
                ]
                \sql -> do
                    expected <- query_ conn sql :: IO [Only FieldValue]
                    actual <- reverse <$> Streaming.fold_ conn sql [] (\acc row -> pure (row : acc))
                    actual @?= expected
        , testCase "fold and foldNamed use executed parameter types" $ withDb \conn -> do
            positional <- Streaming.fold conn "SELECT coalesce(?, 'x') FROM range(3)" (Only (5000000000 :: Int64)) [] (\acc row -> pure (row : acc))
            positional @?= replicate 3 (Only (5000000000 :: Int64))
            named <- Streaming.foldNamed conn "SELECT $value FROM range(3)" ["value" := ("λ" :: Text)] [] (\acc row -> pure (row : acc))
            named @?= replicate 3 (Only ("λ" :: Text))
        , testCase "rebinding an active cursor replaces its metadata" $ withDb \conn ->
            withStatement conn "SELECT ? FROM range(3)" \stmt -> do
                bind stmt [toField (5000000000 :: Int64)]
                Streaming.nextRow stmt >>= (@?= Just (Only (5000000000 :: Int64)))
                bind stmt [toField ("λ" :: Text)]
                Streaming.nextRow stmt >>= (@?= Just (Only ("λ" :: Text)))
        , testCase "schema changes replace prepare-time metadata" $ withDb \conn -> do
            void $ execute_ conn "CREATE TABLE stream_rebind(x VARCHAR)"
            withStatement conn "SELECT x FROM stream_rebind" \stmt -> do
                void $ execute_ conn "DROP TABLE stream_rebind"
                void $ execute_ conn "CREATE TABLE stream_rebind(x BIGINT)"
                void $ execute_ conn "INSERT INTO stream_rebind VALUES (5000000000)"
                Streaming.nextRow stmt >>= (@?= Just (Only (5000000000 :: Int64)))
        , testCase "EOF persists until an explicit binding reset" $ withDb \conn ->
            withStatement conn "SELECT ?::BIGINT" \stmt -> do
                bind stmt [toField (1 :: Int64)]
                Streaming.nextRow stmt >>= (@?= Just (Only (1 :: Int64)))
                replicateM_ 3 $ Streaming.nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
                bind stmt [toField (2 :: Int64)]
                Streaming.nextRow stmt >>= (@?= Just (Only (2 :: Int64)))
                Streaming.nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
        , testCase "clearing bindings discards an active result" $ withDb \conn ->
            withStatement conn "SELECT ?::BIGINT FROM range(3)" \stmt -> do
                bind stmt [toField (7 :: Int64)]
                Streaming.nextRow stmt >>= (@?= Just (Only (7 :: Int64)))
                clearStatementBindings stmt
                void $ expectSqlError (Streaming.nextRow stmt :: IO (Maybe (Only Int64)))
                bind stmt [toField (8 :: Int64)]
                Streaming.nextRow stmt >>= (@?= Just (Only (8 :: Int64)))
        , testCase "a DML cursor does not execute again after EOF" $ withDb \conn -> do
            void $ execute_ conn "CREATE TABLE stream_once(x BIGINT)"
            withStatement conn "INSERT INTO stream_once VALUES (1)" \stmt ->
                replicateM_ 3 $ Streaming.nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
            query_ conn "SELECT count(*) FROM stream_once" >>= (@?= [Only (1 :: Int64)])
        , testCase "nextRowWith runs the supplied parser" $ withDb \conn ->
            withStatement conn "SELECT 20, 22" \stmt ->
                Streaming.nextRowWith ((+) <$> field <*> field) stmt >>= (@?= Just (42 :: Int64))
        , testCase "a fetch failure follows an already delivered batch" $ withDb \conn -> do
            void $ execute_ conn "SET streaming_buffer_size = '64KB'"
            failNext <- newIORef False
            delivered <- newIORef (0 :: Int)
            batchSize <- fromIntegral <$> duckdb_vector_size
            createFunction conn "late_stream_error" \(value :: Int64) -> do
                shouldFail <- readIORef failNext
                when shouldFail (throwIO (userError "late stream error"))
                pure value
            err <- expectSqlError $ Streaming.fold_ conn "SELECT late_stream_error(i) FROM range(1000000) t(i)" (0 :: Int) \count (Only (_ :: Int64)) -> do
                let next = count + 1
                writeIORef delivered next
                when (next == batchSize) (atomicWriteIORef failNext True)
                pure next
            count <- readIORef delivered
            assertBool "the first batch must precede the error" (count >= batchSize)
            assertBool "fetch error must retain its message" ("late stream error" `Text.isInfixOf` sqlErrorMessage err)
            assertReusable conn
        , testCase "Arrow reports a fetch failure after a delivered batch" $ withDb \conn -> do
            void $ execute_ conn "SET streaming_buffer_size = '64KB'"
            failNext <- newIORef False
            createFunction conn "late_arrow_error" \(value :: Int64) -> do
                shouldFail <- readIORef failNext
                when shouldFail (throwIO (userError "late Arrow error"))
                pure value
            err <- expectSqlError $ Streaming.foldArrow_ conn "SELECT late_arrow_error(i) FROM range(1000000) t(i)" () \() _ _ ->
                atomicWriteIORef failNext True
            readIORef failNext >>= assertBool "a batch reached the callback"
            assertBool "fetch error must retain its message" ("late Arrow error" `Text.isInfixOf` sqlErrorMessage err)
            assertReusable conn
        , testCase "row decode failure exhausts and releases the cursor" $ withDb \conn -> do
            withStatement conn "SELECT {'n': i} FROM range(5000) t(i)" \stmt -> do
                void $ expectSqlError (Streaming.nextRow stmt :: IO (Maybe (Only Int64)))
                Streaming.nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
            assertReusable conn
        , testCase "fold callback failure releases the active chunk" $ withDb \conn -> do
            outcome <- try $ Streaming.fold_ conn "SELECT {'n': i, 'list': [i, NULL]} FROM range(5000) t(i)" (0 :: Int) \count (Only (_ :: FieldValue)) ->
                if count == 2050 then throwIO (userError "stream step failure") else pure (count + 1)
            case outcome of
                Left (err :: IOException) -> assertBool "original callback error" ("stream step failure" `isInfixOf` show err)
                Right _ -> assertFailure "expected the callback to fail"
            assertReusable conn
        ]

-- | Run each case with one native worker so row order is stable.
withDb :: (Connection -> IO a) -> IO a
withDb = withConnectionWithConfig ":memory:" [("threads", "1")]

-- | Require a SQL or field conversion error and retain its diagnostic fields.
expectSqlError :: IO a -> IO SQLError
expectSqlError action = do
    outcome <- try action
    case outcome of
        Left err -> pure err
        Right _ -> assertFailure "expected SQLError" >> fail "expected SQLError"

-- | Verify that cleanup leaves the connection ready for another query.
assertReusable :: Connection -> Assertion
assertReusable conn = do
    rows <- query_ conn "SELECT 42" :: IO [Only Int64]
    rows @?= [Only 42]
