{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration checks for SQL features in the DuckDB 2.0 development snapshot.
module PreviewTests (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Data.Array (Array, listArray)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Variant (Variant (..), variantObject)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (assertFailureIO)

-- | Exercise the public query, binding, cursor, and fold operations.
tests :: TestTree
tests =
    testGroup
        "DuckDB 2.0 SQL features"
        [ testCase "row triggers audit bound inserts and roll back failures" $
            withConnection ":memory:" \conn -> do
                void (execute_ conn "CREATE TABLE target(id BIGINT, label VARCHAR)")
                void (execute_ conn "CREATE TABLE audit(id BIGINT PRIMARY KEY, label VARCHAR)")
                void (execute_ conn "CREATE TRIGGER record_insert AFTER INSERT ON target FOR EACH ROW INSERT INTO audit VALUES (NEW.id, NEW.label)")
                void (executeMany conn "INSERT INTO target VALUES (?, ?)" [(1 :: Int64, "first" :: Text), (2, "O'Reilly")])
                query_ conn "SELECT * FROM audit ORDER BY id" >>= (@?= [(1 :: Int64, "first" :: Text), (2, "O'Reilly")])
                assertFailureIO (execute conn "INSERT INTO target VALUES (?, ?)" (1 :: Int64, "duplicate" :: Text))
                query_ conn "SELECT count(*) FROM target" >>= (@?= [Only (2 :: Int64)])
        , testCase "DML CTEs bind values and deliver RETURNING rows to folds" $
            withConnection ":memory:" \conn -> do
                void (execute_ conn "CREATE TABLE moved(id BIGINT)")
                query conn "WITH ins AS (INSERT INTO moved VALUES (?), (?) RETURNING id) SELECT id FROM ins ORDER BY id" (1 :: Int64, 2 :: Int64)
                    >>= (@?= [Only (1 :: Int64), Only 2])
                total <- fold_ conn "WITH del AS (DELETE FROM moved RETURNING id) SELECT id FROM del" (0 :: Int64) \acc (Only value) -> pure (acc + value)
                total @?= 3
                query_ conn "SELECT count(*) FROM moved" >>= (@?= [Only (0 :: Int64)])
        , testCase "nested schemas accept qualified parameterized DML" $
            withConnection ":memory:" \conn -> do
                void (execute_ conn "CREATE SCHEMA parent_schema")
                void (execute_ conn "CREATE SCHEMA parent_schema.child_schema")
                void (execute_ conn "CREATE TABLE parent_schema.child_schema.items(id BIGINT)")
                void (execute conn "INSERT INTO parent_schema.child_schema.items VALUES (?)" (Only (17 :: Int64)))
                query_ conn "SELECT id FROM memory.parent_schema.child_schema.items" >>= (@?= [Only (17 :: Int64)])
                query_ conn "SELECT parent_schema FROM duckdb_schemas() WHERE schema_name='child_schema'" >>= (@?= [Only ("parent_schema" :: Text)])
        , testCase "session variables supply cursor and fold defaults; named bindings override them" $
            withConnection ":memory:" \conn -> do
                void (execute_ conn "SET VARIABLE preview_value = 41")
                query_ conn "SELECT $preview_value + 1" >>= (@?= [Only (42 :: Int64)])
                withStatement conn "SELECT $preview_value + 1" \stmt -> do
                    nextRow stmt >>= (@?= Just (Only (42 :: Int64)))
                    nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
                queryNamed conn "SELECT $preview_value + 1" ["preview_value" := (9 :: Int64)] >>= (@?= [Only (10 :: Int64)])
                queryNamed conn "SELECT $preview_value + $amount" ["amount" := (1 :: Int64)] >>= (@?= [Only (42 :: Int64)])
                queryNamed conn "SELECT $preview_value" [] >>= (@?= [Only (41 :: Int64)])
                assertFailureIO (queryNamed conn "SELECT $preview_value + $missing" [] :: IO [Only Int64])
                void (execute_ conn "SET VARIABLE preview_value = 83")
                total <- fold_ conn "SELECT $preview_value + 1" (0 :: Int64) \acc (Only value) -> pure (acc + value)
                total @?= 84
                void (execute_ conn "RESET VARIABLE preview_value")
                assertFailureIO (query_ conn "SELECT $preview_value" :: IO [Only Int64])
        , testCase "FETCH FIRST and OFFSET work with bound counts" $
            withConnection ":memory:" \conn ->
                query conn "SELECT i FROM range(10) t(i) ORDER BY i OFFSET ? ROWS FETCH FIRST ? ROWS ONLY" (3 :: Int64, 2 :: Int64)
                    >>= (@?= [Only (3 :: Int64), Only 4])
        , testCase "NEAREST joins rank candidates for bound input rows" $
            withConnection ":memory:" \conn ->
                query conn "SELECT q.id::BIGINT, t.value::BIGINT FROM (VALUES (1, ?::BIGINT), (2, ?::BIGINT)) q(id, value) INNER JOIN (VALUES (11), (19), (25), (100)) t(value) EXACT NEAREST 2 BY DISTANCE abs(q.value - t.value) ORDER BY q.id, t.value" (10 :: Int64, 20 :: Int64)
                    >>= (@?= [(1 :: Int64, 11 :: Int64), (1, 19), (2, 19), (2, 25)])
        , testCase "recursive USING KEY aggregates bound values before folding" $
            withConnection ":memory:" \conn -> do
                total <- fold conn "WITH RECURSIVE totals(k, total) USING KEY (k, sum(total)) AS (VALUES (1, ?::BIGINT) UNION SELECT k, 1::BIGINT FROM totals WHERE total < ?::BIGINT) SELECT total::BIGINT FROM totals" (1 :: Int64, 4 :: Int64) (0 :: Int64) \acc (Only value) -> pure (acc + value)
                total @?= 4
        , testCase "lambda filter, transform, and reduce accept bound lists" $
            withConnection ":memory:" \conn ->
                query conn "SELECT list_reduce(list_transform(list_filter(?::BIGINT[], lambda x: x % 2 = 0), lambda x: x * 3), lambda x, y: x + y)" (Only (listArray (0, 3) [1, 2, 3, 4] :: Array Int Int64))
                    >>= (@?= [Only (18 :: Int64)])
        , testCase "JSON mutation accepts bound documents, paths, and values" $
            withConnection ":memory:" \conn -> do
                query conn "SELECT json_set(?::JSON, ?, ?::JSON)::VARCHAR" ("{\"a\":1}" :: Text, "$.b" :: Text, "[2,3]" :: Text)
                    >>= (@?= [Only ("{\"a\":1,\"b\":[2,3]}" :: Text)])
                query_ conn "SELECT json_remove(json_replace(json_insert('{\"a\":1}', '$.b', '2'), '$.a', '3'), '$.b')::VARCHAR"
                    >>= (@?= [Only ("{\"a\":3}" :: Text)])
        , testCase "default 2.0 storage preserves bound VARIANT values after checkpoint and reopen" $
            withFiles \path -> do
                let value = Variant (variantObject [("n", FieldInt64 7), ("text", FieldText "saved")])
                withConnection path \conn -> do
                    void (execute_ conn "CREATE TABLE stored(value VARIANT)")
                    void (execute conn "INSERT INTO stored VALUES (?)" (Only value))
                    void (execute_ conn "CHECKPOINT")
                    query_ conn "SELECT tags['storage_version'] FROM duckdb_databases() WHERE NOT internal" >>= (@?= [Only ("v2.0.0+" :: Text)])
                withConnection path \conn -> do
                    query_ conn "SELECT tags['storage_version'] FROM duckdb_databases() WHERE NOT internal" >>= (@?= [Only ("v2.0.0+" :: Text)])
                    query_ conn "SELECT value FROM stored" >>= (@?= [Only value])
        , testCase "VARIANT values survive native storage and explicit Parquet shredding" $
            withFiles \path -> do
                let parquet = path <> ".parquet"
                    expected = [Only (Variant (variantObject [("n", FieldInt64 i), ("text", FieldText (Text.pack (show i)))])) | i <- [0, 2048, 4096]]
                withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                    void (execute_ conn "SET variant_minimum_shredding_size = 0")
                    void (execute_ conn "CREATE TABLE variants AS SELECT i, {'n': i, 'text': i::VARCHAR}::VARIANT AS v FROM range(5000) t(i)")
                    void (execute_ conn "CHECKPOINT")
                withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
                    query_ conn "SELECT v FROM variants WHERE i % 2048 = 0 ORDER BY i" >>= (@?= expected)
                    void (execute conn "COPY variants TO ? (FORMAT PARQUET, SHREDDING {'v': 'STRUCT(n BIGINT, text VARCHAR)'})" (Only (Text.pack parquet)))
                    query conn "SELECT v FROM read_parquet(?) WHERE i % 2048 = 0 ORDER BY i" (Only (Text.pack parquet)) >>= (@?= expected)
                    query conn "SELECT count(*) > 0 FROM parquet_schema(?) WHERE name='typed_value'" (Only (Text.pack parquet)) >>= (@?= [Only True])
                    total <- fold conn "SELECT v.n::BIGINT FROM read_parquet(?)" (Only (Text.pack parquet)) (0 :: Int64) \acc (Only value) -> pure (acc + value)
                    total @?= 12497500
        ]

-- | Reserve a database path. Remove the database, WAL, and Parquet file on exit.
withFiles :: (FilePath -> IO a) -> IO a
withFiles = bracket acquire release
  where
    acquire = do
        directory <- getTemporaryDirectory
        (path, handle) <- openBinaryTempFile directory "duckdb-preview"
        hClose handle
        removeFile path
        pure path
    release path =
        forM_ [path, path <> ".wal", path <> ".parquet"] \file -> do
            exists <- doesFileExist file
            when exists (removeFile file)
