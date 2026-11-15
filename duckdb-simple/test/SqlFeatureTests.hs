{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : SqlFeatureTests
-- Description : Integration checks for DuckDB 2.0 SQL features.
module SqlFeatureTests (tests) where

import Control.Exception (bracket, try)
import Control.Monad (forM_, void, when)
import Data.Array (Array, listArray)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.LocalTime (TimeOfDay (..))
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue (..), TimeWithZone (..))
import Database.DuckDB.Simple.Variant (Variant (..), variantObject)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
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
          void (execute_ conn "SET VARIABLE session_value = 41")
          query_ conn "SELECT $session_value + 1" >>= (@?= [Only (42 :: Int64)])
          withStatement conn "SELECT $session_value + 1" \stmt -> do
            nextRow stmt >>= (@?= Just (Only (42 :: Int64)))
            nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
          queryNamed conn "SELECT $session_value + 1" ["session_value" := (9 :: Int64)] >>= (@?= [Only (10 :: Int64)])
          queryNamed conn "SELECT $session_value + $amount" ["amount" := (1 :: Int64)] >>= (@?= [Only (42 :: Int64)])
          queryNamed conn "SELECT $session_value" [] >>= (@?= [Only (41 :: Int64)])
          assertFailureIO (queryNamed conn "SELECT $session_value + $missing" [] :: IO [Only Int64])
          void (execute_ conn "SET VARIABLE session_value = 83")
          total <- fold_ conn "SELECT $session_value + 1" (0 :: Int64) \acc (Only value) -> pure (acc + value)
          total @?= 84
          void (execute_ conn "RESET VARIABLE session_value")
          assertFailureIO (query_ conn "SELECT $session_value" :: IO [Only Int64])
    , testCase "FETCH FIRST and OFFSET work with bound counts" $
        withConnection ":memory:" \conn ->
          query conn "SELECT i FROM range(10) t(i) ORDER BY i OFFSET ? ROWS FETCH FIRST ? ROWS ONLY" (3 :: Int64, 2 :: Int64)
            >>= (@?= [Only (3 :: Int64), Only 4])
    , testCase "SELECT expressions use the query and cursor paths" $
        withConnection ":memory:" \conn -> do
          query conn "SELECT ?::BIGINT + 1" (Only (41 :: Int64)) >>= (@?= [Only (42 :: Int64)])
          withStatement conn "SELECT 42::BIGINT" \stmt -> do
            nextRow stmt >>= (@?= Just (Only (42 :: Int64)))
            nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
    , testCase "NEAREST joins rank candidates for bound input rows" $
        withConnection ":memory:" \conn ->
          query
            conn
            "SELECT q.id::BIGINT, t.value::BIGINT FROM (VALUES (1, ?::BIGINT), (2, ?::BIGINT)) q(id, value) INNER JOIN (VALUES (11), (19), (25), (100)) t(value) EXACT NEAREST 2 BY DISTANCE abs(q.value - t.value) ORDER BY q.id, t.value"
            (10 :: Int64, 20 :: Int64)
            >>= (@?= [(1 :: Int64, 11 :: Int64), (1, 19), (2, 19), (2, 25)])
    , testCase "recursive USING KEY aggregates bound values before folding" $
        withConnection ":memory:" \conn -> do
          total <- fold
            conn
            "WITH RECURSIVE totals(k, total) USING KEY (k, sum(total)) AS (VALUES (1, ?::BIGINT) UNION SELECT k, 1::BIGINT FROM totals WHERE total < ?::BIGINT) SELECT total::BIGINT FROM totals"
            (1 :: Int64, 4 :: Int64)
            (0 :: Int64)
            \acc (Only value) -> pure (acc + value)
          total @?= 4
    , testCase "lambda filter, transform, and reduce accept bound lists" $
        withConnection ":memory:" \conn ->
          query
            conn
            "SELECT list_reduce(list_transform(list_filter(?::BIGINT[], lambda x: x % 2 = 0), lambda x: x * 3), lambda x, y: x + y)"
            (Only (listArray (0, 3) [1, 2, 3, 4] :: Array Int Int64))
            >>= (@?= [Only (18 :: Int64)])
    , testCase "JSON mutation accepts bound documents, paths, and values" $
        withConnection ":memory:" \conn -> do
          query conn "SELECT json_set(?::JSON, ?, ?::JSON)" ("{\"a\":1}" :: Text, "$.b" :: Text, "[2,3]" :: Text)
            >>= (@?= [Only ("{\"a\":1,\"b\":[2,3]}" :: Text)])
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
              expected = [(i, Text.pack (show i)) | i <- [0, 2048, 4096 :: Int64]]
          withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
            void (execute_ conn "SET variant_minimum_shredding_size = 0")
            void (execute_ conn "CREATE TABLE variants AS SELECT i, {'n': i, 'text': i::VARCHAR}::VARIANT AS v FROM range(5000) t(i)")
            void (execute_ conn "CHECKPOINT")
          withConnectionWithConfig path [("storage_compatibility_version", "v1.5.0")] \conn -> do
            query_ conn "SELECT v.n::BIGINT, v.text::VARCHAR FROM variants WHERE i % 2048 = 0 ORDER BY i" >>= (@?= expected)
            query_ conn "SELECT v FROM variants WHERE i % 2048 = 0 ORDER BY i"
              >>= (@?= [Only (Variant (variantObject [("n", FieldInt64 i), ("text", FieldText text)])) | (i, text) <- expected])
            void (execute conn "COPY variants TO ? (FORMAT PARQUET, SHREDDING {'v': 'STRUCT(n BIGINT, text VARCHAR)'})" (Only (Text.pack parquet)))
            query conn "SELECT v.n::BIGINT, v.text::VARCHAR FROM read_parquet(?) WHERE i % 2048 = 0 ORDER BY i" (Only (Text.pack parquet))
              >>= (@?= expected)
            query conn "SELECT count(*) > 0 FROM parquet_schema(?) WHERE name='typed_value'" (Only (Text.pack parquet)) >>= (@?= [Only True])
            total <- fold conn "SELECT v.n::BIGINT FROM read_parquet(?)" (Only (Text.pack parquet)) (0 :: Int64) \acc (Only value) -> pure (acc + value)
            total @?= 12497500
    , -- Each case of this group runs the SQL form of a limit that the
      -- documentation of duckdb-simple states. The case fails when DuckDB lifts
      -- the limit.
      testGroup
        "limits that DuckDB sets"
        [ testCase "an ARRAY type needs at least one element" $ withConnection ":memory:" \conn ->
            assertDuckDBError
              ErrorQueryBinder
              "Binder Error: ARRAY type size must be at least 1"
              (query_ conn "SELECT []::INTEGER[0]" :: IO [Only Text])
        , testCase "a STRUCT for a VARIANT cannot have two equal keys" $ withConnection ":memory:" \conn ->
            assertDuckDBError
              ErrorQueryBinder
              "Binder Error: Duplicate named argument \"a\""
              (query_ conn "SELECT {'a': 1, 'a': 2}::VARIANT" :: IO [Only Variant])
        , testCase "a VARIANT does not keep the CRS of a GEOMETRY" $ withConnection ":memory:" \conn ->
            query_ conn "SELECT typeof('POINT (1 2)'::GEOMETRY('OGC:CRS84')::VARIANT::GEOMETRY), typeof('POINT (1 2)'::GEOMETRY('OGC:CRS84'))"
              >>= (@?= [("GEOMETRY" :: Text, "GEOMETRY('OGC:CRS84')" :: Text)])
        , testCase "a BIT value needs at least one bit" $ withConnection ":memory:" \conn -> do
            assertDuckDBError
              ErrorTypeConversion
              "Conversion Error: Cannot cast empty string to BIT"
              (query_ conn "SELECT bitstring('', 0)" :: IO [Only Text])
            -- An empty string literal is one zero bit.
            query_ conn "SELECT ''::BIT::VARCHAR" >>= (@?= [Only ("0" :: Text)])
        , testCase "a value that is not NULL has no cast to the type NULL" $ withConnection ":memory:" \conn ->
            assertDuckDBError
              ErrorTypeConversion
              "Conversion Error: Unimplemented type for cast (INTEGER -> \"NULL\")"
              (query_ conn "SELECT 7::\"NULL\"" :: IO [Only Text])
        , testCase "a TIMETZ offset is at most 15:59:59 in each direction" $ withConnection ":memory:" \conn -> do
            forM_ ["12:00:00+16:00:00", "12:00:00-16:00:00"] \value ->
              assertDuckDBError
                ErrorTypeConversion
                ("Conversion Error: time field value out of range: \"" <> value <> "\"")
                (query_ conn (Query ("SELECT '" <> value <> "'::TIMETZ")) :: IO [Only Text])
            query_ conn "SELECT '12:00:00+15:59:59'::TIMETZ, '12:00:00-15:59:59'::TIMETZ"
              >>= (@?= [(TimeWithZone (TimeOfDay 12 0 0) 57599, TimeWithZone (TimeOfDay 12 0 0) (-57599))])
        , testCase "DuckDB has no savepoints" $ withConnection ":memory:" \conn ->
            assertDuckDBError ErrorQueryParser "Parser Error: syntax error at or near \"SAVEPOINT\"" (execute_ conn "SAVEPOINT a")
        , testCase "the CRS of a GEOMETRY cast is a constant" $ withConnection ":memory:" \conn ->
            assertDuckDBError
              ErrorQueryParser
              "Parser Error: Expected a constant as type modifier"
              (withStatement conn "SELECT ?::GEOMETRY(?)" columnCount)
        , testCase "VARIANT columns need the storage version v1.5.0" $ withFiles \path ->
            withConnectionWithConfig path [("storage_compatibility_version", "v1.0.0")] \conn ->
              assertDuckDBError
                ErrorInputInvalid
                "Invalid Input Error: VARIANT columns are not supported in storage versions prior to v1.5.0"
                (execute_ conn "CREATE TABLE old_variant (v VARIANT)")
        ]
    ]

-- | Require an SQL error whose message starts with the text of DuckDB, and
-- whose type is the given one.
assertDuckDBError :: ErrorType -> Text -> IO a -> Assertion
assertDuckDBError expectedType expected action = do
  outcome <- try action
  case outcome of
    Left err -> do
      assertBool (Text.unpack (sqlErrorMessage err)) (expected `Text.isPrefixOf` sqlErrorMessage err)
      sqlErrorType err @?= Just expectedType
    Right _ -> assertFailure ("expected the DuckDB error " <> Text.unpack expected)

-- | Reserve a database path. Remove the database, WAL, and Parquet file on exit.
withFiles :: (FilePath -> IO a) -> IO a
withFiles = bracket acquire release
  where
    acquire = do
      directory <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile directory "duckdb-sql-feature"
      hClose handle
      removeFile path
      pure path
    release path =
      forM_ [path, path <> ".wal", path <> ".parquet"] \file -> do
        exists <- doesFileExist file
        when exists (removeFile file)
