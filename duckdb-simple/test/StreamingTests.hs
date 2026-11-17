{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : StreamingTests
-- Description : Check the explicit native streaming API and its cursor ownership.
module StreamingTests (nativeStreamingTests) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, throwIO, try)
import Control.Monad (forM_, replicateM_, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.IORef (atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isInfixOf, sort, sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.DuckDB.FFI as Retained (c_duckdb_vector_size)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (Field (fieldType), FieldValue (..))
import Database.DuckDB.Simple.Geometry (RawGeometry (..))
import Database.DuckDB.Simple.Internal (
  DuckDBV2ResultHandle (..),
  Statement (statementStream),
  StatementStream (statementStreamResult),
  StatementStreamState (..),
  nullHandle,
 )
import qualified Database.DuckDB.Simple.Streaming as Streaming
import Database.DuckDB.Simple.Variant (Variant (..), variantObject)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestUtils (assertReusable, filteredQueries)

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
              assertBool "expected a live native cursor" (statementStreamResult stream /= nullHandle)
            _ -> assertFailure "expected an active cursor"
    , testCase "default execution remains materialized when cursor entry points change" $ withDb \conn ->
        withStatement conn "SELECT i FROM range(10000) t(i)" \stmt -> do
          nextRow stmt >>= (@?= Just (Only (0 :: Int64)))
          Streaming.nextRow stmt >>= (@?= Just (Only (1 :: Int64)))
          state <- readIORef (statementStream stmt)
          case state of
            StatementStreamActive stream -> do
              assertBool "expected a released execution cursor" (statementStreamResult stream == nullHandle)
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
        withStatement conn "INSERT INTO stream_once VALUES (1)" \stmt -> do
          Streaming.nextRow stmt >>= (@?= Just (Only (1 :: Int64)))
          replicateM_ 3 $ Streaming.nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
        query_ conn "SELECT count(*) FROM stream_once" >>= (@?= [Only (1 :: Int64)])
        Streaming.fold_ conn "INSERT INTO stream_once VALUES (2)" [] (\acc row -> pure (row : acc)) >>= (@?= [Only (1 :: Int64)])
        query_ conn "SELECT count(*) FROM stream_once" >>= (@?= [Only (2 :: Int64)])
    , testCase "nextRowWith runs the supplied parser" $ withDb \conn ->
        withStatement conn "SELECT 20, 22" \stmt ->
          Streaming.nextRowWith ((+) <$> field <*> field) stmt >>= (@?= Just (42 :: Int64))
    , testGroup "decoder cursor ownership" $
        map (uncurry decoderCursorTests) [("materialized", nextRowWith), ("streaming", Streaming.nextRowWith)]
    , testCase "eager decoder cannot read uncached native metadata after connection close" $ withDb \conn -> do
        let parser = fieldWith \column -> liftConversion (close conn) >> fieldType column
        err <- expectSqlError (queryWith_ parser conn "SELECT 42")
        assertBool "expected closed connection" ("connection is closed" `Text.isInfixOf` sqlErrorMessage err)
    , testCase "a fetch failure follows an already delivered batch" $ withDb \conn -> do
        void $ execute_ conn "SET streaming_buffer_size = '64KB'"
        failNext <- newIORef False
        delivered <- newIORef (0 :: Int)
        batchSize <- fromIntegral <$> Retained.c_duckdb_vector_size
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
          cursor <- readIORef (statementStream stmt)
          case cursor of
            StatementStreamExhausted -> pure ()
            _ -> assertFailure "expected an exhausted cursor after the decode failure"
          Streaming.nextRow stmt >>= (@?= (Nothing :: Maybe (Only Int64)))
        assertReusable conn
        -- DuckDB refuses each other query of the connection while a native stream
        -- runs. A query that works while the statement is open shows that the
        -- failure released the native result. The large result keeps the stream
        -- running after the first chunk.
        withStatement conn "SELECT {'n': i} FROM range(1000000) t(i)" \stmt -> do
          void $ expectSqlError (Streaming.nextRow stmt :: IO (Maybe (Only Int64)))
          assertReusable conn
    , -- The case of the leak test for a statement that closes with a live cursor
      -- checks the release of the chunks.
      testCase "a failing step of a streaming fold raises its error and leaves the connection usable" $ withDb \conn -> do
        outcome <- try $ Streaming.fold_ conn "SELECT {'n': i, 'list': [i, NULL]} FROM range(5000) t(i)" (0 :: Int) \count (Only (_ :: FieldValue)) ->
          if count == 2050 then throwIO (userError "stream step failure") else pure (count + 1)
        case outcome of
          Left (err :: IOException) -> assertBool "original callback error" ("stream step failure" `isInfixOf` show err)
          Right _ -> assertFailure "expected the callback to fail"
        assertReusable conn
    , testCase "a query of another thread during an explicit stream fails and the stream continues" $ withDb \conn -> do
        -- The case needs a statement that still runs after the first row, so
        -- the stream reads a large result.
        finished <- timeout 120000000 $ withStatement conn "SELECT i FROM range(1000000) t(i)" \stmt -> do
          Streaming.nextRow stmt >>= (@?= Just (Only (0 :: Int64)))
          other <- newEmptyMVar
          _ <- forkIO (try (query_ conn "SELECT 42" :: IO [Only Int64]) >>= putMVar other)
          outcome <- takeMVar other
          case outcome of
            Left (err :: SQLError) -> sqlErrorType err @?= Just ErrorResourceInUse
            Right _ -> assertFailure "expected a busy connection during the stream"
          let readFrom expected = do
                row <- Streaming.nextRow stmt
                case row of
                  Nothing -> pure expected
                  Just (Only value) -> do
                    when (value /= expected) $ assertFailure ("expected row " <> show expected <> ", got " <> show value)
                    readFrom (expected + 1)
          readFrom (1 :: Int64) >>= (@?= 1000000)
        finished @?= Just ()
        assertReusable conn
    , testCase "filtered results give each row through each read function" $
        forM_ ["1", "4"] \threads -> withConnectionWithConfig ":memory:" [("threads", threads)] \conn ->
          forM_ filteredQueries \(sql, expected) ->
            forM_ readFunctions \(name, readRows) -> do
              rows <- map fromOnly <$> readRows conn sql
              -- With more than one thread, DuckDB can give the rows in another order.
              assertEqual (name <> " with " <> Text.unpack threads <> " threads: " <> show sql) expected (if threads == "1" then rows else sort rows)
    , testCase "filtered and joined VARIANT and GEOMETRY columns give the value of their row" $ withDb \conn -> do
        -- A filter or the probe side of a hash join can give a vector with a
        -- selection. Each row holds its own key, so a value of another row fails.
        void $
          execute_ conn "CREATE TABLE shaped AS SELECT i, {'n': i}::VARIANT AS v, ('POINT (' || i || ' 0)')::GEOMETRY AS g FROM range(5000) t(i)"
        forM_
          [ ("SELECT i, v, g FROM shaped WHERE i % 3 = 1", [1, 4 .. 4999])
          , ("SELECT a.i, a.v, a.g FROM shaped a JOIN (SELECT j FROM range(0, 5000, 7) t(j)) b ON a.i = b.j", [0, 7 .. 4999])
          ]
          \(sql, keys) ->
            forM_ readFunctions \(name, readRows) -> do
              rows <- readRows conn sql
              assertEqual
                (name <> ": " <> show sql)
                [(key, Variant (variantObject [("n", FieldInt64 key)]), pointWkb key) | key <- keys]
                (sortOn (\(key, _, _) -> key) [(key, value, rawGeometryWKB geometry) | (key, value, geometry) <- rows])
    ]

-- | Check operations on the same statement from inside its decoder.
decoderCursorTests :: String -> (RowParser Int64 -> Statement -> IO (Maybe Int64)) -> TestTree
decoderCursorTests name next =
  testGroup
    name
    [ testCase "a nested fetch consumes the next row" $ withDb \conn ->
        withStatement conn "SELECT i FROM range(1,4) t(i)" \stmt -> do
          let parser = fieldWith \column -> do
                liftConversion (next field stmt >>= (@?= Just 2))
                fromField column
          next parser stmt >>= (@?= Just 1)
          next field stmt >>= (@?= Just 3)
    , testCase "reset on the last row leaves the statement idle" $ withDb \conn ->
        withStatement conn "SELECT 42" \stmt -> do
          let parser = fieldWith \column -> do
                liftConversion (clearStatementBindings stmt)
                fromField column
          next parser stmt >>= (@?= Just 42)
          next field stmt >>= (@?= Just 42)
    , testCase "decoder failure preserves a replacement cursor" $ withDb \conn ->
        withStatement conn "SELECT ?::BIGINT + i FROM range(2) t(i)" \stmt -> do
          bind stmt [toField (10 :: Int64)]
          let parser = fieldWith \_ -> liftConversion do
                bind stmt [toField (20 :: Int64)]
                next field stmt >>= (@?= Just 20)
                throwIO (userError "decoder failed after rebind")
          outcome <- try (next parser stmt)
          case outcome of
            Left (err :: IOException) -> assertBool "original decoder error" ("decoder failed after rebind" `isInfixOf` show err)
            Right _ -> assertFailure "expected the decoder to fail"
          next field stmt >>= (@?= Just 21)
    ]

-- | Read the rows of a query with each read function of the library.
readFunctions :: (FromRow row) => [(String, Connection -> Query -> IO [row])]
readFunctions =
  [ ("query_", query_)
  , ("fold_", \conn sql -> reverse <$> fold_ conn sql [] (\rows row -> pure (row : rows)))
  , ("nextRow", \conn sql -> withStatement conn sql (readEach nextRow))
  , ("Streaming.fold_", \conn sql -> reverse <$> Streaming.fold_ conn sql [] (\rows row -> pure (row : rows)))
  , ("Streaming.nextRow", \conn sql -> withStatement conn sql (readEach Streaming.nextRow))
  ]
  where
    readEach :: (Statement -> IO (Maybe row)) -> Statement -> IO [row]
    readEach next stmt = go []
      where
        go rows = next stmt >>= maybe (pure (reverse rows)) (\row -> go (row : rows))

-- | The WKB of the point (x, 0) in the little-endian byte order. DuckDB 2.0.0
-- gives the same bytes for 'POINT (x 0)'::GEOMETRY::BLOB.
pointWkb :: Int64 -> BS.ByteString
pointWkb x = BL.toStrict (Builder.toLazyByteString (Builder.word8 1 <> Builder.word32LE 1 <> Builder.doubleLE (fromIntegral x) <> Builder.doubleLE 0))

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
