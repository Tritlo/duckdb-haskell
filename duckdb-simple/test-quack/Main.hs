{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Check authenticated Quack connections with local extension artifacts.
module Main (main) where

import Control.Exception (finally, try)
import Control.Monad (unless, void)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (lookupEnv)
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit

-- | Require the matching artifacts before starting the integration test.
main :: IO ()
main = do
    httpfs <- extensionPath "DUCKDB_HTTPFS_EXTENSION"
    quack <- extensionPath "DUCKDB_QUACK_EXTENSION"
    defaultMain $
        testCase "authenticated Quack CONNECT, ATTACH, parameters and streaming" $
            withQuackConnection [httpfs, quack] \server -> do
                void $ execute_ server "CREATE TABLE items(id BIGINT, label VARCHAR)"
                void $ executeMany server "INSERT INTO items VALUES (?, ?)" [(1 :: Int64, "one" :: Text), (2, "two")]
                void $ execute_ server "CREATE TABLE large AS SELECT i FROM range(5000) t(i)"
                void $ execute_ server "CREATE TABLE instants(value TIMESTAMPTZ_NS)"
                let precise = posixSecondsToUTCTime 0.123456789
                void $ execute server "INSERT INTO instants VALUES (?)" (Only precise)
                [(uri, _, _)] <- query_ server "CALL quack_serve('quack:localhost:0', token='duckdb-haskell-loopback', disable_ssl=true)" :: IO [(Text, Text, Text)]
                finally
                    ( do
                        withQuackConnection [httpfs, quack] \client -> do
                            void $ execute_ client (remoteConnection "CONNECT" uri "duckdb-haskell-loopback")
                            (query_ client "SELECT id, label FROM items ORDER BY id" :: IO [(Int64, Text)])
                                >>= (@?= [(1, "one"), (2, "two")])
                            void $ execute_ client "DISCONNECT"
                            void $ execute_ client (remoteConnection "ATTACH" uri "duckdb-haskell-loopback")
                            void $ execute_ client "SET disabled_optimizers='remote_pushdown'"
                            (query client "SELECT id, label FROM remote.items WHERE id >= ? ORDER BY id" (Only (2 :: Int64)) :: IO [(Int64, Text)])
                                >>= (@?= [(2, "two")])
                            (queryNamed client "SELECT label FROM remote.items WHERE id=$id" ["id" := (1 :: Int64)] :: IO [Only Text])
                                >>= (@?= [Only "one"])
                            void $ execute client "INSERT INTO remote.items VALUES (?, ?)" (3 :: Int64, "bound 'λ'" :: Text)
                            (query_ server "SELECT label FROM items WHERE id=3" :: IO [Only Text])
                                >>= (@?= [Only "bound 'λ'"])
                            streamed <- Streaming.fold client "SELECT i FROM remote.large WHERE i < ?" (Only (5000 :: Int64)) (0 :: Int, 0 :: Int64) \(count, total) (Only value) ->
                                pure (count + 1, total + value)
                            streamed @?= (5000, 12497500)
                            (query_ client "SELECT value FROM remote.instants" :: IO [Only UTCTime])
                                >>= (@?= [Only precise])
                        withQuackConnection [httpfs, quack] \rejected -> do
                            result <- try (execute_ rejected (remoteConnection "CONNECT" uri "wrong-token"))
                            case result of
                                Left SQLError{sqlErrorMessage} ->
                                    assertBool "wrong credentials must fail authentication" ("Authentication failed" `Text.isInfixOf` sqlErrorMessage)
                                Right _ -> assertFailure "expected Quack to reject the wrong token"
                            (query_ server "SELECT count(*) FROM items" :: IO [Only Int64]) >>= (@?= [Only 3])
                    )
                    (void (query server "CALL quack_stop(?)" (Only uri) :: IO [Only Text]))

-- | Fail when an extension path is absent or does not name a local file.
extensionPath :: String -> IO FilePath
extensionPath name = do
    configured <- lookupEnv name
    path <- case configured of
        Just value | not (null value) -> pure value
        _ -> fail (name <> " must name a local DuckDB extension artifact")
    exists <- doesFileExist path
    unless exists $ fail (name <> " does not name an existing file: " <> path)
    canonicalizePath path

-- | Load the local extensions with the configuration required by this fixture.
withQuackConnection :: [FilePath] -> (Connection -> IO a) -> IO a
withQuackConnection extensions action =
    withConnectionWithConfig ":memory:" [("allow_unsigned_extensions", "true"), ("threads", "2")] \connection -> do
        mapM_ (\path -> void (execute_ connection (Query ("LOAD " <> sqlLiteral (Text.pack path))))) extensions
        action connection

-- | Build CONNECT or ATTACH SQL with quoted URI and token literals.
remoteConnection :: Text -> Text -> Text -> Query
remoteConnection command uri token =
    Query (command <> " " <> sqlLiteral uri <> alias <> " (TOKEN " <> sqlLiteral token <> ")")
  where
    alias = if command == "ATTACH" then " AS remote" else ""

-- | Quote a SQL string literal, including apostrophes in local paths.
sqlLiteral :: Text -> Text
sqlLiteral value = "'" <> Text.replace "'" "''" value <> "'"
