{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tasty-based test suite for duckdb-simple.
module Main (main) where

import Control.Exception (ErrorCall, Exception, try)
import Database.DuckDB.Simple
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "duckdb-simple"
        [ connectionTests
        , withConnectionTests
        , statementTests
        ]

connectionTests :: TestTree
connectionTests =
    testGroup
        "open/close"
        [ testCase "opens and closes an in-memory database" $ do
            conn <- open ":memory:"
            close conn
        , testCase "allows closing the same connection twice" $ do
            conn <- open ":memory:"
            close conn
            close conn
        ]

withConnectionTests :: TestTree
withConnectionTests =
    testGroup
        "withConnection"
        [ testCase "returns the action result" $ do
            result <- withConnection ":memory:" \_ -> pure (21 :: Int)
            assertEqual "action result" 21 result
        , testCase "propagates exceptions from the action" $
            assertThrowsErrorCall $
                withConnection ":memory:" (\_ -> error "boom" :: IO ())
        ]

statementTests :: TestTree
statementTests =
    testGroup
        "statements"
        [ testCase "prepares and closes a simple statement" $
            withConnection ":memory:" \conn -> do
                stmt <- openStatement conn "SELECT 42"
                closeStatement stmt
        , testCase "allows closing a statement twice" $
            withConnection ":memory:" \conn -> do
                stmt <- openStatement conn "SELECT 1"
                closeStatement stmt
                closeStatement stmt
        , testCase "rejects preparing statements on a closed connection" $ do
            conn <- open ":memory:"
            close conn
            assertThrowsSQLError $
                openStatement conn "SELECT 1"
        , testCase "throws SQLError for invalid SQL" $
            withConnection ":memory:" \conn ->
                assertThrowsSQLError $
                    openStatement conn "THIS IS NOT VALID SQL"
        , testCase "withStatement closes the statement automatically" $
            withConnection ":memory:" \conn -> do
                result <- withStatement conn "SELECT 1" \_ -> pure ("done" :: String)
                assertEqual "withStatement result" "done" result
        , testCase "withStatement closes statements even when the action fails" $
            withConnection ":memory:" \conn -> do
                assertThrowsErrorCall $
                    withStatement conn "SELECT 1" (\_ -> error "boom" :: IO ())
                stmt <- openStatement conn "SELECT 1"
                closeStatement stmt
        , testCase "executes statements and discards results" $
            withConnection ":memory:" \conn -> do
                execute_ conn "CREATE TABLE test_exec (x INTEGER)"
                withStatement conn "INSERT INTO test_exec VALUES (1)" \stmt ->
                    executeStatement stmt
        , testCase "execute runs with positional parameters" $
            withConnection ":memory:" \conn -> do
                execute_ conn "CREATE TABLE exec_params (a INTEGER, b TEXT)"
                execute conn "INSERT INTO exec_params VALUES (?, ?)" (5 :: Int, "hi" :: String)
        , testCase "rejects invalid direct execution" $
            withConnection ":memory:" \conn ->
                assertThrowsSQLError $
                    execute_ conn "THIS IS NOT SQL"
        , testCase "binds positional parameters" $
            withConnection ":memory:" \conn -> do
                execute_ conn "CREATE TABLE params (a INTEGER, b TEXT)"
                withStatement conn "INSERT INTO params VALUES (?, ?)" \stmt -> do
                    bind stmt [toField (5 :: Int), toField ("hi" :: String)]
                    executeStatement stmt
        , testCase "executeMany reuses prepared statements" $
            withConnection ":memory:" \conn -> do
                execute_ conn "CREATE TABLE params_many (a INTEGER, b TEXT)"
                executeMany conn "INSERT INTO params_many VALUES (?, ?)" [(1 :: Int, "x" :: String), (2 :: Int, "y" :: String)]
        , testCase "query_ fetches rows" $
            withConnection ":memory:" \conn -> do
                execute_ conn "CREATE TABLE query_rows (a INTEGER, b TEXT)"
                executeMany conn "INSERT INTO query_rows VALUES (?, ?)" [(1 :: Int, "x" :: String), (2 :: Int, "y" :: String)]
                rows <- query_ conn "SELECT a, b FROM query_rows ORDER BY a"
                assertEqual "query rows" [(1 :: Int, "x" :: String), (2 :: Int, "y" :: String)] rows
        , testCase "query fetches rows with parameters" $
            withConnection ":memory:" \conn -> do
                execute_ conn "CREATE TABLE query_params (a INTEGER, b TEXT)"
                executeMany conn "INSERT INTO query_params VALUES (?, ?)" [(1 :: Int, "x" :: String), (2 :: Int, "y" :: String)]
                rows <- query conn "SELECT a FROM query_params WHERE b = ?" (Only ("y" :: String))
                assertEqual "query result" [Only (2 :: Int)] rows
        , testCase "clears statement bindings without error" $
            withConnection ":memory:" \conn -> do
                stmt <- openStatement conn "SELECT ?"
                clearStatementBindings stmt
                closeStatement stmt
        , testCase "resolves named parameter indices" $
            withConnection ":memory:" \conn -> do
                stmt <- openStatement conn "SELECT $named_param"
                idx <- namedParameterIndex stmt "named_param"
                assertEqual "named parameter index" (Just 1) idx
                none <- namedParameterIndex stmt "missing"
                assertEqual "missing parameter index" Nothing none
                closeStatement stmt
        ]

assertThrows :: forall e a. (Exception e) => IO a -> (e -> Bool) -> Assertion
assertThrows action predicate = do
    outcome <- try action
    case outcome of
        Left (err :: e) -> assertBool "unexpected exception" (predicate err)
        Right _ -> assertFailure "expected exception, but action succeeded"

assertThrowsSQLError :: IO a -> Assertion
assertThrowsSQLError action =
    assertThrows action (const True :: SQLError -> Bool)

assertThrowsErrorCall :: IO a -> Assertion
assertThrowsErrorCall action =
    assertThrows action (const True :: ErrorCall -> Bool)
