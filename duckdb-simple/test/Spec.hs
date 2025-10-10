{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tasty-based test suite for duckdb-simple.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (ErrorCall, Exception, try)
import Control.Monad (replicateM)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Int (Int64)
import qualified Data.Text as Text
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (Field (..))
import GHC.Generics (Generic)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

data Person = Person
    { personId :: Int
    , personName :: Text.Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromRow)

data WithRemaining = WithRemaining Int Int
    deriving (Eq, Show)

instance FromRow WithRemaining where
    fromRow = do
        firstVal <- field
        remaining <- numFieldsRemaining
        _ <- replicateM remaining (fieldWith (const (Right ())))
        pure (WithRemaining firstVal remaining)

newtype YesNo = YesNo Bool
    deriving (Eq, Show)

instance FromRow YesNo where
    fromRow = parseYes <|> parseNo
      where
        parseYes = YesNo True <$ fieldWith (match (Text.pack "yes"))
        parseNo = YesNo False <$ fieldWith (match (Text.pack "no"))

        match expected fld@Field{fieldIndex} =
            case fromField fld :: Either ResultError Text.Text of
                Left err -> Left err
                Right txt ->
                    let normalized = Text.toLower txt
                     in if normalized == expected
                            then Right ()
                            else
                                Left
                                    ConversionError
                                        { resultErrorColumn = fieldIndex
                                        , resultErrorMessage =
                                            Text.concat
                                                [ "duckdb-simple: expected "
                                                , expected
                                                , Text.pack " but found "
                                                , normalized
                                                ]
                                        }

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "duckdb-simple"
        [ connectionTests
        , withConnectionTests
        , statementTests
        , functionsTests
        , transactionTests
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
        , testCase "execute returns affected row count" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE test_exec (x INTEGER)"
                count <- execute conn "INSERT INTO test_exec VALUES (?)" (Only (1 :: Int))
                assertEqual "rows affected" 1 count
        , testCase "execute runs with positional parameters" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE exec_params (a INTEGER, b TEXT)"
                count <- execute conn "INSERT INTO exec_params VALUES (?, ?)" (5 :: Int, "hi" :: String)
                assertEqual "rows affected" 1 count
        , testCase "rejects invalid direct execution" $
            withConnection ":memory:" \conn ->
                assertThrowsSQLError $
                    execute_ conn "THIS IS NOT SQL"
        , testCase "binds positional parameters" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE params (a INTEGER, b TEXT)"
                withStatement conn "INSERT INTO params VALUES (?, ?)" \stmt -> do
                    bind stmt [toField (5 :: Int), toField ("hi" :: String)]
                    changed <- executeStatement stmt
                    assertEqual "rows affected" 1 changed
        , testCase "executeMany reuses prepared statements" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE params_many (a INTEGER, b TEXT)"
                total <- executeMany conn "INSERT INTO params_many VALUES (?, ?)" [(1 :: Int, "x" :: String), (2 :: Int, "y" :: String)]
                assertEqual "rows affected" 2 total
        , testCase "executeNamed binds named parameters" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE named_params (a INTEGER, b TEXT)"
                count <- executeNamed conn "INSERT INTO named_params VALUES ($a, $b)" ["$a" := (1 :: Int), "$b" := ("named" :: String)]
                assertEqual "rows affected" 1 count
                rows <- queryNamed conn "SELECT a FROM named_params WHERE b = $label" ["$label" := ("named" :: String)]
                assertEqual "named query" [Only (1 :: Int)] rows
        , testCase "rejects incorrect positional argument counts" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE param_count (a INTEGER, b INTEGER)"
                assertThrowsFormatError
                    (execute conn "INSERT INTO param_count VALUES (?, ?)" (Only (1 :: Int)))
                    (Text.isInfixOf "parameter(s)" . formatErrorMessage)
        , testCase "rejects positional bindings on named statements" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE mixed_mode (a INTEGER)"
                assertThrowsFormatError
                    (execute conn "INSERT INTO mixed_mode VALUES ($a)" (Only (1 :: Int)))
                    (Text.isInfixOf "named parameters" . formatErrorMessage)
        , testCase "reports error when mixing positional and named parameters" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE mixed_params (a INTEGER, b TEXT)"
                assertThrows
                    ( withStatement conn "INSERT INTO mixed_params VALUES (?, $label)" \stmt -> do
                        bind stmt [toField (1 :: Int)]
                        bindNamed stmt ["$label" := ("combo" :: String)]
                        _ <- executeStatement stmt
                        pure ()
                    )
                    ( \(err :: SQLError) ->
                        Text.isInfixOf "Mixing named and positional parameters" (sqlErrorMessage err)
                    )
        , testCase "generic FromRow derivation works" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE person (id INTEGER, name TEXT)"
                _ <- executeMany conn "INSERT INTO person VALUES (?, ?)" [(1 :: Int, "Alice" :: String), (2 :: Int, "Bob" :: String)]
                people <- query_ conn "SELECT id, name FROM person ORDER BY id" :: IO [Person]
                assertEqual "person rows" [Person 1 (Text.pack "Alice"), Person 2 (Text.pack "Bob")] people
        , testCase "query_ fetches rows" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE query_rows (a INTEGER, b TEXT)"
                _ <- executeMany conn "INSERT INTO query_rows VALUES (?, ?)" [(1 :: Int, "x" :: String), (2 :: Int, "y" :: String)]
                rows <- query_ conn "SELECT a, b FROM query_rows ORDER BY a"
                assertEqual "query rows" [(1 :: Int, "x" :: String), (2 :: Int, "y" :: String)] rows
        , testCase "query decodes NULL as Maybe" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE maybe_vals (a TEXT)"
                _ <- execute conn "INSERT INTO maybe_vals VALUES (?)" (Only (Just ("present" :: String)))
                _ <- execute conn "INSERT INTO maybe_vals VALUES (?)" (Only (Nothing :: Maybe String))
                rows <- query_ conn "SELECT a FROM maybe_vals ORDER BY a IS NULL, a"
                assertEqual "maybe decoding" [Only (Just ("present" :: String)), Only Nothing] rows
        , testCase "query fetches rows with parameters" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE query_params (a INTEGER, b TEXT)"
                _ <- executeMany conn "INSERT INTO query_params VALUES (?, ?)" [(1 :: Int, "x" :: String), (2 :: Int, "y" :: String)]
                rows <- query conn "SELECT a FROM query_params WHERE b = ?" (Only ("y" :: String))
                assertEqual "query result" [Only (2 :: Int)] rows
        , testCase "column mismatch surfaces as SQLError" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE mismatch (a INTEGER, b INTEGER)"
                _ <- execute_ conn "INSERT INTO mismatch VALUES (1, 2)"
                assertThrows
                    (query_ conn "SELECT a, b FROM mismatch" :: IO [Only Int])
                    (Text.isInfixOf "expected 1 columns" . sqlErrorMessage)
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
                idxWithPrefix <- namedParameterIndex stmt "$named_param"
                assertEqual "named parameter index with prefix" (Just 1) idxWithPrefix
                colonIdx <- namedParameterIndex stmt ":named_param"
                assertEqual "named parameter index with colon" (Just 1) colonIdx
                none <- namedParameterIndex stmt "missing"
                assertEqual "missing parameter index" Nothing none
                closeStatement stmt
        , testCase "bindNamed rejects statements without named placeholders" $
            withConnection ":memory:" \conn -> do
                stmt <- openStatement conn "SELECT ?"
                assertThrowsFormatError
                    (bindNamed stmt [":value" := (1 :: Int)])
                    (Text.isInfixOf "does not define named parameters" . formatErrorMessage)
                closeStatement stmt
        , testCase "bindNamed rejects unknown parameters" $
            withConnection ":memory:" \conn -> do
                stmt <- openStatement conn "SELECT $known"
                assertThrowsFormatError
                    (bindNamed stmt ["$missing" := (1 :: Int)])
                    (Text.isInfixOf "unknown named parameter" . formatErrorMessage)
                closeStatement stmt
        , testCase "numFieldsRemaining reports remaining columns" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE remaining (a INTEGER, b INTEGER, c INTEGER)"
                _ <- execute conn "INSERT INTO remaining VALUES (?, ?, ?)" (1 :: Int, 2 :: Int, 3 :: Int)
                rows <- query_ conn "SELECT a, b, c FROM remaining" :: IO [WithRemaining]
                assertEqual "remaining count" [WithRemaining 1 2] rows
        , testCase "RowParser alternatives fall back" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE yesno (answer TEXT)"
                _ <- executeMany conn "INSERT INTO yesno VALUES (?)" [Only ("yes" :: String), Only ("no" :: String)]
                rows <- query_ conn "SELECT answer FROM yesno ORDER BY answer" :: IO [YesNo]
                assertEqual "yes/no parsing" [YesNo False, YesNo True] rows
        ]

functionsTests :: TestTree
functionsTests =
    testGroup
        "user-defined functions"
        [ testCase "registers pure scalar function" $
            withConnection ":memory:" \conn -> do
                createFunction conn "hs_times_two" (\(x :: Int64) -> x * 2)
                result <- query_ conn "SELECT hs_times_two(21)" :: IO [Only Int64]
                assertEqual "times_two result" [Only 42] result
        , testCase "handles nullable arguments and results" $
            withConnection ":memory:" \conn -> do
                createFunction conn "hs_optional" (\(mx :: Maybe Int64) -> fmap (+ 1) mx)
                rows <-
                    query_ conn "SELECT hs_optional(x) FROM (VALUES (NULL), (41)) AS t(x)"
                        :: IO [Only (Maybe Int64)]
                assertEqual "optional results" [Only Nothing, Only (Just 42)] rows
        , testCase "supports IO-based functions" $
            withConnection ":memory:" \conn -> do
                ref <- newIORef (0 :: Int)
                createFunction conn "hs_counter" $ do
                    atomicModifyIORef' ref \n ->
                        let next = n + 1
                         in (next, next)
                first <- query_ conn "SELECT hs_counter()" :: IO [Only Int]
                second <- query_ conn "SELECT hs_counter()" :: IO [Only Int]
                assertEqual "first counter call" [Only 1] first
                assertEqual "second counter call" [Only 2] second
        , testCase "deleteFunction reports unsupported drop" $
            withConnection ":memory:" \conn -> do
                createFunction conn "hs_temp" (\(x :: Int64) -> x + 1)
                _ <- query_ conn "SELECT hs_temp(1)" :: IO [Only Int64]
                result <- try (deleteFunction conn "hs_temp")
                case result of
                    Left err ->
                        assertBool
                            "expected unsupported drop message"
                            (Text.isInfixOf "DuckDB does not allow dropping scalar functions" (sqlErrorMessage err))
                    Right () ->
                        assertFailure "expected deleteFunction to report unsupported operation"
        ]

transactionTests :: TestTree
transactionTests =
    testGroup
        "transactions"
        [ testCase "withTransaction commits on success" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE tx_commit (x INTEGER)"
                withTransaction conn $ do
                    _ <- execute conn "INSERT INTO tx_commit VALUES (?)" (Only (1 :: Int))
                    pure ()
                rows <- query_ conn "SELECT COUNT(*) FROM tx_commit" :: IO [Only Int]
                assertEqual "commit" [Only 1] rows
        , testCase "withTransaction rolls back on exception" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE tx_rollback (x INTEGER)"
                assertThrowsErrorCall $
                    withTransaction conn $ do
                        _ <- execute conn "INSERT INTO tx_rollback VALUES (?)" (Only (1 :: Int))
                        error "boom"
                rows <- query_ conn "SELECT COUNT(*) FROM tx_rollback" :: IO [Only Int]
                assertEqual "rollback" [Only 0] rows
        , testCase "withImmediateTransaction behaves like deferred" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE tx_immediate (x INTEGER)"
                withImmediateTransaction conn $ do
                    _ <- execute conn "INSERT INTO tx_immediate VALUES (?)" (Only (1 :: Int))
                    pure ()
                rows <- query_ conn "SELECT COUNT(*) FROM tx_immediate" :: IO [Only Int]
                assertEqual "immediate" [Only 1] rows
        , testCase "withExclusiveTransaction commits work" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE tx_exclusive (x INTEGER)"
                withExclusiveTransaction conn $ do
                    _ <- execute conn "INSERT INTO tx_exclusive VALUES (?)" (Only (1 :: Int))
                    pure ()
                rows <- query_ conn "SELECT COUNT(*) FROM tx_exclusive" :: IO [Only Int]
                assertEqual "exclusive" [Only 1] rows
        , testCase "withSavepoint reports unsupported feature" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE tx_savepoint (x INTEGER)"
                assertThrows
                    (withSavepoint conn "sp1" $ pure ())
                    (\err -> Text.isInfixOf "savepoints are not supported" (sqlErrorMessage err))
                rows <- query_ conn "SELECT COUNT(*) FROM tx_savepoint" :: IO [Only Int]
                assertEqual "savepoint fallback leaves table untouched" [Only 0] rows
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

assertThrowsFormatError :: IO a -> (FormatError -> Bool) -> Assertion
assertThrowsFormatError action predicate =
    assertThrows action predicate
