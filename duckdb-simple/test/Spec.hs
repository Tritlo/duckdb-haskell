{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

-- | Tasty-based test suite for duckdb-simple.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (ErrorCall, Exception, SomeException, displayException, fromException, try)
import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import Data.Array (Array, listArray)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Time.LocalTime (
    LocalTime (..),
    TimeOfDay (..),
    localTimeToUTC,
    minutesToTimeZone,
    timeOfDayToTime,
    utc,
 )
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Ratio ((%))
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (
    BigNum (..),
    BitString (..),
    bsFromBool,
    DecimalValue (..),
    Field (..),
    FieldValue (..),
    IntervalValue (..),
    TimeWithZone (..),
    fromBigNumBytes,
    returnError,
    toBigNumBytes,
 )
import Database.DuckDB.Simple.Ok (Ok (..))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===))
import Test.Tasty.ExpectedFailure (expectFailBecause)
import qualified Data.UUID as UUID
import Data.Maybe (fromJust)

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
        replicateM_ remaining (fieldWith (const (Ok ())))
        pure (WithRemaining firstVal remaining)

newtype YesNo = YesNo Bool
    deriving (Eq, Show)

instance FromRow YesNo where
    fromRow = parseYes <|> parseNo
      where
        parseYes = YesNo True <$ fieldWith (match (Text.pack "yes"))
        parseNo = YesNo False <$ fieldWith (match (Text.pack "no"))

        match expected fld@Field{} =
            case fromField fld :: Ok Text.Text of
                Errors err -> Errors err
                Ok txt ->
                    let normalized = Text.toLower txt
                     in if normalized == expected
                            then Ok ()
                            else returnError ConversionFailed fld "failed to match exact string"

newtype NonEmptyText = NonEmptyText Text.Text
    deriving (Eq, Show)

nonEmptyTextParser :: FieldParser NonEmptyText
nonEmptyTextParser f@Field{} =
    case fromField f of
        Errors err -> Errors err
        Ok txt
            | Text.null txt ->
                returnError ConversionFailed f "NonEmptyText requires a non-empty string"
            | otherwise -> Ok (NonEmptyText txt)

instance FromField NonEmptyText where
    fromField = nonEmptyTextParser
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "duckdb-simple"
        [ connectionTests
        , withConnectionTests
        , statementTests
        , typeTests
        , streamingTests
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
        , testCase "(:.) composes row parsing and parameter encoding" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE dot_pair (a INTEGER, b INTEGER, label TEXT)"
                let payload = Only (7 :: Int) :. (8 :: Int, Text.pack "hi")
                _ <- execute conn "INSERT INTO dot_pair VALUES (?, ?, ?)" payload
                rows <- query_ conn "SELECT a, b, label FROM dot_pair" :: IO [Only Int :. (Int, Text.Text)]
                case rows of
                    [Only a :. (b, label)] -> do
                        assertEqual "(:.) first" 7 a
                        assertEqual "(:.) second" 8 b
                        assertEqual "(:.) label" (Text.pack "hi") label
                    other -> assertFailure ("unexpected (:.) rows: " <> show other)
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
                    (Text.isInfixOf "column index" . sqlErrorMessage)
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
        , testCase "reports column metadata for statements" $
            withConnection ":memory:" \conn -> do
                stmt <- openStatement conn "SELECT 1 AS a, 2 AS b"
                count <- columnCount stmt
                assertEqual "column count" 2 count
                name0 <- columnName stmt 0
                name1 <- columnName stmt 1
                assertEqual "column names" [Text.pack "a", Text.pack "b"] [name0, name1]
                assertThrows
                    (columnName stmt 2)
                    (Text.isInfixOf "out of bounds" . sqlErrorMessage)
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

data DuckDBExpectation
    = ExpectEquals FieldValue
    | ExpectSatisfies (FieldValue -> Assertion)
    | ExpectException (SomeException -> Assertion)

data DuckDBExpression
    = CastExpression Text.Text Text.Text
    | DirectExpression Text.Text

data DuckDBCastCase = DuckDBCastCase
    { castLabel :: String
    , castExpression :: DuckDBExpression
    , castExpectation :: DuckDBExpectation
    , castExpectFailureReason :: Maybe String
    }

duckdbTypeCastTests :: TestTree
duckdbTypeCastTests =
    testGroup
        "duckdb type casts"
        (map buildCase duckdbCastCases)
  where
    buildCase DuckDBCastCase{castLabel, castExpression, castExpectation, castExpectFailureReason} =
        let base =
                testCase castLabel $
                    withConnection ":memory:" \conn -> do
                        let sql =
                                Query
                                    (case castExpression of
                                        CastExpression expressionValue expressionType ->
                                            Text.concat ["SELECT CAST(", expressionValue, " AS ", expressionType, ")"]
                                        DirectExpression expressionSQL ->
                                            Text.concat ["SELECT ", expressionSQL]
                                    )
                        result <- try (query_ conn sql) :: IO (Either SomeException [Only FieldValue])
                        runExpectation castLabel castExpectation result
                        maybe (pure ()) assertFailure castExpectFailureReason
         in maybe base (`expectFailBecause` base) castExpectFailureReason

runExpectation :: String -> DuckDBExpectation -> Either SomeException [Only FieldValue] -> Assertion
runExpectation label expectation outcome =
    case expectation of
        ExpectEquals expected ->
            case outcome of
                Right [Only fieldValue] ->
                    assertEqual (label <> " result mismatch") expected fieldValue
                Right other ->
                    assertFailure (label <> " expected single row, got " <> show other)
                Left err ->
                    assertFailure (label <> " expected success, but query failed: " <> displayException err)
        ExpectSatisfies predicate ->
            case outcome of
                Right [Only fieldValue] ->
                    predicate fieldValue
                Right other ->
                    assertFailure (label <> " expected single row, got " <> show other)
                Left err ->
                    assertFailure (label <> " expected success, but query failed: " <> displayException err)
        ExpectException predicate ->
            case outcome of
                Left err ->
                    predicate err
                Right rows ->
                    assertFailure (label <> " expected failure, but query succeeded with " <> show rows)

duckdbCastCases :: [DuckDBCastCase]
duckdbCastCases =
    [ successCase "BOOLEAN" (quoted "true") "BOOLEAN" (ExpectEquals (FieldBool True))
    , successCase "TINYINT" (quotedValue tinyIntValue) "TINYINT" (ExpectEquals (FieldInt8 tinyIntValue))
    , successCase "SMALLINT" (quotedValue smallIntValue) "SMALLINT" (ExpectEquals (FieldInt16 smallIntValue))
    , successCase "INTEGER" (quotedValue intValue) "INTEGER" (ExpectEquals (FieldInt32 intValue))
    , successCase "BIGINT" (quotedValue bigIntValue) "BIGINT" (ExpectEquals (FieldInt64 bigIntValue))
    , successCase "UTINYINT" (quotedValue uTinyValue) "UTINYINT" (ExpectEquals (FieldWord8 uTinyValue))
    , successCase "USMALLINT" (quotedValue uSmallValue) "USMALLINT" (ExpectEquals (FieldWord16 uSmallValue))
    , successCase "UINTEGER" (quotedValue uIntValue) "UINTEGER" (ExpectEquals (FieldWord32 uIntValue))
    , successCase "UBIGINT" (quotedValue uBigValue) "UBIGINT" (ExpectEquals (FieldWord64 uBigValue))
    , successCase "FLOAT" (quoted "3.25") "FLOAT" (ExpectEquals (FieldFloat floatValue))
    , successCase "DOUBLE" (quoted "2.5") "DOUBLE" (ExpectEquals (FieldDouble doubleValue))
    , successCase "TIMESTAMP" (quoted "2024-01-02 03:04:05.123456") "TIMESTAMP" (ExpectEquals (FieldTimestamp timestampLocalTimeMicros))
    , successCase "DATE" (quoted "2024-10-12") "DATE" (ExpectEquals (FieldDate dateValue))
    , successCase "TIME" (quoted "14:30:15.123456") "TIME" (ExpectEquals (FieldTime timeValue))
    , successCase "INTERVAL" (quoted "1 day") "INTERVAL" (ExpectEquals (FieldInterval intervalValue))
    , successCase "HUGEINT" (quotedValue hugeIntLiteral) "HUGEINT" (ExpectEquals (FieldHugeInt hugeIntLiteral))
    , successCase "UHUGEINT" (quotedValue uHugeIntLiteral) "UHUGEINT" (ExpectEquals (FieldUHugeInt uHugeIntLiteral))
    , successCase "VARCHAR" (quoted varcharText) "VARCHAR" (ExpectEquals (FieldText varcharText))
    , successCase "BLOB" (quoted "duckdb") "BLOB" (ExpectEquals (FieldBlob blobValue))
    , successCase "DECIMAL" (quoted "12345.6789") "DECIMAL(18,4)" (ExpectEquals (FieldDecimal decimalValue))
    , successCase "TIMESTAMP_S" (quoted "2024-01-02 03:04:05") "TIMESTAMP_S" (ExpectEquals (FieldTimestamp timestampLocalTimeSeconds))
    , successCase "TIMESTAMP_MS" (quoted "2024-01-02 03:04:05.123") "TIMESTAMP_MS" (ExpectEquals (FieldTimestamp timestampLocalTimeMillis))
    , successCase "TIMESTAMP_NS" (quoted "2024-01-02 03:04:05.123456789") "TIMESTAMP_NS" (ExpectEquals (FieldTimestamp timestampLocalTimeNanos))
    , successCase "ENUM" (quoted "beta") "ENUM('alpha','beta')" (ExpectEquals (FieldEnum 1))
    , successCase "LIST" (quoted "[1,2,3]") "INTEGER[]" (ExpectEquals (FieldList listElements))
    , successDirect "MAP" "MAP(['a','b'], [1,2])" (expectMapEntries mapPairs)
    , successCase "TIMETZ" (quoted "03:04:05+02:30") "TIME WITH TIME ZONE" (ExpectEquals (FieldTimeTZ timeWithZoneValue))
    , successCase "TIMESTAMPTZ" (quoted "2024-01-02 03:04:05+02:30") "TIMESTAMP WITH TIME ZONE" (ExpectEquals (FieldTimestampTZ timestampTzUtc))
    , successCase "BIGNUM" (quoted bigNumLiteralText) "BIGNUM" (ExpectEquals (FieldBigNum (BigNum bigNumLiteral)))
    , successCase "UUID" (quoted $ UUID.toText uuid) "UUID" (ExpectEquals (FieldUUID uuid))
    , successCase "BIT" (quoted $ Text.pack $ show bits) "BIT" (ExpectEquals (FieldBit bits))
    , successCase "ARRAY" (quoted "[1,2,3]") "INTEGER[3]" (ExpectEquals (FieldArray arrayElements))
     -- This one is broken upstream, instead of a DuckDBTypeTimeNs, we get a DuckDBType 0
     , failCaseWith "TIME_NS" (quoted "03:04:05.123456789") "TIME_NS" "TIME_NS decoding unsupported" (expectErrorCallContaining "INVALID")
      -- not implemented yet
    , failCaseWith "STRUCT" (quoted "{\"a\":1,\"b\":2}") "STRUCT(a INTEGER, b INTEGER)" "STRUCT decoding unsupported" (expectErrorCallContaining "STRUCT columns are not supported")
    , failCaseWith "UNION" (quoted "1") "UNION(\"value\" INTEGER)" "UNION casts unsupported" (expectSQLErrorContaining "UNION")
    -- These can never surface from a query, only internally.
    , failCaseOK "ANY" (quoted "1") "ANY" (expectSQLErrorContaining "ANY")
    , failCaseOK "STRING_LITERAL" (quoted literalText) "STRING_LITERAL" (expectSQLErrorContaining "STRING_LITERAL")
    , failCaseOK "INTEGER_LITERAL" (quoted "123") "INTEGER_LITERAL" (expectSQLErrorContaining "INTEGER_LITERAL")
    , failCaseOK "INVALID" "0" "INVALID" (expectSQLErrorContaining "INVALID")
    , failCaseOK "SQLNULL" "NULL" "SQLNULL" (expectSQLErrorContaining "SQLNULL")
    ]
  where
    bits = BitString 7 $ BS.pack [1, 255]
    quoted t = Text.concat ["'", t, "'"]
    quotedValue v = quoted (valueText v)
    valueText :: (Show a) => a -> Text.Text
    valueText = Text.pack . show
    successCase label value ty expectation =
        DuckDBCastCase
            { castLabel = label
            , castExpression = CastExpression value ty
            , castExpectation = expectation
            , castExpectFailureReason = Nothing
            }
    successDirect label expr expectation =
        DuckDBCastCase
            { castLabel = label
            , castExpression = DirectExpression expr
            , castExpectation = expectation
            , castExpectFailureReason = Nothing
            }
    failCaseWith label value ty reason expectation =
        DuckDBCastCase
            { castLabel = label
            , castExpression = CastExpression value ty
            , castExpectation = expectation
            , castExpectFailureReason = Just reason
            }
    failCaseOK label value ty expectation =
        DuckDBCastCase
            { castLabel = label
            , castExpression = CastExpression value ty
            , castExpectation = expectation
            , castExpectFailureReason = Nothing
            }
    expectMapEntries expectedPairs =
        ExpectSatisfies $
           \case
             FieldMap actualPairs ->
                 let normalize = sortOn (mapKey . fst)
                     mapKey (FieldText txt) = txt
                     mapKey other = Text.pack (show other)
                  in assertEqual "map entries" (normalize expectedPairs) (normalize actualPairs)
             other ->
                 assertFailure ("expected FieldMap, but saw " <> show other)
    expectSQLErrorContaining :: Text.Text -> DuckDBExpectation
    expectSQLErrorContaining needle =
        ExpectException $ \err ->
            case fromException err :: Maybe SQLError of
                Just sqlErr ->
                    let message = sqlErrorMessage sqlErr
                        in assertBool
                            ("expected SQLError containing " <> Text.unpack needle <> ", but saw: " <> Text.unpack message)
                            (Text.isInfixOf needle message)
                Nothing ->
                    assertFailure ("expected SQLError, but saw " <> displayException err)
    expectErrorCallContaining :: Text.Text -> DuckDBExpectation
    expectErrorCallContaining needle =
        ExpectException $ \err ->
            case fromException err :: Maybe ErrorCall of
                Just callErr ->
                    let message = Text.pack (displayException callErr)
                        in assertBool
                            ("expected ErrorCall containing " <> Text.unpack needle <> ", but saw: " <> Text.unpack message)
                            (Text.isInfixOf needle message)
                Nothing ->
                    assertFailure ("expected ErrorCall, but saw " <> displayException err)
    bsFromString :: String -> BS.ByteString
    bsFromString = BS.pack . map (fromIntegral . fromEnum)
    secondsWithFraction :: Integer -> Integer -> Integer -> Rational
    secondsWithFraction whole numerator denominator =
        fromIntegral whole + numerator % denominator
    tinyIntValue :: Int8
    tinyIntValue = 42
    smallIntValue :: Int16
    smallIntValue = 32000
    intValue :: Int32
    intValue = 2147483647
    bigIntValue :: Int64
    bigIntValue = 9223372036854775807
    uTinyValue :: Word8
    uTinyValue = 200
    uSmallValue :: Word16
    uSmallValue = 60000
    uIntValue :: Word32
    uIntValue = maxBound
    uBigValue :: Word64
    uBigValue = maxBound
    floatValue :: Float
    floatValue = 3.25
    doubleValue :: Double
    doubleValue = 2.5
    timestampDay = fromGregorian 2024 1 2
    timestampLocalTimeMicros =
        LocalTime timestampDay (TimeOfDay 3 4 (fromRational (secondsWithFraction 5 123456 1000000)))
    timestampLocalTimeSeconds =
        LocalTime timestampDay (TimeOfDay 3 4 5)
    timestampLocalTimeMillis =
        LocalTime timestampDay (TimeOfDay 3 4 (fromRational (secondsWithFraction 5 123 1000)))
    timestampLocalTimeNanos =
        LocalTime timestampDay (TimeOfDay 3 4 (fromRational (secondsWithFraction 5 123456789 1000000000)))
    dateValue = fromGregorian 2024 10 12
    timeValue =
        TimeOfDay 14 30 (fromRational (secondsWithFraction 15 123456 1000000))
    intervalValue =
        IntervalValue{intervalMonths = 0, intervalDays = 1, intervalMicros = 0}
    decimalValue =
        DecimalValue{decimalWidth = 18, decimalScale = 4, decimalInteger = 123456789}
    listElements =
        [FieldInt32 1, FieldInt32 2, FieldInt32 3]
    arrayElements =
        listArray (0, 2) listElements
    mapPairs =
        [ (FieldText (Text.pack "a"), FieldInt32 1)
        , (FieldText (Text.pack "b"), FieldInt32 2)
        ]
    uuid :: UUID.UUID
    uuid = fromJust $ UUID.fromText "123e4567-e89b-12d3-a456-426614174000"
    timeWithZoneValue =
        TimeWithZone{timeWithZoneTime = TimeOfDay 3 4 5, timeWithZoneZone = minutesToTimeZone 150}
    timestampTzUtc =
        UTCTime timestampDay (secondsToDiffTime 2045)
    bigNumLiteral :: Integer
    bigNumLiteral = 1234567890123456789012345678901234567890
    bigNumLiteralText = valueText bigNumLiteral
    varcharText :: Text.Text
    varcharText = "Hello, DuckDB"
    blobValue = bsFromString "duckdb"
    hugeIntLiteral :: Integer
    hugeIntLiteral = (2 ^ (120 :: Int)) + 123456789
    uHugeIntLiteral :: Integer
    uHugeIntLiteral = (2 ^ (128 :: Int)) - 1
    literalText :: Text.Text
    literalText = "literal"

typeTests :: TestTree
typeTests =
    testGroup
        "types"
        [ duckdbTypeCastTests
        , testCase "round-trips date/time/timestamp" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE temporals (d DATE, t TIME, ts TIMESTAMP)"
                let dayVal = fromGregorian 2024 10 12
                    timeVal = TimeOfDay 14 30 15.123456
                    tsVal = LocalTime dayVal timeVal
                _ <- execute conn "INSERT INTO temporals VALUES (?, ?, ?)" (dayVal, timeVal, tsVal)
                [(dRes, tRes, tsRes)] <- query_ conn "SELECT d, t, ts FROM temporals"
                assertEqual "date round-trip" dayVal dRes
                assertEqual "time round-trip" timeVal tRes
                assertEqual "timestamp round-trip" tsVal tsRes
                [Only utcRes] <- query_ conn "SELECT ts FROM temporals" :: IO [Only UTCTime]
                assertEqual "timestamp as UTC" (localTimeToUTC utc tsVal) utcRes
        , testCase "round-trips blob payloads" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE blobs (payload BLOB)"
                let payload = BS.pack [0, 1, 2, 3, 255]
                _ <- execute conn "INSERT INTO blobs VALUES (?)" (Only payload)
                [Only blobOut] <- query_ conn "SELECT payload FROM blobs"
                assertEqual "blob round-trip" payload blobOut
        , testCase "round-trips unsigned integers" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE unsigneds (u8 UTINYINT, u16 USMALLINT, u32 UINTEGER, u64 UBIGINT)"
                let w8 = 200 :: Word8
                    w16 = 60000 :: Word16
                    w32 = 4000000000 :: Word32
                    w64 = maxBound :: Word64
                _ <- execute conn "INSERT INTO unsigneds VALUES (?, ?, ?, ?)" (w8, w16, w32, w64)
                [(r8, r16, r32, r64)] <- query_ conn "SELECT u8, u16, u32, u64 FROM unsigneds"
                assertEqual "Word8 round-trip" w8 r8
                assertEqual "Word16 round-trip" w16 r16
                assertEqual "Word32 round-trip" w32 r32
                assertEqual "Word64 round-trip" w64 r64
                [Only (asWord :: Word)] <- query_ conn "SELECT u64 FROM unsigneds"
                assertEqual "Word from UBIGINT" (fromIntegral w64) asWord
        , testCase "round-trips fixed-length arrays" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE arrays (vals INTEGER[3])"
                let inputs =
                        [ listArray (0, 2) [1, 2, 3]
                        , listArray (0, 2) [4, 5, 6]
                        ]
                _ <- executeMany conn "INSERT INTO arrays VALUES (?)" (Only <$> inputs)
                rows <- query_ conn "SELECT vals FROM arrays ORDER BY rowid" :: IO [Only (Array Int Int)]
                let expected =
                        fmap Only inputs
                assertEqual "array round-trip" expected rows
        , testCase "decodes huge integers as Integer" $
            withConnection ":memory:" \conn -> do
                let hugeValue = 170141183460469231731687303715884105727 :: Integer
                [Only (hugeOut :: Integer)] <-
                    query_ conn "SELECT 170141183460469231731687303715884105727::HUGEINT"
                assertEqual "hugeint" hugeValue hugeOut
        , testCase "decodes huge integers as Natural" $
            withConnection ":memory:" \conn -> do
                let hugeValue = 170141183460469231731687303715884105727 :: Natural
                [Only (naturalOut :: Natural)] <-
                    query_ conn "SELECT 170141183460469231731687303715884105727::HUGEINT"
                assertEqual "hugeint natural" hugeValue naturalOut
        , testCase "decodes unsigned huge integers as Natural" $
            withConnection ":memory:" \conn -> do
                let uhValue = 170141183460469231731687303715884105727 :: Natural
                [Only (naturalOut :: Natural)] <-
                    query_ conn "SELECT 170141183460469231731687303715884105727::UHUGEINT"
                assertEqual "uhugeint natural" uhValue naturalOut
        , testCase "FromField Integer consumes FieldBigNum" $ do
            let original = 1234567899876543210123456789 :: Integer
                bigField =
                    Field
                        { fieldName = "big"
                        , fieldIndex = 0
                        , fieldValue = FieldBigNum (BigNum original)
                        }
            case (fromField bigField :: Ok Integer) of
                Ok result -> assertEqual "FieldBigNum Integer" original result
                Errors err -> assertFailure ("unexpected parse failure: " <> show err)
        , testCase "FromField Natural rejects negative FieldBigNum" $ do
            let bigValue = BigNum (-5)
                bigField =
                    Field
                        { fieldName = "big"
                        , fieldIndex = 0
                        , fieldValue = FieldBigNum bigValue
                        }
            case (fromField bigField :: Ok Natural) of
                Errors _ -> pure ()
                Ok result -> assertFailure ("expected failure, got " <> show result)
        , testCase "BigNum conversions round-trip Integer" $ do
            let original = (2 ^ (200 :: Int)) + 8675309 :: Integer
                converted = toBigNumBytes original
            assertEqual "round-trip Integer" original (fromBigNumBytes converted)
        , testProperty "fromBigNumBytes . toBigNumBytes is identity" \(n :: Integer) ->
            fromBigNumBytes (toBigNumBytes n) === n
        , testCase "selects BIGNUM values as BigNum" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE bignums (val BIGNUM)"
                let bigValues :: [Integer]
                    bigValues =
                        [ (2 ^ (200 :: Int)) + 8675309
                        , 0
                        , negate ((2 ^ (180 :: Int)) + 12345)
                        ]
                rows <- concat <$> mapM (\bv -> query_ conn $ fromString $ "SELECT CAST('" <> show bv <> "' AS BIGNUM)") bigValues
                --  query_ conn "SELECT val FROM bignums ORDER BY rowid" :: IO [Only BigNum]
                assertEqual "BIGNUM results" (fmap (Only . BigNum) bigValues) rows
        , testCase "round-trips BIGNUM values through the database" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE bignum_roundtrip (val BIGNUM)"
                let ints =
                        [ (2 ^ (200 :: Int)) + 8675309
                        , 0
                        , negate ((2 ^ (180 :: Int)) + 12345)
                        ]
                _ <- executeMany conn "INSERT INTO bignum_roundtrip VALUES (?)" (fmap (Only . BigNum) ints)
                rows <- query_ conn "SELECT val FROM bignum_roundtrip ORDER BY rowid" :: IO [Only Integer]
                assertEqual "BIGNUM round-trip" (fmap Only ints) rows
        , testCase "duckdbColumnType reports BIGNUM for Integer" $
            assertEqual
                "duckdbColumnType Integer"
                (Text.pack "BIGNUM")
                (duckdbColumnType (Proxy :: Proxy Integer))
        , testCase "duckdbColumnType preserves Maybe parameter" $
            assertEqual
                "duckdbColumnType Maybe Word16"
                (Text.pack "USMALLINT")
                (duckdbColumnType (Proxy :: Proxy (Maybe Word16)))
        , testCase "decodes interval components" $
            withConnection ":memory:" \conn -> do
                [Only intervalVal] <-
                    query_ conn "SELECT INTERVAL '2 months 3 days 04:05:06.007008'"
                let IntervalValue{intervalMonths, intervalDays, intervalMicros} = intervalVal
                    expectedMicros =
                        (((4 * 60 + 5) * 60) + 6) * 1000000 + 7008
                assertEqual "interval months" 2 intervalMonths
                assertEqual "interval days" 3 intervalDays
                assertEqual "interval micros" expectedMicros intervalMicros
        , testCase "decodes decimals via Double conversion" $
            withConnection ":memory:" \conn -> do
                [Only decimalAsDouble] <-
                    (query_ conn "SELECT CAST(12345.6789 AS DECIMAL(18,4))" :: IO [Only Double])
                let diff = abs (decimalAsDouble - 12345.6789)
                assertBool
                    ("decimal as double: " <> show diff)
                    (diff < 1e-4)
        , testCase "decodes time with time zone" $
            withConnection ":memory:" \conn -> do
                [Only tzVal] <- query_ conn "SELECT TIMETZ '14:30:15+02:30'"
                let expectedTime = TimeOfDay 14 30 15
                    expectedZone = minutesToTimeZone 150
                assertEqual "timetz time" expectedTime (timeWithZoneTime tzVal)
                assertEqual "timetz zone" expectedZone (timeWithZoneZone tzVal)
        , testCase "decodes list columns into Haskell lists" $
            withConnection ":memory:" \conn -> do
                [Only (listOut :: [Int])] <-
                    query_ conn "SELECT LIST_VALUE(1, 2, 3)::INTEGER[]"
                assertEqual "list decode" [1, 2, 3] listOut
        , testCase "decodes map columns into strict Map" $
            withConnection ":memory:" \conn -> do
                [Only (mapOut :: Map.Map Text.Text Int)] <-
                    query_ conn "SELECT MAP(['a', 'b']::TEXT[], [1, 2]::INTEGER[])"
                let expected = Map.fromList [(Text.pack "a", 1), (Text.pack "b", 2)]
                assertEqual "map decode" expected mapOut
        , testCase "decodes timestamp with time zone as UTC" $
            withConnection ":memory:" \conn -> do
                [Only utcVal] <-
                    (query_ conn "SELECT TIMESTAMPTZ '2024-10-12 14:30:15+02:30'" :: IO [Only UTCTime])
                let expected =
                        UTCTime
                            (fromGregorian 2024 10 12)
                            (timeOfDayToTime (TimeOfDay 12 0 15))
                assertEqual "timestamptz utc" expected utcVal
        , testCase "custom FieldParser enforces invariants" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE nonempty (name TEXT)"
                _ <- execute conn "INSERT INTO nonempty VALUES (?)" (Only (Text.pack "okay"))
                rows <- query_ conn "SELECT name FROM nonempty" :: IO [Only NonEmptyText]
                assertEqual "non-empty success" [Only (NonEmptyText (Text.pack "okay"))] rows
                _ <- execute conn "INSERT INTO nonempty VALUES (?)" (Only (Text.pack ""))
                assertThrows
                    (query_ conn "SELECT name FROM nonempty" :: IO [Only NonEmptyText])
                    (Text.isInfixOf "NonEmptyText requires a non-empty string" . sqlErrorMessage)
        , testCase "round-trips uuid payloads" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE uuids (uuid UUID)"
                let uuidTexts =
                        [ "123e4567-e89b-12d3-a456-426614174000"
                        , "923e4567-e89b-12d3-a456-426614174000"
                        , "ffffffff-ffff-ffff-ffff-ffffffffffff"
                        ]
                    maybeUUIDs = traverse UUID.fromText uuidTexts
                uuids <-
                    case maybeUUIDs of
                        Nothing -> assertFailure "invalid UUID literal in test data" >> pure []
                        Just parsed -> pure parsed
                _ <- executeMany conn "INSERT INTO uuids VALUES (?)" (fmap Only uuids)
                rows <- query_ conn "SELECT uuid FROM uuids ORDER BY rowid" :: IO [Only UUID.UUID]
                assertEqual "uuid round-trip" (fmap Only uuids) rows
        , testCase "round-trips bitstring payloads" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE bits (bit BIT)"
                let bitValues =
                        [ bsFromBool [True]
                        , bsFromBool [False, False, False, True, True, True, False, True, False]
                        , bsFromBool [False, False, False, False, True, True, True, False, True, False]
                        , bsFromBool [True, False, False, False, True, True, True, False, True, False]
                        ]
                _ <- executeMany conn "INSERT INTO bits VALUES (?)" (fmap Only bitValues)
                rows <- query_ conn "SELECT bit FROM bits ORDER BY rowid" :: IO [Only BitString]
                assertEqual "bit round-trip" (fmap Only bitValues) rows
        ]

streamingTests :: TestTree
streamingTests =
    testGroup
        "streaming"
        [ testCase "fold_ accumulates large result sets" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE stream_fold (n INTEGER)"
                let rows = fmap Only [1 .. 1000 :: Int]
                _ <- executeMany conn "INSERT INTO stream_fold VALUES (?)" rows
                total <- fold_ conn "SELECT n FROM stream_fold ORDER BY n" 0 \acc (Only n) -> pure (acc + n)
                assertEqual "folded sum" (sum ([1 .. 1000] :: [Int])) total
        , testCase "fold and foldNamed respect parameters" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE stream_filter (n INTEGER)"
                let values = fmap Only [1 .. 50 :: Int]
                _ <- executeMany conn "INSERT INTO stream_filter VALUES (?)" values
                gtTotal <-
                    fold conn "SELECT n FROM stream_filter WHERE n > ?" (Only (40 :: Int)) 0 $
                        \acc (Only n) -> pure (acc + n)
                let expected = sum ([41 .. 50] :: [Int])
                assertEqual "filtered fold sum" expected gtTotal
                leCount <-
                    foldNamed conn "SELECT n FROM stream_filter WHERE n <= $limit" ["$limit" := (10 :: Int)] 0 $
                        \acc (Only (_ :: Int)) -> pure (acc + 1)
                assertEqual "foldNamed count" (10 :: Int) leCount
        , testCase "nextRow streams rows sequentially" $
            withConnection ":memory:" \conn -> do
                _ <- execute_ conn "CREATE TABLE stream_cursor (n INTEGER)"
                _ <- executeMany conn "INSERT INTO stream_cursor VALUES (?)" (fmap Only [1 .. 3 :: Int])
                withStatement conn "SELECT n FROM stream_cursor ORDER BY n" \stmt -> do
                    first <- nextRow stmt
                    second <- nextRow stmt
                    third <- nextRow stmt
                    done <- nextRow stmt
                    assertEqual
                        "cursor iteration"
                        [Just (Only (1 :: Int)), Just (Only 2), Just (Only 3), Nothing]
                        [first, second, third, done]
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
                    query_ conn "SELECT hs_optional(x) FROM (VALUES (NULL), (41)) AS t(x)" ::
                        IO [Only (Maybe Int64)]
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
assertThrowsFormatError = assertThrows
