{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Main
-- Description : Tasty-based test suite for duckdb-simple.
module Main (main) where

import ArrowTests (arrowTests)
import CancellationTests (callbackLockTests, cancellationTests)
import Control.Applicative ((<|>))
import Control.Exception (ErrorCall, Exception, SomeException, bracket, displayException, fromException, throwIO, try)
import Control.Monad (forM_, replicateM_, void, when)
import CopyTests (copyTests)
import CoreRegressionTests (coreRegressionTests)
import Data.Array (Array, elems, listArray)
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Time.LocalTime (
  LocalTime (..),
  TimeOfDay (..),
  localTimeToUTC,
  timeOfDayToTime,
  utc,
 )
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Catalog as Catalog
import qualified Database.DuckDB.Simple.Config as Config
import qualified Database.DuckDB.Simple.Copy as Copy
import qualified Database.DuckDB.Simple.FileSystem as FileSystem
import Database.DuckDB.Simple.FromField (
  BigNum (..),
  BitString (..),
  DecimalValue (..),
  Field (..),
  FieldValue (..),
  IntervalValue (..),
  TimeWithZone (..),
  bsFromBool,
  fromBigNumBytes,
  returnError,
  toBigNumBytes,
 )
import Database.DuckDB.Simple.Generic (
  DuckValue (..),
  ViaDuckDB (..),
  genericFromFieldValue,
  genericLogicalType,
  genericToFieldValue,
  genericToStructValue,
 )
import qualified Database.DuckDB.Simple.Logging as Logging
import Database.DuckDB.Simple.LogicalRep (
  StructField (..),
  StructValue (..),
  UnionMemberType (..),
  UnionValue (..),
 )
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.Time (Unbounded (..))
import Database.DuckDB.Simple.ToField (ToDuckValue (..))
import Database.DuckDB.Simple.Variant (Variant)
import ExtensionRegressionTests (extensionRegressionTests)
import GHC.Generics (Generic)
import qualified GeometryRegressionTests
import Numeric.Natural (Natural)
import Properties (roundTripTests)
import RegistrationTests (registrationTests)
import qualified SqlFeatureTests
import StreamingTests (nativeStreamingTests)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, Timeout (..), adjustOption, defaultMain, mkTimeout, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===))
import TimeTests (timeTests)
import TypedRegistrationTests (typedRegistrationTests)
import UserCastTests (geometryArgumentTests, userCastTests)
import ValueRegressionTests (valueRegressionTests)
import qualified VariantRegressionTests

data Person = Person
  { personId :: Int
  , personName :: Text.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow)

data WithRemaining = WithRemaining Int Int
  deriving (Eq, Show)

data GenericRecord = GenericRecord
  { grId :: Int
  , grName :: Text.Text
  }
  deriving stock (Eq, Show, Generic)

data GenericSum
  = SumInt Int
  | SumBool Bool
  | SumPair Int Text.Text
  deriving stock (Eq, Show, Generic)

data GenericEnum
  = EnumRed
  | EnumBlue
  deriving stock (Eq, Show, Generic)

data ViaUser = ViaUser
  { vuId :: Int64
  , vuName :: Text.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToDuckValue, ToField, FromField) via (ViaDuckDB ViaUser)

-- | A newtype with its own type text, for a scalar function argument.
newtype Price = Price DecimalValue

instance DuckDBColumnType Price where
  duckdbColumnTypeFor _ = "DECIMAL(18,3)"

instance FromField Price where
  fromField value = Price <$> fromField value

-- | A newtype with its own type text, for a scalar function result.
newtype Amount = Amount Double

instance DuckDBColumnType Amount where
  duckdbColumnTypeFor _ = "DECIMAL(18,3)"

instance ToDuckValue Amount where
  toDuckValue factory (Amount value) = toDuckValue factory value

data ViaShape
  = ViaCircle Double
  | ViaSquare Double
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToDuckValue, ToField, FromField) via (ViaDuckDB ViaShape)

-- Edge case test types
data TwoFieldStruct = TwoFieldStruct Int Text.Text
  deriving stock (Eq, Show, Generic)

data LargeSumType
  = LargeC1 Int
  | LargeC2 Int
  | LargeC3 Int
  | LargeC4 Int
  | LargeC5 Int
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB LargeSumType)

data EmptyStruct = EmptyStruct
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB EmptyStruct)

-- | A field of the type EmptyStruct uses the generic encoding.
instance DuckValue EmptyStruct where
  duckToField = genericToFieldValue
  duckFromField = genericFromFieldValue
  duckLogicalType = genericLogicalType

-- | A record with two optional fields of a type without fields.
data OptionalEmpty = OptionalEmpty
  { oeFirst :: Maybe EmptyStruct
  , oeSecond :: Maybe EmptyStruct
  }
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB OptionalEmpty)

-- | A record with a field of a type without fields.
data WithEmpty = WithEmpty
  { weId :: Int64
  , weEmpty :: EmptyStruct
  }
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB WithEmpty)

-- | A sum with a constructor that has a field and a constructor without fields.
data Tally
  = Counted Int64
  | Uncounted
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB Tally)

data OptionalFields = OptionalFields
  { ofInt :: Maybe Int
  , ofText :: Maybe Text.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB OptionalFields)

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
            in  if normalized == expected
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

-- | Run each case with a time limit of 10 minutes. Thus a deadlock fails its
-- case, and not the job. The limit is long, because the suite also runs under
-- Valgrind. The limit cannot stop a thread in a native call. A time limit of
-- the command line (@--timeout@) replaces this limit.
main :: IO ()
main = defaultMain (adjustOption limit tests)
  where
    limit NoTimeout = mkTimeout (10 * 60 * 1000000)
    limit given = given

tests :: TestTree
tests =
  testGroup
    "duckdb-simple"
    [ connectionTests
    , coreRegressionTests
    , arrowTests
    , cancellationTests
    , callbackLockTests
    , nativeStreamingTests
    , extensionRegressionTests
    , registrationTests
    , typedRegistrationTests
    , copyTests
    , GeometryRegressionTests.tests
    , VariantRegressionTests.tests
    , userCastTests
    , geometryArgumentTests
    , valueRegressionTests
    , timeTests
    , SqlFeatureTests.tests
    , withConnectionTests
    , statementTests
    , roundTripTests
    , typeTests
    , streamingTests
    , functionsTests
    , v20Tests
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
    , testCase "opens with startup config" $ do
        conn <- openWithConfig ":memory:" [("threads", "1")]
        mValue <- Config.getConfigOption conn "threads"
        close conn
        case mValue of
          Nothing -> assertFailure "expected threads config value"
          Just value -> Config.configValueText value @?= Just "1"
    ]

v20Tests :: TestTree
v20Tests =
  testGroup
    "DuckDB 2.0 wrappers"
    [ testCase "lists config flags" $ do
        flags <- withConnection ":memory:" Config.listConfigFlags
        assertBool "expected at least one config flag" (not (null flags))
        assertBool "expected threads flag" (any ((== "threads") . Config.configFlagName) flags)
    , testCase "catalog lookup works inside a transaction" $
        withConnection ":memory:" \conn -> do
          _ <- execute_ conn "CREATE TABLE catalog_probe(id INTEGER)"
          withTransaction conn $ do
            mType <- Catalog.catalogTypeName conn "memory"
            assertBool "expected catalog type name" (mType /= Nothing)
            mEntry <- Catalog.lookupCatalogEntry conn "memory" "main" "catalog_probe" Catalog.CatalogTable
            case mEntry of
              Nothing -> assertFailure "expected catalog entry"
              Just entry -> Catalog.catalogEntryName entry @?= "catalog_probe"
    , testCase "file system wrappers round-trip bytes" $ withTemporaryPath \path -> do
        let payload = BS.pack [0, 1, 2, 3, 255]
        withConnection ":memory:" \conn -> do
          FileSystem.withFileHandle conn path [FileSystem.FileFlagCreate, FileSystem.FileFlagWrite] \handle -> do
            written <- FileSystem.writeFileHandleBytes handle payload
            written @?= fromIntegral (BS.length payload)
            FileSystem.fileHandleSync handle
          FileSystem.withFileHandle conn path [FileSystem.FileFlagRead] \handle -> do
            size <- FileSystem.fileHandleSize handle
            size @?= fromIntegral (BS.length payload)
            bytes <- FileSystem.readFileHandleChunk handle 32
            bytes @?= payload
        removeFile path
    , testCase "copy wrappers receive COPY TO rows" $
        withTemporaryPath \path -> withConnection ":memory:" \conn -> do
          capturedRef <- newIORef ([] :: [Int])
          pathRef <- newIORef Nothing
          Copy.registerCopyToFunction
            conn
            "hs_capture"
            (\bindInfo -> pure (length (Copy.copyBindColumnTypes bindInfo)))
            ( \initInfo -> do
                atomicModifyIORef' pathRef \_ -> (Just (Copy.copyInitFilePath initInfo), ())
                pure (Copy.copyInitBindState initInfo)
            )
            ( \sinkInfo rows -> do
                Copy.copySinkBindState sinkInfo @?= 1
                Copy.copySinkGlobalState sinkInfo @?= 1
                values <- mapM fieldInt rows
                atomicModifyIORef' capturedRef \existing -> (existing <> values, ())
            )
            ( \finalizeInfo -> do
                Copy.copyFinalizeBindState finalizeInfo @?= 1
                Copy.copyFinalizeGlobalState finalizeInfo @?= 1
            )
          _ <- execute conn "COPY (SELECT * FROM (VALUES (1), (2), (3)) AS t(x)) TO ? (FORMAT hs_capture)" (Only (Text.pack path))
          captured <- readIORef capturedRef
          filePath <- readIORef pathRef
          captured @?= [1, 2, 3]
          filePath @?= Just path
    , testCase "custom log storage registers a native sink" do
        entries <- newIORef []
        withConnection ":memory:" \conn -> do
          Logging.registerLogStorage conn "hs_log_capture" (\entry -> atomicModifyIORef' entries (\values -> (entry : values, ())))
          void $ execute_ conn "CALL enable_logging(); SET logging_storage = 'hs_log_capture'; SELECT write_log('registered')"
        -- The sink receives the entry of write_log one time.
        readIORef entries >>= (@?= ["registered"]) . filter (== "registered") . map Logging.logEntryMessage
    ]

fieldInt :: [Field] -> IO Int
fieldInt [fld] =
  case fromField fld of
    Errors err -> assertFailure (show err) >> pure 0
    Ok value -> pure value
fieldInt fields = assertFailure ("expected one field, got " <> show (length fields)) >> pure 0

withConnectionTests :: TestTree
withConnectionTests =
  testGroup
    "withConnection"
    [ testCase "returns the action result" $ do
        result <- withConnection ":memory:" \_ -> pure (21 :: Int)
        assertEqual "action result" 21 result
    , testCase "propagates exceptions from the action"
        $ assertThrowsErrorCall
        $ withConnection ":memory:" (\_ -> error "boom" :: IO ())
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
          (result, stmt) <- withStatement conn "SELECT 1" \stmt -> pure ("done" :: String, stmt)
          assertEqual "withStatement result" "done" result
          -- A read of the closed statement raises an error.
          assertThrowsSQLError (nextRow stmt :: IO (Maybe (Only Int)))
    , testCase "withStatement closes statements even when the action fails" $
        withConnection ":memory:" \conn -> do
          used <- newIORef Nothing
          assertThrowsErrorCall $
            withStatement conn "SELECT 1" (\stmt -> atomicModifyIORef' used (const (Just stmt, ())) >> error "boom" :: IO ())
          -- A read of the closed statement raises an error.
          readIORef used
            >>= maybe (assertFailure "the action did not run") (\closed -> assertThrowsSQLError (nextRow closed :: IO (Maybe (Only Int))))
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
    , testCase "executeMany inserts each row and gives the total count" $
        withConnection ":memory:" \conn -> do
          _ <- execute_ conn "CREATE TABLE params_many (a INTEGER, b TEXT)"
          total <- executeMany conn "INSERT INTO params_many VALUES (?, ?)" [(1 :: Int, "x" :: String), (2 :: Int, "y" :: String)]
          assertEqual "rows affected" 2 total
          query_ conn "SELECT a, b FROM params_many ORDER BY a" >>= (@?= [(1 :: Int, "x" :: String), (2, "y")])
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
  }

duckdbTypeCastTests :: TestTree
duckdbTypeCastTests =
  testGroup
    "duckdb type casts"
    (map buildCase duckdbCastCases)
  where
    buildCase DuckDBCastCase{castLabel, castExpression, castExpectation} =
      testCase castLabel $
        withConnection ":memory:" \conn -> do
          let sql =
                Query
                  ( case castExpression of
                      CastExpression expressionValue expressionType ->
                        Text.concat ["SELECT CAST(", expressionValue, " AS ", expressionType, ")"]
                      DirectExpression expressionSQL ->
                        Text.concat ["SELECT ", expressionSQL]
                  )
          result <- try (query_ conn sql) :: IO (Either SomeException [Only FieldValue])
          runExpectation castLabel castExpectation result

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
  , successCase "TIMESTAMP" (quoted "2024-01-02 03:04:05.123456") "TIMESTAMP" (ExpectEquals (FieldTimestamp (Finite timestampLocalTimeMicros)))
  , successCase "DATE" (quoted "2024-10-12") "DATE" (ExpectEquals (FieldDate (Finite dateValue)))
  , successCase "TIME" (quoted "14:30:15.123456") "TIME" (ExpectEquals (FieldTime timeValue))
  , successCase "INTERVAL" (quoted "1 day") "INTERVAL" (ExpectEquals (FieldInterval intervalValue))
  , successCase "HUGEINT" (quotedValue hugeIntLiteral) "HUGEINT" (ExpectEquals (FieldHugeInt hugeIntLiteral))
  , successCase "UHUGEINT" (quotedValue uHugeIntLiteral) "UHUGEINT" (ExpectEquals (FieldUHugeInt uHugeIntLiteral))
  , successCase "VARCHAR" (quoted varcharText) "VARCHAR" (ExpectEquals (FieldText varcharText))
  , successCase "BLOB" (quoted "duckdb") "BLOB" (ExpectEquals (FieldBlob blobValue))
  , successCase "DECIMAL" (quoted "12345.6789") "DECIMAL(18,4)" (ExpectEquals (FieldDecimal decimalValue))
  , successCase "TIMESTAMP_S" (quoted "2024-01-02 03:04:05") "TIMESTAMP_S" (ExpectEquals (FieldTimestamp (Finite timestampLocalTimeSeconds)))
  , successCase
      "TIMESTAMP_MS"
      (quoted "2024-01-02 03:04:05.123")
      "TIMESTAMP_MS"
      (ExpectEquals (FieldTimestamp (Finite timestampLocalTimeMillis)))
  , successCase
      "TIMESTAMP_NS"
      (quoted "2024-01-02 03:04:05.123456789")
      "TIMESTAMP_NS"
      (ExpectEquals (FieldTimestamp (Finite timestampLocalTimeNanos)))
  , successCase "ENUM" (quoted "beta") "ENUM('alpha','beta')" (ExpectEquals (FieldEnum 1 "beta"))
  , successCase "LIST" (quoted "[1,2,3]") "INTEGER[]" (ExpectEquals (FieldList listElements))
  , successDirect "MAP" "MAP(['a','b'], [1,2])" (expectMapEntries mapPairs)
  , successCase "TIMETZ" (quoted "03:04:05+02:30") "TIME WITH TIME ZONE" (ExpectEquals (FieldTimeTZ timeWithZoneValue))
  , successCase
      "TIMESTAMPTZ"
      (quoted "2024-01-02 03:04:05+02:30")
      "TIMESTAMP WITH TIME ZONE"
      (ExpectEquals (FieldTimestampTZ (Finite timestampTzUtc)))
  , successCase "BIGNUM" (quoted bigNumLiteralText) "BIGNUM" (ExpectEquals (FieldBigNum (BigNum bigNumLiteral)))
  , successCase "UUID" (quoted $ UUID.toText uuid) "UUID" (ExpectEquals (FieldUUID uuid))
  , successCase "BIT" (quoted $ Text.pack $ show bits) "BIT" (ExpectEquals (FieldBit bits))
  , successCase "ARRAY" (quoted "[1,2,3]") "INTEGER[3]" (ExpectEquals (FieldArray arrayElements))
  , successCase "TIME_NS" (quoted "03:04:05.123456789") "TIME_NS" (ExpectEquals (FieldTime (TimeOfDay 3 4 5.123456789)))
  , successDirect "STRUCT" "{'a': 1, 'b': 2}" (expectStruct structFields)
  , successDirect "UNION" "CAST(union_value(a := 42) AS UNION(a INTEGER, b VARCHAR))" (expectUnion 0 (Text.pack "a") (FieldInt32 42))
  , -- These can never surface from a query, only internally.
    failCaseOK "ANY" (quoted "1") "ANY" (expectSQLErrorContaining "ANY")
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
        }
    successDirect label expr expectation =
      DuckDBCastCase
        { castLabel = label
        , castExpression = DirectExpression expr
        , castExpectation = expectation
        }
    failCaseOK label value ty expectation =
      DuckDBCastCase
        { castLabel = label
        , castExpression = CastExpression value ty
        , castExpectation = expectation
        }
    expectMapEntries expectedPairs =
      ExpectSatisfies $
        \case
          FieldMap actualPairs ->
            let normalize = sortOn (mapKey . fst)
                mapKey (FieldText txt) = txt
                mapKey other = Text.pack (show other)
            in  assertEqual "map entries" (normalize expectedPairs) (normalize actualPairs)
          other ->
            assertFailure ("expected FieldMap, but saw " <> show other)
    structFields =
      [ StructField{structFieldName = Text.pack "a", structFieldValue = FieldInt32 1}
      , StructField{structFieldName = Text.pack "b", structFieldValue = FieldInt32 2}
      ]
    expectStruct expectedFields =
      ExpectSatisfies $
        \case
          FieldStruct StructValue{structValueFields, structValueTypes, structValueIndex} -> do
            let actualFields = elems structValueFields
                actualTypes = elems structValueTypes
                expectedIndex = Map.fromList (zip (map structFieldName expectedFields) [0 ..])
                typeNames = map structFieldName actualTypes
            assertEqual "struct field names" (map structFieldName expectedFields) (map structFieldName actualFields)
            assertEqual "struct field values" (map structFieldValue expectedFields) (map structFieldValue actualFields)
            assertEqual "struct type field names" (map structFieldName expectedFields) typeNames
            assertEqual "struct index map" expectedIndex structValueIndex
          other ->
            assertFailure ("expected FieldStruct, but saw " <> show other)
    expectUnion expectedIndex expectedLabel expectedPayload =
      ExpectSatisfies $
        \case
          FieldUnion UnionValue{unionValueIndex, unionValueLabel, unionValuePayload, unionValueMembers} -> do
            assertEqual "union tag index" expectedIndex unionValueIndex
            assertEqual "union tag label" expectedLabel unionValueLabel
            assertEqual "union payload" expectedPayload unionValuePayload
            let actualMembers = elems unionValueMembers
                expectedMembers = [Text.pack "a", Text.pack "b"]
            assertEqual "union member names" expectedMembers (map unionMemberName actualMembers)
          other ->
            assertFailure ("expected FieldUnion, but saw " <> show other)
    expectSQLErrorContaining :: Text.Text -> DuckDBExpectation
    expectSQLErrorContaining needle =
      ExpectException $ \err ->
        case fromException err :: Maybe SQLError of
          Just sqlErr ->
            let message = sqlErrorMessage sqlErr
            in  assertBool
                  ("expected SQLError containing " <> Text.unpack needle <> ", but saw: " <> Text.unpack message)
                  (Text.isInfixOf needle message)
          Nothing ->
            assertFailure ("expected SQLError, but saw " <> displayException err)
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
      TimeWithZone{timeWithZoneTime = TimeOfDay 3 4 5, timeWithZoneOffsetSeconds = 9000}
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
          -- A TIMESTAMP does not read as UTCTime. AT TIME ZONE 'UTC' gives the instant in UTC.
          [Only utcRes] <- query_ conn "SELECT ts::TIMESTAMP AT TIME ZONE 'UTC' FROM temporals" :: IO [Only UTCTime]
          assertEqual "timestamp at UTC" (localTimeToUTC utc tsVal) utcRes
          (try (query_ conn "SELECT ts FROM temporals" :: IO [Only UTCTime])) >>= \case
            Left SQLError{sqlErrorMessage} -> sqlErrorMessage @?= "duckdb-simple: column ts has type TIMESTAMP but expected UTCTime"
            Right rows -> assertFailure ("expected Incompatible, got " <> show rows)
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
          rows <- concat <$> mapM (\bv -> query_ conn $ fromString $ "SELECT CAST('" <> show bv <> "' AS BIGNUM)") bigNumValues
          assertEqual "BIGNUM results" (fmap (Only . BigNum) bigNumValues) rows
    , testCase "round-trips BIGNUM values through the database" $
        withConnection ":memory:" \conn -> do
          _ <- execute_ conn "CREATE TABLE bignum_roundtrip (val BIGNUM)"
          _ <- executeMany conn "INSERT INTO bignum_roundtrip VALUES (?)" (fmap (Only . BigNum) bigNumValues)
          rows <- query_ conn "SELECT val FROM bignum_roundtrip ORDER BY rowid" :: IO [Only Integer]
          assertEqual "BIGNUM round-trip" (fmap Only bigNumValues) rows
    , testCase "duckdbColumnType reports BIGNUM for Integer" $
        assertEqual
          "duckdbColumnType Integer"
          (Text.pack "BIGNUM")
          (duckdbColumnType (Proxy @Integer))
    , testCase "duckdbColumnType preserves Maybe parameter" $
        assertEqual
          "duckdbColumnType Maybe Word16"
          (Text.pack "USMALLINT")
          (duckdbColumnType (Proxy @(Maybe Word16)))
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
          assertEqual "timetz time" expectedTime (timeWithZoneTime tzVal)
          assertEqual "timetz offset" 9000 (timeWithZoneOffsetSeconds tzVal)
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
    , testCase "round-trips struct and union parameters" $
        withConnection ":memory:" \conn -> do
          _ <- execute_ conn "CREATE TABLE struct_union (s STRUCT(a INT, b INT), u UNION(a INT, b VARCHAR))"
          [(structVal, unionVal)] <-
            query_ conn "SELECT {'a': 1, 'b': 2}, CAST(union_value(a := 42) AS UNION(a INTEGER, b VARCHAR))"
              :: IO [(StructValue FieldValue, UnionValue FieldValue)]
          _ <- execute conn "INSERT INTO struct_union VALUES (?, ?)" (structVal, unionVal)
          rows <- query_ conn "SELECT s, u FROM struct_union" :: IO [(StructValue FieldValue, UnionValue FieldValue)]
          assertEqual "struct/union round-trip" [(structVal, unionVal)] rows
    , testCase "generic struct into database" $
        withConnection ":memory:" \conn -> do
          _ <- execute_ conn "CREATE TABLE generic_struct (payload STRUCT(grId INT, grName TEXT))"
          let records = [GenericRecord 1 (Text.pack "alpha"), GenericRecord 2 (Text.pack "beta")]
              structParams = traverse genericToStructValue records
          case structParams of
            Nothing -> assertFailure "expected struct encoding"
            Just structs -> do
              _ <- executeMany conn "INSERT INTO generic_struct VALUES (?)" (map Only structs)
              rows <- query_ conn "SELECT payload FROM generic_struct ORDER BY payload.grId" :: IO [Only (StructValue FieldValue)]
              let unwrap (Only s) = s
                  roundTripped = traverse (genericFromFieldValue . FieldStruct . unwrap) rows
              roundTripped @?= Right records
    , testCase "generic struct encode/decode" $ do
        let rec = GenericRecord 42 (Text.pack "duck")
        case genericToStructValue rec of
          Just structVal -> do
            map structFieldName (elems (structValueFields structVal)) @?= [Text.pack "grId", Text.pack "grName"]
            map structFieldValue (elems (structValueFields structVal)) @?= [FieldInt64 42, FieldText (Text.pack "duck")]
            genericFromFieldValue (FieldStruct structVal) @?= Right rec
          Nothing -> assertFailure "expected struct encoding"
    , testCase "generic union encode/decode" $ do
        let sumVal = SumBool True
        case genericToFieldValue sumVal of
          FieldUnion uv -> do
            map unionMemberName (elems (unionValueMembers uv))
              @?= [ Text.pack "SumInt"
                  , Text.pack "SumBool"
                  , Text.pack "SumPair"
                  ]
            unionValueLabel uv @?= Text.pack "SumBool"
            genericFromFieldValue (FieldUnion uv) @?= Right sumVal
          other -> assertFailure ("expected FieldUnion, got " <> show other)
    , testCase "generic union complex constructor" $ do
        let sumVal = SumPair 7 (Text.pack "duck")
        case genericToFieldValue sumVal of
          FieldUnion uv -> do
            unionValueLabel uv @?= Text.pack "SumPair"
            case unionValuePayload uv of
              FieldStruct StructValue{structValueFields} -> do
                map structFieldName (elems structValueFields) @?= [Text.pack "field1", Text.pack "field2"]
                map structFieldValue (elems structValueFields)
                  @?= [FieldInt64 7, FieldText (Text.pack "duck")]
              other -> assertFailure ("expected struct payload, got " <> show other)
            genericFromFieldValue (FieldUnion uv) @?= Right sumVal
          other -> assertFailure ("expected FieldUnion, got " <> show other)
    , testCase "deriving via generic struct round-trip" $
        withConnection ":memory:" \conn -> do
          _ <- execute_ conn "CREATE TABLE via_users (payload STRUCT(vuId BIGINT, vuName TEXT))"
          let users = [ViaUser 10 (Text.pack "alice"), ViaUser 11 (Text.pack "bob")]
          _ <- executeMany conn "INSERT INTO via_users VALUES (?)" (map Only users)
          rows <- query_ conn "SELECT payload FROM via_users ORDER BY payload.vuId" :: IO [Only ViaUser]
          rows @?= map Only users
    , testCase "deriving via generic union round-trip" $
        withConnection ":memory:" \conn -> do
          _ <- execute_ conn "CREATE TABLE via_shapes (shape UNION(ViaCircle STRUCT(field1 DOUBLE), ViaSquare STRUCT(field1 DOUBLE)))"
          let shapes = [ViaCircle 1.5, ViaSquare 3.0]
          _ <- executeMany conn "INSERT INTO via_shapes VALUES (?)" (map Only shapes)
          rows <- query_ conn "SELECT shape FROM via_shapes ORDER BY rowid" :: IO [Only ViaShape]
          rows @?= map Only shapes
    , testCase "generic enum payload is a STRUCT without fields" $ do
        case genericToFieldValue EnumRed of
          FieldUnion uv -> do
            unionValuePayload uv @?= FieldStruct (StructValue (listArray (0, -1) []) (listArray (0, -1) []) Map.empty)
            genericFromFieldValue (FieldUnion uv) @?= Right EnumRed
          other -> assertFailure ("expected FieldUnion, got " <> show other)
    , -- Edge case tests
      testCase "struct with multiple basic fields" $ do
        -- Test a struct with multiple fields
        let twoField = TwoFieldStruct 42 (Text.pack "text")
        case genericToFieldValue twoField of
          FieldStruct sv -> do
            genericFromFieldValue (FieldStruct sv) @?= Right twoField
          other -> assertFailure ("expected FieldStruct for TwoFieldStruct, got " <> show other)
    , testCase "large sum type with many constructors" $ do
        let constructors = [LargeC1 1, LargeC2 2, LargeC3 3, LargeC4 4, LargeC5 5]
        forM_ constructors $ \val -> do
          case genericToFieldValue val of
            FieldUnion uv -> do
              genericFromFieldValue (FieldUnion uv) @?= Right val
            other -> assertFailure ("expected FieldUnion, got " <> show other)
    , testCase "empty struct encodes as a STRUCT without fields" $ do
        case genericToFieldValue EmptyStruct of
          FieldStruct structVal -> do
            elems (structValueFields structVal) @?= []
            genericFromFieldValue (FieldStruct structVal) @?= Right EmptyStruct
          other -> assertFailure ("expected a STRUCT without fields for empty struct, got " <> show other)
    , testCase "empty struct binds as a STRUCT without fields" $
        withConnection ":memory:" \conn -> do
          query conn "SELECT typeof(?)" (Only EmptyStruct) >>= (@?= [Only (Text.pack "STRUCT")])
          rows <- query conn "SELECT ?" (Only EmptyStruct) :: IO [Only FieldValue]
          case rows of
            [Only (FieldStruct structVal)] -> elems (structValueFields structVal) @?= []
            other -> assertFailure ("expected a STRUCT without fields, got " <> show other)
    , testCase "Just and Nothing of a type without fields stay different in a record" $
        withConnection ":memory:" \conn -> do
          let value = OptionalEmpty (Just EmptyStruct) Nothing
          _ <- execute_ conn "CREATE TABLE optional_empty (r STRUCT(oeFirst STRUCT, oeSecond STRUCT))"
          _ <- execute conn "INSERT INTO optional_empty VALUES (?)" (Only value)
          (query_ conn "SELECT r::VARCHAR, r.oeFirst IS NULL, r.oeSecond IS NULL, r FROM optional_empty" :: IO [(Text.Text, Bool, Bool, OptionalEmpty)])
            >>= (@?= [(Text.pack "{'oeFirst': {}, 'oeSecond': NULL}", False, True, value)])
    , testCase "a constructor without fields binds the STRUCT without fields as its payload" $
        withConnection ":memory:" \conn -> do
          forM_ [(Counted 5, Text.pack "Counted"), (Uncounted, Text.pack "Uncounted")] \(value, tag) ->
            query conn "SELECT union_tag(?)::VARCHAR" (Only value) >>= (@?= [Only tag])
          _ <- execute_ conn "CREATE TABLE tallies (s UNION(Counted STRUCT(field1 BIGINT), Uncounted STRUCT))"
          _ <- executeMany conn "INSERT INTO tallies VALUES (?)" [Only (Counted 5), Only Uncounted]
          (query_ conn "SELECT union_tag(s)::VARCHAR, s::VARCHAR, s FROM tallies ORDER BY rowid" :: IO [(Text.Text, Text.Text, Tally)])
            >>= (@?= [(Text.pack "Counted", Text.pack "{'field1': 5}", Counted 5), (Text.pack "Uncounted", Text.pack "{}", Uncounted)])
    , testCase "the NULL of version 0.3 for a type without fields decodes but does not equal a new parameter" $
        withConnection ":memory:" \conn -> do
          -- Version 0.3 wrote NULL in place of the STRUCT without fields.
          genericFromFieldValue FieldNull @?= Right EmptyStruct
          case genericToFieldValue Uncounted of
            FieldUnion uv -> do
              let oldValue = uv{unionValuePayload = FieldNull}
              genericFromFieldValue (FieldUnion oldValue) @?= Right Uncounted
              _ <- execute_ conn "CREATE TABLE old_tallies (v UNION(Counted STRUCT(field1 BIGINT), Uncounted STRUCT))"
              _ <- execute conn "INSERT INTO old_tallies VALUES (?)" (Only oldValue)
              (query_ conn "SELECT v FROM old_tallies" :: IO [Only Tally]) >>= (@?= [Only Uncounted])
              (query conn "SELECT count(*) FROM old_tallies WHERE v = ?" (Only Uncounted) :: IO [Only Int64]) >>= (@?= [Only 0])
              _ <- execute conn "INSERT INTO old_tallies VALUES (?)" (Only Uncounted)
              (query conn "SELECT count(*) FROM old_tallies WHERE v = ?" (Only Uncounted) :: IO [Only Int64]) >>= (@?= [Only 1])
            other -> assertFailure ("expected FieldUnion, got " <> show other)
          case genericToFieldValue (WithEmpty 7 EmptyStruct) of
            FieldStruct sv -> do
              let oldField item = if structFieldName item == Text.pack "weEmpty" then item{structFieldValue = FieldNull} else item
                  oldValue = sv{structValueFields = fmap oldField (structValueFields sv)}
              _ <- execute_ conn "CREATE TABLE old_with_empty (r STRUCT(weId BIGINT, weEmpty STRUCT))"
              _ <- execute conn "INSERT INTO old_with_empty VALUES (?)" (Only oldValue)
              (query_ conn "SELECT r FROM old_with_empty" :: IO [Only WithEmpty]) >>= (@?= [Only (WithEmpty 7 EmptyStruct)])
              (query conn "SELECT count(*) FROM old_with_empty WHERE r = ?" (Only (WithEmpty 7 EmptyStruct)) :: IO [Only Int64]) >>= (@?= [Only 0])
              _ <- execute conn "INSERT INTO old_with_empty VALUES (?)" (Only (WithEmpty 7 EmptyStruct))
              (query conn "SELECT count(*) FROM old_with_empty WHERE r = ?" (Only (WithEmpty 7 EmptyStruct)) :: IO [Only Int64]) >>= (@?= [Only 1])
            other -> assertFailure ("expected FieldStruct, got " <> show other)
    , testCase "a record field of a type without fields round trips" $
        withConnection ":memory:" \conn -> do
          let value = WithEmpty 7 EmptyStruct
          _ <- execute_ conn "CREATE TABLE with_empty (r STRUCT(weId BIGINT, weEmpty STRUCT))"
          _ <- execute conn "INSERT INTO with_empty VALUES (?)" (Only value)
          (query_ conn "SELECT r::VARCHAR, r FROM with_empty" :: IO [(Text.Text, WithEmpty)])
            >>= (@?= [(Text.pack "{'weId': 7, 'weEmpty': {}}", value)])
    , testCase "struct with Maybe fields" $ do
        let withJust = OptionalFields (Just 42) (Just (Text.pack "text"))
            withNothing = OptionalFields Nothing Nothing
        case (genericToFieldValue withJust, genericToFieldValue withNothing) of
          (FieldStruct sv1, FieldStruct sv2) -> do
            genericFromFieldValue (FieldStruct sv1) @?= Right withJust
            genericFromFieldValue (FieldStruct sv2) @?= Right withNothing
          (other1, other2) -> assertFailure ("expected FieldStruct, got " <> show (other1, other2))
    , -- Error case tests
      testCase "decode wrong field type fails" $ do
        let wrongType = FieldInt32 42 -- Trying to decode as GenericRecord
        case genericFromFieldValue wrongType :: Either String GenericRecord of
          Left err -> assertBool "error mentions STRUCT" (Text.pack "STRUCT" `Text.isInfixOf` Text.pack err)
          Right _ -> assertFailure "expected decode error for wrong type"
    , testCase "decode union as struct fails" $ do
        case genericToFieldValue (SumInt 42) of
          FieldUnion uv -> do
            case genericFromFieldValue (FieldUnion uv) :: Either String GenericRecord of
              Left err -> assertBool "error mentions product type" (Text.pack "product type" `Text.isInfixOf` Text.pack err)
              Right _ -> assertFailure "expected decode error"
          other -> assertFailure ("expected FieldUnion, got " <> show other)
    , testCase "decode struct as union fails" $ do
        case genericToFieldValue (GenericRecord 1 (Text.pack "test")) of
          FieldStruct sv -> do
            case genericFromFieldValue (FieldStruct sv) :: Either String GenericSum of
              Left err -> assertBool "error mentions sum type" (Text.pack "sum type" `Text.isInfixOf` Text.pack err)
              Right _ -> assertFailure "expected decode error"
          other -> assertFailure ("expected FieldStruct, got " <> show other)
    , testCase "decode struct with wrong field count fails" $
        withConnection ":memory:" \conn -> do
          -- Get a real struct with 3 fields from the database
          _ <- execute_ conn "CREATE TABLE three_field (s STRUCT(a INT, b TEXT, c INT))"
          [(Only threeField)] <- query_ conn "SELECT {'a': 1, 'b': 'test', 'c': 99}" :: IO [Only (StructValue FieldValue)]
          -- Try to decode it as GenericRecord which only has 2 fields
          case genericFromFieldValue (FieldStruct threeField) :: Either String GenericRecord of
            Left err -> assertBool "error mentions extra fields" (Text.pack "extra" `Text.isInfixOf` Text.pack err)
            Right _ -> assertFailure "expected decode error for extra fields"
    , testCase "decode union with wrong tag fails" $
        withConnection ":memory:" \conn -> do
          -- Get a real union from the database
          [(Only unionVal)] <-
            query_ conn "SELECT CAST(union_value(x := 42) AS UNION(x INT, y VARCHAR))" :: IO [Only (UnionValue FieldValue)]
          -- Manually create a modified version with an out-of-range tag
          let wrongTag = unionVal{unionValueIndex = 99, unionValueLabel = Text.pack "Invalid"}
          case genericFromFieldValue (FieldUnion wrongTag) :: Either String GenericSum of
            Left err ->
              assertBool "error mentions tag or index" (Text.pack "tag" `Text.isInfixOf` Text.pack err || Text.pack "99" `Text.isInfixOf` Text.pack err)
            Right _ -> assertFailure "expected decode error for wrong tag"
    ]
  where
    -- 7 ^ 2900 has 1,018 magnitude bytes. A value of at most 9 magnitude bytes
    -- has inline storage.
    bigNumValues :: [Integer]
    bigNumValues =
      [ (2 ^ (200 :: Int)) + 8675309
      , 0
      , negate ((2 ^ (180 :: Int)) + 12345)
      , 7 ^ (2900 :: Int)
      , negate (7 ^ (2900 :: Int))
      , 1
      , -1
      , 255
      , 256
      , -256
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
            query_ conn "SELECT hs_optional(x) FROM (VALUES (NULL), (41)) AS t(x)"
              :: IO [Only (Maybe Int64)]
          assertEqual "optional results" [Only Nothing, Only (Just 42)] rows
    , testCase "keeps the SQL types of the arguments of version 0.3" $
        withConnection ":memory:" \conn -> do
          createFunction conn "hs_id_bool" (id :: Bool -> Bool)
          createFunction conn "hs_id_double" (id :: Double -> Double)
          createFunction conn "hs_id_float" (id :: Float -> Float)
          createFunction conn "hs_id_int" (id :: Int -> Int)
          createFunction conn "hs_id_int16" (id :: Int16 -> Int16)
          createFunction conn "hs_id_int32" (id :: Int32 -> Int32)
          createFunction conn "hs_id_int64" (id :: Int64 -> Int64)
          createFunction conn "hs_id_maybe" (id :: Maybe Word16 -> Maybe Word16)
          createFunction conn "hs_id_string" (id :: String -> String)
          createFunction conn "hs_id_text" (id :: Text.Text -> Text.Text)
          createFunction conn "hs_id_word" (id :: Word -> Word)
          createFunction conn "hs_id_word16" (id :: Word16 -> Word16)
          createFunction conn "hs_id_word32" (id :: Word32 -> Word32)
          createFunction conn "hs_id_word64" (id :: Word64 -> Word64)
          query_
            conn
            "SELECT parameter_types::VARCHAR FROM duckdb_functions() WHERE function_name LIKE 'hs\\_id\\_%' ESCAPE '\\' ORDER BY function_name"
            >>= ( @?=
                    map
                      Only
                      [ "[BOOLEAN]" :: Text.Text
                      , "[DOUBLE]"
                      , "[FLOAT]"
                      , "[BIGINT]"
                      , "[SMALLINT]"
                      , "[INTEGER]"
                      , "[BIGINT]"
                      , "[USMALLINT]"
                      , "[VARCHAR]"
                      , "[VARCHAR]"
                      , "[UBIGINT]"
                      , "[USMALLINT]"
                      , "[UINTEGER]"
                      , "[UBIGINT]"
                      ]
                )
    , testCase "takes an argument of each type with FromField and DuckDBColumnType" $
        withConnection ":memory:" \conn -> do
          let user = ViaUser 7 "anna"
          createFunction conn "hs_arg_int8" (\(x :: Int8) -> fromIntegral x * 2 :: Int64)
          createFunction conn "hs_arg_integer" (\(x :: Integer) -> Text.pack (show (x * 2)))
          createFunction conn "hs_arg_bytes" (\(x :: BS.ByteString) -> BS.length x)
          createFunction conn "hs_arg_uuid" UUID.toText
          createFunction conn "hs_arg_day" (\(x :: Day) -> Text.pack (show x))
          createFunction conn "hs_arg_local" (\(x :: LocalTime) -> Text.pack (show x))
          createFunction conn "hs_arg_utc" (\(x :: UTCTime) -> Text.pack (show x))
          createFunction conn "hs_arg_list" (\(xs :: [Int64]) -> sum xs)
          createFunction conn "hs_arg_map" (\(pairs :: Map.Map Text.Text Int64) -> sum (Map.elems pairs))
          createFunction conn "hs_arg_interval" (\(IntervalValue months days micros) -> Text.pack (show (months, days, micros)))
          createFunction conn "hs_arg_record" (\(ViaUser identifier name) -> name <> Text.pack (show identifier))
          createFunction conn "hs_arg_records" (\(users :: [ViaUser]) -> length users)
          createFunction conn "hs_arg_maybe_record" (\(found :: Maybe ViaUser) -> maybe "none" vuName found)
          createFunction conn "hs_arg_price" (\(Price DecimalValue{decimalInteger}) -> fromIntegral decimalInteger :: Int64)
          query_
            conn
            "SELECT hs_arg_int8(-3), hs_arg_integer(1267650600228229401496703205376::HUGEINT), hs_arg_bytes('\\x01\\x02'::BLOB), hs_arg_uuid('00000000-0000-0000-0000-000000000001')"
            >>= (@?= [(-6 :: Int64, Text.pack (show (2 * 2 ^ (100 :: Int) :: Integer)), 2 :: Int, "00000000-0000-0000-0000-000000000001" :: Text.Text)])
          query_
            conn
            "SELECT hs_arg_day(DATE '2026-11-15'), hs_arg_local(TIMESTAMP '2026-11-15 12:34:56'), hs_arg_utc(TIMESTAMPTZ '2026-11-15 12:34:56+00')"
            >>= (@?= [("2026-11-15" :: Text.Text, "2026-11-15 12:34:56" :: Text.Text, "2026-11-15 12:34:56 UTC" :: Text.Text)])
          query_ conn "SELECT hs_arg_list([1, 2, 3]), hs_arg_map(MAP {'a': 1, 'b': 2}), hs_arg_interval(INTERVAL '1 month 2 days 3 microseconds')"
            >>= (@?= [(6 :: Int64, 3 :: Int64, "(1,2,3)" :: Text.Text)])
          query conn "SELECT hs_arg_record(?), hs_arg_records([?, ?]), hs_arg_maybe_record(?), hs_arg_maybe_record(NULL)" (user, user, user, user)
            >>= (@?= [("anna7" :: Text.Text, 2 :: Int, "anna" :: Text.Text, "none" :: Text.Text)])
          query_ conn "SELECT hs_arg_price(1.25)" >>= (@?= [Only (1250 :: Int64)])
          query_
            conn
            "SELECT function_name, parameter_types::VARCHAR FROM duckdb_functions() WHERE function_name IN ('hs_arg_int8', 'hs_arg_integer', 'hs_arg_record', 'hs_arg_records', 'hs_arg_price') ORDER BY function_name"
            >>= ( @?=
                    [ ("hs_arg_int8" :: Text.Text, "[TINYINT]" :: Text.Text)
                    , ("hs_arg_integer", "[HUGEINT]")
                    , ("hs_arg_price", "['DECIMAL(18,3)']")
                    , ("hs_arg_record", "['STRUCT(vuId BIGINT, vuName VARCHAR)']")
                    , ("hs_arg_records", "['STRUCT(vuId BIGINT, vuName VARCHAR)[]']")
                    ]
                )
    , testCase "a UTCTime argument with a TIMESTAMP value gets the cast of DuckDB to TIMESTAMPTZ" $
        withConnection ":memory:" \conn -> do
          -- DuckDB casts the argument to the declared TIMESTAMPTZ with the
          -- TimeZone setting of the session. The function reads that instant.
          createFunction conn "hs_arg_utc_text" (\(x :: UTCTime) -> Text.pack (show x))
          _ <- execute_ conn "SET TimeZone = 'Pacific/Auckland'"
          query_ conn "SELECT hs_arg_utc_text(TIMESTAMP '2024-01-02 03:04:05'), (TIMESTAMP '2024-01-02 03:04:05')::TIMESTAMPTZ"
            >>= (@?= [("2024-01-01 14:04:05 UTC" :: Text.Text, UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 50645))])
    , testCase "declares the SQL type of each result type" $
        withConnection ":memory:" \conn -> do
          createFunction conn "hs_res_bool" True
          createFunction conn "hs_res_int" (1 :: Int)
          createFunction conn "hs_res_int8" (1 :: Int8)
          createFunction conn "hs_res_int16" (1 :: Int16)
          createFunction conn "hs_res_int32" (1 :: Int32)
          createFunction conn "hs_res_int64" (1 :: Int64)
          createFunction conn "hs_res_word" (1 :: Word)
          createFunction conn "hs_res_word8" (1 :: Word8)
          createFunction conn "hs_res_word16" (1 :: Word16)
          createFunction conn "hs_res_word32" (1 :: Word32)
          createFunction conn "hs_res_word64" (1 :: Word64)
          createFunction conn "hs_res_float" (1.5 :: Float)
          createFunction conn "hs_res_double" (1.5 :: Double)
          createFunction conn "hs_res_text" ("x" :: Text.Text)
          createFunction conn "hs_res_string" ("x" :: String)
          createFunction conn "hs_res_io" (pure (1 :: Int16) :: IO Int16)
          createFunction conn "hs_res_integer" (2 ^ (100 :: Int) :: Integer)
          createFunction conn "hs_res_bytes" (BS.pack [1, 2])
          createFunction conn "hs_res_uuid" UUID.nil
          createFunction conn "hs_res_day" (fromGregorian 2026 11 15)
          createFunction conn "hs_res_interval" (IntervalValue 1 2 3)
          createFunction conn "hs_res_amount" (Amount 1.25)
          forM_
            [ ("hs_res_bool", "BOOLEAN")
            , ("hs_res_int", "BIGINT")
            , ("hs_res_int8", "TINYINT")
            , ("hs_res_int16", "SMALLINT")
            , ("hs_res_int32", "INTEGER")
            , ("hs_res_int64", "BIGINT")
            , ("hs_res_word", "UBIGINT")
            , ("hs_res_word8", "UTINYINT")
            , ("hs_res_word16", "USMALLINT")
            , ("hs_res_word32", "UINTEGER")
            , ("hs_res_word64", "UBIGINT")
            , ("hs_res_float", "FLOAT")
            , ("hs_res_double", "DOUBLE")
            , ("hs_res_text", "VARCHAR")
            , ("hs_res_string", "VARCHAR")
            , ("hs_res_io", "SMALLINT")
            , ("hs_res_integer", "HUGEINT")
            , ("hs_res_bytes", "BLOB")
            , ("hs_res_uuid", "UUID")
            , ("hs_res_day", "DATE")
            , ("hs_res_interval", "INTERVAL")
            , ("hs_res_amount", "DECIMAL(18,3)")
            ]
            \(name, expected) -> query_ conn (Query ("SELECT typeof(" <> name <> "())")) >>= (@?= [Only (expected :: Text.Text)])
          -- An Integer argument or result is HUGEINT, and a Natural one is UHUGEINT.
          -- DuckDB casts no BIGNUM or UHUGEINT argument to HUGEINT, and
          -- toDuckValue raises an error for a result outside the range.
          createFunction conn "hs_arg_wide" (\(x :: Integer) -> x)
          assertThrows
            (query_ conn "SELECT hs_arg_wide('5'::BIGNUM)" :: IO [Only Integer])
            (Text.isInfixOf "No function matches" . sqlErrorMessage)
          assertThrows
            (query_ conn "SELECT hs_arg_wide(5::UHUGEINT)" :: IO [Only Integer])
            (Text.isInfixOf "No function matches" . sqlErrorMessage)
          createFunction conn "hs_res_natural" (2 ^ (127 :: Int) :: Natural)
          createFunction conn "hs_res_wide" (2 ^ (127 :: Int) :: Integer)
          query_ conn "SELECT typeof(hs_res_natural()), hs_res_natural()" >>= (@?= [("UHUGEINT" :: Text.Text, 2 ^ (127 :: Int) :: Natural)])
          assertThrows
            (query_ conn "SELECT hs_res_wide()" :: IO [Only Integer])
            (\err -> sqlErrorType err == Just ErrorTypeConversion && Text.isInfixOf "is outside the range of HUGEINT" (sqlErrorMessage err))
          -- DuckDB casts a native result value to the declared type as the SQL cast does.
          createFunction conn "hs_res_rounded" (Amount 1.2345)
          createFunction conn "hs_res_too_large" (Amount 1e20)
          query_ conn "SELECT hs_res_rounded()::VARCHAR, (1.2345::DOUBLE)::DECIMAL(18,3)::VARCHAR"
            >>= (@?= [("1.235" :: Text.Text, "1.235" :: Text.Text)])
          assertThrows
            (query_ conn "SELECT hs_res_too_large()" :: IO [Only Double])
            (Text.isInfixOf "to DECIMAL(18,3)" . sqlErrorMessage)
          query_ conn "SELECT hs_res_int8(), hs_res_word16(), hs_res_float(), hs_res_string(), hs_res_integer(), hs_res_amount()::DOUBLE"
            >>= (@?= [(1 :: Int8, 1 :: Word16, 1.5 :: Float, "x" :: Text.Text, 2 ^ (100 :: Int) :: Integer, 1.25 :: Double)])
    , testCase "writes nested, record, union, and NULL results" $
        withConnection ":memory:" \conn -> do
          createFunction conn "hs_res_user" (\(i :: Int64) -> ViaUser i ("u" <> Text.pack (show i)))
          createFunction conn "hs_res_maybe_user" (\(i :: Int64) -> if odd i then Nothing else Just (ViaUser i "even"))
          createFunction conn "hs_res_shape" (\(i :: Int64) -> if even i then ViaCircle (fromIntegral i) else ViaSquare 1)
          createFunction conn "hs_res_list" (\(i :: Int64) -> [0 .. i])
          createFunction conn "hs_res_map" (\(i :: Int64) -> Map.fromList [(Text.pack (show i), fromIntegral i :: Int16)])
          createFunction conn "hs_res_maybe_small" (\(i :: Int64) -> if odd i then Nothing else Just (fromIntegral i :: Int16))
          query_
            conn
            "SELECT typeof(hs_res_user(1)), typeof(hs_res_maybe_user(1)), typeof(hs_res_shape(1)), typeof(hs_res_list(1)), typeof(hs_res_map(1)), typeof(hs_res_maybe_small(1))"
            >>= ( @?=
                    [
                      ( "STRUCT(vuId BIGINT, vuName VARCHAR)" :: Text.Text
                      , "STRUCT(vuId BIGINT, vuName VARCHAR)" :: Text.Text
                      , "UNION(ViaCircle STRUCT(field1 DOUBLE), ViaSquare STRUCT(field1 DOUBLE))" :: Text.Text
                      , "BIGINT[]" :: Text.Text
                      , "MAP(VARCHAR, SMALLINT)" :: Text.Text
                      , "SMALLINT" :: Text.Text
                      )
                    ]
                )
          rows <-
            query_
              conn
              "SELECT hs_res_user(i), hs_res_maybe_user(i), hs_res_shape(i), hs_res_list(i), hs_res_map(i), hs_res_maybe_small(i) FROM range(5) t(i) ORDER BY i"
          rows
            @?= [ ( ViaUser i ("u" <> Text.pack (show i))
                  , if odd i then Nothing else Just (ViaUser i "even")
                  , if even i then ViaCircle (fromIntegral i) else ViaSquare 1
                  , [0 .. i]
                  , Map.fromList [(Text.pack (show i), fromIntegral i :: Int16)]
                  , if odd i then Nothing else Just (fromIntegral i :: Int16)
                  )
                | i <- [0 .. 4 :: Int64]
                ]
          -- 5000 rows fill more than one chunk. Each odd row is NULL.
          query_ conn "SELECT count(*) FILTER (hs_res_maybe_user(i) IS NULL), count(*) FILTER (hs_res_maybe_small(i) IS NULL) FROM range(5000) t(i)"
            >>= (@?= [(2500 :: Int64, 2500 :: Int64)])
    , testCase "keeps the SQL type of a narrow result in arithmetic" $
        withConnection ":memory:" \conn -> do
          createFunction conn "hs_small" (id :: Int16 -> Int16)
          createFunction conn "hs_unsigned" (id :: Word16 -> Word16)
          -- DuckDB computes SMALLINT * SMALLINT as SMALLINT, and USMALLINT - USMALLINT as USMALLINT.
          assertThrows
            (query_ conn "SELECT hs_small(30000) * hs_small(30000)" :: IO [Only Int64])
            (Text.isInfixOf "Overflow in multiplication of INT16" . sqlErrorMessage)
          assertThrows
            (query_ conn "SELECT hs_unsigned(1) - hs_unsigned(2)" :: IO [Only Int64])
            (Text.isInfixOf "Overflow in subtraction of UINT16" . sqlErrorMessage)
          query_ conn "SELECT hs_small(30000)::INTEGER * hs_small(30000), hs_unsigned(1)::INTEGER - hs_unsigned(2)"
            >>= (@?= [(900000000 :: Int64, -1 :: Int64)])
    , testCase "rejects an argument type with no declaration or with VARIANT at registration" $
        withConnection ":memory:" \conn -> do
          let rejects expected action = assertThrows action (Text.isInfixOf expected . sqlErrorMessage)
          rejects "hs_bad_struct: argument arg0: StructValue has no static field types" $
            createFunction conn "hs_bad_struct" (\(_ :: StructValue FieldValue) -> True)
          rejects "hs_bad_variant: argument arg0: the type of an argument cannot be or contain VARIANT" $
            createFunction conn "hs_bad_variant" (\(_ :: Variant) -> True)
          rejects "hs_bad_variants: argument arg1: the type of an argument cannot be or contain VARIANT" $
            createFunction conn "hs_bad_variants" (\(_ :: Int64) (_ :: [Maybe Variant]) -> True)
          -- The check visits the value type of a MAP, not only the element type of a LIST.
          rejects "hs_bad_map: argument arg0: the type of an argument cannot be or contain VARIANT" $
            createFunction conn "hs_bad_map" (\(_ :: Map.Map Text.Text Variant) -> True)
          rejects "hs_bad_result: result: StructValue has no static field types" $
            createFunction conn "hs_bad_result" (\(_ :: Int64) -> (undefined :: StructValue FieldValue))
          query_ conn "SELECT count(*) FROM duckdb_functions() WHERE function_name LIKE 'hs\\_bad\\_%' ESCAPE '\\'" >>= (@?= [Only (0 :: Int64)])
    , testCase "rejects an empty name or a name with NUL at registration" $
        withConnection ":memory:" \conn ->
          forM_ ["", "hs_bad\0name"] \name ->
            assertThrows
              (createFunction conn name (id :: Int64 -> Int64))
              (\err -> sqlErrorType err == Just ErrorInputInvalid && sqlErrorMessage err == "duckdb-simple: invalid scalar function name")
    , testCase "raises the name error before the state of a closed connection" $ do
        conn <- open ":memory:"
        close conn
        assertThrows
          (createFunction conn "" (id :: Int64 -> Int64))
          (\err -> sqlErrorType err == Just ErrorInputInvalid && sqlErrorMessage err == "duckdb-simple: invalid scalar function name")
    , testCase "a failing call with constant arguments fails as with column arguments" $
        withConnection ":memory:" \conn -> do
          createFunction conn "hs_fail_negative" (\(x :: Int64) -> (if x < 0 then throwIO (userError "negative") else pure x) :: IO Int64)
          let failsWith expected action = assertThrows action (\err -> sqlErrorType err == Just ErrorInputInvalid && expected `Text.isInfixOf` sqlErrorMessage err)
          failsWith "negative" (query_ conn "SELECT hs_fail_negative(-1)" :: IO [Only Int64])
          failsWith "negative" (query_ conn "SELECT hs_fail_negative(i - 1) FROM range(3) t(i)" :: IO [Only Int64])
          failsWith
            "duckdb-simple: argument arg0 is NULL, but Int64 cannot hold NULL"
            (query_ conn "SELECT hs_fail_negative(NULL::BIGINT)" :: IO [Only Int64])
          -- A failing constant in a CASE branch that is not taken does not raise.
          query_ conn "SELECT CASE WHEN i > 5 THEN hs_fail_negative(-1) ELSE i END FROM range(3) t(i)" >>= (@?= map Only [0, 1, 2 :: Int64])
    , testCase "names the argument that does not convert" $
        withConnection ":memory:" \conn -> do
          createFunction conn "hs_sum" (\(x :: Int64) (y :: Int64) -> x + y)
          assertThrows
            (query_ conn "SELECT hs_sum(1, NULL)" :: IO [Only Int64])
            (Text.isInfixOf "duckdb-simple: argument arg1 is NULL, but Int64 cannot hold NULL" . sqlErrorMessage)
    , testCase "supports IO-based functions" $
        withConnection ":memory:" \conn -> do
          ref <- newIORef (0 :: Int)
          createFunction conn "hs_counter" $ do
            atomicModifyIORef' ref \n ->
              let next = n + 1
              in  (next, next)
          first <- query_ conn "SELECT hs_counter()" :: IO [Only Int]
          second <- query_ conn "SELECT hs_counter()" :: IO [Only Int]
          assertEqual "first counter call" [Only 1] first
          assertEqual "second counter call" [Only 2] second
    , testCase "supports thread-local function state" $ do
        conn <- openWithConfig ":memory:" [("threads", "1")]
        createFunctionWithState conn "hs_stateful_counter" (newIORef (0 :: Int)) \ref -> do
          atomicModifyIORef' ref \n ->
            let next = n + 1
            in  (next, next)
        rows <- query_ conn "SELECT hs_stateful_counter() FROM range(3)" :: IO [Only Int]
        close conn
        assertEqual "stateful rows" [Only 1, Only 2, Only 3] rows
    , testCase "returns text rows of each length over more than one chunk" $
        withConnection ":memory:" \conn -> do
          let texts =
                [ Just "a"
                , Nothing
                , Just "b"
                , Just ""
                , Just "a\0b"
                , Just "123456789012"
                , Just "1234567890123"
                , Just (Text.replicate 4000 "abcdefghijklmnopqrstuvwxy")
                , Just "liður ð þ ✓ 😀"
                ]
              pick (i :: Int64) = texts !! fromIntegral (i `mod` 9) :: Maybe Text.Text
          createFunction conn "hs_pick" pick
          createFunction conn "hs_echo" (\(text :: Maybe Text.Text) -> text)
          -- 5000 rows fill more than one chunk. A NULL row is between two text rows.
          rows <- query_ conn "SELECT hs_pick(i) FROM range(5000) t(i) ORDER BY i"
          assertEqual "text rows" (map (Only . pick) [0 .. 4999]) rows
          -- DuckDB compares the bytes in the vector, also the inline bytes after a short text.
          query_
            conn
            "SELECT bool_and(hs_pick(i) IS NOT DISTINCT FROM CASE i % 9 WHEN 0 THEN 'a' WHEN 1 THEN NULL WHEN 2 THEN 'b' WHEN 3 THEN '' WHEN 4 THEN 'a' || chr(0) || 'b' WHEN 5 THEN '123456789012' WHEN 6 THEN '1234567890123' WHEN 7 THEN repeat('abcdefghijklmnopqrstuvwxy', 4000) ELSE 'liður ð þ ✓ 😀' END) FROM range(5000) t(i)"
            >>= (@?= [Only True])
          query_ conn "SELECT hs_echo('x'), hs_echo(NULL)" >>= (@?= [(Just "x" :: Maybe Text.Text, Nothing :: Maybe Text.Text)])
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

-- | Give the action a unique temporary path where no file exists. Remove the
-- file after the action if the action created it. DuckDB writes a COPY TO
-- target that exists through a second file, so the path has no file.
withTemporaryPath :: (FilePath -> IO a) -> IO a
withTemporaryPath = bracket acquire release
  where
    acquire = do
      directory <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile directory "duckdb-simple-spec"
      hClose handle
      removeFile path
      pure path
    release path = doesFileExist path >>= \exists -> when exists (removeFile path)
