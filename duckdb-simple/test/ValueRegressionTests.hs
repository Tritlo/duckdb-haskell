{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : ValueRegressionTests
-- Description : Regression tests for exact values and controlled conversion failures.
module ValueRegressionTests (main, valueRegressionTests) where

import Control.Exception (IOException, bracket, fromException, try)
import Control.Monad (forM_)
import Data.Array (Array, elems, listArray)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), utc, utcToLocalTime)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (
  BigNum (..),
  BitString (..),
  DecimalValue (..),
  Field (..),
  FieldValue (..),
  IntervalValue (..),
  TimeWithZone (..),
  bsFromBool,
 )
import Database.DuckDB.Simple.Generic (ViaDuckDB (..), genericFromFieldValue, genericToStructValue)
import Database.DuckDB.Simple.Internal (destroyValue, output, withConnectionFactory, withConnectionHandle)
import Database.DuckDB.Simple.LogicalRep
import Database.DuckDB.Simple.Ok (Ok (..))
import qualified Database.DuckDB.Simple.Streaming as Streaming
import Database.DuckDB.Simple.Time (Unbounded (..))
import Database.DuckDB.Simple.ToField (ToDuckValue (..))
import Database.DuckDB.Simple.V2.LogicalRep (destroyLogicalType, logicalTypeFromRep, logicalTypeToRep)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

-- | Native timestamps used to test the full storage range.
data NativeTimestamp = Seconds Int64 | Milliseconds Int64 | Microseconds Int64 | Nanoseconds Int64
  deriving (Show)

instance DuckDBColumnType NativeTimestamp where
  duckdbColumnTypeFor _ = "TIMESTAMP"

instance ToDuckValue NativeTimestamp where
  toDuckValue factory (Seconds value) = output (c_duckdb_v2_value_create_timestamp_sec factory value)
  toDuckValue factory (Milliseconds value) = output (c_duckdb_v2_value_create_timestamp_ms factory value)
  toDuckValue factory (Microseconds value) = output (c_duckdb_v2_value_create_timestamp factory value)
  toDuckValue factory (Nanoseconds value) = output (c_duckdb_v2_value_create_timestamp_ns factory value)

instance ToField NativeTimestamp

-- | An array element with a ToField instance and no ToDuckValue instance.
newtype DelegatedInteger = DelegatedInteger Int64

instance DuckDBColumnType DelegatedInteger where
  duckdbColumnTypeFor _ = "BIGINT"

instance ToField DelegatedInteger where
  toField (DelegatedInteger value) = toField value

-- | Bind an array through a value built with the explicit factory.
newtype Scores = Scores (Array Int Int64)
  deriving (Show)

instance DuckDBColumnType Scores where
  duckdbColumnTypeFor _ = "BIGINT[]"

instance ToDuckValue Scores where
  toDuckValue factory (Scores values) = toDuckValue factory values

instance ToField Scores

-- | Record with identical field types to detect positional decoding.
data NamedRecord = NamedRecord {firstValue :: Int64, secondValue :: Int64}
  deriving (Eq, Show, Generic)

-- | Sum with a payload to test NULL member handling.
data NullableSum = EmptyMember | DataMember Int64
  deriving (Eq, Show, Generic)

-- | A generic record for array parameters.
data Pair = Pair {pairName :: Text, pairValue :: Int64}
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB Pair)

-- | A generic record with an ARRAY field.
data Scored = Scored {scoredName :: Text, scoredValues :: Array Int Int32}
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB Scored)

-- | Generic nullary constructors retain their existing UNION schema.
data Colour = Red | Blue
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB Colour)

-- | A generic record with an optional ARRAY field.
data OptionalScores = OptionalScores {optionalScores :: Maybe (Array Int Int32)}
  deriving stock (Generic)
  deriving (DuckDBColumnType, ToField) via (ViaDuckDB OptionalScores)

-- | A generic record with a LIST field of ARRAY elements.
data ListedScores = ListedScores {listedScores :: [Array Int Int32]}
  deriving stock (Generic)
  deriving (DuckDBColumnType, ToField) via (ViaDuckDB ListedScores)

-- | A generic record with an ARRAY field of ARRAY elements.
data NestedScores = NestedScores {nestedScores :: Array Int (Array Int Int32)}
  deriving stock (Generic)
  deriving (DuckDBColumnType, ToField) via (ViaDuckDB NestedScores)

-- | A generic record with a MAP field of ARRAY values.
data MappedScores = MappedScores {mappedScores :: Map.Map Text (Array Int Int32)}
  deriving stock (Generic)
  deriving (DuckDBColumnType, ToField) via (ViaDuckDB MappedScores)

-- | A generic sum with a constructor that has an ARRAY field.
data ChosenScores = SomeScores (Array Int Int32) | NoScores
  deriving stock (Generic)
  deriving (DuckDBColumnType, ToField) via (ViaDuckDB ChosenScores)

-- | Run this module without the main integration suite.
main :: IO ()
main = defaultMain valueRegressionTests

-- | Focused value regressions that also run against the baseline library.
valueRegressionTests :: TestTree
valueRegressionTests =
  testGroup "value regressions" $
    [ testCase "REAL decodes to Float and Double" $ withConnection ":memory:" \conn -> do
        (query_ conn "SELECT 1.25::REAL" :: IO [Only Float]) >>= (@?= [Only 1.25])
        (query_ conn "SELECT 1.25::REAL" :: IO [Only Double]) >>= (@?= [Only 1.25])
    , testCase "Float parameters still decode as Text" $ withConnection ":memory:" \conn ->
        (query conn "SELECT ?" (Only (1.25 :: Float)) :: IO [Only Text]) >>= (@?= [Only "1.25"])
    , testCase "generic nullary UNION constructors round trip through DuckDB" $ withConnection ":memory:" \conn ->
        forM_ [Red, Blue] \colour ->
          (query conn "SELECT ?" (Only colour) :: IO [Only Colour]) >>= (@?= [Only colour])
    , testCase "UNION NULL payload retains its member type" $ withConnection ":memory:" \conn -> do
        let members =
              listArray
                (0, 1)
                [UnionMemberType "number" (LogicalTypeScalar LogicalTypeIdBigInt), UnionMemberType "text" (LogicalTypeScalar LogicalTypeIdVarchar)]
            original = UnionValue 0 "number" FieldNull members
        (query conn "SELECT ?" (Only original) :: IO [Only (UnionValue FieldValue)]) >>= (@?= [Only original])
    , testCase "a UNION value binds the member of its tag up to the member limit of DuckDB" $ withConnection ":memory:" \conn -> do
        let lastMember :: Int -> UnionValue FieldValue
            lastMember count =
              let name index = "m" <> Text.pack (show index)
                  members = listArray (0, count - 1) [UnionMemberType (name index) (LogicalTypeScalar LogicalTypeIdInteger) | index <- [0 .. count - 1]]
              in  UnionValue (fromIntegral (count - 1)) (name (count - 1)) (FieldInt32 7) members
            accepted = lastMember 255
        (query conn "SELECT union_tag(?)::VARCHAR, ?" (accepted, accepted) :: IO [(Text, UnionValue FieldValue)]) >>= (@?= [("m254", accepted)])
        -- DuckDB 2.0.0 permits at most 255 members in a UNION type.
        forM_ [256, 300] \count ->
          try (query conn "SELECT ?" (Only (lastMember count)) :: IO [Only (UnionValue FieldValue)]) >>= \case
            Left SQLError{sqlErrorMessage, sqlErrorType} -> do
              sqlErrorMessage @?= "Binder Error: UNION type supports at most 255 type modifiers"
              sqlErrorType @?= Just ErrorQueryBinder
            Right rows -> assertFailure (show count <> " members: expected the DuckDB member limit error, got " <> show rows)
    , testCase "array elements need only a ToField instance" $ withConnection ":memory:" \conn -> do
        let values = listArray (0 :: Int, 2) [Nothing, Just (DelegatedInteger 41), Just (DelegatedInteger 99)]
            expected = listArray (0, 2) [Nothing, Just 41, Just 99] :: Array Int (Maybe Int64)
        (query conn "SELECT ?" (Only values) :: IO [Only (Array Int (Maybe Int64))]) >>= (@?= [Only expected])
    , testCase "array children use default ToDuckValue instances" $ withConnection ":memory:" \conn -> do
        let units = [-1, 0, 1234567]
            values = listArray (0 :: Int, 2) (map Microseconds units)
            expected = listArray (0, 2) (map (utcToLocalTime utc . posixSecondsToUTCTime . fromRational . (% 1000000) . toInteger) units)
        (query conn "SELECT ?" (Only values) :: IO [Only (Array Int LocalTime)]) >>= (@?= [Only expected])
    , testCase "arrays bind through ToDuckValue with an explicit connection" $ withConnection ":memory:" \conn -> do
        let values = listArray (0, 2) [1, -2, 3]
        (query conn "SELECT typeof(?), ?" (Scores values, Scores values) :: IO [(Text, Array Int Int64)]) >>= (@?= [("BIGINT[3]", values)])
    , testCase "arrays of STRUCT, record, and ARRAY elements take the type of a present element" $ withConnection ":memory:" \conn -> do
        [Only struct] <- query_ conn "SELECT {'a': 1, 'b': 'x'}" :: IO [Only (StructValue FieldValue)]
        let structs = listArray (0, 1) [struct, struct]
        (query conn "SELECT typeof(?), ?" (structs, structs) :: IO [(Text, Array Int (StructValue FieldValue))])
          >>= (@?= [("STRUCT(a INTEGER, b VARCHAR)[2]", structs)])
        let pairs = listArray (0, 2) [Nothing, Just (Pair "x" 1), Just (Pair "y" 2)]
        (query conn "SELECT typeof(?), ?" (pairs, pairs) :: IO [(Text, Array Int (Maybe Pair))])
          >>= (@?= [("STRUCT(pairName VARCHAR, pairValue BIGINT)[3]", pairs)])
        let nested = listArray (0, 1) [listArray (0, 1) [1, 2], listArray (0, 1) [3, 4]] :: Array Int (Array Int Int64)
        (query conn "SELECT typeof(?), ?" (nested, nested) :: IO [(Text, Array Int (Array Int Int64))]) >>= (@?= [("BIGINT[2][2]", nested)])
    , testCase "generic records with ARRAY fields bind with the size of the value" $ withConnection ":memory:" \conn -> do
        let first = Scored "x" (listArray (0, 2) [1, 2, 3])
            second = Scored "y" (listArray (0, 2) [4, 5, 6])
            rowType = "STRUCT(scoredName VARCHAR, scoredValues INTEGER[3])"
        (query conn "SELECT typeof(?), ?" (first, first) :: IO [(Text, Scored)]) >>= (@?= [(rowType, first)])
        _ <- execute_ conn "CREATE TABLE scored (s STRUCT(scoredName VARCHAR, scoredValues INTEGER[3]))"
        _ <- executeMany conn "INSERT INTO scored VALUES (?)" [Only first, Only second]
        (query_ conn "SELECT s FROM scored ORDER BY s.scoredName" :: IO [Only Scored]) >>= (@?= [Only first, Only second])
        let records = listArray (0, 1) [first, second]
        (query conn "SELECT typeof(?), ?" (records, records) :: IO [(Text, Array Int Scored)]) >>= (@?= [(rowType <> "[2]", records)])
        result <- try (query conn "SELECT ?" (Only (Scored "z" (listArray (0, -1) []))))
        case result of
          Left SQLError{sqlErrorMessage} -> assertBool (Text.unpack sqlErrorMessage) ("an ARRAY must have at least one element" `Text.isInfixOf` sqlErrorMessage)
          Right (_ :: [Only Text]) -> assertFailure "expected the DuckDB empty ARRAY error"
    , testCase "generic records with an ARRAY field that has no size raise the error of DuckDB" $ withConnection ":memory:" \conn -> do
        let scores = listArray (0, 1) [1, 2] :: Array Int Int32
            rejected :: (ToField a) => String -> a -> Assertion
            rejected label value =
              try (query conn "SELECT ?::VARCHAR" (Only value) :: IO [Only Text]) >>= \case
                Left SQLError{sqlErrorMessage, sqlErrorType} -> do
                  assertEqual label "Binder Error: ARRAY type size must be at least 1" sqlErrorMessage
                  assertEqual label (Just ErrorQueryBinder) sqlErrorType
                Right rows -> assertFailure (label <> ": expected the DuckDB ARRAY size error, got " <> show rows)
        rejected "Maybe field with Nothing" (OptionalScores Nothing)
        rejected "LIST field" (ListedScores [scores])
        rejected "empty LIST field" (ListedScores [])
        rejected "ARRAY field" (NestedScores (listArray (0, 0) [scores]))
        rejected "MAP field" (MappedScores (Map.singleton "a" scores))
        rejected "empty MAP field" (MappedScores Map.empty)
        rejected "sum constructor with the ARRAY field" (SomeScores scores)
        rejected "sum constructor without the ARRAY field" NoScores
    , testCase "empty arrays of record elements raise the empty ARRAY error" $ withConnection ":memory:" \conn -> do
        result <- try (query conn "SELECT ?" (Only (listArray (0, -1) [] :: Array Int Pair)))
        case result of
          Left SQLError{sqlErrorMessage} -> assertBool (Text.unpack sqlErrorMessage) ("duckdb-simple: an empty ARRAY parameter is not possible" `Text.isInfixOf` sqlErrorMessage)
          Right (_ :: [Only Text]) -> assertFailure "expected the empty ARRAY error"
    , testCase "composite arrays with different element types take the type of array_value" $ withConnection ":memory:" \conn -> do
        forM_
          [ ("{'a': 1::INTEGER}", "{'a': 2::INTEGER, 'b': 3::INTEGER}", "STRUCT(a INTEGER, b INTEGER)[3]")
          , ("{'a': 1::INTEGER}", "{'a': 1.6::DOUBLE}", "STRUCT(a DOUBLE)[3]")
          , ("{'a': 1.2::DECIMAL(4,1)}", "{'a': 1.26::DECIMAL(4,2)}", "STRUCT(a DECIMAL(5,2))[3]")
          , ("{'a': [1.2::DECIMAL(4,1)]}", "{'a': [1.26::DECIMAL(4,2)]}", "STRUCT(a DECIMAL(5,2)[])[3]")
          ]
          \(firstSql, secondSql, typeName) -> do
            [Only first] <- query_ conn (Query ("SELECT " <> firstSql)) :: IO [Only (StructValue FieldValue)]
            [Only second] <- query_ conn (Query ("SELECT " <> secondSql)) :: IO [Only (StructValue FieldValue)]
            forM_
              [([Nothing, Just first, Just second], ["NULL", firstSql, secondSql]), ([Just second, Nothing, Just first], [secondSql, "NULL", firstSql])]
              \(elements, sqls) -> do
                let values = listArray (0 :: Int, 2) elements
                    literal = "array_value(" <> Text.intercalate ", " sqls <> ")"
                (query conn (Query ("SELECT typeof(?), ?::VARCHAR, (" <> literal <> ")::VARCHAR")) (values, values) :: IO [(Text, Text, Text)]) >>= \case
                  [(actualType, text, literalText)] -> do
                    actualType @?= typeName
                    text @?= literalText
                  rows -> assertFailure (show rows)
                withConnectionHandle conn \native -> withConnectionFactory native \factory -> bracket (toDuckValue factory values) destroyValue (const (pure ()))
        (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
    , testCase "a FieldValue binds back as the value that it was read from" $ withConnection ":memory:" \conn -> do
        -- The third item is the type of the bound value for SELECT ?, when it
        -- is not the type of the source.
        forM_
          [ ("BOOLEAN", "true", Nothing)
          , ("TINYINT", "-7", Nothing)
          , ("SMALLINT", "-300", Nothing)
          , ("INTEGER", "-70000", Nothing)
          , ("BIGINT", "-9007199254740993", Nothing)
          , ("UTINYINT", "200", Nothing)
          , ("USMALLINT", "60000", Nothing)
          , ("UINTEGER", "4000000000", Nothing)
          , ("UBIGINT", "18446744073709551615", Nothing)
          , ("HUGEINT", "-170141183460469231731687303715884105728", Nothing)
          , ("UHUGEINT", "5", Nothing)
          , ("UHUGEINT", "340282366920938463463374607431768211455", Nothing)
          , ("FLOAT", "1.25", Nothing)
          , ("DOUBLE", "0.1", Nothing)
          , ("DECIMAL(4,1)", "123.4", Nothing)
          , ("DECIMAL(18,3)", "123456789012345.678", Nothing)
          , ("DECIMAL(38,10)", "1234567890123456789012345678.0123456789", Nothing)
          , ("VARCHAR", "'text'", Nothing)
          , ("VARCHAR COLLATE NOCASE", "'Text'", Nothing)
          , ("JSON", "'{\"a\": 1}'", Just "VARCHAR")
          , ("BLOB", "'\\x00\\xFF'::BLOB", Nothing)
          , ("UUID", "'00112233-4455-6677-8899-aabbccddeeff'", Nothing)
          , ("BIT", "'10110'", Nothing)
          , ("DATE", "'2026-01-02'", Nothing)
          , ("DATE", "'infinity'", Nothing)
          , ("TIME", "'03:04:05.123456'", Nothing)
          , ("TIME_NS", "'03:04:05.123456789'", Just "TIME")
          , ("TIMETZ", "'03:04:05+01:30:15'", Nothing)
          , ("TIMESTAMP", "'2026-01-02 03:04:05.123456'", Nothing)
          , ("TIMESTAMP", "'-infinity'", Nothing)
          , ("TIMESTAMP_S", "'2026-01-02 03:04:05'", Just "TIMESTAMP")
          , ("TIMESTAMP_MS", "'2026-01-02 03:04:05.123'", Just "TIMESTAMP")
          , ("TIMESTAMP_NS", "'2026-01-02 03:04:05.123456789'", Just "TIMESTAMP")
          , ("TIMESTAMPTZ", "'2026-01-02 03:04:05.123456+00'", Nothing)
          , ("TIMESTAMPTZ_NS", "'2026-01-02 03:04:05.123456789+00'", Just "TIMESTAMP WITH TIME ZONE")
          , ("INTERVAL", "INTERVAL '1 month 2 days 3 microseconds'", Nothing)
          , ("BIGNUM", "'123456789012345678901234567890123456789012345678901234567890'", Nothing)
          , ("GEOMETRY", "'POINT (1 2)'", Nothing)
          , ("GEOMETRY('OGC:CRS84')", "'POINT (1 2)'", Nothing)
          , ("VARIANT", "42::BIGINT", Just "BIGINT")
          , ("VARIANT", "{'k': 1}", Just "STRUCT(k VARIANT)")
          , ("INTEGER", "NULL", Just "\"NULL\"")
          , ("INTEGER[]", "[1, NULL, 3]", Nothing)
          , ("TIMESTAMP_NS[]", "['2026-01-02 03:04:05.123456789', '2026-01-02 03:04:05']", Just "TIMESTAMP[]")
          , ("TIME_NS[]", "['03:04:05.123456789', '03:04:05']", Just "TIME[]")
          , ("TIMESTAMPTZ_NS[]", "['2026-01-02 03:04:05.123456789+00', '2026-01-02 03:04:05+00']", Just "TIMESTAMP WITH TIME ZONE[]")
          , ("DECIMAL(4,1)[]", "[1.5, 2.5]", Nothing)
          , ("INTEGER[2]", "[1, 2]", Nothing)
          , ("VARCHAR[2][]", "[['a', 'b'], ['c', NULL]]", Nothing)
          , ("MAP(VARCHAR, INTEGER)", "MAP {'a': 1, 'b': NULL}", Nothing)
          , ("MAP(INTEGER[], STRUCT(x DOUBLE))", "MAP {[1]: {'x': 1.5}}", Nothing)
          , ("STRUCT(a INTEGER, b VARCHAR)", "{'a': 1, 'b': 'x'}", Nothing)
          , ("STRUCT(e ENUM('a', 'b'), n INTEGER[])", "{'e': 'b', 'n': []}", Nothing)
          , ("TUPLE(INTEGER, VARCHAR)", "(1, 'x')", Nothing)
          , ("UNION(a INTEGER, b VARCHAR)", "union_value(b := 'x')", Nothing)
          , ("STRUCT(a INTEGER)[]", "[{'a': 1}, NULL]", Nothing)
          ]
          \(sqlType, literal, boundType) -> do
            let source = "(" <> literal <> ")::" <> sqlType
                label = Text.unpack source
            [Only value] <- query_ conn (Query ("SELECT " <> source)) :: IO [Only FieldValue]
            rows <- query conn (Query ("SELECT typeof(?), typeof(" <> source <> "), ? IS NOT DISTINCT FROM " <> source)) (value, value)
            case (rows, boundType) of
              ([(actual, expected, same)], Nothing) -> do
                assertEqual label expected actual
                assertBool (label <> ": SELECT ? gives another value") same
              ([(actual, _, _ :: Bool)], Just expected) -> assertEqual label expected (actual :: Text)
              _ -> assertFailure (label <> ": " <> show rows)
            _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE back (v " <> sqlType <> ")"))
            _ <- execute conn "INSERT INTO back VALUES (?)" (Only value)
            (query_ conn (Query ("SELECT v IS NOT DISTINCT FROM " <> source <> " FROM back")) :: IO [Only Bool]) >>= assertEqual label [Only True]
        -- A row read as a list of FieldValue binds back as a row.
        _ <-
          execute_
            conn
            "CREATE TABLE source_row (i INTEGER, t VARCHAR, s STRUCT(a INTEGER), l DOUBLE[]); INSERT INTO source_row VALUES (1, 'x', {'a': 2}, [1.5, NULL]); CREATE TABLE copied AS FROM source_row LIMIT 0"
        [row] <- query_ conn "SELECT * FROM source_row" :: IO [[FieldValue]]
        _ <- execute conn "INSERT INTO copied VALUES (?, ?, ?, ?)" row
        (query_ conn "SELECT (SELECT count(*) FROM copied), (SELECT count(*) FROM (FROM source_row EXCEPT FROM copied))" :: IO [(Int64, Int64)])
          >>= (@?= [(1, 0)])
    , testCase "a FieldValue with no known type raises an error" $ withConnection ":memory:" \conn -> do
        let untyped :: String -> FieldValue -> Text -> IO ()
            untyped label value part =
              try (query conn "SELECT ?" (Only value) :: IO [Only FieldValue]) >>= \case
                Left SQLError{sqlErrorType, sqlErrorMessage} -> do
                  assertEqual label (Just ErrorInputInvalid) sqlErrorType
                  assertBool (Text.unpack sqlErrorMessage) (part `Text.isInfixOf` sqlErrorMessage)
                Right rows -> assertFailure (label <> ": " <> show rows)
        [Only enum] <- query_ conn "SELECT 'b'::ENUM('a', 'b')" :: IO [Only FieldValue]
        untyped "ENUM" enum "a FieldEnum holds the index of an ENUM value, but not its ENUM type"
        untyped "ENUM in a list" (FieldList [enum]) "a FieldEnum holds the index of an ENUM value"
        untyped "empty list" (FieldList []) "an empty FieldList has no element type for a parameter with no known type"
        untyped "empty inner list" (FieldList [FieldList [FieldInt32 1], FieldList []]) "an empty FieldList has no element type"
        untyped "empty array" (FieldArray (listArray (0, -1) [])) "an empty FieldArray has no element, and a DuckDB ARRAY has at least one"
        untyped "empty map" (FieldMap []) "an empty FieldMap has no key type and no value type for a parameter with no known type"
        -- NULL elements, and an empty value for a typed parameter, bind as the SQL literal does.
        forM_
          [ ("NULL list", FieldList [FieldNull, FieldNull], "\"NULL\"[]", "[NULL, NULL]")
          , ("NULL array", FieldArray (listArray (0, 1) [FieldNull, FieldNull]), "\"NULL\"[2]", "[NULL, NULL]")
          , ("NULL map values", FieldMap [(FieldText "a", FieldNull)], "MAP(VARCHAR, \"NULL\")", "{a=NULL}")
          ]
          \(label, value, typeName, text) ->
            (query conn "SELECT typeof(?), ?::VARCHAR" (value, value) :: IO [(Text, Text)]) >>= assertEqual label [(typeName, text)]
        forM_
          [ ("STRUCT(a INTEGER)[]", FieldList [], "[]")
          , ("STRUCT(a INTEGER)[]", FieldList [FieldNull], "[NULL]")
          , ("INTEGER[][]", FieldList [FieldList [FieldInt32 1], FieldList []], "[[1], []]")
          , ("MAP(VARCHAR, STRUCT(a INTEGER))", FieldMap [], "{}")
          , ("MAP(VARCHAR, STRUCT(a INTEGER))", FieldMap [(FieldText "k", FieldNull)], "{k=NULL}")
          ]
          \(sqlType, value, text) -> do
            _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE typed (v " <> sqlType <> ")"))
            _ <- execute conn "INSERT INTO typed VALUES (?)" (Only value)
            (query_ conn "SELECT v::VARCHAR FROM typed" :: IO [Only Text]) >>= assertEqual (Text.unpack sqlType) [Only text]
        (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
    , testCase "a FieldList of scalar values with different types follows the SQL list literal" $ withConnection ":memory:" \conn -> do
        forM_
          [ ([FieldInt32 1, FieldInt64 2], "[1::INTEGER, 2::BIGINT]", "BIGINT[]")
          , ([FieldInt64 9007199254740993, FieldDouble 1.5], "[9007199254740993::BIGINT, 1.5::DOUBLE]", "DOUBLE[]")
          , ([FieldList [FieldInt32 1], FieldList [FieldInt64 2]], "[[1::INTEGER], [2::BIGINT]]", "BIGINT[][]")
          ]
          \(items, literal, typeName) ->
            ( query conn (Query ("SELECT typeof(?), ?::VARCHAR, (" <> literal <> ")::VARCHAR")) (FieldList items, FieldList items)
                :: IO [(Text, Text, Text)]
            )
              >>= \case
                [(actualType, text, literalText)] -> do
                  assertEqual (Text.unpack literal) typeName actualType
                  assertEqual (Text.unpack literal) literalText text
                rows -> assertFailure (show rows)
        (query conn "SELECT ?::VARCHAR" (Only (FieldList [FieldInt64 9007199254740993, FieldDouble 1.5])) :: IO [Only Text])
          >>= (@?= [Only "[9007199254740992.0, 1.5]"])
        [Only mixedVariant] <- query_ conn "SELECT [1::VARIANT, 'x'::VARIANT]::VARIANT" :: IO [Only FieldValue]
        forM_
          [ ("scalars", FieldList [FieldInt32 1, FieldText "a"], "Cannot combine types INTEGER and VARCHAR")
          , ("lists", FieldList [FieldList [FieldInt32 1], FieldList [FieldText "a"]], "Cannot combine types INTEGER[] and VARCHAR[]")
          , ("array", FieldArray (listArray (0, 1) [FieldInt32 1, FieldText "a"]), "Cannot combine types INTEGER and VARCHAR")
          , ("VARIANT array", mixedVariant, "Cannot combine types INTEGER and VARCHAR")
          ]
          \(label, value, part) ->
            try (query conn "SELECT ?" (Only value) :: IO [Only FieldValue]) >>= \case
              Left SQLError{sqlErrorType, sqlErrorMessage} -> do
                assertEqual label (Just ErrorQueryNotImplemented) sqlErrorType
                assertBool (Text.unpack sqlErrorMessage) (part `Text.isInfixOf` sqlErrorMessage)
              Right rows -> assertFailure (label <> ": " <> show rows)
    , testCase "a list or an Array of composite elements with different types follows the SQL list literal" $ withConnection ":memory:" \conn -> do
        let elementOf :: Text -> IO (Maybe (StructValue FieldValue))
            elementOf "NULL" = pure Nothing
            elementOf sql = do
              [Only value] <- query_ conn (Query ("SELECT " <> sql))
              pure (Just value)
            sameAs :: (ToField a) => a -> Text -> Text -> IO ()
            sameAs value literal typeName =
              (query conn (Query ("SELECT typeof(?), ?::VARCHAR, (" <> literal <> ")::VARCHAR")) (value, value) :: IO [(Text, Text, Text)]) >>= \case
                [(actualType, text, literalText)] -> do
                  assertEqual (Text.unpack literal) typeName actualType
                  assertEqual (Text.unpack literal) literalText text
                rows -> assertFailure (show rows)
            duckdbError :: (ToField a) => String -> a -> ErrorType -> Text -> IO ()
            duckdbError label value errorType part =
              try (query conn "SELECT ?" (Only value) :: IO [Only FieldValue]) >>= \case
                Left SQLError{sqlErrorType, sqlErrorMessage} -> do
                  assertEqual label (Just errorType) sqlErrorType
                  assertBool (Text.unpack sqlErrorMessage) (part `Text.isInfixOf` sqlErrorMessage)
                Right rows -> assertFailure (label <> ": " <> show rows)
        forM_
          [ (["{'a': 1}", "{'b': 2}"], "STRUCT(a INTEGER, b INTEGER)")
          , (["{'a': 1::INTEGER}", "{'a': 2::BIGINT}"], "STRUCT(a BIGINT)")
          , (["{'a': 9007199254740993::BIGINT}", "{'a': 1.5::DOUBLE}"], "STRUCT(a DOUBLE)")
          , (["{'a': [1::INTEGER]}", "{'a': [2::BIGINT]}"], "STRUCT(a BIGINT[])")
          , (["{'a': 1}", "NULL"], "STRUCT(a INTEGER)")
          , (["NULL", "{'a': 1}", "{'b': 2}"], "STRUCT(a INTEGER, b INTEGER)")
          ]
          \(sqls, elementType) -> do
            elements <- mapM elementOf sqls
            let literal = Text.intercalate ", " sqls
                count = length elements
            sameAs elements ("[" <> literal <> "]") (elementType <> "[]")
            sameAs
              (listArray (0, count - 1) elements :: Array Int (Maybe (StructValue FieldValue)))
              ("array_value(" <> literal <> ")")
              (elementType <> "[" <> Text.pack (show count) <> "]")
        -- The rule can round, as in SQL.
        large <- mapM elementOf ["{'a': 9007199254740993::BIGINT}", "{'a': 1.5::DOUBLE}"]
        (query conn "SELECT ?::VARCHAR" (Only large) :: IO [Only Text]) >>= (@?= [Only "[{'a': 9007199254740992.0}, {'a': 1.5}]"])
        mismatch <- mapM elementOf ["{'a': 1}", "{'a': 'x'}"]
        duckdbError "list" mismatch ErrorQueryNotImplemented "Cannot combine types STRUCT(a INTEGER) and STRUCT(a VARCHAR)"
        duckdbError
          "Array"
          (listArray (0, 1) mismatch :: Array Int (Maybe (StructValue FieldValue)))
          ErrorQueryNotImplemented
          "Cannot combine types"
        unions <-
          mapM
            (\sql -> query_ conn sql :: IO [Only (UnionValue FieldValue)])
            ["SELECT union_value(a := 1)::UNION(a INTEGER)", "SELECT union_value(b := 'x')::UNION(b VARCHAR)"]
        duckdbError "UNION list" (map fromOnly (concat unions)) ErrorTypeConversion "is not present in target union"
        duckdbError
          "ARRAY sizes"
          [listArray (0, 1) [1, 2] :: Array Int Int32, listArray (0, 2) [1, 2, 3]]
          ErrorInputInvalid
          "Cannot cast array of size 2 to array of size 3"
        [Just first, Just second] <- mapM elementOf ["{'a': 1}", "{'b': 2}"]
        let mapped = Map.fromList [("k1" :: Text, first), ("k2", second)]
        (query conn "SELECT typeof(?), ?::VARCHAR" (mapped, mapped) :: IO [(Text, Text)])
          >>= (@?= [("MAP(VARCHAR, STRUCT(a INTEGER, b INTEGER))", "{k1={'a': 1, 'b': NULL}, k2={'a': NULL, 'b': 2}}")])
        -- The column type does not change the type of the list, as in SQL.
        _ <- execute_ conn "CREATE TABLE wide (v STRUCT(a BIGINT, b BIGINT)[]); CREATE TABLE named (v STRUCT(a VARCHAR)[])"
        _ <- execute conn "INSERT INTO wide VALUES (?)" (Only [first, second])
        (query_ conn "SELECT v::VARCHAR FROM wide" :: IO [Only Text]) >>= (@?= [Only "[{'a': 1, 'b': NULL}, {'a': NULL, 'b': 2}]"])
        try (execute conn "INSERT INTO named VALUES (?)" (Only mismatch)) >>= \case
          Left SQLError{sqlErrorType} -> sqlErrorType @?= Just ErrorQueryNotImplemented
          Right count -> assertFailure ("expected the error of DuckDB, stored " <> show count)
        (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
    , testCase "a LIST parameter fails at an element of the wrong type after earlier elements" $ withConnection ":memory:" \conn -> do
        let struct =
              singleField (LogicalTypeList (LogicalTypeScalar LogicalTypeIdBigInt)) (FieldList [FieldInt64 1, FieldInt64 2, FieldText "x", FieldInt64 4])
        result <- try (query conn "SELECT ?" (Only struct) :: IO [Only FieldValue])
        case result of
          Left (err :: IOException) ->
            assertBool (show err) ("unsupported scalar conversion for LogicalTypeIdBigInt from FieldText \"x\"" `Text.isInfixOf` Text.pack (show err))
          Right _ -> assertFailure "expected a conversion error for the third element"
        (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
    , testCase "a declared NULL type binds NULL and rejects a value that is not NULL" $ withConnection ":memory:" \conn -> do
        let sqlNull = LogicalTypeScalar LogicalTypeIdSQLNull
            nullStruct = singleField sqlNull FieldNull
        (query conn "SELECT typeof(?), CAST(? AS VARCHAR)" (nullStruct, nullStruct) :: IO [(Text, Text)])
          >>= (@?= [("STRUCT(\"value\" \"NULL\")", "{'value': NULL}")])
        result <- try (query conn "SELECT ?" (Only (singleField sqlNull (FieldInt32 7))) :: IO [Only FieldValue])
        case result of
          Left (err :: IOException) ->
            assertBool (show err) ("unsupported scalar conversion for LogicalTypeIdSQLNull from FieldInt32 7" `Text.isInfixOf` Text.pack (show err))
          Right _ -> assertFailure "expected a conversion error for a value that is not NULL"
    , testCase "GEOMETRY decodes as well-known binary" $ withConnection ":memory:" \conn -> do
        [Only bytes] <- query_ conn "SELECT 'POINT(1 2)'::GEOMETRY" :: IO [Only BS.ByteString]
        BS.length bytes @?= 21
        (query conn "SELECT ST_AsText(ST_GeomFromWKB(?))" (Only bytes) :: IO [Only Text]) >>= (@?= [Only "POINT (1 2)"])
    , testCase "Float parameter retains FLOAT type" $ withConnection ":memory:" \conn -> do
        (query conn "SELECT typeof(?), ?" (1.25 :: Float, 1.25 :: Float) :: IO [(Text, Float)]) >>= (@?= [("FLOAT", 1.25)])
    , testCase "Float special values survive decoding" $ withConnection ":memory:" \conn -> do
        [Only value] <- query_ conn "SELECT 'NaN'::REAL" :: IO [Only Float]
        assertBool "expected NaN" (isNaN value)
        [Only value'] <- query_ conn "SELECT 'Infinity'::REAL" :: IO [Only Double]
        assertBool "expected positive infinity" (isInfinite value' && value' > 0)
    , testCase "finite Double overflow to Float fails" $ withConnection ":memory:" \conn ->
        assertConversionError (query_ conn "SELECT 1e300::DOUBLE" :: IO [Only Float])
    , testCase "embedded NUL and Unicode Text round trip" $ withConnection ":memory:" \conn -> do
        let value = "before\0after íslenska λ 😀" :: Text
        (query conn "SELECT ?" (Only value) :: IO [Only Text]) >>= (@?= [Only value])
    , testCase "VARCHAR, BLOB, and BIT values on both sides of the inline length read back" $ withConnection ":memory:" \conn -> do
        -- DuckDB keeps a value of at most 12 bytes inline and a longer value
        -- behind a pointer. A BIT value has one more byte for its padding count.
        let lengths = [0, 1, 11, 12, 13, 64]
            texts = [Text.pack (take n (cycle ['a' .. 'z'])) | n <- lengths]
            blobs = [BS.pack (take n (cycle [0, 255, 1, 128])) | n <- lengths]
            bits = [bsFromBool (take n (cycle [True, False, False])) | n <- [1, 8, 87, 88, 89, 200]]
            present = [(Just text, Just blob, Just bit) | (text, blob, bit) <- zip3 texts blobs bits]
            -- The NULL row is between the values of 11 and 12 bytes.
            rows = take 3 present <> [(Nothing, Nothing, Nothing)] <> drop 3 present
        _ <- execute_ conn "CREATE TABLE inline_boundary (i INTEGER, t VARCHAR, b BLOB, bits BIT)"
        _ <-
          executeMany
            conn
            "INSERT INTO inline_boundary VALUES (?, ?, ?, ?)"
            [(i, text, blob, bit) | (i, (text, blob, bit)) <- zip [0 :: Int32 ..] rows]
        (query_ conn "SELECT t, b, bits FROM inline_boundary ORDER BY i" :: IO [(Maybe Text, Maybe BS.ByteString, Maybe BitString)])
          >>= (@?= rows)
        (query_ conn "SELECT list(t ORDER BY i) FROM inline_boundary" :: IO [Only [Maybe Text]])
          >>= (@?= [Only [text | (text, _, _) <- rows]])
    , testCase "Int8 rejects both overflow directions" $ withConnection ":memory:" \conn -> do
        forM_ ["SELECT 128::BIGINT", "SELECT -129::SMALLINT"] \sql ->
          assertConversionError (query_ conn sql :: IO [Only Int8])
        (query_ conn "SELECT -128::BIGINT UNION ALL SELECT 127" :: IO [Only Int8]) >>= (@?= [Only (-128), Only 127])
    , testCase "unsigned narrowing rejects overflow" $ withConnection ":memory:" \conn -> do
        assertConversionError (query_ conn "SELECT 256::USMALLINT" :: IO [Only Word8])
        assertConversionError (query_ conn "SELECT 65536::UINTEGER" :: IO [Only Word16])
        assertConversionError (query_ conn "SELECT 4294967296::UBIGINT" :: IO [Only Word32])
    , testCase "Integer and Natural parameters store exact values for each numeric target" $ withConnection ":memory:" \conn -> do
        let store :: (Show a, ToField a) => Text -> a -> Text -> Assertion
            store dtype value expected = do
              let label = Text.unpack dtype <> " " <> show value
              _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE exact_value (v " <> dtype <> ")"))
              _ <- execute conn "INSERT INTO exact_value VALUES (?)" (Only value)
              (query_ conn "SELECT v::VARCHAR FROM exact_value" :: IO [Only Text]) >>= assertEqual label [Only expected]
              (query conn "SELECT count(*) FROM exact_value WHERE v = ?" (Only value) :: IO [Only Int64]) >>= assertEqual label [Only 1]
            huge = 2 ^ (128 :: Int) + 5 :: Integer
        forM_ ["TINYINT", "SMALLINT", "INTEGER", "BIGINT", "UBIGINT", "HUGEINT", "UHUGEINT", "DECIMAL(38,0)", "DOUBLE", "VARCHAR", "BIGNUM"] \dtype -> do
          let expected = if dtype == "DOUBLE" then "5.0" else "5"
          store dtype (5 :: Integer) expected
          store dtype (5 :: Natural) expected
        forM_ ["TINYINT", "SMALLINT", "INTEGER", "BIGINT", "HUGEINT", "DECIMAL(38,0)", "DOUBLE", "VARCHAR", "BIGNUM"] \dtype ->
          store dtype (-5 :: Integer) (if dtype == "DOUBLE" then "-5.0" else "-5")
        forM_ ["HUGEINT", "UHUGEINT", "DECIMAL(38,0)", "BIGNUM", "VARCHAR"] \dtype ->
          store dtype (2 ^ (100 :: Int) :: Integer) "1267650600228229401496703205376"
        store "HUGEINT" (negate (2 ^ (127 :: Int)) :: Integer) "-170141183460469231731687303715884105728"
        store "HUGEINT" (2 ^ (127 :: Int) - 1 :: Integer) "170141183460469231731687303715884105727"
        forM_ ["UHUGEINT", "BIGNUM"] \dtype -> do
          store dtype (2 ^ (127 :: Int) :: Integer) "170141183460469231731687303715884105728"
          store dtype (2 ^ (127 :: Int) :: Natural) "170141183460469231731687303715884105728"
          store dtype (2 ^ (128 :: Int) - 1 :: Integer) "340282366920938463463374607431768211455"
          store dtype (2 ^ (128 :: Int) - 1 :: Natural) "340282366920938463463374607431768211455"
        forM_ ["BIGNUM", "VARCHAR"] \dtype -> forM_ [huge, negate huge] \value ->
          store dtype value (Text.pack (show value))
    , testCase "an Integer parameter that its target cannot hold raises an error and stores no row" $ withConnection ":memory:" \conn -> do
        let insertValue :: (Show a, ToField a) => Text -> a -> IO (Either SQLError Int)
            insertValue dtype value = do
              _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE rejected_value (v " <> dtype <> ")"))
              result <- try (execute conn "INSERT INTO rejected_value VALUES (?)" (Only value))
              (query_ conn "SELECT count(*) FROM rejected_value" :: IO [Only Int64]) >>= assertEqual (Text.unpack dtype <> " " <> show value) [Only 0]
              pure result
            insert :: Text -> Integer -> IO (Either SQLError Int)
            insert = insertValue
            huge = 2 ^ (128 :: Int) + 5 :: Integer
        -- DuckDB checks the range of a cast from HUGEINT.
        forM_
          ( [(dtype, -5) | dtype <- ["UTINYINT", "USMALLINT", "UINTEGER", "UBIGINT", "UHUGEINT"]]
              <> [("BIGINT", 2 ^ (70 :: Int)), ("DECIMAL(10,0)", 2 ^ (100 :: Int))]
          )
          \(dtype, value) ->
            insert dtype value >>= \case
              Left SQLError{sqlErrorMessage, sqlErrorType} -> do
                assertBool (Text.unpack sqlErrorMessage) (not ("duckdb-simple:" `Text.isPrefixOf` sqlErrorMessage))
                sqlErrorType @?= Just ErrorTypeConversion
              Right _ -> assertFailure ("expected a DuckDB error for " <> Text.unpack dtype <> " " <> show value)
        -- duckdb-simple checks the range of a value outside the HUGEINT range.
        forM_
          ( [("HUGEINT", 2 ^ (127 :: Int)), ("BIGINT", 2 ^ (127 :: Int)), ("UHUGEINT", 2 ^ (128 :: Int))]
              <> [(dtype, value) | dtype <- ["TINYINT", "BIGINT", "UBIGINT", "HUGEINT", "UHUGEINT", "DECIMAL(38,0)"], value <- [huge, negate huge]]
          )
          \(dtype, value) ->
            insert dtype value >>= \case
              Left SQLError{sqlErrorMessage, sqlErrorType} -> do
                sqlErrorType @?= Just ErrorInputOutOfRange
                sqlErrorMessage @?= "duckdb-simple: the value " <> Text.pack (show value) <> " is out of range for a parameter of the type " <> dtype
              Right _ -> assertFailure ("expected the duckdb-simple range error for " <> Text.unpack dtype <> " " <> show value)
        insertValue "BIGINT" (2 ^ (128 :: Int) + 5 :: Natural) >>= \case
          Left SQLError{sqlErrorMessage, sqlErrorType} -> do
            sqlErrorType @?= Just ErrorInputOutOfRange
            sqlErrorMessage @?= "duckdb-simple: the value 340282366920938463463374607431768211461 is out of range for a parameter of the type BIGINT"
          Right _ -> assertFailure "expected the duckdb-simple range error for a Natural BIGINT value"
        _ <- execute_ conn "CREATE TABLE wrapped_value (v UBIGINT)"
        _ <- execute_ conn "INSERT INTO wrapped_value VALUES (18446744073709551611), (5)"
        (query conn "SELECT count(*) FROM wrapped_value WHERE v = ?" (Only (-5 :: Integer)) :: IO [Only Int64]) >>= (@?= [Only 0])
    , testCase "the SQL type of an Integer parameter follows its range" $ withConnection ":memory:" \conn -> do
        let typed :: (ToField a) => a -> Integer -> Text -> Assertion
            typed value expected dtype = (query conn "SELECT typeof(?), ?" (value, value) :: IO [(Text, Integer)]) >>= (@?= [(dtype, expected)])
        typed (5 :: Integer) 5 "HUGEINT"
        typed (negate (2 ^ (127 :: Int)) :: Integer) (negate (2 ^ (127 :: Int))) "HUGEINT"
        typed (5 :: Natural) 5 "HUGEINT"
        typed (2 ^ (127 :: Int) :: Integer) (2 ^ (127 :: Int)) "BIGNUM"
        typed (2 ^ (130 :: Int) :: Integer) (2 ^ (130 :: Int)) "BIGNUM"
        typed (BigNum 5) 5 "BIGNUM"
    , testCase "executeMany stores Integer rows of each range" $ withConnection ":memory:" \conn ->
        forM_ [("BIGNUM", [5, 2 ^ (127 :: Int), 2 ^ (130 :: Int)]), ("HUGEINT", [5, 2 ^ (100 :: Int)])] \(dtype, values :: [Integer]) -> do
          _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE many_values (v " <> dtype <> ")"))
          _ <- executeMany conn "INSERT INTO many_values VALUES (?)" (map Only values)
          (query_ conn "SELECT v FROM many_values ORDER BY rowid" :: IO [Only Integer]) >>= (@?= map Only values)
    , testCase "an array of Integer values binds BIGNUM elements" $ withConnection ":memory:" \conn ->
        forM_ [listArray (0, 1) [5, 2 ^ (130 :: Int)], listArray (0, 1) [5, 6]] \(values :: Array Int Integer) ->
          (query conn "SELECT typeof(?), ?" (values, values) :: IO [(Text, Array Int Integer)]) >>= (@?= [("BIGNUM[2]", values)])
    , testCase "an array of Integer values takes the element type of its target" $ withConnection ":memory:" \conn -> do
        let store :: (Show a, ToField a) => Text -> a -> Text -> Assertion
            store dtype value expected = do
              _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE array_value (v " <> dtype <> ")"))
              _ <- execute conn "INSERT INTO array_value VALUES (?)" (Only value)
              (query_ conn "SELECT v::VARCHAR FROM array_value" :: IO [Only Text])
                >>= assertEqual (Text.unpack dtype <> " " <> show value) [Only expected]
            pair :: Integer -> Integer -> Array Int Integer
            pair first second = listArray (0, 1) [first, second]
            huge = 2 ^ (128 :: Int) + 7 :: Integer
        forM_ ["BIGINT[2]", "UBIGINT[2]", "HUGEINT[2]", "UHUGEINT[2]", "DECIMAL(38,0)[2]", "BIGNUM[2]", "VARCHAR[2]"] \dtype ->
          store dtype (pair 5 6) "[5, 6]"
        forM_ ["BIGINT[2]", "HUGEINT[2]", "BIGNUM[2]"] \dtype ->
          store dtype (pair 5 (-6)) "[5, -6]"
        store "UHUGEINT[2]" (listArray (0, 1) [5, 2 ^ (127 :: Int)] :: Array Int Natural) "[5, 170141183460469231731687303715884105728]"
        forM_ ["BIGNUM[2]", "VARCHAR[2]"] \dtype ->
          store dtype (pair 5 huge) "[5, 340282366920938463463374607431768211463]"
        store "BIGINT[2][2]" (listArray (0, 1) [pair 1 2, pair 3 (-4)] :: Array Int (Array Int Integer)) "[[1, 2], [3, -4]]"
        store "BIGINT[]" (pair 5 6) "[5, 6]"
    , testCase "an array of Integer values with an element that its target cannot hold raises an error and stores no row" $ withConnection ":memory:" \conn -> do
        let insert :: Text -> Integer -> IO SQLError
            insert dtype second = do
              let label = Text.unpack dtype <> " [5, " <> show second <> "]"
              _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE rejected_array (v " <> dtype <> ")"))
              result <- try (execute conn "INSERT INTO rejected_array VALUES (?)" (Only (listArray (0, 1) [5, second] :: Array Int Integer)))
              (query_ conn "SELECT count(*) FROM rejected_array" :: IO [Only Int64]) >>= assertEqual label [Only 0]
              either pure (\_ -> assertFailure ("expected an error for " <> label)) result
            huge = 2 ^ (128 :: Int) + 7 :: Integer
        -- DuckDB checks the range of each element when it builds the array.
        forM_ ([(dtype, -6) | dtype <- ["UTINYINT[2]", "UBIGINT[2]", "UHUGEINT[2]", "UBIGINT[]"]] <> [("TINYINT[2]", 300)]) \(dtype, second) -> do
          SQLError{sqlErrorMessage, sqlErrorType} <- insert dtype second
          assertBool (Text.unpack sqlErrorMessage) ("is out of range for the destination type" `Text.isInfixOf` sqlErrorMessage)
          sqlErrorType @?= Just ErrorInputInvalid
        -- duckdb-simple checks the range of an element outside the HUGEINT range.
        forM_ [(dtype, second) | dtype <- ["BIGINT[2]", "UBIGINT[2]", "HUGEINT[2]", "DECIMAL(38,0)[2]"], second <- [huge, negate huge]] \(dtype, second) -> do
          SQLError{sqlErrorType} <- insert dtype second
          sqlErrorType @?= Just ErrorInputOutOfRange
    , testCase "a list binds as a LIST parameter in each SQL form" $ withConnection ":memory:" \conn -> do
        let ints = [1, 2, 3] :: [Int64]
            none = [] :: [Int64]
            count :: (ToRow p) => Query -> p -> IO [Only Int64]
            count = query conn
        forM_ [ints, none] \values ->
          (query conn "SELECT typeof(?), ?" (values, values) :: IO [(Text, [Int64])]) >>= (@?= [("BIGINT[]", values)])
        _ <- execute_ conn "CREATE TABLE list_value (i BIGINT, v BIGINT[])"
        _ <- executeMany conn "INSERT INTO list_value VALUES (?, ?)" [(0 :: Int64, ints), (1, none)]
        (query_ conn "SELECT v FROM list_value ORDER BY i" :: IO [Only [Int64]]) >>= (@?= [Only ints, Only none])
        count "SELECT count(*) FROM list_value WHERE v = ?" (Only ints) >>= (@?= [Only 1])
        count "SELECT count(*) FROM list_value WHERE v = ?" (Only none) >>= (@?= [Only 1])
        -- DuckDB casts a LIST to an ARRAY of the same length only.
        _ <- execute_ conn "CREATE TABLE array_column (v BIGINT[3])"
        _ <- execute conn "INSERT INTO array_column VALUES (?)" (Only ints)
        (query_ conn "SELECT v FROM array_column" :: IO [Only [Int64]]) >>= (@?= [Only ints])
        count "SELECT count(*) FROM array_column WHERE v = ?" (Only ints) >>= (@?= [Only 1])
        forM_ [[1, 2], none] \values -> do
          result <- try (execute conn "INSERT INTO array_column VALUES (?)" (Only values))
          case result of
            Left SQLError{sqlErrorMessage, sqlErrorType} -> do
              assertBool (Text.unpack sqlErrorMessage) ("Conversion Error: Cannot cast list with length " `Text.isPrefixOf` sqlErrorMessage)
              sqlErrorType @?= Just ErrorTypeConversion
            Right (_ :: Int) -> assertFailure ("expected the DuckDB length error for " <> show values)
        (query_ conn "SELECT count(*) FROM array_column" :: IO [Only Int64]) >>= (@?= [Only 1])
        _ <- execute_ conn "CREATE TABLE searched (v BIGINT)"
        _ <- execute_ conn "INSERT INTO searched VALUES (1), (2), (5)"
        forM_
          [ "SELECT count(*) FROM searched WHERE v = ANY(?)"
          , "SELECT count(*) FROM searched WHERE v IN (SELECT unnest(?))"
          , "SELECT count(*) FROM searched WHERE list_contains(?, v)"
          ]
          \sql -> do
            count sql (Only ([1, 5] :: [Int64])) >>= (@?= [Only 2])
            count sql (Only none) >>= (@?= [Only 0])
    , testCase "a list binds Maybe, nested, Text, String, and UTC elements" $ withConnection ":memory:" \conn -> do
        (query conn "SELECT typeof(?), (?)::VARCHAR" ([Just 1, Nothing] :: [Maybe Int64], [Just 1, Nothing] :: [Maybe Int64]) :: IO [(Text, Text)])
          >>= (@?= [("BIGINT[]", "[1, NULL]")])
        let nested = [[1, 2], [], [3]] :: [[Int64]]
        (query conn "SELECT typeof(?), ?" (nested, nested) :: IO [(Text, [[Int64]])]) >>= (@?= [("BIGINT[][]", nested)])
        -- A String stays one VARCHAR parameter next to a list of Text.
        let labels = ["a", "b"] :: [Text]
            label = "ab" :: String
        (query conn "SELECT typeof(?), ?, typeof(?), ?" (labels, labels, label, label) :: IO [(Text, [Text], Text, String)])
          >>= (@?= [("VARCHAR[]", labels, "VARCHAR", label)])
        (query conn "SELECT typeof(?), typeof(?)" (["x"] :: [String], [] :: [String]) :: IO [(Text, Text)]) >>= (@?= [("VARCHAR[]", "VARCHAR[]")])
        -- A UTC element keeps its nanoseconds for a nanosecond target.
        let precise = posixSecondsToUTCTime 0.123456789
        (query conn "SELECT ?::TIMESTAMPTZ_NS[]" (Only [Just precise, Nothing]) :: IO [Only [Maybe UTCTime]])
          >>= (@?= [Only [Just precise, Nothing]])
    , testCase "a list of Integer values takes the element type of its target" $ withConnection ":memory:" \conn -> do
        let insert :: Text -> [Integer] -> IO (Either SQLError Int)
            insert dtype values = do
              _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE integer_list (v " <> dtype <> ")"))
              try (execute conn "INSERT INTO integer_list VALUES (?)" (Only values))
            stored = query_ conn "SELECT v::VARCHAR FROM integer_list" :: IO [Only Text]
        insert "UBIGINT[]" [5, 6] >>= either (assertFailure . show) (const (pure ()))
        stored >>= (@?= [Only "[5, 6]"])
        insert "UBIGINT[]" [5, -6] >>= \case
          Left SQLError{sqlErrorType} -> sqlErrorType @?= Just ErrorInputInvalid
          Right _ -> assertFailure "expected the DuckDB range error for -6"
        insert "UBIGINT[]" [5, 2 ^ (130 :: Int)] >>= \case
          Left SQLError{sqlErrorType} -> sqlErrorType @?= Just ErrorInputOutOfRange
          Right _ -> assertFailure "expected the duckdb-simple range error for 2^130"
        stored >>= (@?= [])
        (query conn "SELECT typeof(?)" (Only ([5, 6] :: [Integer])) :: IO [Only Text]) >>= (@?= [Only "BIGNUM[]"])
    , testCase "a list and an Array with the same elements store the same LIST value" $ withConnection ":memory:" \conn -> do
        let ints = [1, 2, 3] :: [Int64]
            arr = listArray (0, 2) ints :: Array Int Int64
        (query conn "SELECT typeof(?), typeof(?)" (ints, arr) :: IO [(Text, Text)]) >>= (@?= [("BIGINT[]", "BIGINT[3]")])
        _ <- execute_ conn "CREATE TABLE same_value (i BIGINT, v BIGINT[])"
        _ <- execute conn "INSERT INTO same_value VALUES (0, ?), (1, ?)" (ints, arr)
        (query_ conn "SELECT v FROM same_value ORDER BY i" :: IO [Only [Int64]]) >>= (@?= [Only ints, Only ints])
        (query_ conn "SELECT count(DISTINCT v) FROM same_value" :: IO [Only Int64]) >>= (@?= [Only 1])
    , testCase "an empty list of composite elements has no element type" $ withConnection ":memory:" \conn -> do
        let rejected :: (ToField a) => Text -> a -> Assertion
            rejected typeName value =
              try (query conn "SELECT ?" (Only value) :: IO [Only FieldValue]) >>= \case
                Left SQLError{sqlErrorMessage} ->
                  sqlErrorMessage
                    @?= "duckdb-simple: an empty list or map of " <> typeName <> " elements has no element type for a parameter with no known type"
                Right _ -> assertFailure ("expected the element type error for " <> Text.unpack typeName)
        rejected "STRUCT" ([] :: [StructValue FieldValue])
        rejected "BIGINT[]" ([] :: [[Int64]])
        -- An Array of NULL STRUCT values gets the type of array_value(NULL).
        let nullArray = listArray (0, 0) [Nothing] :: Array Int (Maybe (StructValue FieldValue))
        (query conn "SELECT typeof(?), ?::VARCHAR" (nullArray, nullArray) :: IO [(Text, Text)]) >>= (@?= [("\"NULL\"[1]", "[NULL]")])
    , testCase "a list of NULL composite elements takes the element type of a typed column" $ withConnection ":memory:" \conn -> do
        let nulls = [Nothing, Nothing] :: [Maybe (StructValue FieldValue)]
        _ <- execute_ conn "CREATE TABLE null_list (v STRUCT(a INTEGER)[]); CREATE TABLE null_array (v STRUCT(a INTEGER)[2])"
        _ <- execute conn "INSERT INTO null_list VALUES (?)" (Only nulls)
        _ <- execute conn "INSERT INTO null_array VALUES (?)" (Only (listArray (0, 1) nulls :: Array Int (Maybe (StructValue FieldValue))))
        (query_ conn "SELECT typeof(v), v::VARCHAR FROM null_list UNION ALL SELECT typeof(v), v::VARCHAR FROM null_array" :: IO [(Text, Text)])
          >>= (@?= [("STRUCT(a INTEGER)[]", "[NULL, NULL]"), ("STRUCT(a INTEGER)[2]", "[NULL, NULL]")])
        (query conn "SELECT typeof(?::STRUCT(a INTEGER)[])" (Only nulls) :: IO [Only Text]) >>= (@?= [Only "STRUCT(a INTEGER)[]"])
        -- An empty list takes the element type of a typed column too.
        _ <- execute_ conn "DELETE FROM null_list"
        _ <- execute conn "INSERT INTO null_list VALUES (?)" (Only ([] :: [StructValue FieldValue]))
        (query_ conn "SELECT v::VARCHAR FROM null_list" :: IO [Only Text]) >>= (@?= [Only "[]"])
        -- With no known type, a list of NULL elements gets the type of the SQL literal [NULL, NULL].
        (query conn "SELECT typeof(?)" (Only nulls) :: IO [Only Text]) >>= (@?= [Only "\"NULL\"[]"])
    , testCase "an IntervalValue binds as an INTERVAL and keeps its three fields" $ withConnection ":memory:" \conn -> do
        let value = IntervalValue 1 2 3
            negative = IntervalValue (-1) (-2) (-3)
            month = IntervalValue 1 0 0
            days30 = IntervalValue 0 30 0
        forM_ [value, negative] \interval ->
          (query conn "SELECT typeof(?), ?" (interval, interval) :: IO [(Text, IntervalValue)]) >>= (@?= [("INTERVAL", interval)])
        _ <- execute_ conn "CREATE TABLE intervals (i BIGINT, v INTERVAL)"
        _ <- executeMany conn "INSERT INTO intervals VALUES (?, ?)" [(0 :: Int64, days30), (1, month), (2, negative)]
        (query_ conn "SELECT v FROM intervals ORDER BY i" :: IO [Only IntervalValue]) >>= (@?= map Only [days30, month, negative])
        -- DuckDB compares intervals with 30 days for each month.
        (query conn "SELECT count(*) FROM intervals WHERE v = ?" (Only days30) :: IO [Only Int64]) >>= (@?= [Only 2])
        (query conn "SELECT count(*) FROM intervals WHERE v < ?" (Only (IntervalValue 0 31 0)) :: IO [Only Int64]) >>= (@?= [Only 3])
        (query conn "SELECT typeof(TIMESTAMP '2026-01-31' + ?), TIMESTAMP '2026-01-31' + ?" (month, month) :: IO [(Text, LocalTime)])
          >>= (@?= [("TIMESTAMP", LocalTime (fromGregorian 2026 2 28) (TimeOfDay 0 0 0))])
        (query conn "SELECT ?" (Only (Nothing :: Maybe IntervalValue)) :: IO [Only (Maybe IntervalValue)]) >>= (@?= [Only Nothing])
        (query conn "SELECT typeof(?), ?" ([value, negative], [value, negative]) :: IO [(Text, [IntervalValue])])
          >>= (@?= [("INTERVAL[]", [value, negative])])
        let array = listArray (0, 1) [value, negative] :: Array Int IntervalValue
        (query conn "SELECT typeof(?), ?" (array, array) :: IO [(Text, [IntervalValue])]) >>= (@?= [("INTERVAL[2]", [value, negative])])
    , testCase "a TimeWithZone binds as a TIMETZ with its time and offset in seconds" $ withConnection ":memory:" \conn -> do
        let at hours minutes offset = TimeWithZone (TimeOfDay hours minutes 0) offset
            value = at 12 34 19800
            -- The offset +01:00:30.
            seconds = at 12 0 3630
            roundTrip time = (query conn "SELECT ?" (Only time) :: IO [Only TimeWithZone]) >>= (@?= [Only time])
        (query conn "SELECT typeof(?), ?" (value, value) :: IO [(Text, TimeWithZone)]) >>= (@?= [("TIME WITH TIME ZONE", value)])
        -- An offset with seconds reads, binds, and prints exactly.
        ( query_ conn "SELECT '12:00:00+01:00:30'::TIMETZ, '12:00:00-15:59:59'::TIMETZ, '24:00:00+15:59:59'::TIMETZ"
            :: IO [(TimeWithZone, TimeWithZone, TimeWithZone)]
          )
          >>= (@?= [(seconds, at 12 0 (-57599), TimeWithZone (TimeOfDay 24 0 0) 57599)])
        (query conn "SELECT ?::VARCHAR" (Only seconds) :: IO [Only Text]) >>= (@?= [Only "12:00:00+01:00:30"])
        mapM_
          roundTrip
          [seconds, at 12 0 57599, at 12 0 (-57599), TimeWithZone (TimeOfDay 24 0 0) 0, TimeWithZone (TimeOfDay 23 59 59.999999) (-3630)]
        -- DuckDB stores an offset from -15:59:59 to +15:59:59.
        forM_ [57600, -57600] \offset ->
          try (query conn "SELECT ?" (Only (at 12 0 offset)) :: IO [Only TimeWithZone]) >>= \case
            Left (err :: IOException) ->
              assertBool
                (show err)
                (("duckdb-simple: the TIMETZ offset " <> Text.pack (show offset) <> " seconds is out of range") `Text.isInfixOf` Text.pack (show err))
            Right rows -> assertFailure ("expected the offset error, got " <> show rows)
        -- A TIME column removes the offset and keeps the local time.
        _ <- execute_ conn "CREATE TABLE times (a TIMETZ, b TIME)"
        _ <- execute conn "INSERT INTO times VALUES (?, ?)" (seconds, seconds)
        (query_ conn "SELECT a, b FROM times" :: IO [(TimeWithZone, TimeOfDay)]) >>= (@?= [(seconds, TimeOfDay 12 0 0)])
        -- DuckDB compares the instant first, and then the offset.
        (query conn "SELECT count(*) FROM times WHERE a = ?" (Only seconds) :: IO [Only Int64]) >>= (@?= [Only 1])
        (query conn "SELECT count(*) FROM times WHERE a = ?" (Only (at 12 0 3631)) :: IO [Only Int64]) >>= (@?= [Only 0])
        (query conn "SELECT count(*) FROM times WHERE ? < a" (Only (at 12 0 3631)) :: IO [Only Int64]) >>= (@?= [Only 1])
        (query conn "SELECT count(*) FROM times WHERE a = ?" (Only (TimeWithZone (TimeOfDay 10 59 30) 0)) :: IO [Only Int64]) >>= (@?= [Only 0])
        -- The digits below one microsecond go away, as in SQL text.
        (query conn "SELECT ?" (Only seconds{timeWithZoneTime = TimeOfDay 1 2 3.0000005}) :: IO [Only TimeWithZone])
          >>= (@?= [Only seconds{timeWithZoneTime = TimeOfDay 1 2 3}])
        (query conn "SELECT ?" (Only (Nothing :: Maybe TimeWithZone)) :: IO [Only (Maybe TimeWithZone)]) >>= (@?= [Only Nothing])
        let times = [value, seconds, at 0 0 (-57599)]
        (query conn "SELECT typeof(?), ?" (times, times) :: IO [(Text, [TimeWithZone])]) >>= (@?= [("TIME WITH TIME ZONE[]", times)])
        let array = listArray (0, 2) times :: Array Int TimeWithZone
        (query conn "SELECT typeof(?), ?" (array, array) :: IO [(Text, [TimeWithZone])]) >>= (@?= [("TIME WITH TIME ZONE[3]", times)])
        let nested = singleField (LogicalTypeScalar LogicalTypeIdTimeTz) (FieldTimeTZ seconds)
        (query conn "SELECT ?, (?).value::VARCHAR" (nested, nested) :: IO [(StructValue FieldValue, Text)])
          >>= (@?= [(nested, "12:00:00+01:00:30")])
    , testCase "a DecimalValue binds as a DECIMAL with its own width and scale" $ withConnection ":memory:" \conn -> do
        let value = DecimalValue 5 3 2125
            roundTrip decimal = (query conn "SELECT ?" (Only decimal) :: IO [Only DecimalValue]) >>= (@?= [Only decimal])
            stored column decimal = do
              _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE decimals (v " <> column <> ")"))
              _ <- execute conn "INSERT INTO decimals VALUES (?)" (Only decimal)
              query_ conn "SELECT v FROM decimals" :: IO [Only DecimalValue]
        (query conn "SELECT typeof(?), ?" (value, value) :: IO [(Text, DecimalValue)]) >>= (@?= [("DECIMAL(5,3)", value)])
        -- A column of another width and scale gives its own type. DuckDB rounds
        -- half away from zero.
        stored "DECIMAL(4,2)" value >>= (@?= [Only (DecimalValue 4 2 213)])
        stored "DECIMAL(4,2)" (DecimalValue 5 3 2135) >>= (@?= [Only (DecimalValue 4 2 214)])
        stored "DECIMAL(4,2)" (DecimalValue 5 3 (-2125)) >>= (@?= [Only (DecimalValue 4 2 (-213))])
        stored "DECIMAL(10,4)" value >>= (@?= [Only (DecimalValue 10 4 21250)])
        try (stored "DECIMAL(4,2)" (DecimalValue 8 3 12345678)) >>= \case
          Left SQLError{sqlErrorMessage, sqlErrorType} -> do
            assertBool
              (Text.unpack sqlErrorMessage)
              ("Casting value \"12345.678\" to type DECIMAL(4,2) failed: value is out of range!" `Text.isInfixOf` sqlErrorMessage)
            sqlErrorType @?= Just ErrorTypeConversion
          Right rows -> assertFailure ("expected the DECIMAL range error, got " <> show rows)
        stored "DECIMAL(3,2)" (DecimalValue 3 2 210) >>= (@?= [Only (DecimalValue 3 2 210)])
        (query conn "SELECT count(*) FROM decimals WHERE v = ?" (Only (DecimalValue 2 1 21)) :: IO [Only Int64]) >>= (@?= [Only 1])
        (query conn "SELECT count(*) FROM decimals WHERE v < ?" (Only (DecimalValue 5 2 211)) :: IO [Only Int64]) >>= (@?= [Only 1])
        (query conn "SELECT typeof(? * 2), ? * 2" (value, value) :: IO [(Text, DecimalValue)]) >>= (@?= [("DECIMAL(15,3)", DecimalValue 15 3 4250)])
        mapM_
          roundTrip
          [ DecimalValue 38 0 (10 ^ (38 :: Int) - 1)
          , DecimalValue 38 0 (negate (10 ^ (38 :: Int) - 1))
          , DecimalValue 38 38 1
          , DecimalValue 2 1 (-15)
          , DecimalValue 1 0 7
          ]
        -- The encoder raises an error for a value that it cannot encode.
        forM_ [DecimalValue 2 1 100, DecimalValue 39 0 1, DecimalValue 2 3 1, DecimalValue 0 0 0] \decimal ->
          assertIOError (query conn "SELECT ?" (Only decimal) :: IO [Only DecimalValue])
        (query conn "SELECT ?" (Only (Nothing :: Maybe DecimalValue)) :: IO [Only (Maybe DecimalValue)]) >>= (@?= [Only Nothing])
    , testCase "a Map binds as a MAP parameter in each SQL form" $ withConnection ":memory:" \conn -> do
        let pairs = Map.fromList [("a", 1), ("b", 2)] :: Map.Map Text Int64
            none = Map.empty :: Map.Map Text Int64
            rejected :: (ToField a) => Text -> a -> Assertion
            rejected message value =
              try (query conn "SELECT ?::VARCHAR" (Only value) :: IO [Only Text]) >>= \case
                Left SQLError{sqlErrorMessage, sqlErrorType} -> do
                  sqlErrorMessage @?= message
                  sqlErrorType @?= Just ErrorInputInvalid
                Right rows -> assertFailure ("expected " <> Text.unpack message <> ", got " <> show rows)
        forM_ [pairs, none] \value ->
          (query conn "SELECT typeof(?), ?" (value, value) :: IO [(Text, Map.Map Text Int64)]) >>= (@?= [("MAP(VARCHAR, BIGINT)", value)])
        -- DuckDB casts the keys and the values to the types of the column.
        forM_ ["MAP(VARCHAR, BIGINT)", "MAP(VARCHAR, INTEGER)"] \dtype -> do
          _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE map_value (i BIGINT, m " <> dtype <> ")"))
          _ <- executeMany conn "INSERT INTO map_value VALUES (?, ?)" [(0 :: Int64, pairs), (1, none)]
          (query_ conn "SELECT m FROM map_value ORDER BY i" :: IO [Only (Map.Map Text Int64)])
            >>= assertEqual (Text.unpack dtype) [Only pairs, Only none]
        _ <- execute_ conn "CREATE TABLE integer_keys (m MAP(INTEGER, BIGINT))"
        _ <- execute conn "INSERT INTO integer_keys VALUES (?)" (Only (Map.fromList [("1", 1)] :: Map.Map Text Int64))
        (query_ conn "SELECT m FROM integer_keys" :: IO [Only (Map.Map Int32 Int64)]) >>= (@?= [Only (Map.fromList [(1, 1)])])
        try (execute conn "INSERT INTO integer_keys VALUES (?)" (Only pairs)) >>= \case
          Left SQLError{sqlErrorMessage, sqlErrorType} -> do
            assertBool (Text.unpack sqlErrorMessage) ("Conversion Error: Could not convert string 'a' to INT32" `Text.isPrefixOf` sqlErrorMessage)
            sqlErrorType @?= Just ErrorTypeConversion
          Right (_ :: Int) -> assertFailure "expected the DuckDB key conversion error"
        -- DuckDB compares two MAP values pair by pair in their order. A map binds
        -- its pairs in ascending key order.
        _ <- execute_ conn "CREATE TABLE ordered_maps (m MAP(VARCHAR, BIGINT))"
        _ <- execute_ conn "INSERT INTO ordered_maps VALUES (MAP {'b': 2, 'a': 1}), (MAP {'a': 1, 'b': 2})"
        (query conn "SELECT count(*) FROM ordered_maps WHERE m = ?" (Only pairs) :: IO [Only Int64]) >>= (@?= [Only 1])
        (query_ conn "SELECT m FROM ordered_maps" :: IO [Only (Map.Map Text Int64)]) >>= (@?= [Only pairs, Only pairs])
        ( query conn "SELECT map_extract(?, 'a'), (?)['a'], map_keys(?), element_at(?, 'b')" (pairs, pairs, pairs, pairs)
            :: IO [([Int64], Int64, [Text], [Int64])]
          )
          >>= (@?= [([1], 1, ["a", "b"], [2])])
        -- A Haskell map can hold two NaN keys. DuckDB rejects them and a NULL key.
        let nan = 0 / 0 :: Double
        rejected "Invalid Input Error: Map keys must be unique." (Map.insert nan 2 (Map.fromList [(nan, 1)]) :: Map.Map Double Int64)
        rejected "Invalid Input Error: Map keys can not be NULL." (Map.fromList [(Nothing, 1), (Just "a", 2)] :: Map.Map (Maybe Text) Int64)
    , testCase "a Map binds NULL, nested, Integer, and UTC values" $ withConnection ":memory:" \conn -> do
        let withNull = Map.fromList [("a", Just 1), ("b", Nothing)] :: Map.Map Text (Maybe Int64)
            nested = Map.fromList [("a", [1, 2]), ("b", [])] :: Map.Map Text [Int64]
            pairs = Map.fromList [("a", 1)] :: Map.Map Text Int64
        (query conn "SELECT typeof(?), ?" (withNull, withNull) :: IO [(Text, Map.Map Text (Maybe Int64))])
          >>= (@?= [("MAP(VARCHAR, BIGINT)", withNull)])
        (query conn "SELECT typeof(?), ?" (nested, nested) :: IO [(Text, Map.Map Text [Int64])]) >>= (@?= [("MAP(VARCHAR, BIGINT[])", nested)])
        (query conn "SELECT ?" (Only (Nothing :: Maybe (Map.Map Text Int64))) :: IO [Only (Maybe (Map.Map Text Int64))]) >>= (@?= [Only Nothing])
        (query conn "SELECT typeof(?), ?" ([pairs, Map.empty], [pairs, Map.empty]) :: IO [(Text, [Map.Map Text Int64])])
          >>= (@?= [("MAP(VARCHAR, BIGINT)[]", [pairs, Map.empty])])
        -- An Integer value takes the value type of its target.
        let insert :: Map.Map Text Integer -> IO (Either SQLError Int)
            insert value = do
              _ <- execute_ conn "CREATE OR REPLACE TABLE unsigned_map (m MAP(VARCHAR, UBIGINT))"
              try (execute conn "INSERT INTO unsigned_map VALUES (?)" (Only value))
        insert (Map.fromList [("a", 5), ("b", 6)]) >>= either (assertFailure . show) (const (pure ()))
        (query_ conn "SELECT m::VARCHAR FROM unsigned_map" :: IO [Only Text]) >>= (@?= [Only "{a=5, b=6}"])
        insert (Map.fromList [("a", -5)]) >>= \case
          Left SQLError{sqlErrorType} -> sqlErrorType @?= Just ErrorInputInvalid
          Right _ -> assertFailure "expected the DuckDB range error for -5"
        insert (Map.fromList [("a", 2 ^ (130 :: Int))]) >>= \case
          Left SQLError{sqlErrorType} -> sqlErrorType @?= Just ErrorInputOutOfRange
          Right _ -> assertFailure "expected the duckdb-simple range error for 2^130"
        (query conn "SELECT typeof(?)" (Only (Map.fromList [("a", 5)] :: Map.Map Text Integer)) :: IO [Only Text])
          >>= (@?= [Only "MAP(VARCHAR, BIGNUM)"])
        -- A UTC value keeps its nanoseconds for a nanosecond target.
        let times = Map.fromList [("t", posixSecondsToUTCTime 0.123456789)] :: Map.Map Text UTCTime
        (query conn "SELECT ?::MAP(VARCHAR, TIMESTAMPTZ_NS)" (Only times) :: IO [Only (Map.Map Text UTCTime)]) >>= (@?= [Only times])
        -- An empty map of nested values has no value type.
        try (query conn "SELECT ?" (Only (Map.empty :: Map.Map Text [Int64])) :: IO [Only FieldValue]) >>= \case
          Left SQLError{sqlErrorMessage} ->
            sqlErrorMessage @?= "duckdb-simple: an empty list or map of BIGINT[] elements has no element type for a parameter with no known type"
          Right rows -> assertFailure ("expected the element type error, got " <> show rows)
    , testCase "an array of BigNum values keeps the BIGNUM casts of DuckDB" $ withConnection ":memory:" \conn -> do
        -- DuckDB 2.0.0 casts the BIGNUM -6 to UBIGINT as a wrapped value with no
        -- error. See Note [Integer parameters] in "Database.DuckDB.Simple.ToField".
        forM_
          [ ("BIGINT[2]", [BigNum 5, BigNum 6], "[5, 6]")
          , ("BIGNUM[2]", [BigNum 5, BigNum 6], "[5, 6]")
          , ("UBIGINT[2]", [BigNum 5, BigNum (-6)], "[5, 18446744073709551610]")
          ]
          \(dtype, values, expected) -> do
            _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE bignum_array (v " <> dtype <> ")"))
            _ <- execute conn "INSERT INTO bignum_array VALUES (?)" (Only (listArray (0, 1) values :: Array Int BigNum))
            (query_ conn "SELECT v::VARCHAR FROM bignum_array" :: IO [Only Text]) >>= assertEqual (Text.unpack dtype) [Only expected]
    , testCase "finite timestamp extrema preserve all units" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
        forM_
          [ (Seconds, 1, "TIMESTAMP_S")
          , (Milliseconds, 1000, "TIMESTAMP_MS")
          , (Microseconds, 1000000, "TIMESTAMP")
          , (Nanoseconds, 1000000000, "TIMESTAMP_NS")
          ]
          \(constructor, units, dtype) ->
            forM_ [minBound, negate (maxBound :: Int64) + 1, -1, 0, maxBound - 1] \value -> do
              let expected = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (toInteger value % units)))
              appendTimestamp conn dtype (constructor value)
              (query_ conn "SELECT value FROM native_timestamp" :: IO [Only LocalTime]) >>= (@?= [Only expected])
              [Only original] <- query_ conn "SELECT {'value': value} FROM native_timestamp" :: IO [Only (StructValue FieldValue)]
              -- DuckDB needs an explicit parameter type for extreme S/MS values.
              query conn (fromString ("SELECT ?::STRUCT(value " <> dtype <> ")")) (Only original) >>= (@?= [Only original])
              _ <- execute_ conn "DROP TABLE native_timestamp"
              pure ()
    , testCase "composite timestamps reject infinity and storage overflow" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
        forM_
          [ (LogicalTypeIdTimestampSec, 1)
          , (LogicalTypeIdTimestampMs, 1000)
          , (LogicalTypeIdTimestamp, 1000000)
          , (LogicalTypeIdTimestampNs, 1000000000)
          ]
          \(dtype, units) ->
            forM_
              [ toInteger (minBound :: Int64) - 1
              , negate (toInteger (maxBound :: Int64))
              , toInteger (maxBound :: Int64)
              , toInteger (maxBound :: Int64) + 1
              ]
              \value -> do
                let timestamp = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (value % units)))
                    struct = singleField (LogicalTypeScalar dtype) (FieldTimestamp (Finite timestamp))
                assertIOError (query conn "SELECT ?" (Only struct) :: IO [Only FieldValue])
    , testCase "composite timestamps floor fractional units before the epoch" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
        forM_
          [ (LogicalTypeIdTimestampSec, 1)
          , (LogicalTypeIdTimestampMs, 1000)
          , (LogicalTypeIdTimestamp, 1000000)
          , (LogicalTypeIdTimestampNs, 1000000000)
          ]
          \(dtype, units) -> do
            let timestamp = utcToLocalTime utc (posixSecondsToUTCTime (fromRational ((-1) % (2 * units))))
                expected = utcToLocalTime utc (posixSecondsToUTCTime (fromRational ((-1) % units)))
                struct = singleField (LogicalTypeScalar dtype) (FieldTimestamp (Finite timestamp))
            (query conn "SELECT (?).value" (Only struct) :: IO [Only LocalTime]) >>= (@?= [Only expected])
    , testCase "composite TIME_NS rejects invalid clock components" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
        forM_
          [TimeOfDay (-1) 0 0, TimeOfDay 25 0 0, TimeOfDay 0 60 0, TimeOfDay 0 0 (-1), TimeOfDay 24 0 0.000000001, TimeOfDay 23 59 60.000000001]
          \value ->
            assertIOError (query conn "SELECT ?" (Only (singleField (LogicalTypeScalar LogicalTypeIdTimeNs) (FieldTime value))) :: IO [Only FieldValue])
    , testCase "microsecond timestamps retain finite extrema" $ withConnection ":memory:" \conn ->
        forM_ [minBound, negate (maxBound :: Int64) + 1, -1, 0, maxBound - 1] \value -> do
          let expected = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (toInteger value % 1000000)))
          _ <- execute_ conn "CREATE TABLE microsecond_timestamp(value TIMESTAMP)"
          _ <- execute conn "INSERT INTO microsecond_timestamp VALUES (?)" (Only expected)
          (query_ conn "SELECT value FROM microsecond_timestamp" :: IO [Only LocalTime]) >>= (@?= [Only expected])
          _ <- execute_ conn "DROP TABLE microsecond_timestamp"
          pure ()
    , testCase "out-of-range dates and timestamps fail without abort" $ withConnection ":memory:" \conn -> do
        let hugeDay = fromGregorian 10000000 1 1
            local = LocalTime hugeDay (TimeOfDay 0 0 0)
        assertIOError (query conn "SELECT ?" (Only hugeDay) :: IO [Only Day])
        assertIOError (query conn "SELECT ?" (Only local) :: IO [Only LocalTime])
        assertIOError (query conn "SELECT ?" (Only (UTCTime hugeDay 0)) :: IO [Only UTCTime])
    , testCase "date finite storage extrema round trip" $ withConnection ":memory:" \conn ->
        forM_ [minBound, negate (maxBound :: Int32) + 1, -1, 0, maxBound - 1] \value -> do
          let day = addDays (toInteger value) (fromGregorian 1970 1 1)
          (query conn "SELECT ?" (Only day) :: IO [Only Day]) >>= (@?= [Only day])
    , testCase "input infinity sentinels are rejected" $ withConnection ":memory:" \conn -> do
        forM_ [toInteger (maxBound :: Int32), negate (toInteger (maxBound :: Int32))] \days ->
          assertIOError (query conn "SELECT ?" (Only (addDays days (fromGregorian 1970 1 1))) :: IO [Only Day])
        forM_ [toInteger (maxBound :: Int64), negate (toInteger (maxBound :: Int64))] \micros -> do
          let value = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (micros % 1000000)))
          assertIOError (query conn "SELECT ?" (Only value) :: IO [Only LocalTime])
    , testCase "UTC binding is TIMESTAMPTZ under a non-UTC zone" $ withConnection ":memory:" \conn -> do
        _ <- execute_ conn "SET TimeZone = 'Pacific/Auckland'"
        let value = UTCTime (fromGregorian 2000 1 2) 12345
        (query conn "SELECT typeof(?), ?" (value, value) :: IO [(Text, UTCTime)]) >>= (@?= [("TIMESTAMP WITH TIME ZONE", value)])
        _ <- execute_ conn "CREATE TABLE utc_binding(value TIMESTAMPTZ)"
        _ <- execute conn "INSERT INTO utc_binding VALUES (?)" (Only value)
        (query_ conn "SELECT value FROM utc_binding" :: IO [Only UTCTime]) >>= (@?= [Only value])
    , testCase "DECIMAL(38) retains exact integer precision" $ withConnection ":memory:" \conn -> do
        let decimal = DecimalValue 38 9 12345678901234567890123456789012345678
            struct = singleField (LogicalTypeDecimal 38 9) (FieldDecimal decimal)
        [Only result] <- query conn "SELECT ?" (Only struct) :: IO [Only (StructValue FieldValue)]
        structValueFields result @?= structValueFields struct
    , testCase "DECIMAL and ENUM columns decode at each storage width" $ withConnection ":memory:" \conn -> do
        -- Each column has a NULL between values, so one reader serves valid and invalid rows.
        forM_ [(4, 1), (9, 3), (18, 3), (38, 9)] \(width, scale) -> do
          let dtype = "DECIMAL(" <> Text.pack (show width) <> "," <> Text.pack (show scale) <> ")"
              decimal units = FieldDecimal (DecimalValue width scale (units * 10 ^ scale))
          rows <-
            query_ conn (Query ("SELECT CASE WHEN i = 1 THEN NULL ELSE (i - 2)::" <> dtype <> " END FROM range(4) t(i)")) :: IO [Only FieldValue]
          rows @?= map Only [decimal (-2), FieldNull, decimal 0, decimal 1]
        forM_ [200, 300, 70000 :: Int] \size -> do
          let label index = "v" <> Text.pack (show index)
              dtype = "enum_width_" <> Text.pack (show size)
          _ <-
            execute_
              conn
              (Query ("CREATE TYPE " <> dtype <> " AS ENUM (" <> Text.intercalate "," ["'" <> label index <> "'" | index <- [0 .. size - 1]] <> ")"))
          rows <-
            query_ conn (Query ("SELECT x::" <> dtype <> " FROM (VALUES ('v0'), (NULL), ('" <> label (size - 1) <> "')) t(x)")) :: IO [Only FieldValue]
          rows @?= map Only [FieldEnum 0, FieldNull, FieldEnum (fromIntegral (size - 1))]
    , testCase "invalid DECIMAL width scale and magnitude are rejected" $ withConnection ":memory:" \conn ->
        forM_ [DecimalValue 0 0 0, DecimalValue 39 0 0, DecimalValue 2 3 1, DecimalValue 2 0 100, DecimalValue 2 0 (-100)] \decimal ->
          assertIOError
            ( query conn "SELECT ?" (Only (singleField (LogicalTypeDecimal (decimalWidth decimal) (decimalScale decimal)) (FieldDecimal decimal)))
                :: IO [Only FieldValue]
            )
    , testCase "nested conversion failure leaves the connection usable" $ withConnection ":memory:" \conn -> do
        let days = listArray (0, 1) [fromGregorian 2000 1 1, fromGregorian 10000000 1 1] :: Array Int Day
        assertIOError (query conn "SELECT ?" (Only days) :: IO [Only FieldValue])
        (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
    , testCase "generic NULL product and payload return Left" $ do
        assertLeft (genericFromFieldValue FieldNull :: Either String NamedRecord)
        let member = UnionMemberType "DataMember" (LogicalTypeScalar LogicalTypeIdBigInt)
            union = UnionValue 0 "DataMember" FieldNull (listArray (0, 0) [member])
        assertLeft (genericFromFieldValue (FieldUnion union) :: Either String NullableSum)
    , testCase "generic records decode by name" $ withConnection ":memory:" \conn -> do
        [Only struct] <- query_ conn "SELECT {'secondValue': 2::BIGINT, 'firstValue': 1::BIGINT}" :: IO [Only (StructValue FieldValue)]
        genericFromFieldValue (FieldStruct struct) @?= Right (NamedRecord 1 2)
    , testCase "generic sums decode by member name" $ withConnection ":memory:" \conn -> do
        [Only union] <- query_ conn "SELECT union_value(DataMember := {'field1': 42::BIGINT})" :: IO [Only (UnionValue FieldValue)]
        genericFromFieldValue (FieldUnion union) @?= Right (DataMember 42)
    , testCase "union binding rejects inconsistent member name" $ withConnection ":memory:" \conn -> do
        let member = UnionMemberType "right" (LogicalTypeScalar LogicalTypeIdBigInt)
            union = UnionValue 0 "wrong" (FieldInt64 42) (listArray (0, 0) [member])
        assertIOError (query conn "SELECT ?" (Only union) :: IO [Only FieldValue])
    , testCase "generic records reject wrong names" $ do
        case genericToStructValue (NamedRecord 1 2) of
          Nothing -> assertFailure "missing generic struct"
          Just struct -> do
            let fields = listArray (0, 1) [StructField "wrong" (FieldInt64 1), StructField "secondValue" (FieldInt64 2)]
            assertLeft (genericFromFieldValue (FieldStruct struct{structValueFields = fields}) :: Either String NamedRecord)
    , testCase "unnamed tuples preserve scalar and nested fields on round trip" $ withConnection ":memory:" \conn -> do
        let tuple =
              TupleValue
                (listArray (0, 1) [FieldInt64 42, FieldText "λ"])
                (listArray (0, 1) [LogicalTypeScalar LogicalTypeIdBigInt, LogicalTypeScalar LogicalTypeIdVarchar])
        (query_ conn "SELECT row(42::BIGINT, 'λ')" :: IO [Only (TupleValue FieldValue)]) >>= (@?= [Only tuple])
        (query_ conn "SELECT [row(42::BIGINT, 'λ')]" :: IO [Only [TupleValue FieldValue]]) >>= (@?= [Only [tuple]])
        (query conn "SELECT ?" (Only tuple) :: IO [Only (TupleValue FieldValue)]) >>= (@?= [Only tuple])
    , testCase "empty tuples retain their type independently of empty structs" $ withConnection ":memory:" \conn -> do
        let tuple = TupleValue (listArray (0, -1) []) (listArray (0, -1) []) :: TupleValue FieldValue
            struct = StructValue (listArray (0, -1) []) (listArray (0, -1) []) Map.empty :: StructValue FieldValue
        (query_ conn "SELECT row(), [row()]" :: IO [(TupleValue FieldValue, [TupleValue FieldValue])]) >>= (@?= [(tuple, [tuple])])
        (query conn "SELECT typeof(?), ?" (tuple, tuple) :: IO [(Text, TupleValue FieldValue)]) >>= (@?= [("TUPLE", tuple)])
        (query conn "SELECT typeof(?), ?" (struct, struct) :: IO [(Text, StructValue FieldValue)]) >>= (@?= [("STRUCT", struct)])
        withConnectionHandle conn \native -> withConnectionFactory native \factory ->
          bracket (logicalTypeFromRep factory (tupleValueTypeRep tuple)) destroyLogicalType \logical ->
            logicalTypeToRep logical >>= (@?= tupleValueTypeRep tuple)
        assertIOError (query conn "SELECT ?" (Only tuple{tupleValueFields = listArray (4, 4) [FieldNull]}) :: IO [Only FieldValue])
    , testCase "filtered tuple cursors retain positional fields" $ withConnection ":memory:" \conn -> do
        rows <- Streaming.fold_ conn "SELECT row(i::BIGINT, 'λ') FROM range(6) t(i) WHERE i % 2 = 1" [] $ \values (Only tuple) ->
          pure (values <> [elems (tupleValueFields tuple)])
        rows @?= [[FieldInt64 value, FieldText "λ"] | value <- [1, 3, 5]]
    , testCase "composite VARCHAR collations retain native comparison semantics" $ withConnection ":memory:" \conn -> do
        let collated = LogicalTypeCollatedVarchar "nocase"
        [Only original] <-
          query_ conn "SELECT {'text':'A' COLLATE nocase,'items':['B' COLLATE nocase], 'optional':NULL::VARCHAR COLLATE nocase}"
            :: IO [Only (StructValue FieldValue)]
        map structFieldValue (elems (structValueTypes original)) @?= [collated, LogicalTypeList collated, collated]
        ( query conn "SELECT value.text='a', value.items[1]='b', value.optional IS NULL FROM (SELECT ? AS value)" (Only original)
            :: IO [(Bool, Bool, Bool)]
          )
          >>= (@?= [(True, True, True)])
        (query conn "SELECT ?" (Only original) :: IO [Only (StructValue FieldValue)]) >>= (@?= [Only original])
        [Only tuple] <- query_ conn "SELECT row('A' COLLATE nocase)" :: IO [Only (TupleValue FieldValue)]
        (query conn "SELECT value[1]='a' FROM (SELECT ? AS value)" (Only tuple) :: IO [Only Bool]) >>= (@?= [Only True])
        withConnectionHandle conn \native -> withConnectionFactory native \factory ->
          bracket (logicalTypeFromRep factory collated) destroyLogicalType \logical -> logicalTypeToRep logical >>= (@?= collated)
    , testCase "logical type Unicode names round trip" $ withConnection ":memory:" \conn -> withConnectionHandle conn \native -> withConnectionFactory native \factory -> do
        let logical = LogicalTypeStruct (listArray (0, 0) [StructField "íslenska_λ_😀" (LogicalTypeScalar LogicalTypeIdBigInt)])
        bracket (logicalTypeFromRep factory logical) destroyLogicalType \handle ->
          logicalTypeToRep handle >>= (@?= logical)
    , testCase "logical type names reject embedded NUL" $ withConnection ":memory:" \conn -> withConnectionHandle conn \native -> withConnectionFactory native \factory -> do
        let logical = LogicalTypeStruct (listArray (0, 0) [StructField "before\0after" (LogicalTypeScalar LogicalTypeIdBigInt)])
        assertIOError (bracket (logicalTypeFromRep factory logical) destroyLogicalType (const (pure ())))
    , testCase "invalid native composite constructor returns controlled error" $ withConnection ":memory:" \conn -> do
        let struct =
              singleField
                (LogicalTypeMap (LogicalTypeScalar LogicalTypeIdBigInt) (LogicalTypeScalar LogicalTypeIdBigInt))
                (FieldMap [(FieldNull, FieldInt64 1)])
        result <- try (query conn "SELECT ?" (Only struct) :: IO [Only FieldValue])
        case result of
          Left SQLError{sqlErrorType} -> sqlErrorType @?= Just ErrorInputInvalid
          Right _ -> assertFailure "expected a V2 native constructor error"
    , testCase "BIT padding preserves SQL bit_count" $ withConnection ":memory:" \conn -> do
        let bits = bsFromBool [True, False, True]
        (query conn "SELECT bit_count(?), ?" (bits, bits) :: IO [(Int64, BitString)]) >>= (@?= [(2, bits)])
    , testCase "unsupported BIT input fails before native use" $ withConnection ":memory:" \conn ->
        forM_ [BitString 0 BS.empty, BitString 8 (BS.singleton 1)] \bits ->
          assertIOError (query conn "SELECT ?" (Only bits) :: IO [Only BitString])
    , testCase "TIMETZ offsets with seconds read without rounding" $ withConnection ":memory:" \conn ->
        (query_ conn "SELECT '12:00:00+01:23:45'::TIMETZ" :: IO [Only TimeWithZone]) >>= (@?= [Only (TimeWithZone (TimeOfDay 12 0 0) 5025)])
    , testCase "a failure of the reader of a chunk gives the error of DuckDB and releases each chunk" $ withConnection ":memory:" \conn -> do
        -- The VARIANT reader runs its metadata query on a helper connection, and
        -- that query needs an expression depth of 11. A GLOBAL depth of 10 thus
        -- fails the reader at the bind of that query, and the SESSION depth of
        -- the caller lets the query of the case run.
        _ <- execute_ conn "SET GLOBAL max_expression_depth = 10"
        _ <- execute_ conn "SET SESSION max_expression_depth = 1000"
        let rows = "SELECT 42::BIGINT::VARIANT FROM range(10000)"
        try (query_ conn rows :: IO [Only FieldValue]) >>= \case
          Left SQLError{sqlErrorMessage, sqlErrorType} -> do
            assertBool (Text.unpack sqlErrorMessage) ("expression depth" `Text.isInfixOf` sqlErrorMessage)
            sqlErrorType @?= Just ErrorQueryBinder
          Right result -> assertFailure ("expected the error of the VARIANT reader, got " <> show (length result) <> " rows")
        (query_ conn "SELECT 42" :: IO [Only Int]) >>= (@?= [Only 42])
        _ <- execute_ conn "SET GLOBAL max_expression_depth = 1000"
        (length <$> (query_ conn rows :: IO [Only FieldValue])) >>= (@?= 10000)
    , testCase "a failure of the reader of a later chunk of a stream gives the error of DuckDB" $
        withEnvironment \environment -> withInstance environment [] \instance' ->
          withConnectionOn instance' \conn -> withConnectionOn instance' \other -> do
            _ <- execute_ conn "ATTACH ':memory:' AS probe"
            _ <- execute_ conn "SET SESSION max_expression_depth = 1000"
            -- The first chunk reads. Then a GLOBAL depth of 10 fails the
            -- metadata query of the VARIANT reader for the next chunk. See the
            -- case before this one.
            let step count (Only (_ :: FieldValue)) = do
                  _ <- if count == 0 then execute_ other "SET GLOBAL max_expression_depth = 10" else pure 0
                  pure (count + 1)
            try (Streaming.fold_ conn "SELECT 42::BIGINT::VARIANT FROM range(10000)" (0 :: Int) step) >>= \case
              Left SQLError{sqlErrorMessage} -> assertBool (Text.unpack sqlErrorMessage) ("expression depth" `Text.isInfixOf` sqlErrorMessage)
              Right count -> assertFailure ("expected the error of the VARIANT reader, got " <> show count <> " rows")
            (query_ conn "SELECT 42" :: IO [Only Int]) >>= (@?= [Only 42])
    , testCase "ENUM payload index must be in its dictionary" $ withConnection ":memory:" \conn ->
        assertIOError
          (query conn "SELECT ?" (Only (singleField (LogicalTypeEnum (listArray (0, 1) ["a", "b"])) (FieldEnum 2))) :: IO [Only FieldValue])
    , testCase "ENUM payload index binds the label of its dictionary" $ withConnection ":memory:" \conn -> do
        let struct = singleField (LogicalTypeEnum (listArray (0, 2) ["a", "b", "c"])) (FieldEnum 1)
        (query conn "SELECT typeof((?).value), (?).value::VARCHAR, ?" (struct, struct, struct) :: IO [(Text, Text, StructValue FieldValue)])
          >>= (@?= [("ENUM('a', 'b', 'c')", "b", struct)])
    , testCase "ENUM values of each dictionary size bind their labels" $ withConnection ":memory:" \conn ->
        -- DuckDB stores the index of a dictionary of 200, 300, and 70000 entries
        -- in 8, 16, and 32 bits.
        forM_ [200, 300, 70000 :: Int] \size -> do
          let label index = "v" <> Text.pack (show index)
              entries = listArray (0, size - 1) (map label [0 .. size - 1])
          forM_ [0, size - 1] \index -> do
            let struct = singleField (LogicalTypeEnum entries) (FieldEnum (fromIntegral index))
            (query conn "SELECT (?).value::VARCHAR, ?" (struct, struct) :: IO [(Text, StructValue FieldValue)])
              >>= assertEqual (show size <> " " <> show index) [(label index, struct)]
    , testCase "TIMETZ input offset overflow fails before native use" $ withConnection ":memory:" \conn -> do
        let value = TimeWithZone (TimeOfDay 12 0 0) maxBound
            struct = singleField (LogicalTypeScalar LogicalTypeIdTimeTz) (FieldTimeTZ value)
        assertIOError (query conn "SELECT ?" (Only struct) :: IO [Only FieldValue])
    , testCase "invalid TIME input returns controlled error" $ withConnection ":memory:" \conn ->
        assertIOError (query conn "SELECT ?" (Only (TimeOfDay 1000000000 0 0)) :: IO [Only TimeOfDay])
    , testCase "nested types decode the same in each chunk and each read mode" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
        _ <- execute_ conn "CREATE TYPE mood AS ENUM ('calm', 'busy', 'tired')"
        let sql =
              "SELECT st, [st, st] AS l, [st, st]::STRUCT(n BIGINT, e mood)[2] AS a, tiers, [tiers, tiers] AS tl, \
              \CASE WHEN i % 2 = 0 THEN union_value(d := ((i % 1000) / 10)::DECIMAL(9, 2))::UNION(d DECIMAL(9, 2), s STRUCT(n BIGINT, e mood)) \
              \ELSE union_value(s := st)::UNION(d DECIMAL(9, 2), s STRUCT(n BIGINT, e mood)) END AS u, \
              \map([st.e], [((i % 1000) / 10)::DECIMAL(9, 2)]) AS m, i::DECIMAL(18, 3) AS d \
              \FROM (SELECT i, {'n': i, 'e': (['calm', 'busy', 'tired'])[i % 3 + 1]::mood} AS st, \
              \{'small': ((i % 1000) / 10)::DECIMAL(4, 1), 'large': (i * 1000 + 7)::DECIMAL(18, 3)} AS tiers FROM range(5000) r(i))"
            mood = LogicalTypeEnum (listArray (0, 2) ["calm", "busy", "tired"])
            bigint = LogicalTypeScalar LogicalTypeIdBigInt
            stType = LogicalTypeStruct (listArray (0, 1) [StructField "n" bigint, StructField "e" mood])
            members = listArray (0, 1) [UnionMemberType "d" (LogicalTypeDecimal 9 2), UnionMemberType "s" stType]
            decimal width scale integer = FieldDecimal (DecimalValue width scale integer)
            expected :: Int64 -> [FieldValue]
            expected i =
              let st = FieldStruct (structOf [("n", FieldInt64 i, bigint), ("e", FieldEnum (fromIntegral (i `mod` 3)), mood)])
                  tenths = toInteger (i `mod` 1000)
                  tiers =
                    FieldStruct
                      ( structOf
                          [ ("small", decimal 4 1 tenths, LogicalTypeDecimal 4 1)
                          , ("large", decimal 18 3 ((toInteger i * 1000 + 7) * 1000), LogicalTypeDecimal 18 3)
                          ]
                      )
                  union
                    | even i = UnionValue 0 "d" (decimal 9 2 (tenths * 10)) members
                    | otherwise = UnionValue 1 "s" st members
              in  [ st
                  , FieldList [st, st]
                  , FieldArray (listArray (0, 1) [st, st])
                  , tiers
                  , FieldList [tiers, tiers]
                  , FieldUnion union
                  , FieldMap [(FieldEnum (fromIntegral (i `mod` 3)), decimal 9 2 (tenths * 10))]
                  , decimal 18 3 (toInteger i * 1000)
                  ]
        materialized <- query_ conn sql :: IO [[FieldValue]]
        cursor <- withStatement conn sql (readAllRows nextRow)
        streamed <- withStatement conn sql (readAllRows Streaming.nextRow)
        length materialized @?= 5000
        cursor @?= materialized
        streamed @?= materialized
        forM_ [0, 2047, 2048, 4095, 4999 :: Int] \i -> (materialized !! i) @?= expected (fromIntegral i)
    , testCase "a statement that runs again with another type reads the new type" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
        withStatement conn "SELECT ? AS s FROM range(3000) r(i)" \stmt ->
          forM_ ([nextRow, Streaming.nextRow] :: [Statement -> IO (Maybe (Only (StructValue FieldValue)))]) \next ->
            forM_ [DecimalValue 4 1 123, DecimalValue 18 3 123456789012, DecimalValue 4 1 (-45)] \value -> do
              let struct = singleField (LogicalTypeDecimal (decimalWidth value) (decimalScale value)) (FieldDecimal value)
              bind stmt [toField struct]
              readAllRows next stmt >>= (@?= replicate 3000 (Only struct))
    , testCase "an Integer outside the HUGEINT range raises the range error for each integer type" $ withConnection ":memory:" \conn -> do
        -- A BIGNUM value for these types stores a wrong value with no error.
        -- See Note [Integer parameters] in "Database.DuckDB.Simple.ToField".
        let huge = 2 ^ (128 :: Int) + 5 :: Integer
            insert :: Text -> Integer -> IO (Either SQLError Int)
            insert dtype value = do
              _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE wide_integer (v " <> dtype <> ")"))
              result <- try (execute conn "INSERT INTO wide_integer VALUES (?)" (Only value))
              (query_ conn "SELECT count(*) FROM wide_integer" :: IO [Only Int64]) >>= assertEqual (Text.unpack dtype <> " " <> show value) [Only 0]
              pure result
        forM_ ["TINYINT", "SMALLINT", "INTEGER", "BIGINT", "HUGEINT", "UTINYINT", "USMALLINT", "UINTEGER", "UBIGINT", "UHUGEINT"] \dtype -> do
          -- DuckDB also rejects the value for the type.
          try (query_ conn (Query ("SELECT '" <> Text.pack (show huge) <> "'::" <> dtype)) :: IO [Only Text]) >>= \case
            Left SQLError{sqlErrorMessage} ->
              assertBool (Text.unpack sqlErrorMessage) ("Conversion Error: Could not convert string" `Text.isPrefixOf` sqlErrorMessage)
            Right rows -> assertFailure ("DuckDB accepts " <> show huge <> " as " <> Text.unpack dtype <> ": " <> show rows)
          forM_ [huge, negate huge] \value ->
            insert dtype value >>= \case
              Left SQLError{sqlErrorMessage, sqlErrorType} -> do
                sqlErrorType @?= Just ErrorInputOutOfRange
                sqlErrorMessage @?= "duckdb-simple: the value " <> Text.pack (show value) <> " is out of range for a parameter of the type " <> dtype
              Right _ -> assertFailure ("expected the duckdb-simple range error for " <> Text.unpack dtype <> " " <> show value)
        -- An element of an array takes the element type of its target. DuckDB
        -- then checks the range of each element.
        forM_ ["UTINYINT[2]", "USMALLINT[2]", "UINTEGER[2]", "UBIGINT[2]"] \dtype -> do
          _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE wide_integer (v " <> dtype <> ")"))
          result <- try (execute conn "INSERT INTO wide_integer VALUES (?)" (Only (listArray (0, 1) [5, -6] :: Array Int Integer)))
          (query_ conn "SELECT count(*) FROM wide_integer" :: IO [Only Int64]) >>= assertEqual (Text.unpack dtype) [Only 0]
          case result of
            Left SQLError{sqlErrorMessage, sqlErrorType} -> do
              assertBool
                (Text.unpack sqlErrorMessage)
                ("-6 can't be cast because the value is out of range for the destination type" `Text.isInfixOf` sqlErrorMessage)
              sqlErrorType @?= Just ErrorInputInvalid
            Right _ -> assertFailure ("expected the DuckDB range error for " <> Text.unpack dtype)
    , testCase "DECIMAL columns decode the largest value at each storage boundary" $ withConnection ":memory:" \conn ->
        -- DuckDB stores a DECIMAL of the width 1 to 4 in 16 bits, 5 to 9 in 32 bits,
        -- 10 to 18 in 64 bits, and 19 to 38 in 128 bits. The text comes from DuckDB.
        forM_ [4, 5, 9, 10, 18, 19, 38] \width -> do
          let largest = 10 ^ width - 1 :: Integer
              dtype = "DECIMAL(" <> Text.pack (show width) <> ",0)"
              sql =
                "SELECT x::"
                  <> dtype
                  <> ", x::"
                  <> dtype
                  <> "::VARCHAR FROM (VALUES ("
                  <> Text.pack (show (negate largest))
                  <> "), (NULL), ("
                  <> Text.pack (show largest)
                  <> ")) t(x)"
          rows <- query_ conn (Query sql) :: IO [(FieldValue, Maybe Text)]
          assertEqual
            (Text.unpack dtype)
            [ (FieldDecimal (DecimalValue width 0 (negate largest)), Just (Text.pack (show (negate largest))))
            , (FieldNull, Nothing)
            , (FieldDecimal (DecimalValue width 0 largest), Just (Text.pack (show largest)))
            ]
            rows
    , testCase "ENUM columns decode the last label at each storage boundary" $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
        -- DuckDB stores the index of an ENUM with at most 255 labels in 8 bits, with
        -- at most 65535 labels in 16 bits, and with more labels in 32 bits.
        forM_ [255, 256, 65535, 65536 :: Int] \size -> do
          let dtype = "enum_boundary_" <> Text.pack (show size)
              lastLabel = "v" <> Text.pack (show (size - 1))
          _ <- execute_ conn (Query ("CREATE TYPE " <> dtype <> " AS ENUM (SELECT 'v' || i FROM range(" <> Text.pack (show size) <> ") t(i))"))
          rows <-
            query_
              conn
              ( Query
                  ( "SELECT x::"
                      <> dtype
                      <> ", enum_code(x::"
                      <> dtype
                      <> ")::BIGINT, x::"
                      <> dtype
                      <> "::VARCHAR FROM (VALUES ('v0'), (NULL), ('"
                      <> lastLabel
                      <> "')) t(x)"
                  )
              )
              :: IO [(FieldValue, Maybe Int64, Maybe Text)]
          -- DuckDB gives the index and the label of each row.
          assertEqual
            (Text.unpack dtype)
            [(Just 0, Just "v0"), (Nothing, Nothing), (Just (fromIntegral (size - 1)), Just lastLabel)]
            [(index, label) | (_, index, label) <- rows]
          forM_ rows \(value, index, label) -> assertEqual (Text.unpack dtype) (enumField index label) value
    , testCase "NULL TUPLE and UNION rows decode as NULL in each chunk" $ withConnection ":memory:" \conn -> do
        rows <-
          query_
            conn
            "SELECT t, u, t IS NULL, u IS NULL FROM (SELECT CASE WHEN i % 7 = 1 THEN NULL ELSE row(i, 'x') END AS t, CASE WHEN i % 7 = 2 THEN NULL ELSE union_value(n := i)::UNION(n BIGINT, s VARCHAR) END AS u FROM range(3000) r(i))"
            :: IO [(FieldValue, FieldValue, Bool, Bool)]
        length rows @?= 3000
        -- DuckDB gives the NULL rows in the last two columns.
        forM_ (zip [0 :: Int ..] rows) \(row, (tuple, member, tupleIsNull, memberIsNull)) -> do
          assertEqual ("TUPLE row " <> show row) tupleIsNull (tuple == FieldNull)
          assertEqual ("UNION row " <> show row) memberIsNull (member == FieldNull)
    , testCase "narrowing reads reject the values that DuckDB rejects for the type" $ withConnection ":memory:" \conn -> do
        let rejects :: forall a. (FromField a, Show a) => Proxy a -> Text -> Text -> Assertion
            rejects _ value sqlType = do
              -- DuckDB raises an error for the same cast.
              try (query_ conn (Query ("SELECT (" <> value <> ")::" <> sqlType)) :: IO [Only Text]) >>= \case
                Left SQLError{sqlErrorMessage} ->
                  assertBool
                    (Text.unpack sqlErrorMessage)
                    ("can't be cast because the value is out of range for the destination type" `Text.isInfixOf` sqlErrorMessage)
                Right rows -> assertFailure ("DuckDB accepts " <> Text.unpack value <> " as " <> Text.unpack sqlType <> ": " <> show rows)
              -- The read raises the conversion error of the query.
              let sql = Query ("SELECT " <> value)
              try (query_ conn sql :: IO [Only a]) >>= \case
                Left SQLError{sqlErrorType, sqlErrorQuery} -> do
                  assertEqual (Text.unpack value) Nothing sqlErrorType
                  assertEqual (Text.unpack value) (Just sql) sqlErrorQuery
                Right rows -> assertFailure (Text.unpack value <> " decodes as " <> show rows)
              -- The cause of that error is a ResultError.
              [Only native] <- query_ conn sql :: IO [Only FieldValue]
              case fromField Field{fieldName = "value", fieldIndex = 0, fieldValue = native} :: Ok a of
                Errors errors ->
                  assertBool ("expected a ResultError, got " <> show errors) (not (null errors) && all isResultError errors)
                Ok decoded -> assertFailure (Text.unpack value <> " converts to " <> show decoded)
            isResultError failure = isJust (fromException failure :: Maybe ResultError)
        rejects (Proxy :: Proxy Word64) "-1::BIGINT" "UBIGINT"
        rejects (Proxy :: Proxy Word) "-1::BIGINT" "UBIGINT"
        rejects (Proxy :: Proxy Word32) "-1::INTEGER" "UINTEGER"
        rejects (Proxy :: Proxy Int16) "32768::INTEGER" "SMALLINT"
        rejects (Proxy :: Proxy Int32) "2147483648::BIGINT" "INTEGER"
        rejects (Proxy :: Proxy Int64) "1180591620717411303424::HUGEINT" "BIGINT"
    ]
      <> [ testCase ("composite temporal round trip: " <> dtype) $ withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
             forM_ ("NULL" : map (\value -> "'" <> value <> "'") values) \value ->
               assertTemporalRoundTrip conn dtype (value <> "::" <> dtype)
         | (dtype, values) <-
             [ (dtype, ["-infinity", "infinity", "1969-12-31 23:59:59.123456789", "2000-01-01 12:34:56.123456789"])
             | dtype <- ["TIMESTAMP", "TIMESTAMP_S", "TIMESTAMP_MS", "TIMESTAMP_NS", "TIMESTAMPTZ"]
             ]
               <> [("DATE", ["-infinity", "infinity", "1969-12-31", "2000-01-01"])]
               <> [("TIME_NS", ["00:00:00", "12:34:56.123456789", "23:59:59.999999999", "24:00:00"])]
         ]
      <> [ testCase (dtype <> " " <> value <> " rejects a finite result type") $ withConnection ":memory:" \conn ->
             assertConversionError (query_ conn (fromString ("SELECT '" <> value <> "'::" <> dtype)) :: IO [Only LocalTime])
         | dtype <- ["DATE", "TIMESTAMP", "TIMESTAMP_S", "TIMESTAMP_MS", "TIMESTAMP_NS", "TIMESTAMPTZ"]
         , value <- ["infinity", "-infinity"]
         ]

-- | Rebind native temporal values in structs, unions, and nested collections.
assertTemporalRoundTrip :: Connection -> String -> String -> Assertion
assertTemporalRoundTrip conn dtype expression = do
  let sql =
        "WITH temporal AS (SELECT "
          <> expression
          <> " AS value) "
          <> "SELECT {'scalar': value, 'list': [value, NULL], 'array': [value, NULL]::"
          <> dtype
          <> "[2], 'map': map([1], [value])}, union_value(value := value) FROM temporal"
  [original] <- query_ conn (fromString sql) :: IO [(StructValue FieldValue, UnionValue FieldValue)]
  query conn "SELECT ?, ?" original >>= (@?= [original])

-- | Store raw V2 timestamp units through the parameter API.
appendTimestamp :: Connection -> String -> NativeTimestamp -> IO ()
appendTimestamp conn dtype value = do
  _ <- execute_ conn (fromString ("CREATE TABLE native_timestamp(value " <> dtype <> ")"))
  _ <- execute conn "INSERT INTO native_timestamp VALUES (?)" (Only value)
  pure ()

-- | Build a struct from the name, the value, and the type of each field.
structOf :: [(Text, FieldValue, LogicalTypeRep)] -> StructValue FieldValue
structOf fields =
  StructValue
    (listArray (0, length fields - 1) [StructField name value | (name, value, _) <- fields])
    (listArray (0, length fields - 1) [StructField name logical | (name, _, logical) <- fields])
    (Map.fromList (zip [name | (name, _, _) <- fields] [0 ..]))

-- | Read the rows of a statement until the cursor gives no row.
readAllRows :: (Statement -> IO (Maybe row)) -> Statement -> IO [row]
readAllRows next stmt = go []
  where
    go rows = next stmt >>= maybe (pure (reverse rows)) (\row -> go (row : rows))

-- | Build a single-field struct for nested binding tests.
singleField :: LogicalTypeRep -> FieldValue -> StructValue FieldValue
singleField logical value =
  StructValue
    (listArray (0, 0) [StructField "value" value])
    (listArray (0, 0) [StructField "value" logical])
    (Map.singleton "value" 0)

-- | Assert a controlled input or materialization error.
assertIOError :: IO a -> Assertion
assertIOError action = do
  result <- try action
  case result of
    Left (_ :: IOException) -> pure ()
    Right _ -> assertFailure "expected IOException"

-- | The field of an ENUM row with the index and the label that DuckDB gives.
-- The field holds only the index.
enumField :: Maybe Int64 -> Maybe Text -> FieldValue
enumField index _ = maybe FieldNull (FieldEnum . fromIntegral) index

-- | Assert a controlled FromField conversion error.
assertConversionError :: IO a -> Assertion
assertConversionError action = do
  result <- try action
  case result of
    Left (_ :: SQLError) -> pure ()
    Right _ -> assertFailure "expected ResultError"

-- | Assert rejection without forcing a partial generic value.
assertLeft :: Either String a -> Assertion
assertLeft (Left _) = pure ()
assertLeft (Right _) = assertFailure "expected Left"
