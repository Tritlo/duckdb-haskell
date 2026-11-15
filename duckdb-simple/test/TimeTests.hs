{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : TimeTests
-- Description : Round trips for finite dates and native temporal infinities.
module TimeTests (timeTests) where

import Control.Exception (fromException, try)
import Control.Monad (forM_, void)
import Data.Array (Array, listArray)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Text as Text
import Data.Time (Day, LocalTime (..), TimeOfDay (..), UTCTime (..), addDays, fromGregorian, localTimeToUTC, utc, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), TimeWithZone (..), returnError)
import Database.DuckDB.Simple.Generic (ViaDuckDB (..))
import Database.DuckDB.Simple.LogicalRep (
  LogicalTypeRep (..),
  StructField (..),
  StructValue (..),
  TupleValue (..),
  UnionMemberType (..),
  UnionValue (..),
 )
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.Time
import Database.DuckDB.Simple.Variant (Variant (..))
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Record fields exercise the generic scalar and collection instances.
data TemporalRecord = TemporalRecord
  { dateValue :: Date
  , localValue :: LocalTimestamp
  , utcValue :: UTCTimestamp
  , optionalValue :: Maybe Date
  , dates :: [Date]
  }
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB TemporalRecord)

-- | Each constructor retains its temporal member type.
data TemporalSum = ADate Date | ALocal LocalTimestamp | AUtc UTCTimestamp | ADateNull (Maybe Date)
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, FromField) via (ViaDuckDB TemporalSum)

-- | A custom decoder must receive infinity before any finite conversion.
newtype InfinitySign = InfinitySign Int
  deriving (Eq, Show)

instance FromField InfinitySign where
  fromField f@Field{fieldValue} = case fieldValue of
    FieldDate NegInfinity -> pure (InfinitySign (-1))
    FieldDate (Finite _) -> pure (InfinitySign 0)
    FieldDate PosInfinity -> pure (InfinitySign 1)
    _ -> returnError Incompatible f "expected DATE"

-- | Exercise native binding, eager queries, cursors, and generic composites.
timeTests :: TestTree
timeTests =
  testGroup
    "temporal infinity"
    $ [ testCase "dates bind and decode with their SQL type" $ withConnection ":memory:" $ \conn ->
          forM_ [NegInfinity, Finite day, PosInfinity] $ \value -> do
            (query conn "SELECT ?, typeof(?)" (value, value) :: IO [(Date, String)]) >>= (@?= [(value, "DATE")])
            (query conn "SELECT ?::VARCHAR" (Only value) :: IO [Only String]) >>= (@?= [Only (dateText value)])
      , testCase "local timestamps bind and decode with their SQL type" $ withConnection ":memory:" $ \conn ->
          forM_ [NegInfinity, Finite local, PosInfinity] $ \value ->
            (query conn "SELECT ?, typeof(?)" (value, value) :: IO [(LocalTimestamp, String)]) >>= (@?= [(value, "TIMESTAMP")])
      , testCase "UTC timestamps preserve instants and infinities outside UTC" $ withConnection ":memory:" $ \conn -> do
          _ <- execute_ conn "SET TimeZone='Pacific/Auckland'"
          forM_ [NegInfinity, Finite instant, PosInfinity] $ \value ->
            (query conn "SELECT ?, typeof(?)" (value, value) :: IO [(UTCTimestamp, String)]) >>= (@?= [(value, "TIMESTAMP WITH TIME ZONE")])
      , testCase "infinity and NULL remain distinct" $ withConnection ":memory:" $ \conn -> do
          ( query
              conn
              "SELECT ?::DATE, ?::TIMESTAMP, ?::TIMESTAMPTZ"
              (Nothing :: Maybe Date, Nothing :: Maybe LocalTimestamp, Nothing :: Maybe UTCTimestamp)
              :: IO [(Maybe Date, Maybe LocalTimestamp, Maybe UTCTimestamp)]
            )
            >>= (@?= [(Nothing, Nothing, Nothing)])
          (query_ conn "SELECT 'infinity'::DATE" :: IO [Only (Maybe Date)]) >>= (@?= [Only (Just PosInfinity)])
      , testCase "ordinary finite targets reject infinity with a conversion error" $ withConnection ":memory:" $ \conn -> do
          assertConversionError (query_ conn "SELECT 'infinity'::DATE" :: IO [Only Day])
          assertConversionError (query_ conn "SELECT '-infinity'::TIMESTAMP" :: IO [Only LocalTime])
          assertConversionError (query_ conn "SELECT 'infinity'::TIMESTAMPTZ" :: IO [Only UTCTime])
          assertConversionError (query_ conn "SELECT 'infinity'::TIMESTAMP" :: IO [Only TimeOfDay])
          (query_ conn "SELECT 42" :: IO [Only Int]) >>= (@?= [Only 42])
      , testCase "a conversion error gives the DuckDB names TIMESTAMPTZ and TIMETZ" $ withConnection ":memory:" $ \conn -> do
          [(timestamp, time)] <- query_ conn "SELECT TIMESTAMPTZ '2000-01-02 03:04:05+00', TIMETZ '03:04:05+00'"
          forM_ [(timestamp, "TIMESTAMPTZ"), (time, "TIMETZ")] $ \(value, name) ->
            case fromField Field{fieldName = "value", fieldIndex = 0, fieldValue = value} :: Ok Int64 of
              Errors errors -> map (fmap errSQLType . fromException) errors @?= [Just name]
              Ok number -> assertFailure ("expected a conversion error, got " <> show number)
          -- The message of a query has the same name.
          forM_ [("SELECT TIMESTAMPTZ '2000-01-02 03:04:05+00'", " has type TIMESTAMPTZ "), ("SELECT TIMETZ '03:04:05+00'", " has type TIMETZ ")] $ \(sql, expected) -> do
            result <- try (query_ conn sql :: IO [Only Int64])
            case result of
              Left SQLError{sqlErrorMessage = message} -> assertBool (Text.unpack message) (expected `Text.isInfixOf` message)
              Right rows -> assertFailure ("expected a conversion error, got " <> show rows)
      , testCase "custom FromField receives native infinity" $ withConnection ":memory:" $ \conn ->
          ( query_ conn "SELECT value FROM (VALUES ('-infinity'::DATE), (DATE '2000-01-02'), ('infinity'::DATE)) t(value) ORDER BY value"
              :: IO [Only InfinitySign]
          )
            >>= (@?= map (Only . InfinitySign) [-1, 0, 1])
      , testCase "cursors preserve infinities" $ withConnection ":memory:" $ \conn -> do
          values <-
            fold_
              conn
              "SELECT value FROM (VALUES ('-infinity'::DATE), (DATE '2000-01-02'), ('infinity'::DATE)) t(value) ORDER BY value"
              []
              (\acc (Only value) -> pure (value : acc))
          reverse values @?= [NegInfinity, Finite day, PosInfinity]
      , testCase "date arrays preserve infinities" $ withConnection ":memory:" $ \conn -> do
          let values = listArray (0 :: Int, 2) [NegInfinity, Finite day, PosInfinity]
          query conn "SELECT ?" (Only values) >>= (@?= [Only values])
      , testCase "generic records retain finite values infinities and NULL" $ withConnection ":memory:" $ \conn -> do
          let values = TemporalRecord NegInfinity (Finite local) PosInfinity Nothing [PosInfinity, Finite day, NegInfinity]
          query conn "SELECT ?" (Only values) >>= (@?= [Only values])
      , testCase "generic union payloads retain their types" $ withConnection ":memory:" $ \conn ->
          forM_ [ADate NegInfinity, ADate (Finite day), ALocal PosInfinity, AUtc NegInfinity, ADateNull Nothing] $ \value ->
            query conn "SELECT ?" (Only value) >>= (@?= [Only value])
      ]
      <> nanosecondTimeTests
  where
    day = fromGregorian 2000 1 2
    local = LocalTime day (TimeOfDay 3 4 5)
    instant = localTimeToUTC utc local
    dateText NegInfinity = "-infinity"
    dateText PosInfinity = "infinity"
    dateText (Finite _) = "2000-01-02"

-- | Check explicit nanosecond targets without changing ordinary UTC parameter types.
nanosecondTimeTests :: [TestTree]
nanosecondTimeTests =
  [ testCase "nanosecond UTC results and VARIANTs preserve precision and infinity" $ withConnection ":memory:" $ \conn -> do
      _ <- execute_ conn "SET TimeZone='Pacific/Auckland'"
      let precise = localTimeToUTC utc (LocalTime (fromGregorian 2000 1 2) (TimeOfDay 3 4 5.123456789))
      forM_ [("'2000-01-02 03:04:05.123456789+00'", Finite precise), ("'infinity'", PosInfinity), ("'-infinity'", NegInfinity)] $ \(literal, expected) -> do
        let sql = Query ("SELECT " <> literal <> "::TIMESTAMPTZ_NS, (" <> literal <> "::TIMESTAMPTZ_NS)::VARIANT")
        (query_ conn sql :: IO [(UTCTimestamp, Variant)]) >>= (@?= [(expected, Variant (FieldTimestampTZ expected))])
      let value = Variant (FieldTimestampTZ (Finite precise))
      query conn "SELECT ?" (Only value) >>= (@?= [Only value])
  , testCase "explicit nanosecond UTC parameters preserve finite units and infinities" $ withConnection ":memory:" $ \conn -> do
      _ <- execute_ conn "SET TimeZone='Pacific/Auckland'"
      forM_ [minBound, negate (maxBound :: Int64) + 1, -1, 0, 1, 123456789, maxBound - 1] $ \nanos -> do
        let instant = posixSecondsToUTCTime (fromRational (toInteger nanos % 1000000000))
        (query conn "SELECT ?::TIMESTAMPTZ_NS" (Only instant) :: IO [Only UTCTime]) >>= (@?= [Only instant])
        (query conn "SELECT ?::TIMESTAMPTZ_NS" (Only (Finite instant)) :: IO [Only UTCTimestamp]) >>= (@?= [Only (Finite instant)])
      forM_ [NegInfinity, PosInfinity] $ \value ->
        (query conn "SELECT ?::TIMESTAMPTZ_NS" (Only (value :: UTCTimestamp)) :: IO [Only UTCTimestamp]) >>= (@?= [Only value])
      let precise = posixSecondsToUTCTime 0.123456789
          ordinary = posixSecondsToUTCTime 0.123456
      (query conn "SELECT typeof(?), ?" (precise, precise) :: IO [(String, UTCTime)]) >>= (@?= [("TIMESTAMP WITH TIME ZONE", ordinary)])
  , testCase "reused statements refresh nanosecond UTC parameter metadata after ALTER" $ withConnection ":memory:" $ \conn -> do
      void $ execute_ conn "CREATE TABLE utc_rebind(value TIMESTAMPTZ)"
      let coarse = posixSecondsToUTCTime 0.123456
          precise = posixSecondsToUTCTime 0.123456789
      withStatement conn "INSERT INTO utc_rebind VALUES (?)" $ \stmt -> do
        bind stmt [toField coarse]
        void $ executeStatement stmt
        void $ execute_ conn "ALTER TABLE utc_rebind ALTER COLUMN value TYPE TIMESTAMPTZ_NS"
        bind stmt [toField precise]
        void $ executeStatement stmt
      (query_ conn "SELECT epoch_ns(value) FROM utc_rebind ORDER BY value" :: IO [Only Int64]) >>= (@?= [Only 123456000, Only 123456789])
  , testCase "executeMany stores the same UTC values as single execute calls" $ withConnection ":memory:" $ \conn -> do
      let rows = [(t, t, t) | t <- map posixSecondsToUTCTime [0.123456789, 1.5, -0.000000001]]
          columns = " (tz TIMESTAMPTZ, ts TIMESTAMP, ns TIMESTAMPTZ_NS)"
          readBack table = query_ conn (Query ("SELECT epoch_ns(tz), epoch_ns(ts), epoch_ns(ns) FROM " <> table <> " ORDER BY ns")) :: IO [(Int64, Int64, Int64)]
      void $ execute_ conn (Query ("CREATE TABLE utc_many" <> columns))
      void $ execute_ conn (Query ("CREATE TABLE utc_single" <> columns))
      executeMany conn "INSERT INTO utc_many VALUES (?, ?, ?)" rows >>= (@?= 3)
      forM_ rows (void . execute conn "INSERT INTO utc_single VALUES (?, ?, ?)")
      many <- readBack "utc_many"
      readBack "utc_single" >>= (@?= many)
      -- The rows after the first row keep their nanoseconds with the kept parameter types.
      map (\(_, _, ns) -> ns) many @?= [-1, 123456789, 1500000000]
  , testCase "nanosecond UTC arrays retain NULLs and reject empty arrays" $ withConnection ":memory:" $ \conn -> do
      let precise = posixSecondsToUTCTime 0.123456789
          negative = posixSecondsToUTCTime (-0.000000001)
          values = listArray (0 :: Int, 2) [Just precise, Nothing, Just negative]
          empty = listArray (0, -1) [] :: Array Int (Maybe UTCTime)
          nulls = listArray (0, 1) [Nothing, Nothing] :: Array Int (Maybe UTCTime)
      (query conn "SELECT ?::TIMESTAMPTZ_NS[]" (Only values) :: IO [Only [Maybe UTCTime]]) >>= (@?= [Only [Just precise, Nothing, Just negative]])
      assertEmptyArrayUnavailable (query conn "SELECT ?::TIMESTAMPTZ_NS[]" (Only empty) :: IO [Only [Maybe UTCTime]])
      (query conn "SELECT ?::TIMESTAMPTZ_NS[]" (Only nulls) :: IO [Only [Maybe UTCTime]]) >>= (@?= [Only [Nothing, Nothing]])
      (query conn "SELECT ?::TIMESTAMPTZ_NS" (Only (Nothing :: Maybe UTCTime)) :: IO [Only (Maybe UTCTime)]) >>= (@?= [Only Nothing])
      let nested = listArray (0 :: Int, 1) [values, values]
      (query conn "SELECT ?::TIMESTAMPTZ_NS[][]" (Only nested) :: IO [Only [[Maybe UTCTime]]])
        >>= (@?= [Only (replicate 2 [Just precise, Nothing, Just negative])])
  , testCase "nanosecond targets preserve matching STRUCT MAP and UNION leaves" $ withConnection ":memory:" $ \conn -> do
      let precise = Finite (posixSecondsToUTCTime 0.123456789)
          coarse = Finite (localTimeToUTC utc (LocalTime (fromGregorian 5000 1 1) (TimeOfDay 0 0 0)))
          utcType = LogicalTypeScalar LogicalTypeIdTimestampTz
          fields =
            listArray
              (0, 3)
              [ StructField "Precise" (FieldTimestampTZ precise)
              , StructField "Coarse" (FieldTimestampTZ coarse)
              , StructField "items" (FieldList [FieldTimestampTZ precise, FieldNull, FieldTimestampTZ NegInfinity])
              , StructField "lookup" (FieldMap [(FieldInt64 1, FieldTimestampTZ precise)])
              ]
          types =
            listArray
              (0, 3)
              [ StructField "Precise" utcType
              , StructField "Coarse" utcType
              , StructField "items" (LogicalTypeList utcType)
              , StructField "lookup" (LogicalTypeMap (LogicalTypeScalar LogicalTypeIdBigInt) utcType)
              ]
          struct = StructValue fields types (Map.fromList [("Precise", 0), ("Coarse", 1), ("items", 2), ("lookup", 3)])
          sql =
            "WITH bound AS (SELECT ?::STRUCT(coarse TIMESTAMPTZ, items TIMESTAMPTZ_NS[], lookup MAP(BIGINT, TIMESTAMPTZ_NS), precise TIMESTAMPTZ_NS) AS value) SELECT value.precise, value.coarse, value.items, map_values(value.lookup) FROM bound"
      (query conn sql (Only struct) :: IO [(UTCTimestamp, UTCTimestamp, [Maybe UTCTimestamp], [UTCTimestamp])])
        >>= (@?= [(precise, coarse, [Just precise, Nothing, Just NegInfinity], [precise])])
      let union = UnionValue 0 "Value" (FieldTimestampTZ precise) (listArray (0, 0) [UnionMemberType "Value" utcType])
      (query conn "SELECT union_extract(?::UNION(value TIMESTAMPTZ_NS), 'value')" (Only union) :: IO [Only UTCTimestamp]) >>= (@?= [Only precise])
      assertInputError
        (query conn sql (Only struct{structValueFields = listArray (0, 0) [StructField "wrong" (FieldTimestampTZ precise)]}) :: IO [Only FieldValue])
      assertInputError (query conn "SELECT ?::UNION(value TIMESTAMPTZ_NS)" (Only union{unionValueLabel = "wrong"}) :: IO [Only FieldValue])
  , testCase "nanosecond tuple targets match positional fields" $ withConnection ":memory:" $ \conn -> do
      let precise = Finite (posixSecondsToUTCTime 0.123456789)
          utcType = LogicalTypeScalar LogicalTypeIdTimestampTz
          structure names =
            StructValue
              (listArray (3, 5) (zipWith StructField names [FieldTimestampTZ precise, FieldTimestampTZ precise, FieldNull]))
              (listArray (7, 9) [StructField name utcType | name <- names])
              (Map.fromList (zip names [0 ..]))
          tuple = TupleValue (listArray (3, 5) [FieldTimestampTZ precise, FieldTimestampTZ precise, FieldNull]) (listArray (7, 9) (replicate 3 utcType))
          named = structure ["first", "second", "optional"]
          sql =
            "SELECT epoch_ns(value[1]), epoch_ns(value[2]), value[3] IS NULL FROM (SELECT ?::TUPLE(TIMESTAMPTZ,TIMESTAMPTZ_NS,TIMESTAMPTZ_NS) AS value)"
      (query conn sql (Only tuple) :: IO [(Int64, Int64, Bool)]) >>= (@?= [(123456000, 123456789, True)])
      (query conn sql (Only named) :: IO [(Int64, Int64, Bool)]) >>= (@?= [(123456000, 123456789, True)])
      ( query
          conn
          "SELECT epoch_ns(value.first), epoch_ns(value.second), value.optional IS NULL FROM (SELECT ?::STRUCT(first TIMESTAMPTZ,second TIMESTAMPTZ_NS,optional TIMESTAMPTZ_NS) AS value)"
          (Only tuple)
          :: IO [(Int64, Int64, Bool)]
        )
        >>= (@?= [(123456000, 123456789, True)])
      result <- try (query conn "SELECT ?::TUPLE(TIMESTAMPTZ,TIMESTAMPTZ_NS)" (Only tuple) :: IO [Only FieldValue])
      case result of
        Left (_ :: SQLError) -> pure ()
        Right _ -> assertFailure "expected tuple arity mismatch"
  , testCase "nested tuple lists retain independent temporal field metadata" $ withConnection ":memory:" $ \conn -> do
      let precise = Finite (posixSecondsToUTCTime 0.123456789)
          utcType = LogicalTypeScalar LogicalTypeIdTimestampTz
          innerTypes = listArray (4, 4) [StructField "value" utcType]
          inner = StructValue (listArray (2, 2) [StructField "value" (FieldTimestampTZ precise)]) innerTypes (Map.singleton "value" 0)
          tuple = TupleValue (listArray (3, 4) (replicate 2 (FieldStruct inner))) (listArray (6, 7) (replicate 2 (LogicalTypeStruct innerTypes)))
          container =
            StructValue
              (listArray (1, 1) [StructField "items" (FieldList [FieldTuple tuple])])
              (listArray (5, 5) [StructField "items" (LogicalTypeList (LogicalTypeTuple (tupleValueTypes tuple)))])
              (Map.singleton "items" 0)
          sql =
            "SELECT epoch_ns((value.items[1][1]).value), epoch_ns((value.items[1][2]).value) FROM (SELECT ?::STRUCT(items TUPLE(STRUCT(value TIMESTAMPTZ),STRUCT(value TIMESTAMPTZ_NS))[]) AS value)"
      (query conn sql (Only container) :: IO [(Int64, Int64)]) >>= (@?= [(123456000, 123456789)])
  , testCase "a time value keeps its digits below a microsecond only for a nanosecond parameter" $ withConnection ":memory:" $ \conn -> do
      _ <- execute_ conn "SET TimeZone='UTC'"
      let day = fromGregorian 2026 1 2
          time = TimeOfDay 3 4 5.123456999
          local = LocalTime day time
          instant = localTimeToUTC utc local
          stored :: (ToField a) => Text.Text -> Text.Text -> a -> IO [Only Text.Text]
          stored column cast value = do
            _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE stored (v " <> column <> ")"))
            _ <- execute conn (Query ("INSERT INTO stored VALUES (?" <> cast <> ")")) (Only value)
            query_ conn "SELECT v::VARCHAR FROM stored"
          check :: (ToField a) => [(Text.Text, Text.Text, Text.Text)] -> a -> Assertion
          check rows value = forM_ rows $ \(column, cast, expected) ->
            stored column cast value >>= assertEqual (Text.unpack (column <> " ?" <> cast)) [Only expected]
          selected :: (ToField a) => Text.Text -> a -> IO [Only Text.Text]
          selected sql value = query conn (Query sql) (Only value)
      check
        [ ("TIMESTAMP_NS", "", "2026-01-02 03:04:05.123456999")
        , ("TIMESTAMP", "", "2026-01-02 03:04:05.123456")
        , ("TIMESTAMP_NS", "::TIMESTAMP_NS", "2026-01-02 03:04:05.123456999")
        ]
        local
      check [("TIMESTAMP_NS", "", "2026-01-02 03:04:05.123456999"), ("TIMESTAMP", "", "2026-01-02 03:04:05.123456")] (Finite local)
      check [("TIME_NS", "", "03:04:05.123456999"), ("TIME", "", "03:04:05.123456"), ("TIME_NS", "::TIME_NS", "03:04:05.123456999")] time
      check
        [ ("TIMESTAMPTZ_NS", "", "2026-01-02 03:04:05.123456999+00")
        , ("TIMESTAMPTZ", "", "2026-01-02 03:04:05.123456+00")
        , ("TIMESTAMPTZ_NS", "::TIMESTAMPTZ_NS", "2026-01-02 03:04:05.123456999+00")
        ]
        instant
      check [("TIMESTAMPTZ_NS", "", "2026-01-02 03:04:05.123456999+00"), ("TIMESTAMPTZ", "", "2026-01-02 03:04:05.123456+00")] (Finite instant)
      -- A parameter with no known type gets the microsecond value.
      selected "SELECT ?::VARCHAR" local >>= (@?= [Only "2026-01-02 03:04:05.123456"])
      selected "SELECT ?::VARCHAR" time >>= (@?= [Only "03:04:05.123456"])
      selected "SELECT ?::VARCHAR" instant >>= (@?= [Only "2026-01-02 03:04:05.123456+00"])
      -- A comparison with a nanosecond column finds the exact value.
      void $ execute_ conn "CREATE TABLE precise (v TIMESTAMP_NS, t TIME_NS)"
      void $ execute_ conn "INSERT INTO precise VALUES ('2026-01-02 03:04:05.123456999', '03:04:05.123456999')"
      (query conn "SELECT count(*) FROM precise WHERE v = ?" (Only local) :: IO [Only Int64]) >>= (@?= [Only 1])
      (query conn "SELECT count(*) FROM precise WHERE t = ?" (Only time) :: IO [Only Int64]) >>= (@?= [Only 1])
  , testCase "an explicit nanosecond cast for a microsecond column rounds as in SQL text" $ withConnection ":memory:" $ \conn -> do
      -- An explicit cast to a nanosecond type makes a nanosecond parameter. The
      -- cast of DuckDB from the nanosecond type to the microsecond type rounds.
      -- A plain parameter for the microsecond column cuts the digits.
      _ <- execute_ conn "SET TimeZone='UTC'"
      let local = LocalTime (fromGregorian 2026 1 2) (TimeOfDay 3 4 5.123456999)
          time = localTimeOfDay local
          instant = localTimeToUTC utc local
          stored :: (ToField a) => Text.Text -> Text.Text -> a -> IO [Only Text.Text]
          stored column cast value = do
            _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE stored (v " <> column <> ")"))
            _ <- execute conn (Query ("INSERT INTO stored VALUES (?" <> cast <> ")")) (Only value)
            query_ conn "SELECT v::VARCHAR FROM stored"
      stored "TIMESTAMP" "::TIMESTAMP_NS" local >>= (@?= [Only "2026-01-02 03:04:05.123457"])
      stored "TIMESTAMP" "" local >>= (@?= [Only "2026-01-02 03:04:05.123456"])
      stored "TIME" "::TIME_NS" time >>= (@?= [Only "03:04:05.123457"])
      stored "TIME" "" time >>= (@?= [Only "03:04:05.123456"])
      stored "TIMESTAMPTZ" "::TIMESTAMPTZ_NS" instant >>= (@?= [Only "2026-01-02 03:04:05.123457+00"])
      stored "TIMESTAMPTZ" "" instant >>= (@?= [Only "2026-01-02 03:04:05.123456+00"])
      (query conn "SELECT (?::TIMESTAMP_NS + INTERVAL 1 DAY)::VARCHAR" (Only local) :: IO [Only Text.Text])
        >>= (@?= [Only "2026-01-03 03:04:05.123457"])
      (query conn "SELECT (? + INTERVAL 1 DAY)::VARCHAR" (Only local) :: IO [Only Text.Text]) >>= (@?= [Only "2026-01-03 03:04:05.123456"])
  , testCase "nested time values keep their digits below a microsecond only for nanosecond leaves" $ withConnection ":memory:" $ \conn -> do
      let day = fromGregorian 2026 1 2
          precise = LocalTime day (TimeOfDay 3 4 5.123456999)
          coarse = LocalTime day (TimeOfDay 3 4 5.123456)
          time = TimeOfDay 3 4 5.123456999
          stored :: (ToField a) => Text.Text -> a -> IO [Only Text.Text]
          stored column value = do
            _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE stored (v " <> column <> ")"))
            _ <- execute conn "INSERT INTO stored VALUES (?)" (Only value)
            query_ conn "SELECT v::VARCHAR FROM stored"
          struct value logical =
            StructValue (listArray (0, 0) [StructField "v" value]) (listArray (0, 0) [StructField "v" logical]) (Map.singleton "v" 0)
      stored "TIMESTAMP_NS[]" [precise, coarse] >>= (@?= [Only "['2026-01-02 03:04:05.123456999', '2026-01-02 03:04:05.123456']"])
      stored "TIMESTAMP[]" [precise, coarse] >>= (@?= [Only "['2026-01-02 03:04:05.123456', '2026-01-02 03:04:05.123456']"])
      stored "TIMESTAMP_NS[2]" (listArray (0, 1) [precise, coarse] :: Array Int LocalTime)
        >>= (@?= [Only "['2026-01-02 03:04:05.123456999', '2026-01-02 03:04:05.123456']"])
      stored "TIME_NS[]" [time] >>= (@?= [Only "['03:04:05.123456999']"])
      stored "TIME[]" [time] >>= (@?= [Only "['03:04:05.123456']"])
      stored "MAP(VARCHAR, TIMESTAMP_NS)" (Map.fromList [("a" :: Text.Text, precise)]) >>= (@?= [Only "{a='2026-01-02 03:04:05.123456999'}"])
      stored "MAP(VARCHAR, TIMESTAMP)" (Map.fromList [("a" :: Text.Text, precise)]) >>= (@?= [Only "{a='2026-01-02 03:04:05.123456'}"])
      let localStruct = struct (FieldTimestamp (Finite precise)) (LogicalTypeScalar LogicalTypeIdTimestamp)
          timeStruct = struct (FieldTime time) (LogicalTypeScalar LogicalTypeIdTime)
      stored "STRUCT(v TIMESTAMP_NS)" localStruct >>= (@?= [Only "{'v': '2026-01-02 03:04:05.123456999'}"])
      stored "STRUCT(v TIMESTAMP)" localStruct >>= (@?= [Only "{'v': '2026-01-02 03:04:05.123456'}"])
      stored "STRUCT(v TIME_NS)" timeStruct >>= (@?= [Only "{'v': '03:04:05.123456999'}"])
      stored "STRUCT(v TIME)" timeStruct >>= (@?= [Only "{'v': '03:04:05.123456'}"])
  , testCase "executeMany binds time rows with and without digits below a microsecond" $ withConnection ":memory:" $ \conn -> do
      let day = fromGregorian 2026 1 2
          rows =
            [ (0 :: Int64, LocalTime day (TimeOfDay 3 4 5.123456999))
            , (1, LocalTime day (TimeOfDay 3 4 5.123456))
            , (2, LocalTime day (TimeOfDay 3 4 5.000000001))
            ]
      forM_
        [ ("TIMESTAMP_NS", ["2026-01-02 03:04:05.123456999", "2026-01-02 03:04:05.123456", "2026-01-02 03:04:05.000000001"])
        , ("TIMESTAMP", ["2026-01-02 03:04:05.123456", "2026-01-02 03:04:05.123456", "2026-01-02 03:04:05"])
        ]
        $ \(column, expected) -> do
          _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE many (i BIGINT, v " <> column <> ")"))
          _ <- executeMany conn "INSERT INTO many VALUES (?, ?)" rows
          (query_ conn "SELECT v::VARCHAR FROM many ORDER BY i" :: IO [Only Text.Text]) >>= assertEqual (Text.unpack column) (map Only expected)
  , testCase "a time value outside the nanosecond range and an infinity for a nanosecond parameter" $ withConnection ":memory:" $ \conn -> do
      let moment year month day hours minutes seconds = LocalTime (fromGregorian year month day) (TimeOfDay hours minutes seconds)
          early = moment 1600 1 2 3 4 5.123456999
          -- DuckDB can cast the microsecond value of this value to TIMESTAMP_NS.
          edge = moment 2262 4 11 23 47 16.854775900
          precise = moment 2026 1 2 3 4 5.123456999
          stamp = FieldTimestamp . Finite
          timestamp = LogicalTypeScalar LogicalTypeIdTimestamp
          pair second =
            StructValue
              (listArray (0, 1) [StructField "a" (stamp precise), StructField "b" (stamp second)])
              (listArray (0, 1) [StructField "a" timestamp, StructField "b" timestamp])
              (Map.fromList [("a", 0), ("b", 1)])
          tuple = TupleValue (listArray (0, 0) [stamp early]) (listArray (0, 0) [timestamp])
          union = UnionValue 0 "v" (stamp early) (listArray (0, 0) [UnionMemberType "v" timestamp])
          insert :: (ToField a) => Text.Text -> a -> IO ()
          insert column value = do
            _ <- execute_ conn (Query ("CREATE OR REPLACE TABLE stored (v " <> column <> ")"))
            void (execute conn "INSERT INTO stored VALUES (?)" (Only value))
          noRows label = (query_ conn "SELECT count(*) FROM stored" :: IO [Only Int64]) >>= assertEqual label [Only 0]
          duckdbError expected label action =
            try action >>= \case
              Left SQLError{sqlErrorMessage, sqlErrorType} -> do
                assertEqual label (Just expected) sqlErrorType
                assertBool label ("Could not convert Timestamp to higher precision" `Text.isInfixOf` sqlErrorMessage)
                noRows label
              Right () -> assertFailure (label <> ": expected the cast error of DuckDB")
          encoderError label action =
            try action >>= \case
              Left SQLError{sqlErrorType, sqlErrorMessage} -> do
                assertEqual label (Just ErrorInputInvalid) sqlErrorType
                assertBool label ("duckdb-simple: TIMESTAMP value out of finite range" `Text.isInfixOf` sqlErrorMessage)
                noRows label
              Right () -> assertFailure (label <> ": expected the range error of the encoder")
      -- DuckDB raises the error when it also cannot cast the microsecond value.
      forM_ [early, moment 1600 1 2 3 4 5, moment 2262 4 11 23 47 16.854776001, moment 1677 9 21 0 12 43.145224100] $ \value ->
        duckdbError ErrorTypeConversion (show value) (insert "TIMESTAMP_NS" value)
      duckdbError ErrorInputInvalid "list" (insert "TIMESTAMP_NS[]" [precise, early])
      duckdbError ErrorInputInvalid "Array" (insert "TIMESTAMP_NS[2]" (listArray (0, 1) [precise, early] :: Array Int LocalTime))
      duckdbError ErrorInputInvalid "Map" (insert "MAP(VARCHAR, TIMESTAMP_NS)" (Map.fromList [("a" :: Text.Text, precise), ("b", early)]))
      duckdbError ErrorTypeConversion "UTCTime with no digits" (insert "TIMESTAMPTZ_NS" (localTimeToUTC utc (moment 1600 1 2 3 4 5)))
      -- The encoder raises the error when the cast would cut the digits.
      encoderError "edge" (insert "TIMESTAMP_NS" edge)
      encoderError "edge list" (insert "TIMESTAMP_NS[]" [precise, edge])
      encoderError "UTCTime" (insert "TIMESTAMPTZ_NS" (localTimeToUTC utc early))
      -- A leaf of a STRUCT, TUPLE, or UNION gets the error of the encoder, also in
      -- TRY_CAST, where a microsecond value would keep the other leaf with its
      -- digits cut.
      encoderError "STRUCT" (insert "STRUCT(a TIMESTAMP_NS, b TIMESTAMP_NS)" (pair early))
      encoderError "STRUCT with no digits" (insert "STRUCT(a TIMESTAMP_NS, b TIMESTAMP_NS)" (pair (moment 1600 1 2 3 4 5)))
      encoderError "TUPLE" (insert "TUPLE(TIMESTAMP_NS)" tuple)
      encoderError "UNION" (insert "UNION(v TIMESTAMP_NS)" union)
      assertInputError
        (query conn "SELECT TRY_CAST(? AS STRUCT(a TIMESTAMP_NS, b TIMESTAMP_NS))::VARCHAR" (Only (pair early)) :: IO [Only Text.Text])
      -- In TRY_CAST, DuckDB gives NULL for a parameter that it cannot cast, and
      -- the constructor of a list raises its error first.
      (query conn "SELECT TRY_CAST(? AS TIMESTAMP_NS) IS NULL" (Only early) :: IO [Only Bool]) >>= (@?= [Only True])
      try (query conn "SELECT TRY_CAST(? AS TIMESTAMP_NS[])::VARCHAR" (Only [precise, early]) :: IO [Only Text.Text]) >>= \case
        Left SQLError{sqlErrorType} -> sqlErrorType @?= Just ErrorInputInvalid
        Right rows -> assertFailure ("TRY_CAST list: " <> show rows)
      -- The last values in the range keep their digits.
      insert "TIMESTAMP_NS[]" [moment 2262 4 11 23 47 16.854775806, moment 1677 9 21 0 12 43.145224500]
      (query_ conn "SELECT v::VARCHAR FROM stored" :: IO [Only Text.Text])
        >>= (@?= [Only "['2262-04-11 23:47:16.854775806', '1677-09-21 00:12:43.1452245']"])
      _ <- execute_ conn "CREATE OR REPLACE TABLE stored (v TIMESTAMP_NS, z TIMESTAMPTZ_NS)"
      _ <- execute conn "INSERT INTO stored VALUES (?, ?)" (PosInfinity :: LocalTimestamp, NegInfinity :: UTCTimestamp)
      (query_ conn "SELECT v::VARCHAR, z::VARCHAR FROM stored" :: IO [(Text.Text, Text.Text)]) >>= (@?= [("infinity", "-infinity")])
  , testCase "a time value with digits below a microsecond keeps them for a VARIANT parameter" $ withConnection ":memory:" $ \conn -> do
      _ <- execute_ conn "SET TimeZone = 'UTC'; CREATE TABLE v (x VARIANT); CREATE TABLE l (x VARIANT[])"
      let day = fromGregorian 2026 1 2
          tod = TimeOfDay 3 4 5.123456789
          local = LocalTime day tod
          early = LocalTime (fromGregorian 1600 1 2) tod
          stored :: (ToField a) => a -> IO [(Text.Text, Text.Text)]
          stored value = do
            _ <- execute_ conn "DELETE FROM v"
            _ <- execute conn "INSERT INTO v VALUES (?)" (Only value)
            query_ conn "SELECT variant_typeof(x), x::VARCHAR FROM v"
      -- A value with such digits gets the nanosecond type.
      stored tod >>= (@?= [("TIME_NANOS", "03:04:05.123456789")])
      stored local >>= (@?= [("TIMESTAMP_NANOS", "2026-01-02 03:04:05.123456789")])
      stored (Finite local) >>= (@?= [("TIMESTAMP_NANOS", "2026-01-02 03:04:05.123456789")])
      stored (localTimeToUTC utc local) >>= (@?= [("TIMESTAMP_NANOS_TZ", "2026-01-02 03:04:05.123456789+00")])
      -- A value with no such digits keeps the microsecond type.
      stored (TimeOfDay 3 4 5.123456) >>= (@?= [("TIME_MICROS", "03:04:05.123456")])
      stored (LocalTime day (TimeOfDay 3 4 5.123456)) >>= (@?= [("TIMESTAMP_MICROS", "2026-01-02 03:04:05.123456")])
      -- A value that the nanosecond type cannot hold gets the microsecond value.
      stored early >>= (@?= [("TIMESTAMP_MICROS", "1600-01-02 03:04:05.123456")])
      stored (LocalTime (fromGregorian 2262 4 11) (TimeOfDay 23 47 16.854775900)) >>= (@?= [("TIMESTAMP_MICROS", "2262-04-11 23:47:16.854775")])
      stored (localTimeToUTC utc early) >>= (@?= [("TIMESTAMP_MICROS_TZ", "1600-01-02 03:04:05.123456+00")])
      -- A Variant payload and a FieldValue read from a VARIANT keep the digits.
      stored (Variant (FieldTimestamp (Finite local))) >>= (@?= [("TIMESTAMP_NANOS", "2026-01-02 03:04:05.123456789")])
      _ <-
        execute_
          conn
          "CREATE TABLE src (i INTEGER, x VARIANT); INSERT INTO src VALUES (1, '03:04:05.123456789'::TIME_NS), (2, '2026-01-02 03:04:05.123456789'::TIMESTAMP_NS), (3, '2026-01-02 03:04:05.123456789+00'::TIMESTAMPTZ_NS)"
      values <- query_ conn "SELECT i, x FROM src ORDER BY i" :: IO [(Int64, FieldValue)]
      forM_ values $ \(i, value) -> do
        _ <- execute_ conn "DELETE FROM v"
        _ <- execute conn "INSERT INTO v VALUES (?)" (Only value)
        (query conn "SELECT count(*) FROM src, v WHERE src.i = ? AND src.x IS NOT DISTINCT FROM v.x" (Only i) :: IO [Only Int64])
          >>= assertEqual (show value) [Only 1]
      -- A VARIANT object reads as a FieldStruct with VARIANT fields, so its leaves keep the digits.
      [Only object] <- query_ conn "SELECT {'t': '2026-01-02 03:04:05.123456789'::TIMESTAMP_NS}::VARIANT" :: IO [Only FieldValue]
      stored object >>= (@?= [("OBJECT(t)", "{'t': '2026-01-02 03:04:05.123456789'}")])
      -- A VARIANT array reads as a FieldList, whose elements get the microsecond value.
      [Only array] <- query_ conn "SELECT ['2026-01-02 03:04:05.123456789'::TIMESTAMP_NS]::VARIANT" :: IO [Only FieldValue]
      stored array >>= (@?= [("ARRAY(1)", "['2026-01-02 03:04:05.123456']")])
      -- An element of a VARIANT list keeps the microsecond type, as the list has one element type.
      _ <- execute conn "INSERT INTO l VALUES (?)" (Only [local, LocalTime day (TimeOfDay 3 4 5)])
      (query_ conn "SELECT variant_typeof(x[1]), x::VARCHAR FROM l" :: IO [(Text.Text, Text.Text)])
        >>= (@?= [("TIMESTAMP_MICROS", "[2026-01-02 03:04:05.123456, 2026-01-02 03:04:05]")])
  , testCase "nanosecond UTC targets reject finite overflow and sentinel collisions" $ withConnection ":memory:" $ \conn -> do
      forM_
        [ toInteger (minBound :: Int64) - 1
        , negate (toInteger (maxBound :: Int64))
        , toInteger (maxBound :: Int64)
        , toInteger (maxBound :: Int64) + 1
        ]
        $ \nanos -> do
          let instant = posixSecondsToUTCTime (fromRational (nanos % 1000000000))
          assertInputError (query conn "SELECT ?::TIMESTAMPTZ_NS" (Only instant) :: IO [Only UTCTime])
          assertInputError (query conn "SELECT ?::TIMESTAMPTZ_NS" (Only (Finite instant)) :: IO [Only UTCTimestamp])
      (query_ conn "SELECT 42" :: IO [Only Int]) >>= (@?= [Only 42])
  , testCase "dates and timestamps at the ends of the range of DuckDB bind, and a value outside raises an error" $ withConnection ":memory:" $ \conn -> do
      -- See Note [Time ranges] in "Database.DuckDB.Simple.ToField".
      let epoch = fromGregorian 1970 1 1
          firstDay = addDays (-2147483646) epoch
          -- Day -2147483647 is -infinity, so -2147483648 is the only INT32 day before the range.
          beforeDay = addDays (-2147483648) epoch
          local units count = utcToLocalTime utc (posixSecondsToUTCTime (fromRational (count % units)))
          firstTime = local 1000000 (-9223372022400000000)
          beforeTime = local 1000000 (-9223372022400000001)
          structOf logical value =
            StructValue
              (listArray (0, 0) [StructField "a" value])
              (listArray (0, 0) [StructField "a" (LogicalTypeScalar logical)])
              (Map.fromList [("a", 0)])
          create column = void (execute_ conn (Query ("CREATE OR REPLACE TABLE stored (v " <> column <> ")")))
          stored :: forall a. (ToField a, FromField a, Eq a, Show a) => Text.Text -> a -> IO ()
          stored column value = do
            create column
            void (execute conn "INSERT INTO stored VALUES (?)" (Only value))
            (query_ conn "SELECT v FROM stored" :: IO [Only a]) >>= assertEqual (Text.unpack column) [Only value]
          rejected :: (ToField a) => Text.Text -> a -> IO ()
          rejected column value = do
            create column
            try (execute conn "INSERT INTO stored VALUES (?)" (Only value)) >>= \case
              Left SQLError{sqlErrorType} -> assertEqual (Text.unpack column) (Just ErrorInputOutOfRange) sqlErrorType
              Right _ -> assertFailure (Text.unpack column <> ": expected the range error")
            (query_ conn "SELECT count(*) FROM stored" :: IO [Only Int64]) >>= assertEqual (Text.unpack column) [Only 0]
      stored "DATE" firstDay
      rejected "DATE" beforeDay
      stored "DATE" (Finite firstDay :: Date)
      rejected "DATE" (Finite beforeDay :: Date)
      stored "DATE[]" [firstDay]
      rejected "DATE[]" [firstDay, beforeDay]
      stored "STRUCT(a DATE)" (structOf LogicalTypeIdDate (FieldDate (Finite firstDay)))
      rejected "STRUCT(a DATE)" (structOf LogicalTypeIdDate (FieldDate (Finite beforeDay)))
      stored "VARIANT" (Variant (FieldDate (Finite firstDay)))
      rejected "VARIANT" (Variant (FieldDate (Finite beforeDay)))
      stored "TIMESTAMP" firstTime
      rejected "TIMESTAMP" beforeTime
      stored "TIMESTAMP" (Finite firstTime :: LocalTimestamp)
      rejected "TIMESTAMP" (Finite beforeTime :: LocalTimestamp)
      stored "TIMESTAMP[]" [firstTime]
      rejected "TIMESTAMP[]" [firstTime, beforeTime]
      stored "STRUCT(a TIMESTAMP)" (structOf LogicalTypeIdTimestamp (FieldTimestamp (Finite firstTime)))
      rejected "STRUCT(a TIMESTAMP)" (structOf LogicalTypeIdTimestamp (FieldTimestamp (Finite beforeTime)))
      stored "VARIANT" (Variant (FieldTimestamp (Finite firstTime)))
      rejected "VARIANT" (Variant (FieldTimestamp (Finite beforeTime)))
      stored "TIMESTAMPTZ" (localTimeToUTC utc firstTime)
      rejected "TIMESTAMPTZ" (localTimeToUTC utc beforeTime)
      stored "TIMESTAMPTZ" (Finite (localTimeToUTC utc firstTime) :: UTCTimestamp)
      rejected "TIMESTAMPTZ" (Finite (localTimeToUTC utc beforeTime) :: UTCTimestamp)
      -- A second or a millisecond value has the range of a microsecond value at
      -- both ends. A STRUCT field can have these types.
      forM_
        [ (LogicalTypeIdTimestampSec, "STRUCT(a TIMESTAMP_S)", 1, 9223372036854)
        , (LogicalTypeIdTimestampMs, "STRUCT(a TIMESTAMP_MS)", 1000, 9223372036854775)
        ]
        $ \(logical, column, units, largest) -> do
          let at count = structOf logical (FieldTimestamp (Finite (local units count)))
          stored column (at (-9223372022400000000 * units `div` 1000000))
          stored column (at largest)
          rejected column (at (-9223372022400000000 * units `div` 1000000 - 1))
          rejected column (at (largest + 1))
  , testCase "a leap second binds as the next second, as make_time and make_timestamp give it" $ withConnection ":memory:" $ \conn -> do
      let leap = TimeOfDay 23 59 60
          next = LocalTime (fromGregorian 2017 1 1) (TimeOfDay 0 0 0)
      -- The SQL literal '23:59:60' raises an error. See Note [Time ranges] in
      -- "Database.DuckDB.Simple.ToField".
      ( query
          conn
          "SELECT ?, make_time(23, 59, 60), ?, make_timestamp(2016, 12, 31, 23, 59, 60), ?, ?"
          (leap, LocalTime (fromGregorian 2016 12 31) leap, TimeWithZone leap 0, UTCTime (fromGregorian 2016 12 31) 86400)
          :: IO [(TimeOfDay, TimeOfDay, LocalTime, LocalTime, TimeWithZone, UTCTime)]
        )
        >>= (@?= [(TimeOfDay 24 0 0, TimeOfDay 24 0 0, next, next, TimeWithZone (TimeOfDay 24 0 0) 0, localTimeToUTC utc next)])
  ]

-- | Require a controlled encoding error before native value construction.
assertInputError :: IO a -> Assertion
assertInputError action = do
  result <- try action
  case result of
    Left SQLError{sqlErrorType} -> sqlErrorType @?= Just ErrorInputInvalid
    Right _ -> assertFailure "expected an SQLError of the type ErrorInputInvalid"

-- | Require a field conversion error, rather than an unrelated exception.
assertConversionError :: IO a -> Assertion
assertConversionError action = do
  result <- try action
  case result of
    Left SQLError{sqlErrorMessage = message} ->
      assertBool "error must identify infinity" ("infinity" `Text.isInfixOf` message)
    Right _ -> assertFailure "expected a conversion error"

-- | Require the error of the library for an ARRAY parameter with no elements.
assertEmptyArrayUnavailable :: IO a -> Assertion
assertEmptyArrayUnavailable action = do
  result <- try action
  case result of
    Left SQLError{sqlErrorMessage = message} -> assertBool "must identify empty ARRAY" ("empty ARRAY" `Text.isInfixOf` message)
    Right _ -> assertFailure "expected the empty ARRAY error"
