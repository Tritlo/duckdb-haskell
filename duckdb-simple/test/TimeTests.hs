{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Round trips for finite dates and native temporal infinities.
module TimeTests (timeTests) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.Array (Array, listArray)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Text as Text
import Data.Time (Day, LocalTime (..), TimeOfDay (..), UTCTime, fromGregorian, localTimeToUTC, utc)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.DuckDB.FFI
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), returnError)
import Database.DuckDB.Simple.Generic (ViaDuckDB (..))
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), StructValue (..), UnionMemberType (..), UnionValue (..))
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
                (query conn "SELECT ?::DATE, ?::TIMESTAMP, ?::TIMESTAMPTZ" (Nothing :: Maybe Date, Nothing :: Maybe LocalTimestamp, Nothing :: Maybe UTCTimestamp) :: IO [(Maybe Date, Maybe LocalTimestamp, Maybe UTCTimestamp)]) >>= (@?= [(Nothing, Nothing, Nothing)])
                (query_ conn "SELECT 'infinity'::DATE" :: IO [Only (Maybe Date)]) >>= (@?= [Only (Just PosInfinity)])
          , testCase "ordinary finite targets reject infinity with a conversion error" $ withConnection ":memory:" $ \conn -> do
                assertConversionError (query_ conn "SELECT 'infinity'::DATE" :: IO [Only Day])
                assertConversionError (query_ conn "SELECT '-infinity'::TIMESTAMP" :: IO [Only LocalTime])
                assertConversionError (query_ conn "SELECT 'infinity'::TIMESTAMPTZ" :: IO [Only UTCTime])
                assertConversionError (query_ conn "SELECT 'infinity'::TIMESTAMP" :: IO [Only TimeOfDay])
                (query_ conn "SELECT 42" :: IO [Only Int]) >>= (@?= [Only 42])
          , testCase "custom FromField receives native infinity" $ withConnection ":memory:" $ \conn ->
                (query_ conn "SELECT value FROM (VALUES ('-infinity'::DATE), (DATE '2000-01-02'), ('infinity'::DATE)) t(value) ORDER BY value" :: IO [Only InfinitySign]) >>= (@?= map (Only . InfinitySign) [-1, 0, 1])
          , testCase "cursors preserve infinities" $ withConnection ":memory:" $ \conn -> do
                values <- fold_ conn "SELECT value FROM (VALUES ('-infinity'::DATE), (DATE '2000-01-02'), ('infinity'::DATE)) t(value) ORDER BY value" [] (\acc (Only value) -> pure (value : acc))
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
    , testCase "nanosecond UTC arrays retain NULLs and empty element types" $ withConnection ":memory:" $ \conn -> do
        let precise = posixSecondsToUTCTime 0.123456789
            negative = posixSecondsToUTCTime (-0.000000001)
            values = listArray (0 :: Int, 2) [Just precise, Nothing, Just negative]
            empty = listArray (0, -1) [] :: Array Int (Maybe UTCTime)
            nulls = listArray (0, 1) [Nothing, Nothing] :: Array Int (Maybe UTCTime)
        (query conn "SELECT ?::TIMESTAMPTZ_NS[]" (Only values) :: IO [Only [Maybe UTCTime]]) >>= (@?= [Only [Just precise, Nothing, Just negative]])
        (query conn "SELECT ?::TIMESTAMPTZ_NS[]" (Only empty) :: IO [Only [Maybe UTCTime]]) >>= (@?= [Only []])
        (query conn "SELECT ?::TIMESTAMPTZ_NS[]" (Only nulls) :: IO [Only [Maybe UTCTime]]) >>= (@?= [Only [Nothing, Nothing]])
        (query conn "SELECT ?::TIMESTAMPTZ_NS" (Only (Nothing :: Maybe UTCTime)) :: IO [Only (Maybe UTCTime)]) >>= (@?= [Only Nothing])
        let nested = listArray (0 :: Int, 1) [values, values]
        (query conn "SELECT ?::TIMESTAMPTZ_NS[][]" (Only nested) :: IO [Only [[Maybe UTCTime]]]) >>= (@?= [Only (replicate 2 [Just precise, Nothing, Just negative])])
    , testCase "nanosecond targets preserve matching STRUCT MAP and UNION leaves" $ withConnection ":memory:" $ \conn -> do
        let precise = Finite (posixSecondsToUTCTime 0.123456789)
            coarse = Finite (localTimeToUTC utc (LocalTime (fromGregorian 5000 1 1) (TimeOfDay 0 0 0)))
            utcType = LogicalTypeScalar DuckDBTypeTimestampTz
            fields = listArray (0, 3) [StructField "Precise" (FieldTimestampTZ precise), StructField "Coarse" (FieldTimestampTZ coarse), StructField "items" (FieldList [FieldTimestampTZ precise, FieldNull, FieldTimestampTZ NegInfinity]), StructField "lookup" (FieldMap [(FieldInt64 1, FieldTimestampTZ precise)])]
            types = listArray (0, 3) [StructField "Precise" utcType, StructField "Coarse" utcType, StructField "items" (LogicalTypeList utcType), StructField "lookup" (LogicalTypeMap (LogicalTypeScalar DuckDBTypeBigInt) utcType)]
            struct = StructValue fields types (Map.fromList [("Precise", 0), ("Coarse", 1), ("items", 2), ("lookup", 3)])
            sql = "WITH bound AS (SELECT ?::STRUCT(coarse TIMESTAMPTZ, items TIMESTAMPTZ_NS[], lookup MAP(BIGINT, TIMESTAMPTZ_NS), precise TIMESTAMPTZ_NS) AS value) SELECT value.precise, value.coarse, value.items, map_values(value.lookup) FROM bound"
        (query conn sql (Only struct) :: IO [(UTCTimestamp, UTCTimestamp, [Maybe UTCTimestamp], [UTCTimestamp])]) >>= (@?= [(precise, coarse, [Just precise, Nothing, Just NegInfinity], [precise])])
        let union = UnionValue 0 "Value" (FieldTimestampTZ precise) (listArray (0, 0) [UnionMemberType "Value" utcType])
        (query conn "SELECT union_extract(?::UNION(value TIMESTAMPTZ_NS), 'value')" (Only union) :: IO [Only UTCTimestamp]) >>= (@?= [Only precise])
        assertIOException (query conn sql (Only struct{structValueFields = listArray (0, 0) [StructField "wrong" (FieldTimestampTZ precise)]}) :: IO [Only FieldValue])
        assertIOException (query conn "SELECT ?::UNION(value TIMESTAMPTZ_NS)" (Only union{unionValueLabel = "wrong"}) :: IO [Only FieldValue])
    , testCase "nanosecond UTC targets reject finite overflow and sentinel collisions" $ withConnection ":memory:" $ \conn -> do
        forM_ [toInteger (minBound :: Int64) - 1, negate (toInteger (maxBound :: Int64)), toInteger (maxBound :: Int64), toInteger (maxBound :: Int64) + 1] $ \nanos -> do
            let instant = posixSecondsToUTCTime (fromRational (nanos % 1000000000))
            assertIOException (query conn "SELECT ?::TIMESTAMPTZ_NS" (Only instant) :: IO [Only UTCTime])
            assertIOException (query conn "SELECT ?::TIMESTAMPTZ_NS" (Only (Finite instant)) :: IO [Only UTCTimestamp])
        (query_ conn "SELECT 42" :: IO [Only Int]) >>= (@?= [Only 42])
    ]

-- | Require a controlled encoding error before native value construction.
assertIOException :: IO a -> Assertion
assertIOException action = do
    result <- try action
    case result of
        Left (_ :: IOException) -> pure ()
        Right _ -> assertFailure "expected IOException"

-- | Require a field conversion error, rather than an unrelated exception.
assertConversionError :: IO a -> Assertion
assertConversionError action = do
    result <- try action
    case result of
        Left SQLError{sqlErrorMessage = message} ->
            assertBool "error must identify infinity" ("infinity" `Text.isInfixOf` message)
        Right _ -> assertFailure "expected a conversion error"
