{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Round trips for finite dates and native temporal infinities.
module TimeTests (timeTests) where

import Control.Exception (try)
import Control.Monad (forM_)
import Data.Array (listArray)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime (..), TimeOfDay (..), UTCTime, fromGregorian, localTimeToUTC, utc)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), returnError)
import Database.DuckDB.Simple.Generic (ViaDuckDB (..))
import Database.DuckDB.Simple.Time

#ifdef DUCKDB_API_V2
import Database.DuckDB.Simple.Variant (Variant (..))

#endif
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
        [ testCase "dates bind and decode with their SQL type" $ withConnection ":memory:" $ \conn ->
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
#ifdef DUCKDB_API_V2
        , testCase "nanosecond UTC results and VARIANTs preserve precision and infinity" $ withConnection ":memory:" $ \conn -> do
            _ <- execute_ conn "SET TimeZone='Pacific/Auckland'"
            let precise = localTimeToUTC utc (LocalTime day (TimeOfDay 3 4 5.123456789))
            forM_ [("'2000-01-02 03:04:05.123456789+00'", Finite precise), ("'infinity'", PosInfinity), ("'-infinity'", NegInfinity)] $ \(literal, expected) -> do
                let sql = Query ("SELECT " <> literal <> "::TIMESTAMPTZ_NS, (" <> literal <> "::TIMESTAMPTZ_NS)::VARIANT")
                (query_ conn sql :: IO [(UTCTimestamp, Variant)]) >>= (@?= [(expected, Variant (FieldTimestampTZ expected))])
            let value = Variant (FieldTimestampTZ (Finite precise))
            query conn "SELECT ?" (Only value) >>= (@?= [Only value])
#endif
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
  where
    day = fromGregorian 2000 1 2
    local = LocalTime day (TimeOfDay 3 4 5)
    instant = localTimeToUTC utc local
    dateText NegInfinity = "-infinity"
    dateText PosInfinity = "infinity"
    dateText (Finite _) = "2000-01-02"

-- | Require a field conversion error, rather than an unrelated exception.
assertConversionError :: IO a -> Assertion
assertConversionError action = do
    result <- try action
    case result of
        Left SQLError{sqlErrorMessage = message} ->
            assertBool "error must identify infinity" ("infinity" `Text.isInfixOf` message)
        Right _ -> assertFailure "expected a conversion error"
