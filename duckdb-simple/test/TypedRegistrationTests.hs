{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration checks for typed registration through SQL.
module TypedRegistrationTests (typedRegistrationTests) where

import Control.Exception (try)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime (..))
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (BigNum (..), bsFromBool)
import Database.DuckDB.Simple.Function (TypeDeclaration (..))
import Database.DuckDB.Simple.Time (Unbounded (..))
import Database.DuckDB.Simple.ToField (ToDuckValue)
import Database.DuckDB.Simple.V2.Registration (registerCustomType)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, (@?=))

-- | Check typed casts with values, NULLs, failures, and errors.
typedRegistrationTests :: TestTree
typedRegistrationTests =
    testGroup
        "typed registration"
        [ testCase "cast from BIGINT to a custom type over several chunks" testCastChunks
        , testCase "cast failure fails CAST and gives NULL in TRY_CAST" testCastFailure
        , testCase "cast to custom aliases of DATE and VARCHAR" testCastTypes
        , testCase "cast exceptions and conversion errors fail CAST" testCastErrors
        , testCase "cast to a custom type on each scalar base type" testCastBaseTypes
        ]

-- | Cast BIGINT values and NULLs to a custom type over more than one chunk.
testCastChunks :: Assertion
testCastChunks = withConnection ":memory:" \conn -> do
    registerCustomType conn "typed_shifted" (TypeExpression "BIGINT")
    createCast conn (TypeExpression "BIGINT") (TypeExpression "typed_shifted") \(value :: Int64) -> Right (value + 100)
    query_ conn "SELECT CAST(i AS typed_shifted)::BIGINT FROM range(3) t(i) ORDER BY i" >>= (@?= map Only [100, 101, 102 :: Int64])
    query_ conn "SELECT typeof(CAST(1::BIGINT AS typed_shifted))" >>= (@?= [Only ("typed_shifted" :: Text)])
    query_ conn "SELECT CAST(NULL::BIGINT AS typed_shifted)::BIGINT" >>= (@?= [Only (Nothing :: Maybe Int64)])
    -- A chunk has at most 2048 rows. Thus 3000 rows need more than one cast call.
    rows <- query_ conn "SELECT CAST(CASE WHEN i % 1000 = 7 THEN NULL ELSE i END AS typed_shifted)::BIGINT FROM range(3000) t(i) ORDER BY i"
    rows @?= [Only (if i `mod` 1000 == 7 then Nothing else Just (i + 100)) | i <- [0 .. 2999 :: Int64]]

-- | Fail CAST with a Left message. TRY_CAST gives NULL for that row only.
testCastFailure :: Assertion
testCastFailure = withConnection ":memory:" \conn -> do
    registerCustomType conn "typed_positive" (TypeExpression "BIGINT")
    createCast conn (TypeExpression "BIGINT") (TypeExpression "typed_positive") \(value :: Int64) ->
        if value > 0 then Right value else Left ("typed_positive: " <> Text.pack (show value) <> " is not positive")
    query_ conn "SELECT CAST(5::BIGINT AS typed_positive)::BIGINT" >>= (@?= [Only (5 :: Int64)])
    assertSqlError "typed_positive: -5 is not positive" (query_ conn "SELECT CAST(-5::BIGINT AS typed_positive)::BIGINT" :: IO [Only Int64])
    query_ conn "SELECT TRY_CAST(v AS typed_positive)::BIGINT FROM (VALUES (0, 1::BIGINT), (1, -2), (2, 3)) t(k, v) ORDER BY k"
        >>= (@?= map Only [Just 1, Nothing, Just (3 :: Int64)])
    query_ conn "SELECT CAST(6::BIGINT AS typed_positive)::BIGINT" >>= (@?= [Only (6 :: Int64)])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Cast to custom types with the base types DATE and VARCHAR.
testCastTypes :: Assertion
testCastTypes = withConnection ":memory:" \conn -> do
    registerCustomType conn "typed_day" (TypeExpression "DATE")
    createCast conn (TypeExpression "VARCHAR") (TypeExpression "typed_day") \(text :: Text) ->
        maybe (Left ("typed_day: " <> text <> " is not a date")) Right (parseTimeM False defaultTimeLocale "%d.%m.%Y" (Text.unpack text) :: Maybe Day)
    -- DuckDB 2.0.0 has no cast from a custom type to its base type DATE. Read the custom type.
    query_ conn "SELECT CAST(v AS typed_day) FROM (VALUES (0, '12.11.2026'), (1, NULL), (2, '29.02.2000')) t(k, v) ORDER BY k"
        >>= (@?= map Only [Just (fromGregorian 2026 11 12), Nothing, Just (fromGregorian 2000 2 29)])
    query_ conn "SELECT typeof(CAST('1.1.2000'::VARCHAR AS typed_day))" >>= (@?= [Only ("typed_day" :: Text)])
    -- A function result of Nothing gives NULL for a value that is not NULL.
    createCast conn (TypeExpression "BIGINT") (TypeExpression "typed_day") \(value :: Int64) ->
        Right (if value < 0 then Nothing else Just (addDays (fromIntegral value) (fromGregorian 2000 1 1)))
    query_ conn "SELECT CAST(v AS typed_day) FROM (VALUES (0, 1::BIGINT), (1, -1)) t(k, v) ORDER BY k"
        >>= (@?= map Only [Just (fromGregorian 2000 1 2), Nothing])
    registerCustomType conn "typed_label" (TypeExpression "VARCHAR")
    createCast conn (TypeExpression "BIGINT") (TypeExpression "typed_label") \(value :: Int64) -> Right ("liður " <> Text.pack (show value))
    query_ conn "SELECT CAST(i AS typed_label)::VARCHAR FROM range(2) t(i) ORDER BY i" >>= (@?= map Only ["liður 0", "liður 1" :: Text])
    query_ conn "SELECT typeof(CAST(1::BIGINT AS typed_label))" >>= (@?= [Only ("typed_label" :: Text)])

-- | Fail CAST with an exception and with arguments that do not convert.
testCastErrors :: Assertion
testCastErrors = withConnection ":memory:" \conn -> do
    registerCustomType conn "typed_partial" (TypeExpression "BIGINT")
    createCast conn (TypeExpression "BIGINT") (TypeExpression "typed_partial") \(value :: Int64) ->
        if value == 13 then error "typed_partial: 13 raises an exception" else Right value
    query_ conn "SELECT CAST(12::BIGINT AS typed_partial)::BIGINT" >>= (@?= [Only (12 :: Int64)])
    assertSqlError "typed_partial: 13 raises an exception" (query_ conn "SELECT CAST(13::BIGINT AS typed_partial)::BIGINT" :: IO [Only Int64])
    -- BIGINT 5 converts to Int8. BIGINT 300 does not.
    registerCustomType conn "typed_small" (TypeExpression "BIGINT")
    createCast conn (TypeExpression "BIGINT") (TypeExpression "typed_small") \(value :: Int8) -> Right (fromIntegral value :: Int64)
    query_ conn "SELECT CAST(5::BIGINT AS typed_small)::BIGINT" >>= (@?= [Only (5 :: Int64)])
    assertSqlError "cannot convert argument 1 from INT8 to Int8" (query_ conn "SELECT CAST(300::BIGINT AS typed_small)::BIGINT" :: IO [Only Int64])
    -- A VARCHAR value does not convert to Int64. The function does not get NULL.
    registerCustomType conn "typed_number" (TypeExpression "BIGINT")
    createCast conn (TypeExpression "VARCHAR") (TypeExpression "typed_number") \(value :: Int64) -> Right value
    query_ conn "SELECT CAST(NULL::VARCHAR AS typed_number)::BIGINT" >>= (@?= [Only (Nothing :: Maybe Int64)])
    assertSqlError "cannot convert argument 1 from TEXT to Int64" (query_ conn "SELECT CAST('7'::VARCHAR AS typed_number)::BIGINT" :: IO [Only Int64])
    -- TRY_CAST gives NULL for these failed rows.
    query_ conn "SELECT TRY_CAST(13::BIGINT AS typed_partial)::BIGINT, TRY_CAST('7'::VARCHAR AS typed_number)::BIGINT"
        >>= (@?= [(Nothing :: Maybe Int64, Nothing :: Maybe Int64)])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Cast to a custom type on each scalar base type and read the same values.
testCastBaseTypes :: Assertion
testCastBaseTypes = withConnection ":memory:" \conn -> do
    -- The text of a TIMESTAMPTZ value depends on the time zone of the session.
    _ <- execute_ conn "SET TimeZone = 'Pacific/Auckland'"
    let day = fromGregorian 2026 11 12
        time = TimeOfDay 23 59 59.000001
        instant = UTCTime day 86399.999999
    roundTrip conn "bool" [True, False]
    roundTrip conn "int8" [minBound, 0, maxBound :: Int8]
    roundTrip conn "int16" [minBound, 0, maxBound :: Int16]
    roundTrip conn "int32" [minBound, 0, maxBound :: Int32]
    roundTrip conn "int64" [minBound, 0, maxBound :: Int64]
    roundTrip conn "int" [minBound, 0, maxBound :: Int]
    roundTrip conn "word8" [0, maxBound :: Word8]
    roundTrip conn "word16" [0, maxBound :: Word16]
    roundTrip conn "word32" [0, maxBound :: Word32]
    roundTrip conn "word64" [0, maxBound :: Word64]
    roundTrip conn "word" [0, maxBound :: Word]
    roundTrip conn "integer" [-(2 ^ (127 :: Int)), 0, 2 ^ (127 :: Int) - 1 :: Integer]
    roundTrip conn "natural" [0, 2 ^ (128 :: Int) - 1 :: Natural]
    roundTrip conn "bignum" [BigNum (-(10 ^ (40 :: Int))), 0, BigNum (10 ^ (40 :: Int))]
    roundTrip conn "double" [-0.1, 0, 1 / 3 :: Double]
    roundTrip conn "float" [-0.1, 0, 1 / 3 :: Float]
    roundTrip conn "text" ["", "liður \\ ' \" \\x00" :: Text]
    roundTrip conn "blob" [BS.empty, BS.pack [0x00, 0xFF, 0x5C, 0x27, 0x22, 0x78, 0x41], BS.pack [0 .. 255]]
    roundTrip conn "bit" [bsFromBool [True], bsFromBool [True, False, False, True, False, True, True, False, True]]
    roundTrip conn "uuid" (mapMaybe UUID.fromString ["00000000-0000-0000-0000-000000000000", "123e4567-e89b-12d3-a456-426614174000", "ffffffff-ffff-ffff-ffff-ffffffffffff"])
    roundTrip conn "day" [fromGregorian 1 1 1, day, fromGregorian 9999 12 31]
    roundTrip conn "time" [TimeOfDay 0 0 0, time]
    roundTrip conn "local" [LocalTime (fromGregorian 1600 2 29) time, LocalTime day time]
    roundTrip conn "utc" [UTCTime (fromGregorian 1600 2 29) 0.000001, instant]
    roundTrip conn "unbounded_day" [NegInfinity, Finite day, PosInfinity]
    roundTrip conn "unbounded_local" [NegInfinity, Finite (LocalTime day time), PosInfinity]
    roundTrip conn "unbounded_utc" [NegInfinity, Finite instant, PosInfinity]

{- | Register a custom type on the column type of the values. Cast a row number
to each value. Require the same values from the custom type.
-}
roundTrip :: forall a. (ToDuckValue a, FromField a, Eq a, Show a) => Connection -> Text -> [a] -> Assertion
roundTrip conn label values = do
    let name = "typed_base_" <> label
    registerCustomType conn name (TypeExpression (duckdbColumnType (Proxy :: Proxy a)))
    createCast conn (TypeExpression "BIGINT") (TypeExpression name) \(row :: Int64) -> Right (values !! fromIntegral row)
    rows <- query_ conn (Query ("SELECT CAST(i AS " <> name <> ") FROM range(" <> Text.pack (show (length values)) <> ") t(i) ORDER BY i"))
    assertEqual (Text.unpack name) values (map fromOnly rows)

-- | Require an 'SQLError' with a message that contains the text.
assertSqlError :: Text -> IO a -> Assertion
assertSqlError expected action = do
    outcome <- try action
    case outcome of
        Left (err :: SQLError) -> assertBool (Text.unpack (sqlErrorMessage err)) (expected `Text.isInfixOf` sqlErrorMessage err)
        Right _ -> assertFailure "expected an SQL error"
