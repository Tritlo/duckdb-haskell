{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration checks for typed registration through SQL.
module TypedRegistrationTests (typedRegistrationTests) where

import Control.Exception (throwIO, try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
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
import Database.DuckDB.Simple.Config (CustomOption (..), SettingScope (..), customOption, getConfigValue, registerCustomOption, setConfigValue)
import Database.DuckDB.Simple.FromField (BigNum (..), FieldValue (..), bsFromBool)
import Database.DuckDB.Simple.Pragma (PragmaCallback (..), registerPragmaFunction)
import Database.DuckDB.Simple.Registration (FunctionParameter (..), Replacement (..), TypeDeclaration (..), defaultValue, parameter, registerCustomType, registerInstanceReplacementScan, registerReplacementScan)
import Database.DuckDB.Simple.Time (Unbounded (..))
import Database.DuckDB.Simple.ToField (ToDuckValue)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, (@?=))

-- | Check the typed registrations through SQL.
typedRegistrationTests :: TestTree
typedRegistrationTests =
    testGroup
        "typed registration"
        [ testCase "cast from BIGINT to a custom type over several chunks" testCastChunks
        , testCase "cast failure fails CAST and gives NULL in TRY_CAST" testCastFailure
        , testCase "cast to custom aliases of DATE and VARCHAR" testCastTypes
        , testCase "cast exceptions and conversion errors fail CAST" testCastErrors
        , testCase "cast to a custom type on each scalar base type" testCastBaseTypes
        , testCase "replacement scan reads the rows of a query" testReplacementQuery
        , testCase "replacement scan that declines gives the unknown table error" testReplacementDecline
        , testCase "replacement scan gets the name parts as written" testReplacementNames
        , testCase "replacement scan errors fail the query" testReplacementErrors
        , testCase "connection replacement scan serves only its connection" testReplacementConnectionScope
        , testCase "instance replacement scan serves each connection" testReplacementInstance
        , testCase "replacement scans run in the order of registration" testReplacementOrder
        , testCase "query pragma with no parameter" testPragmaNoParameter
        , testCase "query pragma with a default parameter" testPragmaDefault
        , testCase "pragma row has the order of the declaration" testPragmaRow
        , testCase "exec pragma counts its calls" testPragmaExec
        , testCase "pragma argument rows that do not decode" testPragmaDecodeErrors
        , testCase "pragma exceptions become SQL errors" testPragmaExceptions
        , testCase "custom option update accepts and rejects BIGINT values" testOptionUpdate
        , testCase "custom option update gets VARCHAR values and can reject RESET" testOptionText
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

-- | Run the SQL of a query pragma without a parameter.
testPragmaNoParameter :: Assertion
testPragmaNoParameter = withConnection ":memory:" \conn -> do
    registerPragmaFunction conn "typed_answer" [] (QueryPragma \() -> pure "SELECT 42::BIGINT")
    query_ conn "PRAGMA typed_answer" >>= (@?= [Only (42 :: Int64)])

-- | Give the default value to the callback when the call omits the argument.
testPragmaDefault :: Assertion
testPragmaDefault = withConnection ":memory:" \conn -> do
    let amount = (parameter "amount" (TypeExpression "BIGINT")){parameterDefault = Just (defaultValue (42 :: Int64))}
    registerPragmaFunction conn "typed_amount" [amount] $
        QueryPragma \(Only (value :: Maybe Int64)) -> pure (maybe "SELECT NULL::BIGINT" (\number -> "SELECT " <> Text.pack (show number) <> "::BIGINT") value)
    query_ conn "PRAGMA typed_amount" >>= (@?= [Only (Just (42 :: Int64))])
    query_ conn "PRAGMA typed_amount(7)" >>= (@?= [Only (Just (7 :: Int64))])
    query_ conn "PRAGMA typed_amount(amount = 9)" >>= (@?= [Only (Just (9 :: Int64))])
    query_ conn "PRAGMA typed_amount(NULL)" >>= (@?= [Only (Nothing :: Maybe Int64)])

{- | Decode two parameters of different types as a tuple. The row has the order
of the declaration when the call gives the arguments by name in another order.
An omitted argument has its default value.
-}
testPragmaRow :: Assertion
testPragmaRow = withConnection ":memory:" \conn -> do
    let label = (parameter "label" (TypeExpression "VARCHAR")){parameterDefault = Just (defaultValue ("none" :: Text))}
    registerPragmaFunction conn "typed_pair" [parameter "amount" (TypeExpression "BIGINT"), label] $
        QueryPragma \(amount :: Int64, text :: Text) -> pure ("SELECT " <> Text.pack (show amount) <> "::BIGINT, '" <> Text.replace "'" "''" text <> "'")
    query_ conn "PRAGMA typed_pair(3, 'liður ''x''')" >>= (@?= [(3 :: Int64, "liður 'x'" :: Text)])
    query_ conn "PRAGMA typed_pair(label = 'named', amount = 4)" >>= (@?= [(4 :: Int64, "named" :: Text)])
    query_ conn "PRAGMA typed_pair(5)" >>= (@?= [(5 :: Int64, "none" :: Text)])

{- | Run the action of an exec pragma once for each statement. The action gets
the argument or the default value.
-}
testPragmaExec :: Assertion
testPragmaExec = withConnection ":memory:" \conn -> do
    calls <- newIORef (0 :: Int64)
    let step = (parameter "step" (TypeExpression "BIGINT")){parameterDefault = Just (defaultValue (1 :: Int64))}
    registerPragmaFunction conn "typed_count" [step] (ExecPragma \(Only increment) -> modifyIORef' calls (+ increment))
    void (execute_ conn "PRAGMA typed_count")
    void (execute_ conn "PRAGMA typed_count")
    readIORef calls >>= (@?= 2)
    void (execute_ conn "PRAGMA typed_count(5)")
    readIORef calls >>= (@?= 7)

-- | Name the pragma when the row has another type, another size, or a NULL.
testPragmaDecodeErrors :: Assertion
testPragmaDecodeErrors = withConnection ":memory:" \conn -> do
    let amount = parameter "amount" (TypeExpression "BIGINT")
    registerPragmaFunction conn "typed_text" [parameter "label" (TypeExpression "VARCHAR")] (QueryPragma \(Only (_ :: Int64)) -> pure "SELECT 1")
    assertSqlError "cannot decode the arguments of pragma typed_text: column label has type" (query_ conn "PRAGMA typed_text('x')" :: IO [Only Int64])
    registerPragmaFunction conn "typed_size" [amount] (QueryPragma \() -> pure "SELECT 1")
    assertSqlError "cannot decode the arguments of pragma typed_size" (query_ conn "PRAGMA typed_size(1)" :: IO [Only Int64])
    registerPragmaFunction conn "typed_strict" [amount] (ExecPragma \(Only (_ :: Int64)) -> pure ())
    assertSqlError "cannot decode the arguments of pragma typed_strict: column amount is NULL" (execute_ conn "PRAGMA typed_strict(NULL)")
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Make an SQL error from an exception of each callback kind.
testPragmaExceptions :: Assertion
testPragmaExceptions = withConnection ":memory:" \conn -> do
    registerPragmaFunction conn "typed_query_failure" [] (QueryPragma \() -> throwIO (userError "typed query pragma failure"))
    assertSqlError "typed query pragma failure" (query_ conn "PRAGMA typed_query_failure" :: IO [Only Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    registerPragmaFunction conn "typed_exec_failure" [] (ExecPragma \() -> throwIO (userError "typed exec pragma failure"))
    assertSqlError "typed exec pragma failure" (execute_ conn "PRAGMA typed_exec_failure")
    query_ conn "SELECT 43" >>= (@?= [Only (43 :: Int64)])

{- | Accept and reject the values of a BIGINT option from SET, RESET, and
'setConfigValue'. The function gets each value after the cast to BIGINT. A
rejected value does not change the option.
-}
testOptionUpdate :: Assertion
testOptionUpdate = withConnection ":memory:" \conn -> do
    seen <- newIORef []
    registerCustomOption
        conn
        (customOption "typed_limit" (TypeExpression "BIGINT"))
            { customOptionDefault = Just (defaultValue (3 :: Int64))
            , customOptionUpdate = Just \value -> do
                modifyIORef' seen (value :)
                case value of
                    FieldInt64 number | number < 0 -> throwIO (userError "typed_limit must not be negative")
                    _ -> pure ()
            }
    let current = fst <$> getConfigValue conn "typed_limit"
    void (execute_ conn "SET typed_limit = 7")
    current >>= (@?= FieldInt64 7)
    void (execute_ conn "SET typed_limit = '5'")
    current >>= (@?= FieldInt64 5)
    assertSqlError "typed_limit must not be negative" (execute_ conn "SET typed_limit = -1")
    current >>= (@?= FieldInt64 5)
    void (execute_ conn "RESET typed_limit")
    current >>= (@?= FieldInt64 3)
    setConfigValue conn SettingScopeDefault "typed_limit" (11 :: Int64)
    current >>= (@?= FieldInt64 11)
    assertSqlError "typed_limit must not be negative" (setConfigValue conn SettingScopeDefault "typed_limit" (-2 :: Int64))
    current >>= (@?= FieldInt64 11)
    readIORef seen >>= (@?= reverse (map FieldInt64 [7, 5, -1, 3, 11, -2]))

{- | Give the values of a VARCHAR option to the function. A RESET of an option
without a default gives NULL. A function that rejects NULL rejects the RESET.
-}
testOptionText :: Assertion
testOptionText = withConnection ":memory:" \conn -> do
    seen <- newIORef []
    registerCustomOption conn (customOption "typed_option_label" (TypeExpression "VARCHAR")){customOptionUpdate = Just \value -> modifyIORef' seen (value :)}
    void (execute_ conn "SET typed_option_label = 'liður'")
    void (execute_ conn "SET typed_option_label = 42")
    setConfigValue conn SettingScopeDefault "typed_option_label" ("set" :: Text)
    void (execute_ conn "RESET typed_option_label")
    readIORef seen >>= (@?= reverse [FieldText "liður", FieldText "42", FieldText "set", FieldNull])
    let required value = case value of
            FieldNull -> throwIO (userError "typed_option_required must have a value")
            _ -> pure ()
    registerCustomOption conn (customOption "typed_option_required" (TypeExpression "VARCHAR")){customOptionUpdate = Just required}
    void (execute_ conn "SET typed_option_required = 'kept'")
    assertSqlError "typed_option_required must have a value" (execute_ conn "RESET typed_option_required")
    fst <$> getConfigValue conn "typed_option_required" >>= (@?= FieldText "kept")

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

-- | Replace one name with a query. Read the rows under the table name and an alias.
testReplacementQuery :: Assertion
testReplacementQuery = withConnection ":memory:" \conn -> do
    _ <- execute_ conn "CREATE TABLE typed_present AS SELECT 9::BIGINT AS amount"
    registerReplacementScan conn \parts ->
        pure (if parts `elem` [["typed_missing"], ["typed_present"]] then Just (ReplaceWithQuery "SELECT i AS amount FROM range(3) t(i)") else Nothing)
    query_ conn "SELECT amount FROM typed_missing ORDER BY amount" >>= (@?= map Only [0, 1, 2 :: Int64])
    query_ conn "SELECT typed_missing.amount FROM typed_missing ORDER BY 1" >>= (@?= map Only [0, 1, 2 :: Int64])
    query_ conn "SELECT x.amount FROM typed_missing AS x ORDER BY 1" >>= (@?= map Only [0, 1, 2 :: Int64])
    -- DuckDB does not call the scan for a table that the catalog has.
    query_ conn "SELECT amount FROM typed_present" >>= (@?= [Only (9 :: Int64)])

-- | Decline a name. Require the same error as on a connection with no scan.
testReplacementDecline :: Assertion
testReplacementDecline = withConnection ":memory:" \conn -> do
    before <- sqlErrorOf (query_ conn "SELECT * FROM typed_unknown" :: IO [Only Int64])
    assertBool (Text.unpack before) ("Table with name typed_unknown does not exist" `Text.isInfixOf` before)
    registerReplacementScan conn \_ -> pure Nothing
    sqlErrorOf (query_ conn "SELECT * FROM typed_unknown" :: IO [Only Int64]) >>= (@?= before)

-- | Return the parts of each name. Compare them with the query text.
testReplacementNames :: Assertion
testReplacementNames = withConnection ":memory:" \conn -> do
    registerReplacementScan conn \parts ->
        pure (Just (ReplaceWithQuery ("SELECT '" <> Text.replace "'" "''" (Text.intercalate "|" parts) <> "' AS parts")))
    let check name expected = query_ conn (Query ("SELECT parts FROM " <> name)) >>= assertEqual (Text.unpack name) [Only (Text.intercalate "|" expected)]
    check "MiXeD" ["MiXeD"]
    check "\"Quoted Name\"" ["Quoted Name"]
    check "\"has.dot\"" ["has.dot"]
    check "\"a\"\"b\"" ["a\"b"]
    -- The catalog "memory" and the schema "main" exist. The others do not.
    check "main.typed_t" ["main", "typed_t"]
    check "MAIN.Typed_T" ["MAIN", "Typed_T"]
    check "typed_schema.typed_t" ["typed_schema", "typed_t"]
    check "memory.main.typed_t" ["memory", "main", "typed_t"]
    check "typed_catalog.main.typed_t" ["typed_catalog", "main", "typed_t"]
    check "'typed file.csv'" ["typed file.csv"]
    check "'typed''quote.parquet'" ["typed'quote.parquet"]

-- | Fail the query with an exception and with query text that is not one SELECT.
testReplacementErrors :: Assertion
testReplacementErrors = withConnection ":memory:" \conn -> do
    registerReplacementScan conn \case
        ["typed_reserved"] -> ioError (userError "typed_reserved: the table name is reserved")
        ["typed_two"] -> pure (Just (ReplaceWithQuery "SELECT 1; SELECT 2"))
        ["typed_create"] -> pure (Just (ReplaceWithQuery "CREATE TABLE typed_created (i BIGINT)"))
        _ -> pure Nothing
    assertSqlError "typed_reserved: the table name is reserved" (query_ conn "SELECT * FROM typed_reserved" :: IO [Only Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    assertSqlError "exactly one SELECT statement" (query_ conn "SELECT * FROM typed_two" :: IO [Only Int64])
    assertSqlError "must be a SELECT statement" (query_ conn "SELECT * FROM typed_create" :: IO [Only Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

{- | Register a scan on one connection of an instance. The other connection does
not use it. The scan replaces a file path before the built-in file scan.
-}
testReplacementConnectionScope :: Assertion
testReplacementConnectionScope = withEnvironment \environment -> withInstance environment [] \instance' ->
    withConnectionOn instance' \first -> withConnectionOn instance' \second -> do
        registerReplacementScan first \parts ->
            pure (if parts `elem` [["typed_scoped"], ["typed_missing.csv"]] then Just (ReplaceWithQuery "SELECT 'first' AS source") else Nothing)
        query_ first "SELECT source FROM typed_scoped" >>= (@?= [Only ("first" :: Text)])
        assertSqlError "Table with name typed_scoped does not exist" (query_ second "SELECT source FROM typed_scoped" :: IO [Only Text])
        query_ first "SELECT source FROM 'typed_missing.csv'" >>= (@?= [Only ("first" :: Text)])
        assertSqlError "No files found" (query_ second "SELECT source FROM 'typed_missing.csv'" :: IO [Only Text])

{- | Register a scan on an instance before its connections. Each connection uses
it. A connection scan comes first and passes a declined name to it.
-}
testReplacementInstance :: Assertion
testReplacementInstance = withEnvironment \environment -> withInstance environment [] \instance' -> do
    registerInstanceReplacementScan instance' \parts -> pure (if parts `elem` [["typed_shared"], ["typed_instance"]] then Just (ReplaceWithQuery "SELECT 'instance' AS source") else Nothing)
    withConnectionOn instance' \first -> withConnectionOn instance' \second -> do
        query_ first "SELECT source FROM typed_shared" >>= (@?= [Only ("instance" :: Text)])
        query_ second "SELECT source FROM typed_shared" >>= (@?= [Only ("instance" :: Text)])
        registerReplacementScan first \parts -> pure (if parts == ["typed_shared"] then Just (ReplaceWithQuery "SELECT 'connection' AS source") else Nothing)
        query_ first "SELECT source FROM typed_shared" >>= (@?= [Only ("connection" :: Text)])
        query_ first "SELECT source FROM typed_instance" >>= (@?= [Only ("instance" :: Text)])
        query_ second "SELECT source FROM typed_shared" >>= (@?= [Only ("instance" :: Text)])

-- | Register two scans on one connection. The first scan that replaces a name wins.
testReplacementOrder :: Assertion
testReplacementOrder = withConnection ":memory:" \conn -> do
    registerReplacementScan conn \parts -> pure (if parts == ["typed_both"] then Just (ReplaceWithQuery "SELECT 'first' AS source") else Nothing)
    registerReplacementScan conn \parts -> pure (if parts `elem` [["typed_both"], ["typed_later"]] then Just (ReplaceWithQuery "SELECT 'second' AS source") else Nothing)
    query_ conn "SELECT source FROM typed_both" >>= (@?= [Only ("first" :: Text)])
    query_ conn "SELECT source FROM typed_later" >>= (@?= [Only ("second" :: Text)])

-- | Require an 'SQLError' and return its message.
sqlErrorOf :: IO a -> IO Text
sqlErrorOf action =
    try action >>= \case
        Left (err :: SQLError) -> pure (sqlErrorMessage err)
        Right _ -> assertFailure "expected an SQL error"

-- | Require an 'SQLError' with a message that contains the text.
assertSqlError :: Text -> IO a -> Assertion
assertSqlError expected action = do
    outcome <- try action
    case outcome of
        Left (err :: SQLError) -> do
            let message = sqlErrorMessage err
            assertBool (Text.unpack message) (expected `Text.isInfixOf` message)
            -- The SQL error has the plain message of the Haskell exception.
            assertBool (Text.unpack message) (not (any (`Text.isInfixOf` message) ["user error", "SQLError {"]))
        Right _ -> assertFailure "expected an SQL error"
