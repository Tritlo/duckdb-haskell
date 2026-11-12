{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration checks for typed registration through SQL.
module TypedRegistrationTests (typedRegistrationTests) where

import Control.Exception (bracket, throwIO, try)
import Control.Monad (forM_, void, when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time (UTCTime (..))
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Config (CustomOption (..), SettingScope (..), customOption, getConfigValue, registerCustomOption, setConfigValue)
import Database.DuckDB.Simple.Copy (CopyBindInfo (..), CopyFromInfo (..), CopyInitInfo (..), CopySinkInfo (..), CopyTo (..), defaultMultiFileOptions, registerCopyFromFunction, registerCopyFunction, registerCopyToFunction, registerMultiFileFunction)
import Database.DuckDB.Simple.FromField (BigNum (..), Field (..), FieldValue (..), bsFromBool)
import Database.DuckDB.Simple.Function (TypedRow)
import Database.DuckDB.Simple.Pragma (PragmaCallback (..), registerPragmaFunction)
import Database.DuckDB.Simple.Registration (FunctionParameter (..), Replacement (..), TypeDeclaration (..), defaultValue, parameter, registerCustomType, registerInstanceReplacementScan, registerReplacementScan)
import Database.DuckDB.Simple.Time (Unbounded (..))
import Database.DuckDB.Simple.ToField (ToDuckValue)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak)
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
        , testCase "table function with one parameter over several chunks" testTableSquares
        , testCase "table function with no, two, and NULL parameters" testTableParameters
        , testCase "table function columns of several types and a record row" testTableTypes
        , testCase "table function runs one time for each scan" testTableRuns
        , testCase "table function reads an infinite list with LIMIT" testTableLazy
        , testCase "table function registration and argument errors" testTableArgumentErrors
        , testCase "table function exceptions become SQL errors" testTableExceptions
        , testCase "table function releases the list of each scan" testTableRelease
        , testCase "multi-file function gives each row of a typed table function one time" testTableMultiFile
        , testCase "COPY format with both directions under one name" testCopyBothDirections
        , testCase "COPY keeps the first registration of a format name" testCopyFirstRegistration
        , testCase "COPY errors in each function fail only their statement" testCopyErrors
        , testCase "COPY format on two connections of one database" testCopySharedDatabase
        , testCase "aggregate over one group and over many groups" testAggregateGroups
        , testCase "aggregate skips NULL rows and gives the initial result" testAggregateNulls
        , testCase "aggregate arguments, states, and result types" testAggregateTypes
        , testCase "aggregate exceptions fail the query only" testAggregateErrors
        , testCase "aggregate as a window function and with modifiers" testAggregateModifiers
        , testCase "aggregate states are released" testAggregateRelease
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

-- | Add BIGINT values.
sumAggregate :: Aggregate Int64 (Only Int64) Int64
sumAggregate =
    Aggregate
        { aggregateInitial = 0
        , aggregateStep = \total (Only value) -> pure (total + value)
        , aggregateCombine = \a b -> pure (a + b)
        , aggregateResult = pure
        }

-- | Compare the two columns of each row.
assertColumnsEqual :: Connection -> Query -> Assertion
assertColumnsEqual conn sql = do
    rows <- query_ conn sql
    assertBool "rows" (not (null rows))
    forM_ rows \(typed, builtin) -> assertEqual (Text.unpack (fromQuery sql)) (builtin :: Maybe Int64) typed

-- | Fold one group and many groups, also with partial states from 4 threads.
testAggregateGroups :: Assertion
testAggregateGroups = withConnection ":memory:" \conn -> do
    createAggregate conn "typed_sum" sumAggregate
    query_ conn "SELECT typed_sum(i) FROM range(10) t(i)" >>= (@?= [Only (45 :: Int64)])
    -- A chunk has at most 2048 rows. Thus 5000 rows need more than one update call.
    assertColumnsEqual conn "SELECT typed_sum(i), sum(i)::BIGINT FROM range(5000) t(i) GROUP BY i % 50"
    query_ conn "SELECT count(*) FROM (SELECT i % 50 AS g, typed_sum(i) FROM range(5000) t(i) GROUP BY g)" >>= (@?= [Only (50 :: Int64)])
    -- A table of 3 row groups lets 4 threads make partial states. DuckDB then combines them.
    void (execute_ conn "SET threads = 4")
    void (execute_ conn "CREATE TABLE typed_numbers AS SELECT i FROM range(300000) t(i)")
    assertColumnsEqual conn "SELECT typed_sum(i), sum(i)::BIGINT FROM typed_numbers GROUP BY i % 1000"
    assertColumnsEqual conn "SELECT typed_sum(i), sum(i)::BIGINT FROM typed_numbers"
    query_ conn "SELECT typed_sum(i), typed_sum(i + 1) FROM range(10) t(i)" >>= (@?= [(45 :: Int64, 55 :: Int64)])

-- | Skip the rows with NULL. A group without other rows gives the initial result.
testAggregateNulls :: Assertion
testAggregateNulls = withConnection ":memory:" \conn -> do
    createAggregate conn "typed_sum" sumAggregate
    query_ conn "SELECT typed_sum(v) FROM (VALUES (1::BIGINT), (NULL), (2)) t(v)" >>= (@?= [Only (3 :: Int64)])
    -- The built-in sum gives NULL for group 2. The initial state gives 0.
    query_ conn "SELECT g, typed_sum(v) FROM (VALUES (1::BIGINT, 10::BIGINT), (1, NULL), (2, NULL)) t(g, v) GROUP BY g ORDER BY g"
        >>= (@?= [(1 :: Int64, 10 :: Int64), (2, 0)])
    query_ conn "SELECT typed_sum(i) FROM range(0) t(i)" >>= (@?= [Only (0 :: Int64)])
    query_ conn "SELECT typed_sum(NULL::BIGINT)" >>= (@?= [Only (0 :: Int64)])
    query_ conn "SELECT count(*) FROM (SELECT typed_sum(i) FROM range(0) t(i) GROUP BY i)" >>= (@?= [Only (0 :: Int64)])

{- | Count the words of each group in a 'Map'. The result lists each word and
its count, and an empty map gives NULL.
-}
tallyAggregate :: Aggregate (Map.Map Text Int64) (Text, Int64) (Maybe Text)
tallyAggregate =
    Aggregate
        { aggregateInitial = Map.empty
        , aggregateStep = \tally (word, count) -> pure (Map.insertWith (+) word count tally)
        , aggregateCombine = \a b -> pure (Map.unionWith (+) a b)
        , aggregateResult = \tally ->
            pure if Map.null tally then Nothing else Just (Text.intercalate "," [word <> "=" <> Text.pack (show count) | (word, count) <- Map.toList tally])
        }

-- | Use a tuple of two argument types, a 'Map' state, and each result type.
testAggregateTypes :: Assertion
testAggregateTypes = withConnection ":memory:" \conn -> do
    createAggregate conn "typed_tally" tallyAggregate
    -- Group 2 has the 12 bytes that fit inline. Group 3 has 13 bytes. Group 4 has 14
    -- bytes in 8 characters. Group 5 has NULL in each row.
    _ <- execute_ conn "CREATE TABLE typed_words AS SELECT * FROM (VALUES (1, 'a', 1::BIGINT), (1, 'a', 2), (1, NULL, 5), (1, 'b', NULL), (2, 'abcdefghij', 1), (3, 'abcdefghijk', 1), (4, '\240\240\240\240\240\240', 1), (5, NULL, 1), (5, 'c', NULL), (6, 'cherry', 2), (6, 'apple', 3), (6, 'banana', 5)) t(g, w, n)"
    let expected = [Just "a=3", Just "abcdefghij=1", Just "abcdefghijk=1", Just "\240\240\240\240\240\240=1", Nothing, Just "apple=3,banana=5,cherry=2" :: Maybe Text]
    query_ conn "SELECT typed_tally(w, n) FROM typed_words GROUP BY g ORDER BY g" >>= (@?= map Only expected)
    -- DuckDB compares the bytes of the results with the bytes of the literals.
    query_ conn "SELECT typed_tally(w, n) = 'abcdefghij=1', typed_tally(w, n) = 'abcdefghijk=1' FROM typed_words WHERE g IN (2, 3) GROUP BY g ORDER BY g"
        >>= (@?= [(Just True, Just False), (Just False, Just True :: Maybe Bool)])
    query_ conn "SELECT typed_tally(w, n) FROM typed_words" >>= (@?= [Only (Just "a=3,abcdefghij=1,abcdefghijk=1,apple=3,banana=5,cherry=2,\240\240\240\240\240\240=1" :: Maybe Text)])
    query_ conn "SELECT typed_tally(w, n) FROM typed_words WHERE g = 5" >>= (@?= [Only (Nothing :: Maybe Text)])
    query_ conn "SELECT typed_tally(w, n) OVER (ORDER BY g ROWS BETWEEN CURRENT ROW AND 1 FOLLOWING) FROM (VALUES (1, 'abcdefghij', 1::BIGINT), (2, 'abcdefghijk', 1), (3, NULL, 1)) t(g, w, n) ORDER BY g"
        >>= (@?= map Only [Just "abcdefghij=1,abcdefghijk=1", Just "abcdefghijk=1", Nothing :: Maybe Text])
    createAggregate conn "typed_any_odd" Aggregate{aggregateInitial = False, aggregateStep = \seen (Only (value :: Int64)) -> pure (seen || odd value), aggregateCombine = \a b -> pure (a || b), aggregateResult = pure}
    createAggregate conn "typed_mean" Aggregate{aggregateInitial = (0, 0), aggregateStep = \(total, count) (Only value) -> pure (total + value, count + 1 :: Int64), aggregateCombine = \(t1, c1) (t2, c2) -> pure (t1 + t2, c1 + c2), aggregateResult = \(total, count) -> pure (if count == 0 then Nothing else Just (total / fromIntegral count :: Double))}
    createAggregate conn "typed_count" Aggregate{aggregateInitial = 0, aggregateStep = \count (Only (_ :: Int32)) -> pure (count + 1), aggregateCombine = \a b -> pure (a + b), aggregateResult = pure :: Word64 -> IO Word64}
    createAggregate conn "typed_small" Aggregate{aggregateInitial = 0, aggregateStep = \total (Only value) -> pure (total + value), aggregateCombine = \a b -> pure (a + b), aggregateResult = pure :: Int32 -> IO Int32}
    createAggregate conn "typed_label" Aggregate{aggregateInitial = 0 :: Int, aggregateStep = \count (Only (_ :: Text)) -> pure (count + 1), aggregateCombine = \a b -> pure (a + b), aggregateResult = \count -> pure (Text.replicate count "ab")}
    query_ conn "SELECT typed_any_odd(i), typed_mean(i::DOUBLE), typed_count(i::INTEGER), typed_small(i::INTEGER), typed_label(i::VARCHAR) FROM range(8) t(i) GROUP BY i < 1 ORDER BY i < 1"
        >>= (@?= [(True, Just (4 :: Double), 7 :: Word64, 28 :: Int32, "ababababababab" :: Text), (False, Just 0, 1, 0, "ab")])
    query_ conn "SELECT typed_any_odd(i), typed_mean(i::DOUBLE), typed_count(i::INTEGER), typed_label(i::VARCHAR) FROM range(0) t(i)"
        >>= (@?= [(False, Nothing :: Maybe Double, 0 :: Word64, "" :: Text)])
    -- An Int32 argument is INTEGER, but an Int32 result is BIGINT.
    query_ conn "SELECT typeof(typed_any_odd(1)), typeof(typed_mean(1)), typeof(typed_count(1)), typeof(typed_small(1)), typeof(typed_label('x'))"
        >>= (@?= [("BOOLEAN" :: Text, "DOUBLE" :: Text, "UBIGINT" :: Text, "BIGINT" :: Text, "VARCHAR" :: Text)])
    query_ conn "SELECT function_name, parameter_types::VARCHAR FROM duckdb_functions() WHERE function_name IN ('typed_small', 'typed_tally') ORDER BY function_name"
        >>= (@?= [("typed_small" :: Text, "[INTEGER]" :: Text), ("typed_tally", "[VARCHAR, BIGINT]")])

-- | Fail the query for an exception in each function. The next query works.
testAggregateErrors :: Assertion
testAggregateErrors = withConnection ":memory:" \conn -> do
    createAggregate conn "typed_bad_step" sumAggregate{aggregateStep = \total (Only value) -> if value == 13 then throwIO (userError "typed_bad_step: 13") else pure (total + value)}
    assertSqlError "typed_bad_step: 13" (query_ conn "SELECT typed_bad_step(i) FROM range(20) t(i) GROUP BY i % 3" :: IO [Only Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    -- The step evaluates its state, so the error belongs to the step.
    createAggregate conn "typed_lazy_step" sumAggregate{aggregateStep = \total (Only value) -> pure (if value == 7 then error "typed_lazy_step: 7" else total + value)}
    assertSqlError "typed_lazy_step: 7" (query_ conn "SELECT typed_lazy_step(i) FROM range(20) t(i)" :: IO [Only Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    -- DuckDB adds rows 0 to 15 and rows 16 to 31 of the frame of row 0 in separate
    -- states. Then it combines them.
    createAggregate conn "typed_bad_combine" sumAggregate{aggregateCombine = \_ _ -> throwIO (userError "typed_bad_combine: combine")}
    assertSqlError "typed_bad_combine: combine" (query_ conn "SELECT typed_bad_combine(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) FROM range(32) t(i)" :: IO [Only Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    createAggregate conn "typed_bad_result" sumAggregate{aggregateResult = \total -> if total > 100 then throwIO (userError "typed_bad_result: too large") else pure total}
    query_ conn "SELECT typed_bad_result(i) FROM range(10) t(i)" >>= (@?= [Only (45 :: Int64)])
    assertSqlError "typed_bad_result: too large" (query_ conn "SELECT typed_bad_result(i) FROM range(100) t(i) GROUP BY i % 2" :: IO [Only Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    createAggregate conn "typed_lazy_result" sumAggregate{aggregateResult = \total -> pure (if total == 0 then error "typed_lazy_result: 0" else total)}
    assertSqlError "typed_lazy_result: 0" (query_ conn "SELECT typed_lazy_result(i) FROM range(1) t(i)" :: IO [Only Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Compare window, DISTINCT, and FILTER uses with the built-in sum. Use ORDER BY in the call.
testAggregateModifiers :: Assertion
testAggregateModifiers = withConnection ":memory:" \conn -> do
    createAggregate conn "typed_sum" sumAggregate
    forM_
        [ "ROWS BETWEEN 2 PRECEDING AND CURRENT ROW"
        , "ROWS UNBOUNDED PRECEDING"
        , "ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING"
        , "PARTITION BY i % 3"
        , "PARTITION BY i % 3 ORDER BY i"
        , ""
        ]
        \frame -> do
            let window = if Text.null frame || "PARTITION" `Text.isPrefixOf` frame then frame else "ORDER BY i " <> frame
            -- The built-in sum gives NULL for a frame without values. The initial state gives 0.
            assertColumnsEqual conn (Query ("SELECT typed_sum(v) OVER (" <> window <> "), coalesce(sum(v) OVER (" <> window <> "), 0)::BIGINT FROM (SELECT i, CASE WHEN i % 7 = 0 THEN NULL ELSE i END AS v FROM range(100) t(i))"))
    assertColumnsEqual conn "SELECT typed_sum(DISTINCT i % 5), sum(DISTINCT i % 5)::BIGINT FROM range(100) t(i)"
    assertColumnsEqual conn "SELECT typed_sum(DISTINCT i % 5), sum(DISTINCT i % 5)::BIGINT FROM range(100) t(i) GROUP BY i % 2"
    assertColumnsEqual conn "SELECT typed_sum(i) FILTER (WHERE i % 2 = 0), sum(i) FILTER (WHERE i % 2 = 0)::BIGINT FROM range(10) t(i)"
    createAggregate conn "typed_words" Aggregate{aggregateInitial = [], aggregateStep = \seen (Only word) -> pure (word : seen), aggregateCombine = \a b -> pure (b <> a), aggregateResult = pure . Text.intercalate "," . reverse}
    query_ conn "SELECT typed_words(w ORDER BY k DESC) FROM (VALUES (1, 'a'), (2, 'b'), (3, 'c')) t(k, w)" >>= (@?= [Only ("c,b,a" :: Text)])

{- | Release each Haskell state after a query, also after a failed query and
after a query that stops early. The step makes a new 'IORef' for each state and
keeps a weak pointer to it.
-}
testAggregateRelease :: Assertion
testAggregateRelease = withConnection ":memory:" \conn -> do
    cells <- newIORef []
    failure <- newIORef ""
    let newCell value = do
            cell <- newIORef value
            weak <- mkWeakIORef cell (pure ())
            atomicModifyIORef' cells \weaks -> (weak : weaks, ())
            pure (Just cell)
        failIn phase = readIORef failure >>= \current -> if current == phase then throwIO (userError ("typed_release: " <> phase)) else pure ()
        step state (Only value) = do
            failIn "step"
            maybe (newCell value) (\cell -> Just cell <$ atomicModifyIORef' cell \total -> (total + value, ())) state
        combine a b = do
            failIn "combine"
            totals <- mapM readIORef [cell | Just cell <- [a, b]]
            if null totals then pure Nothing else newCell (sum totals)
        result state = failIn "result" >> maybe (pure 0) readIORef state
        run phase sql = do
            writeIORef failure phase
            outcome <- try (query_ conn sql :: IO [Only Int64])
            case (phase, outcome) of
                ("", Left err) -> assertFailure (show err)
                ("", Right _) -> pure ()
                (_, Left (_ :: SQLError)) -> pure ()
                (_, Right _) -> assertFailure ("expected an error in " <> Text.unpack (fromQuery sql))
    createAggregate conn "typed_release" Aggregate{aggregateInitial = Nothing, aggregateStep = step, aggregateCombine = combine, aggregateResult = result :: Maybe (IORef Int64) -> IO Int64}
    run "" "SELECT typed_release(i) FROM range(5000) t(i) GROUP BY i % 50"
    run "" "SELECT typed_release(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) FROM range(100) t(i)"
    run "" "SELECT typed_release(DISTINCT i % 5) + typed_release(i) FROM range(100) t(i)"
    -- A prepared statement keeps its bind data over its runs. DuckDB keeps the
    -- states of the last run until the statement runs again or closes.
    withStatement conn "SELECT typed_release(i) FROM range(10) t(i)" \statement -> do
        nextRow statement >>= (@?= Just (Only (45 :: Int64)))
        void (executeStatement statement)
        earlier <- readIORef cells
        nextRow statement >>= (@?= Just (Only (45 :: Int64)))
        assertReleased "the earlier runs of a prepared statement" earlier
    run "step" "SELECT typed_release(i) FROM range(5000) t(i) GROUP BY i % 50"
    run "combine" "SELECT typed_release(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) FROM range(32) t(i)"
    -- DuckDB does not destroy these states. The release of the bind data releases them.
    run "result" "SELECT typed_release(i) FROM range(20000) t(i) GROUP BY i % 10000"
    run "result" "SELECT typed_release(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) FROM range(100) t(i)"
    void (execute_ conn "SET threads = 4")
    void (execute_ conn "CREATE TABLE typed_numbers AS SELECT i FROM range(300000) t(i)")
    run "" "SELECT typed_release(i) FROM range(20000) t(i) GROUP BY i % 10000 LIMIT 1"
    run "" "SELECT typed_release(i) FROM typed_numbers GROUP BY i % 5000 LIMIT 1"
    run "combine" "SELECT typed_release(i) FROM typed_numbers GROUP BY i % 50"
    run "result" "SELECT typed_release(i) FROM typed_numbers GROUP BY i % 5000"
    readIORef cells >>= assertReleased "the queries"

-- | Require that no weak pointer of the list is alive after a major collection.
assertReleased :: String -> [Weak (IORef Int64)] -> Assertion
assertReleased label weaks = do
    assertBool ("states made in " <> label) (not (null weaks))
    performMajorGC
    performMajorGC
    alive <- length . filter isJust <$> mapM deRefWeak weaks
    assertEqual ("live states after " <> label) 0 alive

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

-- | Read one column, both columns, and more than one chunk of the example.
testTableSquares :: Assertion
testTableSquares = withConnection ":memory:" \conn -> do
    createTableFunction conn "typed_squares" ["n", "square"] \(Only count) ->
        pure [(n, n * n) | n <- [0 .. count - 1 :: Int64]]
    query_ conn "SELECT * FROM typed_squares(3)" >>= (@?= [(0 :: Int64, 0 :: Int64), (1, 1), (2, 4)])
    query_ conn "SELECT square FROM typed_squares(3)" >>= (@?= map Only [0, 1, 4 :: Int64])
    query_ conn "SELECT * FROM typed_squares(0)" >>= (@?= ([] :: [(Int64, Int64)]))
    -- A chunk has at most 2048 rows. Thus 5000 rows need three chunks.
    query_ conn "SELECT n, square FROM typed_squares(5000)" >>= (@?= [(n, n * n) | n <- [0 .. 4999 :: Int64]])

-- | Call table functions with no parameter, two parameters, and NULL.
testTableParameters :: Assertion
testTableParameters = withConnection ":memory:" \conn -> do
    createTableFunction conn "typed_words" ["word"] \() -> pure (map Only ["eitt", "tvö" :: Text])
    query_ conn "SELECT word FROM typed_words()" >>= (@?= map Only ["eitt", "tvö" :: Text])
    createTableFunction conn "typed_labels" ["label", "n"] \(prefix :: Text, start :: Int64) ->
        pure [(prefix <> Text.pack (show n), n) | n <- [start .. start + 2]]
    query_ conn "SELECT * FROM typed_labels('x', 5)" >>= (@?= [("x5" :: Text, 5 :: Int64), ("x6", 6), ("x7", 7)])
    -- DuckDB casts an argument to the parameter type.
    query_ conn "SELECT n FROM typed_labels('y', 1::INTEGER)" >>= (@?= map Only [1, 2, 3 :: Int64])
    -- A NULL argument converts to Nothing.
    createTableFunction conn "typed_optional" ["value"] \(Only (value :: Maybe Int64)) -> pure [Only value]
    query_ conn "SELECT value FROM typed_optional(NULL)" >>= (@?= [Only (Nothing :: Maybe Int64)])
    query_ conn "SELECT value FROM typed_optional(4)" >>= (@?= [Only (Just 4 :: Maybe Int64)])

-- | A row type with a 'Generic' instance.
data Reading = Reading
    { readingStation :: Text
    , readingValue :: Double
    , readingDay :: Day
    , readingNote :: Maybe Text
    }
    deriving (Generic)

instance TypedRow Reading

-- | Write columns of several types and NULLs from tuples and from a record.
testTableTypes :: Assertion
testTableTypes = withConnection ":memory:" \conn -> do
    let start = fromGregorian 2026 11 12
        mixed = [("a" :: Text, 0.5 :: Double, start, Just (1 :: Int64)), ("liður", -1.25, addDays 1 start, Nothing)]
    createTableFunction conn "typed_mixed" ["label", "value", "day", "amount"] \() -> pure mixed
    query_ conn "SELECT * FROM typed_mixed()" >>= (@?= mixed)
    query_ conn "SELECT typeof(label), typeof(value), typeof(day), typeof(amount) FROM typed_mixed() LIMIT 1"
        >>= (@?= [("VARCHAR" :: Text, "DOUBLE" :: Text, "DATE" :: Text, "BIGINT" :: Text)])
    createTableFunction conn "typed_readings" ["station", "value", "day", "note"] \(Only count) ->
        pure [Reading ("s" <> Text.pack (show n)) (fromIntegral n / 2) (addDays (fromIntegral n) start) (if even n then Nothing else Just "odd") | n <- [0 .. count - 1 :: Int64]]
    query_ conn "SELECT * FROM typed_readings(3)"
        >>= (@?= [("s0" :: Text, 0 :: Double, start, Nothing :: Maybe Text), ("s1", 0.5, addDays 1 start, Just "odd"), ("s2", 1, addDays 2 start, Nothing)])
    query_ conn "SELECT count(*) FROM typed_readings(3000) WHERE note IS NULL" >>= (@?= [Only (1500 :: Int64)])

-- | Run the function for each scan. Do not run it at registration or preparation.
testTableRuns :: Assertion
testTableRuns = withConnection ":memory:" \conn -> do
    calls <- newIORef (0 :: Int)
    createTableFunction conn "typed_counted" ["n"] \(Only count) -> do
        modifyIORef' calls (+ 1)
        pure (map Only [1 .. count :: Int64])
    readIORef calls >>= (@?= 0)
    withStatement conn "SELECT n FROM typed_counted(2)" \statement -> do
        readIORef calls >>= (@?= 0)
        _ <- executeStatement statement
        readIORef calls >>= (@?= 1)
        _ <- executeStatement statement
        readIORef calls >>= (@?= 2)
    query_ conn "SELECT a.n, b.n FROM typed_counted(2) a JOIN typed_counted(3) b ON a.n = b.n ORDER BY a.n"
        >>= (@?= [(1 :: Int64, 1 :: Int64), (2, 2)])
    readIORef calls >>= (@?= 4)

-- | Read the first rows of an infinite list.
testTableLazy :: Assertion
testTableLazy = withConnection ":memory:" \conn -> do
    createTableFunction conn "typed_naturals" ["n"] \() -> pure (map Only [0 :: Int64 ..])
    query_ conn "SELECT n FROM typed_naturals() LIMIT 3" >>= (@?= map Only [0, 1, 2 :: Int64])
    -- 10000 rows need more than one chunk.
    query_ conn "SELECT sum(n)::BIGINT FROM (SELECT n FROM typed_naturals() LIMIT 10000)" >>= (@?= [Only (49995000 :: Int64)])

-- | Reject wrong column names at registration. Fail a query with an argument that does not convert.
testTableArgumentErrors :: Assertion
testTableArgumentErrors = withConnection ":memory:" \conn -> do
    assertSqlError "1 column names for 2 result columns" (createTableFunction conn "typed_bad_names" ["a"] \() -> pure [(1 :: Int64, 2 :: Int64)])
    assertSqlError "at least one result column" (createTableFunction conn "typed_no_columns" [] \() -> pure [()])
    -- DuckDB casts the argument to BIGNUM. A negative BIGNUM does not convert to Natural.
    createTableFunction conn "typed_natural" ["n"] \(Only (value :: Natural)) -> pure [Only (fromIntegral value :: Int64)]
    query_ conn "SELECT n FROM typed_natural(7)" >>= (@?= [Only (7 :: Int64)])
    assertSqlError "typed_natural: cannot convert argument arg1" (query_ conn "SELECT n FROM typed_natural(-7)" :: IO [Only Int64])
    assertSqlError "typed_natural: argument arg1 is NULL" (query_ conn "SELECT n FROM typed_natural(NULL)" :: IO [Only Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Fail a query with an exception in the function and in a row of the second chunk.
testTableExceptions :: Assertion
testTableExceptions = withConnection ":memory:" \conn -> do
    createTableFunction conn "typed_checked" ["n"] \(Only count) ->
        if count < 0 then throwIO (userError "typed_checked: the count is negative") else pure (map Only [1 .. count :: Int64])
    assertSqlError "typed_checked: the count is negative" (query_ conn "SELECT n FROM typed_checked(-1)" :: IO [Only Int64])
    query_ conn "SELECT n FROM typed_checked(2)" >>= (@?= map Only [1, 2 :: Int64])
    -- Row 3000 is in the second chunk.
    createTableFunction conn "typed_partial" ["n"] \() ->
        pure [Only (if n == 3000 then error "typed_partial: row 3000 fails" else n) | n <- [0 :: Int64 ..]]
    assertSqlError "typed_partial: row 3000 fails" (query_ conn "SELECT count(*) FROM typed_partial()" :: IO [Only Int64])
    query_ conn "SELECT n FROM typed_partial() LIMIT 2" >>= (@?= map Only [0, 1 :: Int64])
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

{- | Release the list of each scan after a full scan, after LIMIT, and after an
error. Each call makes a token. The rows that the scan did not read refer to
the token. Release the function when the connection closes.
-}
testTableRelease :: Assertion
testTableRelease = do
    closure <- withConnection ":memory:" \conn -> do
        tokens <- newIORef []
        offset <- newIORef (0 :: Int64)
        offsetWeak <- mkWeakIORef offset (pure ())
        createTableFunction conn "typed_tokens" ["n"] \(count :: Int64, failure :: Int64) -> do
            token <- readIORef offset >>= newIORef
            weak <- mkWeakIORef token (pure ())
            modifyIORef' tokens (weak :)
            let rowsFrom n
                    | n == count = pure []
                    | otherwise = unsafeInterleaveIO do
                        value <- readIORef token
                        when (n == failure) $ throwIO (userError "typed_tokens: the row fails")
                        (Only (value + n) :) <$> rowsFrom (n + 1)
            rowsFrom 0
        -- Check the token of the last scan before the next statement.
        let released scan = do
                performMajorGC
                performMajorGC
                scans <- readIORef tokens
                case scans of
                    weak : _ -> deRefWeak weak >>= assertBool (scan <> " keeps its list after the scan ends") . isNothing
                    [] -> assertFailure (scan <> " did not run the function")
        query_ conn "SELECT count(*) FROM typed_tokens(3000, -1)" >>= (@?= [Only (3000 :: Int64)])
        released "a full scan"
        query_ conn "SELECT n FROM typed_tokens(-1, -1) LIMIT 3" >>= (@?= map Only [0, 1, 2 :: Int64])
        released "a scan with LIMIT"
        assertSqlError "typed_tokens: the row fails" (query_ conn "SELECT count(*) FROM typed_tokens(-1, 2500)" :: IO [Only Int64])
        released "a failed scan"
        readIORef tokens >>= (@?= 3) . length
        pure offsetWeak
    performMajorGC
    performMajorGC
    deRefWeak closure >>= assertBool "the table function remains after the connection closes" . isNothing

{- | Write 3000 rows with NULLs to a file and read them back with one format.
The rows need more than one chunk in each direction.
-}
testCopyBothDirections :: Assertion
testCopyBothDirections = withDirectory \directory -> withConnection ":memory:" \conn -> do
    calls <- newIORef ([] :: [CopyFromInfo])
    registerCopyFunction conn "typed_lines" linesTo \info -> do
        modifyIORef' calls (info :)
        readLines (copyFromPath info)
    let file = directory </> "lines.txt"
        expected = [if i `mod` 1000 == 7 then Nothing else Just i | i <- [0 .. 2999 :: Int64]]
    void (execute conn "COPY (SELECT CASE WHEN i % 1000 = 7 THEN NULL ELSE i END AS value FROM range(3000) t(i)) TO ? (FORMAT typed_lines)" (Only (Text.pack file)))
    readFile file >>= (@?= unlines (map (maybe "" show) expected))
    void (execute_ conn "CREATE TABLE typed_copied (value BIGINT)")
    -- The function runs one time for each run of a COPY FROM statement.
    forM_ [1 :: Int, 2] \_ -> void (execute conn "COPY typed_copied FROM ? (FORMAT typed_lines)" (Only (Text.pack file)))
    query_ conn "SELECT value FROM typed_copied ORDER BY rowid" >>= (@?= map Only (expected <> expected))
    infos <- readIORef calls
    length infos @?= 2
    forM_ infos \info -> do
        copyFromPath info @?= file
        copyFromColumnNames info @?= ["value"]
        length (copyFromColumnTypes info) @?= 1

{- | DuckDB keeps the first registration of a format name. A later
registration has no effect, so it does not add a COPY FROM side.
-}
testCopyFirstRegistration :: Assertion
testCopyFirstRegistration = withDirectory \directory -> withConnection ":memory:" \conn -> do
    sinks <- newIORef ([] :: [Text])
    let file = Text.pack (directory </> "first.txt")
        register label = registerCopyToFunction conn "typed_first" (\_ -> pure ()) (\info -> writeFile (copyInitFilePath info) "") (\_ _ -> modifyIORef' sinks (label :)) (\_ -> pure ())
    register "first"
    register "second"
    registerCopyFromFunction conn "typed_first" \_ -> pure [Only (1 :: Int64)]
    void (execute conn "COPY (SELECT 1) TO ? (FORMAT typed_first)" (Only file))
    readIORef sinks >>= (@?= ["first"])
    void (execute_ conn "CREATE TABLE typed_first_rows (value BIGINT)")
    assertSqlError "COPY FROM is not supported for FORMAT \"typed_first\"" (execute conn "COPY typed_first_rows FROM ? (FORMAT typed_first)" (Only file))
    -- A format with a COPY FROM side only rejects COPY TO.
    registerCopyFromFunction conn "typed_from_only" \_ -> pure [Only (2 :: Int64)]
    void (execute conn "COPY typed_first_rows FROM ? (FORMAT typed_from_only)" (Only file))
    query_ conn "SELECT value FROM typed_first_rows" >>= (@?= [Only (2 :: Int64)])
    assertSqlError "COPY TO is not supported for FORMAT \"typed_from_only\"" (execute conn "COPY (SELECT 1) TO ? (FORMAT typed_from_only)" (Only file))
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

{- | Raise an error in each COPY TO function, in the COPY FROM function, and in
a lazy row after the first chunk. Each error fails its statement. The next
statement runs.
-}
testCopyErrors :: Assertion
testCopyErrors = withDirectory \directory -> withConnection ":memory:" \conn -> do
    let file = Text.pack (directory </> "errors.txt")
    void (execute_ conn "CREATE TABLE typed_failed (value BIGINT)")
    forM_ ["bind", "init", "sink", "finalize"] \phase -> do
        registerFailingFormat conn phase
        assertSqlError ("typed_copy_error: " <> phase) (execute conn (Query ("COPY (SELECT 1) TO ? (FORMAT typed_error_" <> phase <> ")")) (Only file))
        query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    forM_ ["from", "row"] \phase -> do
        registerFailingFormat conn phase
        assertSqlError ("typed_copy_error: " <> phase) (execute conn (Query ("COPY typed_failed FROM ? (FORMAT typed_error_" <> phase <> ")")) (Only file))
        query_ conn "SELECT count(*) FROM typed_failed" >>= (@?= [Only (0 :: Int64)])
    -- The rows of a format must have one column for each column of the table.
    void (execute_ conn "CREATE TABLE typed_wide (a BIGINT, b BIGINT)")
    registerFailingFormat conn "none"
    assertSqlError "the table of COPY FROM has 2 columns, but a row of typed_error_none has 1" (execute conn "COPY typed_wide FROM ? (FORMAT typed_error_none)" (Only file))
    void (execute conn "COPY typed_failed FROM ? (FORMAT typed_error_none)" (Only file))
    query_ conn "SELECT count(*) FROM typed_failed" >>= (@?= [Only (3000 :: Int64)])

-- | Register a format on one connection. Use it from another connection.
testCopySharedDatabase :: Assertion
testCopySharedDatabase = withDirectory \directory -> withEnvironment \environment -> withInstance environment [] \instance' -> do
    attachDatabase instance' ":memory:" "typed_shared" []
    setDefaultDatabase instance' "typed_shared"
    let file = Text.pack (directory </> "shared.txt")
    withConnectionOn instance' \first -> withConnectionOn instance' \second -> do
        registerCopyFunction first "typed_shared_lines" linesTo (readLines . copyFromPath)
        registerCopyFunction second "typed_shared_lines" linesTo \_ -> ioError (userError "the second registration runs") :: IO [Only Int64]
        void (execute second "COPY (SELECT i FROM range(5) t(i)) TO ? (FORMAT typed_shared_lines)" (Only file))
        void (execute_ first "CREATE TABLE typed_shared_rows (value BIGINT)")
        void (execute second "COPY typed_shared_rows FROM ? (FORMAT typed_shared_lines)" (Only file))
        void (execute first "COPY typed_shared_rows FROM ? (FORMAT typed_shared_lines)" (Only file))
        query_ first "SELECT value FROM typed_shared_rows ORDER BY rowid" >>= (@?= map (Only . Just) ([0 .. 4] <> [0 .. 4 :: Int64]))

-- | A COPY TO side that writes one BIGINT column as lines. A NULL is an empty line.
linesTo :: CopyTo () FilePath
linesTo =
    CopyTo
        { copyToBind = \info -> when (length (copyBindColumnTypes info) /= 1) (ioError (userError "typed_lines writes one column"))
        , copyToInit = \info -> writeFile (copyInitFilePath info) "" >> pure (copyInitFilePath info)
        , copyToSink = \info rows -> appendFile (copySinkGlobalState info) (unlines (map line rows))
        , copyToFinalize = \_ -> pure ()
        }
  where
    line [Field{fieldValue = FieldInt64 value}] = show value
    line [Field{fieldValue = FieldNull}] = ""
    line row = error ("typed_lines: unexpected row " <> show row)

-- | Read a file of 'linesTo'. An empty line is NULL.
readLines :: FilePath -> IO [Only (Maybe Int64)]
readLines path = map (Only . parse . Text.unpack) . Text.lines <$> TextIO.readFile path
  where
    parse "" = Nothing
    parse line = Just (read line)

{- | Register the format @typed_error_@ with the phase as suffix. The function
of that phase raises an error. The phase @row@ makes the row at index 2500 raise
an error. That row is in the second chunk.
-}
registerFailingFormat :: Connection -> Text -> IO ()
registerFailingFormat conn phase =
    registerCopyFunction conn ("typed_error_" <> phase) failing \_ -> failIn "from" >> pure rows
  where
    failIn name = when (phase == name) (ioError (userError ("typed_copy_error: " <> Text.unpack name)))
    failing = CopyTo (\_ -> failIn "bind") (\info -> failIn "init" >> writeFile (copyInitFilePath info) "") (\_ _ -> failIn "sink") (\_ -> failIn "finalize")
    rows = [Only (if phase == "row" && i == 2500 then error "typed_copy_error: row" else i) | i <- [0 .. 2999 :: Int64]]

{- | Read 20 files through a multi-file function over a typed table function.
DuckDB runs exec callbacks of one scan at the same time for a multi-file
function. Each row must come one time.
-}
testTableMultiFile :: Assertion
testTableMultiFile = withDirectory \directory -> withConnection ":memory:" \conn -> do
    createTableFunction conn "typed_numbers" ["value"] \(Only path) ->
        map (Only . read) . lines <$> readFile (Text.unpack path) :: IO [Only Int64]
    registerMultiFileFunction conn "typed_all_numbers" "typed_numbers" defaultMultiFileOptions
    let numbers file = [file * 1000 + row | row <- [0 .. 99]] :: [Int64]
    forM_ [0 .. 19] \file -> writeFile (directory </> ("numbers" <> show file <> ".txt")) (unlines (map show (numbers file)))
    forM_ [1 :: Int .. 5] \_ ->
        query conn "SELECT value FROM typed_all_numbers(?) ORDER BY value" (Only (Text.pack (directory </> "numbers*.txt")))
            >>= (@?= map Only (concatMap numbers [0 .. 19]))

-- | Create and remove a unique directory for the files of a test.
withDirectory :: (FilePath -> IO a) -> IO a
withDirectory = bracket create removePathForcibly
  where
    create = do
        root <- getTemporaryDirectory
        (path, handle) <- openTempFile root "duckdb-typed-copy"
        hClose handle
        removeFile path
        createDirectory path
        pure path
