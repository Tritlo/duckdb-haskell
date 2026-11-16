{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : TypedRegistrationTests
-- Description : Integration checks for typed registration through SQL.
module TypedRegistrationTests (typedRegistrationTests) where

import Control.Exception (SomeException, bracket, displayException, throwIO, try)
import Control.Monad (forM, forM_, void, when)
import Data.Array (listArray)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.LocalTime (TimeOfDay (..))
import Data.Word (Word64)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Config (
  CustomOption (..),
  SettingScope (..),
  customOption,
  getConfigValue,
  registerCustomOption,
  setConfigValue,
 )
import Database.DuckDB.Simple.Copy (
  CopyBindInfo (..),
  CopyFromInfo (..),
  CopyInitInfo (..),
  CopySinkInfo (..),
  CopyTo (..),
  defaultMultiFileOptions,
  registerCopyFromFunction,
  registerCopyFunction,
  registerCopyToFunction,
  registerMultiFileFunction,
 )
import Database.DuckDB.Simple.FromField (BigNum (..), Field (..), FieldValue (..), IntervalValue (..), TimeWithZone (..))
import Database.DuckDB.Simple.Function (FunctionResult)
import Database.DuckDB.Simple.Generic (DuckValue (..), ViaDuckDB (..))
import Database.DuckDB.Simple.Internal (Connection (..))
import qualified Database.DuckDB.Simple.Logging as Logging
import Database.DuckDB.Simple.LogicalRep (
  LogicalTypeRep (..),
  StructValue (..),
  TupleValue,
  UnionValue,
 )
import Database.DuckDB.Simple.Pragma (PragmaCallback (..), registerPragmaFunction)
import Database.DuckDB.Simple.Registration (
  FunctionParameter (..),
  Replacement (..),
  TypeDeclaration (..),
  defaultValue,
  parameter,
  registerCustomType,
  registerInstanceReplacementScan,
  registerReplacementScan,
 )
import Database.DuckDB.Simple.ToField (ToDuckValue (..))
import qualified Database.DuckDB.Simple.V2.Registration as R
import Database.DuckDB.Simple.Variant (Variant)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, (@?=))
import TestUtils (assertInputErrorContaining, assertReleasedAfter, writeNullRows)

-- | Check the typed registrations through SQL.
typedRegistrationTests :: TestTree
typedRegistrationTests =
  testGroup
    "typed registration"
    [ testCase "a scalar function that changes a value gives the rows of its Haskell result in a filter and a join" testScalarFilterJoin
    , testCase "replacement scan reads the rows of a query" testReplacementQuery
    , testCase "replacement scan that declines gives the unknown table error" testReplacementDecline
    , testCase "replacement scan gets the name parts as written" testReplacementNames
    , testCase "replacement scan errors fail the query" testReplacementErrors
    , testCase "connection replacement scan serves only its connection" testReplacementConnectionScope
    , testCase "instance replacement scan serves each connection" testReplacementInstance
    , testCase "replacement scans run in the order of registration" testReplacementOrder
    , testCase "query pragma with no parameter" testPragmaNoParameter
    , testCase "query pragma with a default parameter" testPragmaDefault
    , testCase "query pragma with an Integer default for each integer type" testPragmaIntegerDefault
    , testCase "pragma row has the order of the declaration" testPragmaRow
    , testCase "exec pragma counts its calls" testPragmaExec
    , testCase "pragma argument rows that do not decode" testPragmaDecodeErrors
    , testCase "pragma parameters whose types contain VARIANT fail at registration" testPragmaVariantParameters
    , testCase "pragma exceptions become SQL errors" testPragmaExceptions
    , testCase "pragma registration checks the name before the state of a closed connection" testPragmaClosedName
    , testCase "an SQL error in each callback kind keeps its type and one prefix" testCallbackErrorTypes
    , testCase "an SQL error of each type in a callback under the syntactic invalidation policy" testCallbackErrorPolicy
    , testCase "custom option update accepts and rejects BIGINT values" testOptionUpdate
    , testCase "custom option update gets VARCHAR values and can reject RESET" testOptionText
    , testCase "table function with one parameter over several chunks" testTableSquares
    , testCase "table function with no, two, and NULL parameters" testTableParameters
    , testCase "table function columns of several types and a record row" testTableTypes
    , testCase "table function with Integer and Natural columns" testTableIntegers
    , testCase "table function with INTERVAL and TIMETZ columns" testTableIntervals
    , testCase "table function with a MAP column" testTableMaps
    , testCase "table function and aggregate with record, list, map, and union types" testTableRecords
    , testCase "a record field of an alias type declares the alias" testAliasDeclaration
    , testCase "a scalar function argument gives its type to FromField" testArgumentType
    , testCase "typed registration rejects a type with no declaration" testNoDeclaration
    , testCase "a newtype with a sized ARRAY type text declares an ARRAY in each position" testSizedArray
    , testCase "record field outside the range of its declared type" testRecordFieldRange
    , testCase "table function runs one time for each scan" testTableRuns
    , -- A defect can make the scan run without end. Then the time limit fails
      -- the case. The limit is long, because the suite also runs under Valgrind.
      localOption (mkTimeout (10 * 60 * 1000000)) (testCase "table function reads an infinite list with LIMIT" testTableLazy)
    , testCase "table function registration and argument errors" testTableArgumentErrors
    , testCase "table function exceptions become SQL errors" testTableExceptions
    , testCase "table function evaluates only the columns that the query uses" testTableProjection
    , testCase "table function releases the list of each scan" testTableRelease
    , testCase "multi-file function gives each row of a typed table function one time" testTableMultiFile
    , testCase "COPY format with both directions under one name" testCopyBothDirections
    , testCase "COPY keeps the first registration of a format name" testCopyFirstRegistration
    , testCase "COPY TO registration checks the name before the state of a closed connection" testCopyClosedName
    , testCase "COPY errors in each function fail only their statement" testCopyErrors
    , testCase "COPY format on two connections of one database" testCopySharedDatabase
    , testCase "COPY FROM gives the name and the type of each column" testCopyFromColumns
    , testCase "aggregate over one group and over many groups" testAggregateGroups
    , testCase "aggregate with other state table stripe counts" testAggregateStripes
    , testCase "aggregate registration does not keep its connection" testAggregateConnectionRelease
    , testCase "aggregate skips NULL rows and gives the initial result" testAggregateNulls
    , testCase "aggregate arguments, states, and result types" testAggregateTypes
    , testCase "aggregate exceptions fail the query only" testAggregateErrors
    , testCase "aggregate as a window function and with modifiers" testAggregateModifiers
    , testCase "aggregate SMALLINT and FLOAT results, and the limit of the result types" testAggregateResultTypes
    , testCase "aggregate states are released" testAggregateRelease
    , testGroup
        "the callback closure of each registration kind is released when its database closes"
        [testCase kind (assertReleasedAfter kind use) | (kind, use) <- closureKinds]
    ]

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

-- | Give an Integer default to parameters of several integer types. DuckDB
-- casts the default to the type of the parameter and checks the range.
testPragmaIntegerDefault :: Assertion
testPragmaIntegerDefault = withConnection ":memory:" \conn -> do
  seen <- newIORef FieldNull
  let register name dtype (value :: Integer) =
        registerPragmaFunction conn name [(parameter "amount" (TypeExpression dtype)){parameterDefault = Just (defaultValue value)}] $
          QueryPragma \(Only received) -> writeIORef seen received >> pure "SELECT 1::BIGINT"
      omitted name = do
        query_ conn (Query ("PRAGMA " <> name)) >>= (@?= [Only (1 :: Int64)])
        readIORef seen
  register "typed_default_bigint" "BIGINT" 42
  omitted "typed_default_bigint" >>= (@?= FieldInt64 42)
  register "typed_default_hugeint" "HUGEINT" 42
  omitted "typed_default_hugeint" >>= (@?= FieldHugeInt 42)
  register "typed_default_bignum" "BIGNUM" 42
  omitted "typed_default_bignum" >>= (@?= FieldBigNum (BigNum 42))
  register "typed_default_unsigned" "UBIGINT" (-5)
  assertSqlError "out of range for the destination type UINT64" (query_ conn "PRAGMA typed_default_unsigned" :: IO [Only Int64])

-- | Decode two parameters of different types as a tuple. The row has the order
-- of the declaration when the call gives the arguments by name in another order.
-- An omitted argument has its default value.
testPragmaRow :: Assertion
testPragmaRow = withConnection ":memory:" \conn -> do
  let label = (parameter "label" (TypeExpression "VARCHAR")){parameterDefault = Just (defaultValue ("none" :: Text))}
  registerPragmaFunction conn "typed_pair" [parameter "amount" (TypeExpression "BIGINT"), label] $
    QueryPragma \(amount :: Int64, text :: Text) -> pure ("SELECT " <> Text.pack (show amount) <> "::BIGINT, '" <> Text.replace "'" "''" text <> "'")
  query_ conn "PRAGMA typed_pair(3, 'liður ''x''')" >>= (@?= [(3 :: Int64, "liður 'x'" :: Text)])
  query_ conn "PRAGMA typed_pair(label = 'named', amount = 4)" >>= (@?= [(4 :: Int64, "named" :: Text)])
  query_ conn "PRAGMA typed_pair(5)" >>= (@?= [(5 :: Int64, "none" :: Text)])

-- | Run the action of an exec pragma once for each statement. The action gets
-- the argument or the default value.
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
  registerPragmaFunction
    conn
    "typed_text"
    [parameter "label" (TypeExpression "VARCHAR")]
    (QueryPragma \(Only (_ :: Int64)) -> pure "SELECT 1")
  assertSqlError
    "cannot decode the arguments of pragma typed_text: column label has type"
    (query_ conn "PRAGMA typed_text('x')" :: IO [Only Int64])
  registerPragmaFunction conn "typed_size" [amount] (QueryPragma \() -> pure "SELECT 1")
  assertSqlError "cannot decode the arguments of pragma typed_size" (query_ conn "PRAGMA typed_size(1)" :: IO [Only Int64])
  registerPragmaFunction conn "typed_strict" [amount] (ExecPragma \(Only (_ :: Int64)) -> pure ())
  assertSqlError "cannot decode the arguments of pragma typed_strict: column amount is NULL" (execute_ conn "PRAGMA typed_strict(NULL)")
  assertInputError
    "typed_variant: parameter value: the library cannot read a VARIANT value in a callback; declare another type, such as VARCHAR, and cast the value in SQL, for example v::VARCHAR"
    ( registerPragmaFunction
        conn
        "typed_variant"
        [parameter "value" (TypeExpression "VARIANT")]
        (QueryPragma \(Only (_ :: Variant)) -> pure "SELECT 1")
    )
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Reject a pragma parameter whose type is or contains VARIANT at
-- registration, because the argument reader reads no VARIANT. A GEOMETRY
-- parameter registers, because the reader reads GEOMETRY. A parameter of the
-- type ANY registers, and a VARIANT value fails the call.
testPragmaVariantParameters :: Assertion
testPragmaVariantParameters = withConnection ":memory:" \conn -> do
  let register name declaration =
        registerPragmaFunction conn name [parameter "value" (TypeExpression declaration)] $
          QueryPragma \(Only (_ :: FieldValue)) -> pure "SELECT 1"
  forM_ [("typed_variant_scalar", "VARIANT"), ("typed_variant_list", "VARIANT[]"), ("typed_variant_struct", "STRUCT(v VARIANT)")] \(name, declaration) ->
    assertInputError
      ( name
          <> ": parameter value: the library cannot read a VARIANT value in a callback; declare another type, such as VARCHAR, and cast the value in SQL, for example v::VARCHAR"
      )
      (register name declaration)
  register "typed_geometry_parameter" "GEOMETRY"
  query_ conn "PRAGMA typed_geometry_parameter('POINT (1 2)'::GEOMETRY)" >>= (@?= [Only (1 :: Int64)])
  -- The check sees only the declared type. A parameter of the type ANY
  -- registers, and a VARIANT value fails the call.
  registerPragmaFunction conn "typed_any_parameter" [parameter "value" (StructuredType (LogicalTypeScalar LogicalTypeIdAny))] $
    QueryPragma \(Only (_ :: FieldValue)) -> pure "SELECT 1"
  query_ conn "PRAGMA typed_any_parameter(1)" >>= (@?= [Only (1 :: Int64)])
  assertSqlError
    "typed_any_parameter: parameter value: the library cannot read a VARIANT value in a callback; declare another type, such as VARCHAR, and cast the value in SQL, for example v::VARCHAR"
    (query_ conn "PRAGMA typed_any_parameter(1::VARIANT)" :: IO [Only Int64])

-- | Raise the input error of an empty pragma name, also on a closed
-- connection. The name check comes before the read of the state of the
-- connection.
testPragmaClosedName :: Assertion
testPragmaClosedName = do
  closed <- open ":memory:"
  close closed
  assertInputErrorContaining "pragma function name must be nonempty" $
    registerPragmaFunction closed "" [] (QueryPragma \() -> pure "SELECT 1")

-- | Make an SQL error from an exception of each callback kind.
testPragmaExceptions :: Assertion
testPragmaExceptions = withConnection ":memory:" \conn -> do
  registerPragmaFunction conn "typed_query_failure" [] (QueryPragma \() -> throwIO (userError "typed query pragma failure"))
  assertSqlError "typed query pragma failure" (query_ conn "PRAGMA typed_query_failure" :: IO [Only Int64])
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
  registerPragmaFunction conn "typed_exec_failure" [] (ExecPragma \() -> throwIO (userError "typed exec pragma failure"))
  assertSqlError "typed exec pragma failure" (execute_ conn "PRAGMA typed_exec_failure")
  query_ conn "SELECT 43" >>= (@?= [Only (43 :: Int64)])

-- | Give the type and the text of an error in a callback to the outer
-- statement. The failing statement runs on a connection of another database.
-- See Note [Errors that leave a callback] in "Database.DuckDB.Simple.Callback".
testCallbackErrorTypes :: Assertion
testCallbackErrorTypes = withDirectory \directory -> withConnection ":memory:" \conn -> withConnection ":memory:" \other -> withConnection ":memory:" \third -> do
  let failing = void (query_ other "SELECT 'abc'::INTEGER" :: IO [Only Int64])
      run sql = void (query_ conn sql :: IO [[FieldValue]])
      file = Text.pack (directory </> "rows.txt")
      conversion = "Conversion Error: Could not convert string 'abc' to INT32"
  -- The body of a native error is its text without the prefix.
  try failing >>= \case
    Left (err :: SQLError) -> ("Conversion Error: " <>) <$> sqlErrorBody err @?= Just (sqlErrorMessage err)
    Right () -> assertFailure "expected a Conversion Error"
  writeFile (directory </> "rows.txt") ""
  createFunction conn "error_scalar" (failing >> pure (0 :: Int64))
  createFunctionWithState conn "error_state" failing \() -> (0 :: Int64)
  createAggregate conn "error_update" sumAggregate{aggregateStep = \_ (Only (_ :: Int64)) -> failing >> pure 0}
  createAggregate conn "error_combine" sumAggregate{aggregateCombine = \_ _ -> failing >> pure 0}
  createAggregate conn "error_finalize" sumAggregate{aggregateResult = \_ -> failing >> pure (0 :: Int64)}
  createTableFunction conn "error_init" ["n"] \() -> failing >> pure [Only (1 :: Int64)]
  createTableFunction conn "error_exec" ["n"] \() -> unsafeInterleaveIO (failing >> pure [Only (1 :: Int64)])
  registerCustomType conn "error_target" (TypeExpression "BIGINT")
  R.registerCast conn (TypeExpression "BIGINT") (TypeExpression "error_target") Nothing \_ _ -> failing
  registerPragmaFunction conn "error_query_pragma" [] (QueryPragma \() -> failing >> pure "SELECT 1")
  registerPragmaFunction conn "error_exec_pragma" [] (ExecPragma \() -> failing)
  registerCustomOption conn (customOption "error_option" (TypeExpression "BIGINT")){customOptionUpdate = Just \_ -> failing}
  registerCopyToFunction conn "error_sink" (\_ -> pure ()) (\_ -> pure ()) (\_ _ -> failing) (\_ -> pure ())
  registerCopyToFunction conn "error_final" (\_ -> pure ()) (\_ -> pure ()) (\_ _ -> pure ()) (\_ -> failing)
  registerCopyFromFunction conn "error_from" \_ -> failing >> pure [Only (1 :: Int64)]
  void (execute_ conn "CREATE TABLE error_rows (value BIGINT)")
  registerReplacementScan conn \parts -> if parts == ["error_missing"] then failing >> pure Nothing else pure Nothing
  forM_
    [ ("scalar function", run "SELECT error_scalar()")
    , ("state of a scalar function", run "SELECT error_state() FROM range(3)")
    , ("aggregate update", run "SELECT error_update(i) FROM range(10) t(i)")
    , ("aggregate combine", run "SELECT error_combine(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) FROM range(32) t(i)")
    , ("aggregate finalize", run "SELECT error_finalize(i) FROM range(10) t(i)")
    , ("table function init", run "SELECT * FROM error_init()")
    , ("table function exec", run "SELECT * FROM error_exec()")
    , ("query pragma", run "PRAGMA error_query_pragma")
    , ("exec pragma", void (execute_ conn "PRAGMA error_exec_pragma"))
    , ("option update", void (execute_ conn "SET error_option = 1"))
    , ("COPY TO sink", void (execute conn "COPY (SELECT 1) TO ? (FORMAT error_sink)" (Only file)))
    , ("COPY TO finalize", void (execute conn "COPY (SELECT 1) TO ? (FORMAT error_final)" (Only file)))
    , ("COPY FROM", void (execute conn "COPY error_rows FROM ? (FORMAT error_from)" (Only file)))
    , ("replacement scan", run "SELECT * FROM error_missing")
    ]
    \(label, action) -> assertErrorType label ErrorTypeConversion conversion action
  -- A cast writes the code of a failed cast and the whole message. See
  -- Note [The error code of a failed cast] in
  -- "Database.DuckDB.Simple.V2.Registration".
  assertErrorType "cast" ErrorTypeMismatch ("Mismatch Type Error: " <> conversion) (run "SELECT CAST(1::BIGINT AS error_target)::BIGINT")
  -- An error that passes through two callbacks keeps its type.
  createFunction other "error_middle" (void (query_ third "SELECT 'abc'::INTEGER" :: IO [Only Int64]) >> pure (0 :: Int64))
  createFunction conn "error_outer" (void (query_ other "SELECT error_middle()" :: IO [Only Int64]) >> pure (0 :: Int64))
  assertErrorType "two callbacks" ErrorTypeConversion conversion (run "SELECT error_outer()")
  -- An error of the library keeps its type and gets one prefix.
  createFunction
    conn
    "error_range"
    (void (query other "SELECT ?::TINYINT" (Only (2 ^ (200 :: Int) :: Integer)) :: IO [Only Int64]) >> pure (0 :: Int64))
  assertErrorType "an error of the library" ErrorTypeConversion "Conversion Error: duckdb-simple: the value " (run "SELECT error_range()")
  -- An INTERNAL error does not keep its type, because DuckDB then invalidates
  -- the database.
  createFunction
    conn
    "error_internal"
    (throwIO (SQLError "INTERNAL Error: inner" (Just ErrorRuntimeInternal) Nothing (Just "inner")) :: IO Int64)
  assertErrorType "an INTERNAL error" ErrorInputInvalid "Invalid Input Error: INTERNAL Error: inner" (run "SELECT error_internal()")
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Raise an 'SQLError' of each type in a scalar function inside an INSERT
-- that has written rows, in an explicit transaction, under the DuckDB setting
-- SYNTACTIC_ERRORS_DO_NOT_INVALIDATE. Check the type of the outer error, the
-- rows of that INSERT after COMMIT, and that the same connection and a new
-- connection of the database still run a query. See
-- Note [Errors that leave a callback] in "Database.DuckDB.Simple.Callback".
testCallbackErrorPolicy :: Assertion
testCallbackErrorPolicy = do
  outcomes <- forM [minBound .. maxBound] \errorType -> withEnvironment \environment -> withInstance environment [] \instance' -> do
    attachDatabase instance' ":memory:" "typed_policy" []
    setDefaultDatabase instance' "typed_policy"
    (outerType, rows, same) <- withConnectionOn instance' \conn -> do
      -- With one thread, the INSERT writes two chunks of 2048 rows before the
      -- chunk with row 5000 fails.
      void (execute_ conn "SET threads = 1")
      void (execute_ conn "SET default_transaction_invalidation_policy = 'SYNTACTIC_ERRORS_DO_NOT_INVALIDATE'")
      void (execute_ conn "SET current_transaction_invalidation_policy = 'SYNTACTIC_ERRORS_DO_NOT_INVALIDATE'")
      void (execute_ conn "CREATE TABLE typed_policy_rows (v BIGINT)")
      createFunction conn "typed_policy_fail" \(i :: Int64) ->
        if i < 5000
          then pure i
          else
            throwIO
              SQLError{sqlErrorMessage = "typed failure", sqlErrorType = Just errorType, sqlErrorQuery = Nothing, sqlErrorBody = Just "typed failure"}
              :: IO Int64
      void (execute_ conn "BEGIN")
      void (execute_ conn "INSERT INTO typed_policy_rows VALUES (-1)")
      outer <- try (execute_ conn "INSERT INTO typed_policy_rows SELECT typed_policy_fail(i) FROM range(10000) t(i)")
      void (execute_ conn "COMMIT")
      rows <- query_ conn "SELECT count(*) FROM typed_policy_rows WHERE v >= 0"
      same <- query_ conn "SELECT 42"
      pure (either sqlErrorType (const Nothing) (outer :: Either SQLError Int), rows, same)
    new <- withConnectionOn instance' \other -> query_ other "SELECT 42"
    pure (errorType, outerType, rows :: [Only Int64], same :: [Only Int64], new :: [Only Int64])
  let invalidInput =
        [ ErrorQueryBinder
        , ErrorDatabaseCatalog
        , ErrorResourceConnection
        , ErrorQueryParameterNotAllowed
        , ErrorQueryParser
        , ErrorConfigurationPermission
        , ErrorResourceInUse
        , ErrorApi
        , ErrorIOFileNotFound
        , ErrorIOReadFailure
        , ErrorIOEof
        , ErrorInputParameterInvalid
        , ErrorInputMetadataDeferred
        , ErrorIONetwork
        , ErrorRuntimeInternal
        , ErrorRuntimeFatal
        ]
      expectedType errorType
        | errorType `elem` invalidInput = ErrorInputInvalid
        | otherwise = errorType
  outcomes
    @?= [ (errorType, Just (expectedType errorType), [Only 0], [Only 42], [Only 42])
        | errorType <- [minBound .. maxBound]
        ]

-- | Require an 'SQLError' of the type, with a message that starts with the
-- text.
assertErrorType :: String -> ErrorType -> Text -> IO () -> Assertion
assertErrorType label expected prefix action =
  try action >>= \case
    Left (err :: SQLError) -> do
      assertEqual label (Just expected) (sqlErrorType err)
      assertBool (label <> ": " <> show (sqlErrorMessage err)) (prefix `Text.isPrefixOf` sqlErrorMessage err)
    Right () -> assertFailure (label <> ": expected an SQL error")

-- | Accept and reject the values of a BIGINT option from SET, RESET, and
-- 'setConfigValue'. The function gets each value after the cast to BIGINT. A
-- rejected value does not change the option.
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

-- | Give the values of a VARCHAR option to the function. A RESET of an option
-- without a default gives NULL. A function that rejects NULL rejects the RESET.
testOptionText :: Assertion
testOptionText = withConnection ":memory:" \conn -> do
  seen <- newIORef []
  registerCustomOption
    conn
    (customOption "typed_option_label" (TypeExpression "VARCHAR")){customOptionUpdate = Just \value -> modifyIORef' seen (value :)}
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

-- | A scalar function that changes its argument gives the rows of its Haskell
-- result in a filter, in an equality filter, and in a join condition. A
-- registered cast between two built-in types can give other rows there, so
-- the README names a scalar function for such a conversion.
testScalarFilterJoin :: Assertion
testScalarFilterJoin = withConnection ":memory:" \conn -> do
  let bend x = 100 - x * x :: Int64
      values = [-20 .. 20]
      targets = [100, 99, 64, 0, -300]
  createFunction conn "typed_bend" bend
  void (execute_ conn "CREATE TABLE bend_values AS SELECT i::BIGINT AS c FROM range(-20, 21) t(i)")
  void (execute_ conn "CREATE TABLE bend_targets AS SELECT d::BIGINT AS d FROM (VALUES (100), (99), (64), (0), (-300)) t(d)")
  query_ conn "SELECT c FROM bend_values WHERE typed_bend(c) > 50 ORDER BY c" >>= (@?= [Only c | c <- values, bend c > 50])
  query_ conn "SELECT c FROM bend_values WHERE typed_bend(c) = 100 ORDER BY c" >>= (@?= [Only c | c <- values, bend c == 100])
  query_ conn "SELECT a.c, u.d FROM bend_values a JOIN bend_targets u ON typed_bend(a.c) = u.d ORDER BY a.c"
    >>= (@?= [(c, d) | c <- values, d <- targets, bend c == d])

-- | Replace one name with a query. Read the rows under the table name and an alias.
testReplacementQuery :: Assertion
testReplacementQuery = withConnection ":memory:" \conn -> do
  _ <- execute_ conn "CREATE TABLE typed_present AS SELECT 9::BIGINT AS amount"
  registerReplacementScan conn \parts ->
    pure
      (if parts `elem` [["typed_missing"], ["typed_present"]] then Just (ReplaceWithQuery "SELECT i AS amount FROM range(3) t(i)") else Nothing)
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

-- | Register a scan on one connection of an instance. The other connection does
-- not use it. The scan replaces a file path before the built-in file scan.
testReplacementConnectionScope :: Assertion
testReplacementConnectionScope = withEnvironment \environment -> withInstance environment [] \instance' ->
  withConnectionOn instance' \first -> withConnectionOn instance' \second -> do
    registerReplacementScan first \parts ->
      pure (if parts `elem` [["typed_scoped"], ["typed_missing.csv"]] then Just (ReplaceWithQuery "SELECT 'first' AS source") else Nothing)
    query_ first "SELECT source FROM typed_scoped" >>= (@?= [Only ("first" :: Text)])
    assertSqlError "Table with name typed_scoped does not exist" (query_ second "SELECT source FROM typed_scoped" :: IO [Only Text])
    query_ first "SELECT source FROM 'typed_missing.csv'" >>= (@?= [Only ("first" :: Text)])
    assertSqlError "No files found" (query_ second "SELECT source FROM 'typed_missing.csv'" :: IO [Only Text])

-- | Register a scan on an instance before its connections. Each connection uses
-- it. A connection scan comes first and passes a declined name to it.
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
  combines <- newIORef (0 :: Int)
  createAggregate
    conn
    "typed_sum"
    sumAggregate{aggregateCombine = \a b -> atomicModifyIORef' combines (\count -> (count + 1, ())) >> pure (a + b)}
  query_ conn "SELECT typed_sum(i) FROM range(10) t(i)" >>= (@?= [Only (45 :: Int64)])
  -- A chunk has at most 2048 rows. Thus 5000 rows need more than one update call.
  assertColumnsEqual conn "SELECT typed_sum(i), sum(i)::BIGINT FROM range(5000) t(i) GROUP BY i % 50"
  query_ conn "SELECT count(*) FROM (SELECT i % 50 AS g, typed_sum(i) FROM range(5000) t(i) GROUP BY g)" >>= (@?= [Only (50 :: Int64)])
  -- A table of 3 row groups lets 4 threads make partial states. DuckDB then combines them.
  void (execute_ conn "SET threads = 4")
  void (execute_ conn "CREATE TABLE typed_numbers AS SELECT i FROM range(300000) t(i)")
  assertColumnsEqual conn "SELECT typed_sum(i), sum(i)::BIGINT FROM typed_numbers GROUP BY i % 1000"
  -- An ungrouped aggregate combines the state of each thread into one state.
  -- Thus the combine function runs at least one time.
  writeIORef combines 0
  assertColumnsEqual conn "SELECT typed_sum(i), sum(i)::BIGINT FROM typed_numbers"
  readIORef combines >>= \count -> assertBool ("combine calls: " <> show count) (count > 0)
  query_ conn "SELECT typed_sum(i), typed_sum(i + 1) FROM range(10) t(i)" >>= (@?= [(45 :: Int64, 55 :: Int64)])

-- | One stripe gives the same results as the default stripes, also with
-- partial states from 4 threads. More stripes than states give the same
-- results. A connection with another stripe count calls the aggregate of a
-- first connection. A stripe count of 0 fails the open.
testAggregateStripes :: Assertion
testAggregateStripes = do
  withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionAggregateStripes = 1} \conn -> do
    createAggregate conn "typed_sum" sumAggregate
    void (execute_ conn "SET threads = 4")
    void (execute_ conn "CREATE TABLE typed_numbers AS SELECT i FROM range(300000) t(i)")
    assertColumnsEqual conn "SELECT typed_sum(i), sum(i)::BIGINT FROM typed_numbers GROUP BY i % 1000"
    assertColumnsEqual conn "SELECT typed_sum(i), sum(i)::BIGINT FROM typed_numbers"
  withEnvironment \environment -> withInstance environment [] \instance' ->
    bracket (connectWithOptions instance' defaultConnectionOptions{connectionAggregateStripes = 1000}) close \first ->
      bracket (connectWithOptions instance' defaultConnectionOptions{connectionAggregateStripes = 1}) close \second -> do
        createAggregate first "typed_wide_sum" sumAggregate
        forM_ [first, second] \conn -> do
          assertColumnsEqual conn "SELECT typed_wide_sum(i), sum(i)::BIGINT FROM range(10) t(i) GROUP BY i % 3"
          assertColumnsEqual conn "SELECT typed_wide_sum(i), sum(i)::BIGINT FROM range(10) t(i)"
  try (withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionAggregateStripes = 0} (const (pure ()))) >>= \case
    Left (err :: SQLError) -> do
      sqlErrorType err @?= Just ErrorInputInvalid
      assertBool ("unexpected message: " <> show err) ("connectionAggregateStripes" `Text.isInfixOf` sqlErrorMessage err)
    Right () -> assertFailure "a stripe count of 0 opened a connection"

-- | The callbacks of an aggregate do not keep the connection that registered
-- it. A connection with no close and no query becomes garbage, and its
-- finalizer closes it. The aggregate still runs on another connection of the
-- database.
testAggregateConnectionRelease :: Assertion
testAggregateConnectionRelease = withEnvironment \environment -> withInstance environment [] \instance' -> do
  weak <- do
    conn <- connect instance'
    createAggregate conn "typed_dropped_sum" sumAggregate
    mkWeakIORef (connectionState conn) (pure ())
  performMajorGC
  performMajorGC
  deRefWeak weak >>= assertBool "the aggregate keeps the connection that registered it" . isNothing
  withConnectionOn instance' \other ->
    query_ other "SELECT typed_dropped_sum(i) FROM range(10) t(i)" >>= (@?= [Only (45 :: Int64)])

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

-- | Count the words of each group in a 'Map'. The result lists each word and
-- its count, and an empty map gives NULL.
tallyAggregate :: Aggregate (Map.Map Text Int64) (Text, Int64) (Maybe Text)
tallyAggregate =
  Aggregate
    { aggregateInitial = Map.empty
    , aggregateStep = \tally (word, count) -> pure (Map.insertWith (+) word count tally)
    , aggregateCombine = \a b -> pure (Map.unionWith (+) a b)
    , aggregateResult = \tally ->
        pure
          if Map.null tally then Nothing else Just (Text.intercalate "," [word <> "=" <> Text.pack (show count) | (word, count) <- Map.toList tally])
    }

-- | Use a tuple of two argument types, a 'Map' state, and each result type.
testAggregateTypes :: Assertion
testAggregateTypes = withConnection ":memory:" \conn -> do
  createAggregate conn "typed_tally" tallyAggregate
  -- Group 2 has the 12 bytes that fit inline. Group 3 has 13 bytes. Group 4 has 14
  -- bytes in 8 characters. Group 5 has NULL in each row.
  _ <-
    execute_
      conn
      "CREATE TABLE typed_words AS SELECT * FROM (VALUES (1, 'a', 1::BIGINT), (1, 'a', 2), (1, NULL, 5), (1, 'b', NULL), (2, 'abcdefghij', 1), (3, 'abcdefghijk', 1), (4, '\240\240\240\240\240\240', 1), (5, NULL, 1), (5, 'c', NULL), (6, 'cherry', 2), (6, 'apple', 3), (6, 'banana', 5)) t(g, w, n)"
  let expected =
        [ Just "a=3"
        , Just "abcdefghij=1"
        , Just "abcdefghijk=1"
        , Just "\240\240\240\240\240\240=1"
        , Nothing
        , Just "apple=3,banana=5,cherry=2" :: Maybe Text
        ]
  query_ conn "SELECT typed_tally(w, n) FROM typed_words GROUP BY g ORDER BY g" >>= (@?= map Only expected)
  -- DuckDB compares the bytes of the results with the bytes of the literals.
  query_
    conn
    "SELECT typed_tally(w, n) = 'abcdefghij=1', typed_tally(w, n) = 'abcdefghijk=1' FROM typed_words WHERE g IN (2, 3) GROUP BY g ORDER BY g"
    >>= (@?= [(Just True, Just False), (Just False, Just True :: Maybe Bool)])
  query_ conn "SELECT typed_tally(w, n) FROM typed_words"
    >>= (@?= [Only (Just "a=3,abcdefghij=1,abcdefghijk=1,apple=3,banana=5,cherry=2,\240\240\240\240\240\240=1" :: Maybe Text)])
  query_ conn "SELECT typed_tally(w, n) FROM typed_words WHERE g = 5" >>= (@?= [Only (Nothing :: Maybe Text)])
  query_
    conn
    "SELECT typed_tally(w, n) OVER (ORDER BY g ROWS BETWEEN CURRENT ROW AND 1 FOLLOWING) FROM (VALUES (1, 'abcdefghij', 1::BIGINT), (2, 'abcdefghijk', 1), (3, NULL, 1)) t(g, w, n) ORDER BY g"
    >>= (@?= map Only [Just "abcdefghij=1,abcdefghijk=1", Just "abcdefghijk=1", Nothing :: Maybe Text])
  createAggregate
    conn
    "typed_any_odd"
    Aggregate
      { aggregateInitial = False
      , aggregateStep = \seen (Only (value :: Int64)) -> pure (seen || odd value)
      , aggregateCombine = \a b -> pure (a || b)
      , aggregateResult = pure
      }
  createAggregate
    conn
    "typed_mean"
    Aggregate
      { aggregateInitial = (0, 0)
      , aggregateStep = \(total, count) (Only value) -> pure (total + value, count + 1 :: Int64)
      , aggregateCombine = \(t1, c1) (t2, c2) -> pure (t1 + t2, c1 + c2)
      , aggregateResult = \(total, count) -> pure (if count == 0 then Nothing else Just (total / fromIntegral count :: Double))
      }
  createAggregate
    conn
    "typed_count"
    Aggregate
      { aggregateInitial = 0
      , aggregateStep = \count (Only (_ :: Int32)) -> pure (count + 1)
      , aggregateCombine = \a b -> pure (a + b)
      , aggregateResult = pure :: Word64 -> IO Word64
      }
  createAggregate
    conn
    "typed_small"
    Aggregate
      { aggregateInitial = 0
      , aggregateStep = \total (Only value) -> pure (total + value)
      , aggregateCombine = \a b -> pure (a + b)
      , aggregateResult = pure :: Int32 -> IO Int32
      }
  createAggregate
    conn
    "typed_label"
    Aggregate
      { aggregateInitial = 0 :: Int
      , aggregateStep = \count (Only (_ :: Text)) -> pure (count + 1)
      , aggregateCombine = \a b -> pure (a + b)
      , aggregateResult = \count -> pure (Text.replicate count "ab")
      }
  query_
    conn
    "SELECT typed_any_odd(i), typed_mean(i::DOUBLE), typed_count(i::INTEGER), typed_small(i::INTEGER), typed_label(i::VARCHAR) FROM range(8) t(i) GROUP BY i < 1 ORDER BY i < 1"
    >>= (@?= [(True, Just (4 :: Double), 7 :: Word64, 28 :: Int32, "ababababababab" :: Text), (False, Just 0, 1, 0, "ab")])
  query_ conn "SELECT typed_any_odd(i), typed_mean(i::DOUBLE), typed_count(i::INTEGER), typed_label(i::VARCHAR) FROM range(0) t(i)"
    >>= (@?= [(False, Nothing :: Maybe Double, 0 :: Word64, "" :: Text)])
  -- An Int32 argument and an Int32 result are INTEGER.
  query_
    conn
    "SELECT typeof(typed_any_odd(1)), typeof(typed_mean(1)), typeof(typed_count(1)), typeof(typed_small(1)), typeof(typed_label('x'))"
    >>= (@?= [("BOOLEAN" :: Text, "DOUBLE" :: Text, "UBIGINT" :: Text, "INTEGER" :: Text, "VARCHAR" :: Text)])
  query_
    conn
    "SELECT function_name, parameters::VARCHAR, parameter_types::VARCHAR FROM duckdb_functions() WHERE function_name IN ('typed_small', 'typed_tally') ORDER BY function_name"
    >>= (@?= [("typed_small" :: Text, "[arg0]" :: Text, "[INTEGER]" :: Text), ("typed_tally", "[arg0, arg1]", "[VARCHAR, BIGINT]")])

-- | Fail the query for an exception in each function. The next query works.
testAggregateErrors :: Assertion
testAggregateErrors = withConnection ":memory:" \conn -> do
  createAggregate
    conn
    "typed_bad_step"
    sumAggregate{aggregateStep = \total (Only value) -> if value == 13 then throwIO (userError "typed_bad_step: 13") else pure (total + value)}
  assertSqlError "typed_bad_step: 13" (query_ conn "SELECT typed_bad_step(i) FROM range(20) t(i) GROUP BY i % 3" :: IO [Only Int64])
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
  -- The step evaluates its state, so the error belongs to the step.
  createAggregate
    conn
    "typed_lazy_step"
    sumAggregate{aggregateStep = \total (Only value) -> pure (if value == 7 then error "typed_lazy_step: 7" else total + value)}
  assertSqlError "typed_lazy_step: 7" (query_ conn "SELECT typed_lazy_step(i) FROM range(20) t(i)" :: IO [Only Int64])
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
  -- DuckDB adds rows 0 to 15 and rows 16 to 31 of the frame of row 0 in separate
  -- states. Then it combines them.
  createAggregate conn "typed_bad_combine" sumAggregate{aggregateCombine = \_ _ -> throwIO (userError "typed_bad_combine: combine")}
  assertSqlError
    "typed_bad_combine: combine"
    ( query_ conn "SELECT typed_bad_combine(i) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) FROM range(32) t(i)"
        :: IO [Only Int64]
    )
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
  -- A Natural argument is UHUGEINT. DuckDB casts a BIGINT to it only with an
  -- explicit cast, and the cast of a negative value fails.
  createAggregate
    conn
    "typed_natural_sum"
    Aggregate
      { aggregateInitial = 0
      , aggregateStep = \total (value :: Int64, extra :: Natural) -> pure (total + value + fromIntegral extra)
      , aggregateCombine = \a b -> pure (a + b)
      , aggregateResult = pure :: Int64 -> IO Int64
      }
  query_ conn "SELECT typed_natural_sum(i, i::UHUGEINT) FROM range(10) t(i)" >>= (@?= [Only (90 :: Int64)])
  assertSqlError "No function matches" (query_ conn "SELECT typed_natural_sum(i, i) FROM range(10) t(i)" :: IO [Only Int64])
  assertSqlError
    "out of range for the destination type UINT128"
    (query_ conn "SELECT typed_natural_sum(i, (i - 5)::UHUGEINT) FROM range(10) t(i)" :: IO [Only Int64])
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
  createAggregate
    conn
    "typed_bad_result"
    sumAggregate{aggregateResult = \total -> if total > 100 then throwIO (userError "typed_bad_result: too large") else pure total}
  query_ conn "SELECT typed_bad_result(i) FROM range(10) t(i)" >>= (@?= [Only (45 :: Int64)])
  assertSqlError
    "typed_bad_result: too large"
    (query_ conn "SELECT typed_bad_result(i) FROM range(100) t(i) GROUP BY i % 2" :: IO [Only Int64])
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
  createAggregate
    conn
    "typed_lazy_result"
    sumAggregate{aggregateResult = \total -> pure (if total == 0 then error "typed_lazy_result: 0" else total)}
  assertSqlError "typed_lazy_result: 0" (query_ conn "SELECT typed_lazy_result(i) FROM range(1) t(i)" :: IO [Only Int64])
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Declare the true type of a SMALLINT and a FLOAT result. Write them also in
-- an ordered window, where the result vector of a finalize call has the size 0.
-- Reject a result type that is not raw at registration.
testAggregateResultTypes :: Assertion
testAggregateResultTypes = withConnection ":memory:" \conn -> do
  createAggregate
    conn
    "typed_small_count"
    Aggregate
      { aggregateInitial = 0 :: Int16
      , aggregateStep = \count (Only (_ :: Int64)) -> pure (count + 1)
      , aggregateCombine = \a b -> pure (a + b)
      , aggregateResult = pure
      }
  createAggregate
    conn
    "typed_float_mean"
    Aggregate
      { aggregateInitial = (0, 0 :: Int)
      , aggregateStep = \(total, count) (Only value) -> pure (total + value, count + 1)
      , aggregateCombine = \(t1, c1) (t2, c2) -> pure (t1 + t2, c1 + c2)
      , aggregateResult = \(total, count) -> pure (if count == 0 then Nothing else Just (realToFrac (total / fromIntegral count :: Double) :: Float))
      }
  query_ conn "SELECT typeof(typed_small_count(1)), typeof(typed_float_mean(1.0::DOUBLE))" >>= (@?= [("SMALLINT" :: Text, "FLOAT" :: Text)])
  query_ conn "SELECT min(c), max(c), count(*) FROM (SELECT typed_small_count(i) AS c FROM range(100000) t(i) GROUP BY i % 5000)"
    >>= (@?= [(20 :: Int16, 20 :: Int16, 5000 :: Int64)])
  query_
    conn
    "SELECT min(c), max(c), count(*) FROM (SELECT typed_small_count(i) OVER (ORDER BY i ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS c FROM range(5000) t(i))"
    >>= (@?= [(1 :: Int16, 3 :: Int16, 5000 :: Int64)])
  query_
    conn
    "SELECT bool_and(m = CASE WHEN i = 0 THEN 0 ELSE i - 0.5 END), typeof(any_value(m)) FROM (SELECT i, typed_float_mean(i::DOUBLE) OVER (ORDER BY i ROWS BETWEEN 1 PRECEDING AND CURRENT ROW) AS m FROM range(5000) t(i))"
    >>= (@?= [(True, "FLOAT" :: Text)])
  -- A newtype over a raw type with a derived FunctionResult has the raw writer.
  createAggregate
    conn
    "typed_oldest"
    Aggregate
      { aggregateInitial = Age 0
      , aggregateStep = \oldest (Only value) -> pure (max oldest (Age value))
      , aggregateCombine = \a b -> pure (max a b)
      , aggregateResult = pure
      }
  query_ conn "SELECT typeof(typed_oldest(1::SMALLINT)), typed_oldest(i::SMALLINT) FROM range(10) t(i)"
    >>= (@?= [("SMALLINT" :: Text, Age 9)])
  createFunction conn "typed_age" (Age 5)
  query_ conn "SELECT typeof(typed_age()), typed_age()" >>= (@?= [("SMALLINT" :: Text, Age 5)])
  assertSqlError
    "typed_years: result: the result type of an aggregate function must be BOOLEAN"
    (createAggregate conn "typed_years" sumAggregate{aggregateResult = pure . Years . fromIntegral})
  assertSqlError
    "For a newtype over one of these types, derive FunctionResult with deriving newtype."
    (createAggregate conn "typed_years" sumAggregate{aggregateResult = pure . Years . fromIntegral})
  -- The argument reader does not read VARIANT.
  let unreadable =
        "argument arg0: the library cannot read a VARIANT value in a callback; declare another type, such as VARCHAR, and cast the value in SQL, for example v::VARCHAR"
  assertSqlError
    ("typed_variant_count: " <> unreadable)
    (createAggregate conn "typed_variant_count" countAggregate{aggregateStep = \count (Only (_ :: Variant)) -> pure (count + 1)})
  assertSqlError
    ("typed_variants_count: " <> unreadable)
    (createAggregate conn "typed_variants_count" countAggregate{aggregateStep = \count (Only (_ :: [Maybe Variant])) -> pure (count + 1)})
  query_
    conn
    "SELECT count(*) FROM duckdb_functions() WHERE function_name IN ('typed_years', 'typed_variant_count', 'typed_variants_count')"
    >>= (@?= [Only (0 :: Int64)])
  let limit = "result: the result type of an aggregate function must be BOOLEAN, TINYINT to BIGINT, UTINYINT to UBIGINT, FLOAT, DOUBLE, or VARCHAR"
  assertSqlError
    ("typed_peak_result: " <> limit)
    (createAggregate conn "typed_peak_result" sumAggregate{aggregateResult = \total -> pure (Peak "total" (fromIntegral total) "")})
  assertSqlError
    ("typed_integer_result: " <> limit)
    (createAggregate conn "typed_integer_result" sumAggregate{aggregateResult = pure . toInteger})
  query_ conn "SELECT count(*) FROM duckdb_functions() WHERE function_name IN ('typed_peak_result', 'typed_integer_result')"
    >>= (@?= [Only (0 :: Int64)])
  assertSqlError
    "typed_struct_result: result: StructValue has no static field types"
    (createAggregate conn "typed_struct_result" sumAggregate{aggregateResult = \_ -> pure (undefined :: StructValue FieldValue)})
  assertInputErrorContaining "aggregate function name must be nonempty" (createAggregate conn "" sumAggregate)
  -- The result check comes before the state of the connection.
  closed <- open ":memory:"
  close closed
  assertSqlError
    ("typed_closed_result: " <> limit)
    (createAggregate closed "typed_closed_result" sumAggregate{aggregateResult = pure . toInteger})

-- | An aggregate that counts its rows. Change 'aggregateStep' for another
-- argument type.
countAggregate :: Aggregate Int64 (Only Int64) Int64
countAggregate =
  Aggregate
    { aggregateInitial = 0
    , aggregateStep = \count _ -> pure (count + 1)
    , aggregateCombine = \a b -> pure (a + b)
    , aggregateResult = pure
    }

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
      assertColumnsEqual
        conn
        ( Query
            ( "SELECT typed_sum(v) OVER ("
                <> window
                <> "), coalesce(sum(v) OVER ("
                <> window
                <> "), 0)::BIGINT FROM (SELECT i, CASE WHEN i % 7 = 0 THEN NULL ELSE i END AS v FROM range(100) t(i))"
            )
        )
  assertColumnsEqual conn "SELECT typed_sum(DISTINCT i % 5), sum(DISTINCT i % 5)::BIGINT FROM range(100) t(i)"
  assertColumnsEqual conn "SELECT typed_sum(DISTINCT i % 5), sum(DISTINCT i % 5)::BIGINT FROM range(100) t(i) GROUP BY i % 2"
  assertColumnsEqual conn "SELECT typed_sum(i) FILTER (WHERE i % 2 = 0), sum(i) FILTER (WHERE i % 2 = 0)::BIGINT FROM range(10) t(i)"
  createAggregate
    conn
    "typed_words"
    Aggregate
      { aggregateInitial = []
      , aggregateStep = \seen (Only word) -> pure (word : seen)
      , aggregateCombine = \a b -> pure (b <> a)
      , aggregateResult = pure . Text.intercalate "," . reverse
      }
  query_ conn "SELECT typed_words(w ORDER BY k DESC) FROM (VALUES (1, 'a'), (2, 'b'), (3, 'c')) t(k, w)" >>= (@?= [Only ("c,b,a" :: Text)])

-- | Release each Haskell state after a query, also after a failed query and
-- after a query that stops early. The step makes a new 'IORef' for each state and
-- keeps a weak pointer to it.
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
          (_, Left (err :: SQLError)) ->
            assertBool (Text.unpack (sqlErrorMessage err)) (Text.pack ("typed_release: " <> phase) `Text.isInfixOf` sqlErrorMessage err)
          (_, Right _) -> assertFailure ("expected an error in " <> Text.unpack (fromQuery sql))
  createAggregate
    conn
    "typed_release"
    Aggregate
      { aggregateInitial = Nothing
      , aggregateStep = step
      , aggregateCombine = combine
      , aggregateResult = result :: Maybe (IORef Int64) -> IO Int64
      }
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

-- | Require an 'SQLError' of the type 'ErrorInputInvalid' with a message that
-- contains the text: the error of an input check of the library.
assertInputError :: Text -> IO a -> Assertion
assertInputError expected action =
  try action >>= \case
    Left (err :: SQLError) -> do
      sqlErrorType err @?= Just ErrorInputInvalid
      assertBool (Text.unpack (sqlErrorMessage err)) (expected `Text.isInfixOf` sqlErrorMessage err)
    Right _ -> assertFailure ("expected an SQL error with " <> Text.unpack expected)

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
  query_ conn "SELECT parameters::VARCHAR FROM duckdb_functions() WHERE function_name = 'typed_labels'"
    >>= (@?= [Only ("[arg0, arg1]" :: Text)])
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
    pure
      [ Reading ("s" <> Text.pack (show n)) (fromIntegral n / 2) (addDays (fromIntegral n) start) (if even n then Nothing else Just "odd")
      | n <- [0 .. count - 1 :: Int64]
      ]
  query_ conn "SELECT * FROM typed_readings(3)"
    >>= ( @?=
            [("s0" :: Text, 0 :: Double, start, Nothing :: Maybe Text), ("s1", 0.5, addDays 1 start, Just "odd"), ("s2", 1, addDays 2 start, Nothing)]
        )
  query_ conn "SELECT count(*) FROM typed_readings(3000) WHERE note IS NULL" >>= (@?= [Only (1500 :: Int64)])

-- | Give Integer and Natural columns. The declared column type is HUGEINT for
-- Integer and UHUGEINT for Natural, also for the elements of a list and the
-- values of a MAP. A value outside that range raises an error.
testTableIntegers :: Assertion
testTableIntegers = withConnection ":memory:" \conn -> do
  let integers = [5, -5, 2 ^ (100 :: Int), 2 ^ (127 :: Int) - 1 :: Integer]
  createTableFunction conn "typed_integers" ["v"] \() -> pure (map Only integers)
  query_ conn "SELECT typeof(v), v FROM typed_integers()" >>= (@?= [("HUGEINT" :: Text, value) | value <- integers])
  let naturals = [("small" :: Text, 7 :: Natural), ("large", 2 ^ (128 :: Int) - 1)]
  createTableFunction conn "typed_naturals" ["label", "v"] \() -> pure naturals
  query_ conn "SELECT label, typeof(v), v FROM typed_naturals()"
    >>= (@?= [(label, "UHUGEINT" :: Text, value) | (label, value) <- naturals])
  let nested = [([5, 2 ^ (100 :: Int)], Map.fromList [("a", 2 ^ (127 :: Int) :: Natural)])] :: [([Integer], Map.Map Text Natural)]
  createTableFunction conn "typed_integer_lists" ["vs", "m"] \() -> pure nested
  query_ conn "SELECT typeof(vs), typeof(m), vs, m FROM typed_integer_lists()"
    >>= (@?= [("HUGEINT[]" :: Text, "MAP(VARCHAR, UHUGEINT)" :: Text, vs, m) | (vs, m) <- nested])
  -- A MAP of a type text and an Integer or a Natural, also in a list, declares
  -- the text MAP(K, V) with the type text of the other part.
  let keyed = Map.fromList [([1, 2], 2 ^ (100 :: Int))] :: Map.Map [Int64] Integer
      listed = Map.fromList [([3], [5, -5])] :: Map.Map [Int64] [Integer]
      nestedMaps = [[Map.fromList [([4], 2 ^ (127 :: Int))]], []] :: [[Map.Map [Int64] Natural]]
  createTableFunction conn "typed_text_maps" ["k", "l", "n", "p"] \() ->
    pure [(keyed, listed, maps, Map.fromList [(7 :: Natural, Price 1.5)]) | maps <- nestedMaps]
  query_ conn "SELECT DISTINCT typeof(k), typeof(l), typeof(n), typeof(p) FROM typed_text_maps()"
    >>= ( @?=
            [
              ( "MAP(BIGINT[], HUGEINT)" :: Text
              , "MAP(BIGINT[], HUGEINT[])" :: Text
              , "MAP(BIGINT[], UHUGEINT)[]" :: Text
              , "MAP(UHUGEINT, DECIMAL(18,3))" :: Text
              )
            ]
        )
  query_ conn "SELECT k, l, n, p::VARCHAR FROM typed_text_maps()"
    >>= (@?= [(keyed, listed, maps, "{7=1.500}" :: Text) | maps <- nestedMaps])
  -- DuckDB sums a UHUGEINT column as a DOUBLE.
  query_ conn "SELECT typeof(sum(v)), sum(v) FROM typed_naturals()" >>= (@?= [("DOUBLE" :: Text, 2 ^ (128 :: Int) :: Double)])
  -- toDuckValue raises an error for a value outside the range. A BIGNUM value
  -- would reach the HUGEINT column, and DuckDB casts the BIGNUM 2^128 to 0.
  let outside typeName = "Conversion Error: duckdb-simple: the value 340282366920938463463374607431768211456 is outside the range of " <> typeName
  createTableFunction conn "typed_wide_integer" ["v"] \() -> pure [Only (2 ^ (128 :: Int) :: Integer)]
  assertErrorType
    "Integer"
    ErrorTypeConversion
    (outside "HUGEINT")
    (void (query_ conn "SELECT v FROM typed_wide_integer()" :: IO [Only Integer]))
  createTableFunction conn "typed_wide_list" ["v"] \() -> pure [Only [5, 2 ^ (128 :: Int) :: Integer]]
  assertErrorType
    "Integer list"
    ErrorTypeConversion
    (outside "HUGEINT")
    (void (query_ conn "SELECT v FROM typed_wide_list()" :: IO [Only [Integer]]))
  createTableFunction conn "typed_wide_natural" ["v"] \() -> pure [Only (2 ^ (128 :: Int) :: Natural)]
  assertErrorType
    "Natural"
    ErrorTypeConversion
    (outside "UHUGEINT")
    (void (query_ conn "SELECT v FROM typed_wide_natural()" :: IO [Only Natural]))
  -- A BigNum column holds each value.
  createTableFunction conn "typed_bignums" ["v"] \() -> pure [Only (BigNum (2 ^ (130 :: Int)))]
  query_ conn "SELECT typeof(v), v FROM typed_bignums()" >>= (@?= [("BIGNUM" :: Text, BigNum (2 ^ (130 :: Int)))])

-- | Give IntervalValue and TimeWithZone columns.
testTableIntervals :: Assertion
testTableIntervals = withConnection ":memory:" \conn -> do
  let rows =
        [ (IntervalValue 1 2 3, TimeWithZone (TimeOfDay 12 34 0) 19800)
        , (IntervalValue (-1) 0 0, TimeWithZone (TimeOfDay 0 0 0) 3630)
        ]
  createTableFunction conn "typed_intervals" ["duration", "moment"] \() -> pure rows
  query_ conn "SELECT typeof(duration), typeof(moment) FROM typed_intervals() LIMIT 1"
    >>= (@?= [("INTERVAL" :: Text, "TIME WITH TIME ZONE" :: Text)])
  query_ conn "SELECT duration, moment FROM typed_intervals()" >>= (@?= rows)

-- | Give a MAP column. The pairs go in ascending key order.
testTableMaps :: Assertion
testTableMaps = withConnection ":memory:" \conn -> do
  let rows = map Only [Map.fromList [("b", 2), ("a", 1)], Map.empty :: Map.Map Text Int64]
  createTableFunction conn "typed_maps" ["pairs"] \() -> pure rows
  query_ conn "SELECT typeof(pairs), map_keys(pairs) FROM typed_maps()"
    >>= (@?= [("MAP(VARCHAR, BIGINT)" :: Text, ["a", "b"] :: [Text]), ("MAP(VARCHAR, BIGINT)", [])])
  query_ conn "SELECT pairs FROM typed_maps()" >>= (@?= rows)

-- | A record for a STRUCT column. A String field is VARCHAR.
data Peak = Peak {peak :: Text, metres :: Int32, remark :: String}
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToDuckValue, ToField, FromField) via (ViaDuckDB Peak)

-- | A text of the type JSON. A record field of this type has the type JSON.
newtype JsonText = JsonText Text
  deriving newtype (Eq, Show, FromField, ToField)

instance DuckValue JsonText where
  duckToField (JsonText text) = duckToField text
  duckFromField = fmap JsonText . duckFromField
  duckLogicalType _ = LogicalTypeAlias "JSON" (LogicalTypeScalar LogicalTypeIdVarchar)

-- | A record with a field of the type JSON.
data Doc = Doc {serial :: Int64, body :: JsonText}
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToDuckValue, ToField, FromField) via (ViaDuckDB Doc)

-- | A sum type for a UNION column.
data Weather = Sunny | Rain Double
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToDuckValue, FromField) via (ViaDuckDB Weather)

-- | A record with an Integer field, of the type HUGEINT, and a Natural field,
-- of the type UHUGEINT.
data Counts = Counts {signed :: Integer, unsigned :: Natural}
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToDuckValue, ToField, FromField) via (ViaDuckDB Counts)

-- | A list of three BIGINT values. Its type text gives the size.
newtype Triple = Triple [Int64]
  deriving newtype (Eq, Show, FromField, ToField, ToDuckValue)

instance DuckDBColumnType Triple where
  duckdbColumnTypeFor _ = "BIGINT[3]"

-- | A record with a field of a sized ARRAY type.
newtype Boxed = Boxed {boxed :: SizedArray 3 Int64}
  deriving stock (Generic)
  deriving (DuckDBColumnType, ToDuckValue, FromField) via (ViaDuckDB Boxed)

-- | A newtype over a raw result type. It derives the raw result writer.
newtype Age = Age Int16
  deriving newtype (Eq, Ord, Show, DuckDBColumnType, ToDuckValue, FromField, FunctionResult)

-- | A newtype over a raw result type, with the catch-all 'FunctionResult'.
newtype Years = Years Int16
  deriving newtype (DuckDBColumnType, ToDuckValue)

-- | A newtype with its own type text.
newtype Price = Price Double

instance DuckDBColumnType Price where
  duckdbColumnTypeFor _ = "DECIMAL(18,3)"

instance ToDuckValue Price where
  toDuckValue factory (Price value) = toDuckValue factory value

-- | Declare record, list, MAP, and UNION columns and a record aggregate
-- argument from the generic types. An empty list and an empty MAP of records
-- take the element type from the declaration.
testTableRecords :: Assertion
testTableRecords = withConnection ":memory:" \conn -> do
  let peaks = [Peak "Hekla" 1491 "volcano", Peak "Esja" 914 ""]
      rows =
        [(row, peaks, Map.fromList [(peak p, p) | p <- peaks], weather) | (row, weather) <- zip peaks [Sunny, Rain 2.5]]
          <> [(Peak "Snæfell" 1833 "", [], Map.empty, Rain 0)]
  createTableFunction conn "typed_peaks" ["peak", "nearby", "by_name", "weather"] \() -> pure rows
  query_ conn "SELECT typeof(peak), typeof(nearby), typeof(by_name), typeof(weather) FROM typed_peaks() LIMIT 1"
    >>= ( @?=
            [
              ( "STRUCT(peak VARCHAR, metres INTEGER, remark VARCHAR)" :: Text
              , "STRUCT(peak VARCHAR, metres INTEGER, remark VARCHAR)[]" :: Text
              , "MAP(VARCHAR, STRUCT(peak VARCHAR, metres INTEGER, remark VARCHAR))" :: Text
              , "UNION(Sunny STRUCT, Rain STRUCT(field1 DOUBLE))" :: Text
              )
            ]
        )
  query_ conn "SELECT peak, nearby, by_name, weather FROM typed_peaks()" >>= (@?= rows)
  createAggregate
    conn
    "typed_highest"
    Aggregate
      { aggregateInitial = 0
      , aggregateStep = \highest (Only p) -> pure (max highest (metres p))
      , aggregateCombine = \a b -> pure (max a b)
      , aggregateResult = pure :: Int32 -> IO Int32
      }
  query_ conn "SELECT typed_highest(peak) FROM typed_peaks()" >>= (@?= [Only (1833 :: Int32)])
  query_ conn "SELECT parameter_types::VARCHAR FROM duckdb_functions() WHERE function_name = 'typed_highest'"
    >>= (@?= [Only ("['STRUCT(peak VARCHAR, metres INTEGER, remark VARCHAR)']" :: Text)])
  query conn "SELECT ?, typeof(?)" ([] :: [Peak], [] :: [Peak])
    >>= (@?= [([] :: [Peak], "STRUCT(peak VARCHAR, metres INTEGER, remark VARCHAR)[]" :: Text)])
  -- With no target type, a list of NULL records also takes the declaration.
  -- With a target type, the empty list and the list of NULL records take the
  -- target type, as for each element type.
  query conn "SELECT typeof(?)" (Only [Nothing :: Maybe Peak])
    >>= (@?= [Only ("STRUCT(peak VARCHAR, metres INTEGER, remark VARCHAR)[]" :: Text)])
  _ <- execute_ conn "CREATE TABLE typed_short_peaks (ps STRUCT(peak VARCHAR, metres INTEGER)[])"
  _ <- execute conn "INSERT INTO typed_short_peaks VALUES (?), (?)" ([] :: [Peak], [Nothing :: Maybe Peak])
  query_ conn "SELECT len(ps) FROM typed_short_peaks ORDER BY len(ps)" >>= (@?= [Only (0 :: Int64), Only 1])

-- | The type of a DECIMAL(4,1) argument, as its text.
newtype ArgumentType = ArgumentType Text

instance DuckDBColumnType ArgumentType where
  duckdbColumnTypeFor _ = "DECIMAL(4,1)"

instance FromField ArgumentType where
  fromField Field{fieldType} = ArgumentType . Text.pack . show <$> fieldType

-- | The argument of a scalar function gives the type of its vector.
testArgumentType :: Assertion
testArgumentType = withConnection ":memory:" \conn -> do
  createFunction conn "argument_type" \(ArgumentType text) -> text
  query_ conn "SELECT argument_type(1.5) FROM range(3000) t(i) LIMIT 1" >>= (@?= [Only ("LogicalTypeDecimal 4 1" :: Text)])

-- | A record field of an alias type declares the alias in the STRUCT type of a
-- table function column. A record parameter, a list of records, and a list of
-- NULL records have the same type. A value of the field reads as its base type.
testAliasDeclaration :: Assertion
testAliasDeclaration = withConnection ":memory:" \conn -> do
  let docs = [Doc 1 (JsonText "{\"a\": 1}"), Doc 2 (JsonText "[2]")]
  createTableFunction conn "typed_docs" ["doc"] \() -> pure (map Only docs)
  query_ conn "SELECT typeof(doc), typeof(doc.body) FROM typed_docs() LIMIT 1"
    >>= (@?= [("STRUCT(serial BIGINT, body JSON)" :: Text, "JSON" :: Text)])
  query_ conn "SELECT doc FROM typed_docs()" >>= (@?= map Only docs)
  query conn "SELECT typeof(?), typeof(?), typeof(?)" (Doc 3 (JsonText "{}"), docs, [Nothing :: Maybe Doc])
    >>= ( @?=
            [("STRUCT(serial BIGINT, body JSON)" :: Text, "STRUCT(serial BIGINT, body JSON)[]" :: Text, "STRUCT(serial BIGINT, body JSON)[]" :: Text)]
        )

-- | Reject a column, a parameter, or an argument type with no declaration at
-- registration, also inside a list, and a MAP of a type text and a record.
testNoDeclaration :: Assertion
testNoDeclaration = withConnection ":memory:" \conn -> do
  assertSqlError
    "typed_struct: column s: StructValue has no static field types"
    (createTableFunction conn "typed_struct" ["s"] \() -> pure ([] :: [Only (StructValue FieldValue)]))
  assertSqlError
    "typed_tuple: column t: TupleValue has no static field types"
    (createTableFunction conn "typed_tuple" ["t"] \() -> pure ([] :: [Only (TupleValue FieldValue)]))
  assertSqlError
    "typed_union: column u: UnionValue has no static field types"
    (createTableFunction conn "typed_union" ["u"] \() -> pure ([] :: [Only (UnionValue FieldValue)]))
  assertSqlError
    "typed_structs: column s: StructValue has no static field types"
    (createTableFunction conn "typed_structs" ["n", "s"] \() -> pure ([] :: [(Int64, [Maybe (StructValue FieldValue)])]))
  assertSqlError
    "typed_struct_parameter: parameter arg0: StructValue has no static field types"
    (createTableFunction conn "typed_struct_parameter" ["n"] \(Only (_ :: StructValue FieldValue)) -> pure [Only (1 :: Int64)])
  assertSqlError
    "typed_struct_count: argument arg0: StructValue has no static field types"
    ( createAggregate
        conn
        "typed_struct_count"
        Aggregate
          { aggregateInitial = 0 :: Int64
          , aggregateStep = \count (Only (_ :: StructValue FieldValue)) -> pure (count + 1)
          , aggregateCombine = \a b -> pure (a + b)
          , aggregateResult = pure
          }
    )
  assertSqlError
    "typed_priced: column p: a MAP with the type text DECIMAL(18,3) and a structured type has no declaration"
    (createTableFunction conn "typed_priced" ["p"] \() -> pure ([] :: [Only (Map.Map Price Peak)]))
  query_ conn "SELECT count(*) FROM duckdb_functions() WHERE function_name LIKE 'typed\\_%' ESCAPE '\\'" >>= (@?= [Only (0 :: Int64)])

-- | Use a SizedArray of size 3 in each position, and a newtype with a sized ARRAY type
-- text as an argument. DuckDB rejects an ARRAY of another size when it binds
-- the call, casts a LIST of the same length to the ARRAY, and rejects a LIST
-- of another length when the statement runs. See Note [Sized arrays] in
-- "Database.DuckDB.Simple.ToField".
testSizedArray :: Assertion
testSizedArray = withConnection ":memory:" \conn -> do
  let total = sum :: SizedArray 3 Int64 -> Int64
      triple x = fromJust (sizedArrayFromList @3 [x, x, x :: Int64])
  createFunction conn "sized_argument" total
  createFunction conn "sized_newtype" \(Triple values) -> sum values
  createFunction conn "sized_maybe" (maybe (-1) total :: Maybe (SizedArray 3 Int64) -> Int64)
  createFunction conn "sized_list" (sum . map total :: [SizedArray 3 Int64] -> Int64)
  createFunction conn "sized_record" \(Boxed values) -> total values
  createAggregate
    conn
    "sized_aggregate"
    Aggregate
      { aggregateInitial = 0 :: Int64
      , aggregateStep = \sum' (Only values) -> pure (sum' + total values)
      , aggregateCombine = \a b -> pure (a + b)
      , aggregateResult = pure
      }
  createTableFunction conn "sized_table" ["total"] \(Only values) -> pure [Only (total values)]
  -- The third part is the type of the error for a LIST of another length.
  -- DuckDB casts the parameter of a table function as a constant value, and
  -- that cast raises ErrorInputInvalid "Failed to cast value".
  let positions :: [(String, Text -> Text, ErrorType)]
      positions =
        [ ("scalar argument", \v -> "SELECT sized_argument(" <> v <> ")", ErrorTypeConversion)
        , ("newtype argument", \v -> "SELECT sized_newtype(" <> v <> ")", ErrorTypeConversion)
        , ("Maybe argument", \v -> "SELECT sized_maybe(" <> v <> ")", ErrorTypeConversion)
        , ("list argument", \v -> "SELECT sized_list([" <> v <> "])", ErrorTypeConversion)
        , ("record field", \v -> "SELECT sized_record({'boxed': " <> v <> "})", ErrorTypeConversion)
        , ("aggregate argument", \v -> "SELECT sized_aggregate(" <> v <> ")", ErrorTypeConversion)
        , ("table function parameter", \v -> "SELECT * FROM sized_table(" <> v <> ")", ErrorInputInvalid)
        ]
      values :: ErrorType -> [(String, Text, Either (ErrorType, Text) Int64)]
      values lengthError =
        [ ("ARRAY of 3", "[1, 2, 3]::BIGINT[3]", Right 6)
        , ("ARRAY of 4", "[1, 2, 3, 4]::BIGINT[4]", Left (ErrorQueryBinder, "No function matches"))
        , ("LIST of 3", "[1, 2, 3]", Right 6)
        , ("LIST of 2", "[1, 2]", Left (lengthError, "Cannot cast list with length 2 to array with length 3"))
        ]
      check :: String -> IO [Only Int64] -> Either (ErrorType, Text) Int64 -> Assertion
      check name action expected = do
        outcome <- try action
        case (outcome, expected) of
          (Right rows, Right value) -> assertEqual name [Only value] rows
          (Left err, Left (errorType, text)) -> do
            assertEqual (name <> ": " <> Text.unpack (sqlErrorMessage err)) (Just errorType) (sqlErrorType err)
            assertBool (name <> ": " <> Text.unpack (sqlErrorMessage err)) (text `Text.isInfixOf` sqlErrorMessage err)
          (Right rows, Left _) -> assertFailure (name <> ": expected an error, got " <> show rows)
          (Left (err :: SQLError), Right _) -> assertFailure (name <> ": " <> show err)
  forM_ positions \(position, sql, lengthError) -> forM_ (values lengthError) \(label, value, expected) ->
    check (position <> ", " <> label) (query_ conn (Query (sql value))) expected
  -- A bound SizedArray binds as an ARRAY of its size, and a FieldArray as an
  -- ARRAY of its own size. DuckDB casts a bound list of the same length.
  let fieldArray items = FieldArray (listArray (0, length items - 1) (map FieldInt64 items))
  check "bound SizedArray of 3" (query conn "SELECT sized_argument(?)" (Only (triple 1))) (Right 3)
  check "bound FieldArray of 3" (query conn "SELECT sized_argument(?)" (Only (fieldArray [1, 2, 3]))) (Right 6)
  check
    "bound FieldArray of 4"
    (query conn "SELECT sized_argument(?)" (Only (fieldArray [1, 2, 3, 4])))
    (Left (ErrorQueryBinder, "No function matches"))
  check "bound list of 3" (query conn "SELECT sized_argument(?)" (Only [1, 2, 3 :: Int64])) (Right 6)
  -- A result and a column of a table function are ARRAY values of the size.
  createFunction conn "sized_result" triple
  createTableFunction conn "sized_rows" ["t"] \(Only (x :: Int64)) -> pure [Only (triple x)]
  query_ conn "SELECT typeof(sized_result(1)), sized_result(1)::VARCHAR" >>= (@?= [("BIGINT[3]" :: Text, "[1, 1, 1]" :: Text)])
  query_ conn "SELECT typeof(t), t::VARCHAR FROM sized_rows(2)" >>= (@?= [("BIGINT[3]" :: Text, "[2, 2, 2]" :: Text)])
  -- A result of another size raises the cast error of DuckDB when the function
  -- runs. A SizedArray has no value of another size, but a hand-written newtype has.
  createFunction conn "sized_wrong_result" \(x :: Int64) -> Triple [x, x, x, x]
  check
    "result of 4"
    (query_ conn "SELECT len(sized_wrong_result(1))")
    (Left (ErrorInputInvalid, "Cannot cast list with length 4 to array with length 3"))

-- | Give an error, and no other value, for a record field outside the range
-- of its declared type, as a parameter, as a column, and as a scalar result.
testRecordFieldRange :: Assertion
testRecordFieldRange = withConnection ":memory:" \conn -> do
  let largest = Counts (2 ^ (127 :: Int) - 1) (2 ^ (128 :: Int) - 1)
      outside = [(Counts (2 ^ (127 :: Int)) 0, "HUGEINT value out of range"), (Counts 0 (2 ^ (128 :: Int)), "UHUGEINT value out of range")]
      cases = largest : map fst outside
  query conn "SELECT ?" (Only largest) >>= (@?= [Only largest])
  createFunction conn "typed_counts_result" (\(index :: Int) -> cases !! index)
  query_ conn "SELECT typeof(typed_counts_result(0)), typed_counts_result(0)"
    >>= (@?= [("STRUCT(signed HUGEINT, unsigned UHUGEINT)" :: Text, largest)])
  createTableFunction conn "typed_counts" ["c"] \(Only (index :: Int)) -> pure [Only (cases !! index)]
  query_ conn "SELECT typeof(c), c FROM typed_counts(0)" >>= (@?= [("STRUCT(signed HUGEINT, unsigned UHUGEINT)" :: Text, largest)])
  forM_ (zip [1 :: Int ..] outside) \(index, (counts, message)) -> do
    outcome <- try (query conn "SELECT ?" (Only counts) :: IO [Only Counts])
    case outcome of
      Left (err :: SomeException) -> assertBool (displayException err) (message `Text.isInfixOf` Text.pack (displayException err))
      Right rows -> assertFailure ("expected an error, got " <> show rows)
    assertSqlError message (query_ conn (Query ("SELECT c FROM typed_counts(" <> Text.pack (show index) <> ")")) :: IO [Only Counts])
    assertSqlError message (query_ conn (Query ("SELECT typed_counts_result(" <> Text.pack (show index) <> ")")) :: IO [Only Counts])

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
  assertSqlError
    "1 column names for 2 result columns"
    (createTableFunction conn "typed_bad_names" ["a"] \() -> pure [(1 :: Int64, 2 :: Int64)])
  assertSqlError "at least one result column" (createTableFunction conn "typed_no_columns" [] \() -> pure [()])
  -- A Natural parameter is UHUGEINT. DuckDB does not cast a negative literal to it.
  createTableFunction conn "typed_natural" ["n"] \(Only (value :: Natural)) -> pure [Only (fromIntegral value :: Int64)]
  query_ conn "SELECT n FROM typed_natural(7)" >>= (@?= [Only (7 :: Int64)])
  assertSqlError "No function matches" (query_ conn "SELECT n FROM typed_natural(-7)" :: IO [Only Int64])
  assertSqlError "typed_natural: argument arg0 is NULL" (query_ conn "SELECT n FROM typed_natural(NULL)" :: IO [Only Int64])
  -- The argument reader reads no VARIANT, so the registration of such a
  -- parameter type fails.
  assertInputError
    "typed_variant: parameter arg0: the library cannot read a VARIANT value in a callback; declare another type, such as VARCHAR, and cast the value in SQL, for example v::VARCHAR"
    (createTableFunction conn "typed_variant" ["n"] \(Only (_ :: Variant)) -> pure [Only (1 :: Int64)])
  assertInputError
    "typed_maybe_variant: parameter arg0: the library cannot read a VARIANT value in a callback; declare another type, such as VARCHAR, and cast the value in SQL, for example v::VARCHAR"
    (createTableFunction conn "typed_maybe_variant" ["missing"] \(Only (value :: Maybe Variant)) -> pure [Only (isNothing value)])
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

-- | Evaluate only the columns that the query uses. A column that the query does
-- not use can raise an exception, and the query does not fail. A column in
-- WHERE or in ORDER BY is a column that the query uses. SELECT * reads all
-- columns, and count(*) reads the first column. The row is evaluated with each
-- strict field.
testTableProjection :: Assertion
testTableProjection = withConnection ":memory:" \conn -> do
  createTableFunction conn "typed_pairs" ["n", "failing"] \(Only count) ->
    pure [(n, if n == 2500 then error "typed_pairs: the column fails" else n * 2) | n <- [0 .. count - 1 :: Int64]]
  query_ conn "SELECT n FROM typed_pairs(3000) WHERE n % 1000 = 1" >>= (@?= map Only [1, 1001, 2001 :: Int64])
  query_ conn "SELECT sum(n)::BIGINT FROM typed_pairs(3000)" >>= (@?= [Only (sum [0 .. 2999 :: Int64])])
  assertSqlError "typed_pairs: the column fails" (query_ conn "SELECT failing FROM typed_pairs(3000)" :: IO [Only Int64])
  query_ conn "SELECT failing FROM typed_pairs(3)" >>= (@?= map Only [0, 2, 4 :: Int64])
  query_ conn "SELECT * FROM typed_pairs(3)" >>= (@?= ([(0, 0), (1, 2), (2, 4)] :: [(Int64, Int64)]))
  query_ conn "SELECT failing, n FROM typed_pairs(3)" >>= (@?= ([(0, 0), (2, 1), (4, 2)] :: [(Int64, Int64)]))
  assertSqlError "typed_pairs: the column fails" (query_ conn "SELECT failing, n FROM typed_pairs(3000)" :: IO [(Int64, Int64)])
  query_ conn "SELECT n, n FROM typed_pairs(3000) WHERE n % 1000 = 1" >>= (@?= ([(1, 1), (1001, 1001), (2001, 2001)] :: [(Int64, Int64)]))
  query_ conn "SELECT failing, failing FROM typed_pairs(3)" >>= (@?= ([(0, 0), (2, 2), (4, 4)] :: [(Int64, Int64)]))
  query_ conn "SELECT n FROM typed_pairs(3) WHERE failing > 2" >>= (@?= [Only (2 :: Int64)])
  query_ conn "SELECT n FROM typed_pairs(3) ORDER BY failing DESC" >>= (@?= map Only [2, 1, 0 :: Int64])
  assertSqlError "typed_pairs: the column fails" (query_ conn "SELECT n FROM typed_pairs(3000) WHERE failing >= 0" :: IO [Only Int64])
  assertSqlError "typed_pairs: the column fails" (query_ conn "SELECT n FROM typed_pairs(3000) ORDER BY failing" :: IO [Only Int64])
  query_ conn "SELECT count(*) FROM typed_pairs(3000)" >>= (@?= [Only (3000 :: Int64)])
  createTableFunction conn "typed_pairs_first" ["failing", "n"] \(Only count) ->
    pure [(if n == 2500 then error "typed_pairs_first: the column fails" else n * 2, n) | n <- [0 .. count - 1 :: Int64]]
  assertSqlError "typed_pairs_first: the column fails" (query_ conn "SELECT count(*) FROM typed_pairs_first(3000)" :: IO [Only Int64])
  query_ conn "SELECT n FROM typed_pairs_first(3000) WHERE n % 1000 = 1" >>= (@?= map Only [1, 1001, 2001 :: Int64])
  createTableFunction conn "typed_strict_pairs" ["n", "failing"] \(Only count) ->
    pure [StrictPair n (if n == 2500 then error "typed_strict_pairs: the field fails" else n * 2) | n <- [0 .. count - 1 :: Int64]]
  query_ conn "SELECT n FROM typed_strict_pairs(3)" >>= (@?= map Only [0, 1, 2 :: Int64])
  assertSqlError "typed_strict_pairs: the field fails" (query_ conn "SELECT n FROM typed_strict_pairs(3000)" :: IO [Only Int64])

-- | A row type with strict fields.
data StrictPair = StrictPair !Int64 !Int64
  deriving (Generic)

instance TypedRow StrictPair

-- | Release the list of each scan after a full scan, after LIMIT, and after an
-- error. Each call makes a token. The rows that the scan did not read refer to
-- the token. Release the function when the connection closes.
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

-- | Write 3000 rows with NULLs to a file and read them back with one format.
-- The rows need more than one chunk in each direction.
testCopyBothDirections :: Assertion
testCopyBothDirections = withDirectory \directory -> withConnection ":memory:" \conn -> do
  calls <- newIORef ([] :: [CopyFromInfo])
  registerCopyFunction conn "typed_lines" linesTo \info -> do
    modifyIORef' calls (info :)
    readLines (copyFromPath info)
  let file = directory </> "lines.txt"
      expected = [if i `mod` 1000 == 7 then Nothing else Just i | i <- [0 .. 2999 :: Int64]]
  void
    ( execute
        conn
        "COPY (SELECT CASE WHEN i % 1000 = 7 THEN NULL ELSE i END AS value FROM range(3000) t(i)) TO ? (FORMAT typed_lines)"
        (Only (Text.pack file))
    )
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

-- | DuckDB keeps the first registration of a format name. A later
-- registration has no effect, so it does not add a COPY FROM side.
testCopyFirstRegistration :: Assertion
testCopyFirstRegistration = withDirectory \directory -> withConnection ":memory:" \conn -> do
  sinks <- newIORef ([] :: [Text])
  let file = Text.pack (directory </> "first.txt")
      register label =
        registerCopyToFunction
          conn
          "typed_first"
          (\_ -> pure ())
          (\info -> writeFile (copyInitFilePath info) "")
          (\_ _ -> modifyIORef' sinks (label :))
          (\_ -> pure ())
  register "first"
  register "second"
  registerCopyFromFunction conn "typed_first" \_ -> pure [Only (1 :: Int64)]
  void (execute conn "COPY (SELECT 1) TO ? (FORMAT typed_first)" (Only file))
  readIORef sinks >>= (@?= ["first"])
  void (execute_ conn "CREATE TABLE typed_first_rows (value BIGINT)")
  assertSqlError
    "COPY FROM is not supported for FORMAT \"typed_first\""
    (execute conn "COPY typed_first_rows FROM ? (FORMAT typed_first)" (Only file))
  -- A format with a COPY FROM side only rejects COPY TO.
  registerCopyFromFunction conn "typed_from_only" \_ -> pure [Only (2 :: Int64)]
  void (execute conn "COPY typed_first_rows FROM ? (FORMAT typed_from_only)" (Only file))
  query_ conn "SELECT value FROM typed_first_rows" >>= (@?= [Only (2 :: Int64)])
  assertSqlError
    "COPY TO is not supported for FORMAT \"typed_from_only\""
    (execute conn "COPY (SELECT 1) TO ? (FORMAT typed_from_only)" (Only file))
  query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])

-- | Give the reader of COPY FROM the name and the type of each column, in the
-- order of the table. DuckDB gives the same names and types.
testCopyFromColumns :: Assertion
testCopyFromColumns = withDirectory \directory -> withConnection ":memory:" \conn -> do
  calls <- newIORef ([] :: [CopyFromInfo])
  registerCopyFromFunction conn "typed_columns" \info -> do
    modifyIORef' calls (info :)
    pure [(7 :: Int64, "seven" :: Text, fromGregorian 2026 11 15)]
  let file = directory </> "columns.txt"
  writeFile file ""
  void (execute_ conn "CREATE TABLE typed_three (a BIGINT, b VARCHAR, c DATE)")
  void (execute conn "COPY typed_three FROM ? (FORMAT typed_columns)" (Only (Text.pack file)))
  query_ conn "SELECT column_name, data_type FROM information_schema.columns WHERE table_name = 'typed_three' ORDER BY ordinal_position"
    >>= (@?= [("a" :: Text, "BIGINT" :: Text), ("b", "VARCHAR"), ("c", "DATE")])
  readIORef calls >>= (@?= [CopyFromInfo file ["a", "b", "c"] [LogicalTypeIdBigInt, LogicalTypeIdVarchar, LogicalTypeIdDate]])
  query_ conn "SELECT a, b, c FROM typed_three" >>= (@?= [(7 :: Int64, "seven" :: Text, fromGregorian 2026 11 15)])

-- | Raise an error in each COPY TO function, in the COPY FROM function, and in
-- a lazy row after the first chunk. Each error fails its statement. The next
-- statement runs.
testCopyErrors :: Assertion
testCopyErrors = withDirectory \directory -> withConnection ":memory:" \conn -> do
  let file = Text.pack (directory </> "errors.txt")
  void (execute_ conn "CREATE TABLE typed_failed (value BIGINT)")
  forM_ ["bind", "init", "sink", "finalize"] \phase -> do
    registerFailingFormat conn phase
    assertSqlError
      ("typed_copy_error: " <> phase)
      (execute conn (Query ("COPY (SELECT 1) TO ? (FORMAT typed_error_" <> phase <> ")")) (Only file))
    query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
  forM_ ["from", "row"] \phase -> do
    registerFailingFormat conn phase
    assertSqlError
      ("typed_copy_error: " <> phase)
      (execute conn (Query ("COPY typed_failed FROM ? (FORMAT typed_error_" <> phase <> ")")) (Only file))
    query_ conn "SELECT count(*) FROM typed_failed" >>= (@?= [Only (0 :: Int64)])
  -- The rows of a format must have one column for each column of the table.
  void (execute_ conn "CREATE TABLE typed_wide (a BIGINT, b BIGINT)")
  registerFailingFormat conn "none"
  assertSqlError
    "the table of COPY FROM has 2 columns, but a row of typed_error_none has 1"
    (execute conn "COPY typed_wide FROM ? (FORMAT typed_error_none)" (Only file))
  void (execute conn "COPY typed_failed FROM ? (FORMAT typed_error_none)" (Only file))
  query_ conn "SELECT count(*) FROM typed_failed" >>= (@?= [Only (3000 :: Int64)])

-- | Register a format on one connection. Use it from another connection.
testCopySharedDatabase :: Assertion
testCopySharedDatabase = withDirectory \directory -> withEnvironment \environment -> withInstance environment [] \instance' -> do
  -- An instance starts with no database. The case creates a table, so it
  -- attaches an in-memory database and makes it the default.
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

-- | Raise the input error of an empty format name, also on a closed connection.
-- The name check comes before the read of the state of the connection.
testCopyClosedName :: Assertion
testCopyClosedName = do
  closed <- open ":memory:"
  close closed
  assertInputErrorContaining "invalid copy function name" $
    registerCopyToFunction closed "" (\_ -> pure ()) (\_ -> pure ()) (\_ _ -> pure ()) (\_ -> pure ())
  assertInputErrorContaining "invalid copy function name" $
    registerCopyFunction closed "" linesTo (\_ -> pure [Only (1 :: Int64)])

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
    line row = error ("typed_lines: unexpected row " <> show (map fieldValue row))

-- | Read a file of 'linesTo'. An empty line is NULL.
readLines :: FilePath -> IO [Only (Maybe Int64)]
readLines path = map (Only . parse . Text.unpack) . Text.lines <$> TextIO.readFile path
  where
    parse "" = Nothing
    parse line = Just (read line)

-- | Register the format @typed_error_@ with the phase as suffix. The function
-- of that phase raises an error. The phase @row@ makes the row at index 2500 raise
-- an error. That row is in the second chunk.
registerFailingFormat :: Connection -> Text -> IO ()
registerFailingFormat conn phase =
  registerCopyFunction conn ("typed_error_" <> phase) failing \_ -> failIn "from" >> pure rows
  where
    failIn name = when (phase == name) (ioError (userError ("typed_copy_error: " <> Text.unpack name)))
    failing =
      CopyTo
        (\_ -> failIn "bind")
        (\info -> failIn "init" >> writeFile (copyInitFilePath info) "")
        (\_ _ -> failIn "sink")
        (\_ -> failIn "finalize")
    rows = [Only (if phase == "row" && i == 2500 then error "typed_copy_error: row" else i) | i <- [0 .. 2999 :: Int64]]

-- | Read 8 files through a multi-file function over a typed table function.
-- DuckDB runs exec callbacks of one scan at the same time for a multi-file
-- function. Each row must come one time. The case sets 4 threads, so that it
-- does not use the default number of threads of the machine.
testTableMultiFile :: Assertion
testTableMultiFile = withDirectory \directory -> withConnection ":memory:" \conn -> do
  void (execute_ conn "SET threads = 4")
  createTableFunction conn "typed_numbers" ["value"] \(Only path) ->
    map (Only . read) . lines <$> readFile (Text.unpack path) :: IO [Only Int64]
  registerMultiFileFunction conn "typed_all_numbers" "typed_numbers" defaultMultiFileOptions
  -- A file of 10000 rows needs 5 output chunks. The multi-file function can
  -- then run more than one exec callback for the scan of one file.
  let numbers file = [file * 100000 + row | row <- [0 .. 9999]] :: [Int64]
  forM_ [0 .. 7] \file -> writeFile (directory </> ("numbers" <> show file <> ".txt")) (unlines (map show (numbers file)))
  forM_ [1 :: Int .. 5] \_ ->
    query conn "SELECT value FROM typed_all_numbers(?) ORDER BY value" (Only (Text.pack (directory </> "numbers*.txt")))
      >>= (@?= map Only (concatMap numbers [0 .. 7]))
  -- count(*) uses no column. The typed table function declares projection
  -- pushdown, so the multi-file function can run it.
  query conn "SELECT count(*) FROM typed_all_numbers(?)" (Only (Text.pack (directory </> "numbers*.txt")))
    >>= (@?= [Only (80000 :: Int64)])

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

-- | The registration kinds that have no other check of the release of their
-- callback closure. Each action registers one callback that reads the token,
-- runs the callback one time, and closes the database.
closureKinds :: [(String, IORef Int64 -> IO ())]
closureKinds =
  [
    ( "typed aggregate"
    , \token -> withConnection ":memory:" \conn -> do
        createAggregate
          conn
          "closure_sum"
          sumAggregate{aggregateStep = \total (Only value) -> (\factor -> total + factor * value) <$> readIORef token}
        query_ conn "SELECT closure_sum(i) FROM range(10) t(i)" >>= (@?= [Only (45 :: Int64)])
    )
  ,
    ( "cast"
    , \token -> withConnection ":memory:" \conn -> do
        registerCustomType conn "closure_type" (TypeExpression "BIGINT")
        -- The callback reads the token in each call, so its closure keeps the token.
        R.registerCast conn (TypeExpression "BIGINT") (TypeExpression "closure_type") Nothing \info _ ->
          readIORef token >> writeNullRows info
        query_ conn "SELECT CAST(41::BIGINT AS closure_type) IS NULL" >>= (@?= [Only True])
    )
  ,
    ( "connection replacement scan"
    , \token -> withConnection ":memory:" \conn -> do
        registerReplacementScan conn (closureScan token)
        query_ conn "SELECT value FROM closure_table" >>= (@?= [Only (42 :: Int64)])
    )
  ,
    ( "instance replacement scan"
    , \token -> withEnvironment \environment -> withInstance environment [] \instance' -> do
        registerInstanceReplacementScan instance' (closureScan token)
        withConnectionOn instance' \conn -> query_ conn "SELECT value FROM closure_table" >>= (@?= [Only (42 :: Int64)])
    )
  ,
    ( "custom option update"
    , \token -> withConnection ":memory:" \conn -> do
        registerCustomOption conn (customOption "closure_option" (TypeExpression "BIGINT")){customOptionUpdate = Just \_ -> void (readIORef token)}
        void (execute_ conn "SET closure_option = 5")
    )
  ,
    ( "COPY format"
    , \token -> withConnection ":memory:" \conn -> do
        let use = void (readIORef token)
        registerCopyFunction
          conn
          "closure_copy"
          CopyTo{copyToBind = const use, copyToInit = const use, copyToSink = \_ _ -> use, copyToFinalize = const use}
          (\_ -> use >> pure [Only (42 :: Int64)])
        void (execute_ conn "COPY (SELECT 1::BIGINT) TO 'unused' (FORMAT closure_copy)")
        void (execute_ conn "CREATE TABLE closure_rows (value BIGINT)")
        void (execute_ conn "COPY closure_rows FROM 'unused' (FORMAT closure_copy)")
        query_ conn "SELECT value FROM closure_rows" >>= (@?= [Only (42 :: Int64)])
    )
  ,
    ( "typed pragma"
    , \token -> withConnection ":memory:" \conn -> do
        registerPragmaFunction conn "closure_pragma" [] (ExecPragma \() -> void (readIORef token))
        void (execute_ conn "PRAGMA closure_pragma")
    )
  ,
    ( "log sink"
    , \token -> withConnection ":memory:" \conn -> do
        Logging.registerLogSink conn "closure_log" (\_ -> void (readIORef token)) (void (readIORef token))
        void
          (execute_ conn "CALL enable_logging(); SET logging_storage = 'closure_log'; SELECT write_log('closure'); SET logging_storage = 'memory'")
    )
  ]

-- | A replacement scan that reads the token and replaces @closure_table@.
closureScan :: IORef Int64 -> [Text] -> IO (Maybe Replacement)
closureScan token parts = do
  void (readIORef token)
  pure (if parts == ["closure_table"] then Just (ReplaceWithQuery "SELECT 42::BIGINT AS value") else Nothing)
