{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- The tests call deleteFunction on purpose, so its warning does not apply.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : ExtensionRegressionTests
-- Description : Regression tests for extension callbacks and native handle ownership.
module ExtensionRegressionTests (main, extensionRegressionTests) where

import Control.Exception (
  AsyncException (ThreadKilled),
  ErrorCall,
  Exception (..),
  IOException,
  SomeException,
  bracket,
  bracket_,
  finally,
  throwIO,
  try,
 )
import Control.Monad (forM, forM_, replicateM_, void, when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (find, isInfixOf, nub, sort)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Word (Word64)
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Catalog as Catalog
import qualified Database.DuckDB.Simple.Config as Config
import qualified Database.DuckDB.Simple.Copy as Copy
import qualified Database.DuckDB.Simple.FileSystem as FileSystem
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Internal (
  MacroFunctionType,
  errorTypeCodes,
  errorTypeFromNative,
  logLevelFromNative,
  logLevelToNative,
  logScopeFromNative,
  macroFunctionTypeToNative,
  settingScopeFromNative,
  settingScopeToNative,
  tokenTypeFromNative,
 )
import qualified Database.DuckDB.Simple.Logging as Logging
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..))
import qualified Database.DuckDB.Simple.Tokenizer as Tokenizer
import qualified Database.DuckDB.Simple.V2.Registration as Registration
import Numeric.Natural (Natural)
import System.Directory (doesFileExist, getHomeDirectory, getTemporaryDirectory, removeFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, (@?=))
import TestUtils (assertReleasedAfter)

-- | Run the extension regression tests as a standalone executable.
main :: IO ()
main = defaultMain extensionRegressionTests

-- | Check callback failures, value conversion, and resource cleanup.
extensionRegressionTests :: TestTree
extensionRegressionTests =
  testGroup
    "extension regressions"
    [ -- A registration on a closed connection raises the error before it
      -- allocates a callback.
      testCase "scalar registration on a closed connection raises the closed error" do
        conn <- open ":memory:"
        close conn
        replicateM_ 32 $ assertSqlErrorContaining "connection is closed" $ createFunction conn "closed_scalar" (1 :: Int64)
    , testCase "stateful registration on a closed connection raises the closed error" do
        conn <- open ":memory:"
        close conn
        replicateM_ 32
          $ assertSqlErrorContaining "connection is closed"
          $ createFunctionWithState conn "closed_state" (pure ()) (\() -> (1 :: Int64))
    , testCase "COPY registration on a closed connection raises the closed error" do
        conn <- open ":memory:"
        close conn
        replicateM_ 32 $ assertSqlErrorContaining "connection is closed" $ registerCopy conn "closed_copy" Nothing
    , testCase "logging registration on a closed connection raises the closed error" do
        conn <- open ":memory:"
        close conn
        replicateM_ 32 $ assertSqlErrorContaining "connection is closed" $ Logging.registerLogStorage conn "closed_log" (\_ -> pure ())
    , testCase "scalar replacement releases each replaced callback before the connection closes" $
        withConnection ":memory:" \conn -> do
          createFunction conn "duplicate_scalar" (42 :: Int64)
          assertReleasedAfter "a replaced scalar function" \token -> do
            replicateM_ 32 $ createFunction conn "duplicate_scalar" (readIORef token)
            createFunction conn "duplicate_scalar" (7 :: Int64)
          query_ conn "SELECT duplicate_scalar()" >>= (@?= [Only (7 :: Int64)])
    , testCase "scalar deletion refusal keeps function and connection usable" $
        withConnection ":memory:" \conn -> do
          createFunction conn "retained_scalar" (42 :: Int64)
          assertSqlErrorContaining "Cannot drop internal catalog entry" $ deleteFunction conn "retained_scalar"
          query_ conn "SELECT retained_scalar()" >>= (@?= [Only (42 :: Int64)])
          query_ conn "SELECT 7" >>= (@?= [Only (7 :: Int64)])
    , testCase "absent scalar deletion succeeds" $
        withConnection ":memory:" \conn -> do
          deleteFunction conn "absent_scalar"
          query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    , testCase "deleteFunction reads the name with the qualified-name rules of DuckDB" $
        withConnection ":memory:" \conn -> do
          let macroCount :: Text.Text -> Text.Text -> IO [Only Int64]
              macroCount schema name = query conn "SELECT count(*) FROM duckdb_functions() WHERE schema_name = ? AND function_name = ?" (schema, name)
          void $ execute_ conn "CREATE MACRO \"a.b\"(x) AS x + 1; CREATE SCHEMA s; CREATE MACRO s.f(x) AS x; CREATE MACRO s.g(x) AS x"
          mapM (uncurry macroCount) [("main", "a.b"), ("s", "f"), ("s", "g")] >>= (@?= replicate 3 [Only 1])
          deleteFunction conn "\"a.b\""
          macroCount "main" "a.b" >>= (@?= [Only 0])
          deleteFunction conn "s.f"
          macroCount "s" "f" >>= (@?= [Only 0])
          deleteFunction conn "memory.s.g"
          macroCount "s" "g" >>= (@?= [Only 0])
          -- The parser rejects a quote inside a part that has no quotes, so no
          -- statement runs.
          void $ execute_ conn "CREATE TABLE kept (i INTEGER)"
          assertSqlErrorContaining "Unexpected quote" $ deleteFunction conn "x\"; DROP TABLE kept; --"
          query_ conn "SELECT count(*) FROM duckdb_tables() WHERE table_name = 'kept'" >>= (@?= [Only (1 :: Int64)])
          assertSqlErrorContaining "Unexpected quote" $ deleteFunction conn "a\"b"
          assertSqlErrorContaining "between one and three parts" $ deleteFunction conn "a.b.c.d"
    , testCase "stateful replacement releases each replaced callback before the connection closes" $
        withConnection ":memory:" \conn -> do
          createFunctionWithState conn "duplicate_state" (pure ()) (\() -> (42 :: Int64))
          assertReleasedAfter "a replaced stateful function" \token -> do
            replicateM_ 32 $ createFunctionWithState conn "duplicate_state" (readIORef token) (id :: Int64 -> Int64)
            createFunctionWithState conn "duplicate_state" (pure ()) (\() -> (7 :: Int64))
          query_ conn "SELECT duplicate_state()" >>= (@?= [Only (7 :: Int64)])
    , -- DuckDB refuses the type of the argument after the registration
      -- allocates its callbacks.
      testCase "scalar registration with an invalid signature releases its callbacks at the failure" $
        withConnection ":memory:" \conn ->
          assertReleasedAfter "a scalar function with an invalid signature" \token ->
            replicateM_ 32 $ assertSqlError $ createFunction conn "invalid_scalar" (InvalidSignature token)
    , testCase "stateful registration with an invalid signature releases its callbacks at the failure" $
        withConnection ":memory:" \conn ->
          assertReleasedAfter "a stateful function with an invalid signature" \token ->
            replicateM_ 32 $ assertSqlError $ createFunctionWithState conn "invalid_state" (readIORef token) (\_ -> InvalidSignature token)
    , testCase "logging duplicate registration releases each rejected callback at the failure" $
        withConnection ":memory:" \conn -> do
          Logging.registerLogStorage conn "duplicate_log" (\_ -> pure ())
          assertReleasedAfter "a rejected log storage" \token ->
            replicateM_ 32 $ assertSqlError $ Logging.registerLogStorage conn "duplicate_log" (\_ -> void (readIORef token))
          query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    , testCase "empty registration names fail safely" $
        withConnection ":memory:" \conn -> replicateM_ 32 do
          assertSqlError $ createFunction conn "" (1 :: Int64)
          assertSqlError $ createFunctionWithState conn "" (pure ()) (\() -> (1 :: Int64))
          assertSqlError $ registerCopy conn "" Nothing
          assertSqlError $ Logging.registerLogStorage conn "" (\_ -> pure ())
    , testCase "NUL registration names fail safely" $
        withConnection ":memory:" \conn -> do
          assertSqlError $ createFunction conn "nul\0scalar" (1 :: Int64)
          assertSqlError $ createFunctionWithState conn "nul\0state" (pure ()) (\() -> (1 :: Int64))
          assertSqlError $ registerCopy conn "nul\0copy" Nothing
          assertSqlError $ Logging.registerLogStorage conn "nul\0log" (\_ -> pure ())
    , testCase "a scalar registration whose setup fails releases its callbacks at the failure" $
        -- The registration forces the argument declaration after it acquires the callbacks.
        -- If a change forces the declaration before the acquisition, this check cannot fail.
        withConnection ":memory:" \conn ->
          assertReleasedAfter "a scalar function whose setup fails" \token -> replicateM_ 32 do
            outcome <- try (createFunction conn "broken_signature" (BrokenSignature token)) :: IO (Either ErrorCall ())
            case outcome of
              Left err -> assertBool (displayException err) ("broken scalar signature" `isInfixOf` displayException err)
              Right () -> assertFailure "expected signature exception"
    , testCase "a raw result of another type than the result vector is an SQL error" $
        withConnection ":memory:" \conn -> do
          createFunction conn "mismatched_result" MismatchedResult
          assertSqlErrorContaining
            "but the result value has the type BIGINT"
            (query_ conn "SELECT mismatched_result()" :: IO [Only Text.Text])
          query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    , testCase "scalar exceptions become SQL errors" $
        withConnection ":memory:" \conn -> do
          createFunction conn "throw_scalar" (throwIO (userError "scalar callback failure") :: IO Int64)
          assertSqlErrorContaining "scalar callback failure" (query_ conn "SELECT throw_scalar()" :: IO [Only Int64])
          query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    , testCase "a scalar callback failure gives an SQL error of the type ErrorInputInvalid" $
        withConnection ":memory:" \conn -> do
          -- The library gives DuckDB the code DuckDBV2ErrorApi for each
          -- callback failure. DuckDB 2.0.0 reports that code as an invalid input.
          createFunction conn "throw_typed_scalar" (throwIO (userError "typed callback failure") :: IO Int64)
          outcome <- try (query_ conn "SELECT throw_typed_scalar()" :: IO [Only Int64])
          case outcome of
            Left (err :: SQLError) -> do
              sqlErrorType err @?= Just ErrorInputInvalid
              assertBool (show err) ("typed callback failure" `Text.isInfixOf` sqlErrorMessage err)
            Right _ -> assertFailure "expected SQL error"
    , testCase "exception rendering cannot escape scalar callback" $
        withConnection ":memory:" \conn -> do
          createFunction conn "throw_bad_display" (throwIO BrokenDisplay :: IO Int64)
          assertSqlErrorContaining "Haskell callback failed" (query_ conn "SELECT throw_bad_display()" :: IO [Only Int64])
    , testCase "scalar async exceptions become SQL errors" $
        withConnection ":memory:" \conn -> do
          createFunction conn "throw_async_scalar" (throwIO ThreadKilled :: IO Int64)
          assertSqlErrorContaining "thread killed" (query_ conn "SELECT throw_async_scalar()" :: IO [Only Int64])
    , testCase "scalar initializer exceptions become SQL errors" $
        withConnection ":memory:" \conn -> do
          createFunctionWithState conn "throw_init" (throwIO (userError "state init failure") :: IO ()) (\() -> (1 :: Int64))
          assertSqlErrorContaining "state init failure" (query_ conn "SELECT throw_init()" :: IO [Only Int64])
    , testCase "scalar initializer async exceptions become SQL errors" $
        withConnection ":memory:" \conn -> do
          createFunctionWithState conn "throw_async_init" (throwIO ThreadKilled :: IO ()) (\() -> (1 :: Int64))
          assertSqlErrorContaining "thread killed" (query_ conn "SELECT throw_async_init()" :: IO [Only Int64])
    , testCase "stateful scalar exceptions become SQL errors" $
        withConnection ":memory:" \conn -> do
          createFunctionWithState conn "throw_state" (pure ()) (\() -> throwIO (userError "state execution failure") :: IO Int64)
          assertSqlErrorContaining "state execution failure" (query_ conn "SELECT throw_state()" :: IO [Only Int64])
    , testCase "Float scalar results preserve NaN infinity and negative zero" $
        withConnection ":memory:" \conn ->
          forM_
            [ (0 / 0, isNaN)
            , (1 / 0, \value -> isInfinite value && value > 0)
            , (-1 / 0, \value -> isInfinite value && value < 0)
            , (negate 0, isNegativeZero)
            ]
            \(value, matches) -> do
              createFunction conn "float_value" (value :: Float)
              createFunctionWithState conn "float_state" (pure ()) (\() -> value)
              [(plain, stateful)] <- query_ conn "SELECT float_value(), float_state()" :: IO [(Double, Double)]
              assertBool "plain callback preserves the floating-point value" (matches plain)
              assertBool "stateful callback preserves the floating-point value" (matches stateful)
    , testCase "Word64 scalar preserves unsigned maximum" $
        withConnection ":memory:" \conn -> do
          createFunction conn "word64_identity" (id :: Word64 -> Word64)
          query_ conn "SELECT word64_identity(18446744073709551615::UBIGINT)" >>= (@?= [Only (maxBound :: Word64)])
          query_ conn "SELECT typeof(word64_identity(0::UBIGINT))" >>= (@?= [Only ("UBIGINT" :: Text.Text)])
    , testCase "Word scalar preserves unsigned maximum" $
        withConnection ":memory:" \conn -> do
          createFunction conn "word_maximum" (maxBound :: Word)
          query_ conn "SELECT word_maximum()" >>= (@?= [Only (maxBound :: Word)])
    , testCase "signed scalar preserves limits" $
        withConnection ":memory:" \conn -> do
          createFunction conn "int64_identity" (id :: Int64 -> Int64)
          query_ conn "SELECT int64_identity(x) FROM (VALUES ((-9223372036854775808)::BIGINT), (9223372036854775807::BIGINT)) t(x)"
            >>= (@?= [Only (minBound :: Int64), Only maxBound])
    , testCase "scalar strings preserve UTF8 and embedded NUL" $
        withConnection ":memory:" \conn -> do
          let payload = "Matti \955\0 suffix" :: Text.Text
          createFunction conn "text_payload" payload
          createFunction conn "text_identity" (id :: Text.Text -> Text.Text)
          query_ conn "SELECT text_identity(text_payload())" >>= (@?= [Only payload])
    , testCase "scalar NULL arguments reach Maybe functions" $
        withConnection ":memory:" \conn -> do
          createFunction conn "null_default" (\(x :: Maybe Int64) -> maybe 42 id x)
          query_ conn "SELECT null_default(NULL::BIGINT)" >>= (@?= [Only (42 :: Int64)])
    , testCase "scalar nullable strings across chunks" $
        withConnection ":memory:" \conn -> do
          createFunction conn "optional_text" (\(x :: Int64) -> if even x then Just ("\955\0" <> Text.replicate 32 "x") else Nothing)
          query_ conn "SELECT count(*), count(optional_text(i)), min(length(optional_text(i))) FROM range(5000) t(i)"
            >>= (@?= [(5000 :: Int64, 2500 :: Int64, Just (34 :: Int64))])
    , testCase "scalar DECIMAL inputs use supported numeric casts" $
        withConnection ":memory:" \conn -> do
          createFunction conn "double_identity" (id :: Double -> Double)
          query_ conn "SELECT double_identity(123.25::DECIMAL(10,2))" >>= (@?= [Only (123.25 :: Double)])
    , testCase "COPY exceptions become SQL errors in every phase" $
        withMissingFile \path -> withConnection ":memory:" \conn ->
          mapM_
            ( \phase -> do
                let name = "copy_fail_" <> Text.pack (show phase)
                registerCopy conn name (Just phase)
                assertSqlErrorContaining (Text.pack ("copy phase " <> show phase)) $
                  execute conn (Query ("COPY (SELECT 1) TO ? (FORMAT " <> name <> ")")) (Only (Text.pack path))
            )
            [0 .. 3]
    , testCase "COPY state is valid across repeated queries" $
        withMissingFile \path -> withConnection ":memory:" \conn -> do
          registerCopy conn "copy_repeat" Nothing
          replicateM_ 32 $ void $ execute conn "COPY (SELECT * FROM range(5000)) TO ? (FORMAT copy_repeat)" (Only (Text.pack path))
    , testCase "log sink callback exceptions cannot fail a query" do
        calls <- newIORef (0 :: Int)
        withConnection ":memory:" \conn -> do
          Logging.registerLogStorage conn "throwing_log" (\_ -> atomicModifyIORef' calls (\n -> (n + 1, ())) >> throwIO ThreadKilled)
          void $ execute_ conn "CALL enable_logging(); SET logging_storage = 'throwing_log'; SELECT write_log('contained')"
          query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
        readIORef calls >>= assertBool "the sink ran" . (> 0)
    , testCase "log sinks copy entries and flush when released" do
        entries <- newIORef []
        flushes <- newIORef (0 :: Int)
        start <- getCurrentTime
        withConnection ":memory:" \conn -> do
          Logging.registerLogSink
            conn
            "captured_log"
            (\entry -> atomicModifyIORef' entries (\values -> (entry : values, ())))
            (atomicModifyIORef' flushes (\count -> (count + 1, ())))
          void $
            execute_
              conn
              "CALL enable_logging(); SET logging_storage = 'captured_log'; SELECT write_log('Matti λ', level := 'warning', log_type := 'HaskellTest'); SET logging_storage = 'memory'"
          -- DuckDB 2.0.0 flushes the sink five times for these statements.
          readIORef flushes >>= (@?= 5)
        end <- getCurrentTime
        received <- filter ((== "HaskellTest") . Logging.logEntryType) <$> readIORef entries
        map Logging.logEntryMessage received @?= ["Matti λ"]
        map Logging.logEntrySeverity received @?= [Logging.LogLevelWarning]
        map Logging.logEntryScope received @?= [Logging.LogScopeConnection]
        assertBool "entry has a timestamp" (all ((/= Nothing) . Logging.logEntryTimestamp) received)
        -- A timestamp in the wrong unit is far from the clock of the test.
        let nearClock time = addUTCTime (-300) start <= time && time <= addUTCTime 300 end
        assertBool
          ("timestamps " <> show (map Logging.logEntryTimestamp received) <> " are within five minutes of " <> show (start, end))
          (all (maybe False nearClock . Logging.logEntryTimestamp) received)
        assertBool "entry has a connection id" (all ((/= Nothing) . Logging.logEntryConnectionId) received)
        -- The release flushes the sink one more time.
        readIORef flushes >>= (@?= 6)
    , testCase "instance log sinks receive connection and instance messages" do
        entries <- newIORef []
        withEnvironment \environment -> withInstance environment [] \instance_ -> do
          attachDatabase instance_ ":memory:" "shared" []
          setDefaultDatabase instance_ "shared"
          Logging.registerInstanceLogSink instance_ "instance_log" (\entry -> atomicModifyIORef' entries (\values -> (entry : values, ()))) (pure ())
          withConnectionOn instance_ \conn -> do
            void $ execute_ conn "CALL enable_logging(); SET logging_storage = 'instance_log'"
            Logging.logConnectionMessage conn Logging.LogLevelWarning "HaskellConnection" "from a connection"
            Logging.logInstanceMessage instance_ Logging.LogLevelWarning "HaskellInstance" "from an instance"
            void $ execute_ conn "SET logging_storage = 'memory'"
        received <- readIORef entries
        let ofType kind = filter ((== kind) . Logging.logEntryType) received
        map Logging.logEntryMessage (ofType "HaskellConnection") @?= ["from a connection"]
        map Logging.logEntryScope (ofType "HaskellConnection") @?= [Logging.LogScopeConnection]
        assertBool "connection entry has a connection id" (all ((/= Nothing) . Logging.logEntryConnectionId) (ofType "HaskellConnection"))
        map Logging.logEntryMessage (ofType "HaskellInstance") @?= ["from an instance"]
        map Logging.logEntryScope (ofType "HaskellInstance") @?= [Logging.LogScopeDatabase]
        map Logging.logEntryConnectionId (ofType "HaskellInstance") @?= [Nothing]
    , testCase "log entries carry the connection, transaction, and query ids of DuckDB" do
        entries <- newIORef []
        (connectionId, transactionId, queryId) <- withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
          Logging.registerLogSink conn "id_log" (\entry -> atomicModifyIORef' entries (\values -> (entry : values, ()))) (pure ())
          void $ execute_ conn "CALL enable_logging(); SET logging_storage = 'id_log'"
          -- In a transaction, the transaction id and the query id differ.
          void $ execute_ conn "BEGIN"
          _ <- query_ conn "SELECT 1" :: IO [Only Int64]
          rows <-
            query_
              conn
              "SELECT write_log('ids', level := 'warning', log_type := 'HaskellIds'), current_connection_id(), current_transaction_id(), current_query_id()"
              :: IO [(FieldValue, Word64, Word64, Word64)]
          void $ execute_ conn "COMMIT; SET logging_storage = 'memory'"
          case rows of
            [(_, connection, transaction, queryNumber)] -> pure (connection, transaction, queryNumber)
            _ -> assertFailure ("expected one row, got " <> show rows)
        assertEqual "the ids differ" 3 (length (nub [connectionId, transactionId, queryId]))
        received <- filter ((== "HaskellIds") . Logging.logEntryType) <$> readIORef entries
        map Logging.logEntryConnectionId received @?= [Just connectionId]
        map Logging.logEntryTransactionId received @?= [Just transactionId]
        map Logging.logEntryQueryId received @?= [Just queryId]
        map Logging.logEntryThreadId received @?= [Nothing]
    , testCase "filesystem action exceptions propagate and the file opens again" $
        withTemporaryFile \path ->
          withConnection ":memory:" \conn -> do
            outcome <- try $ FileSystem.withFileHandle conn path [FileSystem.FileFlagWrite] \_ -> throwIO (userError "file action failure")
            case outcome of
              Left (err :: IOException) -> displayException err @?= "user error (file action failure)"
              Right () -> assertFailure "expected file action exception"
            FileSystem.withFileHandle conn path [FileSystem.FileFlagWrite] \handle -> do
              FileSystem.writeFileHandleBytes handle (BS.pack [0, 255]) >>= (@?= 2)
              FileSystem.fileHandleSync handle
            FileSystem.withFileHandle conn path [FileSystem.FileFlagRead] \handle -> do
              FileSystem.readFileHandleChunk handle 0 >>= (@?= BS.empty)
              FileSystem.readFileHandleChunk handle 8 >>= (@?= BS.pack [0, 255])
    , testCase "filesystem errors remain controlled" $
        withConnection ":memory:" \conn -> do
          assertSqlError $ FileSystem.withFileHandle conn "/tmp/duckdb-review-missing-dir/no-file" [FileSystem.FileFlagRead] (\_ -> pure ())
          withTemporaryFile \path ->
            assertInputErrorContaining "file path contains NUL" $
              FileSystem.withFileHandle conn (path <> "\0suffix") [FileSystem.FileFlagRead] (\_ -> pure ())
    , testCase "catalog and config NUL names fail safely" $
        withConnection ":memory:" \conn -> do
          -- The NUL check runs before the handler that gives Nothing for an unknown option.
          nulName <- try (Config.getConfigOption conn "threads\0suffix")
          case nulName of
            Left (err :: SQLError) -> do
              sqlErrorType err @?= Just ErrorInputInvalid
              sqlErrorMessage err @?= "duckdb-simple: config option name contains NUL"
            Right value -> assertFailure ("expected the NUL error, got " <> show (fmap Config.configValueText value))
          -- DuckDB reads the name with its length, so it gets the NUL byte and
          -- reports an unknown option. The library rejects the name before
          -- the native call.
          let rejectsNul = assertInputErrorContaining "config option name contains NUL"
          rejectsNul $ Config.setConfigOption conn Config.SettingScopeSession "threads\0suffix" "1"
          rejectsNul $ Config.setConfigValue conn Config.SettingScopeSession "threads\0suffix" (1 :: Int64)
          rejectsNul $ Config.getConfigValue conn "threads\0suffix"
          rejectsNul $ Config.getConfigDefaultValue conn "threads\0suffix"
          withTransaction conn do
            assertSqlError $ Catalog.catalogTypeName conn "memory\0suffix"
            assertSqlError $ Catalog.lookupCatalogEntry conn "memory" "main\0suffix" "probe" Catalog.CatalogTable
    , testCase "catalog names preserve UTF8" $
        withConnection ":memory:" \conn -> do
          void $ execute_ conn "CREATE TABLE \"\955\" (i INT)"
          withTransaction conn do
            entry <- Catalog.lookupCatalogEntry conn "memory" "main" "\955" Catalog.CatalogTable
            fmap Catalog.catalogEntryName entry @?= Just "\955"
    , testCase "catalog names use ASCII identifier comparison independently of collation" $
        withConnection ":memory:" \conn -> do
          void $ execute_ conn "CREATE TABLE MiXeD (i INT); CREATE TABLE \"Ä\" (i INT); CREATE TABLE \"ä\" (i INT)"
          void $ execute_ conn "SET default_collation = 'nocase'"
          Catalog.catalogTypeName conn "MEMORY" >>= (@?= Just "duckdb")
          entry <- Catalog.lookupCatalogEntry conn "MEMORY" "MAIN" "mixed" Catalog.CatalogTable
          fmap Catalog.catalogEntryName entry @?= Just "MiXeD"
          forM_ ["Ä", "ä"] \name -> do
            found <- Catalog.lookupCatalogEntry conn "memory" "main" name Catalog.CatalogTable
            fmap Catalog.catalogEntryName found @?= Just name
    , testCase "catalog lookup keeps nested and literal-dot schema names distinct" $
        withConnection ":memory:" \conn -> do
          void $ execute_ conn "CREATE SCHEMA parent; CREATE SCHEMA parent.child; CREATE TABLE parent.child.inside (i INT)"
          Catalog.lookupCatalogEntry conn "memory" "child" "inside" Catalog.CatalogTable >>= (@?= Nothing)
          void $ execute_ conn "CREATE SCHEMA \"parent.child\"; CREATE TABLE \"parent.child\".outside (i INT)"
          found <- Catalog.lookupCatalogEntry conn "memory" "parent.child" "outside" Catalog.CatalogTable
          fmap Catalog.catalogEntryName found @?= Just "outside"
    , testCase "catalog and config missing values remain safe" $
        withConnection ":memory:" \conn -> do
          Config.getConfigOption conn "no_such_config_option" >>= (@?= Nothing)
          withTransaction conn do
            Catalog.catalogTypeName conn "no_such_catalog" >>= (@?= Nothing)
            Catalog.lookupCatalogEntry conn "memory" "main" "no_such_table" Catalog.CatalogTable >>= (@?= Nothing)
    , testCase "configuration descriptors remain usable after connection close" $
        withConnection ":memory:" \conn -> do
          Config.setConfigOption conn Config.SettingScopeGlobal "threads" "1"
          options <- Config.listConfigOptions conn
          fmap Config.configValueText <$> Config.getConfigOption conn "threads" >>= (@?= Just (Just "1"))
          close conn
          case find ((== "threads") . Config.configOptionName) options of
            Nothing -> assertFailure "missing threads descriptor"
            Just option -> do
              Config.configOptionSetting option @?= Just "1"
              Config.configOptionName option @?= "threads"
              assertBool "description is retained" (not (Text.null (Config.configOptionDescription option)))
    , testCase "listed options give the aliases of DuckDB" $
        withConnection ":memory:" \conn -> do
          void $ execute_ conn "SET progress_bar_time = 1234"
          options <- Config.listConfigOptions conn
          let option name = find ((== name) . Config.configOptionName) options
          aliases <- query_ conn "SELECT name, aliases FROM duckdb_settings() WHERE len(aliases) > 0 ORDER BY name" :: IO [(Text.Text, [Text.Text])]
          assertBool "DuckDB lists options with aliases" (not (null aliases))
          forM_ aliases \(name, expected) ->
            assertEqual (Text.unpack name) (Just (sort expected)) (sort . Config.configOptionAliases <$> option name)
          Config.configOptionSetting <$> option "progress_bar_time" @?= Just (Just "1234")
    , testCase "option descriptors give Nothing for a NULL value and a missing default" $
        withConnection ":memory:" \conn -> do
          Config.registerCustomOption
            conn
            (Config.customOption "haskell_described" (Registration.TypeExpression "BIGINT"))
              { Config.customOptionDefault = Just (Registration.defaultValue (3 :: Int64))
              }
          void $ execute_ conn "RESET progress_bar_time"
          options <- Config.listConfigOptions conn
          let described get name = get <$> find ((== name) . Config.configOptionName) options
          described Config.configOptionDefault "haskell_described" @?= Just (Just "3")
          -- progress_bar_time has no static default. After RESET its value is 2000.
          described Config.configOptionDefault "progress_bar_time" @?= Just Nothing
          described Config.configOptionSetting "progress_bar_time" @?= Just (Just "2000")
          -- enable_profiling has a NULL value until a write.
          described Config.configOptionSetting "enable_profiling" @?= Just Nothing
          -- getConfigOption shows the NULL value in the same way.
          fmap Config.configValueText <$> Config.getConfigOption conn "enable_profiling" >>= (@?= Just Nothing)
    , testCase "instance options list global values" $
        withEnvironment \environment -> withInstance environment [("threads", "1")] \instance_ -> do
          options <- Config.listInstanceOptions instance_
          fmap Config.configOptionSetting (find ((== "threads") . Config.configOptionName) options) @?= Just (Just "1")
    , testCase "connection options set a session option on one connection only" $
        withEnvironment \environment -> withInstance environment [] \instance_ -> do
          attachDatabase instance_ ":memory:" "shared" []
          setDefaultDatabase instance_ "shared"
          -- DuckDB gives progress_bar_time the session scope by default.
          let options = defaultConnectionOptions{connectionConfig = [("progress_bar_time", "1234")]}
          bracket (connectWithOptions instance_ options) close \configured ->
            withConnectionOn instance_ \other -> do
              query_ configured "SELECT current_setting('progress_bar_time')" >>= (@?= [Only (1234 :: Int64)])
              query_ other "SELECT current_setting('progress_bar_time')" >>= (@?= [Only (2000 :: Int64)])
    , testCase "configuration values resolve case-insensitive names and aliases" $
        withConnection ":memory:" \conn -> do
          Config.setConfigOption conn Config.SettingScopeGlobal "threads" "1"
          forM_ ["threads", "THREADS", "worker_threads", "WORKER_THREADS"] \name ->
            fmap Config.configValueText <$> Config.getConfigOption conn name >>= (@?= Just (Just "1"))
          Config.getConfigOption conn "NO_SUCH_CONFIG_OPTION" >>= (@?= Nothing)
          close conn
          assertSqlError $ Config.getConfigOption conn "threads"
    , testCase "typed reads copy the value of every listed option" $
        withConnection ":memory:" \conn -> do
          options <- Config.listConfigOptions conn
          assertBool "options are listed" (not (null options))
          failures <- forM options \option -> do
            let name = Config.configOptionName option
            outcome <- try (Config.getConfigValue conn name)
            pure case outcome of
              Left (err :: SomeException) -> [(name, displayException err)]
              Right _ -> []
          concat failures @?= []
          assertSqlError $ Config.getConfigValue conn "NO_SUCH_CONFIG_OPTION"
          assertSqlError $ Config.getConfigDefaultValue conn "NO_SUCH_CONFIG_OPTION"
    , testCase "custom options retain typed defaults scopes and callback errors" $
        withConnection ":memory:" \conn -> do
          let option =
                (Config.customOption "haskell_limit" (Registration.StructuredType (LogicalTypeScalar LogicalTypeIdBigInt)))
                  { Config.customOptionDefault = Just (Registration.defaultValue (3 :: Int64))
                  , Config.customOptionDescription = "A Haskell option"
                  , Config.customOptionDefaultScope = Config.SettingScopeSession
                  , Config.customOptionScopes = [Config.SettingScopeGlobal, Config.SettingScopeSession]
                  , Config.customOptionUpdate = Just \value -> case value of
                      FieldInt64 number | number < 0 -> throwIO (userError "limit must be nonnegative")
                      _ -> pure ()
                  }
          Config.registerCustomOption conn option
          Config.getConfigDefaultValue conn "haskell_limit" >>= (@?= FieldInt64 3)
          Config.setConfigValue conn Config.SettingScopeSession "haskell_limit" (7 :: Int64)
          Config.getConfigValue conn "haskell_limit" >>= (@?= (FieldInt64 7, Config.SettingScopeSession))
          assertSqlErrorContaining "limit must be nonnegative" $ Config.setConfigValue conn Config.SettingScopeSession "haskell_limit" (-1 :: Int64)
          void $ execute_ conn "RESET haskell_limit"
          fmap Config.configValueText <$> Config.getConfigOption conn "haskell_limit" >>= (@?= Just (Just "3"))
          assertSqlError $ Config.registerCustomOption conn option
    , testCase "listConfigFlags lists a custom option of the connection" $
        withConnection ":memory:" \conn -> do
          Config.registerCustomOption conn (Config.customOption "haskell_listed" (Registration.TypeExpression "BIGINT"))
          flags <- Config.listConfigFlags conn
          options <- Config.listConfigOptions conn
          assertBool "the custom option is listed" ("haskell_listed" `elem` map Config.configFlagName flags)
          map Config.configFlagName flags @?= map Config.configOptionName options
    , testCase "typed reads of a VARIANT custom option give the value that it holds" $
        withConnection ":memory:" \conn -> do
          let variantType = Registration.TypeExpression "VARIANT"
          Config.registerCustomOption
            conn
            (Config.customOption "haskell_variant" variantType){Config.customOptionDefault = Just (Registration.defaultValue (5 :: Int64))}
          Config.getConfigDefaultValue conn "haskell_variant" >>= (@?= FieldInt64 5)
          Config.setConfigValue conn Config.SettingScopeGlobal "haskell_variant" (7 :: Int64)
          Config.getConfigValue conn "haskell_variant" >>= (@?= (FieldInt64 7, Config.SettingScopeGlobal))
          -- A nested value gives the value of the same VARIANT in a result.
          void $ execute_ conn "SET haskell_variant = {'a': 1, 'b': [1, 2]}"
          [Only expected] <- query_ conn "SELECT {'a': 1, 'b': [1, 2]}::VARIANT" :: IO [Only FieldValue]
          fst <$> Config.getConfigValue conn "haskell_variant" >>= (@?= expected)
          -- The update callback gets no connection, so it rejects a VARIANT value.
          Config.registerCustomOption conn (Config.customOption "haskell_variant_update" variantType){Config.customOptionUpdate = Just \_ -> pure ()}
          assertSqlErrorContaining "configuration values must not contain VARIANT" $ execute_ conn "SET haskell_variant_update = 1"
    , testCase "setConfigValue with an Integer or a Natural checks the range of an unsigned option" $
        withConnection ":memory:" \conn -> do
          -- DuckDB 2.0.0 gives asof_loop_join_threshold the type UBIGINT.
          let option = "asof_loop_join_threshold"
              current = fst <$> Config.getConfigValue conn option
          Config.setConfigValue conn Config.SettingScopeSession option (9 :: Integer)
          current >>= (@?= FieldWord64 9)
          assertSqlErrorContaining "out of range for the destination type UINT64" $
            Config.setConfigValue conn Config.SettingScopeSession option (-5 :: Integer)
          current >>= (@?= FieldWord64 9)
          Config.setConfigValue conn Config.SettingScopeSession option (11 :: Natural)
          current >>= (@?= FieldWord64 11)
    , testCase "setConfigValue with an Integer outside the HUGEINT range keeps the BIGNUM casts of DuckDB" $
        withConnection ":memory:" \conn -> do
          -- The value is a BIGNUM value. DuckDB 2.0.0 reduces it modulo 2^128
          -- and gives no error. See Note [Integer parameters] in
          -- "Database.DuckDB.Simple.ToField".
          Config.setConfigValue conn Config.SettingScopeSession "asof_loop_join_threshold" (2 ^ (128 :: Int) + 5 :: Integer)
          fst <$> Config.getConfigValue conn "asof_loop_join_threshold" >>= (@?= FieldWord64 5)
    , testCase "table descriptions resolve names and copy borrowed column types" $
        withConnection ":memory:" \conn -> do
          void $ execute_ conn "CREATE TABLE \"\955\" (id BIGINT DEFAULT 7, twice BIGINT GENERATED ALWAYS AS (id * 2))"
          description <- Catalog.describeTable conn "\"\955\""
          Catalog.tableDescriptionName description @?= ["memory", "main", "\955"]
          Catalog.tableDescriptionReadonly description @?= False
          map Catalog.columnDescriptionName (Catalog.tableDescriptionColumns description) @?= ["id", "twice"]
          map Catalog.columnDescriptionHasDefault (Catalog.tableDescriptionColumns description) @?= [True, False]
          map Catalog.columnDescriptionGenerated (Catalog.tableDescriptionColumns description) @?= [False, True]
          query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
    , testCase "tokenization copies Unicode byte offsets and unterminated state" $
        withConnection ":memory:" \conn -> do
          let sql = "SELECT \"\955\", 'x'; -- comment\n"
              bytes = Text.encodeUtf8 sql
          parsed <- Tokenizer.tokenize conn sql
          Tokenizer.tokenizationUnterminated parsed @?= False
          forM_ (Tokenizer.tokenizationTokens parsed) \token ->
            BS.take (BS.length (Tokenizer.tokenBytes token)) (BS.drop (Tokenizer.tokenOffset token) bytes) @?= Tokenizer.tokenBytes token
          assertBool "comment token" (any ((== Tokenizer.TokenTypeComment) . Tokenizer.tokenType) (Tokenizer.tokenizationTokens parsed))
          unfinished <- Tokenizer.tokenize conn "SELECT 'unclosed"
          Tokenizer.tokenizationUnterminated unfinished @?= True
    , testCase "tokenization reports the type of each token" $
        withConnection ":memory:" \conn -> do
          parsed <- Tokenizer.tokenize conn "SELECT amount, 'text', 42 + 1; -- note\n"
          [(Tokenizer.tokenType token, Tokenizer.tokenBytes token) | token <- Tokenizer.tokenizationTokens parsed]
            @?= [ (Tokenizer.TokenTypeKeyword, "SELECT")
                , (Tokenizer.TokenTypeIdentifier, "amount")
                , (Tokenizer.TokenTypeOperator, ",")
                , (Tokenizer.TokenTypeStringLiteral, "'text'")
                , (Tokenizer.TokenTypeOperator, ",")
                , (Tokenizer.TokenTypeNumberLiteral, "42")
                , (Tokenizer.TokenTypeOperator, "+")
                , (Tokenizer.TokenTypeNumberLiteral, "1")
                , (Tokenizer.TokenTypeTerminator, ";")
                , (Tokenizer.TokenTypeComment, "-- note\n")
                ]
    , testCase "configuration writes and reads each setting scope" $
        withConnection ":memory:" \conn -> do
          let option name scope =
                (Config.customOption name (Registration.StructuredType (LogicalTypeScalar LogicalTypeIdBigInt)))
                  { Config.customOptionDefault = Just (Registration.defaultValue (0 :: Int64))
                  , Config.customOptionDefaultScope = scope
                  , Config.customOptionScopes = [Config.SettingScopeGlobal, Config.SettingScopeSession]
                  }
          Config.registerCustomOption conn (option "haskell_global_scope" Config.SettingScopeGlobal)
          Config.registerCustomOption conn (option "haskell_session_scope" Config.SettingScopeSession)
          Config.setConfigValue conn Config.SettingScopeDefault "haskell_global_scope" (1 :: Int64)
          Config.getConfigValue conn "haskell_global_scope" >>= (@?= (FieldInt64 1, Config.SettingScopeGlobal))
          Config.setConfigOption conn Config.SettingScopeDefault "haskell_session_scope" "2"
          Config.getConfigValue conn "haskell_session_scope" >>= (@?= (FieldInt64 2, Config.SettingScopeSession))
          Config.setConfigValue conn Config.SettingScopeGlobal "haskell_global_scope" (3 :: Int64)
          Config.getConfigValue conn "haskell_global_scope" >>= (@?= (FieldInt64 3, Config.SettingScopeGlobal))
          Config.setConfigOption conn Config.SettingScopeSession "haskell_global_scope" "4"
          Config.getConfigValue conn "haskell_global_scope" >>= (@?= (FieldInt64 4, Config.SettingScopeSession))
          fmap Config.configValueScope <$> Config.getConfigOption conn "haskell_global_scope" >>= (@?= Just Config.SettingScopeSession)
          options <- Config.listConfigOptions conn
          sort
            [ (Config.configOptionName descriptor, Config.configOptionScope descriptor, Config.configOptionScopes descriptor)
            | descriptor <- options
            , "haskell_" `Text.isPrefixOf` Config.configOptionName descriptor
            ]
            @?= [ ("haskell_global_scope", Config.SettingScopeGlobal, [Config.SettingScopeGlobal, Config.SettingScopeSession])
                , ("haskell_session_scope", Config.SettingScopeSession, [Config.SettingScopeGlobal, Config.SettingScopeSession])
                ]
    , testCase "log sinks receive each log level and its scope" do
        entries <- newIORef []
        withConnection ":memory:" \conn -> do
          Logging.registerLogSink conn "level_log" (\entry -> atomicModifyIORef' entries (\values -> (entry : values, ()))) (pure ())
          void $ execute_ conn "CALL enable_logging(level := 'trace'); SET logging_storage = 'level_log'"
          forM_ [minBound .. maxBound] \level -> Logging.logConnectionMessage conn level "HaskellLevels" (Text.pack (show level))
          void $ execute_ conn "SET logging_storage = 'memory'"
        received <- filter ((== "HaskellLevels") . Logging.logEntryType) <$> readIORef entries
        sort [(Logging.logEntrySeverity entry, Logging.logEntryLevel entry, Logging.logEntryMessage entry) | entry <- received]
          @?= [ (level, name, Text.pack (show level))
              | (level, name) <- zip [minBound .. maxBound] ["TRACE", "DEBUG", "INFO", "WARNING", "ERROR", "FATAL"]
              ]
        map Logging.logEntryScope received @?= replicate 6 Logging.LogScopeConnection
    , testCase "native errors report their error type" $
        withConnection ":memory:" \conn ->
          forM_
            [ ("SELEC 1", ErrorQueryParser)
            , ("SELECT * FROM no_such_table", ErrorDatabaseCatalog)
            , ("SELECT no_such_column", ErrorQueryBinder)
            , ("SELECT 'abc'::INTEGER", ErrorTypeConversion)
            ]
            \(sql, expected) -> do
              outcome <- try (execute_ conn sql)
              case outcome of
                Left (err :: SQLError) -> sqlErrorType err @?= Just expected
                Right _ -> assertFailure ("expected an error from " <> show sql)
    , testCase "each enum value has the name of its native value" do
        assertNativeRoundTrip settingScopeToNative settingScopeFromNative DuckDBV2SettingScopeMaxEnum
        assertNativeRoundTrip logLevelToNative logLevelFromNative DuckDBV2LogLevelMaxEnum
        -- The package converts these three types in one direction only.
        assertFromNative logScopeFromNative [DuckDBV2LogScopeDatabase, DuckDBV2LogScopeConnection, DuckDBV2LogScopeThread] DuckDBV2LogScopeMaxEnum
        assertFromNative
          tokenTypeFromNative
          [ DuckDBV2TokenTypeInvalid
          , DuckDBV2TokenTypeKeyword
          , DuckDBV2TokenTypeIdentifier
          , DuckDBV2TokenTypeStringLiteral
          , DuckDBV2TokenTypeNumberLiteral
          , DuckDBV2TokenTypeOperator
          , DuckDBV2TokenTypeComment
          , DuckDBV2TokenTypeTerminator
          , DuckDBV2TokenTypeEndOfInput
          ]
          DuckDBV2TokenTypeMaxEnum
        let macroTypes = [minBound .. maxBound] :: [MacroFunctionType]
        map (show . macroFunctionTypeToNative) macroTypes @?= map (("DuckDBV2" <>) . show) macroTypes
        let errorTypes = [minBound .. maxBound] :: [ErrorType]
        map (\errorType -> lookup errorType errorTypeCodes >>= errorTypeFromNative) errorTypes @?= map Just errorTypes
        map (fmap show . (`lookup` errorTypeCodes)) errorTypes @?= map (Just . ("DuckDBV2" <>) . show) errorTypes
        map errorTypeFromNative [DuckDBV2ErrorNone, DuckDBV2ErrorMaxEnum] @?= [Nothing, Nothing]
    , testCase "positioned file operations preserve the sequential position" $
        withTemporaryFile \path -> withConnection ":memory:" \conn ->
          FileSystem.withFileHandle conn path [FileSystem.FileFlagRead, FileSystem.FileFlagWrite] \handle -> do
            FileSystem.writeFileHandleBytes handle (BS.pack [1, 2, 3, 4]) >>= (@?= 4)
            FileSystem.fileHandleSeek handle 1
            FileSystem.readFileHandleAt handle 2 2 >>= (@?= BS.pack [3, 4])
            FileSystem.fileHandleTell handle >>= (@?= 1)
            FileSystem.writeFileHandleAt handle 0 (BS.pack [8, 9])
            FileSystem.fileHandleTell handle >>= (@?= 1)
            FileSystem.fileHandleSeek handle 0
            FileSystem.readFileHandleChunk handle 8 >>= (@?= BS.pack [8, 9, 3, 4])
    , testCase "file reads shorter than the file give the requested bytes" $
        withTemporaryFile \path -> withConnection ":memory:" \conn -> do
          let bytes = BS.pack [0 .. 15]
          BS.writeFile path bytes
          FileSystem.withFileHandle conn path [FileSystem.FileFlagRead] \handle -> do
            FileSystem.readFileHandleChunk handle 4 >>= (@?= BS.take 4 bytes)
            FileSystem.fileHandleTell handle >>= (@?= 4)
            FileSystem.readFileHandleAt handle 8 4 >>= (@?= BS.take 4 (BS.drop 8 bytes))
            -- A chunk read gives the bytes that remain after the position.
            FileSystem.readFileHandleChunk handle 100 >>= (@?= BS.drop 4 bytes)
    , testCase "a positioned read past the end gives the error of DuckDB" $
        withTemporaryFile \path -> withConnection ":memory:" \conn -> do
          BS.writeFile path (BS.pack [0 .. 15])
          FileSystem.withFileHandle conn path [FileSystem.FileFlagRead] \handle -> do
            outcome <- try (FileSystem.readFileHandleAt handle 14 4)
            case outcome of
              Left (err :: SQLError) -> do
                sqlErrorType err @?= Just ErrorIOGeneral
                assertBool "DuckDB read error" ("Could not read enough bytes" `Text.isInfixOf` sqlErrorMessage err)
              Right bytes -> assertFailure ("expected an IO error, got " <> show bytes)
            -- The library checks only that the end of the range fits in Int64.
            assertInputErrorContaining "file read range exceeds Int64 range" $ FileSystem.readFileHandleAt handle maxBound 1
            -- A size of 0 gives no bytes, also past the end, as in DuckDB.
            FileSystem.readFileHandleAt handle 20 0 >>= (@?= BS.empty)
            FileSystem.readFileHandleAt handle 12 4 >>= (@?= BS.pack [12 .. 15])
    , testCase "a positioned write checks that its end fits in Int64" $
        withTemporaryFile \path -> withConnection ":memory:" \conn ->
          FileSystem.withFileHandle conn path [FileSystem.FileFlagWrite] \handle -> do
            assertInputErrorContaining "file write range exceeds Int64 range" $ FileSystem.writeFileHandleAt handle (maxBound - 1) (BS.pack [1, 2])
            FileSystem.fileHandleSize handle >>= (@?= 0)
    , testCase "file flags of the other file tests create read and write a file" $
        withTemporaryFile \path -> withConnection ":memory:" \conn -> do
          removeFile path
          FileSystem.withFileHandle conn path [FileSystem.FileFlagCreate, FileSystem.FileFlagWrite] \handle ->
            FileSystem.writeFileHandleBytes handle (BS.pack [1, 2, 3]) >>= (@?= 3)
          FileSystem.withFileHandle conn path [FileSystem.FileFlagWrite] \handle ->
            FileSystem.writeFileHandleBytes handle (BS.pack [4]) >>= (@?= 1)
          FileSystem.withFileHandle conn path [FileSystem.FileFlagRead] \handle ->
            FileSystem.readFileHandleChunk handle 8 >>= (@?= BS.pack [4, 2, 3])
          FileSystem.withFileHandle conn path [FileSystem.FileFlagRead, FileSystem.FileFlagWrite] \handle -> do
            FileSystem.readFileHandleChunk handle 8 >>= (@?= BS.pack [4, 2, 3])
            FileSystem.writeFileHandleBytes handle (BS.pack [5]) >>= (@?= 1)
            FileSystem.fileHandleSize handle >>= (@?= 4)
    , testCase "file flags truncate a file and refuse to create an existing file" $
        withTemporaryFile \path -> withConnection ":memory:" \conn -> do
          BS.writeFile path (BS.pack [1, 2, 3])
          FileSystem.withFileHandle conn path [FileSystem.FileFlagWrite, FileSystem.FileFlagCreateNew] FileSystem.fileHandleSize >>= (@?= 0)
          let exclusive = [FileSystem.FileFlagWrite, FileSystem.FileFlagCreate, FileSystem.FileFlagExclusiveCreate]
          assertSqlError $ FileSystem.withFileHandle conn path exclusive (\_ -> pure ())
          removeFile path
          FileSystem.withFileHandle conn path exclusive \handle ->
            FileSystem.writeFileHandleBytes handle (BS.pack [7]) >>= (@?= 1)
          FileSystem.withFileHandle conn path [FileSystem.FileFlagRead] \handle ->
            FileSystem.readFileHandleChunk handle 8 >>= (@?= BS.pack [7])
    , testCase "file paths support expansion glob movement and removal" $
        withTemporaryFile \path -> withConnection ":memory:" \conn -> do
          FileSystem.pathExists conn path >>= (@?= True)
          -- A directory is not a file.
          FileSystem.pathExists conn (takeDirectory path) >>= (@?= False)
          home <- getHomeDirectory
          FileSystem.expandPath conn "~/duckdb-simple-path" >>= (@?= home <> "/duckdb-simple-path")
          FileSystem.globPaths conn (path <> "*") >>= (@?= [path])
          FileSystem.pathIsRemote conn "https://example.com/data.parquet" >>= (@?= True)
          FileSystem.pathIsRemote conn path >>= (@?= False)
          let moved = path <> ".moved"
          bracket_ (FileSystem.movePath conn path moved) (FileSystem.movePath conn moved path) do
            FileSystem.pathExists conn path >>= (@?= False)
            FileSystem.pathExists conn moved >>= (@?= True)
          ( do
              FileSystem.removePath conn path >>= (@?= True)
              FileSystem.removePath conn path >>= (@?= False)
            )
            `finally` BS.writeFile path BS.empty
          assertSqlError $ FileSystem.globPaths conn (path <> "\0suffix")
    ]

-- | Raise a second exception when the callback error is rendered.
data BrokenDisplay = BrokenDisplay
  deriving (Show)

instance Exception BrokenDisplay where
  displayException _ = error "broken error rendering"

-- | Force a setup exception after scalar callbacks have been acquired. The
-- registration reads the declaration of the argument in the setup. The token
-- lets a case check the release of the value.
newtype BrokenSignature = BrokenSignature (IORef Int64)

instance Function BrokenSignature where
  argumentTypes _ = [Right (error "broken scalar signature")]
  returnType _ = returnType (Proxy :: Proxy Int64)
  isVolatile _ = False
  applyFunction _ _ = error "unused function"

-- | Declare a VARCHAR result, but give a BIGINT value.
data MismatchedResult = MismatchedResult

instance Function MismatchedResult where
  argumentTypes _ = []
  returnType _ = Right (Registration.TypeExpression "VARCHAR")
  isVolatile _ = True
  applyFunction fields _ = applyFunction fields (1 :: Int64)

-- | Supply an argument type which DuckDB rejects at registration. The token
-- lets a case check the release of the value.
newtype InvalidSignature = InvalidSignature (IORef Int64)

instance Function InvalidSignature where
  argumentTypes _ = [Right (Registration.TypeExpression "no_such_type")]
  returnType _ = returnType (Proxy :: Proxy Int64)
  isVolatile _ = False
  applyFunction _ _ = error "unregistered function"

-- | Register a COPY callback and optionally fail one phase. The bind state and
-- the global state are the column count that DuckDB gives. Each query of this
-- module has one column.
registerCopy :: Connection -> Text.Text -> Maybe Int -> IO ()
registerCopy conn name failingPhase =
  Copy.registerCopyToFunction
    conn
    name
    (\info -> failPhase 0 >> pure (length (Copy.copyBindColumnTypes info)))
    (\info -> (Copy.copyInitBindState info @?= 1) >> failPhase 1 >> pure (Copy.copyInitBindState info))
    (\info _ -> (Copy.copySinkGlobalState info @?= 1) >> failPhase 2)
    (\info -> (Copy.copyFinalizeGlobalState info @?= 1) >> failPhase 3)
  where
    failPhase phase =
      if failingPhase == Just phase
        then throwIO (userError ("copy phase " <> show phase))
        else pure ()

-- | Require a controlled SQL exception.
assertSqlError :: IO a -> Assertion
assertSqlError = assertSqlErrorContaining Text.empty

-- | Require the error of an input check of the library, of the type
-- ErrorInputInvalid, which includes the expected message.
assertInputErrorContaining :: Text.Text -> IO a -> Assertion
assertInputErrorContaining expected action = do
  outcome <- try (void action)
  case outcome of
    Left (err :: SQLError) -> do
      sqlErrorType err @?= Just ErrorInputInvalid
      assertBool ("expected an input error containing " <> show expected <> ", got " <> show err) $
        ("duckdb-simple: " `Text.isPrefixOf` sqlErrorMessage err) && expected `Text.isInfixOf` sqlErrorMessage err
    Right () -> assertFailure "expected the error of an input check"

-- | Require a SQL exception which includes the expected message.
assertSqlErrorContaining :: Text.Text -> IO a -> Assertion
assertSqlErrorContaining expected action = do
  outcome <- try (void action)
  case outcome of
    Left (err :: SQLError) ->
      assertBool ("expected SQL error containing " <> show expected <> ", got " <> show err) $
        expected `Text.isInfixOf` sqlErrorMessage err
    Right () -> assertFailure "expected SQL error"

-- | Convert each value to its native value and back. The native value must have
-- the pattern name of the value. The native sentinel must raise 'SQLError'.
assertNativeRoundTrip :: (Bounded a, Enum a, Eq a, Show a, Show native) => (a -> native) -> (native -> IO a) -> native -> Assertion
assertNativeRoundTrip toNative fromNative sentinel = do
  let values = [minBound .. maxBound]
  mapM (fromNative . toNative) values >>= (@?= values)
  map (show . toNative) values @?= map (("DuckDBV2" <>) . show) values
  assertSqlErrorContaining "unknown native" (fromNative sentinel)

-- | Check that the native values convert to all constructors in order, and that
-- each constructor has the pattern name of its native value. The native sentinel
-- must raise 'SQLError'.
assertFromNative :: (Bounded a, Enum a, Eq a, Show a, Show native) => (native -> IO a) -> [native] -> native -> Assertion
assertFromNative fromNative natives sentinel = do
  values <- mapM fromNative natives
  values @?= [minBound .. maxBound]
  map show natives @?= map (("DuckDBV2" <>) . show) values
  assertSqlErrorContaining "unknown native" (fromNative sentinel)

-- | Give the action a unique temporary path where no file exists. Remove the
-- file after the action if the action created it. DuckDB writes a COPY TO
-- target that exists through a second file, so the path has no file.
withMissingFile :: (FilePath -> IO a) -> IO a
withMissingFile = bracket acquire release
  where
    acquire = do
      directory <- getTemporaryDirectory
      (path, handle) <- openBinaryTempFile directory "duckdb-extension-regression"
      hClose handle
      removeFile path
      pure path
    release path = doesFileExist path >>= \exists -> when exists (removeFile path)

-- | Remove a temporary file after the action.
withTemporaryFile :: (FilePath -> IO a) -> IO a
withTemporaryFile action =
  bracket
    (do (path, handle) <- openBinaryTempFile "/tmp" "duckdb-extension-regression"; hClose handle; pure path)
    removeFile
    action
