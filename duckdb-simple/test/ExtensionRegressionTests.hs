{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- The tests call deleteFunction on purpose, so its warning does not apply.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Regression tests for extension callbacks and native handle ownership.
module ExtensionRegressionTests (main, extensionRegressionTests) where

import Control.Exception (AsyncException (ThreadKilled), ErrorCall, Exception (..), SomeException, bracket, bracket_, finally, throwIO, try)
import Control.Monad (forM, forM_, replicateM_, void)
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (find, sort)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
import System.Directory (getHomeDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

-- | Run the extension regression tests as a standalone executable.
main :: IO ()
main = defaultMain extensionRegressionTests

-- | Check callback failures, value conversion, and resource cleanup.
extensionRegressionTests :: TestTree
extensionRegressionTests =
    testGroup
        "extension regressions"
        [ testCase "scalar closed connection cleanup" do
            conn <- open ":memory:"
            close conn
            replicateM_ 32 $ assertSqlError $ createFunction conn "closed_scalar" (1 :: Int64)
        , testCase "stateful closed connection cleanup" do
            conn <- open ":memory:"
            close conn
            replicateM_ 32 $ assertSqlError $ createFunctionWithState conn "closed_state" (pure ()) (\() -> (1 :: Int64))
        , testCase "COPY closed connection cleanup" do
            conn <- open ":memory:"
            close conn
            replicateM_ 32 $ assertSqlError $ registerCopy conn "closed_copy" Nothing
        , testCase "logging closed connection cleanup" do
            conn <- open ":memory:"
            close conn
            replicateM_ 32 $ assertSqlError $ Logging.registerLogStorage conn "closed_log" (\_ -> pure ())
        , testCase "scalar replacement registration cleanup" $
            withConnection ":memory:" \conn -> do
                createFunction conn "duplicate_scalar" (42 :: Int64)
                replicateM_ 32 $ createFunction conn "duplicate_scalar" (7 :: Int64)
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
        , testCase "stateful replacement registration cleanup" $
            withConnection ":memory:" \conn -> do
                createFunctionWithState conn "duplicate_state" (pure ()) (\() -> (42 :: Int64))
                replicateM_ 32 $ createFunctionWithState conn "duplicate_state" (pure ()) (\() -> (7 :: Int64))
                query_ conn "SELECT duplicate_state()" >>= (@?= [Only (7 :: Int64)])
        , testCase "scalar invalid signature registration cleanup" $
            withConnection ":memory:" \conn ->
                replicateM_ 32 $ assertSqlError $ createFunction conn "invalid_scalar" InvalidSignature
        , testCase "stateful invalid signature registration cleanup" $
            withConnection ":memory:" \conn ->
                replicateM_ 32 $ assertSqlError $ createFunctionWithState conn "invalid_state" (pure ()) (\() -> InvalidSignature)
        , testCase "logging duplicate registration cleanup" $
            withConnection ":memory:" \conn -> do
                Logging.registerLogStorage conn "duplicate_log" (\_ -> pure ())
                replicateM_ 32 $ assertSqlError $ Logging.registerLogStorage conn "duplicate_log" (\_ -> pure ())
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
        , testCase "partial scalar setup cleanup" $
            withConnection ":memory:" \conn -> replicateM_ 32 do
                outcome <- try (createFunction conn "broken_signature" BrokenSignature) :: IO (Either ErrorCall ())
                case outcome of
                    Left _ -> pure ()
                    Right () -> assertFailure "expected signature exception"
        , testCase "scalar exceptions become SQL errors" $
            withConnection ":memory:" \conn -> do
                createFunction conn "throw_scalar" (throwIO (userError "scalar callback failure") :: IO Int64)
                assertSqlErrorContaining "scalar callback failure" (query_ conn "SELECT throw_scalar()" :: IO [Only Int64])
                query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
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
            withConnection ":memory:" \conn ->
                mapM_
                    ( \phase -> do
                        let name = "copy_fail_" <> Text.pack (show phase)
                        registerCopy conn name (Just phase)
                        assertSqlErrorContaining (Text.pack ("copy phase " <> show phase)) $
                            execute_ conn (Query ("COPY (SELECT 1) TO '/tmp/duckdb-simple-regression-copy' (FORMAT " <> name <> ")"))
                    )
                    [0 .. 3]
        , testCase "COPY state is valid across repeated queries" $
            withConnection ":memory:" \conn -> do
                registerCopy conn "copy_repeat" Nothing
                replicateM_ 32 $ void $ execute_ conn "COPY (SELECT * FROM range(5000)) TO '/tmp/duckdb-simple-regression-copy' (FORMAT copy_repeat)"
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
            withConnection ":memory:" \conn -> do
                Logging.registerLogSink
                    conn
                    "captured_log"
                    (\entry -> atomicModifyIORef' entries (\values -> (entry : values, ())))
                    (atomicModifyIORef' flushes (\count -> (count + 1, ())))
                void $ execute_ conn "CALL enable_logging(); SET logging_storage = 'captured_log'; SELECT write_log('Matti λ', level := 'warning', log_type := 'HaskellTest'); SET logging_storage = 'memory'"
            received <- filter ((== "HaskellTest") . Logging.logEntryType) <$> readIORef entries
            map Logging.logEntryMessage received @?= ["Matti λ"]
            map Logging.logEntrySeverity received @?= [Logging.LogLevelWarning]
            map Logging.logEntryScope received @?= [Logging.LogScopeConnection]
            assertBool "entry has a timestamp" (all ((/= Nothing) . Logging.logEntryTimestamp) received)
            assertBool "entry has a connection id" (all ((/= Nothing) . Logging.logEntryConnectionId) received)
            readIORef flushes >>= assertBool "flush and final cleanup ran" . (>= 2)
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
        , testCase "filesystem action exceptions release handles" $
            withTemporaryFile \path ->
                withConnection ":memory:" \conn -> do
                    outcome <- try $ FileSystem.withFileHandle conn path [FileSystem.FileFlagWrite] \_ -> throwIO (userError "file action failure")
                    case outcome :: Either SomeException () of
                        Left _ -> pure ()
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
                    assertSqlError $ FileSystem.withFileHandle conn (path <> "\0suffix") [FileSystem.FileFlagRead] (\_ -> pure ())
        , testCase "catalog and config NUL names fail safely" $
            withConnection ":memory:" \conn -> do
                assertSqlError $ Config.getConfigOption conn "threads\0suffix"
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
                fmap Config.configValueText <$> Config.getConfigOption conn "threads" >>= (@?= Just "1")
                close conn
                case find ((== "threads") . Config.configOptionName) options of
                    Nothing -> assertFailure "missing threads descriptor"
                    Just option -> do
                        Config.configOptionSetting option @?= "1"
                        Config.configOptionName option @?= "threads"
                        assertBool "description is retained" (not (Text.null (Config.configOptionDescription option)))
        , testCase "instance options list global values" $
            withEnvironment \environment -> withInstance environment [("threads", "1")] \instance_ -> do
                options <- Config.listInstanceOptions instance_
                fmap Config.configOptionSetting (find ((== "threads") . Config.configOptionName) options) @?= Just "1"
        , testCase "configuration values resolve case-insensitive names and aliases" $
            withConnection ":memory:" \conn -> do
                Config.setConfigOption conn Config.SettingScopeGlobal "threads" "1"
                forM_ ["threads", "THREADS", "worker_threads", "WORKER_THREADS"] \name ->
                    fmap Config.configValueText <$> Config.getConfigOption conn name >>= (@?= Just "1")
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
                fmap Config.configValueText <$> Config.getConfigOption conn "haskell_limit" >>= (@?= Just "3")
                assertSqlError $ Config.registerCustomOption conn option
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
                sort [(Config.configOptionName descriptor, Config.configOptionScope descriptor, Config.configOptionScopes descriptor) | descriptor <- options, "haskell_" `Text.isPrefixOf` Config.configOptionName descriptor]
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
        , testCase "file paths support expansion glob movement and removal" $
            withTemporaryFile \path -> withConnection ":memory:" \conn -> do
                FileSystem.pathExists conn path >>= (@?= True)
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

-- | Force a setup exception after scalar callbacks have been acquired.
data BrokenSignature = BrokenSignature

instance Function BrokenSignature where
    argumentTypes _ = error "broken scalar signature"
    returnType _ = error "unused return type"
    isVolatile _ = False
    applyFunction _ _ = error "unused function"

-- | Supply an argument type which DuckDB rejects at registration.
data InvalidSignature = InvalidSignature

instance Function InvalidSignature where
    argumentTypes _ = [LogicalTypeIdInvalid]
    returnType _ = returnType (Proxy :: Proxy Int64)
    isVolatile _ = False
    applyFunction _ _ = error "unregistered function"

-- | Register a COPY callback and optionally fail one phase.
registerCopy :: Connection -> Text.Text -> Maybe Int -> IO ()
registerCopy conn name failingPhase =
    Copy.registerCopyToFunction
        conn
        name
        (\_ -> failPhase 0)
        (\info -> (Copy.copyInitBindState info @?= ()) >> failPhase 1)
        (\info _ -> (Copy.copySinkGlobalState info @?= ()) >> failPhase 2)
        (\info -> (Copy.copyFinalizeGlobalState info @?= ()) >> failPhase 3)
  where
    failPhase phase =
        if failingPhase == Just phase
            then throwIO (userError ("copy phase " <> show phase))
            else pure ()

-- | Require a controlled SQL exception.
assertSqlError :: IO a -> Assertion
assertSqlError = assertSqlErrorContaining Text.empty

-- | Require a SQL exception which includes the expected message.
assertSqlErrorContaining :: Text.Text -> IO a -> Assertion
assertSqlErrorContaining expected action = do
    outcome <- try (void action)
    case outcome of
        Left (err :: SQLError) ->
            assertBool ("expected SQL error containing " <> show expected <> ", got " <> show err) $
                expected `Text.isInfixOf` sqlErrorMessage err
        Right () -> assertFailure "expected SQL error"

{- | Convert each value to its native value and back. The native value must have
the pattern name of the value. The native sentinel must raise 'SQLError'.
-}
assertNativeRoundTrip :: (Bounded a, Enum a, Eq a, Show a, Show native) => (a -> native) -> (native -> IO a) -> native -> Assertion
assertNativeRoundTrip toNative fromNative sentinel = do
    let values = [minBound .. maxBound]
    mapM (fromNative . toNative) values >>= (@?= values)
    map (show . toNative) values @?= map (("DuckDBV2" <>) . show) values
    assertSqlErrorContaining "unknown native" (fromNative sentinel)

{- | Check that the native values convert to all constructors in order, and that
each constructor has the pattern name of its native value. The native sentinel
must raise 'SQLError'.
-}
assertFromNative :: (Bounded a, Enum a, Eq a, Show a, Show native) => (native -> IO a) -> [native] -> native -> Assertion
assertFromNative fromNative natives sentinel = do
    values <- mapM fromNative natives
    values @?= [minBound .. maxBound]
    map show natives @?= map (("DuckDBV2" <>) . show) values
    assertSqlErrorContaining "unknown native" (fromNative sentinel)

-- | Remove a temporary file after the action.
withTemporaryFile :: (FilePath -> IO a) -> IO a
withTemporaryFile action =
    bracket
        (do (path, handle) <- openBinaryTempFile "/tmp" "duckdb-extension-regression"; hClose handle; pure path)
        removeFile
        action
