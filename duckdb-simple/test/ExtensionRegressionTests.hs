{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression tests for extension callbacks and native handle ownership.
module ExtensionRegressionTests (main, extensionRegressionTests) where

import Control.Exception (AsyncException (ThreadKilled), ErrorCall, Exception (..), SomeException, bracket, bracket_, finally, throwIO, try)
import Control.Monad (forM_, replicateM_, void, when)
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (find)
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
import qualified Database.DuckDB.Simple.Logging as Logging
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..))
import qualified Database.DuckDB.Simple.Registration as Registration
import qualified Database.DuckDB.Simple.Tokenizer as Tokenizer
import System.Directory (removeFile)
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
        , testCase "log sink callback exceptions cannot fail a query" $
            withConnection ":memory:" \conn -> do
                Logging.registerLogStorage conn "throwing_log" (\_ -> throwIO ThreadKilled)
                void $ execute_ conn "CALL enable_logging(); SET logging_storage = 'throwing_log'; SELECT write_log('contained')"
                query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
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
            map Logging.logEntrySeverity received @?= [DuckDBV2LogLevelWarning]
            map Logging.logEntryScope received @?= [DuckDBV2LogScopeConnection]
            assertBool "entry has a timestamp" (all ((/= Nothing) . Logging.logEntryTimestamp) received)
            assertBool "entry has a connection id" (all ((/= Nothing) . Logging.logEntryConnectionId) received)
            readIORef flushes >>= assertBool "flush and final cleanup ran" . (>= 2)
        , testCase "filesystem action exceptions release handles" $
            withTemporaryFile \path ->
                withConnection ":memory:" \conn -> do
                    outcome <- try $ FileSystem.withFileHandle conn path [DuckDBV2FileFlagWrite] \_ -> throwIO (userError "file action failure")
                    case outcome :: Either SomeException () of
                        Left _ -> pure ()
                        Right () -> assertFailure "expected file action exception"
                    FileSystem.withFileHandle conn path [DuckDBV2FileFlagWrite] \handle -> do
                        FileSystem.writeFileHandleBytes handle (BS.pack [0, 255]) >>= (@?= 2)
                        FileSystem.fileHandleSync handle
                    FileSystem.withFileHandle conn path [DuckDBV2FileFlagRead] \handle -> do
                        FileSystem.readFileHandleChunk handle 0 >>= (@?= BS.empty)
                        FileSystem.readFileHandleChunk handle 8 >>= (@?= BS.pack [0, 255])
        , testCase "filesystem errors remain controlled" $
            withConnection ":memory:" \conn -> do
                assertSqlError $ FileSystem.withFileHandle conn "/tmp/duckdb-review-missing-dir/no-file" [DuckDBV2FileFlagRead] (\_ -> pure ())
                withTemporaryFile \path ->
                    assertSqlError $ FileSystem.withFileHandle conn (path <> "\0suffix") [DuckDBV2FileFlagRead] (\_ -> pure ())
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
                Config.setConfigOption conn DuckDBV2SettingScopeGlobal "threads" "1"
                options <- Config.listConfigOptions conn
                fmap Config.configValueText <$> Config.getConfigOption conn "threads" >>= (@?= Just "1")
                close conn
                case find ((== "threads") . Config.configOptionName) options of
                    Nothing -> assertFailure "missing threads descriptor"
                    Just option -> do
                        Config.configOptionSetting option @?= "1"
                        Config.configOptionName option @?= "threads"
                        assertBool "description is retained" (not (Text.null (Config.configOptionDescription option)))
        , testCase "configuration values resolve case-insensitive names and aliases" $
            withConnection ":memory:" \conn -> do
                Config.setConfigOption conn DuckDBV2SettingScopeGlobal "threads" "1"
                forM_ ["threads", "THREADS", "worker_threads", "WORKER_THREADS"] \name ->
                    fmap Config.configValueText <$> Config.getConfigOption conn name >>= (@?= Just "1")
                Config.getConfigOption conn "NO_SUCH_CONFIG_OPTION" >>= (@?= Nothing)
                close conn
                assertSqlError $ Config.getConfigOption conn "threads"
        , testCase "custom options retain typed defaults scopes and callback errors" $
            withConnection ":memory:" \conn -> do
                let option =
                        (Config.customOption "haskell_limit" (Registration.StructuredType (LogicalTypeScalar DuckDBV2LogicalTypeIdBigInt)))
                            { Config.customOptionDefault = Just (Registration.defaultValue (3 :: Int64))
                            , Config.customOptionDescription = "A Haskell option"
                            , Config.customOptionDefaultScope = DuckDBV2SettingScopeSession
                            , Config.customOptionScopes = [DuckDBV2SettingScopeGlobal, DuckDBV2SettingScopeSession]
                            , Config.customOptionUpdate = Just \info _ ->
                                bracket (Registration.output (c_duckdb_v2_custom_option_update_get_value info)) (Registration.destroy c_duckdb_v2_value_destroy) \value -> do
                                    number <- Registration.output (c_duckdb_v2_value_get_bigint value)
                                    when (number < 0) $ throwIO (userError "limit must be nonnegative")
                            }
                Config.registerCustomOption conn option
                Config.withConfigOptionDefaultValue conn "haskell_limit" \value ->
                    Registration.output (c_duckdb_v2_value_get_bigint value) >>= (@?= 3)
                Config.setConfigValue conn DuckDBV2SettingScopeSession "haskell_limit" (7 :: Int64)
                Config.withConfigOptionValue conn "haskell_limit" \value scope -> do
                    Registration.output (c_duckdb_v2_value_get_bigint value) >>= (@?= 7)
                    scope @?= DuckDBV2SettingScopeSession
                assertSqlErrorContaining "limit must be nonnegative" $ Config.setConfigValue conn DuckDBV2SettingScopeSession "haskell_limit" (-1 :: Int64)
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
                assertBool "comment token" (any ((== DuckDBV2TokenTypeComment) . Tokenizer.tokenType) (Tokenizer.tokenizationTokens parsed))
                unfinished <- Tokenizer.tokenize conn "SELECT 'unclosed"
                Tokenizer.tokenizationUnterminated unfinished @?= True
        , testCase "positioned file operations preserve the sequential position" $
            withTemporaryFile \path -> withConnection ":memory:" \conn ->
                FileSystem.withFileHandle conn path [DuckDBV2FileFlagRead, DuckDBV2FileFlagWrite] \handle -> do
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
                FileSystem.expandPath conn path >>= (@?= path)
                FileSystem.globPaths conn path >>= (@?= [path])
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
    argumentTypes _ = [DuckDBV2LogicalTypeIdInvalid]
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

-- | Remove a temporary file after the action.
withTemporaryFile :: (FilePath -> IO a) -> IO a
withTemporaryFile action =
    bracket
        (do (path, handle) <- openBinaryTempFile "/tmp" "duckdb-extension-regression"; hClose handle; pure path)
        removeFile
        action
