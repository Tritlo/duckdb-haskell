{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression tests for extension callbacks and native handle ownership.
module ExtensionRegressionTests (main, extensionRegressionTests) where

import Control.Exception (AsyncException (ThreadKilled), ErrorCall, Exception (..), SomeException, bracket, throwIO, try)
import Control.Monad (forM_, replicateM_, void)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Word (Word64)
import Database.DuckDB.FFI
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Catalog as Catalog
import qualified Database.DuckDB.Simple.Config as Config
import qualified Database.DuckDB.Simple.Copy as Copy
import qualified Database.DuckDB.Simple.FileSystem as FileSystem
import qualified Database.DuckDB.Simple.Logging as Logging
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
        , testCase "logging exceptions stay inside callback" $
            withConnection ":memory:" \conn -> do
                calls <- newIORef (0 :: Int)
                Logging.registerLogStorage conn "throwing_log" \_ -> do
                    modifyIORef' calls (+ 1)
                    throwIO (userError "log callback failure")
                void $ execute_ conn "SET logging_storage = 'throwing_log'"
                void $ execute_ conn "SET enable_logging = true"
                void $ execute_ conn "SELECT write_log('callback regression', level := 'INFO', scope := 'connection')"
                readIORef calls >>= \n -> assertBool "log callback ran" (n > 0)
                query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
        , testCase "logging messages preserve UTF8" $
            withConnection ":memory:" \conn -> do
                messages <- newIORef []
                Logging.registerLogStorage conn "unicode_log" \entry ->
                    modifyIORef' messages (Logging.logEntryMessage entry :)
                void $ execute_ conn "SET logging_storage = 'unicode_log'"
                void $ execute_ conn "SET enable_logging = true"
                void $ execute_ conn "SELECT write_log('\955', level := 'INFO', scope := 'connection')"
                readIORef messages >>= \entries -> assertBool "Unicode log message" ("\955" `elem` entries)
        , testCase "filesystem action exceptions release handles" $
            withTemporaryFile \path ->
                withConnection ":memory:" \conn -> do
                    outcome <- try $ FileSystem.withFileHandle conn path [DuckDBFileFlagWrite] \_ -> throwIO (userError "file action failure")
                    case outcome :: Either SomeException () of
                        Left _ -> pure ()
                        Right () -> assertFailure "expected file action exception"
                    FileSystem.withFileHandle conn path [DuckDBFileFlagWrite] \handle -> do
                        FileSystem.writeFileHandleBytes handle (BS.pack [0, 255]) >>= (@?= 2)
                        FileSystem.fileHandleSync handle
                    FileSystem.withFileHandle conn path [DuckDBFileFlagRead] \handle -> do
                        FileSystem.readFileHandleChunk handle 0 >>= (@?= BS.empty)
                        FileSystem.readFileHandleChunk handle 8 >>= (@?= BS.pack [0, 255])
        , testCase "filesystem errors remain controlled" $
            withConnection ":memory:" \conn -> do
                assertSqlError $ FileSystem.withFileHandle conn "/tmp/duckdb-review-missing-dir/no-file" [DuckDBFileFlagRead] (\_ -> pure ())
                withTemporaryFile \path ->
                    assertSqlError $ FileSystem.withFileHandle conn (path <> "\0suffix") [DuckDBFileFlagRead] (\_ -> pure ())
        , testCase "catalog unsupported kinds fail safely" $
            withConnection ":memory:" \conn ->
                withTransaction conn $
                    mapM_
                        (\kind -> assertSqlError $ Catalog.lookupCatalogEntry conn "memory" "main" "probe" kind)
                        [DuckDBCatalogEntryTypeInvalid, DuckDBCatalogEntryTypeSchema, DuckDBCatalogEntryTypePreparedStatement, DuckDBCatalogEntryTypeDatabase, DuckDBCatalogEntryType 99]
        , testCase "catalog and config NUL names fail safely" $
            withConnection ":memory:" \conn -> do
                assertSqlError $ Config.getConfigOption conn "threads\0suffix"
                withTransaction conn do
                    assertSqlError $ Catalog.catalogTypeName conn "memory\0suffix"
                    assertSqlError $ Catalog.lookupCatalogEntry conn "memory" "main\0suffix" "probe" DuckDBCatalogEntryTypeTable
        , testCase "catalog names preserve UTF8" $
            withConnection ":memory:" \conn -> do
                void $ execute_ conn "CREATE TABLE \"\955\" (i INT)"
                withTransaction conn do
                    entry <- Catalog.lookupCatalogEntry conn "memory" "main" "\955" DuckDBCatalogEntryTypeTable
                    fmap Catalog.catalogEntryName entry @?= Just "\955"
        , testCase "catalog and config missing values remain safe" $
            withConnection ":memory:" \conn -> do
                Config.getConfigOption conn "no_such_config_option" >>= (@?= Nothing)
                withTransaction conn do
                    Catalog.catalogTypeName conn "no_such_catalog" >>= (@?= Nothing)
                    Catalog.lookupCatalogEntry conn "memory" "main" "no_such_table" DuckDBCatalogEntryTypeTable >>= (@?= Nothing)
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
    argumentTypes _ = [DuckDBTypeInvalid]
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
