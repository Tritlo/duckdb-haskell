{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Check a packaged Haskell extension with an explicitly selected DuckDB CLI.
module Main where

import Control.Concurrent (MVar, ThreadId, forkFinally, killThread, newEmptyMVar, putMVar, readMVar, tryReadMVar)
import Control.Exception (IOException, SomeException, bracket, catch, throwIO)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate)
import System.Console.GetOpt
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.IO (Handle, hClose, hPutStr, hPutStrLn, hSetEncoding, stderr, utf8)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
import System.Timeout (timeout)

-- | The CLI executable is explicit. Each run uses a fresh native process.
data Flag = DuckDB FilePath | Help deriving (Eq, Show)

-- | Accepted options match the original smoke CLI.
options :: [OptDescr Flag]
options =
    [ Option [] ["duckdb"] (ReqArg DuckDB "DUCKDB") "explicit path to the matching DuckDB CLI"
    , Option ['h'] ["help"] (NoArg Help) "show this help"
    ]

-- | Usage text for the CLI and packaged extension paths.
usage :: String
usage = usageInfo "Usage: runghc dev/extensions/Smoke.hs --duckdb DUCKDB EXTENSION" options

-- | Quote a path as a SQL string without changing its contents.
sqlLiteral :: String -> IO String
sqlLiteral value = do
    when ('\0' `elem` value) $ ioError (userError "extension path must not contain NUL")
    pure ("'" <> concatMap (\character -> if character == '\'' then "''" else [character]) value <> "'")

-- | Limit captured output in a failure report.
shortOutput :: BS.ByteString -> BS.ByteString
shortOutput bytes
    | BS.null bytes = "[empty]"
    | BS.length bytes > 1200 = BS.take 1200 bytes <> "\n[truncated]"
    | otherwise = bytes

-- | Drain one pipe concurrently so that neither output pipe blocks the child.
startReader :: Handle -> IO (ThreadId, MVar (Either SomeException BS.ByteString))
startReader handle = do
    result <- newEmptyMVar
    thread <- forkFinally (BS.hGetContents handle) (putMVar result)
    pure (thread, result)

-- | Obtain copied output after the reader finishes.
readerResult :: MVar (Either SomeException BS.ByteString) -> IO BS.ByteString
readerResult result = readMVar result >>= either throwIO pure

-- | Kill only the timed-out child. A concurrent child exit is harmless.
killChild :: ProcessHandle -> IO ()
killChild child = do
    processId <- getPid child
    case processId of
        Nothing -> pure ()
        Just pid ->
            signalProcess sigKILL pid `catch` \exception ->
                unless (isDoesNotExistError exception) (throwIO (exception :: IOException))

-- | Run for at most 60 seconds, then kill the child and retain its diagnostics.
runCLI :: FilePath -> String -> IO (Bool, ExitCode, BS.ByteString, BS.ByteString)
runCLI executable sql =
    withCreateProcess configuration $ \input output errors child -> case (input, output, errors) of
        (Just stdinHandle, Just stdoutHandle, Just stderrHandle) ->
            bracket (startReader stdoutHandle) (killThread . fst) $ \(_, stdoutResult) ->
                bracket (startReader stderrHandle) (killThread . fst) $ \(_, stderrResult) -> do
                    result <- timeout (60 * 1000000) $ do
                        hSetEncoding stdinHandle utf8
                        hPutStr stdinHandle sql
                        hClose stdinHandle
                        status <- waitForProcess child
                        stdoutBytes <- readerResult stdoutResult
                        stderrBytes <- readerResult stderrResult
                        pure (status, stdoutBytes, stderrBytes)
                    case result of
                        Just (status, stdoutBytes, stderrBytes) -> pure (False, status, stdoutBytes, stderrBytes)
                        Nothing -> do
                            killChild child
                            status <- waitForProcess child
                            stdoutBytes <- maybe BS.empty (either (const BS.empty) id) <$> tryReadMVar stdoutResult
                            stderrBytes <- maybe BS.empty (either (const BS.empty) id) <$> tryReadMVar stderrResult
                            pure (True, status, stdoutBytes, stderrBytes)
        _ -> ioError (userError "could not create the DuckDB CLI pipes")
  where
    -- STANDARD stdin continues after errors. Skip user startup configuration.
    configuration =
        (proc executable ["-unsigned", "-csv", "-noheader", "-no-init", "-batch", ":memory:"])
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }

-- | Check exact CSV rows and the expected SQL error diagnostics.
checkProcess :: FilePath -> FilePath -> String -> [String] -> [[String]] -> [String] -> IO ()
checkProcess executable extension label statements expectedRows expectedErrors = do
    path <- sqlLiteral extension
    let sql = unlines ([".bail off", "LOAD " <> path <> ";"] <> statements)
    (timedOut, status, stdoutBytes, stderrBytes) <- runCLI executable sql
    let expectedStatus = if null expectedErrors then ExitSuccess else ExitFailure 1
        actualLines = map (BS8.dropWhileEnd (== '\r')) (BS8.lines stdoutBytes)
        expectedLines = map (BS8.pack . intercalate ",") expectedRows
        problems =
            ["DuckDB CLI exceeded 60 seconds" | timedOut]
                <> ["exit status " <> show status <> ", expected " <> show expectedStatus | status /= expectedStatus]
                <> ["CSV results differ from the expected rows" | actualLines /= expectedLines]
                <> ["missing diagnostic: " <> diagnostic | diagnostic <- expectedErrors, not (BS8.pack diagnostic `BS.isInfixOf` stderrBytes)]
    unless (null problems) $ do
        hPutStrLn stderr ("smoke: " <> label <> ": " <> intercalate "; " problems)
        BS.hPut stderr ("stdout:\n" <> shortOutput stdoutBytes <> "\nstderr:\n" <> shortOutput stderrBytes <> "\n")
        exitFailure
    putStrLn ("PASS: " <> label)

-- | Check native batches and recovery in two independently loaded processes.
smoke :: FilePath -> FilePath -> IO ()
smoke executable extension = do
    cli <- canonicalizePath executable
    library <- canonicalizePath extension
    cliExists <- doesFileExist cli
    libraryExists <- doesFileExist library
    unless (cliExists && libraryExists) $ ioError (userError "DuckDB CLI and extension paths must identify files")
    quoted <- sqlLiteral library
    checkProcess
        cli
        library
        "load, repeated load, native batches, NULL, and extension registration"
        [ "SELECT 'loaded', haskell_answer();"
        , "LOAD " <> quoted <> ";"
        , "SELECT 'reloaded', haskell_answer();"
        , "SELECT 'native_batches', count(*), min(value), max(value), sum(value) FROM (SELECT haskell_add_one(i) AS value FROM range(10000) AS t(i));"
        , "SELECT 'null', CASE WHEN haskell_add_one(NULL::BIGINT) IS NULL THEN 'preserved' ELSE 'changed' END;"
        , "SELECT 'text', haskell_greet('Matti λ') = 'Hello Matti λ';"
        , "SELECT 'macro', haskell_twice(21::BIGINT);"
        , "SELECT 'table_macro', answer FROM haskell_table();"
        , "PRAGMA haskell_demo;"
        , "SET enable_logging=true;"
        , "SET logging_storage='haskell_sink';"
        , "SELECT 'log_sink', write_log('Haskell extension smoke', return_value := 42);"
        , "SET logging_storage='memory';"
        , "SET enable_logging=false;"
        , "SET haskell_multiplier=9;"
        , "PRAGMA haskell_demo;"
        ]
        [ ["loaded", "42"]
        , ["reloaded", "42"]
        , ["native_batches", "10000", "1", "10000", "50005000"]
        , ["null", "preserved"]
        , ["text", "true"]
        , ["macro", "42"]
        , ["table_macro", "42"]
        , ["pragma", "7"]
        , ["log_sink", "42"]
        , ["pragma", "9"]
        ]
        []
    checkProcess
        cli
        library
        "fresh runtime startup, callback errors, overflow, and recovery"
        [ "SELECT 'fresh_process', haskell_answer();"
        , "SELECT haskell_fail();"
        , "SELECT 'after_callback_error', 42;"
        , "SELECT haskell_add_one(9223372036854775807::BIGINT);"
        , "SELECT 'after_overflow_error', 42;"
        ]
        [["fresh_process", "42"], ["after_callback_error", "42"], ["after_overflow_error", "42"]]
        ["Haskell callback failure", "Haskell BIGINT overflow"]

-- | Read explicit paths and report failures without a traceback.
main :: IO ()
main = do
    arguments <- getArgs
    let (flags, positional, errors) = getOpt Permute options arguments
        failure message = hPutStrLn stderr ("smoke: " <> message <> "\n" <> usage) >> exitFailure
    when (Help `elem` flags) $ putStr usage >> exitSuccess
    unless (null errors) $ failure (concat errors)
    executable <- case [path | DuckDB path <- flags] of
        [path] -> pure path
        [] -> failure "missing --duckdb"
        _ -> failure "repeated --duckdb"
    extension <- case positional of
        [path] -> pure path
        _ -> failure "supply one packaged extension path"
    smoke executable extension `catch` \exception -> failure (ioeGetErrorString (exception :: IOException))
