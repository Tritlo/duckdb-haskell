{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Main
-- Description : Load the packaged Haskell demo extension into DuckDB CLI processes and check the SQL results.
--
-- Each case starts a fresh CLI process.
module Main (main) where

import Control.Concurrent (MVar, ThreadId, forkFinally, killThread, newEmptyMVar, putMVar, readMVar, tryReadMVar)
import Control.Exception (IOException, SomeException, bracket, catch, throwIO)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate)
import System.Directory (canonicalizePath, createDirectory, doesFileExist, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (Handle, hClose, hPutStr, hSetEncoding, openTempFile, utf8)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
import System.Timeout (timeout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

-- | The DuckDB CLI and the packaged extension under test.
data Paths = Paths FilePath FilePath

-- | Read both paths from the environment. A missing path fails the suite.
main :: IO ()
main = do
  cli <- existingFile "DUCKDB_CLI"
  extension <- existingFile "DUCKDB_HASKELL_DEMO_EXTENSION"
  defaultMain (tests (Paths cli extension))

-- | Fail when a variable is absent or does not name an existing file.
existingFile :: String -> IO FilePath
existingFile name = do
  configured <- lookupEnv name
  path <- case configured of
    Just value | not (null value) -> pure value
    _ -> fail (name <> " must name a local file")
  exists <- doesFileExist path
  unless exists $ fail (name <> " does not name an existing file: " <> path)
  canonicalizePath path

-- | One case for each group of registrations, and the load and error cases.
tests :: Paths -> TestTree
tests paths =
  testGroup
    "Haskell demo extension"
    [ testCase "a second LOAD in the same database instance" $ do
        loadAgain <- loadStatement paths
        checkProcess
          paths
          [ "SELECT 'loaded', haskell_answer();"
          , loadAgain
          , "SELECT 'after_second_load', haskell_answer();"
          , "SELECT 'loaded_once', loaded FROM duckdb_extensions() WHERE extension_name = 'haskell_demo';"
          , "PRAGMA haskell_demo;"
          , "SELECT 'replacement', amount FROM haskell_missing;"
          ]
          [["loaded", "42"], ["after_second_load", "42"], ["loaded_once", "true"], ["pragma", "7"], ["replacement", "42"]]
          []
    , testCase "LOAD in a new database instance reuses the initialized runtime" $ do
        loadAgain <- loadStatement paths
        checkProcess
          paths
          [ "SELECT 'first_instance', haskell_add_one(41::BIGINT);"
          , "SELECT 'first_metadata', haskell_argument_type(1::INTEGER);"
          , "SELECT 'first_alias', haskell_canonical_argument_type(haskell_canonical_bigint(1::INTEGER));"
          , "SELECT 'first_result', haskell_centroid(1, 2)::BLOB = 'POINT (1 2)'::GEOMETRY::BLOB;"
          , ".open :memory:"
          , loadAgain
          , "SELECT 'second_instance', haskell_add_one(41::BIGINT);"
          , "SELECT 'second_aggregate', haskell_sum(i) FROM range(10000) AS t(i);"
          , "SELECT 'second_result', haskell_centroid(3, 4)::BLOB = 'POINT (3 4)'::GEOMETRY::BLOB;"
          , "SELECT 'second_metadata', haskell_argument_type(1::INTEGER);"
          , "SELECT 'second_alias', haskell_canonical_argument_type(haskell_canonical_bigint(1::INTEGER));"
          , "SELECT 'second_replacement', amount FROM haskell_missing;"
          ]
          [ ["first_instance", "42"]
          , ["first_metadata", "1"]
          , ["first_alias", "1"]
          , ["first_result", "true"]
          , ["second_instance", "42"]
          , ["second_aggregate", "49995000"]
          , ["second_result", "true"]
          , ["second_metadata", "1"]
          , ["second_alias", "1"]
          , ["second_replacement", "42"]
          ]
          []
    , testCase "scalar functions: native batches, NULL, and text" $
        checkProcess
          paths
          [ "SELECT 'native_batches', count(*), min(value), max(value), sum(value) FROM (SELECT haskell_add_one(i) AS value FROM range(10000) AS t(i));"
          , "SELECT 'null', CASE WHEN haskell_add_one(NULL::BIGINT) IS NULL THEN 'preserved' ELSE 'changed' END;"
          , "SELECT 'text', haskell_greet('Matti λ') = 'Hello Matti λ';"
          ]
          [["native_batches", "10000", "1", "10000", "50005000"], ["null", "preserved"], ["text", "true"]]
          []
    , testCase "VARIANT arguments in scalar and aggregate callbacks" $
        checkProcess
          paths
          [ "SET threads=4;"
          , "SELECT 'object', haskell_variant_echo(v) = v FROM (SELECT '{\"\":[3, null], \"nested\":{\"x\":true}}'::JSON::VARIANT AS v);"
          , "SELECT 'sum', haskell_variant_sum(i::VARIANT) FROM range(4097) AS t(i);"
          , "SELECT haskell_variant_sum('text'::VARIANT);"
          , "SELECT 'recovered', haskell_variant_sum(v) FROM (VALUES (7::BIGINT::VARIANT), (NULL::VARIANT)) AS t(v);"
          ]
          [["object", "true"], ["sum", "8390656"], ["recovered", "7"]]
          ["haskell_variant_sum expects BIGINT payloads"]
    , testCase "centroid returns native GEOMETRY values and NULL" $
        checkProcess
          paths
          [ "SELECT 'type', return_type FROM duckdb_functions() WHERE function_name = 'haskell_centroid';"
          , "SELECT 'groups', g, haskell_centroid(x, y)::BLOB IS NOT DISTINCT FROM CASE WHEN g = 1 THEN 'POINT (2 3)'::GEOMETRY::BLOB END FROM (VALUES (1, 1, 2), (1, 3, 4), (1, NULL, 9), (2, NULL, 1)) AS t(g, x, y) GROUP BY g ORDER BY g;"
          , "SELECT 'empty', haskell_centroid(i, i) IS NULL FROM range(0) AS t(i);"
          ]
          [["type", "GEOMETRY"], ["groups", "1", "true"], ["groups", "2", "true"], ["empty", "true"]]
          []
    , testCase "a GEOMETRY argument" $
        checkProcess
          paths
          [ "SELECT 'point_x', haskell_point_x('POINT (1 2)'::GEOMETRY);"
          , "SELECT 'null_x', haskell_point_x(NULL::GEOMETRY) IS NULL;"
          , "SELECT 'rows_x', count(*), sum(haskell_point_x(('POINT (' || i || ' 0)')::GEOMETRY)) FROM range(5000) AS t(i);"
          ]
          [["point_x", "1.0"], ["null_x", "true"], ["rows_x", "5000", "12497500.0"]]
          []
    , testCase "GEOMETRY aggregate arguments, nested values, NULLs, and chunks" $
        checkProcess
          paths
          [ "SET threads=4;"
          , "SELECT 'point_sum', haskell_point_x_sum(g) FROM (VALUES ('POINT (1 2)'::GEOMETRY), (NULL::GEOMETRY), ('POINT (3 4)'::GEOMETRY)) AS t(g);"
          , "SELECT 'null_sum', haskell_point_x_sum(NULL::GEOMETRY);"
          , "SELECT 'nested_sum', haskell_points_x_sum(g) FROM (VALUES (['POINT (1 2)'::GEOMETRY, NULL::GEOMETRY, 'POINT (3 4)'::GEOMETRY]), (NULL::GEOMETRY[])) AS t(g);"
          , "SELECT 'rows_sum', haskell_point_x_sum(('POINT (' || i || ' 0)')::GEOMETRY) FROM range(5000) AS t(i);"
          , "SELECT 'nested_rows_sum', haskell_points_x_sum([('POINT (' || i || ' 0)')::GEOMETRY, NULL::GEOMETRY]) FROM range(5000) AS t(i);"
          ]
          [["point_sum", "4.0"], ["null_sum", "0.0"], ["nested_sum", "4.0"], ["rows_sum", "12497500.0"], ["nested_rows_sum", "12497500.0"]]
          []
    , testCase "a GEOMETRY argument does not run a registered cast from GEOMETRY to BLOB" $
        -- The variable makes the extension register a native cast from GEOMETRY to
        -- BLOB that writes NULL and counts its calls (src/TestCast.hs). The reads
        -- come first, so the count after them must be 0. The CAST after them shows
        -- that the registered cast runs.
        checkProcessWith
          [("DUCKDB_HASKELL_DEMO_GEOMETRY_CAST", "1")]
          paths
          [ "SELECT 'point_x', haskell_point_x('POINT (1 2)'::GEOMETRY);"
          , "SELECT 'null_x', haskell_point_x(NULL::GEOMETRY) IS NULL;"
          , "SELECT 'rows_x', count(*), sum(haskell_point_x(('POINT (' || i || ' 0)')::GEOMETRY)) FROM range(5000) AS t(i);"
          , "SELECT 'aggregate_sum', haskell_point_x_sum(g) FROM (VALUES ('POINT (1 2)'::GEOMETRY), (NULL::GEOMETRY)) AS t(g);"
          , "SELECT 'aggregate_chunks', haskell_point_x_sum(('POINT (' || i || ' 0)')::GEOMETRY) FROM range(5000) AS t(i);"
          , "SELECT 'nested_aggregate_chunks', haskell_points_x_sum([('POINT (' || i || ' 0)')::GEOMETRY, NULL::GEOMETRY]) FROM range(5000) AS t(i);"
          , "SELECT 'calls_after_reads', haskell_geometry_cast_calls();"
          , "SELECT 'cast_blob', CAST('POINT (1 2)'::GEOMETRY AS BLOB) IS NULL;"
          , "SELECT 'calls_after_cast', haskell_geometry_cast_calls() > 0;"
          ]
          [ ["point_x", "1.0"]
          , ["null_x", "true"]
          , ["rows_x", "5000", "12497500.0"]
          , ["aggregate_sum", "1.0"]
          , ["aggregate_chunks", "12497500.0"]
          , ["nested_aggregate_chunks", "12497500.0"]
          , ["calls_after_reads", "0"]
          , ["cast_blob", "true"]
          , ["calls_after_cast", "true"]
          ]
          []
    , testCase "scalar and table macros" $
        checkProcess
          paths
          [ "SELECT 'macro', haskell_twice(21::BIGINT);"
          , "SELECT 'table_macro', answer FROM haskell_table();"
          ]
          [["macro", "42"], ["table_macro", "42"]]
          []
    , testCase "PRAGMA before and after a custom option update" $
        checkProcess
          paths
          [ "PRAGMA haskell_demo;"
          , "SET haskell_multiplier=9;"
          , "SELECT 'option', current_setting('haskell_multiplier');"
          , "PRAGMA haskell_demo;"
          ]
          [["pragma", "7"], ["option", "9"], ["pragma", "9"]]
          []
    , testCase "log sink" $
        checkProcess
          paths
          [ "SET enable_logging=true;"
          , "SET logging_sink='haskell_sink';"
          , "SELECT 'log_sink', write_log('Haskell extension test', level := 'warning', return_value := 42);"
          , "SET logging_sink='memory';"
          , "SET enable_logging=false;"
          , "SELECT 'log_entries', haskell_log_entries() > 0;"
          ]
          [["log_sink", "42"], ["log_entries", "true"]]
          []
    , testCase "aggregate function over groups, NULLs, and many rows" $
        checkProcess
          paths
          [ "SELECT 'groups', g, haskell_sum(v) FROM (VALUES (1, 10), (1, NULL), (2, 5), (2, 7), (3, NULL)) AS t(g, v) GROUP BY g ORDER BY g;"
          , "SELECT 'rows', haskell_sum(i) FROM range(10000) AS t(i);"
          , "SELECT 'no_rows', haskell_sum(i) FROM range(0) AS t(i);"
          ]
          [["groups", "1", "10"], ["groups", "2", "12"], ["groups", "3", "0"], ["rows", "49995000"], ["no_rows", "0"]]
          []
    , testCase "aggregate argument metadata preserves casts and canonical aliases" $
        checkProcess
          paths
          [ "SELECT 'base', haskell_argument_type(1::INTEGER);"
          , "SELECT 'alias', haskell_canonical_argument_type(haskell_canonical_bigint(1::INTEGER));"
          , "SELECT 'fresh', haskell_canonical_argument_type(haskell_canonical_bigint(2::INTEGER));"
          , "PREPARE metadata_reuse AS SELECT 'prepared', haskell_canonical_argument_type(haskell_canonical_bigint(i)) FROM range(4097) AS t(i);"
          , "EXECUTE metadata_reuse;"
          , "EXECUTE metadata_reuse;"
          , "DEALLOCATE metadata_reuse;"
          , "SELECT 'chunks', haskell_canonical_argument_type(haskell_canonical_bigint(i)) FROM range(10000) AS t(i);"
          , "SELECT haskell_canonical_argument_type(haskell_canonical_bigint(value)) FROM (VALUES (9223372036854775807::BIGINT), (1::BIGINT)) AS t(value);"
          , "SELECT 'recovered', haskell_canonical_argument_type(haskell_canonical_bigint(7));"
          , "SET threads=4;"
          , "SELECT 'call_sites', haskell_canonical_argument_type(haskell_canonical_bigint(i)), haskell_canonical_argument_type(haskell_canonical_bigint(i+1)) FROM range(100000) AS t(i);"
          ]
          [ ["base", "1"]
          , ["alias", "1"]
          , ["fresh", "2"]
          , ["prepared", "8390656"]
          , ["prepared", "8390656"]
          , ["chunks", "49995000"]
          , ["recovered", "7"]
          , ["call_sites", "4999950000", "5000050000"]
          ]
          ["haskell_canonical_argument_type: BIGINT overflow"]
    , testCase "table function" $
        checkProcess
          paths
          [ "SELECT 'square', square FROM haskell_squares(4);"
          , "SELECT 'reordered', square, n FROM haskell_squares(3);"
          , "SELECT 'chunks', count(*), sum(square), max(n) FROM haskell_squares(3000);"
          ]
          ( [["square", show (n * n)] | n <- [0 .. 3 :: Int]]
              <> [["reordered", show (n * n), show n] | n <- [0 .. 2 :: Int]]
              <> [["chunks", "3000", "8995500500", "2999"]]
          )
          []
    , testCase "column of the custom type" $
        checkProcess
          paths
          [ "CREATE TABLE amounts (value haskell_amount);"
          , "INSERT INTO amounts VALUES (1::BIGINT), (NULL);"
          , "INSERT INTO amounts SELECT 2::BIGINT;"
          , "SELECT 'column', typeof(value), value FROM amounts;"
          ]
          [["column", "haskell_amount", "1"], ["column", "haskell_amount", "NULL"], ["column", "haskell_amount", "2"]]
          []
    , testCase "replacement scan" $
        checkProcess
          paths
          [ "SELECT 'replacement', amount FROM haskell_missing;"
          , "SELECT * FROM haskell_absent;"
          , "SELECT 'after_unknown_table', 42;"
          ]
          [["replacement", "42"], ["after_unknown_table", "42"]]
          ["Table with name haskell_absent does not exist"]
    , testCase "COPY TO and COPY FROM" $ withDirectory $ \directory -> do
        let file = directory </> "numbers.lines"
        quoted <- sqlLiteral file
        checkProcess
          paths
          [ "COPY (SELECT (CASE WHEN i % 1000 = 7 THEN NULL ELSE i END)::VARIANT AS value FROM range(3000) AS t(i)) TO "
              <> quoted
              <> " (FORMAT haskell_lines);"
          , "CREATE TABLE copied (value BIGINT);"
          , "COPY copied FROM " <> quoted <> " (FORMAT haskell_lines);"
          , "SELECT 'copy_from', count(*), count(value), sum(value) FROM copied;"
          , "COPY (SELECT 'text' AS value) TO " <> quoted <> " (FORMAT haskell_lines);"
          , "SELECT 'after_copy_error', 42;"
          ]
          [["copy_from", "3000", "2997", "4495479"], ["after_copy_error", "42"]]
          ["haskell_lines writes one BIGINT or VARIANT column with BIGINT or NULL values"]
        written <- readFile file
        written @?= unlines [if i `mod` 1000 == 7 then "" else show i | i <- [0 .. 2999 :: Int]]
    , testGroup
        "file readers reject BIGINT overflow"
        [ testCase badValue $ withDirectory $ \directory -> do
            writeFile (directory </> "bounds.lines") "-9223372036854775808\n9223372036854775807\n"
            writeFile (directory </> "overflow.lines") (badValue <> "\n")
            bounds <- sqlLiteral (directory </> "bounds.lines")
            overflow <- sqlLiteral (directory </> "overflow.lines")
            checkProcess
              paths
              [ "CREATE TABLE copied (value BIGINT);"
              , "COPY copied FROM " <> bounds <> " (FORMAT haskell_lines);"
              , "SELECT 'bounds', min(value), max(value) FROM copied;"
              , "COPY copied FROM " <> overflow <> " (FORMAT haskell_lines);"
              , "SELECT 'after_copy_overflow', count(*) FROM copied;"
              , "SELECT count(*) FROM haskell_read_line_file(" <> overflow <> ");"
              , "SELECT 'after_single_overflow', 42;"
              , "SELECT sum(value) FROM haskell_read_lines([" <> bounds <> ", " <> overflow <> "]);"
              , "SELECT 'after_multi_overflow', 42;"
              ]
              [ ["bounds", "-9223372036854775808", "9223372036854775807"]
              , ["after_copy_overflow", "2"]
              , ["after_single_overflow", "42"]
              , ["after_multi_overflow", "42"]
              ]
              ["haskell_lines: the line is not a BIGINT: " <> badValue]
        | badValue <- ["9223372036854775808", "-9223372036854775809"]
        ]
    , testCase "multi-file function" $ withDirectory $ \directory -> do
        writeFile (directory </> "first.lines") "7\n"
        writeFile (directory </> "second.lines") "11\n\n"
        writeFile (directory </> "other.txt") "99\n"
        first <- sqlLiteral (directory </> "first.lines")
        second <- sqlLiteral (directory </> "second.lines")
        glob <- sqlLiteral (directory </> "*.lines")
        checkProcess
          paths
          [ "SELECT 'single', value FROM haskell_read_line_file(" <> second <> ");"
          , "SELECT 'glob', value, parse_filename(filename) FROM haskell_read_lines(" <> glob <> ", filename = true) ORDER BY value NULLS LAST;"
          , "SELECT 'list', value FROM haskell_read_lines([" <> first <> ", " <> second <> "]) ORDER BY value NULLS LAST;"
          ]
          [ ["single", "11"]
          , ["single", "NULL"]
          , ["glob", "7", "first.lines"]
          , ["glob", "11", "second.lines"]
          , ["glob", "NULL", "second.lines"]
          , ["list", "7"]
          , ["list", "11"]
          , ["list", "NULL"]
          ]
          []
    , testCase "callback errors, overflow, and recovery" $
        checkProcess
          paths
          [ "SELECT 'fresh_process', haskell_answer();"
          , "SELECT haskell_fail();"
          , "SELECT 'after_callback_error', 42;"
          , "SELECT haskell_add_one(9223372036854775807::BIGINT);"
          , "SELECT 'after_overflow_error', 42;"
          ]
          [["fresh_process", "42"], ["after_callback_error", "42"], ["after_overflow_error", "42"]]
          ["Haskell callback failure", "Haskell BIGINT overflow"]
    , testCase "callback errors in the aggregate, table function, and replacement scan" $
        checkProcess
          paths
          [ "SELECT haskell_sum(v) FROM (VALUES (9223372036854775807), (1)) AS t(v);"
          , "SELECT 'after_update_error', 42;"
          , -- The frame of row 0 has 32 rows. DuckDB adds rows 0 to 15 and rows 16 to 31
            -- in separate update calls. Then the combine callback adds the two sums. No row has
            -- the value 2, so only the combine callback can report the second overflow.
            "SELECT haskell_sum(v) OVER (ORDER BY i ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) FROM (SELECT i, CASE WHEN i = 0 THEN 9223372036854775807 WHEN i IN (16, 17) THEN 1 ELSE 0 END AS v FROM range(32) AS t(i));"
          , "SELECT 'after_combine_error', 42;"
          , "SELECT * FROM haskell_squares(-1);"
          , "SELECT 'after_bind_error', 42;"
          , "SELECT * FROM haskell_reserved;"
          , "SELECT 'after_replacement_error', 42;"
          ]
          [ ["after_update_error", "42"]
          , ["after_combine_error", "42"]
          , ["after_bind_error", "42"]
          , ["after_replacement_error", "42"]
          ]
          [ "haskell_sum: BIGINT overflow in 9223372036854775807 + 1"
          , "haskell_sum: BIGINT overflow in 9223372036854775807 + 2"
          , "haskell_squares: the count is negative: -1"
          , "haskell_reserved: the table name is reserved"
          ]
    , testCase "callback errors for files with a bad line" $ withDirectory $ \directory -> do
        writeFile (directory </> "good.lines") "7\n"
        writeFile (directory </> "copy.lines") "1\ncopy_from\n"
        writeFile (directory </> "single.lines") "2\nsingle_file\n"
        writeFile (directory </> "first.lines") "first_file\n"
        writeFile (directory </> "later.lines") "3\nlater_file\n"
        good <- sqlLiteral (directory </> "good.lines")
        copy <- sqlLiteral (directory </> "copy.lines")
        single <- sqlLiteral (directory </> "single.lines")
        first <- sqlLiteral (directory </> "first.lines")
        later <- sqlLiteral (directory </> "later.lines")
        -- An aggregate gives no row before the scan ends, so a failed scan gives no row.
        checkProcess
          paths
          [ "CREATE TABLE copied (value BIGINT);"
          , "COPY copied FROM " <> copy <> " (FORMAT haskell_lines);"
          , "SELECT 'after_copy_from_error', count(*) FROM copied;"
          , "SELECT sum(value) FROM haskell_read_line_file(" <> single <> ");"
          , "SELECT 'after_single_file_error', 42;"
          , -- DuckDB binds the first file before the scan.
            "SELECT sum(value) FROM haskell_read_lines([" <> first <> ", " <> good <> "]);"
          , "SELECT 'after_first_file_error', 42;"
          , -- DuckDB opens a later file during the scan.
            "SELECT sum(value) FROM haskell_read_lines([" <> good <> ", " <> later <> "]);"
          , "SELECT 'after_later_file_error', 42;"
          ]
          [["after_copy_from_error", "0"], ["after_single_file_error", "42"], ["after_first_file_error", "42"], ["after_later_file_error", "42"]]
          [ "haskell_lines: the line is not a BIGINT: copy_from"
          , "haskell_lines: the line is not a BIGINT: single_file"
          , "haskell_lines: the line is not a BIGINT: first_file"
          , "haskell_lines: the line is not a BIGINT: later_file"
          ]
    ]

-- | Quote a path as a SQL string without changing its contents.
sqlLiteral :: String -> IO String
sqlLiteral value = do
  when ('\0' `elem` value) $ ioError (userError "a path must not contain NUL")
  pure ("'" <> concatMap (\character -> if character == '\'' then "''" else [character]) value <> "'")

-- | The statement that loads the extension under test.
loadStatement :: Paths -> IO String
loadStatement (Paths _ extension) = do
  path <- sqlLiteral extension
  pure ("LOAD " <> path <> ";")

-- | Create and remove a unique directory for the files of one case.
withDirectory :: (FilePath -> IO a) -> IO a
withDirectory = bracket create removePathForcibly
  where
    create = do
      root <- getTemporaryDirectory
      (path, handle) <- openTempFile root "duckdb-haskell-demo"
      hClose handle
      removeFile path
      createDirectory path
      pure path

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
runCLI :: [(String, String)] -> FilePath -> String -> IO (Bool, ExitCode, BS.ByteString, BS.ByteString)
runCLI extraEnvironment executable sql = do
  inherited <- getEnvironment
  let environment = if null extraEnvironment then Nothing else Just (extraEnvironment <> inherited)
  withCreateProcess configuration{env = environment} $ \input output errors child -> case (input, output, errors) of
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

-- | Load the extension, run the statements, and check the exact CSV rows and
-- the expected SQL error diagnostics.
checkProcess :: Paths -> [String] -> [[String]] -> [String] -> Assertion
checkProcess = checkProcessWith []

-- | 'checkProcess' with more environment variables for the DuckDB CLI.
checkProcessWith :: [(String, String)] -> Paths -> [String] -> [[String]] -> [String] -> Assertion
checkProcessWith extraEnvironment paths@(Paths executable _) statements expectedRows expectedErrors = do
  load <- loadStatement paths
  (timedOut, status, stdoutBytes, stderrBytes) <- runCLI extraEnvironment executable (unlines ([".bail off", load] <> statements))
  let expectedStatus = if null expectedErrors then ExitSuccess else ExitFailure 1
      actualLines = map (BS8.dropWhileEnd (== '\r')) (BS8.lines stdoutBytes)
      expectedLines = map (BS8.pack . intercalate ",") expectedRows
      problems =
        ["DuckDB CLI exceeded 60 seconds" | timedOut]
          <> ["exit status " <> show status <> ", expected " <> show expectedStatus | status /= expectedStatus]
          <> ["CSV results differ from the expected rows" | actualLines /= expectedLines]
          <> ["missing diagnostic: " <> diagnostic | diagnostic <- expectedErrors, not (BS8.pack diagnostic `BS.isInfixOf` stderrBytes)]
  unless (null problems) $
    assertFailure
      (intercalate "; " problems <> "\nstdout:\n" <> BS8.unpack (shortOutput stdoutBytes) <> "\nstderr:\n" <> BS8.unpack (shortOutput stderrBytes))
