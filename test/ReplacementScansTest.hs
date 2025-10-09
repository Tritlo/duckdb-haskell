{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module ReplacementScansTest (tests) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Replacement Scans"
    [ replacementScanRewritesAndErrors
    ]

replacementScanRewritesAndErrors :: TestTree
replacementScanRewritesAndErrors =
  testCase "replacement scan rewrites table name and can report errors" $
    runInBoundThread do
      seenTablesRef <- newIORef []

      let startValue = 5
          countValue = 4
          endValue = startValue + countValue

      withReplacementCallback seenTablesRef startValue endValue \callback -> do
        withDatabase \db -> do
          c_duckdb_add_replacement_scan db callback nullPtr nullFunPtr
          withConnection db \conn -> do
            assertReplacementQuery conn startValue countValue
            assertReplacementError conn

      seenTables <- readIORef seenTablesRef
      assertBool "replacement callback should run for rewrite target" ("haskell_magic" `elem` seenTables)
      assertBool "replacement callback should run for error target" ("failing_magic" `elem` seenTables)

withReplacementCallback ::
  IORef [String] ->
  Int64 ->
  Int64 ->
  (DuckDBReplacementCallback -> IO a) ->
  IO a
withReplacementCallback seenTablesRef startValue endValue =
  bracket acquire freeHaskellFunPtr
  where
    acquire =
      mkReplacementCallback (replacementCallback seenTablesRef startValue endValue)

replacementCallback ::
  IORef [String] ->
  Int64 ->
  Int64 ->
  DuckDBReplacementScanInfo ->
  CString ->
  Ptr () ->
  IO ()
replacementCallback seenTablesRef startValue endValue info tableName _extra = do
  name <- peekCString tableName
  modifyIORef' seenTablesRef (name :)
  case name of
    "haskell_magic" -> do
      withCString "range" \fn ->
        c_duckdb_replacement_scan_set_function_name info fn
      withValue (c_duckdb_create_int64 startValue) \startVal ->
        c_duckdb_replacement_scan_add_parameter info startVal
      withValue (c_duckdb_create_int64 endValue) \endVal ->
        c_duckdb_replacement_scan_add_parameter info endVal
    "failing_magic" ->
      withCString "replacement rejected by test callback" \msg ->
        c_duckdb_replacement_scan_set_error info msg
    _ ->
      pure ()

assertReplacementQuery :: DuckDBConnection -> Int64 -> Int64 -> IO ()
assertReplacementQuery conn startValue countValue =
  withResult conn "SELECT range FROM haskell_magic ORDER BY range" \resPtr -> do
    rowCount <- c_duckdb_row_count resPtr
    rowCount @?= fromIntegral countValue
    forM_ [0 .. countValue - 1] \idx -> do
      value <- c_duckdb_value_int64 resPtr 0 (fromIntegral idx)
      value @?= startValue + idx

assertReplacementError :: DuckDBConnection -> IO ()
assertReplacementError conn =
  withCString "SELECT * FROM failing_magic" \sql ->
    alloca \resPtr -> do
      state <- c_duckdb_query conn sql resPtr
      state @?= DuckDBError
      errPtr <- c_duckdb_result_error resPtr
      errMsg <- peekCString errPtr
      assertBool "replacement error message should surface" ("rejected" `isInfixOf` errMsg)
      c_duckdb_destroy_result resPtr

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase action =
  withCString ":memory:" \path ->
    alloca \dbPtr -> do
      st <- c_duckdb_open path dbPtr
      st @?= DuckDBSuccess
      db <- peek dbPtr
      result <- action db
      c_duckdb_close dbPtr
      pure result

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db action =
  alloca \connPtr -> do
    st <- c_duckdb_connect db connPtr
    st @?= DuckDBSuccess
    conn <- peek connPtr
    result <- action conn
    c_duckdb_disconnect connPtr
    pure result

withResult :: DuckDBConnection -> String -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn sql action =
  withCString sql \sqlPtr ->
    alloca \resPtr -> do
      st <- c_duckdb_query conn sqlPtr resPtr
      st @?= DuckDBSuccess
      result <- action resPtr
      c_duckdb_destroy_result resPtr
      pure result

withValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO a
withValue acquire action = bracket acquire destroy action
  where
    destroy value =
      alloca \ptr -> do
        poke ptr value
        c_duckdb_destroy_value ptr

foreign import ccall safe "wrapper"
  mkReplacementCallback ::
    (DuckDBReplacementScanInfo -> CString -> Ptr () -> IO ()) ->
    IO DuckDBReplacementCallback
