{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module ReplacementScansTest (tests) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Void (Void)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Utils (withConnection, withConstCString, withDatabase, withResult, withValue)

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
                    duckdb_add_replacement_scan db callback (coerce (nullPtr :: Ptr Void)) (coerce (nullFunPtr :: FunPtr Void))
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
    (Duckdb_replacement_callback_t -> IO a) ->
    IO a
withReplacementCallback seenTablesRef startValue endValue =
    bracket acquire (freeHaskellFunPtr . coerce)
  where
    acquire =
        mkReplacementCallback (replacementCallback seenTablesRef startValue endValue)

replacementCallback ::
    IORef [String] ->
    Int64 ->
    Int64 ->
    Duckdb_replacement_scan_info ->
    (ConstPtr CChar) ->
    Ptr Void ->
    IO ()
replacementCallback seenTablesRef startValue endValue info tableName _extra = do
    name <- (peekCString . coerce) tableName
    modifyIORef' seenTablesRef (name :)
    case name of
        "haskell_magic" -> do
            withConstCString "range" $ \fn ->
                duckdb_replacement_scan_set_function_name info fn
            withValue (duckdb_create_int64 startValue) $ \startVal ->
                duckdb_replacement_scan_add_parameter info startVal
            withValue (duckdb_create_int64 endValue) $ \endVal ->
                duckdb_replacement_scan_add_parameter info endVal
        "failing_magic" ->
            withConstCString "replacement rejected by test callback" $ \msg ->
                duckdb_replacement_scan_set_error info msg
        _ ->
            pure ()

assertReplacementQuery :: Duckdb_connection -> Int64 -> Int64 -> IO ()
assertReplacementQuery conn startValue countValue =
    withResult conn "SELECT range FROM haskell_magic ORDER BY range" \resPtr -> do
        rowCount <- duckdb_row_count resPtr
        rowCount @?= fromIntegral countValue
        forM_ [0 .. countValue - 1] \idx -> do
            value <- duckdb_value_int64 resPtr 0 (fromIntegral idx)
            value @?= startValue + idx

assertReplacementError :: Duckdb_connection -> IO ()
assertReplacementError conn =
    withConstCString "SELECT * FROM failing_magic" \sql ->
        alloca \resPtr -> do
            state <- duckdb_query conn sql resPtr
            state @?= DuckDBError
            errPtr <- duckdb_result_error resPtr
            errMsg <- (peekCString . coerce) errPtr
            assertBool "replacement error message should surface" ("rejected" `isInfixOf` errMsg)
            duckdb_destroy_result resPtr

mkReplacementCallback ::
    (Duckdb_replacement_scan_info -> ConstPtr CChar -> Ptr Void -> IO ()) ->
    IO Duckdb_replacement_callback_t
mkReplacementCallback = fmap Duckdb_replacement_callback_t . toFunPtr . Duckdb_replacement_callback_t_Aux
