{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module V2ReplacementScanTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Coerce (coerce)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import GHC.Records (getField)
import HsBindgen.Runtime.Prelude (toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (
  borrowedOpaque,
  callbackGuard,
  checked,
  failOnPurpose,
  output,
  readIntegers,
  readView,
  statementError,
  statementFails,
  withConnection,
  withFactory,
  withHandle,
  withResult,
  withType,
  withView,
 )

tests :: TestTree
tests =
  testGroup
    "DuckDB V2 replacement scans"
    [ testCase
        "replacement scans claim names as table function calls and as column data collections, and refuse a wrong claim"
        testReplacementScan
    ]

-- | Read the last part of a qualified name, which is the object name.
objectName :: DuckDBV2QnameHandle -> IO String
objectName name = do
  count <- output (c_duckdb_v2_qname_get_part_count name)
  alloca $ \part -> do
    checked (c_duckdb_v2_qname_get_part name (count - 1) part)
    peek part >>= readView . coerce

-- | Claim the reference as a call of the table function with this name.
setFunction :: DuckDBV2ReplacementScanInfoHandle -> String -> IO ()
setFunction info text = withView text $ \view ->
  withHandle (c_duckdb_v2_qname_parse view) c_duckdb_v2_qname_destroy $ \name ->
    checked (c_duckdb_v2_replacement_scan_set_function_name info name)

-- | Add a positional BIGINT argument to the claimed function call.
addArgument :: DuckDBV2FactoryHandle -> DuckDBV2ReplacementScanInfoHandle -> Int64 -> IO ()
addArgument factory info number = withHandle (c_duckdb_v2_value_create_bigint factory number) c_duckdb_v2_value_destroy $ \value ->
  checked (c_duckdb_v2_replacement_scan_add_argument info value)

-- | The first scan claims the names v2_range, v2_repeat, v2_named, v2_default and v2_unknown, and fails on purpose
-- for v2_failing. It declines all other names. The second scan has no user data and claims v2_plain.
testReplacementScan :: IO ()
testReplacementScan = do
  failures <- newIORef []
  claimed <- newIORef []
  -- The connection creates the collection after the callback exists, so the callback reads it from a cell.
  collectionCell <- newIORef (coerce nullPtr)
  allocaBytes 1 $ \user -> do
    let claim info context err = callbackGuard failures err $ do
          output (c_duckdb_v2_replacement_scan_get_user_data info) >>= (@?= user)
          name <- withHandle (c_duckdb_v2_replacement_scan_get_name info) c_duckdb_v2_qname_destroy objectName
          factory <- output (c_duckdb_v2_context_get_factory context)
          collection <- readIORef collectionCell
          -- A call that must fail gets no error slot, so its failure does not fail the callback.
          let refused what call = call nullPtr >>= assertBool (what <> " fails") . (/= DuckDBV2ErrorNone)
          case name of
            "v2_range" -> do
              withHandle (c_duckdb_v2_value_create_bigint factory 2) c_duckdb_v2_value_destroy $ \value ->
                refused "an argument before the function name" (c_duckdb_v2_replacement_scan_add_argument info value)
              -- A second function name replaces the first one and keeps the arguments.
              setFunction info "v2_absent_function"
              mapM_ (addArgument factory info) [2, 11, 3]
              setFunction info "range"
              -- A claim through a different form fails, and the function claim stays.
              refused "a collection after a function name" (c_duckdb_v2_replacement_scan_set_collection info collection (coerce nullPtr) 0)
              withView "v2_alias" $ \alias -> checked (c_duckdb_v2_replacement_scan_set_alias info (coerce alias))
            "v2_repeat" -> do
              let addRows call = withView "num_rows" $ \parameter -> withHandle (c_duckdb_v2_value_create_bigint factory 3) c_duckdb_v2_value_destroy $ \value ->
                    call (c_duckdb_v2_replacement_scan_add_named_argument info (coerce parameter) value)
              addRows (refused "a named argument before the function name")
              setFunction info "repeat_row"
              addArgument factory info 7
              addRows checked
            "v2_named" -> do
              -- With column names, the name count must equal the column count, and no name may be empty.
              withView "amount" $ \amount -> withView "extra" $ \extra -> do
                names <- mapM (fmap DuckDBV2IdentifierT . peek . coerce) [amount, extra]
                withArray names $ \columns ->
                  refused "two names for one column" (c_duckdb_v2_replacement_scan_set_collection info collection (ConstPtr columns) 2)
              withView "" $ \column -> refused "an empty column name" (c_duckdb_v2_replacement_scan_set_collection info collection (coerce column) 1)
              withView "amount" $ \column -> checked (c_duckdb_v2_replacement_scan_set_collection info collection (coerce column) 1)
            "v2_default" -> do
              checked (c_duckdb_v2_replacement_scan_set_collection info collection (coerce nullPtr) 0)
              withView "range" $ \text -> withHandle (c_duckdb_v2_qname_parse text) c_duckdb_v2_qname_destroy $ \function ->
                refused "a function name after a collection" (c_duckdb_v2_replacement_scan_set_function_name info function)
            -- The scan does not resolve the name, so the claim succeeds and the statement fails later.
            "v2_unknown" -> setFunction info "v2_absent_function"
            "v2_failing" -> failOnPurpose "replacement scan"
            _ -> pure ()
          atomicModifyIORef' claimed (\names -> (name : names, ()))
        plain info context err = callbackGuard failures err $ do
          -- The second scan has no user data, so the user data is null.
          output (c_duckdb_v2_replacement_scan_get_user_data info) >>= (@?= nullPtr)
          name <- withHandle (c_duckdb_v2_replacement_scan_get_name info) c_duckdb_v2_qname_destroy objectName
          factory <- output (c_duckdb_v2_context_get_factory context)
          when (name == "v2_plain") $ setFunction info "range" >> addArgument factory info 2
    bracket
      (DuckDBV2ReplacementScanCallbackFn <$> toFunPtr (DuckDBV2ReplacementScanCallbackFn_Aux claim))
      (freeHaskellFunPtr . getField @"unwrap")
      $ \claimPtr ->
        bracket
          (DuckDBV2ReplacementScanCallbackFn <$> toFunPtr (DuckDBV2ReplacementScanCallbackFn_Aux plain))
          (freeHaskellFunPtr . getField @"unwrap")
          $ \plainPtr ->
            withConnection $ \conn -> withFactory conn $ \factory -> withType factory "BIGINT" $ \ty -> withArray [ty] $ \types ->
              withHandle (c_duckdb_v2_column_data_collection_create factory (ConstPtr types) 1) c_duckdb_v2_column_data_collection_destroy $ \collection -> do
                writeIORef collectionCell collection
                -- Append count rows start, start + 10, ... to the collection.
                let append start count = withHandle (c_duckdb_v2_data_chunk_create factory (ConstPtr types) 1) c_duckdb_v2_data_chunk_destroy $ \chunk -> do
                      vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                      checked (c_duckdb_v2_vector_make_sequence vector start 10 count)
                      withHandle (c_duckdb_v2_column_data_collection_append_state_create collection) c_duckdb_v2_column_data_collection_append_state_destroy $ \state ->
                        checked (c_duckdb_v2_column_data_collection_append collection state chunk)
                append 10 3
                withHandle (c_duckdb_v2_replacement_scan_create factory) c_duckdb_v2_replacement_scan_destroy $ \scan -> do
                  checked (c_duckdb_v2_replacement_scan_set_callback scan claimPtr)
                  with (borrowedOpaque user) $ \opaque -> checked (c_duckdb_v2_replacement_scan_set_user_data scan opaque)
                  checked (c_duckdb_v2_connection_register_replacement_scan conn scan)
                let refused what code = assertBool (what <> " is refused: " <> show code) (code /= DuckDBV2ErrorNone)
                -- Registration requires a callback, and a scan registers only once.
                withHandle (c_duckdb_v2_replacement_scan_create factory) c_duckdb_v2_replacement_scan_destroy $ \scan -> do
                  c_duckdb_v2_connection_register_replacement_scan conn scan nullPtr >>= refused "a scan without a callback"
                  checked (c_duckdb_v2_replacement_scan_set_callback scan plainPtr)
                  checked (c_duckdb_v2_connection_register_replacement_scan conn scan)
                  c_duckdb_v2_connection_register_replacement_scan conn scan nullPtr >>= refused "a second registration of a scan"
                withConnection $ \other -> withFactory other $ \otherFactory ->
                  withHandle (c_duckdb_v2_replacement_scan_create otherFactory) c_duckdb_v2_replacement_scan_destroy $ \scan -> do
                    checked (c_duckdb_v2_replacement_scan_set_callback scan plainPtr)
                    c_duckdb_v2_connection_register_replacement_scan conn scan nullPtr >>= refused "a scan of another database"
                -- The query names the alias, so it binds only if the scan sets the alias.
                withResult conn "SELECT v2_alias.* FROM v2_range" $ \result -> readIntegers result >>= (@?= [Just 2, Just 5, Just 8])
                withResult conn "SELECT * FROM v2_repeat" $ \result -> readIntegers result >>= (@?= replicate 3 (Just 7))
                withResult conn "SELECT amount FROM v2_named" $ \result -> readIntegers result >>= (@?= [Just 10, Just 20, Just 30])
                -- The scan borrows the collection, so a later query reads the rows appended since.
                append 40 1
                withResult conn "SELECT col1 FROM v2_default" $ \result -> readIntegers result >>= (@?= [Just 10, Just 20, Just 30, Just 40])
                withResult conn "SELECT * FROM v2_plain" $ \result -> readIntegers result >>= (@?= [Just 0, Just 1])
                statementFails conn "SELECT * FROM v2_unknown"
                -- The statement fails with the text of the callback, and the next statement on the connection works.
                -- The header names no code for this failure.
                (code, text) <- statementError conn "SELECT * FROM v2_failing"
                assertBool "the statement of the failing scan fails" (code /= DuckDBV2ErrorNone)
                assertBool ("the error contains the text of the callback: " <> text) ("v2 deliberate replacement scan failure" `isInfixOf` text)
                atomicModifyIORef' failures (\seen -> ([], seen)) >>= (@?= ["user error (v2 deliberate replacement scan failure)"])
                withResult conn "SELECT * FROM v2_repeat" $ \result -> readIntegers result >>= (@?= replicate 3 (Just 7))
  readIORef failures >>= (@?= [])
  names <- readIORef claimed
  forM_ ["v2_range", "v2_repeat", "v2_named", "v2_default"] $ \name ->
    assertBool ("the callback saw " <> name <> ": " <> show names) (name `elem` names)
