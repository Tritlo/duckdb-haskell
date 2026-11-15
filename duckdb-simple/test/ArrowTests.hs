{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : ArrowTests
-- Description : Integration tests for scoped Arrow batches and their native ownership.
module ArrowTests (arrowTests) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (ThreadKilled), IOException, SomeException, bracket, bracket_, fromException, mask_, throwIO, try)
import Control.Monad (forM, forM_, void, when)
import Data.Bits (testBit)
import Data.Coerce (coerce)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (sort)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Void (Void)
import Data.Word (Word8)
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple
import Database.DuckDB.Simple.Arrow (releaseArrowArray, releaseArrowSchema)
import qualified Database.DuckDB.Simple.Arrow as Arrow
import Database.DuckDB.Simple.FromField (DecimalValue (..))
import Database.DuckDB.Simple.Internal (peekUtf8CString)
import qualified Database.DuckDB.Simple.Streaming as Streaming
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import GHC.Records (getField)
import HsBindgen.Runtime.HasCField (fromPtr)
import HsBindgen.Runtime.Marshal (staticSizeOf)
import HsBindgen.Runtime.Support.FunPtr (fromFunPtr, toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TestUtils (filteredQueries)

-- | Exercise native results and count their Arrow release callbacks.
arrowTests :: TestTree
arrowTests =
  testGroup "Arrow batches" [arrowModeTests False, arrowModeTests True]

-- | Run the same ownership checks for both public Arrow interfaces.
arrowModeTests :: Bool -> TestTree
arrowModeTests streaming =
  testGroup
    (if streaming then "native streaming" else "materialized")
    [ testCase "parameters, Unicode names, NULLs and multiple batches" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
          (batches, chunks) <-
            foldArrow
              conn
              "SELECT CASE WHEN i % 3 = 0 THEN NULL ELSE i END AS \"íslenska_λ\" FROM range(?::BIGINT) t(i)"
              (Only (5000 :: Int64))
              (0 :: Int, [])
              \(count, acc) schemaPtr arrayPtr -> do
                schema <- peek schemaPtr
                (getField @"n_children") schema @?= 1
                child <- peekElemOff ((getField @"children") schema) 0 >>= peek
                peekUtf8CString ((getField @"name") child) >>= (@?= "íslenska_λ")
                (peekCString . coerce) ((getField @"format") child) >>= (@?= "l")
                values <- readInt64Batch arrayPtr
                pure (count + 1, values : acc)
          assertBool "more than one native batch" (batches > 1)
          concat (reverse chunks) @?= [if i `rem` 3 == 0 then Nothing else Just i | i <- [0 .. 4999]]
    , testCase "the batch size option sets the rows of each batch" $ do
        let run options count = withConnectionWithOptions ":memory:" options{connectionConfig = [("threads", "1")]} \conn ->
              reverse <$> foldArrow conn "SELECT i::BIGINT AS i FROM range(?::BIGINT) t(i)" (Only (count :: Int64)) [] \acc _ arrayPtr -> do
                values <- readInt64Batch arrayPtr
                pure (values : acc)
            -- Each array has at most the given number of rows. The arrays give
            -- each row in order.
            checkBatches size limit count = do
              batches <- run defaultConnectionOptions{connectionArrowBatchSize = size} count
              assertBool ("array lengths " <> show (map length batches)) (all ((<= limit) . length) batches)
              concat batches @?= map Just [0 .. count - 1]
        defaultRows <- run defaultConnectionOptions 5
        singleRows <- run defaultConnectionOptions{connectionArrowBatchSize = 1} 5
        concat defaultRows @?= map Just [0 .. 4]
        concat singleRows @?= concat defaultRows
        (length defaultRows, length singleRows) @?= (1, 5)
        -- As in DuckDB, 0 means 131072 rows.
        checkBatches 0 131072 300000
        checkBatches 20000 20000 10000
        checkBatches 3000 3000 10000
        negative <- try (withConnectionWithOptions ":memory:" defaultConnectionOptions{connectionArrowBatchSize = -1} (const (pure ())))
        case negative of
          Left (err :: SQLError) -> do
            sqlErrorType err @?= Just ErrorInputInvalid
            assertBool ("unexpected message: " <> show err) ("connectionArrowBatchSize" `Text.isInfixOf` sqlErrorMessage err)
          Right () -> assertFailure "a negative batch size opened a connection"
    , testCase "an error of the step keeps its own query, and a native error of the fold has the fold query" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> withConnection ":memory:" \other -> do
          let failingStep :: IO ()
              failingStep = void (query other "SELECT ?" (Only (DecimalValue 2 1 100)) :: IO [Only Int64])
              stepError :: String -> IO a -> IO ()
              stepError label action =
                try action >>= \case
                  Left (err :: SQLError) -> do
                    assertEqual label (Just ErrorTypeConversion) (sqlErrorType err)
                    assertEqual label Nothing (sqlErrorQuery err)
                  Right _ -> assertFailure (label <> ": expected the DECIMAL error")
          stepError "Arrow.foldArrow" (Arrow.foldArrow conn "SELECT 1" () () \acc _ _ -> failingStep >> pure acc)
          stepError "Streaming.foldArrow" (Streaming.foldArrow conn "SELECT 1" () () \acc _ _ -> failingStep >> pure acc)
          stepError "fold" (fold conn "SELECT 1" () () \acc (Only (_ :: Int64)) -> failingStep >> pure acc)
          stepError "Streaming.fold" (Streaming.fold conn "SELECT 1" () () \acc (Only (_ :: Int64)) -> failingStep >> pure acc)
          forM_ [("Arrow.foldArrow", Arrow.foldArrow), ("Streaming.foldArrow", Streaming.foldArrow)] \(label, foldBatches) ->
            try (foldBatches conn "SELECT error('boom')" () () \acc _ _ -> pure acc) >>= \case
              Left (err :: SQLError) -> assertEqual label (Just "SELECT error('boom')") (sqlErrorQuery err)
              Right () -> assertFailure (label <> ": expected the error of DuckDB")
    , testCase "empty results retain the initial accumulator" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
          result <- foldArrow_ conn "SELECT 1::BIGINT WHERE false" (17 :: Int) \_ _ _ -> assertFailure "unexpected batch" >> pure 0
          result @?= 17
    , testCase "result metadata follows bound parameter types" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
          values <- foldArrow conn "SELECT ? AS value" (Only (5000000000 :: Int64)) [] \acc _ array -> do
            batch <- readInt64Batch array
            pure (acc <> batch)
          values @?= [Just 5000000000]
    , testCase "success releases the schema and every batch" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
          withReleaseCounters \schemaReleases arrayReleases observe -> do
            batches <- foldArrow_ conn "SELECT i FROM range(5000) t(i)" (0 :: Int) \count schema array -> do
              observe schema array
              pure (count + 1)
            readIORef schemaReleases >>= (@?= batches)
            readIORef arrayReleases >>= (@?= batches)
            assertConnectionUsable conn
    , testCase "consumers can release each schema and batch" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
          withReleaseCounters \schemaReleases arrayReleases observe -> do
            batches <- foldArrow_ conn "SELECT i FROM range(5000) t(i)" (0 :: Int) \count schema array -> do
              observe schema array
              releaseArrowArray array
              releaseArrowSchema schema
              pure (count + 1)
            assertBool "more than one native batch" (batches > 1)
            readIORef schemaReleases >>= (@?= batches)
            readIORef arrayReleases >>= (@?= batches)
            assertConnectionUsable conn
    , testCase "moved objects survive query and connection close" $
        withReleaseCounters \schemaReleases arrayReleases observe -> do
          alloca \savedSchema -> alloca \savedArray ->
            bracket_
              ( do
                  fillBytes savedSchema 0 (staticSizeOf (Proxy @ArrowSchema))
                  fillBytes savedArray 0 (staticSizeOf (Proxy @ArrowArray))
              )
              (releaseArrowArray savedArray >> releaseArrowSchema savedSchema)
              do
                withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
                  foldArrow_ conn "SELECT 42::BIGINT AS id" () \() schemaPtr arrayPtr -> mask_ do
                    observe schemaPtr arrayPtr
                    schema <- peek schemaPtr
                    array <- peek arrayPtr
                    poke savedSchema schema
                    poke (fromPtr (Proxy @"release") (schemaPtr :: Ptr ArrowSchema)) (coerce (nullFunPtr @Void))
                    poke savedArray array
                    poke (fromPtr (Proxy @"release") (arrayPtr :: Ptr ArrowArray)) (coerce (nullFunPtr @Void))
                readIORef schemaReleases >>= (@?= 0)
                readIORef arrayReleases >>= (@?= 0)
                schema <- peek savedSchema
                child <- peekElemOff ((getField @"children") schema) 0 >>= peek
                (peekCString . coerce) ((getField @"name") child) >>= (@?= "id")
                readInt64Batch savedArray >>= (@?= [Just 42])
          readIORef schemaReleases >>= (@?= 1)
          readIORef arrayReleases >>= (@?= 1)
    , testCase "exceptions during consumption release each object once" $
        forM_ [False, True] \consumeSchema ->
          withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
            withReleaseCounters \schemaReleases arrayReleases observe -> do
              result <- try $ foldArrow_ conn "SELECT 42::BIGINT" () \() schema array -> do
                observe schema array
                releaseArrowArray array
                when consumeSchema (releaseArrowSchema schema)
                throwIO (userError "consumer failed after release")
              case result of
                Left (err :: IOException) -> assertBool "consumer exception" ("consumer failed" `Text.isInfixOf` Text.pack (show err))
                Right () -> assertFailure "expected consumer exception"
              readIORef schemaReleases >>= (@?= 1)
              readIORef arrayReleases >>= (@?= 1)
              assertConnectionUsable conn
    , testCase "callback exceptions release the active batch and schema" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
          withReleaseCounters \schemaReleases arrayReleases observe -> do
            result <- try $ foldArrow_ conn "SELECT i FROM range(5000) t(i)" () \_ schema array -> do
              observe schema array
              throwIO (userError "Arrow callback failed")
            case result of
              Left (err :: IOException) -> assertBool "original error" ("Arrow callback failed" `Text.isInfixOf` Text.pack (show err))
              Right () -> assertFailure "expected callback exception"
            readIORef schemaReleases >>= (@?= 1)
            readIORef arrayReleases >>= (@?= 1)
            assertConnectionUsable conn
    , testCase "cancellation releases the active batch and schema" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
          withReleaseCounters \schemaReleases arrayReleases observe -> do
            entered <- newEmptyMVar
            blocked <- newEmptyMVar
            finished <- newEmptyMVar
            worker <- forkIO do
              result <- try $ foldArrow_ conn "SELECT i FROM range(5000) t(i)" () \_ schema array -> do
                observe schema array
                putMVar entered ()
                takeMVar blocked
              putMVar finished (result :: Either SomeException ())
            takeMVar entered
            killThread worker
            result <- takeMVar finished
            case result of
              Left err -> fromException err @?= Just ThreadKilled
              Right () -> assertFailure "expected cancellation"
            readIORef schemaReleases >>= (@?= 1)
            readIORef arrayReleases >>= (@?= 1)
            assertConnectionUsable conn
    , testCase "SQL errors leave the connection usable" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
          result <- try $ foldArrow_ conn "SELECT error('Arrow query failed')" () \_ _ _ -> assertFailure "unexpected batch"
          case result of
            Left (err :: SQLError) -> assertBool "native error" ("Arrow query failed" `Text.isInfixOf` sqlErrorMessage err)
            Right () -> assertFailure "expected query error"
          assertConnectionUsable conn
    , testCase "callback connection use follows the execution mode" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
          count <- foldArrow_ conn "SELECT i FROM range(5000) t(i)" (0 :: Int) \acc _ _ -> do
            if streaming
              then do
                outcome <- try (query_ conn "SELECT 42" :: IO [Only Int64])
                case outcome of
                  Left (err :: SQLError) -> sqlErrorType err @?= Just ErrorResourceInUse
                  Right _ -> assertFailure "expected a busy streaming connection"
              else query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
            pure (acc + 1)
          assertBool "expected multiple callbacks" (count > 1)
    , testCase "SQL errors report the query that failed" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
          let outer = "SELECT error('Arrow query failed')"
              inner = "SELECT error('callback query failed')"
          failed <- try $ foldArrow_ conn outer () \_ _ _ -> assertFailure "unexpected batch"
          case failed of
            Left (err :: SQLError) -> sqlErrorQuery err @?= Just outer
            Right () -> assertFailure "expected query error"
          nested <- try $ foldArrow_ conn "SELECT 42" () \_ _ _ -> void (query_ conn inner :: IO [Only Int64])
          case nested of
            Left (err :: SQLError) -> sqlErrorQuery err @?= Just inner
            Right () -> assertFailure "expected callback query error"
          assertConnectionUsable conn
    , testCase "closing the connection stops further batch callbacks" $
        withConnectionWithConfig ":memory:" [("threads", "1")] \conn ->
          withReleaseCounters \schemaReleases arrayReleases observe -> do
            callbacks <- newIORef (0 :: Int)
            result <- try $ foldArrow_ conn "SELECT i FROM range(5000) t(i)" () \_ schema array -> do
              observe schema array
              modifyIORef' callbacks (+ 1)
              close conn
            case result of
              Left (err :: SQLError) -> sqlErrorMessage err @?= "duckdb-simple: connection is closed"
              Right () -> assertFailure "expected closed connection error"
            readIORef callbacks >>= (@?= 1)
            readIORef schemaReleases >>= (@?= 1)
            readIORef arrayReleases >>= (@?= 1)
    , testCase "filtered results give each row in the batches" $
        forM_ ["1", "4"] \threads -> withConnectionWithConfig ":memory:" [("threads", threads)] \conn ->
          forM_ filteredQueries \(sql, expected) -> do
            batches <- foldArrow_ conn sql [] \acc _ array -> (: acc) <$> readInt64Batch array
            let rows = concat (reverse batches)
            -- With more than one thread, DuckDB can give the rows in another order.
            assertEqual (Text.unpack threads <> " threads: " <> show sql) (map Just expected) (if threads == "1" then rows else sort rows)
    ]
  where
    foldArrow :: (ToRow q) => Connection -> Query -> q -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
    foldArrow = if streaming then Streaming.foldArrow else Arrow.foldArrow

    foldArrow_ :: Connection -> Query -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
    foldArrow_ = if streaming then Streaming.foldArrow_ else Arrow.foldArrow_

-- | Copy a BIGINT Arrow column, including its validity bitmap.
readInt64Batch :: Ptr ArrowArray -> IO [Maybe Int64]
readInt64Batch arrayPtr = do
  array <- peek arrayPtr
  child <- peekElemOff ((getField @"children") array) 0 >>= peek
  validity <- peekElemOff ((getField @"buffers") child) 0
  values <- peekElemOff ((getField @"buffers") child) 1
  forM [0 .. fromIntegral ((getField @"length") array) - 1] \row -> do
    let index = fromIntegral ((getField @"offset") child) + row
    valid <-
      if validity == (coerce (nullPtr @Void))
        then pure True
        else do
          byte <- peekElemOff (coerce validity :: Ptr Word8) (index `div` 8)
          pure (testBit byte (index `rem` 8))
    if valid then Just <$> peekElemOff (coerce values) index else pure Nothing

-- | Wrap real release callbacks to observe cleanup without changing ownership.
withReleaseCounters :: (IORef Int -> IORef Int -> (Ptr ArrowSchema -> Ptr ArrowArray -> IO ()) -> IO a) -> IO a
withReleaseCounters action = do
  schemaReleases <- newIORef 0
  arrayReleases <- newIORef 0
  originalSchema <- newIORef Nothing
  originalArray <- newIORef Nothing
  bracket
    ( toFunPtr \ptr -> do
        modifyIORef' schemaReleases (+ 1)
        callback <- readIORef originalSchema
        maybe (assertFailure "missing schema release") (\release -> fromFunPtr release ptr) callback
    )
    (freeHaskellFunPtr . coerce)
    \schemaCallback ->
      bracket
        ( toFunPtr \ptr -> do
            modifyIORef' arrayReleases (+ 1)
            callback <- readIORef originalArray
            maybe (assertFailure "missing array release") (\release -> fromFunPtr release ptr) callback
        )
        (freeHaskellFunPtr . coerce)
        \arrayCallback -> do
          let observe schemaPtr arrayPtr = do
                schema <- peek schemaPtr
                assertBool "schema must not have been released" ((getField @"release") schema /= (coerce (nullFunPtr @Void)))
                when ((getField @"release") schema /= schemaCallback) do
                  modifyIORef' originalSchema (const (Just ((getField @"release") schema)))
                  poke (fromPtr (Proxy @"release") (schemaPtr :: Ptr ArrowSchema)) schemaCallback
                array <- peek arrayPtr
                modifyIORef' originalArray (const (Just ((getField @"release") array)))
                poke (fromPtr (Proxy @"release") (arrayPtr :: Ptr ArrowArray)) arrayCallback
          action schemaReleases arrayReleases observe

-- | Check that no failed Arrow operation leaves the connection busy.
assertConnectionUsable :: Connection -> Assertion
assertConnectionUsable conn = (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])
