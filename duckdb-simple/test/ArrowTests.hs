{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Integration tests for scoped Arrow batches and their native ownership.
module ArrowTests (arrowTests) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (ThreadKilled), IOException, SomeException, bracket, fromException, throwIO, try)
import Control.Monad (forM, when)
import Data.Bits (testBit)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Text as Text
import Data.Word (Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Arrow as Arrow
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Database.DuckDB.Simple.Internal (peekUtf8CString)
import Foreign.C.String (peekCString)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | Exercise native results and count their Arrow release callbacks.
arrowTests :: TestTree
arrowTests =
    testGroup "Arrow batches" [arrowModeTests False, arrowModeTests True]

-- | Run the same ownership checks for both public Arrow interfaces.
arrowModeTests :: Bool -> TestTree
arrowModeTests streaming =
    testGroup
        (if streaming then "deprecated streaming" else "materialized")
        [ testCase "parameters, Unicode names, NULLs and multiple batches" $
            withConnectionWithConfig ":memory:" [("threads", "1")] \conn -> do
                (batches, chunks) <-
                    foldArrow conn "SELECT CASE WHEN i % 3 = 0 THEN NULL ELSE i END AS \"íslenska_λ\" FROM range(?::BIGINT) t(i)" (Only (5000 :: Int64)) (0 :: Int, []) \(count, acc) schemaPtr arrayPtr -> do
                        schema <- peek schemaPtr
                        arrowSchemaChildCount schema @?= 1
                        child <- peekElemOff (arrowSchemaChildren schema) 0 >>= peek
                        peekUtf8CString (arrowSchemaName child) >>= (@?= "íslenska_λ")
                        peekCString (arrowSchemaFormat child) >>= (@?= "l")
                        values <- readInt64Batch arrayPtr
                        pure (count + 1, values : acc)
                assertBool "more than one native batch" (batches > 1)
                concat (reverse chunks) @?= [if i `rem` 3 == 0 then Nothing else Just i | i <- [0 .. 4999]]
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
                    readIORef schemaReleases >>= (@?= 1)
                    readIORef arrayReleases >>= (@?= batches)
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
    child <- peekElemOff (arrowArrayChildren array) 0 >>= peek
    validity <- peekElemOff (arrowArrayBuffers child) 0
    values <- peekElemOff (arrowArrayBuffers child) 1
    forM [0 .. fromIntegral (arrowArrayLength array) - 1] \row -> do
        let index = fromIntegral (arrowArrayOffset child) + row
        valid <-
            if validity == nullPtr
                then pure True
                else do
                    byte <- peekElemOff (castPtr validity :: Ptr Word8) (index `div` 8)
                    pure (testBit byte (index `rem` 8))
        if valid then Just <$> peekElemOff (castPtr values) index else pure Nothing

-- | Wrap real release callbacks to observe cleanup without changing ownership.
withReleaseCounters :: (IORef Int -> IORef Int -> (Ptr ArrowSchema -> Ptr ArrowArray -> IO ()) -> IO a) -> IO a
withReleaseCounters action = do
    schemaReleases <- newIORef 0
    arrayReleases <- newIORef 0
    originalSchema <- newIORef Nothing
    originalArray <- newIORef Nothing
    bracket
        ( wrapSchemaRelease \ptr -> do
            modifyIORef' schemaReleases (+ 1)
            callback <- readIORef originalSchema
            maybe (assertFailure "missing schema release") (\release -> mkArrowSchemaRelease release ptr) callback
        )
        freeHaskellFunPtr
        \schemaCallback ->
            bracket
                ( wrapArrayRelease \ptr -> do
                    modifyIORef' arrayReleases (+ 1)
                    callback <- readIORef originalArray
                    maybe (assertFailure "missing array release") (\release -> mkArrowArrayRelease release ptr) callback
                )
                freeHaskellFunPtr
                \arrayCallback -> do
                    let observe schemaPtr arrayPtr = do
                            schema <- peek schemaPtr
                            when (arrowSchemaRelease schema /= schemaCallback) do
                                modifyIORef' originalSchema (const (Just (arrowSchemaRelease schema)))
                                poke schemaPtr schema{arrowSchemaRelease = schemaCallback}
                            array <- peek arrayPtr
                            modifyIORef' originalArray (const (Just (arrowArrayRelease array)))
                            poke arrayPtr array{arrowArrayRelease = arrayCallback}
                    action schemaReleases arrayReleases observe

-- | Check that no failed Arrow operation leaves the connection busy.
assertConnectionUsable :: Connection -> Assertion
assertConnectionUsable conn = (query_ conn "SELECT 42" :: IO [Only Int64]) >>= (@?= [Only 42])

-- | Observe schema cleanup through a native callback.
foreign import ccall "wrapper"
    wrapSchemaRelease :: (Ptr ArrowSchema -> IO ()) -> IO (FunPtr (Ptr ArrowSchema -> IO ()))

-- | Observe array cleanup through a native callback.
foreign import ccall "wrapper"
    wrapArrayRelease :: (Ptr ArrowArray -> IO ()) -> IO (FunPtr (Ptr ArrowArray -> IO ()))
