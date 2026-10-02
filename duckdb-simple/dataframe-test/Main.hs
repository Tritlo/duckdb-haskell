{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Consume DuckDB Arrow batches with the dataframe library.
module Main (main) where

import Control.Exception (ErrorCall, displayException, try)
import Control.Monad (forM_)
import Data.Int (Int64)
import qualified Data.Text as Text
import qualified DataFrame.Core as DataFrame
import qualified DataFrame.IO.Arrow as DataFrameArrow
import Database.DuckDB.FFI (ArrowArray (..), ArrowSchema (..))
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Arrow as Arrow
import qualified Database.DuckDB.Simple.Deprecated.Streaming as Streaming
import Foreign.Ptr (Ptr, castPtr, nullFunPtr)
import Foreign.Storable (peek)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

-- | Test both execution modes against the same Arrow consumer.
main :: IO ()
main = defaultMain $ testGroup "DataFrame Arrow import" [dataframeTests False, dataframeTests True]

-- | Check data and ownership across successful and failed imports.
dataframeTests :: Bool -> TestTree
dataframeTests streaming =
    testGroup
        (if streaming then "deprecated streaming" else "materialized")
        [ testCase "several batches remain readable after connection close" do
            (rowCount, batches) <- withDb \conn ->
                foldArrow
                    conn
                    "SELECT i::INTEGER AS id, CASE WHEN i % 3 = 0 THEN NULL ELSE i - 2500 END AS \"íslenska_λ\", (i::FLOAT / 4)::FLOAT AS small, CASE WHEN i % 5 = 0 THEN NULL ELSE i::DOUBLE / 4 END AS real, CASE WHEN i % 7 = 0 THEN NULL WHEN i % 7 = 1 THEN '' ELSE ? || i::VARCHAR END AS label, NULL::BIGINT AS missing FROM range(?::BIGINT) t(i)"
                    ("λ\0雪" :: Text.Text, 5000 :: Int64)
                    (0 :: Int, [])
                    \(offset, acc) schema array -> do
                        assertLiveSchema schema
                        count <- fromIntegral . arrowArrayLength <$> peek array
                        frame <- DataFrameArrow.arrowToDataframe (castPtr schema) (castPtr array)
                        assertConsumed schema array
                        pure (offset + count, (offset, count, frame) : acc)
            rowCount @?= 5000
            assertBool "multiple native batches" (length batches > 1)
            forM_ batches \(offset, count, frame) -> do
                let expected = expectedFrame [offset .. offset + count - 1]
                DataFrame.columnNames frame @?= DataFrame.columnNames expected
                frame @?= expected
        , testCase "an unsupported type releases the failed export" $
            withDb \conn -> do
                -- Import one supported column before the unsupported column fails.
                result <- try $ foldArrow conn "SELECT 42::BIGINT AS n, true AS unsupported" () () \() schema array -> do
                    _ <- DataFrameArrow.arrowToDataframe (castPtr schema) (castPtr array)
                    pure ()
                case result of
                    Left (err :: ErrorCall) ->
                        assertBool "DataFrame format error" ("unsupported format" `Text.isInfixOf` Text.pack (displayException err))
                    Right () -> assertFailure "expected an unsupported Arrow type"
                query_ conn "SELECT 42" >>= (@?= [Only (42 :: Int64)])
        , testCase "an empty result does not call the consumer" $
            withDb \conn -> do
                count <- foldArrow conn "SELECT 1::BIGINT AS n WHERE false" () (0 :: Int) \_ schema array -> do
                    _ <- DataFrameArrow.arrowToDataframe (castPtr schema) (castPtr array)
                    assertFailure "unexpected batch"
                count @?= 0
        ]
  where
    foldArrow :: (ToRow q) => Connection -> Query -> q -> a -> (a -> Ptr ArrowSchema -> Ptr ArrowArray -> IO a) -> IO a
    foldArrow = if streaming then Streaming.foldArrow else Arrow.foldArrow

-- | Construct the expected values independently of the Arrow buffer layout.
expectedFrame :: [Int] -> DataFrame.DataFrame
expectedFrame rows =
    DataFrame.fromNamedColumns
        [ ("id", DataFrame.fromList rows)
        , ("íslenska_λ", DataFrame.fromList [if i `rem` 3 == 0 then Nothing else Just (i - 2500) | i <- rows])
        , ("small", DataFrame.fromList [fromIntegral i / 4 :: Double | i <- rows])
        , ("real", DataFrame.fromList [if i `rem` 5 == 0 then Nothing else Just (fromIntegral i / 4 :: Double) | i <- rows])
        , ("label", DataFrame.fromList [label i | i <- rows])
        , ("missing", DataFrame.fromList [Nothing :: Maybe Int | _ <- rows])
        ]
  where
    label i
        | i `rem` 7 == 0 = Nothing
        | i `rem` 7 == 1 = Just ""
        | otherwise = Just ("λ\0雪" <> Text.pack (show i))

-- | Fail before the consumer can dereference a previously released schema.
assertLiveSchema :: Ptr ArrowSchema -> Assertion
assertLiveSchema ptr = do
    schema <- peek ptr
    assertBool "each batch needs an unreleased schema" (arrowSchemaRelease schema /= nullFunPtr)

-- | The DataFrame importer releases both objects after copying their contents.
assertConsumed :: Ptr ArrowSchema -> Ptr ArrowArray -> Assertion
assertConsumed schema array = do
    arrowSchemaRelease <$> peek schema >>= (@?= nullFunPtr)
    arrowArrayRelease <$> peek array >>= (@?= nullFunPtr)

-- | Keep native batch ordering deterministic in both execution modes.
withDb :: (Connection -> IO a) -> IO a
withDb = withConnectionWithConfig ":memory:" [("threads", "1")]
