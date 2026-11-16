{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cases of the sized ARRAY type 'SizedArray'. See Note [Sized arrays] in
-- "Database.DuckDB.Simple.ToField".
module SizedArrayTests (sizedArrayTests) where

import Control.Exception (try)
import Data.Int (Int32, Int64, Int8)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Database.DuckDB.Simple
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestUtils (assertInputErrorContaining)

-- | The cases of 'SizedArray' as a parameter, a column, and a function argument.
sizedArrayTests :: TestTree
sizedArrayTests =
  testGroup
    "sized arrays"
    [ testCase "a SizedArray binds as an ARRAY of its size and reads back from a parameter and a column" $ withConnection ":memory:" \conn -> do
        let values = fromJust (sizedArray @3 [1, 2, 3 :: Int64])
        sizedArrayToList values @?= [1, 2, 3]
        sizedArray @3 [1, 2 :: Int64] @?= Nothing
        query conn "SELECT typeof(?), ?" (values, values) >>= (@?= [("BIGINT[3]" :: Text.Text, values)])
        _ <- execute_ conn "CREATE TABLE stored (v BIGINT[3])"
        _ <- execute conn "INSERT INTO stored VALUES (?)" (Only values)
        query_ conn "SELECT v FROM stored" >>= (@?= [Only values])
        -- A LIST of three elements reads, as the cast to BIGINT[3] does.
        query_ conn "SELECT [1, 2, 3]::BIGINT[]" >>= (@?= [Only values])
    , testCase "a SizedArray raises an error for an ARRAY or a LIST of another length" $ withConnection ":memory:" \conn -> do
        let refused expression sqlName count =
              try (query_ conn (Query ("SELECT " <> expression <> " AS v")) :: IO [Only (SizedArray 3 Int64)]) >>= \case
                Left SQLError{sqlErrorMessage} ->
                  sqlErrorMessage
                    @?= "duckdb-simple: column v has type " <> sqlName <> " but expected SizedArray 3 Int64: the value has " <> count <> " elements"
                Right rows -> assertFailure ("expected Incompatible for " <> Text.unpack expression <> ", got " <> show rows)
        refused "[1, 2]::BIGINT[2]" "BIGINT[2]" "2"
        refused "[1, 2, 3, 4]::BIGINT[4]" "BIGINT[4]" "4"
        refused "[1, 2]::BIGINT[]" "BIGINT[]" "2"
    , testCase "a nested SizedArray and a SizedArray of Maybe elements round-trip" $ withConnection ":memory:" \conn -> do
        let row = fromJust . sizedArray @3
            nested = fromJust (sizedArray @2 [row [1, 2, 3 :: Int32], row [4, 5, 6]])
        query conn "SELECT typeof(?), ?" (nested, nested) >>= (@?= [("INTEGER[3][2]" :: Text.Text, nested)])
        let optional = fromJust (sizedArray @3 [Just 1, Nothing, Just (3 :: Int64)])
        query conn "SELECT typeof(?), ?" (optional, optional) >>= (@?= [("BIGINT[3]" :: Text.Text, optional)])
    , testCase "a SizedArray of size 0 raises an input error at the bind and at the declaration" $ withConnection ":memory:" \conn -> do
        let empty = fromJust (sizedArray @0 ([] :: [Int64]))
        assertInputErrorContaining "an empty ARRAY parameter is not possible" (query conn "SELECT ?" (Only empty) :: IO [Only (SizedArray 0 Int64)])
        assertInputErrorContaining "a SizedArray of size 0 has no ARRAY type" $
          createFunction conn "sized_empty" \(_ :: SizedArray 0 Int64) -> (1 :: Int64)
    , testCase "a SizedArray of the largest ARRAY size of DuckDB binds and declares, and a larger SizedArray does not declare" $ withConnection ":memory:" \conn -> do
        let largest = fromJust (sizedArray @100000 (replicate 100000 (1 :: Int8)))
        query conn "SELECT typeof(?), len(?)" (largest, largest) >>= (@?= [("TINYINT[100000]" :: Text.Text, 100000 :: Int64)])
        createFunction conn "sized_largest" \(v :: SizedArray 100000 Int8) -> length v
        query conn "SELECT sized_largest(?)" (Only largest) >>= (@?= [Only (100000 :: Int)])
        try (createFunction conn "sized_above" \(v :: SizedArray 100001 Int8) -> length v) >>= \case
          Left SQLError{sqlErrorType, sqlErrorMessage} -> do
            sqlErrorType @?= Just ErrorQueryBinder
            sqlErrorMessage @?= "Binder Error: ARRAY type size must be at most 100000"
          Right () -> assertFailure "expected the Binder Error of DuckDB for a SizedArray of size 100001"
    ]
