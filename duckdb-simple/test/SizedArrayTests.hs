{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cases of the sized ARRAY type 'SizedArray'. See Note [Sized arrays] in
-- "Database.DuckDB.Simple.ToField".
module SizedArrayTests (sizedArrayTests) where

import Control.Exception (try)
import Control.Monad (forM_)
import Data.Array (bounds, elems, listArray, (!))
import Data.Foldable (toList)
import Data.Int (Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Generic (DuckValue (..), ViaDuckDB (..))
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), TypeDeclaration (..))
import Database.DuckDB.Simple.ToField (ToDuckValue)
import qualified Database.DuckDB.Simple.V2.Registration as R
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestUtils (assertInputErrorContaining)

-- | A record with a field of a sized ARRAY type.
newtype Sized = Sized {sizedValues :: SizedArray 3 Int64}
  deriving stock (Eq, Show, Generic)
  deriving (DuckDBColumnType, ToField, ToDuckValue, FromField) via (ViaDuckDB Sized)

-- | A typed NULL must not allow an unsupported ARRAY size in a generic field.
newtype Above = Above {aboveValues :: Maybe (SizedArray 100001 Int64)}
  deriving stock (Show, Generic)
  deriving (DuckDBColumnType, ToField, ToDuckValue, FromField) via (ViaDuckDB Above)

-- | This size would wrap to one element if narrowed directly to Word64.
newtype Overflow = Overflow {overflowValues :: Maybe (SizedArray 18446744073709551617 Int64)}
  deriving stock (Show, Generic)
  deriving (DuckDBColumnType, ToField, ToDuckValue, FromField) via (ViaDuckDB Overflow)

-- | The generic declaration must check ARRAY sizes below each container.
newtype NestedAbove = NestedAbove {nestedValues :: [Map.Map Text.Text (Maybe (SizedArray 100001 Int64))]}
  deriving stock (Show, Generic)
  deriving (DuckDBColumnType, ToField, ToDuckValue, FromField) via (ViaDuckDB NestedAbove)

-- | The cases of 'SizedArray' as a parameter, a column, and a function argument.
sizedArrayTests :: TestTree
sizedArrayTests =
  testGroup
    "sized arrays"
    [ testCase "a SizedArray rejects a longer list without forcing its tail or elements" $ do
        assertBool "an infinite list is too long" (isNothing (sizedArrayFromList @3 ([1 ..] :: [Int64])))
        assertBool
          "a fourth element is enough to reject the list"
          (isNothing (sizedArrayFromList @3 ([undefined, undefined, undefined, undefined] ++ undefined :: [Int64])))
    , testCase "a SizedArray binds as an ARRAY of its size and reads back from a parameter and a column" $ withConnection ":memory:" \conn -> do
        let values = fromJust (sizedArrayFromList @3 [1, 2, 3 :: Int64])
        toList values @?= [1, 2, 3]
        sizedArrayFromList @3 [1, 2 :: Int64] @?= Nothing
        -- An Array with any bounds gives the bounds (0, n - 1) and the same
        -- elements. An Array of another count gives Nothing.
        let shifted = listArray (5, 7) [1, 2, 3 :: Int64]
            fromShifted = fromJust (sizedArray @3 shifted)
        bounds (sizedArrayToArray fromShifted) @?= (0, 2)
        elems (sizedArrayToArray fromShifted) @?= elems shifted
        fromShifted @?= values
        show values @?= "SizedArray (array (0,2) [(0,1),(1,2),(2,3)])"
        -- The count is checked before the Array is built, for an Array and a
        -- list of fewer and of more values.
        sizedArray @3 (listArray (0, 1) [1, 2 :: Int64]) @?= Nothing
        sizedArray @3 (listArray (0, 3) [1, 2, 3, 4 :: Int64]) @?= Nothing
        sizedArrayFromList @3 [1, 2, 3, 4 :: Int64] @?= Nothing
        -- Size 0 has the bounds (0, -1). A size above the range of Int gives
        -- Nothing, and no size wraps into the count of the values.
        bounds (sizedArrayToArray (fromJust (sizedArrayFromList @0 ([] :: [Int64])))) @?= (0, -1)
        sizedArrayFromList @9223372036854775808 [1, 2, 3 :: Int64] @?= Nothing
        sizedArrayFromList @18446744073709551619 [1, 2, 3 :: Int64] @?= Nothing
        sizedArray @18446744073709551619 (listArray (0, 2) [1, 2, 3 :: Int64]) @?= Nothing
        query conn "SELECT typeof(?), ?" (values, values) >>= (@?= [("BIGINT[3]" :: Text.Text, values)])
        _ <- execute_ conn "CREATE TABLE stored (v BIGINT[3])"
        _ <- execute conn "INSERT INTO stored VALUES (?)" (Only values)
        [Only stored] <- query_ conn "SELECT v FROM stored"
        stored @?= values
        -- A value that is read has the bounds (0, n - 1).
        bounds (sizedArrayToArray stored) @?= (0, 2)
        sizedArrayToArray stored ! 1 @?= (2 :: Int64)
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
        let row = fromJust . sizedArrayFromList @3
            nested = fromJust (sizedArrayFromList @2 [row [1, 2, 3 :: Int32], row [4, 5, 6]])
        query conn "SELECT typeof(?), ?" (nested, nested) >>= (@?= [("INTEGER[3][2]" :: Text.Text, nested)])
        let optional = fromJust (sizedArrayFromList @3 [Just 1, Nothing, Just (3 :: Int64)])
        query conn "SELECT typeof(?), ?" (optional, optional) >>= (@?= [("BIGINT[3]" :: Text.Text, optional)])
    , testCase "a SizedArray of size 0 raises an input error at the bind and at the declaration" $ withConnection ":memory:" \conn -> do
        let empty = fromJust (sizedArrayFromList @0 ([] :: [Int64]))
        assertInputErrorContaining "an empty ARRAY parameter is not possible" (query conn "SELECT ?" (Only empty) :: IO [Only (SizedArray 0 Int64)])
        assertInputErrorContaining "a SizedArray of size 0 has no ARRAY type" $
          createFunction conn "sized_empty" \(_ :: SizedArray 0 Int64) -> (1 :: Int64)
    , testCase "a SizedArray of the largest ARRAY size binds and declares, and a larger one raises an input error" $ withConnection ":memory:" \conn -> do
        let largest = fromJust (sizedArrayFromList @100000 (replicate 100000 (1 :: Int8)))
            above = fromJust (sizedArrayFromList @100001 (replicate 100001 (1 :: Int8)))
        query conn "SELECT typeof(?), len(?)" (largest, largest) >>= (@?= [("TINYINT[100000]" :: Text.Text, 100000 :: Int64)])
        createFunction conn "sized_largest" \(v :: SizedArray 100000 Int8) -> length v
        query conn "SELECT sized_largest(?)" (Only largest) >>= (@?= [Only (100000 :: Int)])
        -- The value API of DuckDB would bind the larger value, but its SQL has no such type.
        assertInputErrorContaining
          "an ARRAY parameter of 100001 elements is not possible, because a DuckDB ARRAY has at most 100000 elements"
          (query conn "SELECT len(?)" (Only above) :: IO [Only Int64])
        assertInputErrorContaining "a SizedArray of size 100001 has no ARRAY type, because a DuckDB ARRAY has at most 100000 elements" $
          createFunction conn "sized_above" \(v :: SizedArray 100001 Int8) -> length v
    , testCase "generic ARRAY bounds are checked for NULL, nested containers, and a Nat above Word64" $ withConnection ":memory:" \conn -> do
        assertInputErrorContaining "a DuckDB ARRAY has at most 100000 elements" (query conn "SELECT ?" (Only (Above Nothing)) :: IO [Only Above])
        assertInputErrorContaining
          "a DuckDB ARRAY has at most 100000 elements"
          (query conn "SELECT ?" (Only (Overflow Nothing)) :: IO [Only Overflow])
        let above = Above (Just (fromJust (sizedArrayFromList @100001 (replicate 100001 (1 :: Int64)))))
        assertInputErrorContaining "an ARRAY parameter of 100001 elements is not possible" (query conn "SELECT ?" (Only above) :: IO [Only Above])
        assertInputErrorContaining "a DuckDB ARRAY must have 1 to 100000 elements" $
          createFunction conn "generic_above" \(_ :: Above) -> (1 :: Int64)
        assertInputErrorContaining "a DuckDB ARRAY must have 1 to 100000 elements" $
          createFunction conn "generic_overflow" \(_ :: Overflow) -> (1 :: Int64)
        assertInputErrorContaining "a DuckDB ARRAY must have 1 to 100000 elements" $
          createFunction conn "generic_nested_above" \(_ :: NestedAbove) -> (1 :: Int64)
        assertInputErrorContaining "a DuckDB ARRAY has at most 100000 elements" $
          R.registerCustomType
            conn
            "invalid_array"
            (StructuredType (LogicalTypeAlias "invalid_array_base" (LogicalTypeArray (LogicalTypeScalar LogicalTypeIdBigInt) 100001)))
        case duckLogicalType (Proxy @(SizedArray 18446744073709551617 Int64)) of
          LogicalTypeArray _ size -> size @?= maxBound
          logical -> assertFailure ("expected an invalid ARRAY size sentinel, got " <> show logical)
    , testCase "a list goes into an ARRAY column of its length and reads an ARRAY of each size" $ withConnection ":memory:" \conn -> do
        _ <- execute_ conn "CREATE TABLE fixed (v BIGINT[3])"
        _ <- execute conn "INSERT INTO fixed VALUES (?)" (Only [1, 2, 3 :: Int64])
        query conn "SELECT count(*) FROM fixed WHERE v = ?" (Only [1, 2, 3 :: Int64]) >>= (@?= [Only (1 :: Int64)])
        -- DuckDB casts a list of another length to the column with an error.
        try (execute conn "INSERT INTO fixed VALUES (?)" (Only [1, 2 :: Int64])) >>= \case
          Left SQLError{sqlErrorType, sqlErrorMessage} -> do
            sqlErrorType @?= Just ErrorTypeConversion
            assertBool (Text.unpack sqlErrorMessage) ("Cannot cast list with length 2 to array with length 3" `Text.isInfixOf` sqlErrorMessage)
          Right count -> assertFailure ("expected the cast error of DuckDB, got " <> show count)
        query_ conn "SELECT [1, 2]::BIGINT[2], [1, 2, 3]::BIGINT[3]" >>= (@?= [([1, 2], [1, 2, 3]) :: ([Int64], [Int64])])
    , testCase "a FieldArray binds an ARRAY of a size that the program gives at run time" $ withConnection ":memory:" \conn ->
        forM_ [1, 4, 7] \count -> do
          let value = FieldArray (listArray (0, count - 1) (map FieldInt64 [1 .. fromIntegral count]))
          query conn "SELECT typeof(?), len(?)" (value, value) >>= (@?= [(Text.pack ("BIGINT[" <> show count <> "]"), count)])
    , testCase "a SizedArray field of a record declares and reads the ARRAY of its size" $ withConnection ":memory:" \conn -> do
        let row = Sized (fromJust (sizedArrayFromList @3 [1, 2, 3]))
        query conn "SELECT typeof(?), ?" (row, row) >>= (@?= [("STRUCT(sizedValues BIGINT[3])" :: Text.Text, row)])
        createFunction conn "sized_field" \(Sized sized) -> sum sized
        query conn "SELECT typeof(sized_field(?)), sized_field(?)" (row, row) >>= (@?= [("BIGINT" :: Text.Text, 6 :: Int64)])
        try (query_ conn "SELECT {'sizedValues': [1, 2]::BIGINT[2]}" :: IO [Only Sized]) >>= \case
          Left SQLError{sqlErrorMessage} ->
            assertBool
              (Text.unpack sqlErrorMessage)
              ("has type BIGINT[2] but expected SizedArray 3: the value has 2 elements" `Text.isInfixOf` sqlErrorMessage)
          Right rows -> assertFailure ("expected the size error, got " <> show rows)
        try (query_ conn "SELECT {'sizedValues': ['x', 'y']::VARCHAR[]}" :: IO [Only Sized]) >>= \case
          Left SQLError{sqlErrorMessage} ->
            assertBool
              (Text.unpack sqlErrorMessage)
              ("has type VARCHAR[] but expected SizedArray 3: the value has 2 elements" `Text.isInfixOf` sqlErrorMessage)
          Right rows -> assertFailure ("expected the size error before an element error, got " <> show rows)
    ]
