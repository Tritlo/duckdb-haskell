{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : Database.DuckDB.Simple.FromRow
Description : Convert rows of 'Field's into Haskell values.
-}
module Database.DuckDB.Simple.FromRow (
    FromRow (..),
    resultErrorToSqlError,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.FromField
import Database.DuckDB.Simple.Internal (SQLError (..))
import Database.DuckDB.Simple.Types (Only (..), Query)

-- | Types that can be constructed from a list of 'Field's.
class FromRow a where
    fromRow :: [Field] -> Either ResultError a

instance FromRow () where
    fromRow fields
        | null fields = Right ()
        | otherwise =
            Left
                ColumnCountMismatch
                    { resultErrorExpectedCols = 0
                    , resultErrorActualCols = length fields
                    }

instance (FromField a) => FromRow (Only a) where
    fromRow [field] = Only <$> fromField field
    fromRow fields =
        Left
            ColumnCountMismatch
                { resultErrorExpectedCols = 1
                , resultErrorActualCols = length fields
                }

instance (FromField a, FromField b) => FromRow (a, b) where
    fromRow [f1, f2] = (,) <$> fromField f1 <*> fromField f2
    fromRow fields =
        Left
            ColumnCountMismatch
                { resultErrorExpectedCols = 2
                , resultErrorActualCols = length fields
                }

instance (FromField a, FromField b, FromField c) => FromRow (a, b, c) where
    fromRow [f1, f2, f3] = (,,) <$> fromField f1 <*> fromField f2 <*> fromField f3
    fromRow fields =
        Left
            ColumnCountMismatch
                { resultErrorExpectedCols = 3
                , resultErrorActualCols = length fields
                }

instance (FromField a, FromField b, FromField c, FromField d) => FromRow (a, b, c, d) where
    fromRow [f1, f2, f3, f4] = (,,,) <$> fromField f1 <*> fromField f2 <*> fromField f3 <*> fromField f4
    fromRow fields =
        Left
            ColumnCountMismatch
                { resultErrorExpectedCols = 4
                , resultErrorActualCols = length fields
                }

instance (FromField a) => FromRow [a] where
    fromRow = traverse fromField

-- | Convert a 'ResultError' into a user-facing 'SQLError'.
resultErrorToSqlError :: Query -> ResultError -> SQLError
resultErrorToSqlError query err =
    SQLError
        { sqlErrorMessage = renderError err
        , sqlErrorType = Nothing
        , sqlErrorQuery = Just query
        }

renderError :: ResultError -> Text
renderError = \case
    IncompatibleType{resultErrorColumn, resultErrorExpected, resultErrorActual} ->
        Text.pack $
            concat
                [ "duckdb-simple: column "
                , show (humanIndex resultErrorColumn)
                , " has type "
                , resultErrorActual
                , " but expected "
                , resultErrorExpected
                ]
    UnexpectedNull{resultErrorColumn, resultErrorExpected} ->
        Text.pack $
            concat
                [ "duckdb-simple: column "
                , show (humanIndex resultErrorColumn)
                , " is NULL but expected "
                , resultErrorExpected
                ]
    ColumnCountMismatch{resultErrorExpectedCols, resultErrorActualCols} ->
        Text.pack $
            concat
                [ "duckdb-simple: expected "
                , show resultErrorExpectedCols
                , " columns but query returned "
                , show resultErrorActualCols
                ]
    ConversionError{resultErrorColumn, resultErrorMessage} ->
        Text.concat
            [ Text.pack "duckdb-simple: column "
            , Text.pack (show (humanIndex resultErrorColumn))
            , Text.pack ": "
            , resultErrorMessage
            ]

humanIndex :: Int -> Int
humanIndex = (+ 1)
