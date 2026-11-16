{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Database.DuckDB.Simple.FromRow
-- Description : Parse rows of @Field@s as Haskell values.
module Database.DuckDB.Simple.FromRow (
  -- * Row parsing
  RowParser (..),
  field,
  fieldWith,
  numFieldsRemaining,
  parseRow,

  -- * Generic derivation
  GFromRow (..),
  FromRow (..),

  -- * Error conversion
  resultErrorToSqlError,
  rowErrorsToSqlError,
) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..), SomeException, fromException)
import Control.Monad (MonadPlus, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as Text
import Database.DuckDB.Simple.Conversion (Conversion, conversionError)
import Database.DuckDB.Simple.FromField
import Database.DuckDB.Simple.Internal (SQLError (..))
import Database.DuckDB.Simple.Types (Only (..), Query, (:.) (..))
import GHC.Generics

-- | Row parsing environment (read-only data available to the parser).
newtype RowParseRO = RowParseRO
  { rowParseColumnCount :: Int
  }

-- | Column-out-of-bounds sentinel used internally to map parser failures.
newtype ColumnOutOfBounds = ColumnOutOfBounds {columnOutOfBoundsIndex :: Int}
  deriving stock (Eq, Show)

instance Exception ColumnOutOfBounds

-- | Parser used by @FromRow@ implementations.
newtype RowParser a = RowParser
  { runRowParser :: ReaderT RowParseRO (StateT (Int, [Field]) Conversion) a
  }
  deriving stock (Functor)
  deriving newtype (Applicative, Alternative, Monad, MonadPlus)

-- | Generic derivation helper mirroring @sqlite-simple@.
class GFromRow f where
  gFromRow :: RowParser (f p)

instance GFromRow U1 where
  gFromRow = pure U1

instance (GFromRow a) => GFromRow (M1 i c a) where
  gFromRow = M1 <$> gFromRow

instance (FromField a) => GFromRow (K1 i a) where
  gFromRow = K1 <$> field

instance (GFromRow a, GFromRow b) => GFromRow (a :*: b) where
  gFromRow = (:*:) <$> gFromRow <*> gFromRow

-- | Types that can be constructed from database rows.
class FromRow a where
  fromRow :: RowParser a
  default fromRow :: (Generic a, GFromRow (Rep a)) => RowParser a
  fromRow = to <$> gFromRow

-- | Read the next field with the supplied @FieldParser@.
fieldWith :: FieldParser a -> RowParser a
fieldWith fieldParser = RowParser $ do
  RowParseRO{rowParseColumnCount} <- ask
  (columnIndex, remaining) <- lift get
  case remaining of
    [] -> do
      lift (put (columnIndex + 1, []))
      lift (lift (conversionError (ColumnOutOfBounds (columnIndex + 1))))
    (f : rest) -> do
      lift (put (columnIndex + 1, rest))
      if columnIndex >= rowParseColumnCount
        then lift (lift (conversionError (ColumnOutOfBounds (columnIndex + 1))))
        else lift (lift (fieldParser f))

-- | Read the next field with its @FromField@ instance.
field :: (FromField a) => RowParser a
field = fieldWith fromField

-- | Report how many columns remain unread in the current row.
numFieldsRemaining :: RowParser Int
numFieldsRemaining = RowParser $ do
  RowParseRO{rowParseColumnCount} <- ask
  (columnIndex, _) <- lift get
  pure (rowParseColumnCount - columnIndex)

-- | Execute a @RowParser@ against the provided row.
parseRow :: RowParser a -> [Field] -> Conversion a
parseRow parser fields =
  let context = RowParseRO (length fields)
      initialState = (0, fields)
  in  do
        (value, (columnCount, _)) <- runStateT (runReaderT (runRowParser parser) context) initialState
        if columnCount == length fields
          then pure value
          else conversionError (ColumnOutOfBounds (columnCount + 1))

instance FromRow () where
  fromRow = pure ()

instance (FromField a) => FromRow (Only a) where
  fromRow = Only <$> field

instance (FromRow a, FromRow b) => FromRow (a :. b) where
  fromRow = (:.) <$> fromRow <*> fromRow

instance (FromField a, FromField b) => FromRow (a, b) where
  fromRow = (,) <$> field <*> field

instance (FromField a, FromField b, FromField c) => FromRow (a, b, c) where
  fromRow = (,,) <$> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d) => FromRow (a, b, c, d) where
  fromRow = (,,,) <$> field <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e) => FromRow (a, b, c, d, e) where
  fromRow = (,,,,) <$> field <*> field <*> field <*> field <*> field

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f) => FromRow (a, b, c, d, e, f) where
  fromRow = (,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g)
  => FromRow (a, b, c, d, e, f, g)
  where
  fromRow = (,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h)
  => FromRow (a, b, c, d, e, f, g, h)
  where
  fromRow = (,,,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance
  ( FromField a
  , FromField b
  , FromField c
  , FromField d
  , FromField e
  , FromField f
  , FromField g
  , FromField h
  , FromField i
  )
  => FromRow (a, b, c, d, e, f, g, h, i)
  where
  fromRow =
    (,,,,,,,,)
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance
  ( FromField a
  , FromField b
  , FromField c
  , FromField d
  , FromField e
  , FromField f
  , FromField g
  , FromField h
  , FromField i
  , FromField j
  )
  => FromRow (a, b, c, d, e, f, g, h, i, j)
  where
  fromRow =
    (,,,,,,,,,)
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance (FromField a) => FromRow [a] where
  fromRow = do
    remaining <- numFieldsRemaining
    replicateM remaining field

-- | Convert a @ResultError@ into a user-facing @SQLError@.
resultErrorToSqlError :: Query -> ResultError -> SQLError
resultErrorToSqlError query err =
  SQLError
    { sqlErrorMessage = Text.pack (displayException err)
    , sqlErrorType = Nothing
    , sqlErrorQuery = Just query
    , sqlErrorBody = Nothing
    }

-- | Convert parser errors to an @SQLError@ that retains the query.
rowErrorsToSqlError :: Query -> [SomeException] -> SQLError
rowErrorsToSqlError query errs =
  case listToMaybe (mapMaybe (fromException :: SomeException -> Maybe ResultError) errs) of
    Just resultErr -> resultErrorToSqlError query resultErr
    Nothing ->
      case listToMaybe (mapMaybe (fromException :: SomeException -> Maybe ColumnOutOfBounds) errs) of
        Just (ColumnOutOfBounds idx) ->
          SQLError
            { sqlErrorMessage =
                Text.concat
                  [ "duckdb-simple: column index "
                  , Text.pack (show idx)
                  , " out of bounds"
                  ]
            , sqlErrorType = Nothing
            , sqlErrorQuery = Just query
            , sqlErrorBody = Nothing
            }
        Nothing ->
          SQLError
            { sqlErrorMessage =
                Text.pack $ "duckdb-simple: row-parsing failed:" <> show errs
            , sqlErrorType = Nothing
            , sqlErrorQuery = Just query
            , sqlErrorBody = Nothing
            }
