{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Database.DuckDB.Simple
import Database.DuckDB.Simple.FromField (FieldValue (..))
import Database.DuckDB.Simple.Internal (c_duckdb_v2_prepared_statement_reuses_plan, output, withStatementHandle)
import Database.DuckDB.Simple.Variant (Variant (..), variantObject)

-- | Check metadata plan reuse before and after varied parameter bindings.
main :: IO ()
main = withConnection ":memory:" $ \conn -> do
  let sql =
        "SELECT system.main.list_transform(?::VARIANT[], lambda value: system.main.struct_pack(tag := system.main.variant_typeof(value), keys := system.main.variant_keys(value))) AS metadata"
      fixtures =
        [ []
        , [Variant FieldNull]
        , [Variant (FieldInt64 7), Variant (FieldText "x")]
        , [Variant (FieldList [FieldInt8 3, FieldNull]), Variant (variantObject [("a", FieldInt64 4)])]
        , replicate 4096 (Variant (FieldInt64 9))
        , [Variant (variantObject [("", FieldWord64 1), ("A", FieldWord64 2), ("a", FieldWord64 3)])]
        , []
        ]
      reusable stmt label = do
        reuses <- withStatementHandle stmt (output . c_duckdb_v2_prepared_statement_reuses_plan)
        putStrLn (label <> ": " <> show reuses)
        unless (reuses /= 0) (fail "VARIANT metadata plan cannot be reused")
  withStatement conn sql $ \stmt -> do
    reusable stmt "before binding"
    forM_ fixtures $ \values -> do
      expected <- query conn sql (Only values) :: IO [Only FieldValue]
      bind stmt (toRow (Only values))
      reusable stmt ("after binding " <> show (length values))
      actual <- nextRow stmt :: IO (Maybe (Only FieldValue))
      end <- nextRow stmt :: IO (Maybe (Only FieldValue))
      unless (maybe [] pure actual == expected && end == Nothing) (fail "metadata changed between fresh and reused plans")
      reusable stmt ("after execution " <> show (length values))
