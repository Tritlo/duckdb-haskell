{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Check the guard for a program that is not linked with @-threaded@. This
-- suite has no @-threaded@ in its @ghc-options@. See Note [Callbacks without
-- the threaded runtime] in "Database.DuckDB.Simple.Callback".
module Main (main) where

import Control.Exception (try)
import Control.Monad (unless, void)
import Data.Int (Int64)
import qualified Data.Text as Text
import Database.DuckDB.Simple
import System.Exit (exitFailure)

main :: IO ()
main = do
  -- With the setting threads above 1, the registration raises the input error.
  withConnection ":memory:" \conn -> do
    void (execute_ conn "SET threads = 4")
    result <- try (createFunction conn "plus_one" ((+ 1) :: Int64 -> Int64))
    case result of
      Left SQLError{sqlErrorType = Just ErrorInputInvalid, sqlErrorMessage}
        | "not linked with -threaded" `Text.isInfixOf` sqlErrorMessage -> pure ()
      other -> failWith ("threads = 4: expected the error of the guard, got " <> show other)
  -- With the setting threads at 1, a query of a registered function runs.
  withConnection ":memory:" \conn -> do
    void (execute_ conn "SET threads = 1")
    createFunction conn "plus_one" ((+ 1) :: Int64 -> Int64)
    rows <- query_ conn "SELECT sum(plus_one(i))::BIGINT FROM range(100000) t(i)" :: IO [Only Int64]
    unless (rows == [Only 5000050000]) (failWith ("threads = 1: expected 5000050000, got " <> show rows))
  where
    failWith message = putStrLn message >> exitFailure
