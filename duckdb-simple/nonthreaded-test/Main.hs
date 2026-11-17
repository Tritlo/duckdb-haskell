{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Check the guard for a program that is not linked with @-threaded@. This
-- suite has no @-threaded@ in its @ghc-options@. See Note [Callbacks without
-- the threaded runtime] in "Database.DuckDB.Simple.Callback".
module Main (main) where

import Control.Exception (try)
import Control.Monad (unless, void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Text as Text
import Database.DuckDB.Simple
import qualified Database.DuckDB.Simple.Copy as Copy
import qualified Database.DuckDB.Simple.Logging as Logging
import qualified Database.DuckDB.Simple.V2.Copy as V2Copy
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
    checkGuard "COPY" $ Copy.registerCopyFromFunction conn "lines" (\_ -> pure [Only (1 :: Int64)])
    let noop _ _ = pure ()
    checkGuard "COPY" $ V2Copy.registerCopyToCallbacks conn "raw_lines" (V2Copy.copyToCallbacks noop noop)
    flushCount <- newIORef (0 :: Int)
    checkGuard "log sink" $ Logging.registerLogSink conn "events" (const (pure ())) (modifyIORef' flushCount (+ 1))
    flushed <- readIORef flushCount
    unless (flushed == 1) (failWith ("rejected log sink: expected one flush, got " <> show flushed))
  where
    checkGuard kind action =
      try action >>= \result -> case result of
        Left SQLError{sqlErrorType = Just ErrorInputInvalid, sqlErrorMessage}
          | (kind <> " callbacks require a program linked with -threaded") `Text.isInfixOf` sqlErrorMessage -> pure ()
        other -> failWith ("threads = 1: expected the " <> Text.unpack kind <> " guard, got " <> show (other :: Either SQLError ()))
    failWith message = putStrLn message >> exitFailure
