{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Advanced
-- Description : Register SQL objects with typed Haskell functions on the loading DuckDB host.
--
-- Each registration uses a function of "Database.DuckDB.Simple.Extension". The
-- module makes no native V2 call.
module Advanced (register) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import Database.DuckDB.Simple (Aggregate (..), Only (..))
import qualified Database.DuckDB.Simple.Copy as Copy
import Database.DuckDB.Simple.Extension (Extension)
import qualified Database.DuckDB.Simple.Extension as Extension
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..))
import Database.DuckDB.Simple.Registration (Replacement (..), TypeDeclaration (..))

-- | Register an aggregate, a table function, a custom type and its cast, a
-- replacement scan, a COPY format, and a multi-file reader.
register :: Extension -> IO ()
register extension = do
  registerSum extension
  registerSquares extension
  registerShifted extension
  Extension.registerReplacementScan extension replaceMissing
  registerLines extension
  registerLineFiles extension

-- | Register the aggregate @haskell_sum(value)@. It adds the values that are
-- not NULL. The step and the combine functions report overflow.
registerSum :: Extension -> IO ()
registerSum extension =
  Extension.createAggregate
    extension
    "haskell_sum"
    Aggregate
      { aggregateInitial = 0 :: Int64
      , aggregateStep = \total (Only value) -> addOrThrow "haskell_sum" total value
      , aggregateCombine = addOrThrow "haskell_sum"
      , aggregateResult = pure
      }

-- | Register the table function @haskell_squares(count)@ with the columns @n@
-- and @square@. A negative count is an error. DuckDB reads the list one chunk at
-- a time.
registerSquares :: Extension -> IO ()
registerSquares extension =
  Extension.createTableFunction extension "haskell_squares" ["n", "square"] \(Only count) -> do
    when (count < 0) $ throwIO (userError ("haskell_squares: the count is negative: " <> show count))
    pure [(n, n * n) | n <- [0 .. count - 1 :: Int64]]

-- | Register the custom type @haskell_shifted@ with the base type BIGINT. Then
-- register a typed cast from BIGINT to @haskell_shifted@ that adds 100. An
-- overflow fails CAST. TRY_CAST gives NULL for the overflow.
registerShifted :: Extension -> IO ()
registerShifted extension = do
  Extension.registerCustomType extension "haskell_shifted" (TypeExpression "BIGINT")
  Extension.createCast extension (TypeExpression "BIGINT") (TypeExpression "haskell_shifted") \value ->
    maybe (Left ("haskell_shifted: BIGINT overflow in " <> Text.pack (show value) <> " + 100")) Right (addBigInt value 100)

-- | Replace the unknown table @haskell_missing@ with a query. Reject the
-- reserved name @haskell_reserved@. Ignore all other names.
replaceMissing :: [Text] -> IO (Maybe Replacement)
replaceMissing name = case name of
  ["haskell_missing"] -> pure (Just (ReplaceWithQuery "SELECT 42::BIGINT AS amount"))
  ["haskell_reserved"] -> throwIO (userError "haskell_reserved: the table name is reserved")
  _ -> pure Nothing

-- | Register the COPY format @haskell_lines@ in both directions. A file has one
-- BIGINT value on each line. An empty line is NULL. One registration gives the two
-- directions, because DuckDB keeps only the first registration of a name.
registerLines :: Extension -> IO ()
registerLines extension = Extension.registerCopyFunction extension "haskell_lines" copyTo (readLines . Copy.copyFromPath)
  where
    copyTo =
      Copy.CopyTo
        { Copy.copyToBind = \info ->
            unless (Copy.copyBindColumnTypes info == [Copy.LogicalTypeIdBigInt]) $ throwIO (userError oneColumn)
        , Copy.copyToInit = \info -> writeFile (Copy.copyInitFilePath info) "" >> pure (Copy.copyInitFilePath info)
        , Copy.copyToSink = \info rows -> mapM line rows >>= appendFile (Copy.copySinkGlobalState info) . unlines
        , Copy.copyToFinalize = \_ -> pure ()
        }
    line [Field{fieldValue = FieldInt64 value}] = pure (show value)
    line [Field{fieldValue = FieldNull}] = pure ""
    line _ = throwIO (userError oneColumn)
    oneColumn = "haskell_lines writes exactly one BIGINT column"

-- | Register the table function @haskell_read_line_file(path)@. It reads one
-- file in the format of @haskell_lines@. Then register the multi-file function
-- @haskell_read_lines@, which reads a list of files or a glob pattern.
registerLineFiles :: Extension -> IO ()
registerLineFiles extension = do
  Extension.createTableFunction extension "haskell_read_line_file" ["value"] \(Only path) -> readLines (Text.unpack path)
  Extension.registerMultiFileFunction extension "haskell_read_lines" "haskell_read_line_file" Copy.defaultMultiFileOptions

-- | Read a file with one BIGINT value on each line. An empty line is NULL.
readLines :: FilePath -> IO [Only (Maybe Int64)]
readLines path = TextIO.readFile path >>= mapM parse . Text.lines
  where
    parse line
      | Text.null line = pure (Only Nothing)
      | otherwise = case TextRead.signed TextRead.decimal line of
          Right (value, rest) | Text.null rest -> pure (Only (Just value))
          _ -> throwIO (userError ("haskell_lines: the line is not a BIGINT: " <> Text.unpack line))

-- | Add two BIGINT values. Overflow gives 'Nothing'.
addBigInt :: Int64 -> Int64 -> Maybe Int64
addBigInt a b
  | b > 0 && a > maxBound - b = Nothing
  | b < 0 && a < minBound - b = Nothing
  | otherwise = Just (a + b)

-- | Add two BIGINT values. Report overflow as an error of the SQL object @name@.
addOrThrow :: String -> Int64 -> Int64 -> IO Int64
addOrThrow name a b = maybe (throwIO (userError (name <> ": BIGINT overflow in " <> show a <> " + " <> show b))) pure (addBigInt a b)
