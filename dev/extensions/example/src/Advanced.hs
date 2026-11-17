{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Data.Geometry as G
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import Database.DuckDB.Simple (Aggregate (..), Only (..))
import qualified Database.DuckDB.Simple.Copy as Copy
import Database.DuckDB.Simple.Extension (Extension)
import qualified Database.DuckDB.Simple.Extension as Extension
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..), FromField (..), ResultError (ConversionFailed), returnError)
import Database.DuckDB.Simple.Function (FunctionResult)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeId (LogicalTypeIdBigInt), LogicalTypeRep (..))
import Database.DuckDB.Simple.Registration (Replacement (..), TypeDeclaration (..))
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), ToDuckValue)
import Database.DuckDB.Simple.Variant (Variant (..))

-- | Register an aggregate, a table function, a custom type, a replacement scan,
-- a COPY format, and a multi-file reader.
register :: Extension -> IO ()
register extension = do
  registerSum extension
  registerGeometryAggregates extension
  Extension.createFunction extension "haskell_variant_echo" (id :: Variant -> Variant)
  Extension.createAggregate extension "haskell_variant_sum" $
    Aggregate
      { aggregateInitial = 0 :: Int64
      , aggregateStep = \total (Only (Variant payload)) -> case payload of
          FieldInt64 value -> addOrThrow "haskell_variant_sum" total value
          _ -> throwIO (userError "haskell_variant_sum expects BIGINT payloads")
      , aggregateCombine = addOrThrow "haskell_variant_sum"
      , aggregateResult = pure
      }
  registerArgumentTypes extension
  registerSquares extension
  Extension.registerCustomType extension "haskell_amount" (TypeExpression "BIGINT")
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

-- | Register aggregates that add point X coordinates in GEOMETRY arguments.
-- The list aggregate ignores NULL list elements.
registerGeometryAggregates :: Extension -> IO ()
registerGeometryAggregates extension = do
  Extension.createAggregate extension "haskell_point_x_sum" $
    Aggregate
      { aggregateInitial = 0 :: Double
      , aggregateStep = \total (Only geometry) -> pure (total + pointX geometry)
      , aggregateCombine = \a b -> pure (a + b)
      , aggregateResult = pure
      }
  Extension.createAggregate extension "haskell_points_x_sum" $
    Aggregate
      { aggregateInitial = 0 :: Double
      , aggregateStep = \total (Only geometries) -> pure (total + sum (map (maybe 0 pointX) geometries))
      , aggregateCombine = \a b -> pure (a + b)
      , aggregateResult = pure
      }
  where
    pointX :: G.Geometry -> Double
    pointX (G.PointGeometry (G.PointXY (G.XY x _))) = x
    pointX _ = 0

-- | A BIGINT argument whose decoder reads its native metadata.
newtype ArgumentType = ArgumentType Int64
  deriving newtype (ToDuckValue)

instance DuckDBColumnType ArgumentType where
  duckdbColumnTypeFor _ = "BIGINT"

instance FromField ArgumentType where
  fromField field = do
    logical <- fieldType field
    if logical == LogicalTypeScalar LogicalTypeIdBigInt
      then ArgumentType <$> fromField field
      else returnError ConversionFailed field ("unexpected argument type: " <> Text.pack (show logical))

-- | A BIGINT argument with an alias equal to the canonical type name.
newtype CanonicalArgumentType = CanonicalArgumentType Int64
  deriving newtype (ToDuckValue, FunctionResult)

instance DuckDBColumnType CanonicalArgumentType where
  duckdbColumnTypeFor _ = "BIGINT"
  duckdbTypeDeclaration _ = Right (StructuredType (LogicalTypeAlias "BIGINT" (LogicalTypeScalar LogicalTypeIdBigInt)))

instance FromField CanonicalArgumentType where
  fromField field = do
    logical <- fieldType field
    if logical == LogicalTypeAlias "BIGINT" (LogicalTypeScalar LogicalTypeIdBigInt)
      then CanonicalArgumentType <$> fromField field
      else returnError ConversionFailed field ("unexpected canonical argument type: " <> Text.pack (show logical))

-- | Register aggregates that check the type of their update argument vector.
-- DuckDB casts an INTEGER to the ordinary BIGINT argument after bind. The
-- scalar constructor supplies values with the canonical BIGINT alias.
registerArgumentTypes :: Extension -> IO ()
registerArgumentTypes extension = do
  Extension.createFunction extension "haskell_canonical_bigint" CanonicalArgumentType
  Extension.createAggregate extension "haskell_argument_type" $
    Aggregate
      { aggregateInitial = 0 :: Int64
      , aggregateStep = \total (Only (ArgumentType value)) -> addOrThrow "haskell_argument_type" total value
      , aggregateCombine = addOrThrow "haskell_argument_type"
      , aggregateResult = pure
      }
  Extension.createAggregate extension "haskell_canonical_argument_type" $
    Aggregate
      { aggregateInitial = 0 :: Int64
      , aggregateStep = \total (Only (CanonicalArgumentType value)) -> addOrThrow "haskell_canonical_argument_type" total value
      , aggregateCombine = addOrThrow "haskell_canonical_argument_type"
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

-- | Replace the unknown table @haskell_missing@ with a query. Reject the
-- reserved name @haskell_reserved@. Ignore all other names.
replaceMissing :: [Text] -> IO (Maybe Replacement)
replaceMissing name = case name of
  ["haskell_missing"] -> pure (Just (ReplaceWithQuery "SELECT 42::BIGINT AS amount"))
  ["haskell_reserved"] -> throwIO (userError "haskell_reserved: the table name is reserved")
  _ -> pure Nothing

-- | Register the COPY format @haskell_lines@ in both directions. A file has one
-- BIGINT value on each line. It can also write a VARIANT column of BIGINT
-- values. An empty line is NULL. One registration gives the two
-- directions, because DuckDB keeps only the first registration of a name.
registerLines :: Extension -> IO ()
registerLines extension = Extension.registerCopyFunction extension "haskell_lines" copyTo (readLines . Copy.copyFromPath)
  where
    copyTo =
      Copy.CopyTo
        { Copy.copyToBind = \info ->
            unless (Copy.copyBindColumnTypes info `elem` [[Copy.LogicalTypeIdBigInt], [Copy.LogicalTypeIdVariant]]) $ throwIO (userError oneColumn)
        , Copy.copyToInit = \info -> writeFile (Copy.copyInitFilePath info) "" >> pure (Copy.copyInitFilePath info)
        , Copy.copyToSink = \info rows -> mapM line rows >>= appendFile (Copy.copySinkGlobalState info) . unlines
        , Copy.copyToFinalize = \_ -> pure ()
        }
    line [Field{fieldValue = FieldInt64 value}] = pure (show value)
    line [Field{fieldValue = FieldNull}] = pure ""
    line _ = throwIO (userError oneColumn)
    oneColumn = "haskell_lines writes one BIGINT or VARIANT column with BIGINT or NULL values"

-- | Register the table function @haskell_read_line_file(path)@. It reads one
-- file in the format of @haskell_lines@. Then register the multi-file function
-- @haskell_read_lines@, which reads a list of files or a glob pattern.
registerLineFiles :: Extension -> IO ()
registerLineFiles extension = do
  Extension.createTableFunction extension "haskell_read_line_file" ["value"] \(Only path) -> readLines (Text.unpack path)
  Extension.registerMultiFileFunction extension "haskell_read_lines" "haskell_read_line_file" Copy.defaultMultiFileOptions

-- | Read a file with one BIGINT value on each line. An empty line is NULL.
-- Reject a number outside the BIGINT range before converting it to 'Int64'.
readLines :: FilePath -> IO [Only (Maybe Int64)]
readLines path = TextIO.readFile path >>= mapM parse . Text.lines
  where
    parse line
      | Text.null line = pure (Only Nothing)
      | otherwise = case (TextRead.signed TextRead.decimal line :: Either String (Integer, Text)) of
          Right (value, rest)
            | Text.null rest
            , value >= toInteger (minBound :: Int64)
            , value <= toInteger (maxBound :: Int64) ->
                pure (Only (Just (fromInteger value)))
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
