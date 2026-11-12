{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Register functions with native V2 callbacks on the loading DuckDB host.
These registrations use the 'RegistrationTarget' of the extension. The
aggregate reads and writes vectors one row at a time. This is simple but slow.
All other registrations use the typed functions of 'Extension'.
-}
module Advanced (register) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, forM_, unless, when)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import Database.DuckDB.FFI.V2
import Database.DuckDB.Simple (Only (..))
import qualified Database.DuckDB.Simple.Copy as Copy
import Database.DuckDB.Simple.Extension (Extension)
import qualified Database.DuckDB.Simple.Extension as Extension
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (..))
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (LogicalTypeScalar))
import Database.DuckDB.Simple.Registration (Replacement (..))
import Database.DuckDB.Simple.V2.Registration (RegistrationTarget, TypeDeclaration (..), checked, destroy, output)
import qualified Database.DuckDB.Simple.V2.Registration as Registration
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)

{- | Register an aggregate, a table function, a custom type and its cast, a
replacement scan, a COPY format, and a multi-file reader.
-}
register :: Extension -> IO ()
register extension = do
    Registration.withRegistrationFactory target (registerSum target)
    registerSquares extension
    registerShifted extension
    Extension.registerReplacementScan extension replaceMissing
    registerLines extension
    registerLineFiles extension
  where
    target = Extension.extensionTarget extension

-- | The BIGINT type of all values in this module.
bigint :: TypeDeclaration
bigint = StructuredType (LogicalTypeScalar DuckDBV2LogicalTypeIdBigInt)

{- | Register the aggregate @haskell_sum(value)@. It adds the values that are
not NULL. The update and combine callbacks report overflow.
-}
registerSum :: RegistrationTarget -> DuckDBV2FactoryHandle -> IO ()
registerSum target factory =
    Registration.registerAggregateFunctionWith target configure (Registration.aggregateCallbacks stateSize initialize update combine finalize)
  where
    configure function = do
        Registration.withIdentifier "haskell_sum" (checked . c_duckdb_v2_aggregate_function_set_name function)
        output (c_duckdb_v2_aggregate_function_get_signature function)
            >>= Registration.configureFunctionSignature factory (Registration.FunctionSignature [Registration.parameter "value" bigint] (Just bigint))
    stateSize info = checked (c_duckdb_v2_aggregate_function_size_set_state_size info (fromIntegral (sizeOf (0 :: Int64))))
    initialize info = do
        count <- output (c_duckdb_v2_aggregate_function_init_get_state_count info)
        states <- output (c_duckdb_v2_aggregate_function_init_get_states info)
        forM_ [0 .. fromIntegral count - 1] \index -> peekElemOff states index >>= \state -> poke (castPtr state) (0 :: Int64)
    update info = do
        count <- output (c_duckdb_v2_aggregate_function_update_get_row_count info)
        states <- output (c_duckdb_v2_aggregate_function_update_get_states info)
        values <- output (c_duckdb_v2_aggregate_function_update_get_arg info 0) >>= readColumn count
        forM_ (zip [0 ..] values) \(index, value) -> forM_ value \number -> do
            state <- castPtr <$> peekElemOff states index
            total <- peek state
            addOrThrow "haskell_sum" total number >>= poke state
    combine info = do
        count <- output (c_duckdb_v2_aggregate_function_combine_get_state_count info)
        sources <- output (c_duckdb_v2_aggregate_function_combine_get_sources info)
        targets <- output (c_duckdb_v2_aggregate_function_combine_get_targets info)
        forM_ [0 .. fromIntegral count - 1] \index -> do
            source <- peekElemOff sources index >>= peek . castPtr
            state <- castPtr <$> peekElemOff targets index
            total <- peek state
            addOrThrow "haskell_sum" total source >>= poke state
    finalize info = do
        count <- output (c_duckdb_v2_aggregate_function_finalize_get_state_count info)
        states <- output (c_duckdb_v2_aggregate_function_finalize_get_states info)
        offset <- output (c_duckdb_v2_aggregate_function_finalize_get_result_offset info)
        result <- output (c_duckdb_v2_aggregate_function_finalize_get_result info)
        raw <- output (c_duckdb_v2_vector_get_data_mutable result)
        forM_ [0 .. fromIntegral count - 1] \index -> do
            total <- peekElemOff states index >>= peek . castPtr
            pokeElemOff (castPtr raw) (fromIntegral offset + index) (total :: Int64)

{- | Register the table function @haskell_squares(count)@ with the columns @n@
and @square@. A negative count is an error. DuckDB reads the list one chunk at
a time.
-}
registerSquares :: Extension -> IO ()
registerSquares extension =
    Extension.createTableFunction extension "haskell_squares" ["n", "square"] \(Only count) -> do
        when (count < 0) $ throwIO (userError ("haskell_squares: the count is negative: " <> show count))
        pure [(n, n * n) | n <- [0 .. count - 1 :: Int64]]

{- | Register the custom type @haskell_shifted@ with the base type BIGINT. Then
register a typed cast from BIGINT to @haskell_shifted@ that adds 100. An
overflow fails CAST. TRY_CAST gives NULL for the overflow.
-}
registerShifted :: Extension -> IO ()
registerShifted extension = do
    Extension.registerCustomType extension "haskell_shifted" (TypeExpression "BIGINT")
    Extension.createCast extension (TypeExpression "BIGINT") (TypeExpression "haskell_shifted") \value ->
        maybe (Left ("haskell_shifted: BIGINT overflow in " <> Text.pack (show value) <> " + 100")) Right (addBigInt value 100)

{- | Replace the unknown table @haskell_missing@ with a query. Reject the
reserved name @haskell_reserved@. Ignore all other names.
-}
replaceMissing :: [Text] -> IO (Maybe Replacement)
replaceMissing name = case name of
    ["haskell_missing"] -> pure (Just (ReplaceWithQuery "SELECT 42::BIGINT AS amount"))
    ["haskell_reserved"] -> throwIO (userError "haskell_reserved: the table name is reserved")
    _ -> pure Nothing

{- | Register the COPY format @haskell_lines@ in both directions. A file has one
BIGINT value on each line. An empty line is NULL. One registration gives the two
directions, because DuckDB keeps only the first registration of a name.
-}
registerLines :: Extension -> IO ()
registerLines extension = Extension.registerCopyFunction extension "haskell_lines" copyTo (readLines . Copy.copyFromPath)
  where
    copyTo =
        Copy.CopyTo
            { Copy.copyToBind = \info ->
                unless (length (Copy.copyBindColumnTypes info) == 1) $ throwIO (userError oneColumn)
            , Copy.copyToInit = \info -> writeFile (Copy.copyInitFilePath info) "" >> pure (Copy.copyInitFilePath info)
            , Copy.copyToSink = \info rows -> mapM line rows >>= appendFile (Copy.copySinkGlobalState info) . unlines
            , Copy.copyToFinalize = \_ -> pure ()
            }
    line [Field{fieldValue = FieldInt64 value}] = pure (show value)
    line [Field{fieldValue = FieldNull}] = pure ""
    line _ = throwIO (userError oneColumn)
    oneColumn = "haskell_lines writes exactly one BIGINT column"

{- | Register the table function @haskell_read_line_file(path)@. It reads one
file in the format of @haskell_lines@. Then register the multi-file function
@haskell_read_lines@, which reads a list of files or a glob pattern.
-}
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

-- | Read the first rows of a BIGINT vector. A NULL row becomes 'Nothing'.
readColumn :: DuckDBIdx -> DuckDBV2VectorHandle -> IO [Maybe Int64]
readColumn count vector =
    forM (takeWhile (< count) [0 ..]) \row ->
        bracket (output (c_duckdb_v2_vector_get_value vector row)) (destroy c_duckdb_v2_value_destroy) \value -> do
            isNull <- output (c_duckdb_v2_value_is_null value)
            if isNull /= 0 then pure Nothing else Just <$> output (c_duckdb_v2_value_get_bigint value)
