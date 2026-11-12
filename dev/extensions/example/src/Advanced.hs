{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Register functions with native V2 callbacks on the loading DuckDB host.
These registrations use the 'RegistrationTarget' of the extension. The
callbacks read and write vectors one row at a time. This is simple but slow.
-}
module Advanced (register) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, forM_, unless, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import Database.DuckDB.FFI.V2
import qualified Database.DuckDB.Simple.Copy as Copy
import Database.DuckDB.Simple.Extension (Extension)
import qualified Database.DuckDB.Simple.Extension as Extension
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (LogicalTypeScalar))
import Database.DuckDB.Simple.Registration (RegistrationTarget, TypeDeclaration (..), checked, destroy, getCallbackState, output, setCallbackState)
import qualified Database.DuckDB.Simple.Registration as Registration
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)

{- | Register an aggregate, a table function, a custom type and its cast, a
replacement scan, a COPY format, and a multi-file reader.
-}
register :: Extension -> IO ()
register extension =
    Registration.withRegistrationFactory target \factory -> do
        registerSum target factory
        registerSquares target factory
        registerShifted target factory
        Registration.registerReplacementScanOn target replaceMissing
        registerLines target
        registerLineFiles target factory
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
and @square@. It uses projection pushdown, so it writes only the requested
columns. A negative count is an error.
-}
registerSquares :: RegistrationTarget -> DuckDBV2FactoryHandle -> IO ()
registerSquares target factory =
    Registration.registerTableFunctionWith
        target
        configure
        (Registration.tableCallbacks bind execute)
            { Registration.tableInitGlobalCallback = Just initialize
            , Registration.tableProjectionPushdown = True
            }
  where
    configure function = do
        Registration.withIdentifier "haskell_squares" (checked . c_duckdb_v2_table_function_set_name function)
        output (c_duckdb_v2_table_function_get_signature function)
            >>= Registration.configureFunctionSignature factory (Registration.FunctionSignature [Registration.parameter "count" bigint] Nothing)
    bind input result context = do
        count <- bracket (output (c_duckdb_v2_function_bind_get_arg_value input 0)) (destroy c_duckdb_v2_value_destroy) (output . c_duckdb_v2_value_get_bigint)
        when (count < 0) $ throwIO (userError ("haskell_squares: the count is negative: " <> show count))
        setCallbackState (c_duckdb_v2_function_bind_set_bind_data input) (count :: Int64)
        addColumn result context "n"
        addColumn result context "square"
    initialize info _ = do
        count <- getCallbackState (c_duckdb_v2_table_function_init_global_get_bind_data info)
        next <- newIORef 0
        setCallbackState (c_duckdb_v2_table_function_init_global_set_global_state info) (count :: Int64, next :: IORef Int64)
    execute info _ = do
        (count, next) <- getCallbackState (c_duckdb_v2_table_function_exec_get_global_state info) :: IO (Int64, IORef Int64)
        chunk <- output (c_duckdb_v2_table_function_exec_get_output_chunk info)
        capacity <- fromIntegral <$> output (c_duckdb_v2_data_chunk_get_capacity chunk)
        start <- atomicModifyIORef' next (\first -> (min count (first + capacity), first))
        let numbers = [start .. min count (start + capacity) - 1]
        columns <- output (c_duckdb_v2_table_function_exec_get_column_count info)
        forM_ (takeWhile (< columns) [0 ..]) \index -> do
            column <- output (c_duckdb_v2_table_function_exec_get_column_index info index)
            vector <- output (c_duckdb_v2_data_chunk_get_vector chunk index)
            writeColumn vector [Just (if column == 0 then n else n * n) | n <- numbers]

{- | Register the custom type @haskell_shifted@ with the base type BIGINT. Then
register a cast from BIGINT to @haskell_shifted@ that adds 100. The cast writes
NULL for a value that overflows, and then reports the overflow. CAST fails with
the error. TRY_CAST ignores the error and keeps the NULL.
-}
registerShifted :: RegistrationTarget -> DuckDBV2FactoryHandle -> IO ()
registerShifted target factory = do
    Registration.registerCustomTypeWith target \custom -> do
        Registration.withIdentifier "haskell_shifted" (checked . c_duckdb_v2_custom_type_set_name custom)
        Registration.withTypeDeclaration factory bigint (checked . c_duckdb_v2_custom_type_set_base_type custom)
    Registration.registerCastWith target configure shift
  where
    configure function = do
        Registration.withTypeDeclaration factory bigint (checked . c_duckdb_v2_cast_function_set_source_type function)
        Registration.withTypeDeclaration factory (TypeExpression "haskell_shifted") (checked . c_duckdb_v2_cast_function_set_target_type function)
    shift info _ = do
        count <- output (c_duckdb_v2_cast_function_exec_get_row_count info)
        values <- output (c_duckdb_v2_cast_function_exec_get_input info) >>= readColumn count
        result <- output (c_duckdb_v2_cast_function_exec_get_output info)
        writeColumn result (map (>>= \value -> addBigInt value 100) values)
        forM_ (catMaybes values) \value -> addOrThrow "haskell_shifted" value 100

{- | Replace the unknown table @haskell_missing@ with a query. Reject the
reserved name @haskell_reserved@. Ignore all other names.
-}
replaceMissing :: Registration.ContextCallback DuckDBV2ReplacementScanInfoHandle
replaceMissing info _ =
    bracket (output (c_duckdb_v2_replacement_scan_get_name info)) (destroy c_duckdb_v2_qname_destroy) \name -> do
        parts <- output (c_duckdb_v2_qname_get_part_count name)
        when (parts == 1) do
            part <- output (c_duckdb_v2_qname_get_part name 0) >>= Registration.readIdentifier
            when (part == "haskell_missing") $
                Registration.withText "SELECT 42::BIGINT AS amount" (checked . c_duckdb_v2_replacement_scan_set_subquery info)
            when (part == "haskell_reserved") $
                throwIO (userError "haskell_reserved: the table name is reserved")

{- | Register the COPY format @haskell_lines@ in both directions. A file has one
BIGINT value on each line. An empty line is NULL.
-}
registerLines :: RegistrationTarget -> IO ()
registerLines target = Copy.registerCopyFunctionWith target configure (Just copyTo) (Just copyFrom)
  where
    configure function = Registration.withIdentifier "haskell_lines" (checked . c_duckdb_v2_copy_function_set_name function)
    copyTo = (Copy.copyToCallbacks batch flush){Copy.copyToBindCallback = Just bindTo, Copy.copyToInitCallback = Just initialize}
    bindTo info _ = do
        count <- output (c_duckdb_v2_copy_to_bind_get_column_count info)
        types <- forM (takeWhile (< count) [0 ..]) \index ->
            bracket (output (c_duckdb_v2_copy_to_bind_get_column_type info index)) (destroy c_duckdb_v2_logical_type_destroy) (output . c_duckdb_v2_logical_type_get_id)
        unless (types == [DuckDBV2LogicalTypeIdBigInt]) $ throwIO (userError "haskell_lines writes exactly one BIGINT column")
    initialize info _ = do
        path <- Text.unpack <$> (output (c_duckdb_v2_copy_to_init_get_file_path info) >>= Registration.readText)
        writeFile path ""
        setCallbackState (c_duckdb_v2_copy_to_init_set_init_data info) path
    batch info context = do
        rows <- bracket (output (c_duckdb_v2_copy_to_batch_take_input info)) (destroy c_duckdb_v2_column_data_collection_destroy) (readCollection context)
        setCallbackState (c_duckdb_v2_copy_to_batch_set_batch_data info) rows
    flush info _ = do
        path <- getCallbackState (c_duckdb_v2_copy_to_flush_get_init_data info)
        rows <- getCallbackState (c_duckdb_v2_copy_to_flush_get_batch_data info)
        appendFile path (unlines (map (maybe "" show) (rows :: [Maybe Int64])))
    copyFrom = (Copy.copyFromCallbacks bindFrom executeFrom){Copy.copyFromInitGlobalCallback = Just initializeFrom}
    bindFrom info _ = do
        path <- output (c_duckdb_v2_copy_from_bind_get_file_path info) >>= Registration.readText
        rows <- readLines (Text.unpack path)
        setCallbackState (c_duckdb_v2_copy_from_bind_set_bind_data info) rows
    initializeFrom info _ = do
        rows <- getCallbackState (c_duckdb_v2_copy_from_init_global_get_bind_data info)
        remaining <- newIORef (rows :: [Maybe Int64])
        setCallbackState (c_duckdb_v2_copy_from_init_global_set_global_state info) remaining
    executeFrom info _ = do
        remaining <- getCallbackState (c_duckdb_v2_copy_from_exec_get_global_state info)
        output (c_duckdb_v2_copy_from_exec_get_output_chunk info) >>= writeNext remaining

{- | Register the table function @haskell_read_line_file(path)@. It reads one
file in the format of @haskell_lines@. Then register the multi-file function
@haskell_read_lines@, which reads a list of files or a glob pattern.
-}
registerLineFiles :: RegistrationTarget -> DuckDBV2FactoryHandle -> IO ()
registerLineFiles target factory = do
    Registration.registerTableFunctionWith target configure (Registration.tableCallbacks bind execute){Registration.tableInitGlobalCallback = Just initialize}
    Copy.registerMultiFileFunctionWith target \function -> do
        Registration.withText "haskell_read_lines" \(ConstPtr name) -> checked (c_duckdb_v2_multi_file_function_set_name function name)
        Registration.withText "haskell_read_line_file" \(ConstPtr name) -> checked (c_duckdb_v2_multi_file_function_set_single_file_function function name)
  where
    configure function = do
        Registration.withIdentifier "haskell_read_line_file" (checked . c_duckdb_v2_table_function_set_name function)
        output (c_duckdb_v2_table_function_get_signature function)
            >>= Registration.configureFunctionSignature factory (Registration.FunctionSignature [Registration.parameter "path" (TypeExpression "VARCHAR")] Nothing)
    bind input result context = do
        path <- bracket (output (c_duckdb_v2_function_bind_get_arg_value input 0)) (destroy c_duckdb_v2_value_destroy) \value ->
            output (c_duckdb_v2_value_get_varchar value) >>= Registration.readText
        rows <- readLines (Text.unpack path)
        setCallbackState (c_duckdb_v2_function_bind_set_bind_data input) rows
        addColumn result context "value"
    initialize info _ = do
        rows <- getCallbackState (c_duckdb_v2_table_function_init_global_get_bind_data info)
        remaining <- newIORef (rows :: [Maybe Int64])
        setCallbackState (c_duckdb_v2_table_function_init_global_set_global_state info) remaining
    execute info _ = do
        remaining <- getCallbackState (c_duckdb_v2_table_function_exec_get_global_state info)
        output (c_duckdb_v2_table_function_exec_get_output_chunk info) >>= writeNext remaining

-- | Declare a BIGINT result column in the bind callback of a table function.
addColumn :: DuckDBV2TableFunctionBindInfoHandle -> DuckDBV2ContextHandle -> Text -> IO ()
addColumn result context name = do
    factory <- output (c_duckdb_v2_context_get_factory context)
    Registration.withTypeDeclaration factory bigint \dtype ->
        Registration.withIdentifier name \identifier -> checked (c_duckdb_v2_table_function_bind_add_result_column result identifier dtype)

-- | Read a file with one BIGINT value on each line. An empty line is NULL.
readLines :: FilePath -> IO [Maybe Int64]
readLines path = TextIO.readFile path >>= mapM parse . Text.lines
  where
    parse line
        | Text.null line = pure Nothing
        | otherwise = case TextRead.signed TextRead.decimal line of
            Right (value, rest) | Text.null rest -> pure (Just value)
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

-- | Write the rows of a BIGINT vector and set its size. 'Nothing' becomes NULL.
writeColumn :: DuckDBV2VectorHandle -> [Maybe Int64] -> IO ()
writeColumn vector values = do
    checked (c_duckdb_v2_vector_set_size vector (fromIntegral (length values)))
    checked (c_duckdb_v2_vector_flatten vector)
    raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
    forM_ (zip [0 ..] values) \(row, value) -> case value of
        Just number -> pokeElemOff (castPtr raw) row number
        Nothing -> checked (c_duckdb_v2_vector_set_null vector (fromIntegral row))

-- | Copy the rows of a collection with one BIGINT column.
readCollection :: DuckDBV2ContextHandle -> DuckDBV2ColumnDataCollectionHandle -> IO [Maybe Int64]
readCollection context collection = do
    factory <- output (c_duckdb_v2_context_get_factory context)
    Registration.withTypeDeclaration factory bigint \dtype ->
        withArray [dtype] \types ->
            bracket (output (c_duckdb_v2_data_chunk_create factory (ConstPtr types) 1)) (destroy c_duckdb_v2_data_chunk_destroy) \chunk ->
                bracket (output (c_duckdb_v2_column_data_collection_shared_scan_state_create collection)) (destroy c_duckdb_v2_column_data_collection_shared_scan_state_destroy) \shared ->
                    bracket (output (c_duckdb_v2_column_data_collection_worker_scan_state_create collection)) (destroy c_duckdb_v2_column_data_collection_worker_scan_state_destroy) \worker ->
                        let scan chunks = do
                                produced <- output (c_duckdb_v2_column_data_collection_scan collection shared worker chunk)
                                if produced == 0
                                    then pure (concat (reverse chunks))
                                    else do
                                        size <- output (c_duckdb_v2_data_chunk_get_size chunk)
                                        rows <- output (c_duckdb_v2_data_chunk_get_vector chunk 0) >>= readColumn size
                                        scan (rows : chunks)
                         in scan []

-- | Move the next rows that fit into a one-column output chunk. No rows end the scan.
writeNext :: IORef [Maybe Int64] -> DuckDBV2DataChunkHandle -> IO ()
writeNext remaining chunk = do
    capacity <- fromIntegral <$> output (c_duckdb_v2_data_chunk_get_capacity chunk)
    rows <- atomicModifyIORef' remaining (\rows -> let (now, later) = splitAt capacity rows in (later, now))
    vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
    writeColumn vector rows
