{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module V2ArrowTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM, forM_, unless)
import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef)
import Data.Int (Int64)
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import GHC.Records (getField)
import HsBindgen.Runtime.Prelude (fromFunPtr, toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import V2Test (callbackGuard, checked, consume, countCall, output, readIntegers, readView, withConnection, withFactory, withHandle, withResult, withStatement, withType, withView)

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 Arrow"
        [ testCase "rows exported to Arrow in batches and imported again keep their values" testRoundTrip
        , testCase "results step to the end with waits, and Arrow results report their shape and changed rows" testResults
        ]

-- | The rows of the round trip: a BIGINT and a VARCHAR column, each with a NULL.
sourceRows :: [(Maybe Int64, Maybe String)]
sourceRows = [(Just 1, Just "a"), (Nothing, Just "long enough to be stored outside the inline buffer"), (Just 3, Nothing), (Just 4, Just ""), (Just 5, Just "e")]

-- | Read one row of a vector, or Nothing for NULL.
cell :: (DuckDBV2ValueHandle -> IO a) -> DuckDBV2VectorHandle -> Int -> IO (Maybe a)
cell get vector row = withHandle (c_duckdb_v2_vector_get_value vector (fromIntegral row)) c_duckdb_v2_value_destroy $ \value -> do
    isNull <- output (c_duckdb_v2_value_is_null value)
    if isNull == CBool 0 then Just <$> get value else pure Nothing

bigint :: DuckDBV2ValueHandle -> IO Int64
bigint value = output (c_duckdb_v2_value_get_bigint value)

varchar :: DuckDBV2ValueHandle -> IO String
varchar value = output (c_duckdb_v2_value_get_varchar value) >>= readView

-- | Read the BIGINT column and the VARCHAR column of each row of a chunk.
readRows :: DuckDBV2DataChunkHandle -> IO [(Maybe Int64, Maybe String)]
readRows chunk = do
    size <- output (c_duckdb_v2_data_chunk_get_size chunk)
    amounts <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
    labels <- output (c_duckdb_v2_data_chunk_get_vector chunk 1)
    forM [0 .. fromIntegral size - 1] $ \row -> (,) <$> cell bigint amounts row <*> cell varchar labels row

-- | Run an action with a zeroed Arrow array. At the end, release the array if it still has a release callback.
withArrowArray :: (Ptr ArrowArray -> IO a) -> IO a
withArrowArray action = alloca $ \ptr -> do
    fillBytes ptr 0 (sizeOf (undefined :: ArrowArray))
    action ptr `finally` (peek ptr >>= \array -> unless (getField @"release" array == nullFunPtr) (fromFunPtr (getField @"release" array) ptr))

-- | Run an action with a zeroed Arrow schema. At the end, release the schema if it still has a release callback.
withArrowSchema :: (Ptr ArrowSchema -> IO a) -> IO a
withArrowSchema action = alloca $ \ptr -> do
    fillBytes ptr 0 (sizeOf (undefined :: ArrowSchema))
    action ptr `finally` (peek ptr >>= \schema -> unless (getField @"release" schema == nullFunPtr) (fromFunPtr (getField @"release" schema) ptr))

-- | Tell if an Arrow array has no release callback, that is, if it holds no data of the caller.
released :: Ptr ArrowArray -> IO Bool
released ptr = (== nullFunPtr) . getField @"release" <$> peek ptr

-- | Read the name and the format of each column of an Arrow schema. The schema itself is a struct.
arrowColumns :: Ptr ArrowSchema -> IO [(String, String)]
arrowColumns ptr = do
    schema <- peek ptr
    peekCString (coerce (getField @"format" schema)) >>= (@?= "+s")
    forM [0 .. fromIntegral (getField @"n_children" schema) - 1] $ \i -> do
        column <- peekElemOff (getField @"children" schema) i >>= peek
        (,) <$> peekCString (coerce (getField @"name" column)) <*> peekCString (coerce (getField @"format" column))

-- | Read the name and the type id of each field of a schema.
schemaFields :: DuckDBV2SchemaHandle -> IO [(String, DuckDBV2LogicalTypeId)]
schemaFields schema = do
    count <- output (c_duckdb_v2_schema_get_count schema)
    forM [0 .. fromIntegral count - 1 :: Int] $ \i -> alloca $ \name -> do
        ty <- output (c_duckdb_v2_schema_get_field schema (fromIntegral i) name)
        (,) <$> (peek name >>= readView . coerce) <*> output (c_duckdb_v2_logical_type_get_id ty)

-- | Write the source rows into a new chunk and give the chunk to the exporter, which destroys it.
exportRows :: DuckDBV2FactoryHandle -> Ptr DuckDBV2LogicalTypeHandle -> DuckDBV2ArrowExporterHandle -> IO ()
exportRows factory types exporter = alloca $ \slot -> do
    poke slot (coerce nullPtr)
    flip finally (c_duckdb_v2_data_chunk_destroy slot) $ do
        checked (c_duckdb_v2_data_chunk_create factory (ConstPtr types) 2 slot)
        chunk <- peek slot
        amounts <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
        labels <- output (c_duckdb_v2_data_chunk_get_vector chunk 1)
        checked (c_duckdb_v2_vector_set_size amounts (fromIntegral (length sourceRows)))
        checked (c_duckdb_v2_vector_set_size labels (fromIntegral (length sourceRows)))
        forM_ (zip [0 ..] sourceRows) $ \(row, (amount, label)) -> do
            case amount of
                Just number -> withHandle (c_duckdb_v2_value_create_bigint factory number) c_duckdb_v2_value_destroy $ \value ->
                    checked (c_duckdb_v2_vector_set_value amounts row value)
                Nothing -> checked (c_duckdb_v2_vector_set_null amounts row)
            case label of
                Just text -> withView text $ \view -> withHandle (c_duckdb_v2_value_create_varchar factory view) c_duckdb_v2_value_destroy $ \value ->
                    checked (c_duckdb_v2_vector_set_value labels row value)
                Nothing -> checked (c_duckdb_v2_vector_set_null labels row)
        checked (c_duckdb_v2_arrow_exporter_append exporter slot (CBool 1) (CBool 0))
        peek slot >>= (@?= coerce nullPtr)

-- | Take the next array of the exporter into the slot and return its row count.
nextArray :: DuckDBV2ArrowExporterHandle -> Ptr ArrowArray -> IO Int64
nextArray exporter ptr = do
    checked (c_duckdb_v2_arrow_exporter_next_array exporter ptr)
    released ptr >>= assertBool "the exporter has an array ready" . not
    getField @"length" <$> peek ptr

-- | Take every chunk that the importer has ready, and read the rows of each chunk.
drain :: DuckDBV2ArrowImporterHandle -> IO [[(Maybe Int64, Maybe String)]]
drain importer = alloca $ \slot -> do
    poke slot (coerce nullPtr)
    checked (c_duckdb_v2_arrow_importer_next_chunk importer slot)
    chunk <- peek slot
    if chunk == coerce nullPtr
        then pure []
        else do
            rows <- readRows chunk `finally` c_duckdb_v2_data_chunk_destroy slot
            (rows :) <$> drain importer

{- | Move the arrays of the exporter into the importer, and return the rows of each imported chunk.
The exporter makes arrays of 2 rows and the importer makes chunks of 3 rows.
-}
transfer :: DuckDBV2ArrowExporterHandle -> DuckDBV2ArrowImporterHandle -> IO [[(Maybe Int64, Maybe String)]]
transfer exporter importer =
    withArrowArray $ \first -> withArrowArray $ \second -> withArrowArray $ \third -> withArrowArray $ \none -> do
        -- The 5 rows fill two arrays. The last row waits for more rows or a flush.
        nextArray exporter first >>= (@?= 2)
        nextArray exporter second >>= (@?= 2)
        checked (c_duckdb_v2_arrow_exporter_next_array exporter none)
        released none >>= assertBool "no third array before the flush"
        checked (c_duckdb_v2_arrow_exporter_append exporter nullPtr (CBool 0) (CBool 1))
        nextArray exporter third >>= (@?= 1)
        checked (c_duckdb_v2_arrow_exporter_next_array exporter none)
        released none >>= assertBool "no array after the flushed array"
        drain importer >>= (@?= [])
        -- With consume, the importer takes the array and clears its release callback.
        checked (c_duckdb_v2_arrow_importer_append importer first (CBool 1) (CBool 0))
        released first >>= assertBool "the consumed array has no release callback"
        -- The importer did not read the first array yet, so it refuses another array.
        c_duckdb_v2_arrow_importer_append importer second (CBool 0) (CBool 0) nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
        -- Two rows do not fill a chunk of 3, so the importer holds them back.
        held <- drain importer
        -- Without consume, the caller keeps the array and releases it.
        checked (c_duckdb_v2_arrow_importer_append importer second (CBool 0) (CBool 0))
        released second >>= assertBool "the kept array has its release callback" . not
        joined <- drain importer
        checked (c_duckdb_v2_arrow_importer_append importer third (CBool 0) (CBool 1))
        flushed <- drain importer
        pure (held <> joined <> flushed)

testRoundTrip :: IO ()
testRoundTrip = do
    failures <- newIORef []
    binds <- newIORef 0
    -- The exporter and the importer need a context with an active transaction, so the bind callback runs the round trip.
    let bind _info _result context err = callbackGuard failures err $ do
            countCall binds
            factory <- output (c_duckdb_v2_context_get_factory context)
            withType factory "BIGINT" $ \integer -> withType factory "VARCHAR" $ \text -> withArray [integer, text] $ \types ->
                withView "amount" $ \amount -> withView "label" $ \label -> do
                    names <- mapM (peek . coerce) [amount, label]
                    withArray names $ \columns ->
                        withHandle (c_duckdb_v2_arrow_exporter_create context (ConstPtr types) (ConstPtr columns) 2 2) c_duckdb_v2_arrow_exporter_destroy $ \exporter ->
                            withArrowSchema $ \schema -> do
                                checked (c_duckdb_v2_arrow_exporter_get_schema exporter schema)
                                columnFormats <- arrowColumns schema
                                map fst columnFormats @?= ["amount", "label"]
                                lookup "amount" columnFormats @?= Just "l"
                                -- The importer reads the schema and does not consume it.
                                withHandle (c_duckdb_v2_arrow_importer_create context schema 3) c_duckdb_v2_arrow_importer_destroy $ \importer -> do
                                    withHandle (c_duckdb_v2_arrow_importer_get_schema importer) c_duckdb_v2_schema_destroy $ \resolved ->
                                        schemaFields resolved >>= (@?= [("amount", DuckDBV2LogicalTypeIdBigInt), ("label", DuckDBV2LogicalTypeIdVarchar)])
                                    exportRows factory types exporter
                                    transfer exporter importer >>= (@?= [take 3 sourceRows, drop 3 sourceRows])
        execute info _context err = callbackGuard failures err $ do
            rows <- output (c_duckdb_v2_scalar_function_exec_get_row_count info)
            vector <- output (c_duckdb_v2_scalar_function_exec_get_result info)
            raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
            forM_ [0 .. fromIntegral rows - 1] $ \i -> pokeElemOff (coerce raw) i (1 :: Int64)
    bracket (DuckDBV2ScalarFunctionBindCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionBindCallbackFn_Aux bind)) (freeHaskellFunPtr . getField @"unwrap") $ \bindPtr ->
        bracket (DuckDBV2ScalarFunctionExecCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux execute)) (freeHaskellFunPtr . getField @"unwrap") $ \executePtr ->
            withConnection $ \conn -> withFactory conn $ \factory -> do
                withType factory "BIGINT" $ \ty ->
                    withHandle (c_duckdb_v2_scalar_function_create factory) c_duckdb_v2_scalar_function_destroy $ \function -> do
                        withView "v2_arrow_round_trip" $ \name -> checked (c_duckdb_v2_scalar_function_set_name function (coerce name))
                        signature <- output (c_duckdb_v2_scalar_function_get_signature function)
                        checked (c_duckdb_v2_function_signature_set_return_type signature ty)
                        checked (c_duckdb_v2_scalar_function_set_bind_callback function bindPtr)
                        checked (c_duckdb_v2_scalar_function_set_exec_callback function executePtr)
                        checked (c_duckdb_v2_connection_register_scalar_function conn function)
                withResult conn "SELECT v2_arrow_round_trip()" $ \result -> readIntegers result >>= (@?= [Just 1])
    readIORef failures >>= (@?= [])
    readIORef binds >>= assertBool "the bind callback was invoked" . (> 0)

{- | Step a result to the end and read its BIGINT rows. Wait after each WAITING status, as the header advises.
A wait after a chunk could block, so the loop does not wait there.
-}
stepRows :: DuckDBV2ResultHandle -> IO [Maybe Int64]
stepRows result = do
    (status, rows) <- alloca $ \slot -> alloca $ \statusSlot -> do
        poke slot (coerce nullPtr)
        checked (c_duckdb_v2_result_step result slot statusSlot)
        stepped <- peek statusSlot
        chunk <- peek slot
        if stepped == DuckDBV2ResultStepStatusChunk
            then do
                rows <- flip finally (c_duckdb_v2_data_chunk_destroy slot) $ do
                    size <- output (c_duckdb_v2_data_chunk_get_size chunk)
                    vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                    forM [0 .. fromIntegral size - 1] (cell bigint vector)
                pure (stepped, rows)
            else do
                chunk @?= coerce nullPtr
                pure (stepped, [])
    if status == DuckDBV2ResultStepStatusChunk
        then (rows <>) <$> stepRows result
        else
            if status == DuckDBV2ResultStepStatusWaiting
                then checked (c_duckdb_v2_result_wait result) >> stepRows result
                else [] <$ (status @?= DuckDBV2ResultStepStatusFinished)

-- | Read the values of the first column of an Arrow array. The column is a BIGINT column without NULL.
arrayValues :: Ptr ArrowArray -> IO [Int64]
arrayValues ptr = do
    array <- peek ptr
    column <- peekElemOff (getField @"children" array) 0 >>= peek
    buffer <- peekElemOff (getField @"buffers" column) 1
    forM [0 .. fromIntegral (getField @"length" column) - 1] $ \i ->
        peekElemOff (coerce buffer :: Ptr Int64) (fromIntegral (getField @"offset" column) + i)

-- | Step an Arrow result to the end and read the values of each array. Wait after each WAITING status.
stepArrays :: DuckDBV2ArrowResultHandle -> IO [[Int64]]
stepArrays result = do
    (status, values) <- withArrowArray $ \array -> alloca $ \statusSlot -> do
        checked (c_duckdb_v2_arrow_result_step result array statusSlot)
        stepped <- peek statusSlot
        if stepped == DuckDBV2ResultStepStatusChunk
            then (,) stepped <$> arrayValues array
            else do
                released array >>= assertBool "only the status CHUNK gives an array"
                pure (stepped, [])
    if status == DuckDBV2ResultStepStatusChunk
        then (values :) <$> stepArrays result
        else
            if status == DuckDBV2ResultStepStatusWaiting
                then checked (c_duckdb_v2_arrow_result_wait result) >> stepArrays result
                else [] <$ (status @?= DuckDBV2ResultStepStatusFinished)

-- | Execute a statement as an Arrow result, with a maximum number of rows for each array.
withArrowResult :: DuckDBV2ConnectionHandle -> String -> DuckDBIdx -> (DuckDBV2ArrowResultHandle -> IO a) -> IO a
withArrowResult conn sql batchSize action = withStatement conn sql $ \statement ->
    withHandle (c_duckdb_v2_statement_execute_arrow conn statement (coerce nullPtr) (coerce nullPtr) 0 batchSize) c_duckdb_v2_arrow_result_destroy action

testResults :: IO ()
testResults = withConnection $ \conn -> do
    withResult conn "SELECT i::BIGINT FROM range(5) t(i)" $ \result -> do
        stepRows result >>= (@?= map Just [0 .. 4])
        -- A wait on a finished result returns at once, and the status FINISHED stays.
        checked (c_duckdb_v2_result_wait result)
        stepRows result >>= (@?= [])
    withArrowResult conn "SELECT i::BIGINT AS amount FROM range(5) t(i)" 2 $ \result -> do
        output (c_duckdb_v2_arrow_result_get_result_type result) >>= (@?= DuckDBV2ResultTypeQueryResult)
        output (c_duckdb_v2_arrow_result_get_statement_type result) >>= (@?= DuckDBV2StatementTypeSelect)
        arrays <- stepArrays result
        concat arrays @?= [0 .. 4]
        assertBool ("no array has more rows than the batch size: " <> show arrays) (all ((<= 2) . length) arrays)
        checked (c_duckdb_v2_arrow_result_wait result)
        stepArrays result >>= (@?= [])
    withResult conn "CREATE TABLE v2_amounts (amount BIGINT)" consume
    withArrowResult conn "INSERT INTO v2_amounts SELECT i FROM range(3) t(i)" 0 $ \result -> do
        output (c_duckdb_v2_arrow_result_get_result_type result) >>= (@?= DuckDBV2ResultTypeChangedRows)
        output (c_duckdb_v2_arrow_result_get_statement_type result) >>= (@?= DuckDBV2StatementTypeInsert)
        output (c_duckdb_v2_arrow_result_drain result) >>= (@?= 3)
        -- The first drain read the count, so a second drain reports 0.
        output (c_duckdb_v2_arrow_result_drain result) >>= (@?= 0)
    withResult conn "SELECT count(*)::BIGINT FROM v2_amounts" $ \result -> readIntegers result >>= (@?= [Just 3])
