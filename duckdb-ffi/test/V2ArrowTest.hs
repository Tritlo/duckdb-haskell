{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module V2ArrowTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM, forM_, unless, when)
import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Database.DuckDB.FFI.V2
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import GHC.Records (getField)
import HsBindgen.Runtime.HasCField (fromPtr)
import HsBindgen.Runtime.Prelude (fromFunPtr, toFunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))
import V2Test (
  callbackGuard,
  checked,
  consume,
  countCall,
  output,
  readIntegers,
  readView,
  withConnection,
  withFactory,
  withHandle,
  withResult,
  withStatement,
  withType,
  withView,
 )

tests :: TestTree
tests =
  testGroup
    "DuckDB V2 Arrow"
    [ testCase "rows exported to Arrow in batches and imported again keep their values, and a wrong shape is rejected" testRoundTrip
    , testCase "results step to the end with waits, and Arrow results report their shape, changed rows, cancellation and errors" testResults
    ]

-- | The rows of the round trip: a BIGINT and a VARCHAR column, each with a NULL.
sourceRows :: [(Maybe Int64, Maybe String)]
sourceRows =
  [ (Just 1, Just "a")
  , (Nothing, Just "long enough to be stored outside the inline buffer")
  , (Just 3, Nothing)
  , (Just 4, Just "")
  , (Just 5, Just "e")
  ]

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
  action ptr
    `finally` (peek ptr >>= \schema -> unless (getField @"release" schema == nullFunPtr) (fromFunPtr (getField @"release" schema) ptr))

-- | Set a field to a value for the duration of the action, and restore the field after.
patched :: (Storable a) => Ptr a -> a -> IO b -> IO b
patched field value action = do
  original <- peek field
  poke field value
  action `finally` poke field original

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

-- | Move the arrays of the exporter into a new importer, and return the rows of each imported chunk.
-- The exporter makes arrays of 3 rows and the importer makes chunks of 2 rows.
transfer :: DuckDBV2ContextHandle -> Ptr ArrowSchema -> DuckDBV2ArrowExporterHandle -> IO [[(Maybe Int64, Maybe String)]]
transfer context schema exporter =
  withArrowArray $ \first -> withArrowArray $ \second -> withArrowArray $ \none -> alloca $ \kept -> do
    poke kept (coerce nullPtr)
    flip finally (c_duckdb_v2_data_chunk_destroy kept) $ do
      -- The 5 rows fill one array. The last 2 rows wait for more rows or a flush.
      nextArray exporter first >>= (@?= 3)
      checked (c_duckdb_v2_arrow_exporter_next_array exporter none)
      released none >>= assertBool "no second array before the flush"
      checked (c_duckdb_v2_arrow_exporter_append exporter nullPtr (CBool 0) (CBool 1))
      nextArray exporter second >>= (@?= 2)
      checked (c_duckdb_v2_arrow_exporter_next_array exporter none)
      released none >>= assertBool "no array after the flushed array"
      -- The importer reads the schema and does not consume it.
      rest <- withHandle (c_duckdb_v2_arrow_importer_create context schema 2) c_duckdb_v2_arrow_importer_destroy $ \importer -> do
        withHandle (c_duckdb_v2_arrow_importer_get_schema importer) c_duckdb_v2_schema_destroy $ \resolved ->
          schemaFields resolved >>= (@?= [("amount", DuckDBV2LogicalTypeIdBigInt), ("label", DuckDBV2LogicalTypeIdVarchar)])
        drain importer >>= (@?= [])
        -- The importer rejects an array whose shape differs from the schema, before it reads the array. Each
        -- patch changes one field of the first array, and ends before the next append.
        array <- peek first
        child <- peekElemOff (getField @"children" array) 0
        childLength <- peek (fromPtr (Proxy @"length") child)
        forM_
          [ ("a different child count", patched (fromPtr (Proxy @"n_children") first) 1)
          , ("a null child", patched (getField @"children" array) nullPtr)
          , ("a child of another length", patched (fromPtr (Proxy @"length") child) (childLength + 1))
          , ("a released child", patched (fromPtr (Proxy @"release") child) nullFunPtr)
          ]
          $ \(what, patch) -> do
            patch (c_duckdb_v2_arrow_importer_append importer first (CBool 1) (CBool 0) nullPtr)
              >>= assertEqual (what <> " is rejected") DuckDBV2ErrorInputInvalid
            released first >>= assertBool (what <> " leaves the array with the caller") . not
        -- With consume, the importer takes the array and clears its release callback.
        checked (c_duckdb_v2_arrow_importer_append importer first (CBool 1) (CBool 0))
        released first >>= assertBool "the consumed array has no release callback"
        -- The importer did not read the first array yet, so it refuses another array.
        c_duckdb_v2_arrow_importer_append importer second (CBool 0) (CBool 0) nullPtr >>= (@?= DuckDBV2ErrorInputInvalid)
        -- The first chunk references the buffers of the consumed array. The test reads it after the importer is destroyed.
        checked (c_duckdb_v2_arrow_importer_next_chunk importer kept)
        peek kept >>= assertBool "the consumed array gives a chunk" . (/= coerce nullPtr)
        -- The third row does not fill a chunk of 2, so the importer holds it back.
        drain importer >>= (@?= [])
        -- Without consume, the caller keeps the array and releases it.
        checked (c_duckdb_v2_arrow_importer_append importer second (CBool 0) (CBool 1))
        released second >>= assertBool "the kept array has its release callback" . not
        drain importer
      firstRows <- peek kept >>= readRows
      pure (firstRows : rest)

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
              withHandle (c_duckdb_v2_arrow_exporter_create context (ConstPtr types) (ConstPtr columns) 2 3) c_duckdb_v2_arrow_exporter_destroy $ \exporter ->
                withArrowSchema $ \schema -> do
                  checked (c_duckdb_v2_arrow_exporter_get_schema exporter schema)
                  columnFormats <- arrowColumns schema
                  map fst columnFormats @?= ["amount", "label"]
                  lookup "amount" columnFormats @?= Just "l"
                  -- An empty chunk with the column types of the exporter adds no rows. The exporter rejects an
                  -- empty chunk with other column types, and leaves it to the caller.
                  withArray [text, integer] $ \swapped -> forM_ [(types, DuckDBV2ErrorNone), (swapped, DuckDBV2ErrorInputInvalid)] $ \(chunkTypes, expected) ->
                    alloca $ \slot -> do
                      poke slot (coerce nullPtr)
                      flip finally (c_duckdb_v2_data_chunk_destroy slot) $ do
                        checked (c_duckdb_v2_data_chunk_create factory (ConstPtr chunkTypes) 2 slot)
                        c_duckdb_v2_arrow_exporter_append exporter slot (CBool 1) (CBool 0) nullPtr >>= (@?= expected)
                  exportRows factory types exporter
                  transfer context schema exporter >>= (@?= [take 2 sourceRows, take 2 (drop 2 sourceRows), drop 4 sourceRows])
      execute info _context err = callbackGuard failures err $ do
        rows <- output (c_duckdb_v2_scalar_function_exec_get_row_count info)
        vector <- output (c_duckdb_v2_scalar_function_exec_get_result info)
        raw <- output (c_duckdb_v2_vector_get_data_mutable vector)
        forM_ [0 .. fromIntegral rows - 1] $ \i -> pokeElemOff (coerce raw) i (1 :: Int64)
  bracket
    (DuckDBV2ScalarFunctionBindCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionBindCallbackFn_Aux bind))
    (freeHaskellFunPtr . getField @"unwrap")
    $ \bindPtr ->
      bracket
        (DuckDBV2ScalarFunctionExecCallbackFn <$> toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux execute))
        (freeHaskellFunPtr . getField @"unwrap")
        $ \executePtr ->
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

-- | Step a result to the end and read its BIGINT rows. Wait after each WAITING status, as the header advises.
-- A wait after a chunk could block, so the loop does not wait there.
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
  withHandle
    (c_duckdb_v2_statement_execute_arrow conn statement (coerce nullPtr) (coerce nullPtr) 0 batchSize)
    c_duckdb_v2_arrow_result_destroy
    action

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
  -- A dynamic PIVOT expands into several statements. The result type is known only after a step binds the
  -- statement that returns rows. The statement does not accept parameter values.
  withArrowResult conn "PIVOT v2_amounts ON amount" 0 $ \result -> do
    alloca $ \kind -> c_duckdb_v2_arrow_result_get_result_type result kind nullPtr >>= (@?= DuckDBV2ErrorInputMetadataDeferred)
    concat <$> stepArrays result >>= (@?= [1])
    output (c_duckdb_v2_arrow_result_get_result_type result) >>= (@?= DuckDBV2ResultTypeQueryResult)
  withFactory conn $ \factory -> withHandle (c_duckdb_v2_value_create_bigint factory 7) c_duckdb_v2_value_destroy $ \value -> withArray [value, value] $ \values -> do
    -- Execute the statement with the first count values and their names. The execution must fail with INPUT_INVALID.
    let refused sql names count = withStatement conn sql $ \statement -> alloca $ \slot -> do
          poke slot (coerce nullPtr)
          c_duckdb_v2_statement_execute_arrow conn statement names (ConstPtr values) count 0 slot nullPtr
            >>= assertEqual sql DuckDBV2ErrorInputInvalid
          peek slot >>= (@?= coerce nullPtr)
        -- Execute the statement with the first value and its name, and read the value back.
        accepted sql names = withStatement conn sql $ \statement ->
          withHandle (c_duckdb_v2_statement_execute_arrow conn statement names (ConstPtr values) 1 0) c_duckdb_v2_arrow_result_destroy $ \result ->
            stepArrays result >>= (@?= [[7]])
        positional = DuckDBV2IdentifierT (DuckDBV2Str (ConstPtr nullPtr) 0)
    refused "PIVOT v2_amounts ON amount" (coerce nullPtr) 1
    -- A name for a positional parameter is rejected, and so is a positional value for a named parameter. A {NULL, 0}
    -- name keeps the value positional. Named and positional keys cannot be mixed.
    withView "amount" $ \amount -> do
      refused "SELECT $1::BIGINT" (coerce amount) 1
      accepted "SELECT $amount::BIGINT" (coerce amount)
      named <- peek (coerce amount)
      withArray [DuckDBV2IdentifierT named, positional] $ \names -> refused "SELECT $amount::BIGINT" (ConstPtr names) 2
    refused "SELECT $amount::BIGINT" (coerce nullPtr) 1
    with positional $ \name -> accepted "SELECT $1::BIGINT" (ConstPtr name)
  -- A statement that fails to bind gives no result.
  withStatement conn "SELECT absent_column" $ \statement -> alloca $ \slot -> do
    poke slot (coerce nullPtr)
    code <- c_duckdb_v2_statement_execute_arrow conn statement (coerce nullPtr) (coerce nullPtr) 0 0 slot nullPtr
    assertBool ("a statement that fails to bind fails: " <> show code) (code /= DuckDBV2ErrorNone)
    peek slot >>= (@?= coerce nullPtr)
  -- One statement runs on a connection at a time. An interrupt cancels the running result: a step reports
  -- CANCELLED, a wait returns at once, and a drain reports the interrupt.
  withArrowResult conn "SELECT i::BIGINT FROM range(100000) t(i)" 0 $ \running -> do
    withStatement conn "SELECT 9::BIGINT" $ \statement -> alloca $ \slot -> do
      poke slot (coerce nullPtr)
      c_duckdb_v2_statement_execute_arrow conn statement (coerce nullPtr) (coerce nullPtr) 0 0 slot nullPtr >>= (@?= DuckDBV2ErrorResourceInUse)
      peek slot >>= (@?= coerce nullPtr)
    checked (c_duckdb_v2_connection_interrupt conn)
    withArrowArray $ \array -> alloca $ \status -> do
      checked (c_duckdb_v2_arrow_result_step running array status)
      peek status >>= (@?= DuckDBV2ResultStepStatusCancelled)
      released array >>= assertBool "a cancelled step gives no array"
    checked (c_duckdb_v2_arrow_result_wait running)
    alloca $ \rows -> c_duckdb_v2_arrow_result_drain running rows nullPtr >>= (@?= DuckDBV2ErrorRuntimeInterrupt)
  withArrowResult conn "SELECT 9::BIGINT" 0 $ \result -> stepArrays result >>= (@?= [[9]])
  -- An error during the execution comes back from a step. Later steps and a drain report the same code, and a wait
  -- returns at once.
  withArrowResult conn "SELECT CASE WHEN i = 3 THEN error('v2 execution failure') ELSE i END::BIGINT FROM range(5) t(i)" 0 $ \result -> do
    let stepToError = do
          (code, status) <- withArrowArray $ \array -> alloca $ \statusSlot -> do
            code <- c_duckdb_v2_arrow_result_step result array statusSlot nullPtr
            when (code /= DuckDBV2ErrorNone) $ released array >>= assertBool "a failed step gives no array"
            (,) code <$> peek statusSlot
          if code /= DuckDBV2ErrorNone
            then pure code
            else
              if status == DuckDBV2ResultStepStatusFinished
                then assertFailure "the result finishes without an error"
                else when (status == DuckDBV2ResultStepStatusWaiting) (checked (c_duckdb_v2_arrow_result_wait result)) >> stepToError
    failed <- stepToError
    stepToError >>= (@?= failed)
    checked (c_duckdb_v2_arrow_result_wait result)
    alloca $ \rows -> c_duckdb_v2_arrow_result_drain result rows nullPtr >>= (@?= failed)
  withArrowResult conn "SELECT 9::BIGINT" 0 $ \result -> stepArrays result >>= (@?= [[9]])
