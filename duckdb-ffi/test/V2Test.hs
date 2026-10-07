{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module V2Test (tests) where

import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (forM, forM_, unless, void)
import Data.Bits (testBit)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Database.DuckDB.FFI.V2
import Foreign.C.String (peekCStringLen, withCString, withCStringLen)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "DuckDB V2 preview"
        [ testCase "environment refuses destruction while an instance is alive" testEnvironment
        , testCase "streamed query exposes schema, selection and validity" testQuery
        , testCase "statement binding exposes output and parameter schemas" testBind
        , testCase "live results reject another execution and cancellation releases the cursor" testCancellation
        , testCase "prepared execution binds named values repeatedly" testPrepared
        , testCase "length-delimited strings retain interior null bytes" testStrings
        , testCase "numeric structures round-trip through values" testNumericStructures
        , testCase "result errors own their diagnostic text" testError
        , testCase "scalar callbacks write result vectors" testScalar
        , testCase "native C text sink receives the rendered result" testTextSink
        , testCase "Arrow stream transfers result ownership and yields rows" testArrow
        ]

checked :: (Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO ()
checked action = alloca $ \err -> do
    poke err nullPtr
    code <- action err
    info <- peek err
    message <-
        if info == nullPtr
            then pure ""
            else alloca $ \text -> do
                textCode <- c_duckdb_v2_error_info_get_text info text
                if textCode == DuckDBV2ErrorNone
                    then peek text >>= readView
                    else pure ("diagnostic failed: " <> show textCode)
    void (c_duckdb_v2_error_info_destroy err)
    unless (code == DuckDBV2ErrorNone) $ assertFailure (show code <> ": " <> message)

withHandle :: (Ptr (Ptr a) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> (Ptr (Ptr a) -> IO DuckDBV2Error) -> (Ptr a -> IO b) -> IO b
withHandle create destroy action = alloca $ \slot -> do
    poke slot nullPtr
    checked (create slot)
    handle <- peek slot
    assertBool "created handle is non-null" (handle /= nullPtr)
    action handle `finally` (destroy slot >>= (@?= DuckDBV2ErrorNone))

withConnection :: (DuckDBV2ConnectionHandle -> IO a) -> IO a
withConnection action =
    withHandle c_duckdb_v2_environment_create c_duckdb_v2_environment_destroy $ \env ->
        withHandle (c_duckdb_v2_instance_create env) c_duckdb_v2_instance_destroy $ \instanceHandle -> do
            withView ":memory:" $ \path ->
                checked (c_duckdb_v2_instance_attach instanceHandle path nullPtr nullPtr (CBool 1))
            withHandle (c_duckdb_v2_connection_create instanceHandle) c_duckdb_v2_connection_destroy action

withView :: String -> (Ptr DuckDBV2Str -> IO a) -> IO a
withView text action = withCStringLen text $ \(ptr, len) -> with (DuckDBV2Str ptr (fromIntegral len)) action

readView :: DuckDBV2Str -> IO String
readView (DuckDBV2Str ptr len)
    | len == 0 = pure ""
    | otherwise = peekCStringLen (ptr, fromIntegral len)

withStatement :: DuckDBV2ConnectionHandle -> String -> (DuckDBV2SqlStatementHandle -> IO a) -> IO a
withStatement conn sql action = withCString sql $ \text ->
    withHandle (c_duckdb_v2_parse_sql conn text) c_duckdb_v2_statement_iterator_destroy $ \iterator ->
        withHandle (c_duckdb_v2_statement_iterator_next iterator) c_duckdb_v2_sql_statement_destroy action

withResult :: DuckDBV2ConnectionHandle -> String -> (DuckDBV2ResultHandle -> IO a) -> IO a
withResult conn sql action = withStatement conn sql $ \statement ->
    withHandle (c_duckdb_v2_statement_execute conn statement nullPtr nullPtr 0) c_duckdb_v2_result_destroy action

output :: (Storable a) => (Ptr a -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO a
output action = alloca $ \slot -> checked (action slot) >> peek slot

readIntegers :: DuckDBV2ResultHandle -> IO [Maybe Int64]
readIntegers result = alloca $ \chunkSlot -> do
    poke chunkSlot nullPtr
    checked (c_duckdb_v2_result_fetch_chunk result chunkSlot)
    chunk <- peek chunkSlot
    if chunk == nullPtr
        then pure []
        else do
            rows <-
                ( do
                    vector <- output (c_duckdb_v2_data_chunk_get_vector chunk 0)
                    view <- output (c_duckdb_v2_vector_get_view vector)
                    forM [0 .. fromIntegral (duckdbV2VectorViewCount view) - 1] $ \i -> do
                        index <-
                            if duckdbV2VectorViewSel view == nullPtr
                                then pure i
                                else fromIntegral <$> peekElemOff (duckdbV2VectorViewSel view) i
                        valid <-
                            if duckdbV2VectorViewValidity view == nullPtr
                                then pure True
                                else do
                                    word <- peekElemOff (duckdbV2VectorViewValidity view) (index `div` 64)
                                    pure (testBit word (index `mod` 64))
                        if valid
                            then Just <$> peekElemOff (castPtr (duckdbV2VectorViewData view)) index
                            else pure Nothing
                )
                    `finally` (c_duckdb_v2_data_chunk_destroy chunkSlot >>= (@?= DuckDBV2ErrorNone))
            rest <- readIntegers result
            pure (rows <> rest)

testEnvironment :: IO ()
testEnvironment = alloca $ \envSlot -> do
    poke envSlot nullPtr
    checked (c_duckdb_v2_environment_create envSlot)
    env <- peek envSlot
    withHandle (c_duckdb_v2_instance_create env) c_duckdb_v2_instance_destroy $ \_ -> do
        output (c_duckdb_v2_environment_get_instance_count env) >>= (@?= 1)
        c_duckdb_v2_environment_destroy envSlot >>= (@?= DuckDBV2ErrorResourceInUse)
        peek envSlot >>= (@?= env)
    output (c_duckdb_v2_environment_get_instance_count env) >>= (@?= 0)
    c_duckdb_v2_environment_destroy envSlot >>= (@?= DuckDBV2ErrorNone)
    peek envSlot >>= (@?= nullPtr)

testQuery :: IO ()
testQuery = withConnection $ \conn ->
    withResult conn "SELECT CASE WHEN i % 3 = 0 THEN NULL ELSE i END::BIGINT AS amount FROM range(10) t(i) WHERE i % 2 = 0 ORDER BY i" $ \result -> do
        withHandle (c_duckdb_v2_result_get_schema result) c_duckdb_v2_schema_destroy $ \schema -> do
            output (c_duckdb_v2_schema_get_count schema) >>= (@?= 1)
            alloca $ \name -> do
                ty <- output (c_duckdb_v2_schema_get_field schema 0 name)
                peek name >>= readView >>= (@?= "amount")
                output (c_duckdb_v2_logical_type_get_id ty) >>= (@?= DuckDBV2LogicalTypeIdBigint)
        readIntegers result >>= (@?= [Nothing, Just 2, Just 4, Nothing, Just 8])
        readIntegers result >>= (@?= [])

testBind :: IO ()
testBind = withConnection $ \conn -> withStatement conn "SELECT $amount::BIGINT AS total" $ \statement ->
    alloca $ \parameters -> do
        poke parameters nullPtr
        withHandle (\schema err -> c_duckdb_v2_statement_bind conn statement schema parameters err) c_duckdb_v2_schema_destroy $ \schema -> do
            output (c_duckdb_v2_schema_get_count schema) >>= (@?= 1)
            params <- peek parameters
            assertBool "parameter schema is non-null" (params /= nullPtr)
            (output (c_duckdb_v2_schema_get_count params) >>= (@?= 1))
                `finally` void (c_duckdb_v2_schema_destroy parameters)

testCancellation :: IO ()
testCancellation = withConnection $ \conn -> withResult conn "SELECT i::BIGINT FROM range(100000) t(i)" $ \result -> do
    withStatement conn "SELECT 9::BIGINT" $ \statement -> alloca $ \slot -> do
        poke slot nullPtr
        code <- c_duckdb_v2_statement_execute conn statement nullPtr nullPtr 0 slot nullPtr
        code @?= DuckDBV2ErrorResourceInUse
        peek slot >>= (@?= nullPtr)
    checked (c_duckdb_v2_connection_interrupt conn)
    alloca $ \chunk -> alloca $ \status -> do
        poke chunk nullPtr
        checked (c_duckdb_v2_result_step result chunk status)
        peek status >>= (@?= DuckDBV2ResultStepStatusCancelled)
        peek chunk >>= (@?= nullPtr)
    withResult conn "SELECT 9::BIGINT" $ \second -> readIntegers second >>= (@?= [Just 9])

testPrepared :: IO ()
testPrepared = withConnection $ \conn ->
    withStatement conn "SELECT $amount::BIGINT + 1 AS answer" $ \statement ->
        withHandle (c_duckdb_v2_prepared_statement_create conn statement (CBool 1)) c_duckdb_v2_prepared_statement_destroy $ \prepared -> do
            output (c_duckdb_v2_prepared_statement_reuses_plan prepared) >>= (@?= CBool 1)
            forM_ [40, 41] $ \amount ->
                withHandle (c_duckdb_v2_value_create_bigint_with_connection conn amount) c_duckdb_v2_value_destroy $ \value ->
                    withView "amount" $ \name -> withArray [value] $ \values ->
                        withHandle (c_duckdb_v2_prepared_statement_execute prepared name values 1) c_duckdb_v2_result_destroy $ \result ->
                            readIntegers result >>= (@?= [Just (amount + 1)])

testStrings :: IO ()
testStrings = withConnection $ \conn -> withView "alpha\0omega" $ \input ->
    withHandle (c_duckdb_v2_value_create_varchar_with_connection conn input) c_duckdb_v2_value_destroy $ \value -> do
        output (c_duckdb_v2_value_get_varchar value) >>= readView >>= (@?= "alpha\0omega")
        withStatement conn "SELECT length($text)::BIGINT" $ \statement ->
            withView "text" $ \name -> withArray [value] $ \values ->
                withHandle (c_duckdb_v2_statement_execute conn statement name values 1) c_duckdb_v2_result_destroy $ \result ->
                    readIntegers result >>= (@?= [Just 11])

testNumericStructures :: IO ()
testNumericStructures = withConnection $ \conn -> do
    let huge = DuckDBV2HugeintT 0xffffffffffffffff (-7)
        unsigned = DuckDBV2UhugeintT 0xfffffffffffffffd 13
        interval = DuckDBV2IntervalT 3 (-2) 1234567
    with huge $ \ptr ->
        withHandle (c_duckdb_v2_value_create_hugeint_with_connection conn ptr) c_duckdb_v2_value_destroy $ \value ->
            output (c_duckdb_v2_value_get_hugeint value) >>= (@?= huge)
    with unsigned $ \ptr ->
        withHandle (c_duckdb_v2_value_create_uhugeint_with_connection conn ptr) c_duckdb_v2_value_destroy $ \value ->
            output (c_duckdb_v2_value_get_uhugeint value) >>= (@?= unsigned)
    with interval $ \ptr ->
        withHandle (c_duckdb_v2_value_create_interval_with_connection conn ptr) c_duckdb_v2_value_destroy $ \value ->
            output (c_duckdb_v2_value_get_interval value) >>= (@?= interval)

testError :: IO ()
testError = withConnection $ \conn -> withStatement conn "SELECT absent_column" $ \statement -> alloca $ \result -> alloca $ \err -> do
    poke result nullPtr
    poke err nullPtr
    code <- c_duckdb_v2_statement_execute conn statement nullPtr nullPtr 0 result err
    code @?= DuckDBV2ErrorQueryBinder
    peek result >>= (@?= nullPtr)
    info <- peek err
    assertBool "failure has an owned error handle" (info /= nullPtr)
    alloca $ \stored -> do
        c_duckdb_v2_error_info_get_code info stored >>= (@?= DuckDBV2ErrorNone)
        peek stored >>= (@?= code)
    alloca $ \text -> do
        c_duckdb_v2_error_info_get_text info text >>= (@?= DuckDBV2ErrorNone)
        message <- peek text >>= readView
        assertBool "error identifies the missing column" ("absent_column" `isInfixOf` message)
    c_duckdb_v2_error_info_destroy err >>= (@?= DuckDBV2ErrorNone)
    peek err >>= (@?= nullPtr)

callbackGuard :: IORef [String] -> Ptr DuckDBV2ErrorInfoHandle -> IO () -> IO ()
callbackGuard failures err action =
    action `catch` \(failure :: SomeException) -> do
        modifyIORef' failures (show failure :)
        info <- peek err
        void (c_duckdb_v2_error_info_set_code info DuckDBV2ErrorApi)

testScalar :: IO ()
testScalar = do
    failures <- newIORef []
    calls <- newIORef (0 :: Int)
    let callback info _context err = callbackGuard failures err $ do
            modifyIORef' calls (+ 1)
            rows <- output (c_duckdb_v2_scalar_function_exec_get_row_count info)
            vector <- output (c_duckdb_v2_scalar_function_exec_get_result info)
            dataPtr <- output (c_duckdb_v2_vector_get_data_mutable vector)
            forM_ [0 .. fromIntegral rows - 1] $ \i -> pokeElemOff (castPtr dataPtr) i (42 :: Int64)
    bracket (mkDuckDBV2ScalarFunctionExecCallbackFn callback) freeHaskellFunPtr $ \callbackPtr ->
        withConnection $ \conn ->
            withHandle (c_duckdb_v2_connection_create_type_from_id conn DuckDBV2LogicalTypeIdBigint nullPtr nullPtr 0) c_duckdb_v2_logical_type_destroy $ \ty ->
                withHandle (c_duckdb_v2_scalar_function_create_with_connection conn) c_duckdb_v2_scalar_function_destroy $ \function -> do
                    withView "v2_answer" $ \name -> checked (c_duckdb_v2_scalar_function_set_name function name)
                    signature <- output (c_duckdb_v2_scalar_function_get_signature function)
                    checked (c_duckdb_v2_function_signature_set_return_type signature ty)
                    checked (c_duckdb_v2_scalar_function_set_exec_callback function callbackPtr)
                    checked (c_duckdb_v2_scalar_function_register function)
                    withResult conn "SELECT v2_answer() FROM range(3)" $ \result -> readIntegers result >>= (@?= replicate 3 (Just 42))
    readIORef failures >>= (@?= [])
    readIORef calls >>= assertBool "scalar callback was invoked" . (> 0)

testTextSink :: IO ()
testTextSink = do
    received <- newIORef []
    failures <- newIORef []
    let callback text _user err = callbackGuard failures err $ do
            rendered <- peek text >>= readView
            modifyIORef' received (rendered :)
    bracket (mkDuckDBV2TextSinkFn callback) freeHaskellFunPtr $ \sink -> withConnection $ \conn ->
        withStatement conn "SELECT 42::BIGINT AS answer" $ \statement -> alloca $ \result -> do
            poke result nullPtr
            checked (c_duckdb_v2_statement_execute conn statement nullPtr nullPtr 0 result)
            original <- peek result
            assertBool "execution returns a live result" (original /= nullPtr)
            flip finally (c_duckdb_v2_result_destroy result >>= (@?= DuckDBV2ErrorNone)) $
                withView "" $ \nullText -> do
                    alloca $ \err -> do
                        poke err nullPtr
                        flip finally (c_duckdb_v2_error_info_destroy err >>= (@?= DuckDBV2ErrorNone)) $ do
                            code <- c_duckdb_v2_result_render_box result 0 80 20 nullText 2 0 sink nullPtr err
                            code @?= DuckDBV2ErrorInputInvalid
                            peek result >>= (@?= original)
                            readIORef received >>= (@?= [])
                    checked (c_duckdb_v2_result_render_box result 0 80 20 nullText 0 0 sink nullPtr)
                    peek result >>= (@?= nullPtr)
    readIORef failures >>= (@?= [])
    rendered <- readIORef received
    length rendered @?= 1
    assertBool "rendered box contains its column and value" (all (\text -> "answer" `isInfixOf` text && "42" `isInfixOf` text) rendered)

testArrow :: IO ()
testArrow = withConnection $ \conn -> withStatement conn "SELECT i::BIGINT AS amount FROM range(5) t(i)" $ \statement -> alloca $ \result -> alloca $ \streamPtr -> do
    poke result nullPtr
    checked (c_duckdb_v2_statement_execute_arrow conn statement nullPtr nullPtr 0 0 result)
    fillBytes streamPtr 0 (sizeOf (undefined :: DuckDBV2ArrowArrayStream))
    checked (c_duckdb_v2_arrow_result_to_arrow_c_stream result streamPtr)
    peek result >>= (@?= nullPtr)
    stream <- peek streamPtr
    assertBool "Arrow stream owns a release callback" (duckdbV2ArrowArrayStreamRelease stream /= nullFunPtr)
    flip finally (callDuckDBV2ArrowArrayStreamReleaseFn (duckdbV2ArrowArrayStreamRelease stream) streamPtr) $ do
        alloca $ \schemaPtr -> do
            fillBytes schemaPtr 0 (sizeOf (undefined :: DuckDBV2ArrowSchema))
            callDuckDBV2ArrowArrayStreamGetSchemaFn (duckdbV2ArrowArrayStreamGetSchema stream) streamPtr schemaPtr >>= (@?= 0)
            schema <- peek schemaPtr
            duckdbV2ArrowSchemaNChildren schema @?= 1
            callDuckDBV2ArrowSchemaReleaseFn (duckdbV2ArrowSchemaRelease schema) schemaPtr
        let fetch total = alloca $ \arrayPtr -> do
                fillBytes arrayPtr 0 (sizeOf (undefined :: DuckDBV2ArrowArray))
                callDuckDBV2ArrowArrayStreamGetNextFn (duckdbV2ArrowArrayStreamGetNext stream) streamPtr arrayPtr >>= (@?= 0)
                array <- peek arrayPtr
                if duckdbV2ArrowArrayRelease array == nullFunPtr
                    then pure total
                    else do
                        let count = duckdbV2ArrowArrayLength array
                        callDuckDBV2ArrowArrayReleaseFn (duckdbV2ArrowArrayRelease array) arrayPtr
                        fetch (total + count)
        fetch 0 >>= (@?= 5)
