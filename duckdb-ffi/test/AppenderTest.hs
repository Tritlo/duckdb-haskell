{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module AppenderTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, when, (>=>))
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (isInfixOf)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString, withCStringLen)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, pokeElemOff)
import GHC.Records (getField)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (
    destroyErrorData,
    withConnection,
    withConstCString,
    withDatabase,
    withDuckValue,
    withLogicalType,
    withResultCString,
 )

tests :: TestTree
tests =
    testGroup
        "Appender"
        [ appenderRowwiseLifecycle
        , appenderColumnSubset
        , appenderDataChunkInsert
        , appenderQueryAppender
        , appenderNumericAndFloatScalars
        , appenderTemporalTypes
        , appenderStringAndBlob
        , appenderChunkDefaults
        , appenderErrorDataInspection
        ]

appenderRowwiseLifecycle :: TestTree
appenderRowwiseLifecycle =
    testCase "append rows using scalar APIs and inspect results" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement conn "CREATE TABLE appender_demo(id INTEGER, name VARCHAR, active BOOLEAN DEFAULT TRUE)"

                withTableAppender conn "appender_demo" \app -> do
                    c_duckdb_appender_column_count app >>= (@?= 3)
                    checkColumnType app 0 DUCKDB_TYPE_INTEGER
                    checkColumnType app 1 DUCKDB_TYPE_VARCHAR
                    checkColumnType app 2 DUCKDB_TYPE_BOOLEAN

                    c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                    c_duckdb_append_int32 app 1 >>= (@?= DuckDBSuccess)
                    withConstCString "alice" (c_duckdb_append_varchar app >=> (@?= DuckDBSuccess))
                    c_duckdb_append_bool app (CBool 1) >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                    c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                    c_duckdb_append_int32 app 2 >>= (@?= DuckDBSuccess)
                    c_duckdb_append_null app >>= (@?= DuckDBSuccess)
                    c_duckdb_append_bool app (CBool 0) >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                    c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                    c_duckdb_append_int32 app 3 >>= (@?= DuckDBSuccess)
                    withDuckValue (withConstCString "via_value" c_duckdb_create_varchar) (c_duckdb_append_value app >=> (@?= DuckDBSuccess))
                    c_duckdb_append_default app >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                    errPtr0 <- c_duckdb_appender_error app
                    when (errPtr0 /= (coerce (nullPtr :: Ptr Void))) $ do
                        msg <- (peekCString . coerce) errPtr0
                        msg @?= ""

                    c_duckdb_appender_flush app >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_close app >>= (@?= DuckDBSuccess)

                    withConstCString "SELECT id, name, active FROM appender_demo ORDER BY id" \sql ->
                        withResultCString conn sql \resPtr -> do
                            c_duckdb_row_count resPtr >>= (@?= 3)

                            c_duckdb_value_int32 resPtr 0 0 >>= (@?= 1)
                            fetchString resPtr 1 0 >>= (@?= "alice")
                            fetchBool resPtr 2 0 >>= (@?= True)

                            c_duckdb_value_int32 resPtr 0 1 >>= (@?= 2)
                            c_duckdb_value_is_null resPtr 1 1 >>= (@?= CBool 1)
                            fetchBool resPtr 2 1 >>= (@?= False)

                            c_duckdb_value_int32 resPtr 0 2 >>= (@?= 3)
                            fetchString resPtr 1 2 >>= (@?= "via_value")
                            fetchBool resPtr 2 2 >>= (@?= True)

appenderColumnSubset :: TestTree
appenderColumnSubset =
    testCase "restrict active columns and rely on table defaults" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement
                    conn
                    "CREATE TABLE subset_demo(id INTEGER DEFAULT 100, name VARCHAR, note VARCHAR, active BOOLEAN DEFAULT FALSE)"

                withTableAppender conn "subset_demo" \app -> do
                    c_duckdb_appender_clear_columns app >>= (@?= DuckDBSuccess)
                    withConstCString "name" (c_duckdb_appender_add_column app >=> (@?= DuckDBSuccess))
                    withConstCString "note" (c_duckdb_appender_add_column app >=> (@?= DuckDBSuccess))

                    c_duckdb_appender_column_count app >>= (@?= 2)

                    c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                    withConstCString "subset-one" (c_duckdb_append_varchar app >=> (@?= DuckDBSuccess))
                    withConstCString "note one" (c_duckdb_append_varchar app >=> (@?= DuckDBSuccess))
                    c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                    c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                    withConstCString "subset-two" (c_duckdb_append_varchar app >=> (@?= DuckDBSuccess))
                    c_duckdb_append_null app >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                    c_duckdb_appender_flush app >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_close app >>= (@?= DuckDBSuccess)

                    withConstCString "SELECT id, name, note, active FROM subset_demo ORDER BY rowid" \sql ->
                        withResultCString conn sql \resPtr -> do
                            c_duckdb_row_count resPtr >>= (@?= 2)

                            c_duckdb_value_int32 resPtr 0 0 >>= (@?= 100)
                            fetchString resPtr 1 0 >>= (@?= "subset-one")
                            fetchString resPtr 2 0 >>= (@?= "note one")
                            fetchBool resPtr 3 0 >>= (@?= False)

                            c_duckdb_value_int32 resPtr 0 1 >>= (@?= 100)
                            fetchString resPtr 1 1 >>= (@?= "subset-two")
                            c_duckdb_value_is_null resPtr 2 1 >>= (@?= CBool 1)
                            fetchBool resPtr 3 1 >>= (@?= False)

appenderDataChunkInsert :: TestTree
appenderDataChunkInsert =
    testCase "append via data chunk using extended constructor" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement conn "CREATE TABLE chunk_demo(id INTEGER, label VARCHAR)"

                withTableAppenderExt conn "chunk_demo" \app -> do
                    c_duckdb_appender_column_count app >>= (@?= 2)

                    withLogicalType (c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_INTEGER)) \intType ->
                        withLogicalType (c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_VARCHAR)) \textType ->
                            withArray [intType, textType] \typeArray ->
                                withDataChunk (c_duckdb_create_data_chunk typeArray 2) \chunk -> do
                                    intVec <- c_duckdb_data_chunk_get_vector chunk 0
                                    fillIntVector intVec [10, 11]

                                    textVec <- c_duckdb_data_chunk_get_vector chunk 1
                                    assignStrings textVec ["ten", "eleven"]

                                    c_duckdb_data_chunk_set_size chunk 2
                                    c_duckdb_append_data_chunk app chunk >>= (@?= DuckDBSuccess)

                    c_duckdb_appender_flush app >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_close app >>= (@?= DuckDBSuccess)

                    withConstCString "SELECT id, label FROM chunk_demo ORDER BY id" \sql ->
                        withResultCString conn sql \resPtr -> do
                            c_duckdb_row_count resPtr >>= (@?= 2)
                            c_duckdb_value_int32 resPtr 0 0 >>= (@?= 10)
                            fetchString resPtr 1 0 >>= (@?= "ten")
                            c_duckdb_value_int32 resPtr 0 1 >>= (@?= 11)
                            fetchString resPtr 1 1 >>= (@?= "eleven")

appenderQueryAppender :: TestTree
appenderQueryAppender =
    testCase "append rows through query-based appender" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement conn "CREATE TABLE query_target(id INTEGER, label VARCHAR)"

                withLogicalType (c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_INTEGER)) \intType ->
                    withLogicalType (c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_VARCHAR)) \textType -> do
                        let types = [intType, textType]
                        withQueryAppender conn "INSERT INTO query_target SELECT * FROM appended_data" types \app -> do
                            c_duckdb_appender_column_count app >>= (@?= 2)

                            c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                            c_duckdb_append_int32 app 21 >>= (@?= DuckDBSuccess)
                            withConstCString "twenty-one" (c_duckdb_append_varchar app >=> (@?= DuckDBSuccess))
                            c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                            c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                            c_duckdb_append_int32 app 22 >>= (@?= DuckDBSuccess)
                            withConstCString "twenty-two" (c_duckdb_append_varchar app >=> (@?= DuckDBSuccess))
                            c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                            c_duckdb_appender_flush app >>= (@?= DuckDBSuccess)
                            c_duckdb_appender_close app >>= (@?= DuckDBSuccess)

                        withConstCString "SELECT id, label FROM query_target ORDER BY id" \sql ->
                            withResultCString conn sql \resPtr -> do
                                c_duckdb_row_count resPtr >>= (@?= 2)
                                c_duckdb_value_int32 resPtr 0 0 >>= (@?= 21)
                                fetchString resPtr 1 0 >>= (@?= "twenty-one")
                                c_duckdb_value_int32 resPtr 0 1 >>= (@?= 22)
                                fetchString resPtr 1 1 >>= (@?= "twenty-two")

appenderNumericAndFloatScalars :: TestTree
appenderNumericAndFloatScalars =
    testCase "append numeric scalar types" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement
                    conn
                    ( unlines
                        [ "CREATE TABLE numeric_scalars_demo("
                        , "  signed8 TINYINT,"
                        , "  signed16 SMALLINT,"
                        , "  signed64 BIGINT,"
                        , "  signed128 HUGEINT,"
                        , "  unsigned8 UTINYINT,"
                        , "  unsigned16 USMALLINT,"
                        , "  unsigned32 UINTEGER,"
                        , "  unsigned64 UBIGINT,"
                        , "  unsigned128 UHUGEINT,"
                        , "  real32 FLOAT,"
                        , "  real64 DOUBLE"
                        , ")"
                        ]
                    )

                let int8Val = (-12) :: Int8
                    int16Val = (-32000) :: Int16
                    int64Val = (-9876543210) :: Int64
                    hugeVal = DuckDBHugeInt 0xFEDCBA9876543210 0
                    uint8Val = 200 :: Word8
                    uint16Val = 60000 :: Word16
                    uint32Val = 4000000000 :: Word32
                    uint64Val = 12345678901234567890 :: Word64
                    uhugeVal = DuckDBUHugeInt 0x0123456789ABCDEF 0x0011223344556677
                    floatVal = (-12.5) :: Float
                    doubleVal = 1234.5678 :: Double

                withTableAppender conn "numeric_scalars_demo" \app -> do
                    c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                    c_duckdb_append_int8 app int8Val >>= (@?= DuckDBSuccess)
                    c_duckdb_append_int16 app int16Val >>= (@?= DuckDBSuccess)
                    c_duckdb_append_int64 app int64Val >>= (@?= DuckDBSuccess)
                    c_duckdb_append_hugeint app hugeVal >>= (@?= DuckDBSuccess)
                    c_duckdb_append_uint8 app uint8Val >>= (@?= DuckDBSuccess)
                    c_duckdb_append_uint16 app uint16Val >>= (@?= DuckDBSuccess)
                    c_duckdb_append_uint32 app uint32Val >>= (@?= DuckDBSuccess)
                    c_duckdb_append_uint64 app uint64Val >>= (@?= DuckDBSuccess)
                    c_duckdb_append_uhugeint app uhugeVal >>= (@?= DuckDBSuccess)
                    c_duckdb_append_float app (realToFrac floatVal) >>= (@?= DuckDBSuccess)
                    c_duckdb_append_double app (realToFrac doubleVal) >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                    c_duckdb_appender_flush app >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_close app >>= (@?= DuckDBSuccess)

                let query =
                        unlines
                            [ "SELECT"
                            , "  signed8,"
                            , "  signed16,"
                            , "  signed64,"
                            , "  signed128,"
                            , "  unsigned8,"
                            , "  unsigned16,"
                            , "  unsigned32,"
                            , "  unsigned64,"
                            , "  unsigned128,"
                            , "  real32,"
                            , "  real64"
                            , "FROM numeric_scalars_demo"
                            ]

                withConstCString query \sql ->
                    withResultCString conn sql \resPtr -> do
                        c_duckdb_row_count resPtr >>= (@?= 1)
                        c_duckdb_value_int8 resPtr 0 0 >>= (@?= int8Val)
                        c_duckdb_value_int16 resPtr 1 0 >>= (@?= int16Val)
                        c_duckdb_value_int64 resPtr 2 0 >>= (@?= int64Val)
                        alloca \ptr -> do
                            (c_duckdb_value_hugeint resPtr 3 0 >>= poke ptr)
                            peek ptr >>= (@?= hugeVal)
                        c_duckdb_value_uint8 resPtr 4 0 >>= (@?= uint8Val)
                        c_duckdb_value_uint16 resPtr 5 0 >>= (@?= uint16Val)
                        c_duckdb_value_uint32 resPtr 6 0 >>= (@?= uint32Val)
                        c_duckdb_value_uint64 resPtr 7 0 >>= (@?= uint64Val)
                        alloca \ptr -> do
                            (c_duckdb_value_uhugeint resPtr 8 0 >>= poke ptr)
                            peek ptr >>= (@?= uhugeVal)
                        fv <- c_duckdb_value_float resPtr 9 0
                        realToFrac fv @?= floatVal
                        dv <- c_duckdb_value_double resPtr 10 0
                        realToFrac dv @?= doubleVal

appenderTemporalTypes :: TestTree
appenderTemporalTypes =
    testCase "append temporal values" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement
                    conn
                    "CREATE TABLE temporal_demo(d DATE, t TIME, ts TIMESTAMP, iv INTERVAL)"

                let dateStruct = DuckDBDateStruct 2024 3 31
                dateVal <- c_duckdb_to_date dateStruct

                let timeStruct = DuckDBTimeStruct 12 34 56 987654
                timeVal <- c_duckdb_to_time timeStruct

                let timestampStruct = DuckDBTimestampStruct dateStruct timeStruct
                timestampVal <- c_duckdb_to_timestamp timestampStruct

                let intervalVal = DuckDBInterval 5 12 3456789

                withTableAppender conn "temporal_demo" \app -> do
                    c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                    c_duckdb_append_date app dateVal >>= (@?= DuckDBSuccess)
                    c_duckdb_append_time app timeVal >>= (@?= DuckDBSuccess)
                    c_duckdb_append_timestamp app timestampVal >>= (@?= DuckDBSuccess)
                    c_duckdb_append_interval app intervalVal >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                    c_duckdb_appender_flush app >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_close app >>= (@?= DuckDBSuccess)

                withConstCString "SELECT d, t, ts, iv FROM temporal_demo" \sql ->
                    withResultCString conn sql \resPtr -> do
                        c_duckdb_row_count resPtr >>= (@?= 1)

                        fetchedDate <- c_duckdb_value_date resPtr 0 0
                        alloca \structPtr -> do
                            (c_duckdb_from_date fetchedDate >>= poke structPtr)
                            peek structPtr >>= (@?= dateStruct)

                        fetchedTime <- c_duckdb_value_time resPtr 1 0
                        alloca \structPtr -> do
                            (c_duckdb_from_time fetchedTime >>= poke structPtr)
                            peek structPtr >>= (@?= timeStruct)

                        fetchedTimestamp <- c_duckdb_value_timestamp resPtr 2 0
                        alloca \structPtr -> do
                            (c_duckdb_from_timestamp fetchedTimestamp >>= poke structPtr)
                            peek structPtr >>= (@?= timestampStruct)

                        alloca \intervalPtr -> do
                            (c_duckdb_value_interval resPtr 3 0 >>= poke intervalPtr)
                            peek intervalPtr >>= (@?= intervalVal)

appenderStringAndBlob :: TestTree
appenderStringAndBlob =
    testCase "append bounded varchar and blob values" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement
                    conn
                    "CREATE TABLE string_blob_demo(text_fragment VARCHAR, payload BLOB)"

                let blobBytes = [0xde, 0xad, 0xbe, 0xef] :: [Word8]

                withTableAppender conn "string_blob_demo" \app -> do
                    c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                    withCStringLen "hello world" \(ptr, _len) -> do
                        c_duckdb_append_varchar_length app (coerce ptr) (fromIntegral @Integer 5) >>= (@?= DuckDBSuccess)
                    withArray blobBytes \ptr -> do
                        c_duckdb_append_blob app (coerce ptr) (fromIntegral (length blobBytes)) >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                    c_duckdb_appender_flush app >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_close app >>= (@?= DuckDBSuccess)

                withConstCString "SELECT text_fragment, payload FROM string_blob_demo" \sql ->
                    withResultCString conn sql \resPtr -> do
                        c_duckdb_row_count resPtr >>= (@?= 1)
                        fetchString resPtr 0 0 >>= (@?= "hello")
                        fetchBlob resPtr 1 0 >>= (@?= blobBytes)

appenderChunkDefaults :: TestTree
appenderChunkDefaults =
    testCase "populate chunk entries using column defaults" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement
                    conn
                    "CREATE TABLE chunk_defaults_demo(val INTEGER DEFAULT 99, note VARCHAR DEFAULT 'fallback')"

                withTableAppender conn "chunk_defaults_demo" \app -> do
                    c_duckdb_appender_column_count app >>= (@?= 2)

                    withLogicalType (c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_INTEGER)) \intType ->
                        withLogicalType (c_duckdb_create_logical_type (DuckDBType DUCKDB_TYPE_VARCHAR)) \textType ->
                            withArray [intType, textType] \typeArray ->
                                withDataChunk (c_duckdb_create_data_chunk typeArray 2) \chunk -> do
                                    intVec <- c_duckdb_data_chunk_get_vector chunk 0
                                    fillIntVector intVec [10]

                                    textVec <- c_duckdb_data_chunk_get_vector chunk 1
                                    assignStrings textVec ["explicit"]

                                    c_duckdb_append_default_to_chunk app chunk 0 1 >>= (@?= DuckDBSuccess)
                                    c_duckdb_append_default_to_chunk app chunk 1 1 >>= (@?= DuckDBSuccess)

                                    c_duckdb_data_chunk_set_size chunk 2
                                    c_duckdb_append_data_chunk app chunk >>= (@?= DuckDBSuccess)

                    c_duckdb_appender_flush app >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_close app >>= (@?= DuckDBSuccess)

                withConstCString "SELECT val, note FROM chunk_defaults_demo ORDER BY rowid" \sql ->
                    withResultCString conn sql \resPtr -> do
                        c_duckdb_row_count resPtr >>= (@?= 2)
                        c_duckdb_value_int32 resPtr 0 0 >>= (@?= 10)
                        fetchString resPtr 1 0 >>= (@?= "explicit")
                        c_duckdb_value_int32 resPtr 0 1 >>= (@?= 99)
                        fetchString resPtr 1 1 >>= (@?= "fallback")

appenderErrorDataInspection :: TestTree
appenderErrorDataInspection =
    testCase "retrieve appender error payloads" $
        withDatabase \db ->
            withConnection db \conn -> do
                runStatement
                    conn
                    "CREATE TABLE error_demo(val INTEGER CHECK (val > 0))"

                withTableAppender conn "error_demo" \app -> do
                    c_duckdb_appender_begin_row app >>= (@?= DuckDBSuccess)
                    c_duckdb_append_int32 app (-1) >>= (@?= DuckDBSuccess)
                    c_duckdb_appender_end_row app >>= (@?= DuckDBSuccess)

                    flushState <- c_duckdb_appender_flush app
                    flushState @?= DuckDBError

                    errData <- c_duckdb_appender_error_data app
                    CBool hasError <- c_duckdb_error_data_has_error errData
                    assertBool "error data indicates failure" (hasError /= 0)

                    errType <- c_duckdb_error_data_error_type errData
                    errType @?= DUCKDB_ERROR_CONSTRAINT

                    errMsgPtr <- c_duckdb_error_data_message errData
                    errMsg <- (peekCString . coerce) errMsgPtr
                    assertBool "constraint violation message mentions CHECK" ("CHECK" `isInfixOf` errMsg)

                    destroyErrorData errData

                withConstCString "SELECT COUNT(*) FROM error_demo" \sql ->
                    withResultCString conn sql \resPtr -> do
                        c_duckdb_row_count resPtr >>= (@?= 1)
                        c_duckdb_value_int64 resPtr 0 0 >>= (@?= 0)

-- Helpers -------------------------------------------------------------------

runStatement :: DuckDBConnection -> String -> IO ()
runStatement conn sql =
    withConstCString sql \sqlPtr ->
        alloca \resPtr -> do
            state <- c_duckdb_query conn sqlPtr resPtr
            if state == DuckDBSuccess
                then c_duckdb_destroy_result resPtr
                else do
                    errPtr <- c_duckdb_result_error resPtr
                    errMsg <-
                        if errPtr == (coerce (nullPtr :: Ptr Void))
                            then pure "unknown error"
                            else (peekCString . coerce) errPtr
                    c_duckdb_destroy_result resPtr
                    assertFailure ("duckdb_query failed: " <> errMsg)

withTableAppender :: DuckDBConnection -> String -> (DuckDBAppender -> IO a) -> IO a
withTableAppender conn tableName action =
    withConstCString tableName \tablePtr ->
        withAppenderAcquire
            (c_duckdb_appender_create conn (coerce (nullPtr :: Ptr Void)) tablePtr)
            action

withTableAppenderExt :: DuckDBConnection -> String -> (DuckDBAppender -> IO a) -> IO a
withTableAppenderExt conn tableName action =
    withConstCString tableName \tablePtr ->
        withAppenderAcquire
            (c_duckdb_appender_create_ext conn (coerce (nullPtr :: Ptr Void)) (coerce (nullPtr :: Ptr Void)) tablePtr)
            action

withQueryAppender :: DuckDBConnection -> String -> [DuckDBLogicalType] -> (DuckDBAppender -> IO a) -> IO a
withQueryAppender conn query types action =
    withConstCString query \queryPtr ->
        withArray types \typeArray ->
            withAppenderAcquire
                (c_duckdb_appender_create_query conn queryPtr (fromIntegral (length types)) typeArray (coerce (nullPtr :: Ptr Void)) (coerce (nullPtr :: Ptr Void)))
                action

withAppenderAcquire :: (Ptr DuckDBAppender -> IO DuckDBState) -> (DuckDBAppender -> IO a) -> IO a
withAppenderAcquire acquire action =
    alloca \appPtr -> do
        state <- acquire appPtr
        state @?= DuckDBSuccess
        app <- peek appPtr
        let release = do
                destroyState <- c_duckdb_appender_destroy appPtr
                assertBool "destroy returns success or error" (destroyState == DuckDBSuccess || destroyState == DuckDBError)
        action app `finally` release

withDataChunk :: IO DuckDBDataChunk -> (DuckDBDataChunk -> IO a) -> IO a
withDataChunk acquire = bracket acquire destroyChunk
  where
    destroyChunk chunk = alloca \ptr -> poke ptr chunk >> c_duckdb_destroy_data_chunk ptr

fillIntVector :: DuckDBVector -> [Int32] -> IO ()
fillIntVector vec values = do
    dataPtrRaw <- c_duckdb_vector_get_data vec
    let dataPtr = coerce dataPtrRaw :: Ptr Int32
    forM_ (zip [0 ..] values) (uncurry (pokeElemOff dataPtr))

assignStrings :: DuckDBVector -> [String] -> IO ()
assignStrings vec values =
    forM_ (zip [0 ..] values) \(idx, val) ->
        withConstCString val $ \str ->
            c_duckdb_vector_assign_string_element vec (fromIntegral @Integer idx) str

checkColumnType :: DuckDBAppender -> DuckDBIdx -> DUCKDB_TYPE -> IO ()
checkColumnType app idx expected =
    do
        logicalType <- c_duckdb_appender_column_type app idx
        withLogicalType (pure logicalType) ((fmap (getField @"unwrap") . c_duckdb_get_type_id) >=> (@?= expected))

fetchString :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO String
fetchString resPtr col row = do
    strPtr <- c_duckdb_value_varchar resPtr col row
    text <- (peekCString . coerce) strPtr
    c_duckdb_free (coerce strPtr)
    pure text

fetchBlob :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO [Word8]
fetchBlob resPtr col row =
    alloca \blobPtr -> do
        (c_duckdb_value_blob resPtr col row >>= poke blobPtr)
        DuckDBBlob dat len <- peek blobPtr
        let dataPtr = coerce dat :: Ptr Word8
        bytes <- peekArray (fromIntegral len) dataPtr
        c_duckdb_free dat
        pure bytes

fetchBool :: Ptr DuckDBResult -> DuckDBIdx -> DuckDBIdx -> IO Bool
fetchBool resPtr col row = do
    CBool val <- c_duckdb_value_boolean resPtr col row
    pure (val /= 0)
