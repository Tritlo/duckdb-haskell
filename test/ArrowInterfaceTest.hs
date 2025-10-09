{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module ArrowInterfaceTest (tests) where

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Bits (testBit)
import Data.Char (toLower)
import Data.Int (Int32, Int64)
import Data.List (isInfixOf)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..), peek, peekByteOff, peekElemOff, poke, pokeByteOff, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Arrow Interface"
    [ arrowQueryMetadata
    , arrowErrorHandling
    , arrowSchemaRoundtrip
    , arrowChunkRoundtrip
    ]

arrowQueryMetadata :: TestTree
arrowQueryMetadata =
  testCase "query_arrow exposes schema and array data" $
    withDatabase \db ->
      withConnection db \conn ->
        withArrowResult conn "SELECT 1::INTEGER AS id, 'alpha'::VARCHAR AS label" \arrow -> do
          c_duckdb_arrow_column_count arrow >>= (@?= 2)
          c_duckdb_arrow_row_count arrow >>= (@?= 1)
          c_duckdb_arrow_rows_changed arrow >>= (@?= 0)

arrowErrorHandling :: TestTree
arrowErrorHandling =
  testCase "query_arrow surfaces planner errors" $
    withDatabase \db ->
      withConnection db \conn ->
        withCString "SELECT * FROM missing_table" \sql ->
          alloca \arrowPtr -> do
            poke arrowPtr nullPtr
            state <- c_duckdb_query_arrow conn sql arrowPtr
            state @?= DuckDBError
            arrow <- peek arrowPtr
            unless (arrow == nullPtr) $ do
              errMsgPtr <- c_duckdb_query_arrow_error arrow
              assertBool "expected non-null error pointer" (errMsgPtr /= nullPtr)
              errMsg <- peekCString errMsgPtr
              assertBool "error message mentions table" ("missing_table" `isInfixOf` map toLower errMsg)
            c_duckdb_destroy_arrow arrowPtr

arrowSchemaRoundtrip :: TestTree
arrowSchemaRoundtrip =
  testCase "to_arrow_schema exposes children and converts back" $
    withDatabase \db ->
      withConnection db \conn ->
        withArrowOptions conn \arrowOpts ->
          withLogicalTypes [DuckDBTypeInteger, DuckDBTypeVarchar] \logicalTypes ->
            withArray logicalTypes \logicalArray ->
              withColumnNames ["id", "label"] \nameArray ->
                withArrowSchemaStruct \schemaPtr -> do
                  errData <- c_duckdb_to_arrow_schema arrowOpts logicalArray nameArray (fromIntegral (length logicalTypes)) (castPtr schemaPtr)
                  assertNoError errData

                  schema <- peek schemaPtr
                  formatStr <- peekCString (arrowSchemaFormat schema)
                  formatStr @?= "+s"
                  arrowSchemaChildCount schema @?= fromIntegral (length logicalTypes)

                  let childCount = fromIntegral (arrowSchemaChildCount schema)
                  childArrayPtr <- pure (arrowSchemaChildren schema)
                  assertBool "children pointer should not be null" (childArrayPtr /= nullPtr)
                  childPtrs <- peekArray childCount childArrayPtr

                  firstChild <- peek (childPtrs !! 0)
                  secondChild <- peek (childPtrs !! 1)
                  peekCString (arrowSchemaName firstChild) >>= (@?= "id")
                  peekCString (arrowSchemaName secondChild) >>= (@?= "label")
                  assertBool "schema release pointer should be set" (arrowSchemaRelease schema /= nullFunPtr)

                  withConvertedSchema conn schemaPtr (const (pure ()))

                  releaseArrowSchema schemaPtr

arrowChunkRoundtrip :: TestTree
arrowChunkRoundtrip =
  testCase "data_chunk_to_arrow and from_arrow preserve values and nulls" $
    withDatabase \db ->
      withConnection db \conn ->
        withArrowOptions conn \arrowOpts ->
          withLogicalTypes [DuckDBTypeInteger] \logicalTypes ->
            withArray logicalTypes \logicalArray ->
              withColumnNames ["val"] \nameArray ->
                withArrowSchemaStruct \schemaPtr -> do
                  errSchema <- c_duckdb_to_arrow_schema arrowOpts logicalArray nameArray 1 (castPtr schemaPtr)
                  assertNoError errSchema

                  withConvertedSchema conn schemaPtr \convertedSchema ->
                    do
                      chunk <- c_duckdb_create_data_chunk logicalArray 1
                      assertBool "create_data_chunk should return chunk" (chunk /= nullPtr)

                      withOwnedChunk chunk \ownedChunk -> do
                        c_duckdb_data_chunk_set_size ownedChunk 2
                        vector <- c_duckdb_data_chunk_get_vector ownedChunk 0
                        dataPtr <- c_duckdb_vector_get_data vector
                        let intPtr = castPtr dataPtr :: Ptr Int32
                        pokeElemOff intPtr 0 42
                        pokeElemOff intPtr 1 0

                        c_duckdb_vector_ensure_validity_writable vector
                        maskPtr <- c_duckdb_vector_get_validity vector
                        assertBool "validity mask pointer should not be null" (maskPtr /= nullPtr)
                        c_duckdb_validity_set_row_valid maskPtr 0
                        c_duckdb_validity_set_row_invalid maskPtr 1

                        withArrowArrayStruct \arrayPtr ->
                          do
                          errArray <- c_duckdb_data_chunk_to_arrow arrowOpts ownedChunk (castPtr arrayPtr)
                          assertNoError errArray

                          array <- peek arrayPtr
                          arrowArrayLength array @?= 2
                          assertBool "release pointer should not be null before transfer" (arrowArrayRelease array /= nullFunPtr)

                          alloca \outChunkPtr -> do
                            poke outChunkPtr nullPtr
                            errFromArrow <- c_duckdb_data_chunk_from_arrow conn (castPtr arrayPtr) convertedSchema outChunkPtr
                            assertNoError errFromArrow
                            restoredChunk <- peek outChunkPtr
                            assertBool "restored chunk should not be null" (restoredChunk /= nullPtr)

                            withOwnedChunk restoredChunk $ \restored -> do
                              restoredSize <- c_duckdb_data_chunk_get_size restored
                              restoredSize @?= 2

                              restoredVector <- c_duckdb_data_chunk_get_vector restored 0
                              restoredDataPtr <- c_duckdb_vector_get_data restoredVector
                              let restoredIntPtr = castPtr restoredDataPtr :: Ptr Int32
                              restoredVal <- peekElemOff restoredIntPtr 0
                              restoredVal @?= 42

                              restoredMaskPtr <- c_duckdb_vector_get_validity restoredVector
                              assertBool "restored validity mask pointer should not be null" (restoredMaskPtr /= nullPtr)
                              restoredMaskWord <- peek restoredMaskPtr
                              assertBool "first row should be valid" (testBit restoredMaskWord 0)
                              assertBool "second row should be null" (not (testBit restoredMaskWord 1))

                            arrayAfter <- peek arrayPtr
                            arrowArrayRelease arrayAfter @?= nullFunPtr

                  releaseArrowSchema schemaPtr

withArrowResult :: DuckDBConnection -> String -> (DuckDBArrow -> IO a) -> IO a
withArrowResult conn sql action =
  withCString sql \sqlPtr ->
    alloca \arrowPtr -> do
      poke arrowPtr nullPtr
      state <- c_duckdb_query_arrow conn sqlPtr arrowPtr
      state @?= DuckDBSuccess
      arrow <- peek arrowPtr
      whenNull arrow $ assertFailure "duckdb_query_arrow returned null handle"
      result <- action arrow
      c_duckdb_destroy_arrow arrowPtr
      pure result
  where
    whenNull ptr actionFn = unless (ptr /= nullPtr) actionFn

withDatabase :: (DuckDBDatabase -> IO a) -> IO a
withDatabase action =
  withCString ":memory:" \path ->
    alloca \dbPtr -> do
      state <- c_duckdb_open path dbPtr
      state @?= DuckDBSuccess
      db <- peek dbPtr
      result <- action db
      c_duckdb_close dbPtr
      pure result

withConnection :: DuckDBDatabase -> (DuckDBConnection -> IO a) -> IO a
withConnection db action =
  alloca \connPtr -> do
    state <- c_duckdb_connect db connPtr
    state @?= DuckDBSuccess
    conn <- peek connPtr
    result <- action conn
    c_duckdb_disconnect connPtr
    pure result

withResult :: DuckDBConnection -> String -> (Ptr DuckDBResult -> IO a) -> IO a
withResult conn sql action =
  withCString sql \sqlPtr ->
    alloca \resPtr -> do
      state <- c_duckdb_query conn sqlPtr resPtr
      state @?= DuckDBSuccess
      result <- action resPtr
      c_duckdb_destroy_result resPtr
      pure result

withArrowOptions :: DuckDBConnection -> (DuckDBArrowOptions -> IO a) -> IO a
withArrowOptions conn action =
  alloca \optsPtr -> do
    let acquire = do
          poke optsPtr nullPtr
          c_duckdb_connection_get_arrow_options conn optsPtr
          opts <- peek optsPtr
          when (opts == nullPtr) $ assertFailure "duckdb_connection_get_arrow_options returned null"
          pure opts
        release _ = c_duckdb_destroy_arrow_options optsPtr
    bracket acquire release action

withLogicalTypes :: [DuckDBType] -> ([DuckDBLogicalType] -> IO a) -> IO a
withLogicalTypes [] action = action []
withLogicalTypes (t : ts) action =
  bracket (c_duckdb_create_logical_type t) destroyLogicalType \lt ->
    withLogicalTypes ts \rest -> action (lt : rest)

withColumnNames :: [String] -> (Ptr CString -> IO a) -> IO a
withColumnNames names action =
  withMany withCString names \cNames -> withArray cNames action

withArrowSchemaStruct :: (Ptr ArrowSchemaStruct -> IO a) -> IO a
withArrowSchemaStruct = withStruct zeroArrowSchema

withArrowArrayStruct :: (Ptr ArrowArrayStruct -> IO a) -> IO a
withArrowArrayStruct action =
  withStruct zeroArrowArray \ptr -> do
    result <- action ptr
    releaseArrowArray ptr
    pure result

withStruct :: Storable a => a -> (Ptr a -> IO b) -> IO b
withStruct initial action =
  alloca \ptr -> do
    poke ptr initial
    action ptr

withOwnedChunk :: DuckDBDataChunk -> (DuckDBDataChunk -> IO a) -> IO a
withOwnedChunk chunk = bracket (pure chunk) destroyChunk

withConvertedSchema :: DuckDBConnection -> Ptr ArrowSchemaStruct -> (DuckDBArrowConvertedSchema -> IO a) -> IO a
withConvertedSchema conn schemaPtr action =
  alloca \convertedPtr -> do
    poke convertedPtr nullPtr
    err <- c_duckdb_schema_from_arrow conn (castPtr schemaPtr) convertedPtr
    assertNoError err
    converted <- peek convertedPtr
    assertBool "converted schema pointer should not be null" (converted /= nullPtr)
    bracket (pure converted) destroyArrowConvertedSchema action

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt =
  alloca \ptr -> do
    poke ptr lt
    c_duckdb_destroy_logical_type ptr

destroyChunk :: DuckDBDataChunk -> IO ()
destroyChunk chunk =
  alloca \ptr -> do
    poke ptr chunk
    c_duckdb_destroy_data_chunk ptr

destroyArrowConvertedSchema :: DuckDBArrowConvertedSchema -> IO ()
destroyArrowConvertedSchema schema =
  alloca \ptr -> do
    poke ptr schema
    c_duckdb_destroy_arrow_converted_schema ptr

destroyErrorData :: DuckDBErrorData -> IO ()
destroyErrorData err =
  alloca \ptr -> do
    poke ptr err
    c_duckdb_destroy_error_data ptr

assertNoError :: DuckDBErrorData -> IO ()
assertNoError err =
  when (err /= nullPtr) $ do
    msgPtr <- c_duckdb_error_data_message err
    msg <- peekCString msgPtr
    destroyErrorData err
    assertFailure ("DuckDB reported error: " <> msg)

releaseArrowSchema :: Ptr ArrowSchemaStruct -> IO ()
releaseArrowSchema schemaPtr = do
  schema <- peek schemaPtr
  let releaseFun = arrowSchemaRelease schema
  when (releaseFun /= nullFunPtr) $ do
    let release = mkArrowSchemaRelease releaseFun
    release schemaPtr

releaseArrowArray :: Ptr ArrowArrayStruct -> IO ()
releaseArrowArray arrayPtr = do
  array <- peek arrayPtr
  let releaseFun = arrowArrayRelease array
  when (releaseFun /= nullFunPtr) $ do
    let release = mkArrowArrayRelease releaseFun
    release arrayPtr

data ArrowSchemaStruct = ArrowSchemaStruct
  { arrowSchemaFormat :: CString
  , arrowSchemaName :: CString
  , arrowSchemaMetadata :: CString
  , arrowSchemaFlags :: Int64
  , arrowSchemaChildCount :: Int64
  , arrowSchemaChildren :: Ptr (Ptr ArrowSchemaStruct)
  , arrowSchemaDictionary :: Ptr ArrowSchemaStruct
  , arrowSchemaRelease :: FunPtr (Ptr ArrowSchemaStruct -> IO ())
  , arrowSchemaPrivateData :: Ptr ()
  }

instance Storable ArrowSchemaStruct where
  sizeOf _ = pointerSize * 9
  alignment _ = alignment (nullPtr :: Ptr ())
  peek ptr =
    ArrowSchemaStruct
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr pointerSize
      <*> peekByteOff ptr (pointerSize * 2)
      <*> peekByteOff ptr (pointerSize * 3)
      <*> peekByteOff ptr (pointerSize * 4)
      <*> peekByteOff ptr (pointerSize * 5)
      <*> peekByteOff ptr (pointerSize * 6)
      <*> peekByteOff ptr (pointerSize * 7)
      <*> peekByteOff ptr (pointerSize * 8)
  poke ptr ArrowSchemaStruct{..} = do
    pokeByteOff ptr 0 arrowSchemaFormat
    pokeByteOff ptr pointerSize arrowSchemaName
    pokeByteOff ptr (pointerSize * 2) arrowSchemaMetadata
    pokeByteOff ptr (pointerSize * 3) arrowSchemaFlags
    pokeByteOff ptr (pointerSize * 4) arrowSchemaChildCount
    pokeByteOff ptr (pointerSize * 5) arrowSchemaChildren
    pokeByteOff ptr (pointerSize * 6) arrowSchemaDictionary
    pokeByteOff ptr (pointerSize * 7) arrowSchemaRelease
    pokeByteOff ptr (pointerSize * 8) arrowSchemaPrivateData

data ArrowArrayStruct = ArrowArrayStruct
  { arrowArrayLength :: Int64
  , arrowArrayNullCount :: Int64
  , arrowArrayOffset :: Int64
  , arrowArrayBufferCount :: Int64
  , arrowArrayChildCount :: Int64
  , arrowArrayBuffers :: Ptr (Ptr ())
  , arrowArrayChildren :: Ptr (Ptr ArrowArrayStruct)
  , arrowArrayDictionary :: Ptr ArrowArrayStruct
  , arrowArrayRelease :: FunPtr (Ptr ArrowArrayStruct -> IO ())
  , arrowArrayPrivateData :: Ptr ()
  }

instance Storable ArrowArrayStruct where
  sizeOf _ = pointerSize * 5 + intFieldSize * 5
  alignment _ = alignment (nullPtr :: Ptr ())
  peek ptr =
    ArrowArrayStruct
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr intFieldSize
      <*> peekByteOff ptr (intFieldSize * 2)
      <*> peekByteOff ptr (intFieldSize * 3)
      <*> peekByteOff ptr (intFieldSize * 4)
      <*> peekByteOff ptr (intFieldSize * 5)
      <*> peekByteOff ptr (intFieldSize * 5 + pointerSize)
      <*> peekByteOff ptr (intFieldSize * 5 + pointerSize * 2)
      <*> peekByteOff ptr (intFieldSize * 5 + pointerSize * 3)
      <*> peekByteOff ptr (intFieldSize * 5 + pointerSize * 4)
  poke ptr ArrowArrayStruct{..} = do
    pokeByteOff ptr 0 arrowArrayLength
    pokeByteOff ptr intFieldSize arrowArrayNullCount
    pokeByteOff ptr (intFieldSize * 2) arrowArrayOffset
    pokeByteOff ptr (intFieldSize * 3) arrowArrayBufferCount
    pokeByteOff ptr (intFieldSize * 4) arrowArrayChildCount
    pokeByteOff ptr (intFieldSize * 5) arrowArrayBuffers
    pokeByteOff ptr (intFieldSize * 5 + pointerSize) arrowArrayChildren
    pokeByteOff ptr (intFieldSize * 5 + pointerSize * 2) arrowArrayDictionary
    pokeByteOff ptr (intFieldSize * 5 + pointerSize * 3) arrowArrayRelease
    pokeByteOff ptr (intFieldSize * 5 + pointerSize * 4) arrowArrayPrivateData

zeroArrowSchema :: ArrowSchemaStruct
zeroArrowSchema =
  ArrowSchemaStruct
    { arrowSchemaFormat = nullPtr
    , arrowSchemaName = nullPtr
    , arrowSchemaMetadata = nullPtr
    , arrowSchemaFlags = 0
    , arrowSchemaChildCount = 0
    , arrowSchemaChildren = nullPtr
    , arrowSchemaDictionary = nullPtr
    , arrowSchemaRelease = nullFunPtr
    , arrowSchemaPrivateData = nullPtr
    }

zeroArrowArray :: ArrowArrayStruct
zeroArrowArray =
  ArrowArrayStruct
    { arrowArrayLength = 0
    , arrowArrayNullCount = 0
    , arrowArrayOffset = 0
    , arrowArrayBufferCount = 0
    , arrowArrayChildCount = 0
    , arrowArrayBuffers = nullPtr
    , arrowArrayChildren = nullPtr
    , arrowArrayDictionary = nullPtr
    , arrowArrayRelease = nullFunPtr
    , arrowArrayPrivateData = nullPtr
    }

pointerSize :: Int
pointerSize = sizeOf (nullPtr :: Ptr ())

intFieldSize :: Int
intFieldSize = sizeOf (0 :: Int64)

foreign import ccall "dynamic"
  mkArrowSchemaRelease :: FunPtr (Ptr ArrowSchemaStruct -> IO ()) -> Ptr ArrowSchemaStruct -> IO ()

foreign import ccall "dynamic"
  mkArrowArrayRelease :: FunPtr (Ptr ArrowArrayStruct -> IO ()) -> Ptr ArrowArrayStruct -> IO ()
