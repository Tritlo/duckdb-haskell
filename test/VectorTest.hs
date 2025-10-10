{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module VectorTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Bits (clearBit, complement, setBit)
import Data.Int (Int32)
import Data.Word (Word64)
import Database.DuckDB.FFI
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, withArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff, sizeOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- | Entry point for vector-centric tests.
tests :: TestTree
tests =
  testGroup
    "Vector Interface"
    [ vectorDataAccess
    , vectorValidityMask
    , listVectorChildManagement
    , arrayVectorChildAccess
    , vectorSliceWithSelection
    , structVectorChildAccess
    , vectorReferenceValue
    ]

-- | Create a numeric vector, write elements, and observe the stored values.
vectorDataAccess :: TestTree
vectorDataAccess =
  testCase "write and read integer vector data" $ do
    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
      withVector intType 4 \vec -> do
        colType <- c_duckdb_vector_get_column_type vec
        withLogicalType (pure colType) \lt -> c_duckdb_get_type_id lt >>= (@?= DuckDBTypeInteger)

        rawPtr <- c_duckdb_vector_get_data vec
        let dataPtr = castPtr rawPtr :: Ptr Int32
        forM_ (zip [0 ..] [10, 20, 30, 40]) \(idx, val) -> poke (dataPtr `plusElem` idx) val

        forM_ (zip [0 ..] [10, 20, 30, 40]) \(idx, val) -> peek (dataPtr `plusElem` idx) >>= (@?= val)

-- | Ensure validity mask allocation and manipulate individual bits.
vectorValidityMask :: TestTree
vectorValidityMask =
  testCase "ensure validity mask and clear single entry" $ do
    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
      withVector intType 4 \vec -> do
        void (c_duckdb_vector_ensure_validity_writable vec)
        validity <- c_duckdb_vector_get_validity vec
        assertBool "validity mask should exist" (validity /= nullPtr)

        setAllValid validity 4
        clearValidityBit validity 2

        current <- peek validity
        let expected = foldl setBit (0 :: Word64) [0, 1, 3]
        current @?= expected

-- | Exercise list vector child management APIs.
listVectorChildManagement :: TestTree
listVectorChildManagement =
  testCase "list vector reserves space and reports size" $ do
    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
      withLogicalType (c_duckdb_create_list_type intType) \listType ->
        withVector listType 2 \listVec -> do
          childVec0 <- c_duckdb_list_vector_get_child listVec
          assertBool "list child vector should be non-null" (childVec0 /= nullPtr)
          initialSize <- c_duckdb_list_vector_get_size listVec
          initialSize @?= 0

          reserveState <- c_duckdb_list_vector_reserve listVec 5
          reserveState @?= DuckDBSuccess
          sizeState <- c_duckdb_list_vector_set_size listVec 5
          sizeState @?= DuckDBSuccess
          c_duckdb_list_vector_get_size listVec >>= (@?= 5)

          childVec <- c_duckdb_list_vector_get_child listVec
          metaRaw <- c_duckdb_vector_get_data listVec
          let metaWords = castPtr metaRaw :: Ptr Word64
          -- row 0 -> offset 0 length 3, row 1 -> offset 3 length 2
          pokeElemOff metaWords 0 0
          pokeElemOff metaWords 1 3
          pokeElemOff metaWords 2 3
          pokeElemOff metaWords 3 2

          childRaw <- c_duckdb_vector_get_data childVec
          let childInts = castPtr childRaw :: Ptr Int32
              payload = [11, 12, 13, 21, 22 :: Int32]
          forM_ (zip [0 ..] payload) \(idx, val) -> pokeElemOff childInts idx val
          fetched <- mapM (peekElemOff childInts) [0 .. length payload - 1]
          fetched @?= payload

          peekElemOff metaWords 1 >>= (@?= 3)
          peekElemOff metaWords 3 >>= (@?= 2)

-- | Verify array child vector exposes a flat buffer sized by row * array length.
arrayVectorChildAccess :: TestTree
arrayVectorChildAccess =
  testCase "array vector child flattens elements" $ do
    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
      withLogicalType (c_duckdb_create_array_type intType 3) \arrayType ->
        withVector arrayType 2 \arrayVec -> do
          childVec <- c_duckdb_array_vector_get_child arrayVec
          childType <- c_duckdb_vector_get_column_type childVec
          withLogicalType (pure childType) \ct -> c_duckdb_get_type_id ct >>= (@?= DuckDBTypeInteger)

          childRaw <- c_duckdb_vector_get_data childVec
          let childInts = castPtr childRaw :: Ptr Int32
              payload = [1, 2, 3, 4, 5, 6 :: Int32]
          forM_ (zip [0 ..] payload) \(idx, val) -> pokeElemOff childInts idx val
          mapM (peekElemOff childInts) [0 .. length payload - 1] >>= (@?= payload)

-- | Slice a vector with a selection vector and materialize the dictionary.
vectorSliceWithSelection :: TestTree
vectorSliceWithSelection =
  testCase "slice vector materializes dictionary order" $ do
    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
      withVector intType 4 \vec -> do
        dataRaw <- c_duckdb_vector_get_data vec
        let vecData = castPtr dataRaw :: Ptr Int32
        forM_ (zip [0 ..] [10, 20, 30, 40 :: Int32]) \(idx, val) -> pokeElemOff vecData idx val

        withSelectionVector 2 \sel -> do
          selPtr <- c_duckdb_selection_vector_get_data_ptr sel
          pokeElemOff selPtr 0 (fromIntegral (3 :: Int))
          pokeElemOff selPtr 1 (fromIntegral (1 :: Int))
          c_duckdb_slice_vector vec sel 2

          withVector intType 2 \materialized ->
            withSelectionVector 2 \copySel -> do
              copyPtr <- c_duckdb_selection_vector_get_data_ptr copySel
              pokeElemOff copyPtr 0 0
              pokeElemOff copyPtr 1 1
              c_duckdb_vector_copy_sel vec materialized copySel 2 0 0
              matRaw <- c_duckdb_vector_get_data materialized
              let matPtr = castPtr matRaw :: Ptr Int32
              mapM (peekElemOff matPtr) [0, 1] >>= (@?= [40, 20])

-- | Ensure struct vectors expose individual child vectors with matching types.
structVectorChildAccess :: TestTree
structVectorChildAccess =
  testCase "struct vector exposes typed child vectors" $ do
    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
      withLogicalType (c_duckdb_create_logical_type DuckDBTypeDouble) \doubleType ->
        allocaArray 2 \typesPtr -> do
          pokeElemOff typesPtr 0 intType
          pokeElemOff typesPtr 1 doubleType
          withCString "ints" \name0 ->
            withCString "doubles" \name1 ->
              withArray [name0, name1] \namesPtr ->
                withLogicalType (c_duckdb_create_struct_type typesPtr namesPtr 2) \structType ->
                  withVector structType 1 \structVec -> do
                    intChild <- c_duckdb_struct_vector_get_child structVec 0
                    dblChild <- c_duckdb_struct_vector_get_child structVec 1

                    intChildType <- c_duckdb_vector_get_column_type intChild
                    withLogicalType (pure intChildType) \lt -> c_duckdb_get_type_id lt >>= (@?= DuckDBTypeInteger)
                    dblChildType <- c_duckdb_vector_get_column_type dblChild
                    withLogicalType (pure dblChildType) \lt -> c_duckdb_get_type_id lt >>= (@?= DuckDBTypeDouble)

                    intRaw <- c_duckdb_vector_get_data intChild
                    dblRaw <- c_duckdb_vector_get_data dblChild
                    let intPtr = castPtr intRaw :: Ptr Int32
                        dblPtr = castPtr dblRaw :: Ptr Double
                    pokeElemOff intPtr 0 7
                    pokeElemOff dblPtr 0 3.5
                    peekElemOff intPtr 0 >>= (@?= 7)
                    peekElemOff dblPtr 0 >>= (@?= 3.5)

-- | Copy a scalar duckdb_value into a vector without materializing a chunk.
vectorReferenceValue :: TestTree
vectorReferenceValue =
  testCase "vector_reference_value writes scalar contents" $ do
    withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intType ->
      withVector intType 1 \vec ->
        withValue (c_duckdb_create_int32 123) \value -> do
          c_duckdb_vector_reference_value vec value
          raw <- c_duckdb_vector_get_data vec
          let dataPtr = castPtr raw :: Ptr Int32
          peekElemOff dataPtr 0 >>= (@?= 123)

-- helpers -------------------------------------------------------------------

withVector :: DuckDBLogicalType -> DuckDBIdx -> (DuckDBVector -> IO a) -> IO a
withVector lt capacity action = bracket (c_duckdb_create_vector lt capacity) destroy action
  where
    destroy vec = alloca \ptr -> poke ptr vec >> c_duckdb_destroy_vector ptr

withLogicalType :: IO DuckDBLogicalType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType acquire action = bracket acquire destroyLogicalType action

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType lt = alloca \ptr -> poke ptr lt >> c_duckdb_destroy_logical_type ptr

withSelectionVector :: DuckDBIdx -> (DuckDBSelectionVector -> IO a) -> IO a
withSelectionVector n action = bracket (c_duckdb_create_selection_vector n) c_duckdb_destroy_selection_vector action

withValue :: IO DuckDBValue -> (DuckDBValue -> IO a) -> IO a
withValue acquire action = bracket acquire destroyValue action
  where
    destroyValue value = alloca \ptr -> poke ptr value >> c_duckdb_destroy_value ptr

plusElem :: Ptr Int32 -> Int -> Ptr Int32
plusElem base idx = base `plusPtr` (idx * sizeOf (undefined :: Int32))

setAllValid :: Ptr Word64 -> Int -> IO ()
setAllValid mask count =
  let totalWords = max 1 ((count + 63) `div` 64)
   in forM_ [0 .. totalWords - 1] \wordIdx -> do
        let start = wordIdx * 64
            end = min count (start + 64)
            bits = foldl setBit 0 [0 .. end - start - 1]
        poke (mask `plusWord` wordIdx) bits

clearValidityBit :: Ptr Word64 -> Int -> IO ()
clearValidityBit mask idx = do
  let wordIdx = idx `div` 64
      bitIdx = idx `mod` 64
      entryPtr = mask `plusWord` wordIdx
  current <- peek entryPtr
  poke entryPtr (clearBit current bitIdx)

plusWord :: Ptr Word64 -> Int -> Ptr Word64
plusWord base idx = base `plusPtr` (idx * sizeOf (undefined :: Word64))
