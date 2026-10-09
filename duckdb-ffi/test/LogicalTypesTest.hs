{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module LogicalTypesTest (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_, when, (>=>))
import Data.Coerce (coerce)
import Data.List (stripPrefix)
import Data.Void (Void)
import Data.Word (Word32, Word8)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Records (getField)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Text.Read (readMaybe)
import Utils (withConnection, withConstCString, withDatabase, withLogicalType, withResult)

tests :: TestTree
tests =
    testGroup
        "Logical Type Interface"
        [ primitiveLogicalTypes
        , decimalLogicalType
        , invalidDecimalLogicalType
        , variantLogicalType
        , enumLogicalType
        , compositeLogicalTypes
        , aliasRoundtrip
        , registerLogicalType
        , geometryTypeCrs
        , geometryTypeCrsOwnership
        ]

primitiveLogicalTypes :: TestTree
primitiveLogicalTypes =
    testCase "primitive logical types report their ids" $
        forM_ primitives \(duckType, expected) ->
            withLogicalType (c_duckdb_create_logical_type (DuckDBType duckType)) \lt -> do
                typeId <- (fmap (getField @"unwrap") . c_duckdb_get_type_id) lt
                typeId @?= expected
  where
    primitives =
        [ (DuckDBTypeBoolean, DuckDBTypeBoolean)
        , (DuckDBTypeInteger, DuckDBTypeInteger)
        , (DuckDBTypeVarchar, DuckDBTypeVarchar)
        , (DuckDBTypeBlob, DuckDBTypeBlob)
        ]

-- | VARIANT needs the complete descriptor from an executed query.
variantLogicalType :: TestTree
variantLogicalType =
    testCase "query-derived VARIANT exposes its physical children" $
        withDatabase \db ->
            withConnection db \conn ->
                withResult conn "SELECT NULL::VARIANT" \result ->
                    withLogicalType (c_duckdb_column_logical_type result 0) \logical -> do
                        (fmap (getField @"unwrap") . c_duckdb_get_type_id) logical >>= (@?= DuckDBTypeVariant)
                        c_duckdb_struct_type_child_count logical >>= (@?= 4)

-- | Invalid decimal metadata became safe to pass to the C API in 1.5.4.
invalidDecimalLogicalType :: TestTree
invalidDecimalLogicalType =
    testCase "invalid decimal types return NULL on DuckDB 1.5.4 and later" $ do
        version <- c_duckdb_library_version >>= (peekCString . coerce)
        case stripPrefix "v1.5." version >>= readMaybe of
            Just patch | patch >= (4 :: Int) ->
                forM_ [(0, 0), (39, 0), (2, 3), (255, 0)] \(width, scale) ->
                    withLogicalType (c_duckdb_create_decimal_type width scale) (@?= (coerce (nullPtr :: Ptr Void)))
            _ -> pure ()

decimalLogicalType :: TestTree
decimalLogicalType =
    testCase "decimal logical type exposes width/scale/internal type" $
        withLogicalType (c_duckdb_create_decimal_type width scale) \lt -> do
            (fmap (getField @"unwrap") . c_duckdb_get_type_id) lt >>= (@?= DuckDBTypeDecimal)
            c_duckdb_decimal_width lt >>= (@?= width)
            c_duckdb_decimal_scale lt >>= (@?= scale)
            (fmap (getField @"unwrap") . c_duckdb_decimal_internal_type) lt >>= (@?= DuckDBTypeBigInt)
  where
    width, scale :: Word8
    width = 18
    scale = 4

enumLogicalType :: TestTree
enumLogicalType =
    testCase "enum logical type exposes dictionary" $ do
        enumType <-
            withMany withConstCString ["Small", "Medium", "Large"] \namePtrs ->
                withArray namePtrs (`c_duckdb_create_enum_type` 3)
        withLogicalType (pure enumType) \lt -> do
            (fmap (getField @"unwrap") . c_duckdb_get_type_id) lt >>= (@?= DuckDBTypeEnum)
            (fmap (getField @"unwrap") . c_duckdb_enum_internal_type) lt >>= (@?= DuckDBTypeUTinyInt)
            c_duckdb_enum_dictionary_size lt >>= (@?= (3 :: Word32))
            valuePtr <- c_duckdb_enum_dictionary_value lt 1
            (peekCString . coerce) valuePtr >>= (@?= "Medium")
            c_duckdb_free (coerce valuePtr)

compositeLogicalTypes :: TestTree
compositeLogicalTypes =
    testCase "composite logical types expose nesting metadata" $ do
        -- List type
        withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeInteger)) \child -> do
            listType <- c_duckdb_create_list_type child
            withLogicalType (pure listType) \lt -> do
                (fmap (getField @"unwrap") . c_duckdb_get_type_id) lt >>= (@?= DuckDBTypeList)
                listChild <- c_duckdb_list_type_child_type lt
                withLogicalType (pure listChild) ((fmap (getField @"unwrap") . c_duckdb_get_type_id) >=> (@?= DuckDBTypeInteger))

        -- Array type
        withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeInteger)) \arrayChild -> do
            arrayType <- c_duckdb_create_array_type arrayChild 5
            withLogicalType (pure arrayType) \lt -> do
                (fmap (getField @"unwrap") . c_duckdb_get_type_id) lt >>= (@?= DuckDBTypeArray)
                arrayChildType <- c_duckdb_array_type_child_type lt
                withLogicalType (pure arrayChildType) ((fmap (getField @"unwrap") . c_duckdb_get_type_id) >=> (@?= DuckDBTypeInteger))
                c_duckdb_array_type_array_size lt >>= (@?= 5)

        -- Map type
        withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeVarchar)) \keyType ->
            withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeInteger)) \valType -> do
                mapType <- c_duckdb_create_map_type keyType valType
                withLogicalType (pure mapType) \lt -> do
                    (fmap (getField @"unwrap") . c_duckdb_get_type_id) lt >>= (@?= DuckDBTypeMap)
                    mapKey <- c_duckdb_map_type_key_type lt
                    withLogicalType (pure mapKey) ((fmap (getField @"unwrap") . c_duckdb_get_type_id) >=> (@?= DuckDBTypeVarchar))
                    mapVal <- c_duckdb_map_type_value_type lt
                    withLogicalType (pure mapVal) ((fmap (getField @"unwrap") . c_duckdb_get_type_id) >=> (@?= DuckDBTypeInteger))

        -- Struct type
        withMany withConstCString ["id", "name"] \fieldNames -> withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeInteger)) \idType ->
            withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeVarchar)) \nameType ->
                withArray [idType, nameType] \childArray ->
                    withArray fieldNames \namesArray -> do
                        structType <- c_duckdb_create_struct_type childArray namesArray 2
                        withLogicalType (pure structType) \lt -> do
                            (fmap (getField @"unwrap") . c_duckdb_get_type_id) lt >>= (@?= DuckDBTypeStruct)
                            childCount <- c_duckdb_struct_type_child_count lt
                            childCount @?= 2
                            childName0 <- c_duckdb_struct_type_child_name lt 0
                            (peekCString . coerce) childName0 >>= (@?= "id")
                            c_duckdb_free (coerce childName0)
                            structChild1 <- c_duckdb_struct_type_child_type lt 1
                            withLogicalType (pure structChild1) ((fmap (getField @"unwrap") . c_duckdb_get_type_id) >=> (@?= DuckDBTypeVarchar))

        -- Union type
        withMany withConstCString ["int_member", "text_member"] \memberNames -> withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeInteger)) \intMember ->
            withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeVarchar)) \textMember ->
                withArray [intMember, textMember] \memberArray ->
                    withArray memberNames \nameArray -> do
                        unionType <- c_duckdb_create_union_type memberArray nameArray 2
                        withLogicalType (pure unionType) \lt -> do
                            (fmap (getField @"unwrap") . c_duckdb_get_type_id) lt >>= (@?= DuckDBTypeUnion)
                            memberCount <- c_duckdb_union_type_member_count lt
                            memberCount @?= 2
                            memberName0 <- c_duckdb_union_type_member_name lt 0
                            (peekCString . coerce) memberName0 >>= (@?= "int_member")
                            c_duckdb_free (coerce memberName0)
                            memberChild <- c_duckdb_union_type_member_type lt 1
                            withLogicalType (pure memberChild) ((fmap (getField @"unwrap") . c_duckdb_get_type_id) >=> (@?= DuckDBTypeVarchar))

aliasRoundtrip :: TestTree
aliasRoundtrip =
    testCase "logical type aliases can be set and retrieved" $
        withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeInteger)) \lt -> do
            aliasBefore <- c_duckdb_logical_type_get_alias lt
            aliasBefore @?= (coerce (nullPtr :: Ptr Void))
            withConstCString "custom_alias" $ \alias -> c_duckdb_logical_type_set_alias lt alias
            aliasAfter <- c_duckdb_logical_type_get_alias lt
            assertBool "alias pointer should not be null" (aliasAfter /= (coerce (nullPtr :: Ptr Void)))
            when (aliasAfter == (coerce (nullPtr :: Ptr Void))) $
                assertFailure "expected alias pointer"
            (peekCString . coerce) aliasAfter >>= (@?= "custom_alias")
            c_duckdb_free (coerce aliasAfter)

registerLogicalType :: TestTree
registerLogicalType =
    testCase "registered logical type alias is accepted in SQL" $
        withDatabase \db ->
            withConnection db \conn ->
                withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeInteger)) \lt -> do
                    let aliasName = "custom_int_alias"
                    withConstCString aliasName $ \aliasPtr ->
                        c_duckdb_logical_type_set_alias lt aliasPtr
                    c_duckdb_register_logical_type conn lt (coerce (nullPtr :: Ptr Void)) >>= (@?= DuckDBSuccess)

                    let createSql = "CREATE TABLE logical_type_demo (val " ++ aliasName ++ ")"
                    withResult conn createSql \_ -> pure ()

                    withResult conn "PRAGMA table_info('logical_type_demo')" \resPtr -> do
                        c_duckdb_row_count resPtr >>= (@?= 1)
                        typePtr <- c_duckdb_value_varchar resPtr 2 0
                        typeName <- (peekCString . coerce) typePtr
                        c_duckdb_free (coerce typePtr)
                        typeName @?= aliasName

geometryTypeCrs :: TestTree
geometryTypeCrs =
    testCase "geometry CRS is null without a coordinate reference system" $
        withDatabase \db ->
            withConnection db \conn -> do
                -- A GEOMETRY column reports the GEOMETRY type id, and carries no
                -- CRS unless the data supplies one.
                withResult conn "SELECT 'POINT(1 2)'::GEOMETRY" \resPtr -> do
                    columnType <- c_duckdb_column_logical_type resPtr 0
                    withLogicalType (pure columnType) \lt -> do
                        (fmap (getField @"unwrap") . c_duckdb_get_type_id) lt >>= (@?= DuckDBTypeGeometry)
                        crs <- c_duckdb_geometry_type_get_crs lt
                        assertBool "plain GEOMETRY has no CRS" (crs == (coerce (nullPtr :: Ptr Void)))

                -- Types other than GEOMETRY never have a CRS.
                withLogicalType (c_duckdb_create_logical_type (DuckDBType DuckDBTypeInteger)) \lt -> do
                    crs <- c_duckdb_geometry_type_get_crs lt
                    assertBool "INTEGER has no CRS" (crs == (coerce (nullPtr :: Ptr Void)))

-- | The caller owns the returned CRS independently of the type and result.
geometryTypeCrsOwnership :: TestTree
geometryTypeCrsOwnership =
    testCase "geometry CRS outlives its type and result" $
        bracket
            ( withDatabase \db ->
                withConnection db \conn ->
                    withResult conn "SELECT ST_SetCRS('POINT(1 2)'::GEOMETRY, 'duckdb-haskell-test-crs')" \result ->
                        withLogicalType (c_duckdb_column_logical_type result 0) c_duckdb_geometry_type_get_crs
            )
            (c_duckdb_free . coerce)
            \crs -> do
                assertBool "expected an owned CRS string" (crs /= (coerce (nullPtr :: Ptr Void)))
                (peekCString . coerce) crs >>= (@?= "duckdb-haskell-test-crs")
