{-# LANGUAGE BlockArguments #-}


module LogicalTypesTest (tests) where

import Control.Monad (forM_, when, (>=>))
import Data.Word (Word32, Word8)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (castPtr, nullPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Utils (withConnection, withDatabase, withLogicalType, withResult)

tests :: TestTree
tests =
    testGroup
        "Logical Type Interface"
        [ primitiveLogicalTypes
        , decimalLogicalType
        , enumLogicalType
        , compositeLogicalTypes
        , aliasRoundtrip
        , registerLogicalType
        ]

primitiveLogicalTypes :: TestTree
primitiveLogicalTypes =
    testCase "primitive logical types report their ids" $
        forM_ primitives \(duckType, expected) ->
            withLogicalType (c_duckdb_create_logical_type duckType) \lt -> do
                typeId <- c_duckdb_get_type_id lt
                typeId @?= expected
  where
    primitives =
        [ (DuckDBTypeBoolean, DuckDBTypeBoolean)
        , (DuckDBTypeInteger, DuckDBTypeInteger)
        , (DuckDBTypeVarchar, DuckDBTypeVarchar)
        , (DuckDBTypeBlob, DuckDBTypeBlob)
        ]

decimalLogicalType :: TestTree
decimalLogicalType =
    testCase "decimal logical type exposes width/scale/internal type" $
        withLogicalType (c_duckdb_create_decimal_type width scale) \lt -> do
            c_duckdb_get_type_id lt >>= (@?= DuckDBTypeDecimal)
            c_duckdb_decimal_width lt >>= (@?= width)
            c_duckdb_decimal_scale lt >>= (@?= scale)
            c_duckdb_decimal_internal_type lt >>= (@?= DuckDBTypeBigInt)
  where
    width, scale :: Word8
    width = 18
    scale = 4

enumLogicalType :: TestTree
enumLogicalType =
    testCase "enum logical type exposes dictionary" $ do
        enumType <-
            withMany withCString ["Small", "Medium", "Large"] \namePtrs ->
                withArray namePtrs (`c_duckdb_create_enum_type` 3)
        withLogicalType (pure enumType) \lt -> do
            c_duckdb_get_type_id lt >>= (@?= DuckDBTypeEnum)
            c_duckdb_enum_internal_type lt >>= (@?= DuckDBTypeUTinyInt)
            c_duckdb_enum_dictionary_size lt >>= (@?= (3 :: Word32))
            valuePtr <- c_duckdb_enum_dictionary_value lt 1
            peekCString valuePtr >>= (@?= "Medium")
            c_duckdb_free (castPtr valuePtr)

compositeLogicalTypes :: TestTree
compositeLogicalTypes =
    testCase "composite logical types expose nesting metadata" $ do
        -- List type
        withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \child -> do
            listType <- c_duckdb_create_list_type child
            withLogicalType (pure listType) \lt -> do
                c_duckdb_get_type_id lt >>= (@?= DuckDBTypeList)
                listChild <- c_duckdb_list_type_child_type lt
                withLogicalType (pure listChild) (c_duckdb_get_type_id >=> (@?= DuckDBTypeInteger))

        -- Array type
        withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \arrayChild -> do
            arrayType <- c_duckdb_create_array_type arrayChild 5
            withLogicalType (pure arrayType) \lt -> do
                c_duckdb_get_type_id lt >>= (@?= DuckDBTypeArray)
                arrayChildType <- c_duckdb_array_type_child_type lt
                withLogicalType (pure arrayChildType) (c_duckdb_get_type_id >=> (@?= DuckDBTypeInteger))
                c_duckdb_array_type_array_size lt >>= (@?= 5)

        -- Map type
        withLogicalType (c_duckdb_create_logical_type DuckDBTypeVarchar) \keyType ->
            withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \valType -> do
                mapType <- c_duckdb_create_map_type keyType valType
                withLogicalType (pure mapType) \lt -> do
                    c_duckdb_get_type_id lt >>= (@?= DuckDBTypeMap)
                    mapKey <- c_duckdb_map_type_key_type lt
                    withLogicalType (pure mapKey) (c_duckdb_get_type_id >=> (@?= DuckDBTypeVarchar))
                    mapVal <- c_duckdb_map_type_value_type lt
                    withLogicalType (pure mapVal) (c_duckdb_get_type_id >=> (@?= DuckDBTypeInteger))

        -- Struct type
        withMany withCString ["id", "name"] \fieldNames -> withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \idType ->
            withLogicalType (c_duckdb_create_logical_type DuckDBTypeVarchar) \nameType ->
                withArray [idType, nameType] \childArray ->
                    withArray fieldNames \namesArray -> do
                        structType <- c_duckdb_create_struct_type childArray namesArray 2
                        withLogicalType (pure structType) \lt -> do
                            c_duckdb_get_type_id lt >>= (@?= DuckDBTypeStruct)
                            childCount <- c_duckdb_struct_type_child_count lt
                            childCount @?= 2
                            childName0 <- c_duckdb_struct_type_child_name lt 0
                            peekCString childName0 >>= (@?= "id")
                            c_duckdb_free (castPtr childName0)
                            structChild1 <- c_duckdb_struct_type_child_type lt 1
                            withLogicalType (pure structChild1) (c_duckdb_get_type_id >=> (@?= DuckDBTypeVarchar))

        -- Union type
        withMany withCString ["int_member", "text_member"] \memberNames -> withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \intMember ->
            withLogicalType (c_duckdb_create_logical_type DuckDBTypeVarchar) \textMember ->
                withArray [intMember, textMember] \memberArray ->
                    withArray memberNames \nameArray -> do
                        unionType <- c_duckdb_create_union_type memberArray nameArray 2
                        withLogicalType (pure unionType) \lt -> do
                            c_duckdb_get_type_id lt >>= (@?= DuckDBTypeUnion)
                            memberCount <- c_duckdb_union_type_member_count lt
                            memberCount @?= 2
                            memberName0 <- c_duckdb_union_type_member_name lt 0
                            peekCString memberName0 >>= (@?= "int_member")
                            c_duckdb_free (castPtr memberName0)
                            memberChild <- c_duckdb_union_type_member_type lt 1
                            withLogicalType (pure memberChild) (c_duckdb_get_type_id >=> (@?= DuckDBTypeVarchar))

aliasRoundtrip :: TestTree
aliasRoundtrip =
    testCase "logical type aliases can be set and retrieved" $
        withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \lt -> do
            aliasBefore <- c_duckdb_logical_type_get_alias lt
            aliasBefore @?= nullPtr
            withCString "custom_alias" $ \alias -> c_duckdb_logical_type_set_alias lt alias
            aliasAfter <- c_duckdb_logical_type_get_alias lt
            assertBool "alias pointer should not be null" (aliasAfter /= nullPtr)
            when (aliasAfter == nullPtr) $
                assertFailure "expected alias pointer"
            peekCString aliasAfter >>= (@?= "custom_alias")
            c_duckdb_free (castPtr aliasAfter)

registerLogicalType :: TestTree
registerLogicalType =
    testCase "registered logical type alias is accepted in SQL" $
        withDatabase \db ->
            withConnection db \conn ->
                withLogicalType (c_duckdb_create_logical_type DuckDBTypeInteger) \lt -> do
                    let aliasName = "custom_int_alias"
                    withCString aliasName $ \aliasPtr ->
                        c_duckdb_logical_type_set_alias lt aliasPtr
                    c_duckdb_register_logical_type conn lt nullPtr >>= (@?= DuckDBSuccess)

                    let createSql = "CREATE TABLE logical_type_demo (val " ++ aliasName ++ ")"
                    withResult conn createSql \_ -> pure ()

                    withResult conn "PRAGMA table_info('logical_type_demo')" \resPtr -> do
                        c_duckdb_row_count resPtr >>= (@?= 1)
                        typePtr <- c_duckdb_value_varchar resPtr 2 0
                        typeName <- peekCString typePtr
                        c_duckdb_free (castPtr typePtr)
                        typeName @?= aliasName
