{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database.DuckDB.Simple.LogicalRep (
    -- * Structured value helpers
    StructField (..),
    StructValue (..),
    UnionMemberType (..),
    UnionValue (..),
    LogicalTypeRep (..),
    structValueTypeRep,
    unionValueTypeRep,
    logicalTypeToRep,
    logicalTypeFromRep,
    destroyLogicalType,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, when)
import Data.Array (Array, elems, listArray)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word64, Word8)
import Database.DuckDB.FFI
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (poke)

data LogicalTypeRep
    = LogicalTypeScalar DuckDBType
    | LogicalTypeDecimal !Word8 !Word8
    | LogicalTypeList LogicalTypeRep
    | LogicalTypeArray LogicalTypeRep !Word64
    | LogicalTypeMap LogicalTypeRep LogicalTypeRep
    | LogicalTypeStruct !(Array Int (StructField LogicalTypeRep))
    | LogicalTypeUnion !(Array Int UnionMemberType)
    | LogicalTypeEnum !(Array Int Text)
    deriving (Eq, Show)

data StructField a = StructField
    { structFieldName :: !Text
    , structFieldValue :: !a
    }
    deriving (Eq, Show)

data StructValue a = StructValue
    { structValueFields :: !(Array Int (StructField a))
    , structValueTypes :: !(Array Int (StructField LogicalTypeRep))
    , structValueIndex :: !(Map Text Int)
    }
    deriving (Eq, Show)

data UnionMemberType = UnionMemberType
    { unionMemberName :: !Text
    , unionMemberType :: !LogicalTypeRep
    }
    deriving (Eq, Show)

data UnionValue a = UnionValue
    { unionValueIndex :: !Word16
    , unionValueLabel :: !Text
    , unionValuePayload :: !a
    , unionValueMembers :: !(Array Int UnionMemberType)
    }
    deriving (Eq, Show)

structValueTypeRep :: StructValue a -> LogicalTypeRep
structValueTypeRep StructValue{structValueTypes} = LogicalTypeStruct structValueTypes

unionValueTypeRep :: UnionValue a -> LogicalTypeRep
unionValueTypeRep UnionValue{unionValueMembers} = LogicalTypeUnion unionValueMembers

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType logical =
    alloca \ptr -> do
        poke ptr logical
        c_duckdb_destroy_logical_type ptr

logicalTypeToRep :: DuckDBLogicalType -> IO LogicalTypeRep
logicalTypeToRep logical = do
    dtype <- c_duckdb_get_type_id logical
    case dtype of
        DuckDBTypeStruct -> do
            childCountRaw <- c_duckdb_struct_type_child_count logical
            childCount <- word64ToInt (Text.pack "struct child count") childCountRaw
            fields <-
                forM [0 .. childCount - 1] \idx -> do
                    namePtr <- c_duckdb_struct_type_child_name logical (fromIntegral idx)
                    when (namePtr == nullPtr) $
                        throwIO (userError "duckdb-simple: struct child name is null")
                    name <- Text.pack <$> peekCString namePtr
                    c_duckdb_free (castPtr namePtr)
                    childLogical <- c_duckdb_struct_type_child_type logical (fromIntegral idx)
                    childRep <-
                        bracket (pure childLogical) destroyLogicalType logicalTypeToRep
                    pure StructField{structFieldName = name, structFieldValue = childRep}
            pure $
                LogicalTypeStruct
                    ( if childCount <= 0
                        then listArray (0, -1) []
                        else listArray (0, childCount - 1) fields
                    )
        DuckDBTypeUnion -> do
            memberCountRaw <- c_duckdb_union_type_member_count logical
            memberCount <- word64ToInt (Text.pack "union member count") memberCountRaw
            members <-
                forM [0 .. memberCount - 1] \idx -> do
                    namePtr <- c_duckdb_union_type_member_name logical (fromIntegral idx)
                    when (namePtr == nullPtr) $
                        throwIO (userError "duckdb-simple: union member name is null")
                    name <- Text.pack <$> peekCString namePtr
                    c_duckdb_free (castPtr namePtr)
                    memberLogical <- c_duckdb_union_type_member_type logical (fromIntegral idx)
                    memberRep <-
                        bracket (pure memberLogical) destroyLogicalType logicalTypeToRep
                    pure UnionMemberType{unionMemberName = name, unionMemberType = memberRep}
            pure $
                LogicalTypeUnion
                    ( if memberCount <= 0
                        then listArray (0, -1) []
                        else listArray (0, memberCount - 1) members
                    )
        DuckDBTypeList -> do
            childLogical <- c_duckdb_list_type_child_type logical
            childRep <- bracket (pure childLogical) destroyLogicalType logicalTypeToRep
            pure (LogicalTypeList childRep)
        DuckDBTypeArray -> do
            childLogical <- c_duckdb_array_type_child_type logical
            childRep <- bracket (pure childLogical) destroyLogicalType logicalTypeToRep
            size <- c_duckdb_array_type_array_size logical
            pure (LogicalTypeArray childRep size)
        DuckDBTypeMap -> do
            keyLogical <- c_duckdb_map_type_key_type logical
            valueLogical <- c_duckdb_map_type_value_type logical
            keyRep <- bracket (pure keyLogical) destroyLogicalType logicalTypeToRep
            valueRep <- bracket (pure valueLogical) destroyLogicalType logicalTypeToRep
            pure (LogicalTypeMap keyRep valueRep)
        DuckDBTypeDecimal -> do
            width <- c_duckdb_decimal_width logical
            scale <- c_duckdb_decimal_scale logical
            pure (LogicalTypeDecimal width scale)
        DuckDBTypeEnum -> do
            dictSize <- c_duckdb_enum_dictionary_size logical
            let count = fromIntegral dictSize :: Int
            entries <-
                forM [0 .. count - 1] \idx -> do
                    entryPtr <- c_duckdb_enum_dictionary_value logical (fromIntegral idx)
                    when (entryPtr == nullPtr) $
                        throwIO (userError "duckdb-simple: enum dictionary value is null")
                    entry <- Text.pack <$> peekCString entryPtr
                    c_duckdb_free (castPtr entryPtr)
                    pure entry
            pure $
                LogicalTypeEnum
                    ( if count <= 0
                        then listArray (0, -1) []
                        else listArray (0, count - 1) entries
                    )
        _ ->
            pure (LogicalTypeScalar dtype)

logicalTypeFromRep :: LogicalTypeRep -> IO DuckDBLogicalType
logicalTypeFromRep = \case
    LogicalTypeScalar dtype ->
        c_duckdb_create_logical_type dtype
    LogicalTypeDecimal width scale ->
        c_duckdb_create_decimal_type width scale
    LogicalTypeList elemRep ->
        bracket (logicalTypeFromRep elemRep) destroyLogicalType c_duckdb_create_list_type
    LogicalTypeArray elemRep size ->
        bracket (logicalTypeFromRep elemRep) destroyLogicalType \child ->
            c_duckdb_create_array_type child size
    LogicalTypeMap keyRep valueRep ->
        bracket (logicalTypeFromRep keyRep) destroyLogicalType \keyType ->
            bracket (logicalTypeFromRep valueRep) destroyLogicalType \valueType ->
                c_duckdb_create_map_type keyType valueType
    LogicalTypeStruct fieldArray -> do
        let fields = elems fieldArray
            count = length fields
        if count == 0
            then c_duckdb_create_struct_type nullPtr nullPtr 0
            else do
                childTypes <- mapM (logicalTypeFromRep . structFieldValue) fields
                let names = map (Text.unpack . structFieldName) fields
                result <-
                    withMany withCString names \namePtrs ->
                        withArray namePtrs \nameArray ->
                            withArray childTypes \typeArray ->
                                c_duckdb_create_struct_type typeArray nameArray (fromIntegral count)
                mapM_ destroyLogicalType childTypes
                pure result
    LogicalTypeUnion memberArray -> do
        let members = elems memberArray
            count = length members
        if count == 0
            then c_duckdb_create_union_type nullPtr nullPtr 0
            else do
                memberTypes <- mapM (logicalTypeFromRep . unionMemberType) members
                let names = map (Text.unpack . unionMemberName) members
                result <-
                    withMany withCString names \namePtrs ->
                        withArray namePtrs \nameArray ->
                            withArray memberTypes \memberPtr ->
                                c_duckdb_create_union_type memberPtr nameArray (fromIntegral count)
                mapM_ destroyLogicalType memberTypes
                pure result
    LogicalTypeEnum dictArray -> do
        let entries = elems dictArray
            count = length entries
        if count == 0
            then c_duckdb_create_enum_type nullPtr 0
            else
                withMany withCString (map Text.unpack entries) \namePtrs ->
                    withArray namePtrs \nameArray ->
                        c_duckdb_create_enum_type nameArray (fromIntegral count)

word64ToInt :: Text -> Word64 -> IO Int
word64ToInt label value =
    let actual = toInteger value
        limit = toInteger (maxBound :: Int)
     in if actual <= limit
            then pure (fromIntegral value)
            else
                throwIO
                    ( userError
                        ( "duckdb-simple: "
                            <> Text.unpack label
                            <> " exceeds Int range"
                        )
                    )
