{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : Database.DuckDB.Simple.LogicalRep
Description : Structured logical-type and value representations for DuckDB.
-}
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
    logicalTypeFromRepWith,
    destroyLogicalType,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, when)
import Data.Array (Array, elems, listArray)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word16, Word64, Word8)
import Database.DuckDB.FFI
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (poke)

-- | A Haskell description of a DuckDB logical type tree.
data LogicalTypeRep
    = LogicalTypeScalar DUCKDB_TYPE
    | LogicalTypeDecimal !Word8 !Word8
    | LogicalTypeGeometry !(Maybe Text)
    | LogicalTypeList LogicalTypeRep
    | LogicalTypeArray LogicalTypeRep !Word64
    | LogicalTypeMap LogicalTypeRep LogicalTypeRep
    | LogicalTypeStruct !(Array Int (StructField LogicalTypeRep))
    | LogicalTypeUnion !(Array Int UnionMemberType)
    | LogicalTypeEnum !(Array Int Text)
    deriving (Eq, Show)

-- | A named field within a STRUCT-like value or type.
data StructField a = StructField
    { structFieldName :: !Text
    , structFieldValue :: !a
    }
    deriving (Eq, Show)

-- | A fully materialized STRUCT value together with its type metadata.
data StructValue a = StructValue
    { structValueFields :: !(Array Int (StructField a))
    , structValueTypes :: !(Array Int (StructField LogicalTypeRep))
    , structValueIndex :: !(Map Text Int)
    }
    deriving (Eq, Show)

-- | A named member within a UNION type.
data UnionMemberType = UnionMemberType
    { unionMemberName :: !Text
    , unionMemberType :: !LogicalTypeRep
    }
    deriving (Eq, Show)

-- | A fully materialized UNION value together with its member metadata.
data UnionValue a = UnionValue
    { unionValueIndex :: !Word16
    , unionValueLabel :: !Text
    , unionValuePayload :: !a
    , unionValueMembers :: !(Array Int UnionMemberType)
    }
    deriving (Eq, Show)

-- | Recover the logical STRUCT type corresponding to a @StructValue@.
structValueTypeRep :: StructValue a -> LogicalTypeRep
structValueTypeRep StructValue{structValueTypes} = LogicalTypeStruct structValueTypes

-- | Recover the logical UNION type corresponding to a @UnionValue@.
unionValueTypeRep :: UnionValue a -> LogicalTypeRep
unionValueTypeRep UnionValue{unionValueMembers} = LogicalTypeUnion unionValueMembers

-- | Destroy a logical type handle obtained from DuckDB.
destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType logical =
    alloca \ptr -> do
        poke ptr logical
        c_duckdb_destroy_logical_type ptr

-- | Convert a DuckDB logical type handle into the pure @LogicalTypeRep@ tree.
logicalTypeToRep :: DuckDBLogicalType -> IO LogicalTypeRep
logicalTypeToRep logical = do
    DuckDBType dtype <- c_duckdb_get_type_id logical
    case dtype of
        DuckDBTypeGeometry ->
            bracket (c_duckdb_geometry_type_get_crs logical) (c_duckdb_free . castPtr) \ptr ->
                LogicalTypeGeometry <$> if ptr == nullPtr then pure Nothing else Just . TextEncoding.decodeUtf8 <$> BS.packCString ptr
        DuckDBTypeStruct -> do
            childCountRaw <- c_duckdb_struct_type_child_count logical
            childCount <- word64ToInt (Text.pack "struct child count") (fromIntegral childCountRaw)
            fields <-
                forM [0 .. childCount - 1] \idx -> do
                    name <- bracket (c_duckdb_struct_type_child_name logical (fromIntegral idx)) (c_duckdb_free . castPtr) $ \ptr -> do
                        when (ptr == nullPtr) $
                            throwIO (userError "duckdb-simple: struct child name is null")
                        TextEncoding.decodeUtf8 <$> BS.packCString ptr
                    childRep <-
                        bracket (c_duckdb_struct_type_child_type logical (fromIntegral idx)) destroyLogicalType logicalTypeToRep
                    pure StructField{structFieldName = name, structFieldValue = childRep}
            pure $
                LogicalTypeStruct
                    (listArray (0, childCount - 1) fields)
        DuckDBTypeUnion -> do
            memberCountRaw <- c_duckdb_union_type_member_count logical
            memberCount <- word64ToInt (Text.pack "union member count") (fromIntegral memberCountRaw)
            members <-
                forM [0 .. memberCount - 1] \idx -> do
                    name <- bracket (c_duckdb_union_type_member_name logical (fromIntegral idx)) (c_duckdb_free . castPtr) $ \ptr -> do
                        when (ptr == nullPtr) $
                            throwIO (userError "duckdb-simple: union member name is null")
                        TextEncoding.decodeUtf8 <$> BS.packCString ptr
                    memberRep <-
                        bracket (c_duckdb_union_type_member_type logical (fromIntegral idx)) destroyLogicalType logicalTypeToRep
                    pure UnionMemberType{unionMemberName = name, unionMemberType = memberRep}
            pure $
                LogicalTypeUnion
                    (listArray (0, memberCount - 1) members)
        DuckDBTypeList -> do
            childRep <- bracket (c_duckdb_list_type_child_type logical) destroyLogicalType logicalTypeToRep
            pure (LogicalTypeList childRep)
        DuckDBTypeArray -> do
            childRep <- bracket (c_duckdb_array_type_child_type logical) destroyLogicalType logicalTypeToRep
            size <- c_duckdb_array_type_array_size logical
            pure (LogicalTypeArray childRep (fromIntegral size))
        DuckDBTypeMap -> do
            keyRep <- bracket (c_duckdb_map_type_key_type logical) destroyLogicalType logicalTypeToRep
            valueRep <- bracket (c_duckdb_map_type_value_type logical) destroyLogicalType logicalTypeToRep
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
                    entry <- bracket (c_duckdb_enum_dictionary_value logical (fromIntegral idx)) (c_duckdb_free . castPtr) $ \ptr -> do
                        when (ptr == nullPtr) $
                            throwIO (userError "duckdb-simple: enum dictionary value is null")
                        TextEncoding.decodeUtf8 <$> BS.packCString ptr
                    pure entry
            pure $
                LogicalTypeEnum
                    (listArray (0, count - 1) entries)
        _ ->
            pure (LogicalTypeScalar dtype)

{- | Materialize a DuckDB logical type handle from a @LogicalTypeRep@ tree.
The C API cannot create a usable VARIANT type or a GEOMETRY type with a CRS.
This function raises an error for VARIANT. It creates GEOMETRY without a CRS
and ignores the CRS of 'LogicalTypeGeometry'. Parameter binding uses the
types that a connection reads the first time a parameter needs one.
-}
logicalTypeFromRep :: LogicalTypeRep -> IO DuckDBLogicalType
logicalTypeFromRep = logicalTypeFromRepWith \case
    -- TODO: improve this when this becomes available in the C API.
    -- See https://github.com/Tritlo/duckdb-haskell/issues/26 and
    -- https://github.com/Tritlo/duckdb-haskell/issues/27.
    LogicalTypeScalar DuckDBTypeVariant ->
        throwIO (userError "duckdb-simple: a VARIANT type needs the type cache of a connection; bind the value as a parameter or cast a plain value with ?::VARIANT")
    _ -> c_duckdb_create_logical_type (DuckDBType DuckDBTypeGeometry)

{- | Materialize a type tree. The function argument creates the leaves that the
C API cannot create: VARIANT, and GEOMETRY with a CRS. The caller must
destroy the result.
-}
logicalTypeFromRepWith :: (LogicalTypeRep -> IO DuckDBLogicalType) -> LogicalTypeRep -> IO DuckDBLogicalType
logicalTypeFromRepWith resolve rep = do
    logical <- create rep
    when (logical == DuckDBLogicalType nullPtr) $
        throwIO (userError "duckdb-simple: DuckDB logical type construction failed")
    pure logical
  where
    create = \case
        leaf@(LogicalTypeScalar DuckDBTypeVariant) -> resolve leaf
        LogicalTypeScalar dtype -> c_duckdb_create_logical_type (DuckDBType dtype)
        leaf@(LogicalTypeGeometry (Just _)) -> resolve leaf
        LogicalTypeGeometry Nothing -> c_duckdb_create_logical_type (DuckDBType DuckDBTypeGeometry)
        LogicalTypeDecimal width scale -> do
            when (width < 1 || width > 38 || scale > width) $
                throwIO (userError "duckdb-simple: invalid DECIMAL width or scale")
            c_duckdb_create_decimal_type width scale
        LogicalTypeList elemRep ->
            bracket (logicalTypeFromRepWith resolve elemRep) destroyLogicalType c_duckdb_create_list_type
        LogicalTypeArray elemRep size ->
            bracket (logicalTypeFromRepWith resolve elemRep) destroyLogicalType $
                flip c_duckdb_create_array_type (fromIntegral size)
        LogicalTypeMap keyRep valueRep ->
            bracket (logicalTypeFromRepWith resolve keyRep) destroyLogicalType \keyType ->
                bracket (logicalTypeFromRepWith resolve valueRep) destroyLogicalType $
                    c_duckdb_create_map_type keyType
        LogicalTypeStruct fieldArray -> do
            let fields = elems fieldArray
            withMany (\field -> bracket (logicalTypeFromRepWith resolve (structFieldValue field)) destroyLogicalType) fields \childTypes ->
                withMany withTypeName (map structFieldName fields) \names ->
                    withArray names \nameArray ->
                        withArray childTypes \typeArray ->
                            c_duckdb_create_struct_type typeArray nameArray (fromIntegral (length fields))
        LogicalTypeUnion memberArray -> do
            let members = elems memberArray
            withMany (\member -> bracket (logicalTypeFromRepWith resolve (unionMemberType member)) destroyLogicalType) members \memberTypes ->
                withMany withTypeName (map unionMemberName members) \names ->
                    withArray names \nameArray ->
                        withArray memberTypes \typeArray ->
                            c_duckdb_create_union_type typeArray nameArray (fromIntegral (length members))
        LogicalTypeEnum dictArray ->
            withMany withTypeName (elems dictArray) \names ->
                withArray names \nameArray ->
                    c_duckdb_create_enum_type nameArray (fromIntegral (length names))

-- | Encode native type names as UTF-8 and reject embedded NUL.
withTypeName :: Text -> (ConstPtr CChar -> IO a) -> IO a
withTypeName name action = do
    when (Text.any (== '\0') name) $
        throwIO (userError "duckdb-simple: logical type name contains NUL")
    BS.useAsCString (TextEncoding.encodeUtf8 name) (action . ConstPtr)

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
