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
    logicalTypeFromRepWithFactory,
    destroyLogicalType,
) where

import Control.Exception (bracket, mask_, onException, throwIO)
import Control.Monad (forM, when)
import Data.Array (Array, elems, listArray)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word64, Word8)
import Database.DuckDB.Simple.Native
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)

-- | A Haskell description of a DuckDB logical type tree.
data LogicalTypeRep
    = LogicalTypeScalar DuckDBV2LogicalTypeId
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

-- | Destroy an owned logical type handle.
destroyLogicalType :: DuckDBV2LogicalTypeHandle -> IO ()
destroyLogicalType = destroy c_duckdb_v2_logical_type_destroy

-- | Copy a native logical type into a Haskell type tree.
logicalTypeToRep :: DuckDBV2LogicalTypeHandle -> IO LogicalTypeRep
logicalTypeToRep logical = do
    dtype <- output (c_duckdb_v2_logical_type_get_id logical)
    let parameter index action = alloca \nameSlot ->
            bracket (output (c_duckdb_v2_logical_type_get_param logical index nameSlot)) (destroy c_duckdb_v2_value_destroy) \value -> do
                name <- peek nameSlot >>= readIdentifier
                action name value
        child index = parameter index \_ value ->
            bracket (output (c_duckdb_v2_value_get_type value)) destroyLogicalType logicalTypeToRep
        string index = parameter index \_ value -> output (c_duckdb_v2_value_get_varchar value) >>= readText
        count = output (c_duckdb_v2_logical_type_get_param_count logical) >>= word64ToInt
        fields make = do
            size <- count
            listArray (0, size - 1)
                <$> forM
                    [0 .. size - 1]
                    ( \index -> parameter (fromIntegral index) \name value -> do
                        rep <- bracket (output (c_duckdb_v2_value_get_type value)) destroyLogicalType logicalTypeToRep
                        pure (make name rep)
                    )
    case dtype of
        DuckDBV2LogicalTypeIdGeometry -> do
            size <- count
            LogicalTypeGeometry <$> if size == 0 then pure Nothing else Just <$> string 0
        DuckDBV2LogicalTypeIdStruct -> LogicalTypeStruct <$> fields StructField
        DuckDBV2LogicalTypeIdTuple -> LogicalTypeStruct <$> fields StructField
        DuckDBV2LogicalTypeIdUnion -> LogicalTypeUnion <$> fields UnionMemberType
        DuckDBV2LogicalTypeIdList -> LogicalTypeList <$> child 0
        DuckDBV2LogicalTypeIdArray -> do
            element <- child 0
            size <- parameter 1 \_ value -> output (c_duckdb_v2_value_get_bigint value)
            pure (LogicalTypeArray element (fromIntegral size))
        DuckDBV2LogicalTypeIdMap -> LogicalTypeMap <$> child 0 <*> child 1
        DuckDBV2LogicalTypeIdDecimal ->
            LogicalTypeDecimal
                <$> parameter 0 (\_ value -> output (c_duckdb_v2_value_get_utinyint value))
                <*> parameter 1 (\_ value -> output (c_duckdb_v2_value_get_utinyint value))
        DuckDBV2LogicalTypeIdEnum -> do
            size <- count
            LogicalTypeEnum . listArray (0, size - 1) <$> mapM (string . fromIntegral) [0 .. size - 1]
        _ -> pure (LogicalTypeScalar dtype)

{- | Construct an owned logical type in the supplied connection.
CRS labels must be recognized by the native type constructor. Use the resolver
in 'logicalTypeFromRepWith' for SQL-derived types with arbitrary CRS labels.
-}
logicalTypeFromRep :: DuckDBV2ConnectionHandle -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
logicalTypeFromRep conn = logicalTypeFromRepWith conn (const (pure Nothing))

-- | Construct a type tree. Use the resolver for owned native leaf types.
logicalTypeFromRepWith :: DuckDBV2ConnectionHandle -> (LogicalTypeRep -> IO (Maybe DuckDBV2LogicalTypeHandle)) -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
logicalTypeFromRepWith conn resolve rep =
    withConnectionFactory conn \factory -> logicalTypeFromFactory factory resolve rep

-- | Construct an owned type through a borrowed native factory.
logicalTypeFromRepWithFactory :: DuckDBV2FactoryHandle -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
logicalTypeFromRepWithFactory factory = logicalTypeFromFactory factory (const (pure Nothing))

-- | Resolve native leaf types before constructing their parent types.
logicalTypeFromFactory :: DuckDBV2FactoryHandle -> (LogicalTypeRep -> IO (Maybe DuckDBV2LogicalTypeHandle)) -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
logicalTypeFromFactory factory resolve = create
  where
    owned = destroy c_duckdb_v2_value_destroy
    boxed rep = bracket (create rep) destroyLogicalType (output . c_duckdb_v2_value_create_type factory)
    text value = withText value (output . c_duckdb_v2_value_create_varchar factory)
    fromText value = withText value (output . c_duckdb_v2_factory_create_type_from_text factory)
    construct dtype names actions = do
        when (any (Text.any (== '\0')) names) $
            throwIO (userError "duckdb-simple: logical type name contains NUL")
        withMany (\action -> bracket action owned) actions \values ->
            withArray values \valuePtr ->
                if null names
                    then output (c_duckdb_v2_factory_create_type_from_id factory dtype (ConstPtr nullPtr) (ConstPtr valuePtr) (fromIntegral (length values)))
                    else withMany (\name action -> withIdentifier name (\(ConstPtr ptr) -> peek ptr >>= action)) names \identifiers ->
                        withArray identifiers \namePtr ->
                            output (c_duckdb_v2_factory_create_type_from_id factory dtype (ConstPtr namePtr) (ConstPtr valuePtr) (fromIntegral (length values)))
    create rep = resolve rep >>= maybe (constructRep rep) pure
    constructRep = \case
        LogicalTypeScalar DuckDBV2LogicalTypeIdVariant -> fromText (Text.pack "VARIANT")
        LogicalTypeScalar DuckDBV2LogicalTypeIdGeometry -> fromText (Text.pack "GEOMETRY")
        LogicalTypeScalar DuckDBV2LogicalTypeIdSQLNull -> fromText (Text.pack "\"NULL\"")
        LogicalTypeScalar dtype -> construct dtype [] []
        LogicalTypeGeometry Nothing -> fromText (Text.pack "GEOMETRY")
        LogicalTypeGeometry (Just crs) -> mask_ do
            logical <- construct DuckDBV2LogicalTypeIdGeometry [] [text crs]
            flip onException (destroyLogicalType logical) do
                count <- output (c_duckdb_v2_logical_type_get_param_count logical)
                when (count == 0) $
                    throwIO (SQLError (Text.pack "duckdb-simple: DuckDB did not preserve the requested GEOMETRY CRS; use a recognized identifier or a complete CRS definition") Nothing Nothing)
                pure logical
        LogicalTypeDecimal width scale -> do
            when (width < 1 || width > 38 || scale > width) $
                throwIO (userError "duckdb-simple: invalid DECIMAL width or scale")
            construct DuckDBV2LogicalTypeIdDecimal [] [output (c_duckdb_v2_value_create_utinyint factory width), output (c_duckdb_v2_value_create_utinyint factory scale)]
        LogicalTypeList element -> construct DuckDBV2LogicalTypeIdList [] [boxed element]
        LogicalTypeArray element size -> do
            when (toInteger size > toInteger (maxBound :: Int)) $
                throwIO (userError "duckdb-simple: ARRAY size exceeds Int range")
            construct DuckDBV2LogicalTypeIdArray [] [boxed element, output (c_duckdb_v2_value_create_bigint factory (fromIntegral size))]
        LogicalTypeMap key value -> construct DuckDBV2LogicalTypeIdMap [] [boxed key, boxed value]
        LogicalTypeStruct fields
            | null (elems fields) ->
                bracket (output (c_duckdb_v2_value_create_struct factory (ConstPtr nullPtr) (ConstPtr nullPtr) 0)) owned (output . c_duckdb_v2_value_get_logical_type)
        LogicalTypeStruct fields -> construct DuckDBV2LogicalTypeIdStruct (map structFieldName (elems fields)) (map (boxed . structFieldValue) (elems fields))
        LogicalTypeUnion members -> construct DuckDBV2LogicalTypeIdUnion (map unionMemberName (elems members)) (map (boxed . unionMemberType) (elems members))
        LogicalTypeEnum entries -> construct DuckDBV2LogicalTypeIdEnum [] (map text (elems entries))

-- | Reject a native count that does not fit a Haskell array index.
word64ToInt :: (Integral a) => a -> IO Int
word64ToInt value
    | toInteger value <= toInteger (maxBound :: Int) = pure (fromIntegral value)
    | otherwise = throwIO (userError "duckdb-simple: logical type parameter count exceeds Int range")
