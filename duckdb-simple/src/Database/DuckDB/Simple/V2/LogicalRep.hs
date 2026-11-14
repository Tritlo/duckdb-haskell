{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Database.DuckDB.Simple.V2.LogicalRep
Description : Convert logical type trees to and from native logical types.

The functions of this module read and create native logical type handles.
"Database.DuckDB.Simple.LogicalRep" has the Haskell type trees.
-}
module Database.DuckDB.Simple.V2.LogicalRep (
    logicalTypeToRep,
    logicalTypeFromRep,
    logicalTypeFromRepWith,
    destroyLogicalType,
) where

import Control.Exception (bracket, mask_, onException, throwIO)
import Control.Monad (foldM, forM, when)
import Data.Array (elems, listArray)
import qualified Data.Text as Text
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), UnionMemberType (..))
import Database.DuckDB.Simple.Native
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)

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
        DuckDBV2LogicalTypeIdTuple -> LogicalTypeTuple <$> fields (\_ rep -> rep)
        DuckDBV2LogicalTypeIdVarchar -> do
            size <- count
            if size == 0 then pure (LogicalTypeScalar LogicalTypeIdVarchar) else LogicalTypeCollatedVarchar <$> string 0
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
            -- A dictionary can have many entries, and each entry makes native
            -- calls. The fold keeps a constant stack.
            LogicalTypeEnum . listArray (0, size - 1) . reverse <$> foldM (\entries index -> (: entries) <$> string (fromIntegral index)) [] [0 .. size - 1]
        _ -> LogicalTypeScalar <$> logicalTypeIdFromNative dtype

{- | Construct an owned logical type through a borrowed native factory.
Keep the owner of the factory alive until you destroy the type.
CRS labels must be recognized by the native type constructor. Use the resolver
in 'logicalTypeFromRepWith' for SQL-derived types with arbitrary CRS labels.
-}
logicalTypeFromRep :: DuckDBV2FactoryHandle -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
logicalTypeFromRep factory = logicalTypeFromRepWith factory (const (pure Nothing))

{- | Construct a type tree through a borrowed native factory. Use the resolver
for owned native leaf types. It runs before the parent types are constructed.
-}
logicalTypeFromRepWith :: DuckDBV2FactoryHandle -> (LogicalTypeRep -> IO (Maybe DuckDBV2LogicalTypeHandle)) -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
logicalTypeFromRepWith factory resolve = create
  where
    boxed rep = bracket (create rep) destroyLogicalType (output . c_duckdb_v2_value_create_type factory)
    text value = withText value (output . c_duckdb_v2_value_create_varchar factory)
    fromText value = withText value (output . c_duckdb_v2_factory_create_type_from_text factory)
    construct dtype names actions = do
        when (any (Text.any (== '\0')) names) $
            throwIO (userError "duckdb-simple: logical type name contains NUL")
        withCreatedValues actions \values ->
            withArray values \valuePtr ->
                if null names
                    then output (c_duckdb_v2_factory_create_type_from_id factory dtype (ConstPtr nullPtr) (ConstPtr valuePtr) (fromIntegral (length values)))
                    else withIdentifierArray names \namePtr ->
                        output (c_duckdb_v2_factory_create_type_from_id factory dtype (ConstPtr namePtr) (ConstPtr valuePtr) (fromIntegral (length values)))
    create rep = resolve rep >>= maybe (constructRep rep) pure
    constructRep = \case
        LogicalTypeScalar LogicalTypeIdVariant -> fromText (Text.pack "VARIANT")
        LogicalTypeScalar LogicalTypeIdGeometry -> fromText (Text.pack "GEOMETRY")
        LogicalTypeScalar LogicalTypeIdSQLNull -> fromText (Text.pack "\"NULL\"")
        LogicalTypeScalar dtype -> construct (logicalTypeIdToNative dtype) [] []
        LogicalTypeCollatedVarchar collation -> construct DuckDBV2LogicalTypeIdVarchar [Text.pack "collation"] [text collation]
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
                bracket (output (c_duckdb_v2_value_create_struct factory (ConstPtr nullPtr) (ConstPtr nullPtr) 0)) destroyValue (output . c_duckdb_v2_value_get_logical_type)
        LogicalTypeStruct fields -> construct DuckDBV2LogicalTypeIdStruct (map structFieldName (elems fields)) (map (boxed . structFieldValue) (elems fields))
        LogicalTypeTuple fields
            | null (elems fields) ->
                bracket (output (c_duckdb_v2_value_create_tuple factory (ConstPtr nullPtr) 0)) destroyValue (output . c_duckdb_v2_value_get_logical_type)
            | otherwise -> construct DuckDBV2LogicalTypeIdTuple [] (map boxed (elems fields))
        LogicalTypeUnion members -> construct DuckDBV2LogicalTypeIdUnion (map unionMemberName (elems members)) (map (boxed . unionMemberType) (elems members))
        LogicalTypeEnum entries -> construct DuckDBV2LogicalTypeIdEnum [] (map text (elems entries))

-- | Reject a native count that does not fit a Haskell array index.
word64ToInt :: (Integral a) => a -> IO Int
word64ToInt value
    | toInteger value <= toInteger (maxBound :: Int) = pure (fromIntegral value)
    | otherwise = throwIO (userError "duckdb-simple: logical type parameter count exceeds Int range")
