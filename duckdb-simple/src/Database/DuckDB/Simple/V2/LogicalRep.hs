{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Database.DuckDB.Simple.V2.LogicalRep
-- Description : Convert logical type trees to and from native logical types.
--
-- The functions of this module read and create native logical type handles.
-- "Database.DuckDB.Simple.LogicalRep" has the Haskell type trees.
--
-- The examples use the imports of the module header of "Database.DuckDB.Simple.V2.Registration".
module Database.DuckDB.Simple.V2.LogicalRep (
  logicalTypeToRep,
  logicalTypeToBaseRep,
  valuesHaveType,
  logicalTypeFromRep,
  logicalTypeFromRepWith,
  bindingLogicalType,
  destroyLogicalType,
) where

import Control.Exception (bracket, mask_, onException, tryJust)
import Control.Monad (foldM, forM, when)
import Data.Array (elems, listArray)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), StructField (..), UnionMemberType (..))
import Database.DuckDB.Simple.Native
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)

-- | Copy a native logical type into a Haskell type tree. A type with an alias
-- gives 'LogicalTypeAlias', at each depth. See Note [Type aliases] in
-- "Database.DuckDB.Simple.LogicalRep". The function does not destroy the
-- handle. Keep the factory's owner alive until the function returns:
--
-- > bracket (R.output (c_duckdb_v2_vector_get_logical_type vector)) destroyLogicalType (logicalTypeToRep factory)
logicalTypeToRep :: DuckDBV2FactoryHandle -> DuckDBV2LogicalTypeHandle -> IO LogicalTypeRep
logicalTypeToRep factory = convertLogicalType (Just factory) ReadEnumDictionary

-- | Copy a native logical type into a Haskell type tree with no alias, at each
-- depth. Use it when decoding or binding needs only base types. The function
-- reads no type name. See Note [Type aliases] in
-- "Database.DuckDB.Simple.LogicalRep". The function does not destroy the
-- handle.
logicalTypeToBaseRep :: DuckDBV2LogicalTypeHandle -> IO LogicalTypeRep
logicalTypeToBaseRep = convertLogicalType Nothing ReadEnumDictionary

-- | Check the types of borrowed values against a borrowed logical type,
-- including exact field names, collations, and CRS definitions. Each temporary
-- value type is released before the next value is checked. See
-- Note [Element types of a list] in "Database.DuckDB.Simple.ToField".
valuesHaveType :: DuckDBV2LogicalTypeHandle -> [DuckDBV2ValueHandle] -> IO Bool
valuesHaveType logical values = do
  expected <- key logical
  let check [] = pure True
      check (value : remaining) = do
        matches <- bracket (output (c_duckdb_v2_value_get_logical_type value)) destroyLogicalType \actual -> do
          equal <- output (c_duckdb_v2_logical_type_is_equal logical actual)
          if equal == 0 then pure False else (== expected) <$> key actual
        if matches then check remaining else pure False
  check values
  where
    -- Native equality checks aliases and ENUM dictionaries. The partial trees
    -- are comparison keys only. They do not leave this function.
    key = convertLogicalType Nothing OmitEnumDictionary

-- | Select complete dictionaries or private comparison keys.
data EnumDictionary = ReadEnumDictionary | OmitEnumDictionary

-- | Copy a native logical type into a Haskell type tree. With a factory, keep
-- each alias. Without a factory, read no type name.
convertLogicalType :: Maybe DuckDBV2FactoryHandle -> EnumDictionary -> DuckDBV2LogicalTypeHandle -> IO LogicalTypeRep
convertLogicalType factory dictionary logical = do
  dtype <- output (c_duckdb_v2_logical_type_get_id logical)
  let parameter index action = alloca \nameSlot ->
        bracket (output (c_duckdb_v2_logical_type_get_param logical index nameSlot)) (destroy c_duckdb_v2_value_destroy) \value -> do
          name <- peek nameSlot >>= readIdentifier
          action name value
      child index = parameter index \_ value ->
        bracket (output (c_duckdb_v2_value_get_type value)) destroyLogicalType (convertLogicalType factory dictionary)
      string index = parameter index \_ value -> output (c_duckdb_v2_value_get_varchar value) >>= readText
      count = output (c_duckdb_v2_logical_type_get_param_count logical) >>= word64ToInt
      fields make = do
        size <- count
        listArray (0, size - 1)
          <$> forM
            [0 .. size - 1]
            ( \index -> parameter (fromIntegral index) \name value -> do
                rep <- bracket (output (c_duckdb_v2_value_get_type value)) destroyLogicalType (convertLogicalType factory dictionary)
                pure (make name rep)
            )
  base <- case dtype of
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
    DuckDBV2LogicalTypeIdEnum -> case dictionary of
      OmitEnumDictionary -> pure (LogicalTypeEnum (listArray (0, -1) []))
      ReadEnumDictionary -> do
        size <- count
        -- A dictionary can have many entries, and each entry makes native
        -- calls. The fold keeps a constant stack. See
        -- Note [Constant stack at safe calls] in "Database.DuckDB.Simple.Native".
        LogicalTypeEnum . listArray (0, size - 1) . reverse
          <$> foldM (\entries index -> (: entries) <$> string (fromIntegral index)) [] [0 .. size - 1]
    _ -> LogicalTypeScalar <$> logicalTypeIdFromNative dtype
  case factory of
    Nothing -> pure base
    Just nativeFactory -> do
      ident <- logicalTypeIdFromNative dtype
      typeName <- output (c_duckdb_v2_logical_type_get_name logical) >>= readIdentifier
      -- See Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
      hasAlias <-
        if typeName /= logicalTypeIdName ident
          then pure True
          else withIdentifier typeName \name ->
            bracket
              (output (c_duckdb_v2_factory_create_type_with_alias nativeFactory logical name))
              destroyLogicalType
              (\aliased -> (/= 0) <$> output (c_duckdb_v2_logical_type_is_equal logical aliased))
      pure (if hasAlias then LogicalTypeAlias typeName base else base)

-- | Construct an owned logical type through a borrowed native factory.
-- Keep the owner of the factory alive until you destroy the type.
-- CRS labels must be recognized by the native type constructor. Use the resolver
-- in 'logicalTypeFromRepWith' for SQL-derived types with arbitrary CRS labels.
-- 'Database.DuckDB.Simple.V2.Registration.withTypeDeclaration' constructs the type for an action and then destroys it.
--
-- > bracket (logicalTypeFromRep factory (LogicalTypeList (LogicalTypeScalar LogicalTypeIdBigInt))) destroyLogicalType (logicalTypeToRep factory)
logicalTypeFromRep :: DuckDBV2FactoryHandle -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
logicalTypeFromRep factory = logicalTypeFromRepWith factory (const (pure Nothing))

-- | Construct a type tree through a borrowed native factory. Use the resolver
-- for owned native leaf types. It runs before the parent types are constructed.
logicalTypeFromRepWith
  :: DuckDBV2FactoryHandle -> (LogicalTypeRep -> IO (Maybe DuckDBV2LogicalTypeHandle)) -> LogicalTypeRep -> IO DuckDBV2LogicalTypeHandle
logicalTypeFromRepWith factory resolve = create
  where
    boxed rep = bracket (create rep) destroyLogicalType (output . c_duckdb_v2_value_create_type factory)
    text value = withText value (output . c_duckdb_v2_value_create_varchar factory)
    fromText value = withText value (output . c_duckdb_v2_factory_create_type_from_text factory)
    fromName value = withIdentifier value \part ->
      bracket (output (c_duckdb_v2_qname_create part 1)) (destroy c_duckdb_v2_qname_destroy) \name ->
        output (c_duckdb_v2_factory_create_type_from_name factory name (ConstPtr nullPtr) (ConstPtr nullPtr) 0)
    construct dtype names actions = do
      when (any (Text.any (== '\0')) names) $
        throwInvalidArgument (Text.pack "logical type name contains NUL")
      withCreatedValues actions \values ->
        withArray values \valuePtr ->
          if null names
            then output (c_duckdb_v2_factory_create_type_from_id factory dtype (ConstPtr nullPtr) (ConstPtr valuePtr) (fromIntegral (length values)))
            else withIdentifierArray names \namePtr ->
              output (c_duckdb_v2_factory_create_type_from_id factory dtype (ConstPtr namePtr) (ConstPtr valuePtr) (fromIntegral (length values)))
    -- The binding resolver supplies CRS leaves. See Note [Bind-time CRS types].
    create rep = resolve rep >>= maybe (constructRep rep) pure
    constructRep = \case
      LogicalTypeScalar LogicalTypeIdVariant -> fromName (Text.pack "VARIANT")
      LogicalTypeScalar LogicalTypeIdGeometry -> fromText (Text.pack "GEOMETRY")
      LogicalTypeScalar LogicalTypeIdSQLNull -> fromName (Text.pack "NULL")
      LogicalTypeScalar dtype -> construct (logicalTypeIdToNative dtype) [] []
      LogicalTypeCollatedVarchar collation -> construct DuckDBV2LogicalTypeIdVarchar [Text.pack "collation"] [text collation]
      LogicalTypeGeometry Nothing -> fromText (Text.pack "GEOMETRY")
      LogicalTypeGeometry (Just crs) ->
        directGeometryType factory crs
          >>= maybe
            ( throwInvalidArgument
                (Text.pack "DuckDB did not preserve the requested GEOMETRY CRS; use a recognized identifier or a complete CRS definition")
            )
            pure
      LogicalTypeDecimal width scale -> do
        when (width < 1 || width > 38 || scale > width) $
          throwInvalidArgument (Text.pack "invalid DECIMAL width or scale")
        construct
          DuckDBV2LogicalTypeIdDecimal
          []
          [output (c_duckdb_v2_value_create_utinyint factory width), output (c_duckdb_v2_value_create_utinyint factory scale)]
      LogicalTypeList element -> construct DuckDBV2LogicalTypeIdList [] [boxed element]
      -- See Note [Sized ARRAY bounds] in "Database.DuckDB.Simple.Native".
      LogicalTypeArray element size -> do
        maybe (pure ()) throwInvalidArgument (arraySizeReason (toInteger size))
        construct DuckDBV2LogicalTypeIdArray [] [boxed element, output (c_duckdb_v2_value_create_bigint factory (fromIntegral size))]
      LogicalTypeMap key value -> construct DuckDBV2LogicalTypeIdMap [] [boxed key, boxed value]
      LogicalTypeStruct fields
        | null (elems fields) ->
            bracket
              (output (c_duckdb_v2_value_create_struct factory (ConstPtr nullPtr) (ConstPtr nullPtr) 0))
              destroyValue
              (output . c_duckdb_v2_value_get_logical_type)
      LogicalTypeStruct fields -> construct DuckDBV2LogicalTypeIdStruct (map structFieldName (elems fields)) (map (boxed . structFieldValue) (elems fields))
      LogicalTypeTuple fields
        | null (elems fields) ->
            bracket (output (c_duckdb_v2_value_create_tuple factory (ConstPtr nullPtr) 0)) destroyValue (output . c_duckdb_v2_value_get_logical_type)
        | otherwise -> construct DuckDBV2LogicalTypeIdTuple [] (map boxed (elems fields))
      LogicalTypeUnion members -> construct DuckDBV2LogicalTypeIdUnion (map unionMemberName (elems members)) (map (boxed . unionMemberType) (elems members))
      LogicalTypeEnum entries -> construct DuckDBV2LogicalTypeIdEnum [] (map text (elems entries))
      -- See Note [Type aliases] in "Database.DuckDB.Simple.LogicalRep".
      LogicalTypeAlias name base -> do
        when (Text.any (== '\0') name) $
          throwInvalidArgument (Text.pack "logical type name contains NUL")
        bracket (create base) destroyLogicalType \logical ->
          withIdentifier name (output . c_duckdb_v2_factory_create_type_with_alias factory logical)

-- | Reject a native count that does not fit a Haskell array index.
word64ToInt :: (Integral a) => a -> IO Int
word64ToInt value
  | toInteger value <= toInteger (maxBound :: Int) = pure (fromIntegral value)
  | otherwise = throwInternalError (Text.pack "logical type parameter count exceeds Int range")

-- | Construct an owned binding type. Resolve CRS leaves at every depth.
-- The factory must belong to a connection on the supplied instance.
bindingLogicalType
  :: DuckDBV2FactoryHandle
  -> DuckDBV2InstanceHandle
  -> LogicalTypeRep
  -> IO DuckDBV2LogicalTypeHandle
bindingLogicalType factory instance_ = logicalTypeFromRepWith factory resolve
  where
    resolve (LogicalTypeGeometry (Just crs)) = do
      direct <- tryJust unrecognized (directGeometryType factory crs)
      Just <$> case direct of
        Right (Just logical) -> pure logical
        _ -> queryGeometryType crs instance_
    resolve _ = pure Nothing
    unrecognized err
      | sqlErrorType err == Just ErrorQueryBinder
      , Just body <- sqlErrorBody err
      , Text.pack "Encountered unrecognized coordinate system '" `Text.isPrefixOf` body =
          Just ()
      | otherwise = Nothing

{- Note [Bind-time CRS types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Bindings construct native types from their declarations at every depth. The
connection factory constructs recognized CRS types directly. For an unknown
label, DuckDB either reports its specific binder error or drops the CRS when
ignore_unknown_crs is set. Only these two outcomes use the SQL fallback.
Any other error from the native constructor propagates to the caller.

The fallback opens a separate connection on the same instance. It cannot run
SQL on the caller connection because the caller can have an active result.
The query applies fully qualified built-ins to a WKB literal and the bound label.
The SELECT has a schema before its first execution step. Copy its field type
while the schema is live. The helper can close without fetching a row.
Typed NULLs and empty containers use the same resolver as nonempty values.
-}

-- | Construct a CRS type directly. Return 'Nothing' if DuckDB drops its CRS.
-- Keep native errors so the binding resolver can select the unknown CRS error.
directGeometryType :: DuckDBV2FactoryHandle -> Text -> IO (Maybe DuckDBV2LogicalTypeHandle)
directGeometryType factory crs = mask_ do
  validateGeometryCRS crs
  logical <- withText crs \text ->
    bracket (output (c_duckdb_v2_value_create_varchar factory text)) destroyValue \value ->
      with value \values ->
        output (c_duckdb_v2_factory_create_type_from_id factory DuckDBV2LogicalTypeIdGeometry (ConstPtr nullPtr) (ConstPtr values) 1)
  count <- output (c_duckdb_v2_logical_type_get_param_count logical) `onException` destroyLogicalType logical
  if count == 0 then destroyLogicalType logical >> pure Nothing else pure (Just logical)

-- | Reject CRS text that cannot identify a coordinate system.
validateGeometryCRS :: Text -> IO ()
validateGeometryCRS crs =
  when (Text.all isSpace crs || Text.any (== '\0') crs) $
    throwInvalidArgument (Text.pack "a GEOMETRY CRS must be nonblank and must not contain NUL")

-- | Read one owned GEOMETRY type from a short-lived helper connection.
queryGeometryType :: Text -> DuckDBV2InstanceHandle -> IO DuckDBV2LogicalTypeHandle
queryGeometryType crs instance_ = mask_ $
  bracket (output (c_duckdb_v2_connection_create instance_)) (destroy c_duckdb_v2_connection_destroy) \connection ->
    withSQLStatement connection sql \parsed -> withConnectionFactory connection \factory ->
      withText crs \name -> bracket (output (c_duckdb_v2_value_create_varchar factory name)) destroyValue \label ->
        with label \param ->
          bracket
            (output (c_duckdb_v2_statement_execute connection parsed (ConstPtr nullPtr) (ConstPtr param) 1))
            (destroy c_duckdb_v2_result_destroy)
            \result ->
              bracket (output (c_duckdb_v2_result_get_schema result)) (destroy c_duckdb_v2_schema_destroy) \schema -> alloca \nameSlot -> do
                logical <- output (c_duckdb_v2_schema_get_field schema 0 nameSlot)
                count <- output (c_duckdb_v2_logical_type_get_param_count logical)
                when (count == 0) $ throwInvalidArgument (Text.pack "the type query did not preserve the GEOMETRY CRS")
                output (c_duckdb_v2_logical_type_copy logical)
  where
    -- Little-endian WKB for GEOMETRYCOLLECTION EMPTY. The hex literal has type BLOB.
    sql = Text.pack "SELECT system.main.ST_SetCRS(system.main.ST_GeomFromWKB(X'010700000000000000'), ?)"
