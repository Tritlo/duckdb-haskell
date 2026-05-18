{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Database.DuckDB.Simple.Appender.Generic (
    AppenderDuckValue (..),
    ViaJSON (..),
    ViaShow (..),
    ViaDuckStruct (..),
    ViaDuckUnion (..),
    ViaDuckEnum (..),
    AppendTableRow (..),
    DuckTypeName (..),
    appenderLogicalType,
    cacheAppenderLogicalType,
    DuckStructField(..),
    duckStructLogicalType,
    duckStructTypeName,
    duckStructValue,
    Allocated(..),
    DuckDBValue,
    Uncached (..),
    assertSuccess,
    GDuckUnion(..)
) where

import Control.Monad (join, (>=>))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, newIORef, readIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import qualified Data.Text.Foreign as TextForeign
import qualified Data.Text.Lazy as LText
import Data.Time (LocalTime (..), UTCTime, utc, utcToLocalTime, NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Typeable (TypeRep, Typeable, typeRep)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Ptr, Storable (poke), alloca, castPtr, withMany)
import Foreign.C.Types (CDouble (..), CFloat (CFloat))
import Foreign.Marshal.Array (withArray)
import GHC.Generics
import GHC.IO (unsafePerformIO, throwIO)
import GHC.IORef (atomicModifyIORef'_)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (BigNum (..), IntervalValue (..), TimeWithZone (..))
import Database.DuckDB.Simple.Internal
import Database.DuckDB.Simple.Internal.ValueHelpers
import Data.Scientific (Scientific)
import GHC.Stack (HasCallStack)
import Foreign.C (peekCString)
import Data.Bifunctor (first)
import qualified Data.Vector.Mutable as VM
import Data.Bits (unsafeShiftR)
import qualified Data.Vector as V
import Control.Monad.ST

newtype Allocated a = Allocated {allocate :: IO a}

newtype DuckTypeName = DuckTypeName {renderDuckTypeName :: Text}
    deriving newtype (Semigroup, Monoid, IsString)

class Destroy a where
    destroyAllocated :: a -> IO ()

instance Destroy DuckDBValue where
    destroyAllocated = destroyValue

instance Destroy DuckDBLogicalType where
    destroyAllocated = destroyLogicalType

withAllocated :: (Destroy a) => Allocated a -> (a -> IO b) -> IO b
withAllocated alloc go = do
    val <- allocate alloc
    go val <* destroyAllocated val
{-# SPECIALIZE INLINE withAllocated :: Allocated DuckDBValue -> (DuckDBValue -> IO DuckDBState) -> IO DuckDBState #-}

withManyAllocated :: (Destroy a, Foldable f, Traversable f) => f (Allocated a) -> (f a -> IO b) -> IO b
withManyAllocated alloc go = do
    a <- mapM allocate alloc
    go a <* mapM_ destroyAllocated a
{-# SPECIALIZE INLINE withManyAllocated :: [Allocated DuckDBValue] -> ([DuckDBValue] -> IO [DuckDBState]) -> IO [DuckDBState] #-}

cache :: IORef (HashMap TypeRep DuckDBLogicalType)
cache = unsafePerformIO (newIORef mempty)
{-# NOINLINE cache #-}

{- | We only materialize duckdb logical types once and we never release them.
This resulted in a significant performance gain when using high-performance appender API.
-}
appenderLogicalType :: (AppenderDuckValue a, Typeable a) => Proxy a -> IO DuckDBLogicalType
appenderLogicalType pxy = cacheAppenderLogicalType (typeRep pxy) (appenderLogicalTypeUncached pxy)

newtype Uncached a = Uncached (IO a)

cacheAppenderLogicalType :: TypeRep -> Uncached DuckDBLogicalType -> IO DuckDBLogicalType
cacheAppenderLogicalType hsRep (Uncached alloc) = do
    cachedMb <- HashMap.lookup hsRep <$> readIORef cache
    case cachedMb of
        Just cached -> pure cached
        Nothing -> do
            rep <- alloc
            _ <- atomicModifyIORef'_ cache (HashMap.insert hsRep rep)
            pure rep

class AppenderDuckValue a where
    appenderDuckValue :: a -> Allocated DuckDBValue

    appendDuckValue :: DuckDBAppender -> a -> IO DuckDBState
    appendDuckValue appender val = withAllocated (appenderDuckValue val) (c_duckdb_append_value appender)

    appenderLogicalTypeUncached :: Proxy a -> Uncached DuckDBLogicalType
    appenderTypeName :: Proxy a -> DuckTypeName

primitiveType :: DuckDBType -> Uncached DuckDBLogicalType
primitiveType = Uncached . c_duckdb_create_logical_type

instance AppenderDuckValue Bool where
    appenderDuckValue = Allocated . boolDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeBoolean
    appendDuckValue app value = c_duckdb_append_bool app (if value then 1 else 0)
    appenderTypeName _ = "BOOL"

instance AppenderDuckValue Int where
    appenderDuckValue = Allocated . int64DuckValue . fromIntegral
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeBigInt
    appendDuckValue app = c_duckdb_append_int64 app . fromIntegral
    appenderTypeName _ = "BIGINT"

instance AppenderDuckValue Int8 where
    appenderDuckValue = Allocated . int8DuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeTinyInt
    appendDuckValue = c_duckdb_append_int8
    appenderTypeName _ = "TINYINT"

instance AppenderDuckValue Int16 where
    appenderDuckValue = Allocated . int16DuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeSmallInt
    appendDuckValue = c_duckdb_append_int16
    appenderTypeName _ = "SMALLINT"

instance AppenderDuckValue Int32 where
    appenderDuckValue = Allocated . int32DuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeInteger
    appendDuckValue = c_duckdb_append_int32
    appenderTypeName _ = "INT"

instance AppenderDuckValue Int64 where
    appenderDuckValue = Allocated . int64DuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeBigInt
    appendDuckValue = c_duckdb_append_int64
    appenderTypeName _ = "BIGINT"

instance AppenderDuckValue Integer where
    appenderDuckValue = Allocated . bigNumDuckValue . BigNum
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeBigNum
    appenderTypeName _ = "BIGNUM"

instance AppenderDuckValue Word where
    appenderDuckValue = Allocated . uint64DuckValue . fromIntegral
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeUBigInt
    appendDuckValue app = c_duckdb_append_uint64 app . fromIntegral
    appenderTypeName _ = "UBIGINT"

instance AppenderDuckValue Word8 where
    appenderDuckValue = Allocated . uint8DuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeUTinyInt
    appendDuckValue = c_duckdb_append_uint8
    appenderTypeName _ = "UTINYINT"

instance AppenderDuckValue Word16 where
    appenderDuckValue = Allocated . uint16DuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeUSmallInt
    appendDuckValue = c_duckdb_append_uint16
    appenderTypeName _ = "USMALLINT"

instance AppenderDuckValue Word32 where
    appenderDuckValue = Allocated . uint32DuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeUInteger
    appendDuckValue = c_duckdb_append_uint32
    appenderTypeName _ = "UINTEGER"

instance AppenderDuckValue Word64 where
    appenderDuckValue = Allocated . uint64DuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeUBigInt
    appendDuckValue = c_duckdb_append_uint64
    appenderTypeName _ = "UBIGINT"

instance AppenderDuckValue Float where
    appenderDuckValue = Allocated . floatDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeFloat
    appendDuckValue app = c_duckdb_append_float app . CFloat
    appenderTypeName _ = "FLOAT"

instance AppenderDuckValue Double where
    appenderDuckValue = Allocated . doubleDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeDouble
    appendDuckValue app = c_duckdb_append_double app . CDouble
    appenderTypeName _ = "DOUBLE"

instance AppenderDuckValue Scientific where
    appenderDuckValue = Allocated . doubleDuckValue . realToFrac
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeDouble
    appendDuckValue app = c_duckdb_append_double app . CDouble . realToFrac
    appenderTypeName _ = "DOUBLE"

instance AppenderDuckValue Text where
    appenderDuckValue = Allocated . textDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeVarchar
    appendDuckValue app txt = TextForeign.withCString txt (c_duckdb_append_varchar app)
    appenderTypeName _ = "VARCHAR"

instance AppenderDuckValue BS.ByteString where
    appenderDuckValue = Allocated . blobDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeBlob
    appendDuckValue app bs = BS.useAsCStringLen bs \(ptr, len) ->
        c_duckdb_append_blob app (castPtr ptr :: Ptr ()) (fromIntegral len)
    appenderTypeName _ = "BLOB"

instance AppenderDuckValue Day where
    appenderDuckValue = Allocated . dayDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeDate
    appendDuckValue app = encodeDay >=> c_duckdb_append_date app
    appenderTypeName _ = "DATE"

instance AppenderDuckValue TimeOfDay where
    appenderDuckValue = Allocated . timeOfDayDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeTime
    appendDuckValue app = encodeTimeOfDay >=> c_duckdb_append_time app
    appenderTypeName _ = "TIME"

instance AppenderDuckValue LocalTime where
    appenderDuckValue = Allocated . localTimeDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeTimestamp
    appendDuckValue app = encodeLocalTime >=> c_duckdb_append_timestamp app
    appenderTypeName _ = "TIMESTAMP"

instance AppenderDuckValue UTCTime where
    appenderDuckValue = Allocated . utcTimeDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeTimestampTz
    appendDuckValue app = encodeLocalTime . utcToLocalTime utc >=> c_duckdb_append_timestamp app
    appenderTypeName _ = "TIMESTAMPTZ"

instance AppenderDuckValue UUID.UUID where
    appenderDuckValue = Allocated . uuidDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeUUID
    appenderTypeName _ = "UUID"

instance AppenderDuckValue NominalDiffTime where
  appenderDuckValue v = appenderDuckValue $ IntervalValue{intervalMonths = 0, intervalDays = 0, intervalMicros = truncate $ nominalDiffTimeToSeconds v * 1e6}
  appendDuckValue app v = appendDuckValue app $ IntervalValue{intervalMonths = 0, intervalDays = 0, intervalMicros = truncate $ nominalDiffTimeToSeconds v * 1e6}
  appenderLogicalTypeUncached _ = appenderLogicalTypeUncached (Proxy @IntervalValue)
  appenderTypeName _ = appenderTypeName (Proxy @IntervalValue)


instance AppenderDuckValue IntervalValue where
    appenderDuckValue = Allocated . intervalDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeInterval
    appendDuckValue app IntervalValue{intervalMonths, intervalDays, intervalMicros} =
        alloca \ptr -> do
            poke ptr (DuckDBInterval intervalMonths intervalDays intervalMicros)
            c_duckdb_append_interval app ptr
    appenderTypeName _ = "INTERVAL"

instance AppenderDuckValue TimeWithZone where
    appenderDuckValue = Allocated . timeWithZoneDuckValue
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeTimeTz
    appenderTypeName _ = "TIMETZ"

instance (AppenderDuckValue a) => AppenderDuckValue (Maybe a) where
    appenderDuckValue (Just x) = appenderDuckValue x
    appenderDuckValue Nothing = Allocated nullDuckValue
    {-# INLINE appenderDuckValue #-}
    appenderLogicalTypeUncached _ = appenderLogicalTypeUncached (Proxy :: Proxy a)
    appendDuckValue app = maybe (c_duckdb_append_null app) (appendDuckValue app)
    appenderTypeName _ = appenderTypeName (Proxy @a)

instance (AppenderDuckValue a, Typeable a) => AppenderDuckValue [a] where
    appenderDuckValue = arrayDuckValue
    {-# INLINE appenderDuckValue #-}
    appenderLogicalTypeUncached _ = Uncached $ appenderLogicalType (Proxy @a) >>= c_duckdb_create_list_type
    appenderTypeName _ = appenderTypeName (Proxy @a) <> "[]"

-- instance (AppenderDuckValue a) => AppenderDuckValue (Vector len a) where -- from vector-sized, I don't need it right now so whatever
--     appenderDuckValue = arrayDuckValue
--     appenderLogicalTypeUncached _ = do
--       inner <- leakAllocated <$> appenderLogicalType (Proxy @a)
--       Allocated <$> c_duckdb_create_array_type size inner

arrayDuckValue ::
    forall a f.
    (AppenderDuckValue a, Foldable f, Typeable a) =>
    f a ->
    Allocated DuckDBValue
arrayDuckValue arr = Allocated $ do
    elementType <- appenderLogicalType (Proxy :: Proxy a)
    let elemsList = toList arr
        count = length elemsList
    withManyAllocated (appenderDuckValue <$> elemsList) $ \values ->
        withArray values \ptr ->
            c_duckdb_create_array_value elementType ptr (fromIntegral count)

-- | NonEmpty list values encode as DuckDB LIST (variable-length).
instance (AppenderDuckValue a, Typeable a) => AppenderDuckValue (NonEmpty a) where
    appenderDuckValue = appenderDuckValue . toList
    appenderLogicalTypeUncached _ = Uncached $ appenderLogicalType (Proxy @[a])
    appenderTypeName _ = appenderTypeName (Proxy @a) <> "[]"

instance (AppenderDuckValue a, Ord a, Typeable a) => AppenderDuckValue (Set a) where
    appenderDuckValue = appenderDuckValue . toList
    appenderLogicalTypeUncached _ = Uncached $ appenderLogicalType (Proxy @[a])
    appenderTypeName _ = appenderTypeName (Proxy @a) <> "[]"

instance AppenderDuckValue Aeson.Value where
    appenderDuckValue = Allocated . textDuckValue . LText.toStrict . Aeson.encodeToLazyText
    appenderLogicalTypeUncached _ = primitiveType DuckDBTypeVarchar
    appendDuckValue app = appendDuckValue app . LText.toStrict . Aeson.encodeToLazyText
    appenderTypeName _ = "JSON"

instance (Ord k, AppenderDuckValue k, AppenderDuckValue v, Typeable k, Typeable v) => AppenderDuckValue (Map.Map k v) where
    appenderDuckValue m = Allocated $ do
        mapType <- appenderLogicalType (Proxy @(Map k v))
        let count = Map.size m
        withManyAllocated (appenderDuckValue <$> Map.keys m) $ \keys ->
            withManyAllocated (appenderDuckValue <$> Map.elems m) $ \values ->
                withArray keys \ptrK ->
                    withArray values \ptrV -> c_duckdb_create_map_value mapType ptrK ptrV (fromIntegral count)

    appenderLogicalTypeUncached _ = Uncached $ join $ c_duckdb_create_map_type <$> appenderLogicalType (Proxy @k) <*> appenderLogicalType (Proxy @v)
    appenderTypeName _ = "MAP(" <> appenderTypeName (Proxy @k) <> ", " <> appenderTypeName (Proxy @v) <> ")"

---

newtype ViaDuckStruct a = ViaDuckStruct a

instance (Typeable a, GDuckStruct (Rep a), Generic a, GDuckStructType (Rep a)) => AppenderDuckValue (ViaDuckStruct a) where
    appenderDuckValue (ViaDuckStruct v) = duckStructValue (cacheAppenderLogicalType (typeRep $ Proxy @a) (duckStructLogicalType $ gstructType $ Proxy @(Rep a))) (gstructValue $ from v)

    appenderLogicalTypeUncached _ = duckStructLogicalType $ gstructType $ Proxy @(Rep a)

    appenderTypeName _ = duckStructTypeName $ gstructType $ Proxy @(Rep a)

data DuckStructField = DuckStructField {structFieldName :: Text, structFieldType :: DuckTypeName, structLogicalType :: IO DuckDBLogicalType}

duckStructValue :: IO DuckDBLogicalType -> V.Vector (Allocated DuckDBValue) -> Allocated DuckDBValue
duckStructValue getType vals = Allocated $ do
    structType <- getType
    withManyAllocated vals $ \childValues ->
        withArray (V.toList childValues) $ c_duckdb_create_struct_value structType

duckStructLogicalType :: [DuckStructField] -> Uncached DuckDBLogicalType
duckStructLogicalType flds = Uncached $ do
    evaluatedTypes <- mapM structLogicalType flds
    withMany Text.withCString (structFieldName <$> flds) $ \namePtrs ->
        withArray namePtrs $ \nameArray ->
            withArray evaluatedTypes $ \typeArray ->
                c_duckdb_create_struct_type typeArray nameArray (fromIntegral $ length flds)

gunionTypeLogical :: [DuckStructField] -> Uncached DuckDBLogicalType
gunionTypeLogical ctors = Uncached $ do
    evaluatedTypes <- mapM structLogicalType ctors
    withMany Text.withCString (structFieldName <$> ctors) $ \namePtrs ->
        withArray namePtrs $ \nameArray ->
            withArray evaluatedTypes $ \typeArray ->
                c_duckdb_create_union_type typeArray nameArray (fromIntegral $ length ctors)

duckStructTypeName :: [DuckStructField] -> DuckTypeName
duckStructTypeName = gstructTypeName "STRUCT"

gstructTypeName :: Text -> [DuckStructField] -> DuckTypeName
gstructTypeName pfx flds = DuckTypeName $ pfx <> "(" <> Text.intercalate ", " ["\"" <> nme <> "\" " <> renderDuckTypeName tpeNme | DuckStructField nme tpeNme _ <- flds] <> ")"

class GDuckStruct (f :: Type -> Type) where
    gstructValue :: f b -> V.Vector (Allocated DuckDBValue)

instance (AppenderDuckValue a, KnownSymbol selectorName, Typeable a) => GDuckStruct (S1 ('MetaSel ('Just selectorName) q w e) (K1 i a)) where
    gstructValue (M1 (K1 v)) = V.singleton $ appenderDuckValue v
    {-# INLINE gstructValue #-}

instance (WriteProduct a, WriteProduct b, ProductSize a, ProductSize b) => GDuckStruct (a :*: b) where
    gstructValue p =
         V.create $ do
          mv <- VM.unsafeNew lenProduct
          writeProduct mv 0 lenProduct p
          pure mv
        where
          lenProduct = productSize (Proxy @(a :*: b))
    {-# INLINE gstructValue #-}

instance (GDuckStruct a) => GDuckStruct (M1 C c a) where
    gstructValue (M1 v) = gstructValue v
    {-# INLINE gstructValue #-}

instance (GDuckStruct a) => GDuckStruct (M1 D c a) where
    gstructValue (M1 v) = gstructValue v

class ProductSize (f :: Type -> Type) where
    productSize :: Proxy f -> Int

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize _ = productSize (Proxy @a) + productSize (Proxy @b)

instance ProductSize (S1 s a) where
    productSize _ = 1

class WriteProduct f where
    writeProduct :: VM.MVector s (Allocated DuckDBValue) -> Int -> Int -> f a -> ST s ()

instance (WriteProduct a, WriteProduct b) => WriteProduct (a :*: b) where
    writeProduct  mv ix len (a :*: b) = do
      writeProduct  mv ix  lenL a
      writeProduct  mv ixR lenR b
        where
          lenL = len `unsafeShiftR` 1
          lenR = len - lenL
          ixR  = ix  + lenL
    {-# INLINE writeProduct #-}

instance (AppenderDuckValue a) => WriteProduct (S1 meta (K1 i a)) where
    writeProduct mv ix _ (M1 (K1 val)) = VM.unsafeWrite mv ix (appenderDuckValue val)
    {-# INLINE writeProduct #-}

class GDuckStructType (f :: Type -> Type) where
    gstructType :: Proxy f -> [DuckStructField]

instance (AppenderDuckValue a, KnownSymbol selectorName, Typeable a) => GDuckStructType (S1 ('MetaSel ('Just selectorName) q w e) (K1 i a)) where
    gstructType _ = pure $ DuckStructField (Text.pack $ symbolVal (Proxy @selectorName)) (appenderTypeName (Proxy @a)) (appenderLogicalType (Proxy @a))

instance (GDuckStructType a, GDuckStructType b) => GDuckStructType (a :*: b) where
    gstructType _ = gstructType (Proxy @a) <> gstructType (Proxy @b)

instance (GDuckStructType a) => GDuckStructType (M1 C c a) where
    gstructType _ = gstructType (Proxy @a)

instance (GDuckStructType a) => GDuckStructType (M1 D c a) where
    gstructType _ = gstructType (Proxy @a)

--

newtype ViaDuckUnion a = ViaDuckUnion a

instance (Typeable a, GDuckUnion (Rep a), Generic a) => AppenderDuckValue (ViaDuckUnion a) where
    appenderDuckValue (ViaDuckUnion v) = Allocated $ do
        unionType <- cacheAppenderLogicalType (typeRep $ Proxy @a) (gunionTypeLogical $ gunionType (Proxy @a) (Proxy @(Rep a)))
        let (ix, valueIO) = gunionValue (Proxy @a) $ from v
        withAllocated valueIO $ c_duckdb_create_union_value unionType ix
    appenderLogicalTypeUncached _ = gunionTypeLogical $ gunionType (Proxy @a) $ Proxy @(Rep a)

    appenderTypeName _ = gstructTypeName "UNION" $ gunionType (Proxy @a) $ Proxy @(Rep a)

class GDuckUnion (f :: Type -> Type) where
    gunionValue :: (Typeable root) => Proxy root -> f b -> (Word64, Allocated DuckDBValue)
    gunionType :: (Typeable root) => Proxy root -> Proxy f -> [DuckStructField]

data Tople (a :: Type) (b :: Symbol)

instance (GDuckUnion a, GDuckUnion b, GConstructorCount a) => GDuckUnion (a :+: b) where
    gunionValue root (L1 l) = gunionValue root l
    gunionValue root (R1 r) = first (+ gconstructorCount  (Proxy @a)) $ gunionValue root r
    {-# INLINE gunionValue #-}
    gunionType root _ = gunionType root (Proxy @a) <> gunionType root (Proxy @b)

instance {-# OVERLAPPABLE #-} (KnownSymbol conName) => GDuckUnion (C1 ('MetaCons conName foo bar) U1) where
    gunionValue (_ :: Proxy root) (M1 _) = (0, Allocated $ int8DuckValue 0)
    {-# INLINE gunionValue #-}
    gunionType _root _ = [DuckStructField (Text.pack $ symbolVal (Proxy @conName)) "TINYINT" (appenderLogicalType (Proxy @Int8))]

instance {-# OVERLAPPABLE #-} (KnownSymbol conName, Typeable a, AppenderDuckValue a) => GDuckUnion (C1 ('MetaCons conName foo bar) (S1 ms (Rec0 a))) where
    gunionValue (_ :: Proxy root) (M1 (M1 (K1 v))) = (0,  appenderDuckValue v)
    {-# INLINE gunionValue #-}
    gunionType _root _ = [DuckStructField (Text.pack $ symbolVal (Proxy @conName)) (appenderTypeName (Proxy @a)) (appenderLogicalType (Proxy @a))]

instance {-# OVERLAPS #-} (GDuckStructType a, GDuckStruct a, KnownSymbol conName) => GDuckUnion (C1 ('MetaCons conName foo bar) a) where
    gunionValue (_ :: Proxy root) (M1 v) = (0,) $ Allocated $ do
        structType <- cacheAppenderLogicalType (typeRep $ Proxy @(Tople root conName)) (duckStructLogicalType $ gstructType $ Proxy @a)
        withManyAllocated (gstructValue v) $ \childValues ->
            withArray (V.toList childValues) $ c_duckdb_create_struct_value structType
    {-# INLINE gunionValue #-}
    gunionType (_ :: Proxy root) _ = [DuckStructField (Text.pack $ symbolVal (Proxy @conName)) (gstructTypeName "STRUCT" t) (cacheAppenderLogicalType (typeRep $ Proxy @(Tople root conName)) $ duckStructLogicalType t)]
      where
        t = gstructType $ Proxy @a

instance (GDuckUnion a) => GDuckUnion (M1 D c a) where
    gunionValue root (M1 v) = gunionValue root v
    {-# INLINE gunionValue #-}
    gunionType root _ = gunionType root (Proxy @a)

class GConstructorCount (f :: Type -> Type) where
  gconstructorCount :: Proxy f -> Word64

instance (GConstructorCount a, GConstructorCount b) => GConstructorCount (a :+: b) where
  gconstructorCount _ = gconstructorCount (Proxy @a) + gconstructorCount (Proxy @b)
  {-# INLINE gconstructorCount #-}
instance GConstructorCount (C1 c f) where
  gconstructorCount _ = 1
  {-# INLINE gconstructorCount #-}

newtype ViaDuckEnum a = ViaDuckEnum a

instance (GDuckEnum (Rep a), Generic a, Typeable a, Show a, Enum a) => AppenderDuckValue (ViaDuckEnum a) where
    appenderDuckValue (ViaDuckEnum v) = Allocated $ do
        unionType <- cacheAppenderLogicalType (typeRep $ Proxy @a) (genumLogicalType $ genumType (Proxy @(Rep a)))
        c_duckdb_create_enum_value unionType (fromIntegral $ fromEnum v)
    appenderLogicalTypeUncached _ = genumLogicalType $ genumType (Proxy @(Rep a))
    appenderTypeName _ = DuckTypeName $ "ENUM(" <> Text.intercalate ", " ["\'" <> ctor <> "\'" | ctor <- genumType $ Proxy @(Rep a)] <> ")"

genumLogicalType :: [Text] -> Uncached DuckDBLogicalType
genumLogicalType els = Uncached $ do
    withMany Text.withCString els $ \namePtrs ->
        withArray namePtrs \nameArray ->
            c_duckdb_create_enum_type nameArray (fromIntegral $ length els)

class GDuckEnum (f :: Type -> Type) where
    genumValue :: Word64 -> f b -> Word64
    genumType :: Proxy f -> [Text]

instance (GDuckEnum a, GDuckEnum b) => GDuckEnum (a :+: b) where
    genumValue ix (L1 l) = genumValue ix l
    genumValue ix (R1 r) = genumValue (succ ix) r
    genumType _ = genumType (Proxy @a) <> genumType (Proxy @b)

instance (KnownSymbol conName) => GDuckEnum (C1 ('MetaCons conName foo bar) U1) where
    genumValue ix (M1 _) = ix
    genumType _ = [Text.pack $ symbolVal (Proxy @conName)]

instance (GDuckEnum a) => GDuckEnum (M1 D c a) where
    genumValue ix (M1 v) = genumValue ix v
    genumType _ = genumType (Proxy @a)

--

newtype ViaJSON a = ViaJSON a

instance (A.ToJSON a) => AppenderDuckValue (ViaJSON a) where
    appenderDuckValue (ViaJSON v) = appenderDuckValue $ A.toJSON v
    appenderLogicalTypeUncached _ = appenderLogicalTypeUncached (Proxy @A.Value)
    appenderTypeName _ = appenderTypeName (Proxy @A.Value)

--

newtype ViaShow a = ViaShow a

instance (Show a) => AppenderDuckValue (ViaShow a) where
    appenderDuckValue (ViaShow v) = appenderDuckValue $ Text.pack $ show v
    appenderLogicalTypeUncached _ = appenderLogicalTypeUncached (Proxy @Text)
    appenderTypeName _ = appenderTypeName (Proxy @Text)

---

class AppendTableRow (a :: Type) where
    appendDuckRow :: DuckDBAppender -> a -> IO ()
    default appendDuckRow :: (Generic a, GAppendTableRow (Rep a)) => DuckDBAppender -> a -> IO ()
    appendDuckRow app = gappendDuckRow app . from

    appendDuckRowSchema :: Proxy a -> [(Text, DuckTypeName)]
    default appendDuckRowSchema :: (Generic a, GAppendTableRow (Rep a)) => Proxy a -> [(Text, DuckTypeName)]
    appendDuckRowSchema _ = gappendDuckRowSchema (Proxy @(Rep a))

class GAppendTableRow (f :: Type -> Type) where
    gappendDuckRow :: DuckDBAppender -> f b -> IO ()
    gappendDuckRowSchema :: Proxy f -> [(Text, DuckTypeName)]

instance (AppenderDuckValue a, KnownSymbol selectorName) => GAppendTableRow (S1 ('MetaSel ('Just selectorName) q w e) (K1 i a)) where
    gappendDuckRowSchema _ = [(Text.pack $ symbolVal (Proxy @selectorName), appenderTypeName (Proxy @a))]
    gappendDuckRow app (M1 (K1 v)) = assertSuccess app $ appendDuckValue app v

instance (GAppendTableRow a, GAppendTableRow b) => GAppendTableRow (a :*: b) where
    gappendDuckRowSchema _ = gappendDuckRowSchema (Proxy @a) <> gappendDuckRowSchema (Proxy @b)
    gappendDuckRow app (a :*: b) = gappendDuckRow app a >> gappendDuckRow app b

instance (GAppendTableRow a) => GAppendTableRow (M1 C c a) where
    gappendDuckRowSchema _ = gappendDuckRowSchema (Proxy @a)
    gappendDuckRow app (M1 v) = gappendDuckRow app v

instance (GAppendTableRow a) => GAppendTableRow (M1 D c a) where
    gappendDuckRowSchema _ = gappendDuckRowSchema (Proxy @a)
    gappendDuckRow app (M1 v) = gappendDuckRow app v

assertSuccess :: (HasCallStack) => DuckDBAppender -> IO DuckDBState -> IO ()
assertSuccess app f = f >>= \case
  DuckDBSuccess -> pure ()
  _errorStatus -> do
        err <- c_duckdb_appender_error_data app >>= c_duckdb_error_data_message >>= peekCString
        throwIO $ SQLError ("duckdb-simple: appender error" <> Text.pack err) Nothing Nothing -- FIXME: proper error handling
{-# INLINE assertSuccess #-}
