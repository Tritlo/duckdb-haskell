{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Database.DuckDB.Simple.Function.Internal
-- Description : Share typed scalar registration for connections and extensions.
--
-- The cabal file lists this hidden module in @other-modules@.
-- "Database.DuckDB.Simple.Function" exports the forms for a connection.
-- The forms that take a registration target,
-- 'createFunctionOn' and 'createFunctionWithStateOn', are for the module of
-- the loadable extensions. Both forms use the same argument conversion, result
-- conversion, and callback resources.
module Database.DuckDB.Simple.Function.Internal (
  ScalarValue,
  FunctionResult (..),
  Function (..),
  createFunctionOn,
  createFunctionWithStateOn,
  prepareResultWriter,
) where

import Control.Exception (
  SomeException,
  bracket,
  throwIO,
 )
import Control.Monad (forM, forM_, when)
import Data.Bits (clearBit, setBit)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Callback (
  Allocator,
  RegistrationTarget,
  attachOpaque,
  install,
  installAll,
  runCallback,
  targetCastFactory,
  targetConfig,
  transferCallbackState,
  withCallbackResources,
  withRegistrationFactory,
  withTarget,
 )
import Database.DuckDB.Simple.Element (vectorElementType)
import Database.DuckDB.Simple.FromField (
  Field,
  FromField (..),
 )
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (..), TypeDeclaration, logicalTypeSQL)
import Database.DuckDB.Simple.Materialize (ReadFactory, callbackReadFactory)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..), ToDuckValue (..))
import Database.DuckDB.Simple.Typed (argumentName, checkArgumentType, conversionMessage, prepareArgumentReader, rowDeclarations)
import Database.DuckDB.Simple.V2.Registration (withTypeDeclaration)
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, deRefStablePtr)
import Foreign.Storable (Storable, peekElemOff, poke, pokeElemOff)
import GHC.Records (getField)
import HsBindgen.Runtime.Marshal (staticSizeOf)

{- Note [Result writers]
~~~~~~~~~~~~~~~~~~~~~~~~
A scalar function and a typed aggregate function write their results with
writeScalarValue. The FunctionResult instance of the result type selects one of
two paths.

- A raw result (rawResult gives Just): Bool, Int, Int8 to Int64, Word, Word8 to
  Word64, Float, Double, Text, String, and Maybe of one of them. The writer
  writes the value to the memory of the vector, with no native call. A VARCHAR
  value of more than 12 bytes needs 2 native calls for the arena of the vector
  (writeArenaText).
- A result of each other type (rawResult gives Nothing). toDuckValue makes a
  native value with the factory of the callback context, and
  vector_set_value writes it. DuckDB casts the value to the type of the
  vector. Each such row needs at least 3 native calls: the value, the write,
  and the destroy. The writer borrows the factory one time for each call, only
  when a row needs it.

The writer reads the logical type of the vector one time for each call
(prepareResultWriter), with 3 native calls. Each raw value must have the type
of the vector, else the writer raises an error. Thus a raw write has the width
of the vector. The writer does not read the size of the vector: in an ordered
window, the result vector of a finalize call has the size 0.

The finalize callback of an aggregate function has no context, so it has no
factory. Thus an aggregate result must be a raw result. createAggregateOn in
"Database.DuckDB.Simple.Typed.Aggregate" checks this at registration.
-}

-- | The value of one result row. See Note [Result writers].
data ScalarValue
  = ScalarNull
  | ScalarBoolean Bool
  | ScalarInt8 Int8
  | ScalarInt16 Int16
  | ScalarInt32 Int32
  | ScalarInt64 Int64
  | ScalarWord8 Word8
  | ScalarWord16 Word16
  | ScalarWord32 Word32
  | ScalarWord64 Word64
  | ScalarFloat Float
  | ScalarDouble Double
  | ScalarText Text
  | ScalarNative (DuckDBV2FactoryHandle -> IO DuckDBV2ValueHandle)

-- | A result type of a scalar function or of a typed aggregate function. Each
-- type with a 'ToDuckValue' instance is a result type. Its SQL type is its
-- 'duckdbTypeDeclaration'. A newtype over a type with a raw writer gets that
-- writer with @deriving newtype (FunctionResult)@. See Note [Result writers].
class (ToDuckValue a) => FunctionResult a where
  -- | The raw value of a result, or 'Nothing' when 'toDuckValue' writes the
  -- result.
  rawResult :: Maybe (a -> ScalarValue)
  rawResult = Nothing

-- | A result that 'toDuckValue' writes.
instance {-# OVERLAPPABLE #-} (ToDuckValue a) => FunctionResult a

instance FunctionResult Bool where
  rawResult = Just ScalarBoolean

instance FunctionResult Int where
  rawResult = Just (ScalarInt64 . fromIntegral)

instance FunctionResult Int8 where
  rawResult = Just ScalarInt8

instance FunctionResult Int16 where
  rawResult = Just ScalarInt16

instance FunctionResult Int32 where
  rawResult = Just ScalarInt32

instance FunctionResult Int64 where
  rawResult = Just ScalarInt64

instance FunctionResult Word where
  rawResult = Just (ScalarWord64 . fromIntegral)

instance FunctionResult Word8 where
  rawResult = Just ScalarWord8

instance FunctionResult Word16 where
  rawResult = Just ScalarWord16

instance FunctionResult Word32 where
  rawResult = Just ScalarWord32

instance FunctionResult Word64 where
  rawResult = Just ScalarWord64

instance FunctionResult Float where
  rawResult = Just ScalarFloat

instance FunctionResult Double where
  rawResult = Just ScalarDouble

instance FunctionResult Text where
  rawResult = Just ScalarText

instance FunctionResult String where
  rawResult = Just (ScalarText . Text.pack)

instance (FunctionResult a) => FunctionResult (Maybe a) where
  rawResult = maybe ScalarNull <$> (rawResult :: Maybe (a -> ScalarValue))

-- | Give the value of one result row. See Note [Result writers].
scalarValue :: (FunctionResult a) => a -> ScalarValue
scalarValue value = case rawResult of
  Just raw -> raw value
  Nothing -> ScalarNative (`toDuckValue` value)

-- | A Haskell function that DuckDB can call as a scalar function: a result
-- value, an 'IO' action that gives a result, or a function of one more
-- argument. 'ScalarValue' is abstract, so an instance outside the library can
-- only use 'applyFunction' of another instance.
class Function a where
  -- | The SQL type of each argument, in order, or the reason why an argument
  -- type has no SQL type. See Note [Type declarations] in
  -- "Database.DuckDB.Simple.ToField".
  argumentTypes :: Proxy a -> [Either Text TypeDeclaration]

  -- | The SQL type of the result, or the reason why the result type has no SQL
  -- type.
  returnType :: Proxy a -> Either Text TypeDeclaration

  -- | 'True' for an 'IO' result. The registration then sets the stability of
  -- the function to VOLATILE: the result can be different for each row, and
  -- DuckDB calls the function one time for each row. 'False' keeps the
  -- default stability CONSISTENT: the function always gives the same result
  -- for the same arguments. DuckDB then calls a function with constant
  -- arguments one time for each call site, when it prepares the statement. It
  -- calls the function one time for each row in each other case, also for an
  -- argument with few different values.
  isVolatile :: Proxy a -> Bool

  -- | Convert the fields of one row to the arguments, and give the result of
  -- the function. A field that does not convert raises an error that names
  -- the argument.
  applyFunction :: [Field] -> a -> IO ScalarValue

-- | A result value: a function with no argument. It is not volatile.
instance {-# OVERLAPPABLE #-} (FunctionResult a) => Function a where
  argumentTypes _ = []
  returnType _ = duckdbTypeDeclaration (Proxy @a)
  isVolatile _ = False
  applyFunction [] value = pure (scalarValue value)
  applyFunction _ _ = throwInternalError (Text.pack "unexpected arguments supplied")

-- | An 'IO' action that gives a result: a function with no argument. It is
-- volatile, so DuckDB runs the action one time for each row.
instance {-# OVERLAPPING #-} (FunctionResult a) => Function (IO a) where
  argumentTypes _ = []
  returnType _ = duckdbTypeDeclaration (Proxy @a)
  isVolatile _ = True
  applyFunction [] action = scalarValue <$> action
  applyFunction _ _ = throwInternalError (Text.pack "unexpected arguments supplied")

-- | A function of one more argument. The argument has the SQL type of
-- 'duckdbTypeDeclaration'. The function is volatile when its result is
-- volatile. See Note [Type declarations] in "Database.DuckDB.Simple.ToField".
instance {-# OVERLAPPABLE #-} (FromField a, DuckDBColumnType a, Function r) => Function (a -> r) where
  argumentTypes _ = duckdbTypeDeclaration (Proxy @a) : argumentTypes (Proxy @r)
  returnType _ = returnType (Proxy @r)
  isVolatile _ = isVolatile (Proxy @r)
  applyFunction [] _ =
    throwInternalError (Text.pack "insufficient arguments supplied")
  applyFunction (field : rest) fn =
    case fromField field of
      Errors err -> throwIO (argumentConversionError err)
      Ok value -> applyFunction rest (fn value)

-- | Register a typed function on a connection or a loading extension.
createFunctionOn :: forall f. (Function f) => RegistrationTarget -> Text -> f -> IO ()
createFunctionOn target name fn =
  registerScalarFunction target name (Proxy @f) \casts allocate function ->
    [ install
        allocate
        DuckDBV2ScalarFunctionExecCallbackFn
        (DuckDBV2ScalarFunctionExecCallbackFn_Aux (scalarFunctionHandler casts fn))
        (c_duckdb_v2_scalar_function_set_exec_callback function)
    ]

-- | Register a typed function with per-worker state on a connection or extension.
createFunctionWithStateOn :: forall s f. (Function f) => RegistrationTarget -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithStateOn target name initState mkFn =
  registerScalarFunction target name (Proxy @f) \casts allocate function ->
    [ install
        allocate
        DuckDBV2ScalarFunctionExecCallbackFn
        (DuckDBV2ScalarFunctionExecCallbackFn_Aux (scalarFunctionHandlerWithState casts mkFn))
        (c_duckdb_v2_scalar_function_set_exec_callback function)
    , install
        allocate
        DuckDBV2ScalarFunctionInitCallbackFn
        (DuckDBV2ScalarFunctionInitCallbackFn_Aux (scalarFunctionInitHandler initState))
        (c_duckdb_v2_scalar_function_set_init_callback function)
    ]

-- | Configure a scalar function and transfer its callbacks to DuckDB. An
-- argument type or a result type with no declaration, or an argument type that
-- is or contains VARIANT, raises an error at registration. The check of the
-- name and the declarations of the arguments and the result come before the
-- read of the state of the target, so their input errors win over a closed
-- connection. The VARIANT check ('checkArgumentType') needs the resolved type
-- of the argument, so it comes after the read of the state, and a closed
-- connection wins over it. The last argument gives the install actions of the
-- callbacks for the builder, from the factory of 'targetCastFactory'. Their
-- setters run after the properties. See 'installAll'.
registerScalarFunction
  :: (Function f)
  => RegistrationTarget
  -> Text
  -> Proxy f
  -> (Maybe DuckDBV2FactoryHandle -> Allocator -> DuckDBV2ScalarFunctionHandle -> [IO (IO ())])
  -> IO ()
registerScalarFunction target name proxy installers = do
  when (Text.null name || Text.any (== '\0') name) $
    throwInvalidArgument "invalid scalar function name"
  -- See Note [Type declarations] in "Database.DuckDB.Simple.ToField".
  parameters <- rowDeclarations name ["argument " <> argumentName index | index <- [0 ..]] (argumentTypes proxy)
  [result] <- rowDeclarations name ["result"] [returnType proxy]
  casts <- targetCastFactory target
  -- withTarget keeps its factory private. Borrow another one for the signature types.
  withRegistrationFactory target \factory ->
    withTarget
      target
      c_duckdb_v2_scalar_function_create
      c_duckdb_v2_scalar_function_destroy
      c_duckdb_v2_connection_register_scalar_function
      c_duckdb_v2_extension_register_scalar_function
      \scalarFun register ->
        withCallbackResources
          (targetConfig target)
          (\allocate -> installAll (installers casts allocate scalarFun))
          (attachOpaque (c_duckdb_v2_scalar_function_set_user_data scalarFun))
          \installCallbacks -> do
            withIdentifier name $ checked . c_duckdb_v2_scalar_function_set_name scalarFun
            signature <- output (c_duckdb_v2_scalar_function_get_signature scalarFun)
            forM_ (zip [0 :: Int ..] parameters) \(index, dtype) ->
              withTypeDeclaration factory dtype \logical -> do
                checkArgumentType True name ("argument " <> argumentName index) logical
                withIdentifier (argumentName index) \argument ->
                  checked (c_duckdb_v2_function_signature_add_parameter signature argument logical nullHandle DuckDBV2FunctionParameterKindPositionalOnly)
            withTypeDeclaration factory result $ checked . c_duckdb_v2_function_signature_set_return_type signature
            when (isVolatile proxy) $
              checked (c_duckdb_v2_scalar_function_set_property scalarFun DuckDBV2FunctionPropertyStability DuckDBV2FunctionPropertyStabilityVolatile)
            checked
              (c_duckdb_v2_scalar_function_set_property scalarFun DuckDBV2FunctionPropertyNullHandling DuckDBV2FunctionPropertyNullHandlingSpecial)
            checked (c_duckdb_v2_scalar_function_set_property scalarFun DuckDBV2FunctionPropertyFallibility DuckDBV2FunctionPropertyFallibilityFallible)
            installCallbacks
            register

scalarFunctionHandler
  :: forall f
   . (Function f)
  => Maybe DuckDBV2FactoryHandle -> f -> DuckDBV2ScalarFunctionExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
scalarFunctionHandler casts fn info context err = runCallback err (executeScalarFunction casts fn info context)

-- | Run one call of a scalar function.
executeScalarFunction
  :: forall f. (Function f) => Maybe DuckDBV2FactoryHandle -> f -> DuckDBV2ScalarFunctionExecInfoHandle -> DuckDBV2ContextHandle -> IO ()
executeScalarFunction casts fn info context = withContextFactory context \contextFactory -> do
  let expected = length (argumentTypes (Proxy @f))
  columnCount <- allocaArray 4 $ \counts -> do
    checked (c_duckdb_v2_scalar_function_exec_get_arg_count info counts (advancePtr counts 1) (advancePtr counts 2) (advancePtr counts 3))
    fromIntegral . sum <$> peekArray 4 counts
  when (columnCount /= expected) $
    throwInternalError (Text.pack ("function expected " <> show expected <> " arguments but received " <> show columnCount))
  rawRowCount <- output (c_duckdb_v2_scalar_function_exec_get_row_count info)
  let rowCount = fromIntegral rawRowCount :: Int
  readers <- mapM (makeColumnReader (callbackReadFactory casts contextFactory) info) [0 .. expected - 1]
  rows <- forM [0 .. rowCount - 1] $ \row -> forM readers ($ row)
  results <- mapM (`applyFunction` fn) rows
  outVec <- output (c_duckdb_v2_scalar_function_exec_get_result info)
  writeResults context results outVec

scalarFunctionHandlerWithState
  :: forall s f
   . (Function f)
  => Maybe DuckDBV2FactoryHandle
  -> (s -> f)
  -> DuckDBV2ScalarFunctionExecInfoHandle
  -> DuckDBV2ContextHandle
  -> Ptr DuckDBV2ErrorInfoHandle
  -> IO ()
scalarFunctionHandlerWithState casts mkFn info context err = runCallback err do
  statePtr <- output (c_duckdb_v2_scalar_function_exec_get_init_data info)
  when (statePtr == nullPtr) $
    throwInternalError "scalar function state was not initialised"
  -- The cast has no type check, and it is safe. createFunctionWithStateOn has
  -- one signature for initState :: IO s and mkFn :: s -> f. Thus the init
  -- callback stores a StablePtr of the same type s that this cast gives.
  state <- deRefStablePtr (castPtrToStablePtr (castPtr statePtr) :: StablePtr s)
  executeScalarFunction casts (mkFn state) info context

scalarFunctionInitHandler :: IO s -> DuckDBV2ScalarFunctionInitInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
scalarFunctionInitHandler initState info _context err = runCallback err do
  state <- initState
  transferCallbackState (attachOpaque (c_duckdb_v2_scalar_function_init_set_init_data info)) state

-- | Prepare the reader of one argument. A GEOMETRY value reads with the
-- factory, at each depth of the type.
makeColumnReader :: ReadFactory -> DuckDBV2ScalarFunctionExecInfoHandle -> Int -> IO (Int -> IO Field)
makeColumnReader factory info columnIndex =
  output (c_duckdb_v2_scalar_function_exec_get_arg info (fromIntegral columnIndex))
    >>= prepareArgumentReader (Just factory) columnIndex

-- | Write the results of one call to the result vector. Set the size, then
-- flatten, then read the pointers. A row of a result that is not raw borrows
-- the factory of the context. See Note [Vector flatten] in
-- "Database.DuckDB.Simple.Materialize" and Note [Result writers].
writeResults :: DuckDBV2ContextHandle -> [ScalarValue] -> DuckDBV2VectorHandle -> IO ()
writeResults context values outVec = do
  checked (c_duckdb_v2_vector_set_size outVec (fromIntegral (length values)))
  checked (c_duckdb_v2_vector_flatten outVec)
  let native = \case
        ScalarNative _ -> True
        _ -> False
      withFactory action
        | any native values = withContextFactory context (action . Just)
        | otherwise = action Nothing
  withFactory \factory -> do
    write <- prepareResultWriter factory outVec
    forM_ (zip [0 ..] values) (uncurry write)

-- | Prepare a writer for the rows of a flat result vector. The writer does not
-- change the size of the vector. It reads the logical type of the vector one
-- time, and it raises an error for a raw value of another type. It writes a
-- value of 'ScalarNative' with the factory, and raises an error without a
-- factory. A vector that is not flat raises an error. See
-- Note [Result writers] and Note [Vector flatten] in
-- "Database.DuckDB.Simple.Materialize".
prepareResultWriter :: Maybe DuckDBV2FactoryHandle -> DuckDBV2VectorHandle -> IO (Int -> ScalarValue -> IO ())
prepareResultWriter factory vector = do
  kind <- output (c_duckdb_v2_vector_get_vector_type vector)
  when (kind /= DuckDBV2VectorTypeFlat) $
    throwInternalError (Text.pack ("the result vector is not flat: " <> show kind))
  vectorType <- vectorElementType vector
  dataPtr <- output (c_duckdb_v2_vector_get_data_mutable vector)
  validityPtr <- output (c_duckdb_v2_vector_flat_get_validity_mutable vector)
  pure (writeScalarValue factory vector vectorType dataPtr validityPtr)

-- | Write one value at a row of a flat vector. The data and validity pointers
-- belong to the vector. A raw value must have the logical type of the vector.
-- See Note [Result writers].
writeScalarValue
  :: Maybe DuckDBV2FactoryHandle -> DuckDBV2VectorHandle -> DuckDBV2LogicalTypeId -> Ptr Void -> Ptr Word64 -> Int -> ScalarValue -> IO ()
writeScalarValue factory vector vectorType dataPtr validityPtr idx = \case
  ScalarNull -> markInvalid validityPtr idx
  -- A BOOLEAN value has one byte.
  ScalarBoolean flag -> raw DuckDBV2LogicalTypeIdBoolean (if flag then 1 else 0 :: Word8)
  ScalarInt8 value -> raw DuckDBV2LogicalTypeIdTinyInt value
  ScalarInt16 value -> raw DuckDBV2LogicalTypeIdSmallInt value
  ScalarInt32 value -> raw DuckDBV2LogicalTypeIdInteger value
  ScalarInt64 value -> raw DuckDBV2LogicalTypeIdBigInt value
  ScalarWord8 value -> raw DuckDBV2LogicalTypeIdUTinyInt value
  ScalarWord16 value -> raw DuckDBV2LogicalTypeIdUSmallInt value
  ScalarWord32 value -> raw DuckDBV2LogicalTypeIdUInteger value
  ScalarWord64 value -> raw DuckDBV2LogicalTypeIdUBigInt value
  ScalarFloat value -> raw DuckDBV2LogicalTypeIdFloat value
  ScalarDouble value -> raw DuckDBV2LogicalTypeIdDouble value
  ScalarText text -> do
    expect DuckDBV2LogicalTypeIdVarchar
    markValid validityPtr idx
    writeArenaText vector dataPtr idx text
  ScalarNative make -> case factory of
    Just valueFactory -> bracket (make valueFactory) destroyValue (checked . c_duckdb_v2_vector_set_value vector (fromIntegral idx))
    Nothing -> throwInternalError "the result writer has no factory for a value of this result type"
  where
    raw :: (Storable s) => DuckDBV2LogicalTypeId -> s -> IO ()
    raw expected value = do
      expect expected
      markValid validityPtr idx
      pokeElemOff (castPtr dataPtr) idx value
    expect expected = when (vectorType /= expected) do
      actualName <- logicalTypeSQL . LogicalTypeScalar <$> logicalTypeIdFromNative vectorType
      expectedName <- logicalTypeSQL . LogicalTypeScalar <$> logicalTypeIdFromNative expected
      throwInternalError ("the result vector has the type " <> actualName <> ", but the result value has the type " <> expectedName)

-- | Write a VARCHAR value at a row of a flat vector without a factory. A value
-- of more than @DUCKDB_V2_BYTES_INLINE_LENGTH@ (12) bytes goes to the arena of
-- the vector. See Note [Vector memory layouts] in "Database.DuckDB.Simple.Element".
writeArenaText :: DuckDBV2VectorHandle -> Ptr Void -> Int -> Text -> IO ()
writeArenaText vector dataPtr idx txt = do
  -- Write each field through its generated field pointer, as chunkDecodeBlob
  -- in "Database.DuckDB.Simple.Element" reads it.
  let len = TextForeign.lengthWord8 txt
      slot = advancePtr @DuckDBV2Bytes (castPtr dataPtr) idx
      inlined = getField @"inlined" (getField @"value" slot)
      pointer = getField @"pointer" (getField @"value" slot)
  when (len > fromIntegral (maxBound :: Word32)) $
    throwIO (functionInvocationError "duckdb-simple: a VARCHAR result is longer than 4 GiB")
  -- DuckDB compares the unused inline bytes. They must be zero.
  fillBytes slot 0 (staticSizeOf (Proxy @DuckDBV2Bytes))
  if len <= fromIntegral dUCKDB_V2_BYTES_INLINE_LENGTH
    then do
      poke (getField @"length" inlined) (fromIntegral len)
      TextForeign.unsafeCopyToPtr txt (castPtr (getField @"inlined" inlined))
    else do
      arena <- output (c_duckdb_v2_vector_get_arena vector)
      bytes <- output (c_duckdb_v2_arena_allocate arena (fromIntegral len))
      TextForeign.unsafeCopyToPtr txt bytes
      poke (getField @"length" pointer) (fromIntegral len)
      copyBytes (castPtr (getField @"prefix" pointer)) bytes 4
      poke (getField @"ptr" pointer) (castPtr bytes)

-- | Clear the validity bit of a row. See Note [Vector memory layouts] in
-- "Database.DuckDB.Simple.Element".
markInvalid :: Ptr Word64 -> Int -> IO ()
markInvalid validity idx = do
  let (wordIndex, bitIndex) = idx `divMod` 64
  word <- peekElemOff validity wordIndex
  pokeElemOff validity wordIndex (clearBit word bitIndex)

-- | Set the validity bit of a row. See Note [Vector memory layouts] in
-- "Database.DuckDB.Simple.Element".
markValid :: Ptr Word64 -> Int -> IO ()
markValid validity idx = do
  let (wordIndex, bitIndex) = idx `divMod` 64
  word <- peekElemOff validity wordIndex
  pokeElemOff validity wordIndex (setBit word bitIndex)

argumentConversionError :: [SomeException] -> SQLError
argumentConversionError err = functionInvocationError (Text.pack ("duckdb-simple: " <> conversionMessage err))

functionInvocationError :: Text -> SQLError
functionInvocationError message =
  SQLError
    { sqlErrorMessage = message
    , sqlErrorType = Nothing
    , sqlErrorQuery = Nothing
    , sqlErrorBody = Nothing
    }
