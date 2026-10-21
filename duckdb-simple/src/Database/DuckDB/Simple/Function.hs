{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Database.DuckDB.Simple.Function
Description : Register scalar Haskell functions with DuckDB connections.

This module mirrors the high-level API provided by @sqlite-simple@ for user
defined functions, adapted to DuckDB's chunked execution model.  It allows
pure and 'IO'-based Haskell functions to be exposed to SQL while reusing the
existing field-decoding and result-marshalling machinery for arguments and
return values.
-}
module Database.DuckDB.Simple.Function (
    ScalarType,
    ScalarValue,
    FunctionArg (),
    FunctionResult (),
    Function (..),
    createFunction,
    createFunctionWithState,
    deleteFunction,
) where

import Control.Exception (
    SomeException,
    bracket,
    throwIO,
 )
import Control.Monad (forM, forM_, when)
import Data.Bits (clearBit, setBit)
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.Simple.Callback (attachOpaque, runCallback, transferCallbackState, withCallbackResources)
import Database.DuckDB.Simple.FromField (
    Field (..),
    FromField (..),
 )
import Database.DuckDB.Simple.Internal (
    Connection,
    destroyLogicalType,
    executeParsedResult,
    runInterruptibleQuery,
    withConnectionHandle,
    withParsedStatements,
 )
import Database.DuckDB.Simple.Materialize (prepareVectorReader)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, deRefStablePtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GHC.Float (float2Double)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)

data ScalarFunctionResources = ScalarFunctionResources
    { scalarFunctionExecPtr :: !DuckDBV2ScalarFunctionExecCallbackFn
    , scalarFunctionInitPtr :: !(Maybe DuckDBV2ScalarFunctionInitCallbackFn)
    }

-- | Tag DuckDB logical types we support for scalar return values.
data ScalarType
    = ScalarTypeBoolean
    | ScalarTypeBigInt
    | ScalarTypeUBigInt
    | ScalarTypeDouble
    | ScalarTypeVarchar

-- | Runtime representation of values returned to DuckDB.
data ScalarValue
    = ScalarNull
    | ScalarBoolean !Bool
    | ScalarInteger !Int64
    | ScalarUnsigned !Word64
    | ScalarDouble !Double
    | ScalarText !Text

-- | Class of scalar results that can be produced by user-defined functions.
class FunctionResult a where
    scalarReturnType :: Proxy a -> ScalarType
    toScalarValue :: a -> IO ScalarValue

instance FunctionResult Int where
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger (fromIntegral value))

instance FunctionResult Int16 where
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger (fromIntegral value))

instance FunctionResult Int32 where
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger (fromIntegral value))

instance FunctionResult Int64 where
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger value)

instance FunctionResult Word where
    scalarReturnType _ = ScalarTypeUBigInt
    toScalarValue value = pure (ScalarUnsigned (fromIntegral value))

instance FunctionResult Word16 where
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger (fromIntegral value))

instance FunctionResult Word32 where
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger (fromIntegral value))

instance FunctionResult Word64 where
    scalarReturnType _ = ScalarTypeUBigInt
    toScalarValue value = pure (ScalarUnsigned (fromIntegral value))

instance FunctionResult Double where
    scalarReturnType _ = ScalarTypeDouble
    toScalarValue value = pure (ScalarDouble value)

instance FunctionResult Float where
    scalarReturnType _ = ScalarTypeDouble
    toScalarValue value = pure (ScalarDouble (float2Double value))

instance FunctionResult Bool where
    scalarReturnType _ = ScalarTypeBoolean
    toScalarValue value = pure (ScalarBoolean value)

instance FunctionResult Text where
    scalarReturnType _ = ScalarTypeVarchar
    toScalarValue value = pure (ScalarText value)

instance FunctionResult String where
    scalarReturnType _ = ScalarTypeVarchar
    toScalarValue value = pure (ScalarText (Text.pack value))

instance (FunctionResult a) => FunctionResult (Maybe a) where
    scalarReturnType _ = scalarReturnType (Proxy :: Proxy a)
    toScalarValue Nothing = pure ScalarNull
    toScalarValue (Just value) = toScalarValue value

-- | Argument types supported by the scalar function machinery.
class FunctionArg a where
    argumentType :: Proxy a -> DuckDBV2LogicalTypeId

instance FunctionArg Int where
    argumentType _ = DuckDBV2LogicalTypeIdBigInt

instance FunctionArg Int16 where
    argumentType _ = DuckDBV2LogicalTypeIdSmallInt

instance FunctionArg Int32 where
    argumentType _ = DuckDBV2LogicalTypeIdInteger

instance FunctionArg Int64 where
    argumentType _ = DuckDBV2LogicalTypeIdBigInt

instance FunctionArg Word where
    argumentType _ = DuckDBV2LogicalTypeIdUBigInt

instance FunctionArg Word16 where
    argumentType _ = DuckDBV2LogicalTypeIdUSmallInt

instance FunctionArg Word32 where
    argumentType _ = DuckDBV2LogicalTypeIdUInteger

instance FunctionArg Word64 where
    argumentType _ = DuckDBV2LogicalTypeIdUBigInt

instance FunctionArg Double where
    argumentType _ = DuckDBV2LogicalTypeIdDouble

instance FunctionArg Float where
    argumentType _ = DuckDBV2LogicalTypeIdFloat

instance FunctionArg Bool where
    argumentType _ = DuckDBV2LogicalTypeIdBoolean

instance FunctionArg Text where
    argumentType _ = DuckDBV2LogicalTypeIdVarchar

instance FunctionArg String where
    argumentType _ = DuckDBV2LogicalTypeIdVarchar

instance (FunctionArg a) => FunctionArg (Maybe a) where
    argumentType _ = argumentType (Proxy :: Proxy a)

-- | Typeclass describing Haskell functions that can be exposed to DuckDB.
class Function a where
    argumentTypes :: Proxy a -> [DuckDBV2LogicalTypeId]
    returnType :: Proxy a -> ScalarType
    isVolatile :: Proxy a -> Bool
    applyFunction :: [Field] -> a -> IO ScalarValue

instance {-# OVERLAPPABLE #-} (FunctionResult a) => Function a where
    argumentTypes _ = []
    returnType _ = scalarReturnType (Proxy :: Proxy a)
    isVolatile _ = False
    applyFunction [] value = toScalarValue value
    applyFunction _ _ = throwIO (functionInvocationError (Text.pack "unexpected arguments supplied"))

instance {-# OVERLAPPING #-} (FunctionResult a) => Function (IO a) where
    argumentTypes _ = []
    returnType _ = scalarReturnType (Proxy :: Proxy a)
    isVolatile _ = True
    applyFunction [] action = action >>= toScalarValue
    applyFunction _ _ = throwIO (functionInvocationError (Text.pack "unexpected arguments supplied"))

instance {-# OVERLAPPABLE #-} (FromField a, FunctionArg a, Function r) => Function (a -> r) where
    argumentTypes _ = argumentType (Proxy :: Proxy a) : argumentTypes (Proxy :: Proxy r)
    returnType _ = returnType (Proxy :: Proxy r)
    isVolatile _ = isVolatile (Proxy :: Proxy r)
    applyFunction [] _ =
        throwIO (functionInvocationError (Text.pack "insufficient arguments supplied"))
    applyFunction (field : rest) fn =
        case fromField field of
            Errors err -> throwIO (argumentConversionError (fieldIndex field) err)
            Ok value -> applyFunction rest (fn value)

-- | Register a Haskell function under the supplied name.
createFunction :: forall f. (Function f) => Connection -> Text -> f -> IO ()
createFunction conn name fn =
    registerScalarFunction conn name (Proxy :: Proxy f) \allocate -> do
        scalarFunctionExecPtr <- DuckDBV2ScalarFunctionExecCallbackFn <$> allocate (toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux (scalarFunctionHandler fn)))
        pure ScalarFunctionResources{scalarFunctionExecPtr, scalarFunctionInitPtr = Nothing}

-- | Register a scalar function with per-worker thread-local state.
createFunctionWithState :: forall s f. (Function f) => Connection -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState conn name initState mkFn =
    registerScalarFunction conn name (Proxy :: Proxy f) \allocate -> do
        scalarFunctionExecPtr <- DuckDBV2ScalarFunctionExecCallbackFn <$> allocate (toFunPtr (DuckDBV2ScalarFunctionExecCallbackFn_Aux (scalarFunctionHandlerWithState mkFn)))
        initPtr <- DuckDBV2ScalarFunctionInitCallbackFn <$> allocate (toFunPtr (DuckDBV2ScalarFunctionInitCallbackFn_Aux (scalarFunctionInitHandler initState)))
        pure ScalarFunctionResources{scalarFunctionExecPtr, scalarFunctionInitPtr = Just initPtr}

-- | Configure a scalar function and transfer its callbacks to DuckDB.
registerScalarFunction :: (Function f) => Connection -> Text -> Proxy f -> ((forall a. IO (FunPtr a) -> IO (FunPtr a)) -> IO ScalarFunctionResources) -> IO ()
registerScalarFunction conn name proxy acquire = do
    when (Text.null name || Text.any (== '\0') name) $
        throwIO (functionInvocationError "duckdb-simple: invalid scalar function name")
    withConnectionHandle conn \connection ->
        bracket
            (withConnectionFactory connection (\nativeFactory -> output (c_duckdb_v2_scalar_function_create nativeFactory)))
            (destroy c_duckdb_v2_scalar_function_destroy)
            \scalarFun ->
                withCallbackResources
                    acquire
                    (attachOpaque (c_duckdb_v2_scalar_function_set_user_data scalarFun))
                    \ScalarFunctionResources{scalarFunctionExecPtr, scalarFunctionInitPtr} -> do
                        withIdentifier name $ checked . c_duckdb_v2_scalar_function_set_name scalarFun
                        signature <- output (c_duckdb_v2_scalar_function_get_signature scalarFun)
                        forM_ (zip [0 :: Int ..] (argumentTypes proxy)) \(index, dtype) ->
                            withLogicalType connection dtype \logical ->
                                withIdentifier (Text.pack ("arg" <> show index)) \argument ->
                                    checked (c_duckdb_v2_function_signature_add_parameter signature argument logical nullHandle DuckDBV2FunctionParameterKindPositionalOnly)
                        withLogicalType connection (duckTypeForScalar (returnType proxy)) $ \logical ->
                            checked (c_duckdb_v2_function_signature_set_return_type signature logical)
                        when (isVolatile proxy) $
                            checked (c_duckdb_v2_scalar_function_set_property scalarFun DuckDBV2FunctionPropertyStability DuckDBV2FunctionPropertyStabilityVolatile)
                        checked (c_duckdb_v2_scalar_function_set_property scalarFun DuckDBV2FunctionPropertyNullHandling DuckDBV2FunctionPropertyNullHandlingSpecial)
                        checked (c_duckdb_v2_scalar_function_set_property scalarFun DuckDBV2FunctionPropertyFallibility DuckDBV2FunctionPropertyFallibilityFallible)
                        checked (c_duckdb_v2_scalar_function_set_exec_callback scalarFun scalarFunctionExecPtr)
                        forM_ scalarFunctionInitPtr $ checked . c_duckdb_v2_scalar_function_set_init_callback scalarFun
                        checked (c_duckdb_v2_connection_register_scalar_function connection scalarFun)

{- | Run @DROP FUNCTION IF EXISTS@ and report native errors.
DuckDB does not permit deletion of functions registered through the C API.
-}
deleteFunction :: Connection -> Text -> IO ()
deleteFunction conn name = do
    let dropQuery = Query ("DROP FUNCTION IF EXISTS " <> qualifyIdentifier name)
    withParsedStatements conn dropQuery $ \parsed ->
        forM_ parsed $ \statement ->
            bracket
                (executeParsedResult conn dropQuery statement)
                (destroy c_duckdb_v2_result_destroy)
                \result -> do
                    _ <- runInterruptibleQuery conn (output (c_duckdb_v2_result_drain result))
                    pure ()

withLogicalType :: DuckDBV2ConnectionHandle -> DuckDBV2LogicalTypeId -> (DuckDBV2LogicalTypeHandle -> IO a) -> IO a
withLogicalType connection dtype =
    bracket
        (withConnectionFactory connection (\nativeFactory -> output (c_duckdb_v2_factory_create_type_from_id nativeFactory dtype (ConstPtr nullPtr) (ConstPtr nullPtr) 0)))
        destroyLogicalType

duckTypeForScalar :: ScalarType -> DuckDBV2LogicalTypeId
duckTypeForScalar = \case
    ScalarTypeBoolean -> DuckDBV2LogicalTypeIdBoolean
    ScalarTypeBigInt -> DuckDBV2LogicalTypeIdBigInt
    ScalarTypeUBigInt -> DuckDBV2LogicalTypeIdUBigInt
    ScalarTypeDouble -> DuckDBV2LogicalTypeIdDouble
    ScalarTypeVarchar -> DuckDBV2LogicalTypeIdVarchar

scalarFunctionHandler :: forall f. (Function f) => f -> DuckDBV2ScalarFunctionExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
scalarFunctionHandler fn info context err = runCallback err (executeScalarFunction fn info context)

executeScalarFunction :: forall f. (Function f) => f -> DuckDBV2ScalarFunctionExecInfoHandle -> DuckDBV2ContextHandle -> IO ()
executeScalarFunction fn info context = do
    let expected = length (argumentTypes (Proxy :: Proxy f))
    columnCount <- allocaArray 4 $ \counts -> do
        checked (c_duckdb_v2_scalar_function_exec_get_arg_count info counts (advancePtr counts 1) (advancePtr counts 2) (advancePtr counts 3))
        fromIntegral . sum <$> peekArray 4 counts
    when (columnCount /= expected) $
        throwIO
            ( functionInvocationError
                (Text.pack ("duckdb-simple: function expected " <> show expected <> " arguments but received " <> show columnCount))
            )
    rawRowCount <- output (c_duckdb_v2_scalar_function_exec_get_row_count info)
    let rowCount = fromIntegral rawRowCount :: Int
    readers <- mapM (makeColumnReader info) [0 .. expected - 1]
    rows <- forM [0 .. rowCount - 1] $ \row -> forM readers ($ row)
    results <- mapM (`applyFunction` fn) rows
    outVec <- output (c_duckdb_v2_scalar_function_exec_get_result info)
    writeResults context (returnType (Proxy :: Proxy f)) results outVec

scalarFunctionHandlerWithState :: forall s f. (Function f) => (s -> f) -> DuckDBV2ScalarFunctionExecInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
scalarFunctionHandlerWithState mkFn info context err = runCallback err do
    statePtr <- output (c_duckdb_v2_scalar_function_exec_get_init_data info)
    when (statePtr == nullPtr) $
        throwIO (functionInvocationError "duckdb-simple: scalar function state was not initialised")
    state <- deRefStablePtr (castPtrToStablePtr (castPtr statePtr) :: StablePtr s)
    executeScalarFunction (mkFn state) info context

scalarFunctionInitHandler :: IO s -> DuckDBV2ScalarFunctionInitInfoHandle -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
scalarFunctionInitHandler initState info _context err = runCallback err do
    state <- initState
    transferCallbackState (attachOpaque (c_duckdb_v2_scalar_function_init_set_init_data info)) state

type ColumnReader = Int -> IO Field

makeColumnReader :: DuckDBV2ScalarFunctionExecInfoHandle -> Int -> IO ColumnReader
makeColumnReader info columnIndex = do
    readValue <- output (c_duckdb_v2_scalar_function_exec_get_arg info (fromIntegral columnIndex)) >>= prepareVectorReader (\_ -> throwIO (functionInvocationError "duckdb-simple: scalar function arguments must use ScalarType"))
    let name = Text.pack ("arg" <> show columnIndex)
    pure \rowIdx -> do
        value <- readValue rowIdx
        pure
            Field
                { fieldName = name
                , fieldIndex = columnIndex
                , fieldValue = value
                }

writeResults :: DuckDBV2ContextHandle -> ScalarType -> [ScalarValue] -> DuckDBV2VectorHandle -> IO ()
writeResults context resultType values outVec = do
    checked (c_duckdb_v2_vector_set_size outVec (fromIntegral (length values)))
    checked (c_duckdb_v2_vector_flatten outVec)
    dataPtr <- output (c_duckdb_v2_vector_get_data_mutable outVec)
    validityPtr <- output (c_duckdb_v2_vector_flat_get_validity_mutable outVec)
    forM_ (zip [0 ..] values) \(idx, val) ->
        case (resultType, val) of
            (_, ScalarNull) ->
                markInvalid validityPtr idx
            (ScalarTypeBoolean, ScalarBoolean flag) -> do
                markValid validityPtr idx
                pokeElemOff (castPtr dataPtr :: Ptr Word8) idx (if flag then 1 else 0)
            (ScalarTypeBigInt, ScalarInteger intval) -> do
                markValid validityPtr idx
                pokeElemOff (castPtr dataPtr :: Ptr Int64) idx intval
            (ScalarTypeUBigInt, ScalarUnsigned intval) -> do
                markValid validityPtr idx
                pokeElemOff (castPtr dataPtr :: Ptr Word64) idx intval
            (ScalarTypeDouble, ScalarDouble dbl) -> do
                markValid validityPtr idx
                pokeElemOff (castPtr dataPtr :: Ptr Double) idx dbl
            (ScalarTypeVarchar, ScalarText txt) ->
                bracket
                    (withContextFactory context \factory -> withText txt (output . c_duckdb_v2_value_create_varchar factory))
                    (destroy c_duckdb_v2_value_destroy)
                    \value -> checked (c_duckdb_v2_vector_set_value outVec (fromIntegral idx) value)
            _ ->
                throwIO
                    $ functionInvocationError
                    $ Text.pack "duckdb-simple: result type mismatch when materialising scalar function output"

markInvalid :: Ptr Word64 -> Int -> IO ()
markInvalid validity idx = do
    let (wordIndex, bitIndex) = idx `divMod` 64
    word <- peekElemOff validity wordIndex
    pokeElemOff validity wordIndex (clearBit word bitIndex)

markValid :: Ptr Word64 -> Int -> IO ()
markValid validity idx = do
    let (wordIndex, bitIndex) = idx `divMod` 64
    word <- peekElemOff validity wordIndex
    pokeElemOff validity wordIndex (setBit word bitIndex)

argumentConversionError :: Int -> [SomeException] -> SQLError
argumentConversionError idx err =
    let message =
            Text.concat
                [ Text.pack "duckdb-simple: unable to convert argument #"
                , Text.pack (show (idx + 1))
                , Text.pack ": "
                , Text.pack (show err)
                ]
     in functionInvocationError message

functionInvocationError :: Text -> SQLError
functionInvocationError message =
    SQLError
        { sqlErrorMessage = message
        , sqlErrorType = Nothing
        , sqlErrorQuery = Nothing
        }

qualifyIdentifier :: Text -> Text
qualifyIdentifier rawName =
    let parts = Text.splitOn "." rawName
     in Text.intercalate (Text.pack ".") (map quoteIdent parts)

quoteIdent :: Text -> Text
quoteIdent ident =
    Text.concat
        [ Text.pack "\""
        , Text.replace (Text.pack "\"") (Text.pack "\"\"") ident
        , Text.pack "\""
        ]
