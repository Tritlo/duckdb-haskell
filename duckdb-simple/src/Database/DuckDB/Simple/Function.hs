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
    try,
 )
import Control.Monad (forM, forM_, when)
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as TextForeign
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.Callback (runCallback, transferCallbackState, withCallbackResources)
import Database.DuckDB.Simple.FromField (
    Field (..),
    FromField (..),
 )
import Database.DuckDB.Simple.Internal (
    Connection,
    Query (..),
    SQLError (..),
    destroyLogicalType,
    withConnectionHandle,
    withQueryCString,
    withResult,
 )
import Database.DuckDB.Simple.Materialize (prepareVectorReader)
import Database.DuckDB.Simple.Ok (Ok (..))
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, deRefStablePtr)
import Foreign.Storable (poke, pokeElemOff)
import GHC.Float (float2Double)
import HsBindgen.Runtime.Support.FunPtr (toFunPtr)

data ScalarFunctionResources = ScalarFunctionResources
    { scalarFunctionExecPtr :: !DuckDBScalarFunctionFun
    , scalarFunctionInitPtr :: !(Maybe DuckDBScalarFunctionInitFun)
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
    argumentType :: Proxy a -> DUCKDB_TYPE

instance FunctionArg Int where
    argumentType _ = DUCKDB_TYPE_BIGINT

instance FunctionArg Int16 where
    argumentType _ = DUCKDB_TYPE_SMALLINT

instance FunctionArg Int32 where
    argumentType _ = DUCKDB_TYPE_INTEGER

instance FunctionArg Int64 where
    argumentType _ = DUCKDB_TYPE_BIGINT

instance FunctionArg Word where
    argumentType _ = DUCKDB_TYPE_UBIGINT

instance FunctionArg Word16 where
    argumentType _ = DUCKDB_TYPE_USMALLINT

instance FunctionArg Word32 where
    argumentType _ = DUCKDB_TYPE_UINTEGER

instance FunctionArg Word64 where
    argumentType _ = DUCKDB_TYPE_UBIGINT

instance FunctionArg Double where
    argumentType _ = DUCKDB_TYPE_DOUBLE

instance FunctionArg Float where
    argumentType _ = DUCKDB_TYPE_FLOAT

instance FunctionArg Bool where
    argumentType _ = DUCKDB_TYPE_BOOLEAN

instance FunctionArg Text where
    argumentType _ = DUCKDB_TYPE_VARCHAR

instance FunctionArg String where
    argumentType _ = DUCKDB_TYPE_VARCHAR

instance (FunctionArg a) => FunctionArg (Maybe a) where
    argumentType _ = argumentType (Proxy :: Proxy a)

-- | Typeclass describing Haskell functions that can be exposed to DuckDB.
class Function a where
    argumentTypes :: Proxy a -> [DUCKDB_TYPE]
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
        scalarFunctionExecPtr <- DuckDBScalarFunctionFun <$> allocate (toFunPtr (DuckDBScalarFunctionFun_Aux (scalarFunctionHandler fn)))
        pure ScalarFunctionResources{scalarFunctionExecPtr, scalarFunctionInitPtr = Nothing}

-- | Register a scalar function with per-worker thread-local state.
createFunctionWithState :: forall s f. (Function f) => Connection -> Text -> IO s -> (s -> f) -> IO ()
createFunctionWithState conn name initState mkFn =
    registerScalarFunction conn name (Proxy :: Proxy f) \allocate -> do
        scalarFunctionExecPtr <- DuckDBScalarFunctionFun <$> allocate (toFunPtr (DuckDBScalarFunctionFun_Aux (scalarFunctionHandlerWithState mkFn)))
        initPtr <- DuckDBScalarFunctionInitFun <$> allocate (toFunPtr (DuckDBScalarFunctionInitFun_Aux (scalarFunctionInitHandler initState)))
        pure ScalarFunctionResources{scalarFunctionExecPtr, scalarFunctionInitPtr = Just initPtr}

-- | Configure a scalar function and transfer its callbacks to DuckDB.
registerScalarFunction :: (Function f) => Connection -> Text -> Proxy f -> ((forall a. IO (FunPtr a) -> IO (FunPtr a)) -> IO ScalarFunctionResources) -> IO ()
registerScalarFunction conn name proxy acquire = do
    when (Text.null name || Text.any (== '\0') name) $
        throwIO (functionInvocationError "duckdb-simple: invalid scalar function name")
    bracket c_duckdb_create_scalar_function cleanupScalarFunction \scalarFun -> do
        when (scalarFun == DuckDBScalarFunction nullPtr) $
            throwIO (functionInvocationError "duckdb-simple: failed to allocate scalar function")
        withCallbackResources
            acquire
            (c_duckdb_scalar_function_set_extra_info scalarFun)
            \ScalarFunctionResources{scalarFunctionExecPtr, scalarFunctionInitPtr} -> do
                TextForeign.withCString name $ c_duckdb_scalar_function_set_name scalarFun . ConstPtr
                forM_ (argumentTypes proxy) \dtype ->
                    withLogicalType dtype $ c_duckdb_scalar_function_add_parameter scalarFun
                withLogicalType (duckTypeForScalar (returnType proxy)) $ c_duckdb_scalar_function_set_return_type scalarFun
                when (isVolatile proxy) $ c_duckdb_scalar_function_set_volatile scalarFun
                c_duckdb_scalar_function_set_special_handling scalarFun
                c_duckdb_scalar_function_set_function scalarFun scalarFunctionExecPtr
                forM_ scalarFunctionInitPtr $ c_duckdb_scalar_function_set_init scalarFun
                withConnectionHandle conn \connPtr -> do
                    rc <- c_duckdb_register_scalar_function connPtr scalarFun
                    when (rc /= DuckDBSuccess) $
                        throwIO (functionInvocationError "duckdb-simple: registering function failed")

-- | Drop a previously registered scalar function by issuing a DROP FUNCTION statement.
deleteFunction :: Connection -> Text -> IO ()
deleteFunction conn name =
    do
        outcome <-
            try $
                withConnectionHandle conn \connPtr -> do
                    let dropQuery =
                            Query $
                                Text.concat
                                    [ Text.pack "DROP FUNCTION IF EXISTS "
                                    , qualifyIdentifier name
                                    ]
                    withQueryCString dropQuery \sql ->
                        withResult conn dropQuery (c_duckdb_query connPtr sql) (const (pure ()))
        case outcome of
            Right () -> pure ()
            Left err
                -- DuckDB does not allow dropping scalar functions registered via the C API,
                -- so we ignore that specific error here.
                -- TODO: Update this when DuckDB adds support for dropping such functions.
                | Text.isInfixOf (Text.pack "Cannot drop internal catalog entry") (sqlErrorMessage err) -> return ()
                | otherwise -> throwIO err

cleanupScalarFunction :: DuckDBScalarFunction -> IO ()
cleanupScalarFunction scalarFun =
    alloca \ptr -> do
        poke ptr scalarFun
        c_duckdb_destroy_scalar_function ptr

withLogicalType :: DUCKDB_TYPE -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType dtype =
    bracket
        ( do
            logical <- c_duckdb_create_logical_type (DuckDBType dtype)
            when (logical == DuckDBLogicalType nullPtr)
                $ throwIO
                $ functionInvocationError (Text.pack "duckdb-simple: failed to allocate logical type")
            pure logical
        )
        destroyLogicalType

duckTypeForScalar :: ScalarType -> DUCKDB_TYPE
duckTypeForScalar = \case
    ScalarTypeBoolean -> DUCKDB_TYPE_BOOLEAN
    ScalarTypeBigInt -> DUCKDB_TYPE_BIGINT
    ScalarTypeUBigInt -> DUCKDB_TYPE_UBIGINT
    ScalarTypeDouble -> DUCKDB_TYPE_DOUBLE
    ScalarTypeVarchar -> DUCKDB_TYPE_VARCHAR

scalarFunctionHandler :: forall f. (Function f) => f -> DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()
scalarFunctionHandler fn info chunk outVec =
    runCallback (c_duckdb_scalar_function_set_error info) do
        rawColumnCount <- c_duckdb_data_chunk_get_column_count chunk
        let columnCount = fromIntegral rawColumnCount :: Int
            expected = length (argumentTypes (Proxy :: Proxy f))
        when (columnCount /= expected)
            $ throwIO
            $ functionInvocationError
            $ Text.concat
                [ Text.pack "duckdb-simple: function expected "
                , Text.pack (show expected)
                , Text.pack " arguments but received "
                , Text.pack (show columnCount)
                ]
        rawRowCount <- c_duckdb_data_chunk_get_size chunk
        let rowCount = fromIntegral rawRowCount :: Int
        readers <- mapM (makeColumnReader chunk) [0 .. expected - 1]
        rows <-
            forM [0 .. rowCount - 1] \row ->
                forM readers \reader ->
                    reader (fromIntegral row)
        results <- mapM (`applyFunction` fn) rows
        writeResults (returnType (Proxy :: Proxy f)) results outVec

scalarFunctionHandlerWithState :: forall s f. (Function f) => (s -> f) -> DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()
scalarFunctionHandlerWithState mkFn info chunk outVec =
    runCallback (c_duckdb_scalar_function_set_error info) do
        statePtr <- c_duckdb_scalar_function_get_state info
        when (statePtr == nullPtr) $
            throwIO (functionInvocationError "duckdb-simple: scalar function state was not initialised")
        state <- deRefStablePtr (castPtrToStablePtr (castPtr statePtr) :: StablePtr s)
        scalarFunctionHandler (mkFn state) info chunk outVec

scalarFunctionInitHandler :: IO s -> DuckDBInitInfo -> IO ()
scalarFunctionInitHandler initState info =
    runCallback (c_duckdb_scalar_function_init_set_error info) do
        state <- initState
        transferCallbackState (c_duckdb_scalar_function_init_set_state info) state

type ColumnReader = DuckDBIdx -> IO Field

makeColumnReader :: DuckDBDataChunk -> Int -> IO ColumnReader
makeColumnReader chunk columnIndex = do
    readValue <- c_duckdb_data_chunk_get_vector chunk (fromIntegral columnIndex) >>= prepareVectorReader
    let name = Text.pack ("arg" <> show columnIndex)
    pure \rowIdx -> do
        value <- readValue (fromIntegral rowIdx)
        pure
            Field
                { fieldName = name
                , fieldIndex = columnIndex
                , fieldValue = value
                }
writeResults :: ScalarType -> [ScalarValue] -> DuckDBVector -> IO ()
writeResults resultType values outVec = do
    let hasNulls = any isNullValue values
    when hasNulls $
        c_duckdb_vector_ensure_validity_writable outVec
    dataPtr <- c_duckdb_vector_get_data outVec
    validityPtr <- c_duckdb_vector_get_validity outVec
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
            (ScalarTypeVarchar, ScalarText txt) -> do
                markValid validityPtr idx
                TextForeign.withCStringLen txt \(ptr, len) ->
                    c_duckdb_vector_assign_string_element_len outVec (fromIntegral idx) (ConstPtr ptr) (fromIntegral len)
            _ ->
                throwIO
                    $ functionInvocationError
                    $ Text.pack "duckdb-simple: result type mismatch when materialising scalar function output"

markInvalid :: Ptr Word64 -> Int -> IO ()
markInvalid validity idx
    | validity == nullPtr = pure ()
    | otherwise = c_duckdb_validity_set_row_invalid validity (fromIntegral idx)

markValid :: Ptr Word64 -> Int -> IO ()
markValid validity idx
    | validity == nullPtr = pure ()
    | otherwise = c_duckdb_validity_set_row_valid validity (fromIntegral idx)

isNullValue :: ScalarValue -> Bool
isNullValue = \case
    ScalarNull -> True
    _ -> False

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
