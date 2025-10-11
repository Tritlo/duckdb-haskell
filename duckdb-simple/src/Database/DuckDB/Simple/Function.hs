{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Database.DuckDB.Simple.Function
Description : Register scalar Haskell functions with DuckDB connections.

This module mirrors the high-level API provided by @sqlite-simple@ for user
defined functions, adapted to DuckDB's chunked execution model.  It allows
pure and 'IO'-based Haskell functions to be exposed to SQL while reusing the
existing 'FromField' and 'ToField'-style typeclass machinery for argument
decoding and result marshalling.
-}
module Database.DuckDB.Simple.Function (
    Function,
    createFunction,
    deleteFunction,
) where

import Control.Exception (
    SomeException,
    bracket,
    displayException,
    onException,
    throwIO,
    try,
 )
import Control.Monad (forM, forM_, when)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TextForeign
import Data.Word (Word16, Word32, Word64, Word8)
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField (
    Field (..),
    FieldValue (..),
    FromField (..),
    ResultError (..),
 )
import Database.DuckDB.Simple.Internal (
    Connection,
    Query (..),
    SQLError (..),
    withConnectionHandle,
    withQueryCString,
 )
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peekElemOff, poke, pokeElemOff)

-- | Tag DuckDB logical types we support for scalar return values.
data ScalarType
    = ScalarTypeBoolean
    | ScalarTypeBigInt
    | ScalarTypeDouble
    | ScalarTypeVarchar

-- | Runtime representation of values returned to DuckDB.
data ScalarValue
    = ScalarNull
    | ScalarBoolean !Bool
    | ScalarInteger !Int64
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
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger (fromIntegral value))

instance FunctionResult Word16 where
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger (fromIntegral value))

instance FunctionResult Word32 where
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger (fromIntegral value))

instance FunctionResult Word64 where
    scalarReturnType _ = ScalarTypeBigInt
    toScalarValue value = pure (ScalarInteger (fromIntegral value))

instance FunctionResult Double where
    scalarReturnType _ = ScalarTypeDouble
    toScalarValue value = pure (ScalarDouble value)

instance FunctionResult Float where
    scalarReturnType _ = ScalarTypeDouble
    toScalarValue value = pure (ScalarDouble (realToFrac value))

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
    argumentType :: Proxy a -> DuckDBType

instance FunctionArg Int where
    argumentType _ = DuckDBTypeBigInt

instance FunctionArg Int16 where
    argumentType _ = DuckDBTypeSmallInt

instance FunctionArg Int32 where
    argumentType _ = DuckDBTypeInteger

instance FunctionArg Int64 where
    argumentType _ = DuckDBTypeBigInt

instance FunctionArg Word where
    argumentType _ = DuckDBTypeUBigInt

instance FunctionArg Word16 where
    argumentType _ = DuckDBTypeUSmallInt

instance FunctionArg Word32 where
    argumentType _ = DuckDBTypeUInteger

instance FunctionArg Word64 where
    argumentType _ = DuckDBTypeUBigInt

instance FunctionArg Double where
    argumentType _ = DuckDBTypeDouble

instance FunctionArg Float where
    argumentType _ = DuckDBTypeFloat

instance FunctionArg Bool where
    argumentType _ = DuckDBTypeBoolean

instance FunctionArg Text where
    argumentType _ = DuckDBTypeVarchar

instance FunctionArg String where
    argumentType _ = DuckDBTypeVarchar

instance (FunctionArg a) => FunctionArg (Maybe a) where
    argumentType _ = argumentType (Proxy :: Proxy a)

-- | Typeclass describing Haskell functions that can be exposed to DuckDB.
class Function a where
    argumentTypes :: Proxy a -> [DuckDBType]
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
            Left err -> throwIO (argumentConversionError (fieldIndex field) err)
            Right value -> applyFunction rest (fn value)

-- | Register a Haskell function under the supplied name.
createFunction :: forall f. (Function f) => Connection -> Text -> f -> IO ()
createFunction conn name fn = do
    funPtr <- mkScalarFun (scalarFunctionHandler fn)
    funPtrStable <- newStablePtr funPtr
    destroyCb <- mkDeleteCallback releaseFunctionPtr
    let release = destroyFunctionResources funPtr funPtrStable destroyCb
    bracket c_duckdb_create_scalar_function cleanupScalarFunction \scalarFun ->
        (`onException` release) $ do
            TextForeign.withCString name $ \cName ->
                c_duckdb_scalar_function_set_name scalarFun cName
            forM_ (argumentTypes (Proxy :: Proxy f)) \dtype ->
                withLogicalType dtype $ \logical ->
                    c_duckdb_scalar_function_add_parameter scalarFun logical
            withLogicalType (duckTypeForScalar (returnType (Proxy :: Proxy f))) $ \logical ->
                c_duckdb_scalar_function_set_return_type scalarFun logical
            when (isVolatile (Proxy :: Proxy f)) $
                c_duckdb_scalar_function_set_volatile scalarFun
            c_duckdb_scalar_function_set_function scalarFun funPtr
            c_duckdb_scalar_function_set_extra_info scalarFun (castStablePtrToPtr funPtrStable) destroyCb
            withConnectionHandle conn \connPtr -> do
                rc <- c_duckdb_register_scalar_function connPtr scalarFun
                if rc == DuckDBSuccess
                    then pure ()
                    else throwIO (functionInvocationError (Text.pack "duckdb-simple: registering function failed"))

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
                        alloca \resPtr -> do
                            rc <- c_duckdb_query connPtr sql resPtr
                            if rc == DuckDBSuccess
                                then c_duckdb_destroy_result resPtr
                                else do
                                    errMsg <- fetchResultError resPtr
                                    c_duckdb_destroy_result resPtr
                                    throwIO
                                        SQLError
                                            { sqlErrorMessage = errMsg
                                            , sqlErrorType = Nothing
                                            , sqlErrorQuery = Just dropQuery
                                            }
        case outcome of
            Right () -> pure ()
            Left err
                | Text.isInfixOf (Text.pack "Cannot drop internal catalog entry") (sqlErrorMessage err) ->
                    throwIO
                        err
                            { sqlErrorMessage =
                                Text.pack "duckdb-simple: DuckDB does not allow dropping scalar functions registered via the C API in this build."
                            }
                | otherwise -> throwIO err

cleanupScalarFunction :: DuckDBScalarFunction -> IO ()
cleanupScalarFunction scalarFun =
    alloca \ptr -> do
        poke ptr scalarFun
        c_duckdb_destroy_scalar_function ptr

destroyFunctionResources :: DuckDBScalarFunctionFun -> StablePtr DuckDBScalarFunctionFun -> DuckDBDeleteCallback -> IO ()
destroyFunctionResources funPtr funPtrStable destroyCb = do
    freeHaskellFunPtr funPtr
    freeStablePtr funPtrStable
    freeHaskellFunPtr destroyCb

releaseFunctionPtr :: Ptr () -> IO ()
releaseFunctionPtr rawPtr =
    when (rawPtr /= nullPtr) $ do
        let stablePtr = castPtrToStablePtr rawPtr :: StablePtr DuckDBScalarFunctionFun
        funPtr <- deRefStablePtr stablePtr
        freeHaskellFunPtr funPtr
        freeStablePtr stablePtr

withLogicalType :: DuckDBType -> (DuckDBLogicalType -> IO a) -> IO a
withLogicalType dtype =
    bracket
        ( do
            logical <- c_duckdb_create_logical_type dtype
            when (logical == nullPtr) $
                throwIO $
                    functionInvocationError (Text.pack "duckdb-simple: failed to allocate logical type")
            pure logical
        )
        destroyLogicalType

destroyLogicalType :: DuckDBLogicalType -> IO ()
destroyLogicalType logicalType =
    alloca \ptr -> do
        poke ptr logicalType
        c_duckdb_destroy_logical_type ptr

duckTypeForScalar :: ScalarType -> DuckDBType
duckTypeForScalar = \case
    ScalarTypeBoolean -> DuckDBTypeBoolean
    ScalarTypeBigInt -> DuckDBTypeBigInt
    ScalarTypeDouble -> DuckDBTypeDouble
    ScalarTypeVarchar -> DuckDBTypeVarchar

scalarFunctionHandler :: forall f. (Function f) => f -> DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()
scalarFunctionHandler fn info chunk outVec = do
    result <-
        try do
            rawColumnCount <- c_duckdb_data_chunk_get_column_count chunk
            let columnCount = fromIntegral rawColumnCount :: Int
                expected = length (argumentTypes (Proxy :: Proxy f))
            when (columnCount /= expected) $
                throwIO $
                    functionInvocationError $
                        Text.concat
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
            c_duckdb_data_chunk_set_size chunk (fromIntegral rowCount)
    case result of
        Left (err :: SomeException) -> do
            c_duckdb_data_chunk_set_size chunk 0
            let message = Text.pack (displayException err)
            TextForeign.withCString message $ \cMsg ->
                c_duckdb_scalar_function_set_error info cMsg
        Right () -> pure ()

type ColumnReader = DuckDBIdx -> IO Field

makeColumnReader :: DuckDBDataChunk -> Int -> IO ColumnReader
makeColumnReader chunk columnIndex = do
    vector <- c_duckdb_data_chunk_get_vector chunk (fromIntegral columnIndex)
    logical <- c_duckdb_vector_get_column_type vector
    dtype <- c_duckdb_get_type_id logical
    destroyLogicalType logical
    dataPtr <- c_duckdb_vector_get_data vector
    validity <- c_duckdb_vector_get_validity vector
    let name = Text.pack ("arg" <> show columnIndex)
    pure \rowIdx -> do
        valid <- isRowValid validity rowIdx
        value <-
            if valid
                then fetchValue dtype dataPtr rowIdx
                else pure FieldNull
        pure
            Field
                { fieldName = name
                , fieldIndex = columnIndex
                , fieldValue = value
                }

isRowValid :: Ptr Word64 -> DuckDBIdx -> IO Bool
isRowValid validity rowIdx
    | validity == nullPtr = pure True
    | otherwise = do
        CBool flag <- c_duckdb_validity_row_is_valid validity rowIdx
        pure (flag /= 0)

fetchValue :: DuckDBType -> Ptr () -> DuckDBIdx -> IO FieldValue
fetchValue dtype dataPtr rowIdx
    | dtype == DuckDBTypeBoolean = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Word8) (fromIntegral rowIdx)
        pure (FieldBool (value /= 0))
    | dtype == DuckDBTypeTinyInt = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Int8) (fromIntegral rowIdx)
        pure (FieldInt8 (fromIntegral value))
    | dtype == DuckDBTypeSmallInt = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Int16) (fromIntegral rowIdx)
        pure (FieldInt16 (fromIntegral value))
    | dtype == DuckDBTypeInteger = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Int32) (fromIntegral rowIdx)
        pure (FieldInt32 (fromIntegral value))
    | dtype == DuckDBTypeBigInt || dtype == DuckDBTypeHugeInt = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Int64) (fromIntegral rowIdx)
        pure (FieldInt64 value)
    | dtype == DuckDBTypeUTinyInt = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Word8) (fromIntegral rowIdx)
        pure (FieldWord8 (fromIntegral value))
    | dtype == DuckDBTypeUSmallInt = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Word16) (fromIntegral rowIdx)
        pure (FieldWord16 (fromIntegral value))
    | dtype == DuckDBTypeUInteger = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Word32) (fromIntegral rowIdx)
        pure (FieldWord32 (fromIntegral value))
    | dtype == DuckDBTypeUBigInt || dtype == DuckDBTypeUHugeInt = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Word64) (fromIntegral rowIdx)
        pure (FieldWord64 (fromIntegral value))
    | dtype == DuckDBTypeFloat = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Float) (fromIntegral rowIdx)
        pure (FieldDouble (realToFrac value))
    | dtype == DuckDBTypeDouble = do
        value <- peekElemOff (castPtr dataPtr :: Ptr Double) (fromIntegral rowIdx)
        pure (FieldDouble value)
    | dtype == DuckDBTypeVarchar = do
        textValue <- decodeDuckText dataPtr rowIdx
        pure (FieldText textValue)
    | otherwise =
        throwIO $
            functionInvocationError $
                Text.concat
                    [ Text.pack "duckdb-simple: unsupported argument type "
                    , Text.pack (show dtype)
                    ]

decodeDuckText :: Ptr () -> DuckDBIdx -> IO Text
decodeDuckText dataPtr rowIdx = do
    let bytePtr = castPtr dataPtr :: Ptr Word8
        offset = fromIntegral rowIdx * duckdbStringTSize
        stringPtr = castPtr (bytePtr `plusPtr` offset) :: Ptr DuckDBStringT
    len <- c_duckdb_string_t_length stringPtr
    if len == 0
        then pure Text.empty
        else do
            cstr <- c_duckdb_string_t_data stringPtr
            bytes <- BS.packCStringLen (cstr, fromIntegral len)
            pure (TE.decodeUtf8 bytes)

duckdbStringTSize :: Int
duckdbStringTSize = 16

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
            (ScalarTypeDouble, ScalarDouble dbl) -> do
                markValid validityPtr idx
                pokeElemOff (castPtr dataPtr :: Ptr Double) idx dbl
            (ScalarTypeVarchar, ScalarText txt) -> do
                markValid validityPtr idx
                TextForeign.withCStringLen txt \(ptr, len) ->
                    c_duckdb_vector_assign_string_element_len outVec (fromIntegral idx) ptr (fromIntegral len)
            _ ->
                throwIO $
                    functionInvocationError $
                        Text.pack "duckdb-simple: result type mismatch when materialising scalar function output"

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

argumentConversionError :: Int -> ResultError -> SQLError
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

fetchResultError :: Ptr DuckDBResult -> IO Text
fetchResultError resPtr = do
    msgPtr <- c_duckdb_result_error resPtr
    if msgPtr == nullPtr
        then pure (Text.pack "duckdb-simple: DROP FUNCTION failed")
        else Text.pack <$> peekCString msgPtr

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

foreign import ccall "wrapper"
    mkScalarFun :: (DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ()) -> IO DuckDBScalarFunctionFun

foreign import ccall "wrapper"
    mkDeleteCallback :: (Ptr () -> IO ()) -> IO DuckDBDeleteCallback
