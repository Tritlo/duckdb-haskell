{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.Bits ((.|.), shiftL)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio ((%))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TextForeign
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime
    ( LocalTime (..)
    , TimeOfDay (..)
    , minutesToTimeZone
    , utc
    , utcToLocalTime
    )
import Database.DuckDB.FFI
import Database.DuckDB.Simple.FromField
    ( BigNum (..)
    , BitString (..)
    , DecimalValue (..)
    , Field (..)
    , FieldValue (..)
    , FromField (..)
    , IntervalValue (..)
    , TimeWithZone (..)
    )
import Database.DuckDB.Simple.Internal
    ( Connection
    , Query (..)
    , SQLError (..)
    , withConnectionHandle
    , withQueryCString
    )
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff)
import Database.DuckDB.Simple.Ok (Ok(..))

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
            Errors err -> throwIO (argumentConversionError (fieldIndex field) err)
            Ok value -> applyFunction rest (fn value)

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
    decimalInfo <-
        if dtype == DuckDBTypeDecimal
            then do
                width <- c_duckdb_decimal_width logical
                scale <- c_duckdb_decimal_scale logical
                pure (Just (width, scale))
            else pure Nothing
    enumInternal <-
        if dtype == DuckDBTypeEnum
            then Just <$> c_duckdb_enum_internal_type logical
            else pure Nothing
    destroyLogicalType logical
    dataPtr <- c_duckdb_vector_get_data vector
    validity <- c_duckdb_vector_get_validity vector
    let name = Text.pack ("arg" <> show columnIndex)
    pure \rowIdx -> do
        valid <- isRowValid validity rowIdx
        value <-
            if valid
                then fetchValue dtype decimalInfo enumInternal dataPtr rowIdx
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

fetchValue :: DuckDBType -> Maybe (Word8, Word8) -> Maybe DuckDBType -> Ptr () -> DuckDBIdx -> IO FieldValue
fetchValue dtype decimalInfo enumInternal dataPtr rowIdx =
    let idx = fromIntegral rowIdx
     in case dtype of
            DuckDBTypeBoolean -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Word8) idx
                pure (FieldBool (value /= 0))
            DuckDBTypeTinyInt -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Int8) idx
                pure (FieldInt8 value)
            DuckDBTypeSmallInt -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Int16) idx
                pure (FieldInt16 value)
            DuckDBTypeInteger -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Int32) idx
                pure (FieldInt32 value)
            DuckDBTypeBigInt -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Int64) idx
                pure (FieldInt64 value)
            DuckDBTypeHugeInt -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBHugeInt) idx
                pure (FieldHugeInt (duckDBHugeIntToInteger value))
            DuckDBTypeUTinyInt -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Word8) idx
                pure (FieldWord8 value)
            DuckDBTypeUSmallInt -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Word16) idx
                pure (FieldWord16 value)
            DuckDBTypeUInteger -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Word32) idx
                pure (FieldWord32 value)
            DuckDBTypeUBigInt -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Word64) idx
                pure (FieldWord64 value)
            DuckDBTypeUHugeInt -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBUHugeInt) idx
                pure (FieldUHugeInt (duckDBUHugeIntToInteger value))
            DuckDBTypeFloat -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Float) idx
                pure (FieldFloat value)
            DuckDBTypeDouble -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Double) idx
                pure (FieldDouble value)
            DuckDBTypeVarchar -> FieldText <$> decodeDuckText dataPtr rowIdx
            DuckDBTypeUUID -> FieldText <$> decodeDuckText dataPtr rowIdx
            DuckDBTypeBlob -> FieldBlob <$> decodeDuckBlob dataPtr rowIdx
            DuckDBTypeDate -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBDate) idx
                FieldDate <$> decodeDuckDBDate value
            DuckDBTypeTime -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTime) idx
                FieldTime <$> decodeDuckDBTime value
            DuckDBTypeTimeNs -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimeNs) idx
                pure (FieldTime (decodeDuckDBTimeNs value))
            DuckDBTypeTimeTz -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimeTz) idx
                FieldTimeTZ <$> decodeDuckDBTimeTz value
            DuckDBTypeTimestamp -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestamp) idx
                FieldTimestamp <$> decodeDuckDBTimestamp value
            DuckDBTypeTimestampS -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestampS) idx
                FieldTimestamp <$> decodeDuckDBTimestampSeconds value
            DuckDBTypeTimestampMs -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestampMs) idx
                FieldTimestamp <$> decodeDuckDBTimestampMilliseconds value
            DuckDBTypeTimestampNs -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestampNs) idx
                FieldTimestamp <$> decodeDuckDBTimestampNanoseconds value
            DuckDBTypeTimestampTz -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBTimestamp) idx
                FieldTimestampTZ <$> decodeDuckDBTimestampUTCTime value
            DuckDBTypeInterval -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBInterval) idx
                pure (FieldInterval (intervalValueFromDuckDB value))
            DuckDBTypeDecimal -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBDecimal) idx
                case decimalInfo of
                    Just (width, scale) -> do
                        decimal <- decimalValueFromDuckDB width scale value
                        pure (FieldDecimal decimal)
                    Nothing ->
                        throwIO $
                            functionInvocationError $
                                Text.pack "duckdb-simple: missing decimal metadata for scalar function argument"
            DuckDBTypeBit -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBBit) idx
                FieldBit <$> decodeDuckDBBit value
            DuckDBTypeBigNum -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr DuckDBBignum) idx
                FieldBigNum <$> decodeDuckDBBigNum value
            DuckDBTypeEnum -> do
                case enumInternal of
                    Just DuckDBTypeUTinyInt -> do
                        value <- peekElemOff (castPtr dataPtr :: Ptr Word8) idx
                        pure (FieldEnum (fromIntegral value))
                    Just DuckDBTypeUSmallInt -> do
                        value <- peekElemOff (castPtr dataPtr :: Ptr Word16) idx
                        pure (FieldEnum (fromIntegral value))
                    Just DuckDBTypeUInteger -> do
                        value <- peekElemOff (castPtr dataPtr :: Ptr Word32) idx
                        pure (FieldEnum value)
                    _ ->
                        throwIO $
                            functionInvocationError $
                                Text.pack "duckdb-simple: unsupported enum internal storage type for scalar function argument"
            DuckDBTypeSQLNull ->
                pure FieldNull
            DuckDBTypeStringLiteral -> FieldText <$> decodeDuckText dataPtr rowIdx
            DuckDBTypeIntegerLiteral -> do
                value <- peekElemOff (castPtr dataPtr :: Ptr Int64) idx
                pure (FieldInt64 value)
            other ->
                throwIO $
                    functionInvocationError $
                        Text.concat
                            [ Text.pack "duckdb-simple: unsupported argument type "
                            , Text.pack (show other)
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

decodeDuckBlob :: Ptr () -> DuckDBIdx -> IO BS.ByteString
decodeDuckBlob dataPtr rowIdx = do
    let bytePtr = castPtr dataPtr :: Ptr Word8
        offset = fromIntegral rowIdx * duckdbStringTSize
        stringPtr = castPtr (bytePtr `plusPtr` offset) :: Ptr DuckDBStringT
    len <- c_duckdb_string_t_length stringPtr
    if len == 0
        then pure BS.empty
        else do
            ptr <- c_duckdb_string_t_data stringPtr
            BS.packCStringLen (ptr, fromIntegral len)

duckdbStringTSize :: Int
duckdbStringTSize = 16

decodeDuckDBDate :: DuckDBDate -> IO Day
decodeDuckDBDate raw =
    alloca \ptr -> do
        c_duckdb_from_date raw ptr
        dateStruct <- peek ptr
        pure (dateStructToDay dateStruct)

decodeDuckDBTime :: DuckDBTime -> IO TimeOfDay
decodeDuckDBTime raw =
    alloca \ptr -> do
        c_duckdb_from_time raw ptr
        timeStruct <- peek ptr
        pure (timeStructToTimeOfDay timeStruct)

decodeDuckDBTimestamp :: DuckDBTimestamp -> IO LocalTime
decodeDuckDBTimestamp raw =
    alloca \ptr -> do
        c_duckdb_from_timestamp raw ptr
        DuckDBTimestampStruct{duckDBTimestampStructDate = dateStruct, duckDBTimestampStructTime = timeStruct} <- peek ptr
        pure
            LocalTime
                { localDay = dateStructToDay dateStruct
                , localTimeOfDay = timeStructToTimeOfDay timeStruct
                }

dateStructToDay :: DuckDBDateStruct -> Day
dateStructToDay DuckDBDateStruct{duckDBDateStructYear, duckDBDateStructMonth, duckDBDateStructDay} =
    fromGregorian (fromIntegral duckDBDateStructYear) (fromIntegral duckDBDateStructMonth) (fromIntegral duckDBDateStructDay)

timeStructToTimeOfDay :: DuckDBTimeStruct -> TimeOfDay
timeStructToTimeOfDay DuckDBTimeStruct{duckDBTimeStructHour, duckDBTimeStructMinute, duckDBTimeStructSecond, duckDBTimeStructMicros} =
    let secondsInt = fromIntegral duckDBTimeStructSecond :: Integer
        micros = fromIntegral duckDBTimeStructMicros :: Integer
        fractional = fromRational (micros % 1000000)
        totalSeconds = fromInteger secondsInt + fractional
     in TimeOfDay
            (fromIntegral duckDBTimeStructHour)
            (fromIntegral duckDBTimeStructMinute)
            totalSeconds

decodeDuckDBTimeNs :: DuckDBTimeNs -> TimeOfDay
decodeDuckDBTimeNs (DuckDBTimeNs nanos) =
    let (hours, remainderHours) = nanos `divMod` (60 * 60 * 1000000000)
        (minutes, remainderMinutes) = remainderHours `divMod` (60 * 1000000000)
        (seconds, fractionalNanos) = remainderMinutes `divMod` 1000000000
        fractional = fromRational (toInteger fractionalNanos % 1000000000)
        totalSeconds = fromIntegral seconds + fractional
     in TimeOfDay
            (fromIntegral hours)
            (fromIntegral minutes)
            totalSeconds

decodeDuckDBTimeTz :: DuckDBTimeTz -> IO TimeWithZone
decodeDuckDBTimeTz raw =
    alloca \ptr -> do
        c_duckdb_from_time_tz raw ptr
        DuckDBTimeTzStruct{duckDBTimeTzStructTime = timeStruct, duckDBTimeTzStructOffset = offset} <- peek ptr
        let timeOfDay = timeStructToTimeOfDay timeStruct
            minutes = fromIntegral offset `div` 60
            zone = minutesToTimeZone minutes
        pure TimeWithZone{timeWithZoneTime = timeOfDay, timeWithZoneZone = zone}

decodeDuckDBTimestampSeconds :: DuckDBTimestampS -> IO LocalTime
decodeDuckDBTimestampSeconds (DuckDBTimestampS seconds) =
    decodeDuckDBTimestamp (DuckDBTimestamp (seconds * 1000000))

decodeDuckDBTimestampMilliseconds :: DuckDBTimestampMs -> IO LocalTime
decodeDuckDBTimestampMilliseconds (DuckDBTimestampMs millis) =
    decodeDuckDBTimestamp (DuckDBTimestamp (millis * 1000))

decodeDuckDBTimestampNanoseconds :: DuckDBTimestampNs -> IO LocalTime
decodeDuckDBTimestampNanoseconds (DuckDBTimestampNs nanos) = do
    let utcTime = posixSecondsToUTCTime (fromRational (toInteger nanos % 1000000000))
    pure (utcToLocalTime utc utcTime)

decodeDuckDBTimestampUTCTime :: DuckDBTimestamp -> IO UTCTime
decodeDuckDBTimestampUTCTime (DuckDBTimestamp micros) =
    pure (posixSecondsToUTCTime (fromRational (toInteger micros % 1000000)))

intervalValueFromDuckDB :: DuckDBInterval -> IntervalValue
intervalValueFromDuckDB DuckDBInterval{duckDBIntervalMonths, duckDBIntervalDays, duckDBIntervalMicros} =
    IntervalValue
        { intervalMonths = duckDBIntervalMonths
        , intervalDays = duckDBIntervalDays
        , intervalMicros = duckDBIntervalMicros
        }

decimalValueFromDuckDB :: Word8 -> Word8 -> DuckDBDecimal -> IO DecimalValue
decimalValueFromDuckDB width scale rawDecimal = do
    let decimal = rawDecimal{duckDBDecimalWidth = width, duckDBDecimalScale = scale}
    pure
        DecimalValue
            { decimalWidth = width
            , decimalScale = scale
            , decimalInteger = duckDBHugeIntToInteger (duckDBDecimalValue decimal)
            }

duckDBHugeIntToInteger :: DuckDBHugeInt -> Integer
duckDBHugeIntToInteger DuckDBHugeInt{duckDBHugeIntLower, duckDBHugeIntUpper} =
    (fromIntegral duckDBHugeIntUpper `shiftL` 64) .|. fromIntegral duckDBHugeIntLower

duckDBUHugeIntToInteger :: DuckDBUHugeInt -> Integer
duckDBUHugeIntToInteger DuckDBUHugeInt{duckDBUHugeIntLower, duckDBUHugeIntUpper} =
    (fromIntegral duckDBUHugeIntUpper `shiftL` 64) .|. fromIntegral duckDBUHugeIntLower

decodeDuckDBBit :: DuckDBBit -> IO BitString
decodeDuckDBBit DuckDBBit{duckDBBitData, duckDBBitSize}
    | duckDBBitData == nullPtr || duckDBBitSize == 0 =
        pure BitString{bitStringLength = 0, bitStringBytes = BS.empty}
    | otherwise = do
        let bitLength = duckDBBitSize
            byteLength = fromIntegral ((bitLength + 7) `div` 8) :: Int
        bytes <- BS.packCStringLen (castPtr duckDBBitData, byteLength)
        pure BitString{bitStringLength = bitLength, bitStringBytes = bytes}

decodeDuckDBBigNum :: DuckDBBignum -> IO BigNum
decodeDuckDBBigNum DuckDBBignum{duckDBBignumData, duckDBBignumSize, duckDBBignumIsNegative}
    | duckDBBignumData == nullPtr || duckDBBignumSize == 0 =
        pure BigNum{bigNumIsNegative = duckDBBignumIsNegative /= 0, bigNumMagnitude = BS.empty}
    | otherwise = do
        let byteLength = fromIntegral duckDBBignumSize :: Int
        bytes <- BS.packCStringLen (castPtr duckDBBignumData, byteLength)
        pure BigNum{bigNumIsNegative = duckDBBignumIsNegative /= 0, bigNumMagnitude = bytes}

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
