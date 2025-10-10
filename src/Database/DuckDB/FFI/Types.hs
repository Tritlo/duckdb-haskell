{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Database.DuckDB.FFI.Types (
  -- * Enumerations
  DuckDBState (..),
  pattern DuckDBSuccess,
  pattern DuckDBError,
  DuckDBType (..),
  pattern DuckDBTypeInvalid,
  pattern DuckDBTypeBoolean,
  pattern DuckDBTypeTinyInt,
  pattern DuckDBTypeSmallInt,
  pattern DuckDBTypeInteger,
  pattern DuckDBTypeBigInt,
  pattern DuckDBTypeUTinyInt,
  pattern DuckDBTypeUSmallInt,
  pattern DuckDBTypeUInteger,
  pattern DuckDBTypeUBigInt,
  pattern DuckDBTypeFloat,
  pattern DuckDBTypeDouble,
  pattern DuckDBTypeTimestamp,
  pattern DuckDBTypeDate,
  pattern DuckDBTypeTime,
  pattern DuckDBTypeInterval,
  pattern DuckDBTypeHugeInt,
  pattern DuckDBTypeUHugeInt,
  pattern DuckDBTypeVarchar,
  pattern DuckDBTypeBlob,
  pattern DuckDBTypeDecimal,
  pattern DuckDBTypeTimestampS,
  pattern DuckDBTypeTimestampMs,
  pattern DuckDBTypeTimestampNs,
  pattern DuckDBTypeEnum,
  pattern DuckDBTypeList,
  pattern DuckDBTypeStruct,
  pattern DuckDBTypeMap,
  pattern DuckDBTypeArray,
  pattern DuckDBTypeUUID,
  pattern DuckDBTypeUnion,
  pattern DuckDBTypeBit,
  pattern DuckDBTypeTimeTz,
  pattern DuckDBTypeTimestampTz,
  pattern DuckDBTypeAny,
  pattern DuckDBTypeBigNum,
  pattern DuckDBTypeSQLNull,
  pattern DuckDBTypeStringLiteral,
  pattern DuckDBTypeIntegerLiteral,
  pattern DuckDBTypeTimeNs,
  DuckDBPendingState (..),
  pattern DuckDBPendingResultReady,
  pattern DuckDBPendingResultNotReady,
  pattern DuckDBPendingError,
  pattern DuckDBPendingNoTasksAvailable,
  DuckDBResultType (..),
  pattern DuckDBResultTypeInvalid,
  pattern DuckDBResultTypeChangedRows,
  pattern DuckDBResultTypeNothing,
  pattern DuckDBResultTypeQueryResult,
  DuckDBStatementType (..),
  pattern DuckDBStatementTypeInvalid,
  pattern DuckDBStatementTypeSelect,
  pattern DuckDBStatementTypeInsert,
  pattern DuckDBStatementTypeUpdate,
  pattern DuckDBStatementTypeExplain,
  pattern DuckDBStatementTypeDelete,
  pattern DuckDBStatementTypePrepare,
  pattern DuckDBStatementTypeCreate,
  pattern DuckDBStatementTypeExecute,
  pattern DuckDBStatementTypeAlter,
  pattern DuckDBStatementTypeTransaction,
  pattern DuckDBStatementTypeCopy,
  pattern DuckDBStatementTypeAnalyze,
  pattern DuckDBStatementTypeVariableSet,
  pattern DuckDBStatementTypeCreateFunc,
  pattern DuckDBStatementTypeDrop,
  pattern DuckDBStatementTypeExport,
  pattern DuckDBStatementTypePragma,
  pattern DuckDBStatementTypeVacuum,
  pattern DuckDBStatementTypeCall,
  pattern DuckDBStatementTypeSet,
  pattern DuckDBStatementTypeLoad,
  pattern DuckDBStatementTypeRelation,
  pattern DuckDBStatementTypeExtension,
  pattern DuckDBStatementTypeLogicalPlan,
  pattern DuckDBStatementTypeAttach,
  pattern DuckDBStatementTypeDetach,
  pattern DuckDBStatementTypeMulti,
  DuckDBErrorType (..),
  pattern DuckDBErrorInvalid,
  pattern DuckDBErrorOutOfRange,
  pattern DuckDBErrorConversion,
  pattern DuckDBErrorUnknownType,
  pattern DuckDBErrorDecimal,
  pattern DuckDBErrorMismatchType,
  pattern DuckDBErrorDivideByZero,
  pattern DuckDBErrorObjectSize,
  pattern DuckDBErrorInvalidType,
  pattern DuckDBErrorSerialization,
  pattern DuckDBErrorTransaction,
  pattern DuckDBErrorNotImplemented,
  pattern DuckDBErrorExpression,
  pattern DuckDBErrorCatalog,
  pattern DuckDBErrorParser,
  pattern DuckDBErrorPlanner,
  pattern DuckDBErrorScheduler,
  pattern DuckDBErrorExecutor,
  pattern DuckDBErrorConstraint,
  pattern DuckDBErrorIndex,
  pattern DuckDBErrorStat,
  pattern DuckDBErrorConnection,
  pattern DuckDBErrorSyntax,
  pattern DuckDBErrorSettings,
  pattern DuckDBErrorBinder,
  pattern DuckDBErrorNetwork,
  pattern DuckDBErrorOptimizer,
  pattern DuckDBErrorNullPointer,
  pattern DuckDBErrorIO,
  pattern DuckDBErrorInterrupt,
  pattern DuckDBErrorFatal,
  pattern DuckDBErrorInternal,
  pattern DuckDBErrorInvalidInput,
  pattern DuckDBErrorOutOfMemory,
  pattern DuckDBErrorPermission,
  pattern DuckDBErrorParameterNotResolved,
  pattern DuckDBErrorParameterNotAllowed,
  pattern DuckDBErrorDependency,
  pattern DuckDBErrorHTTP,
  pattern DuckDBErrorMissingExtension,
  pattern DuckDBErrorAutoload,
  pattern DuckDBErrorSequence,
  pattern DuckDBInvalidConfiguration,
  pattern DuckDBErrorInvalidConfiguration,
  DuckDBCastMode (..),
  pattern DuckDBCastNormal,
  pattern DuckDBCastTry,

  -- * Scalar Types
  DuckDBIdx,
  DuckDBSel,
  DuckDBDate (..),
  DuckDBDateStruct (..),
  DuckDBTime (..),
  DuckDBTimeStruct (..),
  DuckDBTimeNs (..),
  DuckDBTimeTz (..),
  DuckDBTimeTzStruct (..),
  DuckDBTimestamp (..),
  DuckDBTimestampStruct (..),
  DuckDBTimestampS (..),
  DuckDBTimestampMs (..),
  DuckDBTimestampNs (..),
  DuckDBInterval (..),
  DuckDBHugeInt (..),
  DuckDBUHugeInt (..),
  DuckDBDecimal (..),
  DuckDBBlob (..),
  DuckDBString (..),
  DuckDBStringT,
  DuckDBBit (..),
  DuckDBBignum (..),
  DuckDBQueryProgress (..),

  -- * Result Structures
  DuckDBResult (..),
  DuckDBColumn,

  -- * Opaque Pointer Types
  DuckDBDatabase,
  DuckDBConnection,
  DuckDBConfig,
  DuckDBInstanceCache,
  DuckDBArrowOptions,
  DuckDBArrow,
  DuckDBArrowSchema,
  DuckDBArrowArray,
  ArrowSchemaPtr (..),
  ArrowArrayPtr (..),
  ArrowStreamPtr (..),
  DuckDBArrowConvertedSchema,
  DuckDBArrowStream,
  DuckDBPreparedStatement,
  DuckDBPendingResult,
  DuckDBExtractedStatements,
  DuckDBLogicalType,
  DuckDBCreateTypeInfo,
  DuckDBVector,
  DuckDBDataChunk,
  DuckDBSelectionVector,
  DuckDBFunctionInfo,
  DuckDBBindInfo,
  DuckDBInitInfo,
  DuckDBScalarFunction,
  DuckDBScalarFunctionSet,
  DuckDBAggregateFunction,
  DuckDBAggregateFunctionSet,
  DuckDBAggregateState,
  DuckDBCastFunction,
  DuckDBExpression,
  DuckDBClientContext,
  DuckDBTableFunction,
  DuckDBValue,
  DuckDBErrorData,
  DuckDBAppender,
  DuckDBTableDescription,
  DuckDBProfilingInfo,
  DuckDBReplacementScanInfo,
  DuckDBTaskState,
  ArrowArray (..),
  ArrowSchema (..),

  -- * Opaque Struct Tags
  DuckDBDatabaseStruct,
  DuckDBConnectionStruct,
  DuckDBConfigStruct,
  DuckDBInstanceCacheStruct,
  DuckDBExtractedStatementsStruct,
  DuckDBFunctionInfoStruct,
  DuckDBBindInfoStruct,
  DuckDBScalarFunctionStruct,
  DuckDBScalarFunctionSetStruct,
  DuckDBAggregateFunctionStruct,
  DuckDBAggregateFunctionSetStruct,
  DuckDBVectorStruct,
  DuckDBDataChunkStruct,
  DuckDBSelectionVectorStruct,
  DuckDBArrowOptionsStruct,
  DuckDBArrowStruct,
  DuckDBArrowConvertedSchemaStruct,
  DuckDBArrowStreamStruct,
  DuckDBExpressionStruct,
  DuckDBClientContextStruct,
  DuckDBPreparedStatementStruct,
  DuckDBValueStruct,
  DuckDBPendingResultStruct,
  DuckDBLogicalTypeStruct,
  DuckDBCreateTypeInfoStruct,
  DuckDBErrorDataStruct,
  DuckDBInitInfoStruct,
  DuckDBCastFunctionStruct,
  DuckDBTableFunctionStruct,
  DuckDBAppenderStruct,
  DuckDBTableDescriptionStruct,
  DuckDBProfilingInfoStruct,
  DuckDBReplacementScanInfoStruct,
  DuckDBAggregateStateStruct,

  -- * Function Pointer Types
  DuckDBScalarFunctionFun,
  DuckDBScalarFunctionBindFun,
  DuckDBDeleteCallback,
  DuckDBCopyCallback,
  DuckDBCastFunctionFun,
  DuckDBAggregateStateSizeFun,
  DuckDBAggregateInitFun,
  DuckDBAggregateDestroyFun,
  DuckDBAggregateUpdateFun,
  DuckDBAggregateCombineFun,
  DuckDBAggregateFinalizeFun,
  DuckDBTableFunctionBindFun,
  DuckDBTableFunctionInitFun,
  DuckDBTableFunctionFun,
  DuckDBReplacementCallback,
) where

import Data.Int (Int32, Int64, Int8)
import Data.Word (Word32, Word64, Word8)
import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..), peekByteOff, pokeByteOff)

-- | Unsigned index type used by DuckDB (mirrors @idx_t@).
type DuckDBIdx = Word64

-- | Selection vector entry type (mirrors @sel_t@).
type DuckDBSel = Word32

-- | Result state returned by most DuckDB C API calls.
newtype DuckDBState = DuckDBState {unDuckDBState :: CInt}
  deriving (Eq, Ord, Show, Storable)

-- | Pattern synonyms for @duckdb_state@ constants.
pattern DuckDBSuccess, DuckDBError :: DuckDBState
pattern DuckDBSuccess = DuckDBState 0
pattern DuckDBError = DuckDBState 1

{-# COMPLETE DuckDBSuccess, DuckDBError #-}

-- | DuckDB primitive physical type identifiers (mirrors @duckdb_type@).
newtype DuckDBType = DuckDBType {unDuckDBType :: CInt}
  deriving (Eq, Ord, Show, Storable)

-- | Pattern synonyms for DuckDB's physical value type tags.
pattern
  DuckDBTypeInvalid
  , DuckDBTypeBoolean
  , DuckDBTypeTinyInt
  , DuckDBTypeSmallInt
  , DuckDBTypeInteger
  , DuckDBTypeBigInt
  , DuckDBTypeUTinyInt
  , DuckDBTypeUSmallInt
  , DuckDBTypeUInteger
  , DuckDBTypeUBigInt
  , DuckDBTypeFloat
  , DuckDBTypeDouble
  , DuckDBTypeTimestamp
  , DuckDBTypeDate
  , DuckDBTypeTime
  , DuckDBTypeInterval
  , DuckDBTypeHugeInt
  , DuckDBTypeUHugeInt
  , DuckDBTypeVarchar
  , DuckDBTypeBlob
  , DuckDBTypeDecimal
  , DuckDBTypeTimestampS
  , DuckDBTypeTimestampMs
  , DuckDBTypeTimestampNs
  , DuckDBTypeEnum
  , DuckDBTypeList
  , DuckDBTypeStruct
  , DuckDBTypeMap
  , DuckDBTypeArray
  , DuckDBTypeUUID
  , DuckDBTypeUnion
  , DuckDBTypeBit
  , DuckDBTypeTimeTz
  , DuckDBTypeTimestampTz
  , DuckDBTypeAny
  , DuckDBTypeBigNum
  , DuckDBTypeSQLNull
  , DuckDBTypeStringLiteral
  , DuckDBTypeIntegerLiteral
  , DuckDBTypeTimeNs
    :: DuckDBType
pattern DuckDBTypeInvalid = DuckDBType 0
pattern DuckDBTypeBoolean = DuckDBType 1
pattern DuckDBTypeTinyInt = DuckDBType 2
pattern DuckDBTypeSmallInt = DuckDBType 3
pattern DuckDBTypeInteger = DuckDBType 4
pattern DuckDBTypeBigInt = DuckDBType 5
pattern DuckDBTypeUTinyInt = DuckDBType 6
pattern DuckDBTypeUSmallInt = DuckDBType 7
pattern DuckDBTypeUInteger = DuckDBType 8
pattern DuckDBTypeUBigInt = DuckDBType 9
pattern DuckDBTypeFloat = DuckDBType 10
pattern DuckDBTypeDouble = DuckDBType 11
pattern DuckDBTypeTimestamp = DuckDBType 12
pattern DuckDBTypeDate = DuckDBType 13
pattern DuckDBTypeTime = DuckDBType 14
pattern DuckDBTypeInterval = DuckDBType 15
pattern DuckDBTypeHugeInt = DuckDBType 16
pattern DuckDBTypeUHugeInt = DuckDBType 32
pattern DuckDBTypeVarchar = DuckDBType 17
pattern DuckDBTypeBlob = DuckDBType 18
pattern DuckDBTypeDecimal = DuckDBType 19
pattern DuckDBTypeTimestampS = DuckDBType 20
pattern DuckDBTypeTimestampMs = DuckDBType 21
pattern DuckDBTypeTimestampNs = DuckDBType 22
pattern DuckDBTypeEnum = DuckDBType 23
pattern DuckDBTypeList = DuckDBType 24
pattern DuckDBTypeStruct = DuckDBType 25
pattern DuckDBTypeMap = DuckDBType 26
pattern DuckDBTypeArray = DuckDBType 33
pattern DuckDBTypeUUID = DuckDBType 27
pattern DuckDBTypeUnion = DuckDBType 28
pattern DuckDBTypeBit = DuckDBType 29
pattern DuckDBTypeTimeTz = DuckDBType 30
pattern DuckDBTypeTimestampTz = DuckDBType 31
pattern DuckDBTypeAny = DuckDBType 34
pattern DuckDBTypeBigNum = DuckDBType 35
pattern DuckDBTypeSQLNull = DuckDBType 36
pattern DuckDBTypeStringLiteral = DuckDBType 37
pattern DuckDBTypeIntegerLiteral = DuckDBType 38
pattern DuckDBTypeTimeNs = DuckDBType 39

-- | Pending result state returned from @duckdb_pending_*@ APIs.
newtype DuckDBPendingState = DuckDBPendingState {unDuckDBPendingState :: CInt}
  deriving (Eq, Ord, Show, Storable)

-- | Pattern synonyms for @duckdb_pending_state@ constants.
pattern
  DuckDBPendingResultReady
  , DuckDBPendingResultNotReady
  , DuckDBPendingError
  , DuckDBPendingNoTasksAvailable
    :: DuckDBPendingState
pattern DuckDBPendingResultReady = DuckDBPendingState 0
pattern DuckDBPendingResultNotReady = DuckDBPendingState 1
pattern DuckDBPendingError = DuckDBPendingState 2
pattern DuckDBPendingNoTasksAvailable = DuckDBPendingState 3

{-# COMPLETE
  DuckDBPendingResultReady
  , DuckDBPendingResultNotReady
  , DuckDBPendingError
  , DuckDBPendingNoTasksAvailable
  #-}

-- | Result payload type returned by DuckDB queries (@duckdb_result_type@).
newtype DuckDBResultType = DuckDBResultType {unDuckDBResultType :: CInt}
  deriving (Eq, Ord, Show, Storable)

-- | Pattern synonyms for @duckdb_result_type@ constants.
pattern
  DuckDBResultTypeInvalid
  , DuckDBResultTypeChangedRows
  , DuckDBResultTypeNothing
  , DuckDBResultTypeQueryResult
    :: DuckDBResultType
pattern DuckDBResultTypeInvalid = DuckDBResultType 0
pattern DuckDBResultTypeChangedRows = DuckDBResultType 1
pattern DuckDBResultTypeNothing = DuckDBResultType 2
pattern DuckDBResultTypeQueryResult = DuckDBResultType 3

{-# COMPLETE
  DuckDBResultTypeInvalid
  , DuckDBResultTypeChangedRows
  , DuckDBResultTypeNothing
  , DuckDBResultTypeQueryResult
  #-}

-- | Classifies the SQL statement executed (@duckdb_statement_type@).
newtype DuckDBStatementType = DuckDBStatementType {unDuckDBStatementType :: CInt}
  deriving (Eq, Ord, Show, Storable)

-- | Pattern synonyms for @duckdb_statement_type@ constants.
pattern
  DuckDBStatementTypeInvalid
  , DuckDBStatementTypeSelect
  , DuckDBStatementTypeInsert
  , DuckDBStatementTypeUpdate
  , DuckDBStatementTypeExplain
  , DuckDBStatementTypeDelete
  , DuckDBStatementTypePrepare
  , DuckDBStatementTypeCreate
  , DuckDBStatementTypeExecute
  , DuckDBStatementTypeAlter
  , DuckDBStatementTypeTransaction
  , DuckDBStatementTypeCopy
  , DuckDBStatementTypeAnalyze
  , DuckDBStatementTypeVariableSet
  , DuckDBStatementTypeCreateFunc
  , DuckDBStatementTypeDrop
  , DuckDBStatementTypeExport
  , DuckDBStatementTypePragma
  , DuckDBStatementTypeVacuum
  , DuckDBStatementTypeCall
  , DuckDBStatementTypeSet
  , DuckDBStatementTypeLoad
  , DuckDBStatementTypeRelation
  , DuckDBStatementTypeExtension
  , DuckDBStatementTypeLogicalPlan
  , DuckDBStatementTypeAttach
  , DuckDBStatementTypeDetach
  , DuckDBStatementTypeMulti
    :: DuckDBStatementType
pattern DuckDBStatementTypeInvalid = DuckDBStatementType 0
pattern DuckDBStatementTypeSelect = DuckDBStatementType 1
pattern DuckDBStatementTypeInsert = DuckDBStatementType 2
pattern DuckDBStatementTypeUpdate = DuckDBStatementType 3
pattern DuckDBStatementTypeExplain = DuckDBStatementType 4
pattern DuckDBStatementTypeDelete = DuckDBStatementType 5
pattern DuckDBStatementTypePrepare = DuckDBStatementType 6
pattern DuckDBStatementTypeCreate = DuckDBStatementType 7
pattern DuckDBStatementTypeExecute = DuckDBStatementType 8
pattern DuckDBStatementTypeAlter = DuckDBStatementType 9
pattern DuckDBStatementTypeTransaction = DuckDBStatementType 10
pattern DuckDBStatementTypeCopy = DuckDBStatementType 11
pattern DuckDBStatementTypeAnalyze = DuckDBStatementType 12
pattern DuckDBStatementTypeVariableSet = DuckDBStatementType 13
pattern DuckDBStatementTypeCreateFunc = DuckDBStatementType 14
pattern DuckDBStatementTypeDrop = DuckDBStatementType 15
pattern DuckDBStatementTypeExport = DuckDBStatementType 16
pattern DuckDBStatementTypePragma = DuckDBStatementType 17
pattern DuckDBStatementTypeVacuum = DuckDBStatementType 18
pattern DuckDBStatementTypeCall = DuckDBStatementType 19
pattern DuckDBStatementTypeSet = DuckDBStatementType 20
pattern DuckDBStatementTypeLoad = DuckDBStatementType 21
pattern DuckDBStatementTypeRelation = DuckDBStatementType 22
pattern DuckDBStatementTypeExtension = DuckDBStatementType 23
pattern DuckDBStatementTypeLogicalPlan = DuckDBStatementType 24
pattern DuckDBStatementTypeAttach = DuckDBStatementType 25
pattern DuckDBStatementTypeDetach = DuckDBStatementType 26
pattern DuckDBStatementTypeMulti = DuckDBStatementType 27

{-# COMPLETE
  DuckDBStatementTypeInvalid
  , DuckDBStatementTypeSelect
  , DuckDBStatementTypeInsert
  , DuckDBStatementTypeUpdate
  , DuckDBStatementTypeExplain
  , DuckDBStatementTypeDelete
  , DuckDBStatementTypePrepare
  , DuckDBStatementTypeCreate
  , DuckDBStatementTypeExecute
  , DuckDBStatementTypeAlter
  , DuckDBStatementTypeTransaction
  , DuckDBStatementTypeCopy
  , DuckDBStatementTypeAnalyze
  , DuckDBStatementTypeVariableSet
  , DuckDBStatementTypeCreateFunc
  , DuckDBStatementTypeDrop
  , DuckDBStatementTypeExport
  , DuckDBStatementTypePragma
  , DuckDBStatementTypeVacuum
  , DuckDBStatementTypeCall
  , DuckDBStatementTypeSet
  , DuckDBStatementTypeLoad
  , DuckDBStatementTypeRelation
  , DuckDBStatementTypeExtension
  , DuckDBStatementTypeLogicalPlan
  , DuckDBStatementTypeAttach
  , DuckDBStatementTypeDetach
  , DuckDBStatementTypeMulti
  #-}

-- | DuckDB error classification codes.
newtype DuckDBErrorType = DuckDBErrorType {unDuckDBErrorType :: CInt}
  deriving (Eq, Ord, Show, Storable)

-- | Pattern synonyms mirroring @duckdb_error_type@ values.
pattern
  DuckDBErrorInvalid
  , DuckDBErrorOutOfRange
  , DuckDBErrorConversion
  , DuckDBErrorUnknownType
  , DuckDBErrorDecimal
  , DuckDBErrorMismatchType
  , DuckDBErrorDivideByZero
  , DuckDBErrorObjectSize
  , DuckDBErrorInvalidType
  , DuckDBErrorSerialization
  , DuckDBErrorTransaction
  , DuckDBErrorNotImplemented
  , DuckDBErrorExpression
  , DuckDBErrorCatalog
  , DuckDBErrorParser
  , DuckDBErrorPlanner
  , DuckDBErrorScheduler
  , DuckDBErrorExecutor
  , DuckDBErrorConstraint
  , DuckDBErrorIndex
  , DuckDBErrorStat
  , DuckDBErrorConnection
  , DuckDBErrorSyntax
  , DuckDBErrorSettings
  , DuckDBErrorBinder
  , DuckDBErrorNetwork
  , DuckDBErrorOptimizer
  , DuckDBErrorNullPointer
  , DuckDBErrorIO
  , DuckDBErrorInterrupt
  , DuckDBErrorFatal
  , DuckDBErrorInternal
  , DuckDBErrorInvalidInput
  , DuckDBErrorOutOfMemory
  , DuckDBErrorPermission
  , DuckDBErrorParameterNotResolved
  , DuckDBErrorParameterNotAllowed
  , DuckDBErrorDependency
  , DuckDBErrorHTTP
  , DuckDBErrorMissingExtension
  , DuckDBErrorAutoload
  , DuckDBErrorSequence
  , DuckDBInvalidConfiguration
    :: DuckDBErrorType
pattern DuckDBErrorInvalid = DuckDBErrorType 0
pattern DuckDBErrorOutOfRange = DuckDBErrorType 1
pattern DuckDBErrorConversion = DuckDBErrorType 2
pattern DuckDBErrorUnknownType = DuckDBErrorType 3
pattern DuckDBErrorDecimal = DuckDBErrorType 4
pattern DuckDBErrorMismatchType = DuckDBErrorType 5
pattern DuckDBErrorDivideByZero = DuckDBErrorType 6
pattern DuckDBErrorObjectSize = DuckDBErrorType 7
pattern DuckDBErrorInvalidType = DuckDBErrorType 8
pattern DuckDBErrorSerialization = DuckDBErrorType 9
pattern DuckDBErrorTransaction = DuckDBErrorType 10
pattern DuckDBErrorNotImplemented = DuckDBErrorType 11
pattern DuckDBErrorExpression = DuckDBErrorType 12
pattern DuckDBErrorCatalog = DuckDBErrorType 13
pattern DuckDBErrorParser = DuckDBErrorType 14
pattern DuckDBErrorPlanner = DuckDBErrorType 15
pattern DuckDBErrorScheduler = DuckDBErrorType 16
pattern DuckDBErrorExecutor = DuckDBErrorType 17
pattern DuckDBErrorConstraint = DuckDBErrorType 18
pattern DuckDBErrorIndex = DuckDBErrorType 19
pattern DuckDBErrorStat = DuckDBErrorType 20
pattern DuckDBErrorConnection = DuckDBErrorType 21
pattern DuckDBErrorSyntax = DuckDBErrorType 22
pattern DuckDBErrorSettings = DuckDBErrorType 23
pattern DuckDBErrorBinder = DuckDBErrorType 24
pattern DuckDBErrorNetwork = DuckDBErrorType 25
pattern DuckDBErrorOptimizer = DuckDBErrorType 26
pattern DuckDBErrorNullPointer = DuckDBErrorType 27
pattern DuckDBErrorIO = DuckDBErrorType 28
pattern DuckDBErrorInterrupt = DuckDBErrorType 29
pattern DuckDBErrorFatal = DuckDBErrorType 30
pattern DuckDBErrorInternal = DuckDBErrorType 31
pattern DuckDBErrorInvalidInput = DuckDBErrorType 32
pattern DuckDBErrorOutOfMemory = DuckDBErrorType 33
pattern DuckDBErrorPermission = DuckDBErrorType 34
pattern DuckDBErrorParameterNotResolved = DuckDBErrorType 35
pattern DuckDBErrorParameterNotAllowed = DuckDBErrorType 36
pattern DuckDBErrorDependency = DuckDBErrorType 37
pattern DuckDBErrorHTTP = DuckDBErrorType 38
pattern DuckDBErrorMissingExtension = DuckDBErrorType 39
pattern DuckDBErrorAutoload = DuckDBErrorType 40
pattern DuckDBErrorSequence = DuckDBErrorType 41
pattern DuckDBInvalidConfiguration = DuckDBErrorType 42

-- | Backwards-compatible alias for 'DuckDBInvalidConfiguration'.
{-# DEPRECATED DuckDBErrorInvalidConfiguration "Use DuckDBInvalidConfiguration (matches upstream duckdb.h)" #-}
pattern DuckDBErrorInvalidConfiguration :: DuckDBErrorType
pattern DuckDBErrorInvalidConfiguration = DuckDBInvalidConfiguration

{-# COMPLETE
  DuckDBErrorInvalid
  , DuckDBErrorOutOfRange
  , DuckDBErrorConversion
  , DuckDBErrorUnknownType
  , DuckDBErrorDecimal
  , DuckDBErrorMismatchType
  , DuckDBErrorDivideByZero
  , DuckDBErrorObjectSize
  , DuckDBErrorInvalidType
  , DuckDBErrorSerialization
  , DuckDBErrorTransaction
  , DuckDBErrorNotImplemented
  , DuckDBErrorExpression
  , DuckDBErrorCatalog
  , DuckDBErrorParser
  , DuckDBErrorPlanner
  , DuckDBErrorScheduler
  , DuckDBErrorExecutor
  , DuckDBErrorConstraint
  , DuckDBErrorIndex
  , DuckDBErrorStat
  , DuckDBErrorConnection
  , DuckDBErrorSyntax
  , DuckDBErrorSettings
  , DuckDBErrorBinder
  , DuckDBErrorNetwork
  , DuckDBErrorOptimizer
  , DuckDBErrorNullPointer
  , DuckDBErrorIO
  , DuckDBErrorInterrupt
  , DuckDBErrorFatal
  , DuckDBErrorInternal
  , DuckDBErrorInvalidInput
  , DuckDBErrorOutOfMemory
  , DuckDBErrorPermission
  , DuckDBErrorParameterNotResolved
  , DuckDBErrorParameterNotAllowed
  , DuckDBErrorDependency
  , DuckDBErrorHTTP
  , DuckDBErrorMissingExtension
  , DuckDBErrorAutoload
  , DuckDBErrorSequence
  , DuckDBInvalidConfiguration
  #-}

-- | Behaviour of DuckDB's casting functions (@duckdb_cast_mode@).
newtype DuckDBCastMode = DuckDBCastMode {unDuckDBCastMode :: CInt}
  deriving (Eq, Ord, Show, Storable)

-- | Pattern synonyms for @duckdb_cast_mode@ values.
pattern DuckDBCastNormal, DuckDBCastTry :: DuckDBCastMode
pattern DuckDBCastNormal = DuckDBCastMode 0
pattern DuckDBCastTry = DuckDBCastMode 1

{-# COMPLETE DuckDBCastNormal, DuckDBCastTry #-}

-- | Represents DuckDB's @duckdb_date@.
newtype DuckDBDate = DuckDBDate {unDuckDBDate :: Int32}
  deriving (Eq, Ord, Show, Storable)

-- | Decomposed representation of a @duckdb_date@.
data DuckDBDateStruct = DuckDBDateStruct
  { duckDBDateStructYear :: !Int32
  , duckDBDateStructMonth :: !Int8
  , duckDBDateStructDay :: !Int8
  }
  deriving (Eq, Show)

instance Storable DuckDBDateStruct where
  sizeOf _ = alignedSize
    where
      rawSize = sizeOf (undefined :: Int32) + 2 * sizeOf (undefined :: Int8)
      align = alignment (undefined :: Int32)
      alignedSize = ((rawSize + align - 1) `div` align) * align
  alignment _ = alignment (undefined :: Int32)
  peek ptr = do
    year <- peekByteOff ptr 0
    month <- peekByteOff ptr (sizeOf (undefined :: Int32))
    day <- peekByteOff ptr (sizeOf (undefined :: Int32) + sizeOf (undefined :: Int8))
    pure (DuckDBDateStruct year month day)
  poke ptr DuckDBDateStruct{duckDBDateStructYear = year, duckDBDateStructMonth = month, duckDBDateStructDay = day} = do
    pokeByteOff ptr 0 year
    pokeByteOff ptr (sizeOf (undefined :: Int32)) month
    pokeByteOff ptr (sizeOf (undefined :: Int32) + sizeOf (undefined :: Int8)) day

-- | Represents DuckDB's @duckdb_time@.
newtype DuckDBTime = DuckDBTime {unDuckDBTime :: Int64}
  deriving (Eq, Ord, Show, Storable)

-- | Decomposed representation of a @duckdb_time@.
data DuckDBTimeStruct = DuckDBTimeStruct
  { duckDBTimeStructHour :: !Int8
  , duckDBTimeStructMinute :: !Int8
  , duckDBTimeStructSecond :: !Int8
  , duckDBTimeStructMicros :: !Int32
  }
  deriving (Eq, Show)

instance Storable DuckDBTimeStruct where
  sizeOf _ = 2 * alignment (undefined :: Int32)
  alignment _ = alignment (undefined :: Int32)
  peek ptr = do
    hour <- peekByteOff ptr 0
    minute <- peekByteOff ptr 1
    second <- peekByteOff ptr 2
    micros <- peekByteOff ptr 4
    pure (DuckDBTimeStruct hour minute second micros)
  poke ptr DuckDBTimeStruct{duckDBTimeStructHour = hour, duckDBTimeStructMinute = minute, duckDBTimeStructSecond = second, duckDBTimeStructMicros = micros} = do
    pokeByteOff ptr 0 hour
    pokeByteOff ptr 1 minute
    pokeByteOff ptr 2 second
    pokeByteOff ptr 4 micros

-- | Represents DuckDB's @duckdb_time_ns@.
newtype DuckDBTimeNs = DuckDBTimeNs {unDuckDBTimeNs :: Int64}
  deriving (Eq, Ord, Show, Storable)

-- | Represents DuckDB's @duckdb_time_tz@.
newtype DuckDBTimeTz = DuckDBTimeTz {unDuckDBTimeTz :: Word64}
  deriving (Eq, Ord, Show, Storable)

-- | Decomposed representation of a @duckdb_time_tz@.
data DuckDBTimeTzStruct = DuckDBTimeTzStruct
  { duckDBTimeTzStructTime :: !DuckDBTimeStruct
  , duckDBTimeTzStructOffset :: !Int32
  }
  deriving (Eq, Show)

instance Storable DuckDBTimeTzStruct where
  sizeOf _ = sizeOf (undefined :: DuckDBTimeStruct) + sizeOf (undefined :: Int32)
  alignment _ = alignment (undefined :: Int32)
  peek ptr = do
    time <- peekByteOff ptr 0
    offset <- peekByteOff ptr (sizeOf (undefined :: DuckDBTimeStruct))
    pure (DuckDBTimeTzStruct time offset)
  poke ptr DuckDBTimeTzStruct{duckDBTimeTzStructTime = time, duckDBTimeTzStructOffset = offset} = do
    pokeByteOff ptr 0 time
    pokeByteOff ptr (sizeOf (undefined :: DuckDBTimeStruct)) offset

-- | Represents DuckDB's @duckdb_timestamp@.
newtype DuckDBTimestamp = DuckDBTimestamp {unDuckDBTimestamp :: Int64}
  deriving (Eq, Ord, Show, Storable)

-- | Decomposed representation of a @duckdb_timestamp@.
data DuckDBTimestampStruct = DuckDBTimestampStruct
  { duckDBTimestampStructDate :: !DuckDBDateStruct
  , duckDBTimestampStructTime :: !DuckDBTimeStruct
  }
  deriving (Eq, Show)

instance Storable DuckDBTimestampStruct where
  sizeOf _ = sizeOf (undefined :: DuckDBDateStruct) + sizeOf (undefined :: DuckDBTimeStruct)
  alignment _ = alignment (undefined :: DuckDBTimeStruct)
  peek ptr = do
    date <- peekByteOff ptr 0
    time <- peekByteOff ptr (sizeOf (undefined :: DuckDBDateStruct))
    pure (DuckDBTimestampStruct date time)
  poke ptr DuckDBTimestampStruct{duckDBTimestampStructDate = date, duckDBTimestampStructTime = time} = do
    pokeByteOff ptr 0 date
    pokeByteOff ptr (sizeOf (undefined :: DuckDBDateStruct)) time

-- | Represents DuckDB's @duckdb_timestamp_s@.
newtype DuckDBTimestampS = DuckDBTimestampS {unDuckDBTimestampS :: Int64}
  deriving (Eq, Ord, Show, Storable)

-- | Represents DuckDB's @duckdb_timestamp_ms@.
newtype DuckDBTimestampMs = DuckDBTimestampMs {unDuckDBTimestampMs :: Int64}
  deriving (Eq, Ord, Show, Storable)

-- | Represents DuckDB's @duckdb_timestamp_ns@.
newtype DuckDBTimestampNs = DuckDBTimestampNs {unDuckDBTimestampNs :: Int64}
  deriving (Eq, Ord, Show, Storable)

-- | Represents DuckDB's @duckdb_interval@.
data DuckDBInterval = DuckDBInterval
  { duckDBIntervalMonths :: !Int32
  , duckDBIntervalDays :: !Int32
  , duckDBIntervalMicros :: !Int64
  }
  deriving (Eq, Show)

instance Storable DuckDBInterval where
  sizeOf _ = 2 * sizeOf (undefined :: Int32) + sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Int64)
  peek ptr = do
    months <- peekByteOff ptr 0
    days <- peekByteOff ptr (sizeOf (undefined :: Int32))
    micros <- peekByteOff ptr (2 * sizeOf (undefined :: Int32))
    pure (DuckDBInterval months days micros)
  poke ptr (DuckDBInterval months days micros) = do
    pokeByteOff ptr 0 months
    pokeByteOff ptr (sizeOf (undefined :: Int32)) days
    pokeByteOff ptr (2 * sizeOf (undefined :: Int32)) micros

-- | Represents DuckDB's @duckdb_hugeint@.
data DuckDBHugeInt = DuckDBHugeInt
  { duckDBHugeIntLower :: !Word64
  , duckDBHugeIntUpper :: !Int64
  }
  deriving (Eq, Show)

instance Storable DuckDBHugeInt where
  sizeOf _ = sizeOf (undefined :: Word64) + sizeOf (undefined :: Int64)
  alignment _ = alignment (undefined :: Word64)
  peek ptr = do
    lower <- peekByteOff ptr 0
    upper <- peekByteOff ptr (sizeOf (undefined :: Word64))
    pure (DuckDBHugeInt lower upper)
  poke ptr (DuckDBHugeInt lower upper) = do
    pokeByteOff ptr 0 lower
    pokeByteOff ptr (sizeOf (undefined :: Word64)) upper

-- | Represents DuckDB's @duckdb_uhugeint@.
data DuckDBUHugeInt = DuckDBUHugeInt
  { duckDBUHugeIntLower :: !Word64
  , duckDBUHugeIntUpper :: !Word64
  }
  deriving (Eq, Show)

instance Storable DuckDBUHugeInt where
  sizeOf _ = 2 * sizeOf (undefined :: Word64)
  alignment _ = alignment (undefined :: Word64)
  peek ptr = do
    lower <- peekByteOff ptr 0
    upper <- peekByteOff ptr (sizeOf (undefined :: Word64))
    pure (DuckDBUHugeInt lower upper)
  poke ptr (DuckDBUHugeInt lower upper) = do
    pokeByteOff ptr 0 lower
    pokeByteOff ptr (sizeOf (undefined :: Word64)) upper

-- | Represents DuckDB's @duckdb_decimal@.
data DuckDBDecimal = DuckDBDecimal
  { duckDBDecimalWidth :: !Word8
  , duckDBDecimalScale :: !Word8
  , duckDBDecimalValue :: !DuckDBHugeInt
  }
  deriving (Eq, Show)

instance Storable DuckDBDecimal where
  sizeOf _ = valueOffset + sizeOf (undefined :: DuckDBHugeInt)
    where
      alignHuge = alignment (undefined :: DuckDBHugeInt)
      valueOffset = ((2 + alignHuge - 1) `div` alignHuge) * alignHuge
  alignment _ = alignment (undefined :: DuckDBHugeInt)
  peek ptr = do
    width <- peekByteOff ptr 0
    scale <- peekByteOff ptr 1
    let alignHuge = alignment (undefined :: DuckDBHugeInt)
        valueOffset = ((2 + alignHuge - 1) `div` alignHuge) * alignHuge
    value <- peekByteOff ptr valueOffset
    pure (DuckDBDecimal width scale value)
  poke ptr (DuckDBDecimal width scale value) = do
    pokeByteOff ptr 0 width
    pokeByteOff ptr 1 scale
    let alignHuge = alignment (undefined :: DuckDBHugeInt)
        valueOffset = ((2 + alignHuge - 1) `div` alignHuge) * alignHuge
    pokeByteOff ptr valueOffset value

-- | Represents DuckDB's @duckdb_blob@.
data DuckDBBlob = DuckDBBlob
  { duckDBBlobData :: !(Ptr ())
  , duckDBBlobSize :: !DuckDBIdx
  }
  deriving (Eq, Show)

instance Storable DuckDBBlob where
  sizeOf _ = sizeOf (undefined :: Ptr ()) + sizeOf (undefined :: DuckDBIdx)
  alignment _ = alignment (undefined :: Ptr ())
  peek ptr = do
    dat <- peekByteOff ptr 0
    len <- peekByteOff ptr (sizeOf (undefined :: Ptr ()))
    pure (DuckDBBlob dat len)
  poke ptr (DuckDBBlob dat len) = do
    pokeByteOff ptr 0 dat
    pokeByteOff ptr (sizeOf (undefined :: Ptr ())) len

-- | Represents DuckDB's @duckdb_string@.
data DuckDBString = DuckDBString
  { duckDBStringData :: !(Ptr CChar)
  , duckDBStringSize :: !DuckDBIdx
  }
  deriving (Eq, Show)

instance Storable DuckDBString where
  sizeOf _ = sizeOf (undefined :: Ptr CChar) + sizeOf (undefined :: DuckDBIdx)
  alignment _ = alignment (undefined :: Ptr CChar)
  peek ptr = do
    dat <- peekByteOff ptr 0
    len <- peekByteOff ptr (sizeOf (undefined :: Ptr CChar))
    pure (DuckDBString dat len)
  poke ptr (DuckDBString dat len) = do
    pokeByteOff ptr 0 dat
    pokeByteOff ptr (sizeOf (undefined :: Ptr CChar)) len

-- | Represents DuckDB's @duckdb_string_t@.
data DuckDBStringT

-- | Represents DuckDB's @duckdb_bit@.
data DuckDBBit = DuckDBBit
  { duckDBBitData :: !(Ptr Word8)
  , duckDBBitSize :: !DuckDBIdx
  }
  deriving (Eq, Show)

instance Storable DuckDBBit where
  sizeOf _ = sizeOf (undefined :: Ptr Word8) + sizeOf (undefined :: DuckDBIdx)
  alignment _ = alignment (undefined :: Ptr Word8)
  peek ptr = do
    dat <- peekByteOff ptr 0
    len <- peekByteOff ptr (sizeOf (undefined :: Ptr Word8))
    pure (DuckDBBit dat len)
  poke ptr (DuckDBBit dat len) = do
    pokeByteOff ptr 0 dat
    pokeByteOff ptr (sizeOf (undefined :: Ptr Word8)) len

-- | Represents DuckDB's @duckdb_bignum@.
data DuckDBBignum = DuckDBBignum
  { duckDBBignumData :: !(Ptr Word8)
  , duckDBBignumSize :: !DuckDBIdx
  , duckDBBignumIsNegative :: !CBool
  }
  deriving (Eq, Show)

instance Storable DuckDBBignum where
  sizeOf _ = alignedSize
    where
      baseSize = sizeOf (undefined :: Ptr Word8) + sizeOf (undefined :: DuckDBIdx) + sizeOf (undefined :: CBool)
      align = alignment (undefined :: Ptr Word8)
      alignedSize = ((baseSize + align - 1) `div` align) * align
  alignment _ = alignment (undefined :: Ptr Word8)
  peek ptr = do
    dat <- peekByteOff ptr 0
    len <- peekByteOff ptr (sizeOf (undefined :: Ptr Word8))
    isNeg <- peekByteOff ptr (sizeOf (undefined :: Ptr Word8) + sizeOf (undefined :: DuckDBIdx))
    pure (DuckDBBignum dat len isNeg)
  poke ptr (DuckDBBignum dat len isNeg) = do
    pokeByteOff ptr 0 dat
    pokeByteOff ptr (sizeOf (undefined :: Ptr Word8)) len
    pokeByteOff ptr (sizeOf (undefined :: Ptr Word8) + sizeOf (undefined :: DuckDBIdx)) isNeg

-- | Represents DuckDB's @duckdb_query_progress_type@.
data DuckDBQueryProgress = DuckDBQueryProgress
  { duckDBQueryProgressPercentage :: !Double
  , duckDBQueryProgressRowsProcessed :: !Word64
  , duckDBQueryProgressTotalRows :: !Word64
  }
  deriving (Eq, Show)

instance Storable DuckDBQueryProgress where
  sizeOf _ = sizeOf (undefined :: CDouble) + 2 * sizeOf (undefined :: Word64)
  alignment _ = alignment (undefined :: CDouble)
  peek ptr = do
    percentage <- realToFrac <$> (peekByteOff ptr 0 :: IO CDouble)
    let offset1 = sizeOf (undefined :: CDouble)
        offset2 = offset1 + sizeOf (undefined :: Word64)
    processed <- peekByteOff ptr offset1
    total <- peekByteOff ptr offset2
    pure (DuckDBQueryProgress percentage processed total)
  poke ptr (DuckDBQueryProgress percentage processed total) = do
    pokeByteOff ptr 0 (realToFrac percentage :: CDouble)
    let offset1 = sizeOf (undefined :: CDouble)
        offset2 = offset1 + sizeOf (undefined :: Word64)
    pokeByteOff ptr offset1 processed
    pokeByteOff ptr offset2 total

-- | Opaque DuckDB column handle.
data DuckDBColumn

-- | DuckDB result structure (opaque to callers, but required for FFI marshalling).
data DuckDBResult = DuckDBResult
  { duckDBResultDeprecatedColumnCount :: !DuckDBIdx
  , duckDBResultDeprecatedRowCount :: !DuckDBIdx
  , duckDBResultDeprecatedRowsChanged :: !DuckDBIdx
  , duckDBResultDeprecatedColumns :: !(Ptr DuckDBColumn)
  , duckDBResultDeprecatedErrorMessage :: !CString
  , duckDBResultInternalData :: !(Ptr ())
  }

instance Storable DuckDBResult where
  sizeOf _ = 3 * sizeOf (undefined :: DuckDBIdx) + 3 * sizeOf (undefined :: Ptr ())
  alignment _ = alignment (undefined :: DuckDBIdx)
  peek ptr = do
    colCount <- peekByteOff ptr 0
    rowCount <- peekByteOff ptr (sizeOf (undefined :: DuckDBIdx))
    rowsChanged <- peekByteOff ptr (2 * sizeOf (undefined :: DuckDBIdx))
    let basePtr = 3 * sizeOf (undefined :: DuckDBIdx)
    columns <- peekByteOff ptr basePtr
    errMsg <- peekByteOff ptr (basePtr + sizeOf (undefined :: Ptr ()))
    internal <- peekByteOff ptr (basePtr + 2 * sizeOf (undefined :: Ptr ()))
    pure
      DuckDBResult
        { duckDBResultDeprecatedColumnCount = colCount
        , duckDBResultDeprecatedRowCount = rowCount
        , duckDBResultDeprecatedRowsChanged = rowsChanged
        , duckDBResultDeprecatedColumns = columns
        , duckDBResultDeprecatedErrorMessage = errMsg
        , duckDBResultInternalData = internal
        }
  poke ptr result = do
    let columnCount = duckDBResultDeprecatedColumnCount result
        rowCount = duckDBResultDeprecatedRowCount result
        rowsChanged = duckDBResultDeprecatedRowsChanged result
        columns = duckDBResultDeprecatedColumns result
        errorMessage = duckDBResultDeprecatedErrorMessage result
        internalData = duckDBResultInternalData result
        basePtr = 3 * sizeOf (undefined :: DuckDBIdx)
    pokeByteOff ptr 0 columnCount
    pokeByteOff ptr (sizeOf (undefined :: DuckDBIdx)) rowCount
    pokeByteOff ptr (2 * sizeOf (undefined :: DuckDBIdx)) rowsChanged
    pokeByteOff ptr basePtr columns
    pokeByteOff ptr (basePtr + sizeOf (undefined :: Ptr ())) errorMessage
    pokeByteOff ptr (basePtr + 2 * sizeOf (undefined :: Ptr ())) internalData

-- | Tag type backing @duckdb_database@ pointers.
data DuckDBDatabaseStruct

-- | Handle to a DuckDB database instance.
type DuckDBDatabase = Ptr DuckDBDatabaseStruct

-- | Tag type backing @duckdb_connection@ pointers.
data DuckDBConnectionStruct

-- | Handle to a DuckDB connection.
type DuckDBConnection = Ptr DuckDBConnectionStruct

-- | Tag type backing @duckdb_config@ pointers.
data DuckDBConfigStruct

-- | Handle to a DuckDB configuration object.
type DuckDBConfig = Ptr DuckDBConfigStruct

-- | Tag type backing @duckdb_instance_cache@ pointers.
data DuckDBInstanceCacheStruct

-- | Handle to a DuckDB instance cache.
type DuckDBInstanceCache = Ptr DuckDBInstanceCacheStruct

-- | Tag type backing @duckdb_extracted_statements@ pointers.
data DuckDBExtractedStatementsStruct

-- | Handle to extracted SQL statements.
type DuckDBExtractedStatements = Ptr DuckDBExtractedStatementsStruct

-- | Tag type backing @duckdb_function_info@ pointers.
data DuckDBFunctionInfoStruct

-- | Handle to function execution context.
type DuckDBFunctionInfo = Ptr DuckDBFunctionInfoStruct

-- | Tag type backing @duckdb_bind_info@ pointers.
data DuckDBBindInfoStruct

-- | Handle to scalar function bind context.
type DuckDBBindInfo = Ptr DuckDBBindInfoStruct

-- | Tag type backing @duckdb_scalar_function@ pointers.
data DuckDBScalarFunctionStruct

-- | Handle to a scalar function definition.
type DuckDBScalarFunction = Ptr DuckDBScalarFunctionStruct

-- | Tag type backing @duckdb_scalar_function_set@ pointers.
data DuckDBScalarFunctionSetStruct

-- | Handle to a set of scalar function overloads.
type DuckDBScalarFunctionSet = Ptr DuckDBScalarFunctionSetStruct

-- | Tag type backing @duckdb_aggregate_function@ pointers.
data DuckDBAggregateFunctionStruct

-- | Handle to an aggregate function definition.
type DuckDBAggregateFunction = Ptr DuckDBAggregateFunctionStruct

-- | Tag type backing @duckdb_aggregate_function_set@ pointers.
data DuckDBAggregateFunctionSetStruct

-- | Handle to an aggregate function set.
type DuckDBAggregateFunctionSet = Ptr DuckDBAggregateFunctionSetStruct

-- | Tag type backing @duckdb_vector@ pointers.
data DuckDBVectorStruct

-- | Handle to a DuckDB vector.
type DuckDBVector = Ptr DuckDBVectorStruct

-- | Tag type backing @duckdb_data_chunk@ pointers.
data DuckDBDataChunkStruct

-- | Handle to a DuckDB data chunk.
type DuckDBDataChunk = Ptr DuckDBDataChunkStruct

-- | Tag type backing @duckdb_selection_vector@ pointers.
data DuckDBSelectionVectorStruct

-- | Handle to a DuckDB selection vector.
type DuckDBSelectionVector = Ptr DuckDBSelectionVectorStruct

-- | Tag type backing @duckdb_arrow_options@ pointers.
data DuckDBArrowOptionsStruct

-- | Handle to DuckDB Arrow options.
type DuckDBArrowOptions = Ptr DuckDBArrowOptionsStruct

-- | Tag type backing @duckdb_arrow@ pointers.
newtype DuckDBArrowStruct = DuckDBArrowStruct
  { duckdbArrowInternalPtr :: Ptr ()
  }

-- | Handle to an Arrow query result.
type DuckDBArrow = Ptr DuckDBArrowStruct

-- | Tag type backing @duckdb_arrow_schema@ pointers.
newtype DuckDBArrowSchemaStruct = DuckDBArrowSchemaStruct
  { duckdbArrowSchemaInternalPtr :: Ptr ()
  }

-- | Handle to an Arrow schema.
type DuckDBArrowSchema = Ptr DuckDBArrowSchemaStruct


-- | Tag type backing @duckdb_arrow_array@ pointers.
newtype DuckDBArrowArrayStruct = DuckDBArrowArrayStruct
  { duckdbArrowArrayInternalPtr :: Ptr ()
  }

-- | Handle to an Arrow array.
type DuckDBArrowArray = Ptr DuckDBArrowArrayStruct

instance Storable DuckDBArrowStruct where
  sizeOf _ = pointerSize
  alignment _ = alignment (nullPtr :: Ptr ())
  peek ptr =
    DuckDBArrowStruct
      <$> peekByteOff ptr 0
  poke ptr DuckDBArrowStruct{..} =
    pokeByteOff ptr 0 duckdbArrowInternalPtr

instance Storable DuckDBArrowSchemaStruct where
  sizeOf _ = pointerSize
  alignment _ = alignment (nullPtr :: Ptr ())
  peek ptr =
    DuckDBArrowSchemaStruct
      <$> peekByteOff ptr 0
  poke ptr DuckDBArrowSchemaStruct{..} =
    pokeByteOff ptr 0 duckdbArrowSchemaInternalPtr

instance Storable DuckDBArrowArrayStruct where
  sizeOf _ = pointerSize
  alignment _ = alignment (nullPtr :: Ptr ())
  peek ptr =
    DuckDBArrowArrayStruct
      <$> peekByteOff ptr 0
  poke ptr DuckDBArrowArrayStruct{..} =
    pokeByteOff ptr 0 duckdbArrowArrayInternalPtr


-- | Tag type backing @duckdb_arrow_converted_schema@ pointers.
data DuckDBArrowConvertedSchemaStruct

-- | Handle to a converted Arrow schema.
type DuckDBArrowConvertedSchema = Ptr DuckDBArrowConvertedSchemaStruct

-- | Tag type backing @duckdb_arrow_stream@ pointers.
newtype DuckDBArrowStreamStruct = DuckDBArrowStreamStruct
  { duckdbArrowStreamInternalPtr :: Ptr ()
  }

-- | Handle to an Arrow stream.
type DuckDBArrowStream = Ptr DuckDBArrowStreamStruct

instance Storable DuckDBArrowStreamStruct where
  sizeOf _ = pointerSize
  alignment _ = alignment (nullPtr :: Ptr ())
  peek ptr =
    DuckDBArrowStreamStruct
      <$> peekByteOff ptr 0
  poke ptr DuckDBArrowStreamStruct{..} =
    pokeByteOff ptr 0 duckdbArrowStreamInternalPtr

-- | Tag type backing @duckdb_expression@ pointers.
data DuckDBExpressionStruct

-- | Handle to a DuckDB expression.
type DuckDBExpression = Ptr DuckDBExpressionStruct

-- | Tag type backing @duckdb_client_context@ pointers.
data DuckDBClientContextStruct

-- | Handle to a DuckDB client context.
type DuckDBClientContext = Ptr DuckDBClientContextStruct

-- | Tag type backing @duckdb_prepared_statement@ pointers.
data DuckDBPreparedStatementStruct

-- | Handle to a prepared statement.
type DuckDBPreparedStatement = Ptr DuckDBPreparedStatementStruct

-- | Tag type backing @duckdb_value@ pointers.
data DuckDBValueStruct

-- | Handle to a scalar DuckDB value.
type DuckDBValue = Ptr DuckDBValueStruct

-- | Tag type backing @duckdb_pending_result@ pointers.
data DuckDBPendingResultStruct

-- | Handle to a pending (incremental) query result.
type DuckDBPendingResult = Ptr DuckDBPendingResultStruct

-- | Tag type backing @duckdb_logical_type@ pointers.
data DuckDBLogicalTypeStruct

-- | Handle to a DuckDB logical type value.
type DuckDBLogicalType = Ptr DuckDBLogicalTypeStruct

-- | Tag type backing @duckdb_create_type_info@ pointers.
data DuckDBCreateTypeInfoStruct

-- | Handle to logical type registration details.
type DuckDBCreateTypeInfo = Ptr DuckDBCreateTypeInfoStruct

-- | Tag type backing @duckdb_error_data@ pointers.
data DuckDBErrorDataStruct

-- | Handle to DuckDB error data.
type DuckDBErrorData = Ptr DuckDBErrorDataStruct

-- | Tag type backing @duckdb_init_info@ pointers.
data DuckDBInitInfoStruct

-- | Handle to table function initialization state.
type DuckDBInitInfo = Ptr DuckDBInitInfoStruct

-- | Tag type backing @duckdb_cast_function@ pointers.
data DuckDBCastFunctionStruct

-- | Handle to a cast function definition.
type DuckDBCastFunction = Ptr DuckDBCastFunctionStruct

-- | Tag type backing @duckdb_table_function@ pointers.
data DuckDBTableFunctionStruct

-- | Handle to a table function definition.
type DuckDBTableFunction = Ptr DuckDBTableFunctionStruct

-- | Tag type backing @duckdb_appender@ pointers.
data DuckDBAppenderStruct

-- | Handle to an appender.
type DuckDBAppender = Ptr DuckDBAppenderStruct

-- | Tag type backing @duckdb_table_description@ pointers.
data DuckDBTableDescriptionStruct

-- | Handle to a table description.
type DuckDBTableDescription = Ptr DuckDBTableDescriptionStruct

-- | Tag type backing @duckdb_profiling_info@ pointers.
data DuckDBProfilingInfoStruct

-- | Handle to profiling information.
type DuckDBProfilingInfo = Ptr DuckDBProfilingInfoStruct

-- | Tag type backing @duckdb_replacement_scan_info@ pointers.
data DuckDBReplacementScanInfoStruct

-- | Handle to replacement scan context.
type DuckDBReplacementScanInfo = Ptr DuckDBReplacementScanInfoStruct

-- | Handle to a DuckDB aggregate state.
data DuckDBAggregateStateStruct

-- | Opaque pointer to the aggregate-function state handed to user callbacks.
type DuckDBAggregateState = Ptr DuckDBAggregateStateStruct

-- | Handle to a DuckDB task state.
type DuckDBTaskState = Ptr ()

-- | Function pointer used to represent scalar function execution callbacks.
type DuckDBScalarFunctionFun = FunPtr (DuckDBFunctionInfo -> DuckDBDataChunk -> DuckDBVector -> IO ())

-- | Function pointer used to represent scalar function bind callbacks.
type DuckDBScalarFunctionBindFun = FunPtr (DuckDBBindInfo -> IO ())

-- | Function pointer used to destroy user-provided data blobs.
type DuckDBDeleteCallback = FunPtr (Ptr () -> IO ())

-- | Function pointer used to copy user-provided data blobs.
type DuckDBCopyCallback = FunPtr (Ptr () -> IO (Ptr ()))

-- | Function pointer implementing cast functions.
type DuckDBCastFunctionFun =
  FunPtr (DuckDBFunctionInfo -> DuckDBIdx -> DuckDBVector -> DuckDBVector -> IO CBool)

-- | Function pointer returning aggregate state size.
type DuckDBAggregateStateSizeFun = FunPtr (DuckDBFunctionInfo -> IO DuckDBIdx)

-- | Function pointer initializing aggregate state.
type DuckDBAggregateInitFun = FunPtr (DuckDBFunctionInfo -> DuckDBAggregateState -> IO ())

-- | Function pointer destroying aggregate state batches.
type DuckDBAggregateDestroyFun = FunPtr (Ptr DuckDBAggregateState -> DuckDBIdx -> IO ())

-- | Function pointer updating aggregate states.
type DuckDBAggregateUpdateFun =
  FunPtr (DuckDBFunctionInfo -> DuckDBDataChunk -> Ptr DuckDBAggregateState -> IO ())

-- | Function pointer combining aggregate states.
type DuckDBAggregateCombineFun =
  FunPtr
    ( DuckDBFunctionInfo ->
      Ptr DuckDBAggregateState ->
      Ptr DuckDBAggregateState ->
      DuckDBIdx ->
      IO ()
    )

-- | Function pointer finalising aggregate states.
type DuckDBAggregateFinalizeFun =
  FunPtr
    ( DuckDBFunctionInfo ->
      Ptr DuckDBAggregateState ->
      DuckDBVector ->
      DuckDBIdx ->
      DuckDBIdx ->
      IO ()
    )

-- | Function pointer for table function bind callbacks.
type DuckDBTableFunctionBindFun = FunPtr (DuckDBBindInfo -> IO ())

-- | Function pointer for table function init callbacks.
type DuckDBTableFunctionInitFun = FunPtr (DuckDBInitInfo -> IO ())

-- | Function pointer for table function execution callbacks.
type DuckDBTableFunctionFun = FunPtr (DuckDBFunctionInfo -> DuckDBDataChunk -> IO ())

-- | Function pointer for replacement scan callbacks.
type DuckDBReplacementCallback =
  FunPtr (DuckDBReplacementScanInfo -> CString -> Ptr () -> IO ())


-- The full Arrow C Data Interface definitions are not included here to avoid
-- introducing a dependency on the Arrow C headers. Instead, we define only the
-- parts we need for testing DuckDB's Arrow integration.
-- See https://arrow.apache.org/docs/format/CDataInterface.html for the full
-- specification.
-- #ifndef ARROW_C_DATA_INTERFACE
-- #define ARROW_C_DATA_INTERFACE

-- #define ARROW_FLAG_DICTIONARY_ORDERED 1
-- #define ARROW_FLAG_NULLABLE 2
-- #define ARROW_FLAG_MAP_KEYS_SORTED 4

-- struct ArrowSchema {
--   // Array type description
--   const char* format;
--   const char* name;
--   const char* metadata;
--   int64_t flags;
--   int64_t n_children;
--   struct ArrowSchema** children;
--   struct ArrowSchema* dictionary;

--   // Release callback
--   void (*release)(struct ArrowSchema*);
--   // Opaque producer-specific data
--   void* private_data;
-- };

-- struct ArrowArray {
--   // Array data description
--   int64_t length;
--   int64_t null_count;
--   int64_t offset;
--   int64_t n_buffers;
--   int64_t n_children;
--   const void** buffers;
--   struct ArrowArray** children;
--   struct ArrowArray* dictionary;

--   // Release callback
--   void (*release)(struct ArrowArray*);
--   // Opaque producer-specific data
--   void* private_data;
-- };

-- #endif  // ARROW_C_DATA_INTERFACE


-- | Partial Arrow schema view used for tests that require inspecting DuckDB's
-- Arrow wrappers without depending on the full Arrow C Data Interface
-- definitions.
data ArrowSchema = ArrowSchema
  { arrowSchemaFormat :: CString
  , arrowSchemaName :: CString
  , arrowSchemaMetadata :: CString
  , arrowSchemaFlags :: Int64
  , arrowSchemaChildCount :: Int64
  , arrowSchemaChildren :: Ptr (Ptr ArrowSchema)
  , arrowSchemaDictionary :: Ptr ArrowSchema
  , arrowSchemaRelease :: FunPtr (Ptr ArrowSchema -> IO ())
  , arrowSchemaPrivateData :: Ptr ()
  }

instance Storable ArrowSchema where
  sizeOf _ = pointerSize * 9
  alignment _ = alignment (nullPtr :: Ptr ())
  peek ptr =
    ArrowSchema
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr pointerSize
      <*> peekByteOff ptr (pointerSize * 2)
      <*> peekByteOff ptr (pointerSize * 3)
      <*> peekByteOff ptr (pointerSize * 4)
      <*> peekByteOff ptr (pointerSize * 5)
      <*> peekByteOff ptr (pointerSize * 6)
      <*> peekByteOff ptr (pointerSize * 7)
      <*> peekByteOff ptr (pointerSize * 8)
  poke ptr ArrowSchema{..} = do
    pokeByteOff ptr 0 arrowSchemaFormat
    pokeByteOff ptr pointerSize arrowSchemaName
    pokeByteOff ptr (pointerSize * 2) arrowSchemaMetadata
    pokeByteOff ptr (pointerSize * 3) arrowSchemaFlags
    pokeByteOff ptr (pointerSize * 4) arrowSchemaChildCount
    pokeByteOff ptr (pointerSize * 5) arrowSchemaChildren
    pokeByteOff ptr (pointerSize * 6) arrowSchemaDictionary
    pokeByteOff ptr (pointerSize * 7) arrowSchemaRelease
    pokeByteOff ptr (pointerSize * 8) arrowSchemaPrivateData

-- | Partial Arrow array view mirroring the DuckDB C API layout.
data ArrowArray = ArrowArray
  { arrowArrayLength :: Int64
  , arrowArrayNullCount :: Int64
  , arrowArrayOffset :: Int64
  , arrowArrayBufferCount :: Int64
  , arrowArrayChildCount :: Int64
  , arrowArrayBuffers :: Ptr (Ptr ())
  , arrowArrayChildren :: Ptr (Ptr ArrowArray)
  , arrowArrayDictionary :: Ptr ArrowArray
  , arrowArrayRelease :: FunPtr (Ptr ArrowArray -> IO ())
  , arrowArrayPrivateData :: Ptr ()
  }

instance Storable ArrowArray where
  sizeOf _ = pointerSize * 5 + intFieldSize * 5
  alignment _ = alignment (nullPtr :: Ptr ())
  peek ptr =
    ArrowArray
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr intFieldSize
      <*> peekByteOff ptr (intFieldSize * 2)
      <*> peekByteOff ptr (intFieldSize * 3)
      <*> peekByteOff ptr (intFieldSize * 4)
      <*> peekByteOff ptr (intFieldSize * 5)
      <*> peekByteOff ptr (intFieldSize * 5 + pointerSize)
      <*> peekByteOff ptr (intFieldSize * 5 + pointerSize * 2)
      <*> peekByteOff ptr (intFieldSize * 5 + pointerSize * 3)
      <*> peekByteOff ptr (intFieldSize * 5 + pointerSize * 4)
  poke ptr ArrowArray{..} = do
    pokeByteOff ptr 0 arrowArrayLength
    pokeByteOff ptr intFieldSize arrowArrayNullCount
    pokeByteOff ptr (intFieldSize * 2) arrowArrayOffset
    pokeByteOff ptr (intFieldSize * 3) arrowArrayBufferCount
    pokeByteOff ptr (intFieldSize * 4) arrowArrayChildCount
    pokeByteOff ptr (intFieldSize * 5) arrowArrayBuffers
    pokeByteOff ptr (intFieldSize * 5 + pointerSize) arrowArrayChildren
    pokeByteOff ptr (intFieldSize * 5 + pointerSize * 2) arrowArrayDictionary
    pokeByteOff ptr (intFieldSize * 5 + pointerSize * 3) arrowArrayRelease
    pokeByteOff ptr (intFieldSize * 5 + pointerSize * 4) arrowArrayPrivateData

newtype ArrowSchemaPtr = ArrowSchemaPtr {unArrowSchemaPtr :: Ptr ArrowSchema}
  deriving (Eq)

newtype ArrowArrayPtr = ArrowArrayPtr {unArrowArrayPtr :: Ptr ArrowArray}
  deriving (Eq)

newtype ArrowStreamPtr = ArrowStreamPtr {unArrowStreamPtr :: Ptr ()}
  deriving (Eq)

pointerSize :: Int
pointerSize = sizeOf (nullPtr :: Ptr ())

intFieldSize :: Int
intFieldSize = sizeOf (0 :: Int64)
