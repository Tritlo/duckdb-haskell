{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Advanced V2 registration with Haskell callbacks. The library owns the callback
pointers. DuckDB releases them when it releases the registered object. Callback
info and context handles are borrowed and must not escape the callback. Use
@Database.DuckDB.FFI.V2@ operations with 'checked' and 'output' in callbacks.

The registration user-data slot is reserved for callback cleanup. Use Haskell
closures for registration data and 'setCallbackState' for bind or execution
state. A state getter with a different type than its setter raises an error.
Synchronize access to state that DuckDB shares between workers.

The pinned V2 API has no custom file-system registration API. Use
@Database.DuckDB.Simple.Logging@ for managed log-sink registration.
-}
module Database.DuckDB.Simple.Registration (
    RegistrationTarget (..),
    withRegistrationFactory,
    TypeDeclaration (..),
    DefaultValue (..),
    defaultValue,
    FunctionParameter (..),
    parameter,
    FunctionSignature (..),
    FunctionProperties,
    ContextCallback,
    BindCallback,
    ScalarCallbacks (..),
    scalarCallbacks,
    AggregateCallbacks (..),
    aggregateCallbacks,
    TableCallbacks (..),
    tableCallbacks,
    registerScalarFunction,
    registerScalarFunctionWith,
    registerAggregateFunction,
    registerAggregateFunctionWith,
    registerTableFunction,
    registerTableFunctionWith,
    registerCast,
    registerCastWith,
    registerCustomType,
    registerCustomTypeWith,
    registerReplacementScan,
    registerReplacementScanWith,
    registerInstanceReplacementScan,
    setCallbackState,
    getCallbackState,
    withTypeDeclaration,
    withTypeDeclarationWithFactory,
    configureFunctionSignature,
    configureFunctionSignatureWithFactory,
    checked,
    output,
    destroy,
    nullHandle,
    withText,
    readText,
    withIdentifier,
    readIdentifier,
) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM_, when)
import Data.Dynamic (dynTypeRep, fromDynamic, toDyn)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, typeRep)
import Data.Void (Void)
import Database.DuckDB.Simple.Callback (RegistrationTarget (..), attachOpaque, guardContext, install, installOptional, readCallbackState, runCallback, transferCallbackState, withCallbackResources, withRegistrationFactory, withTarget)
import Database.DuckDB.Simple.Internal (Connection, Instance, withConnectionHandle, withInstanceHandle)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep, logicalTypeFromRep, logicalTypeFromRepWithFactory)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.ToField (ToDuckValue (..))
import Foreign.Ptr (Ptr)

-- | A structured type or a SQL type expression, including a registered alias.
data TypeDeclaration = StructuredType LogicalTypeRep | TypeExpression Text
    deriving (Eq, Show)

{- | Construct an owned native default value. Registration releases the value.
Use 'FactoryDefaultValue' for extension signatures. The connection constructor
also supports SQL-derived values through 'defaultValue'.
-}
data DefaultValue
    = DefaultValue (DuckDBV2ConnectionHandle -> IO DuckDBV2ValueHandle)
    | FactoryDefaultValue (DuckDBV2FactoryHandle -> IO DuckDBV2ValueHandle)

{- | Use a typed Haskell value as a parameter default. The value needs a
connection, so registration on an 'ExtensionTarget' raises 'SQLError'. Use
'FactoryDefaultValue' there.
-}
defaultValue :: (ToDuckValue a) => a -> DefaultValue
defaultValue value = DefaultValue (\connection -> toDuckValue connection value)

-- | One parameter. Native V2 validates parameter order, names, and defaults.
data FunctionParameter = FunctionParameter
    { parameterName :: Text
    , parameterType :: TypeDeclaration
    , parameterDefault :: Maybe DefaultValue
    , parameterKind :: DuckDBV2FunctionParameterKind
    }

-- | Declare a required parameter accepted by position or by name.
parameter :: Text -> TypeDeclaration -> FunctionParameter
parameter name dtype = FunctionParameter name dtype Nothing DuckDBV2FunctionParameterKindStandard

-- | Function parameters and an optional return type. Table functions use Nothing.
data FunctionSignature = FunctionSignature
    { signatureParameters :: [FunctionParameter]
    , signatureReturnType :: Maybe TypeDeclaration
    }

-- | Native function properties. Scalar and aggregate functions default to fallible.
type FunctionProperties = [(DuckDBV2FunctionPropertyKey, DuckDBV2FunctionPropertyValue)]

-- | A callback with a borrowed execution context. Exceptions become SQL errors.
type ContextCallback info = info -> DuckDBV2ContextHandle -> IO ()

-- | A bind callback with borrowed input metadata, output metadata, and context.
type BindCallback result = DuckDBV2FunctionBindInfoHandle -> result -> DuckDBV2ContextHandle -> IO ()

-- | All scalar function callbacks. The exec callback is required.
data ScalarCallbacks = ScalarCallbacks
    { scalarResolveTypesCallback :: Maybe (BindCallback DuckDBV2ScalarFunctionResolveTypesInfoHandle)
    , scalarBindCallback :: Maybe (BindCallback DuckDBV2ScalarFunctionBindInfoHandle)
    , scalarInitCallback :: Maybe (ContextCallback DuckDBV2ScalarFunctionInitInfoHandle)
    , scalarExecCallback :: ContextCallback DuckDBV2ScalarFunctionExecInfoHandle
    }

-- | Use one exec callback and no optional scalar callbacks.
scalarCallbacks :: ContextCallback DuckDBV2ScalarFunctionExecInfoHandle -> ScalarCallbacks
scalarCallbacks execute = ScalarCallbacks Nothing Nothing Nothing execute

{- | All aggregate callbacks. Bind and state destruction are optional.
Aggregate state callbacks have no context in native V2. Native cleanup ignores
destroy-callback errors after exception containment.
-}
data AggregateCallbacks = AggregateCallbacks
    { aggregateBindCallback :: Maybe (BindCallback DuckDBV2AggregateFunctionBindInfoHandle)
    , aggregateSizeCallback :: DuckDBV2AggregateFunctionSizeInfoHandle -> IO ()
    , aggregateInitCallback :: DuckDBV2AggregateFunctionInitInfoHandle -> IO ()
    , aggregateUpdateCallback :: DuckDBV2AggregateFunctionUpdateInfoHandle -> IO ()
    , aggregateCombineCallback :: DuckDBV2AggregateFunctionCombineInfoHandle -> IO ()
    , aggregateFinalizeCallback :: DuckDBV2AggregateFunctionFinalizeInfoHandle -> IO ()
    , aggregateDestroyCallback :: Maybe (DuckDBV2AggregateFunctionDestroyInfoHandle -> IO ())
    }

-- | Use the required aggregate callbacks and no optional callbacks.
aggregateCallbacks ::
    (DuckDBV2AggregateFunctionSizeInfoHandle -> IO ()) ->
    (DuckDBV2AggregateFunctionInitInfoHandle -> IO ()) ->
    (DuckDBV2AggregateFunctionUpdateInfoHandle -> IO ()) ->
    (DuckDBV2AggregateFunctionCombineInfoHandle -> IO ()) ->
    (DuckDBV2AggregateFunctionFinalizeInfoHandle -> IO ()) ->
    AggregateCallbacks
aggregateCallbacks size initialize update combine finalize =
    AggregateCallbacks Nothing size initialize update combine finalize Nothing

{- | All table callbacks and the projection-pushdown setting.
The claim-batch callback is part of the pinned unstable V2 API.
-}
data TableCallbacks = TableCallbacks
    { tableBindCallback :: BindCallback DuckDBV2TableFunctionBindInfoHandle
    , tableInitGlobalCallback :: Maybe (ContextCallback DuckDBV2TableFunctionInitGlobalInfoHandle)
    , tableInitLocalCallback :: Maybe (ContextCallback DuckDBV2TableFunctionInitLocalInfoHandle)
    , tableExecCallback :: ContextCallback DuckDBV2TableFunctionExecInfoHandle
    , tableProgressCallback :: Maybe (ContextCallback DuckDBV2TableFunctionProgressInfoHandle)
    , tableFilterPushdownCallback :: Maybe (ContextCallback DuckDBV2TableFunctionFilterPushdownInfoHandle)
    , tablePartitionDataCallback :: Maybe (ContextCallback DuckDBV2TableFunctionPartitionDataInfoHandle)
    , tablePartitioningCallback :: Maybe (ContextCallback DuckDBV2TableFunctionPartitioningInfoHandle)
    , tableClaimBatchCallback :: Maybe (ContextCallback DuckDBV2TableFunctionClaimBatchInfoHandle)
    , tableGetBindInfoCallback :: Maybe (ContextCallback DuckDBV2TableFunctionGetBindInfoInfoHandle)
    , tableProjectionPushdown :: Bool
    }

-- | Use bind and exec callbacks. Disable optional callbacks and projection pushdown.
tableCallbacks :: BindCallback DuckDBV2TableFunctionBindInfoHandle -> ContextCallback DuckDBV2TableFunctionExecInfoHandle -> TableCallbacks
tableCallbacks bind execute = TableCallbacks bind Nothing Nothing execute Nothing Nothing Nothing Nothing Nothing Nothing False

{- | Register an advanced scalar function on the connection's database.
Set an ANY return type only when the resolve-types callback supplies a concrete type.
-}
registerScalarFunction :: Connection -> Text -> FunctionSignature -> FunctionProperties -> ScalarCallbacks -> IO ()
registerScalarFunction connection name signature properties callbacks = do
    validateName name
    withConnectionHandle connection \native ->
        registerScalarFunctionWith
            (ConnectionTarget connection)
            ( \function -> do
                withIdentifier name $ checked . c_duckdb_v2_scalar_function_set_name function
                output (c_duckdb_v2_scalar_function_get_signature function) >>= configureFunctionSignature native signature
                forM_ properties $ \(key, value) -> checked (c_duckdb_v2_scalar_function_set_property function key value)
            )
            callbacks

{- | Configure and register a managed scalar function on a connection or extension.
The configuration action borrows the builder. Do not set callbacks or user data.
-}
registerScalarFunctionWith :: RegistrationTarget -> (DuckDBV2ScalarFunctionHandle -> IO ()) -> ScalarCallbacks -> IO ()
registerScalarFunctionWith target configure callbacks =
    withTarget
        target
        c_duckdb_v2_scalar_function_create
        c_duckdb_v2_scalar_function_destroy
        c_duckdb_v2_connection_register_scalar_function
        c_duckdb_v2_extension_register_scalar_function
        \function register ->
            withCallbackResources
                ( \allocate ->
                    sequence_
                        <$> sequence
                            [ installOptional (scalarResolveTypesCallback callbacks) $ \callback -> install allocate DuckDBV2ScalarFunctionResolveTypesCallbackFn (DuckDBV2ScalarFunctionResolveTypesCallbackFn_Aux (guardBind callback)) (c_duckdb_v2_scalar_function_set_resolve_types_callback function)
                            , installOptional (scalarBindCallback callbacks) $ \callback -> install allocate DuckDBV2ScalarFunctionBindCallbackFn (DuckDBV2ScalarFunctionBindCallbackFn_Aux (guardBind callback)) (c_duckdb_v2_scalar_function_set_bind_callback function)
                            , installOptional (scalarInitCallback callbacks) $ \callback -> install allocate DuckDBV2ScalarFunctionInitCallbackFn (DuckDBV2ScalarFunctionInitCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_scalar_function_set_init_callback function)
                            , install allocate DuckDBV2ScalarFunctionExecCallbackFn (DuckDBV2ScalarFunctionExecCallbackFn_Aux (guardContext (scalarExecCallback callbacks))) (c_duckdb_v2_scalar_function_set_exec_callback function)
                            ]
                )
                (attachOpaque (c_duckdb_v2_scalar_function_set_user_data function))
                \installCallbacks -> do
                    checked (c_duckdb_v2_scalar_function_set_property function DuckDBV2FunctionPropertyFallibility DuckDBV2FunctionPropertyFallibilityFallible)
                    configure function
                    installCallbacks
                    register

{- | Register an aggregate function on the connection's database.
State memory and its size follow the native V2 aggregate contract.
-}
registerAggregateFunction :: Connection -> Text -> FunctionSignature -> FunctionProperties -> AggregateCallbacks -> IO ()
registerAggregateFunction connection name signature properties callbacks = do
    validateName name
    withConnectionHandle connection \native ->
        registerAggregateFunctionWith
            (ConnectionTarget connection)
            ( \function -> do
                withIdentifier name $ checked . c_duckdb_v2_aggregate_function_set_name function
                output (c_duckdb_v2_aggregate_function_get_signature function) >>= configureFunctionSignature native signature
                forM_ properties $ \(key, value) -> checked (c_duckdb_v2_aggregate_function_set_property function key value)
            )
            callbacks

{- | Configure and register a managed aggregate function on a connection or extension.
The configuration action borrows the builder. Do not set callbacks or user data.
-}
registerAggregateFunctionWith :: RegistrationTarget -> (DuckDBV2AggregateFunctionHandle -> IO ()) -> AggregateCallbacks -> IO ()
registerAggregateFunctionWith target configure callbacks =
    withTarget
        target
        c_duckdb_v2_aggregate_function_create
        c_duckdb_v2_aggregate_function_destroy
        c_duckdb_v2_connection_register_aggregate_function
        c_duckdb_v2_extension_register_aggregate_function
        \function register ->
            withCallbackResources
                ( \allocate ->
                    sequence_
                        <$> sequence
                            [ installOptional (aggregateBindCallback callbacks) $ \callback -> install allocate DuckDBV2AggregateFunctionBindCallbackFn (DuckDBV2AggregateFunctionBindCallbackFn_Aux (guardBind callback)) (c_duckdb_v2_aggregate_function_set_bind_callback function)
                            , install allocate DuckDBV2AggregateFunctionSizeCallbackFn (DuckDBV2AggregateFunctionSizeCallbackFn_Aux (guardCallback (aggregateSizeCallback callbacks))) (c_duckdb_v2_aggregate_function_set_size_callback function)
                            , install allocate DuckDBV2AggregateFunctionInitCallbackFn (DuckDBV2AggregateFunctionInitCallbackFn_Aux (guardCallback (aggregateInitCallback callbacks))) (c_duckdb_v2_aggregate_function_set_init_callback function)
                            , install allocate DuckDBV2AggregateFunctionUpdateCallbackFn (DuckDBV2AggregateFunctionUpdateCallbackFn_Aux (guardCallback (aggregateUpdateCallback callbacks))) (c_duckdb_v2_aggregate_function_set_update_callback function)
                            , install allocate DuckDBV2AggregateFunctionCombineCallbackFn (DuckDBV2AggregateFunctionCombineCallbackFn_Aux (guardCallback (aggregateCombineCallback callbacks))) (c_duckdb_v2_aggregate_function_set_combine_callback function)
                            , install allocate DuckDBV2AggregateFunctionFinalizeCallbackFn (DuckDBV2AggregateFunctionFinalizeCallbackFn_Aux (guardCallback (aggregateFinalizeCallback callbacks))) (c_duckdb_v2_aggregate_function_set_finalize_callback function)
                            , installOptional (aggregateDestroyCallback callbacks) $ \callback -> install allocate DuckDBV2AggregateFunctionDestroyCallbackFn (DuckDBV2AggregateFunctionDestroyCallbackFn_Aux (guardCallback callback)) (c_duckdb_v2_aggregate_function_set_destroy_callback function)
                            ]
                )
                (attachOpaque (c_duckdb_v2_aggregate_function_set_user_data function))
                \installCallbacks -> do
                    checked (c_duckdb_v2_aggregate_function_set_property function DuckDBV2FunctionPropertyFallibility DuckDBV2FunctionPropertyFallibilityFallible)
                    configure function
                    installCallbacks
                    register

-- | Register a table function. Its bind callback declares the result columns.
registerTableFunction :: Connection -> Text -> [FunctionParameter] -> TableCallbacks -> IO ()
registerTableFunction connection name parameters callbacks = do
    validateName name
    withConnectionHandle connection \native ->
        registerTableFunctionWith
            (ConnectionTarget connection)
            ( \function -> do
                withIdentifier name $ checked . c_duckdb_v2_table_function_set_name function
                output (c_duckdb_v2_table_function_get_signature function) >>= configureFunctionSignature native (FunctionSignature parameters Nothing)
            )
            callbacks

{- | Configure and register a managed table function on a connection or extension.
The configuration action borrows the builder. Do not set callbacks or user data.
-}
registerTableFunctionWith :: RegistrationTarget -> (DuckDBV2TableFunctionHandle -> IO ()) -> TableCallbacks -> IO ()
registerTableFunctionWith target configure callbacks =
    withTarget
        target
        c_duckdb_v2_table_function_create
        c_duckdb_v2_table_function_destroy
        c_duckdb_v2_connection_register_table_function
        c_duckdb_v2_extension_register_table_function
        \function register ->
            withCallbackResources
                ( \allocate ->
                    sequence_
                        <$> sequence
                            [ install allocate DuckDBV2TableFunctionBindCallbackFn (DuckDBV2TableFunctionBindCallbackFn_Aux (guardBind (tableBindCallback callbacks))) (c_duckdb_v2_table_function_set_bind_callback function)
                            , installOptional (tableInitGlobalCallback callbacks) $ \callback -> install allocate DuckDBV2TableFunctionInitGlobalCallbackFn (DuckDBV2TableFunctionInitGlobalCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_table_function_set_init_global_callback function)
                            , installOptional (tableInitLocalCallback callbacks) $ \callback -> install allocate DuckDBV2TableFunctionInitLocalCallbackFn (DuckDBV2TableFunctionInitLocalCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_table_function_set_init_local_callback function)
                            , install allocate DuckDBV2TableFunctionExecCallbackFn (DuckDBV2TableFunctionExecCallbackFn_Aux (guardContext (tableExecCallback callbacks))) (c_duckdb_v2_table_function_set_exec_callback function)
                            , installOptional (tableProgressCallback callbacks) $ \callback -> install allocate DuckDBV2TableFunctionProgressCallbackFn (DuckDBV2TableFunctionProgressCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_table_function_set_progress_callback function)
                            , installOptional (tableFilterPushdownCallback callbacks) $ \callback -> install allocate DuckDBV2TableFunctionFilterPushdownCallbackFn (DuckDBV2TableFunctionFilterPushdownCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_table_function_set_filter_pushdown_callback function)
                            , installOptional (tablePartitionDataCallback callbacks) $ \callback -> install allocate DuckDBV2TableFunctionPartitionDataCallbackFn (DuckDBV2TableFunctionPartitionDataCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_table_function_set_partition_data_callback function)
                            , installOptional (tablePartitioningCallback callbacks) $ \callback -> install allocate DuckDBV2TableFunctionPartitioningCallbackFn (DuckDBV2TableFunctionPartitioningCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_table_function_set_partitioning_callback function)
                            , installOptional (tableClaimBatchCallback callbacks) $ \callback -> install allocate DuckDBV2TableFunctionClaimBatchCallbackFn (DuckDBV2TableFunctionClaimBatchCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_table_function_set_claim_batch_callback function)
                            , installOptional (tableGetBindInfoCallback callbacks) $ \callback -> install allocate DuckDBV2TableFunctionGetBindInfoCallbackFn (DuckDBV2TableFunctionGetBindInfoCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_table_function_set_get_bind_info_callback function)
                            ]
                )
                (attachOpaque (c_duckdb_v2_table_function_set_user_data function))
                \installCallbacks -> do
                    checked (c_duckdb_v2_table_function_set_projection_pushdown function (if tableProjectionPushdown callbacks then 1 else 0))
                    configure function
                    installCallbacks
                    register

{- | Register a cast. Nothing keeps the native explicit-cast cost.
The callback must inspect the native cast mode to implement TRY_CAST behavior.
-}
registerCast :: Connection -> TypeDeclaration -> TypeDeclaration -> Maybe Int64 -> ContextCallback DuckDBV2CastFunctionExecInfoHandle -> IO ()
registerCast connection source target implicitCost execute =
    withConnectionHandle connection \native ->
        registerCastWith
            (ConnectionTarget connection)
            ( \function -> do
                withTypeDeclaration native source $ checked . c_duckdb_v2_cast_function_set_source_type function
                withTypeDeclaration native target $ checked . c_duckdb_v2_cast_function_set_target_type function
                forM_ implicitCost $ checked . c_duckdb_v2_cast_function_set_implicit_cast_cost function
            )
            execute

{- | Configure and register a managed cast on a connection or extension.
The configuration action borrows the builder. Do not set callbacks or user data.
-}
registerCastWith :: RegistrationTarget -> (DuckDBV2CastFunctionHandle -> IO ()) -> ContextCallback DuckDBV2CastFunctionExecInfoHandle -> IO ()
registerCastWith target configure execute =
    withTarget target c_duckdb_v2_cast_function_create c_duckdb_v2_cast_function_destroy c_duckdb_v2_connection_register_cast_function c_duckdb_v2_extension_register_cast_function \function register ->
        withCallbackResources
            (\allocate -> install allocate DuckDBV2CastFunctionExecCallbackFn (DuckDBV2CastFunctionExecCallbackFn_Aux (guardContext execute)) (c_duckdb_v2_cast_function_set_exec_callback function))
            (attachOpaque (c_duckdb_v2_cast_function_set_user_data function))
            \installCallback -> do
                configure function
                installCallback
                register

-- | Register a custom type with a native base type. Casts are registered separately.
registerCustomType :: Connection -> Text -> TypeDeclaration -> IO ()
registerCustomType connection name baseType = do
    validateName name
    withConnectionHandle connection \native ->
        registerCustomTypeWith (ConnectionTarget connection) \custom -> do
            withIdentifier name $ checked . c_duckdb_v2_custom_type_set_name custom
            withTypeDeclaration native baseType $ checked . c_duckdb_v2_custom_type_set_base_type custom

{- | Configure and register a custom type on a connection or extension.
The configuration action borrows the builder.
-}
registerCustomTypeWith :: RegistrationTarget -> (DuckDBV2CustomTypeHandle -> IO ()) -> IO ()
registerCustomTypeWith target configure =
    withTarget target c_duckdb_v2_custom_type_create c_duckdb_v2_custom_type_destroy c_duckdb_v2_connection_register_custom_type c_duckdb_v2_extension_register_custom_type \custom register ->
        configure custom >> register

-- | Register a connection-local replacement scan. It precedes instance scans.
registerReplacementScan :: Connection -> ContextCallback DuckDBV2ReplacementScanInfoHandle -> IO ()
registerReplacementScan connection = registerReplacementScanWith (ConnectionTarget connection)

{- | Register a managed replacement scan on a connection or loading extension.
Extension scans belong to the instance.
-}
registerReplacementScanWith :: RegistrationTarget -> ContextCallback DuckDBV2ReplacementScanInfoHandle -> IO ()
registerReplacementScanWith target callback =
    withTarget target c_duckdb_v2_replacement_scan_create c_duckdb_v2_replacement_scan_destroy c_duckdb_v2_connection_register_replacement_scan c_duckdb_v2_extension_register_replacement_scan (configureScan callback)

-- | Register an instance-wide replacement scan. It is shared by its connections.
registerInstanceReplacementScan :: Instance -> ContextCallback DuckDBV2ReplacementScanInfoHandle -> IO ()
registerInstanceReplacementScan instance' callback = withInstanceHandle instance' $ \native ->
    withInstanceFactory native $ \factory ->
        bracket (output (c_duckdb_v2_replacement_scan_create factory)) (destroy c_duckdb_v2_replacement_scan_destroy) $ \scan ->
            configureScan callback scan (checked (c_duckdb_v2_instance_register_replacement_scan native scan))

{- | Store Haskell state in a native-owned bind, global, local, or init data slot.
Pass the corresponding native opaque setter. Do not use registration user data.
This helper uses native default equality. Use raw opaque callbacks for custom equality.
Transfer a value at most once to each native state slot. Native init and batch
setters do not release replaced pointers. Store an @IORef@ or another mutable
state cell when the callback must change its state.
-}
setCallbackState :: (Typeable a) => (Ptr DuckDBV2Opaque -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> a -> IO ()
setCallbackState setter = transferCallbackState (attachOpaque setter) . toDyn

{- | Read state previously stored with 'setCallbackState'.
Pass the corresponding native pointer getter. A missing state or a state of
another type raises an error.
-}
getCallbackState :: forall a. (Typeable a) => (Ptr (Ptr Void) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO a
getCallbackState getter = do
    stored <- readCallbackState getter
    case fromDynamic stored of
        Just value -> pure value
        Nothing ->
            throwInvalidArgument $
                "callback state has type " <> Text.pack (show (dynTypeRep stored)) <> ", not " <> Text.pack (show (typeRep (Proxy :: Proxy a)))

-- | Construct a type, borrow it for an action, and release it on every exit path.
withTypeDeclaration :: DuckDBV2ConnectionHandle -> TypeDeclaration -> (DuckDBV2LogicalTypeHandle -> IO a) -> IO a
withTypeDeclaration connection declaration action = case declaration of
    StructuredType rep -> bracket (logicalTypeFromRep connection rep) (destroy c_duckdb_v2_logical_type_destroy) action
    TypeExpression sql -> withConnectionFactory connection $ \factory -> withTypeDeclarationWithFactory factory (TypeExpression sql) action

-- | Construct a type through a borrowed factory. This also supports extension contexts.
withTypeDeclarationWithFactory :: DuckDBV2FactoryHandle -> TypeDeclaration -> (DuckDBV2LogicalTypeHandle -> IO a) -> IO a
withTypeDeclarationWithFactory factory declaration = bracket acquire (destroy c_duckdb_v2_logical_type_destroy)
  where
    acquire = case declaration of
        StructuredType rep -> logicalTypeFromRepWithFactory factory rep
        TypeExpression sql -> withText sql (output . c_duckdb_v2_factory_create_type_from_text factory)

-- | Add parameters and an optional return type to a borrowed native signature.
configureFunctionSignature :: DuckDBV2ConnectionHandle -> FunctionSignature -> DuckDBV2FunctionSignatureHandle -> IO ()
configureFunctionSignature connection declaration signature =
    withConnectionFactory connection $ \factory ->
        configureSignature (withTypeDeclaration connection) (makeDefault factory) declaration signature
  where
    makeDefault _ (DefaultValue make) = make connection
    makeDefault factory (FactoryDefaultValue make) = make factory

{- | Add a signature through a borrowed factory. Defaults must use
'FactoryDefaultValue'. The action does not need a connection and can run in an
extension load callback.
-}
configureFunctionSignatureWithFactory :: DuckDBV2FactoryHandle -> FunctionSignature -> DuckDBV2FunctionSignatureHandle -> IO ()
configureFunctionSignatureWithFactory factory = configureSignature (withTypeDeclarationWithFactory factory) makeDefault
  where
    makeDefault (FactoryDefaultValue make) = make factory
    makeDefault (DefaultValue _) = throwIO (SQLError "duckdb-simple: extension defaults require FactoryDefaultValue" Nothing Nothing)

configureSignature ::
    (forall a. TypeDeclaration -> (DuckDBV2LogicalTypeHandle -> IO a) -> IO a) ->
    (DefaultValue -> IO DuckDBV2ValueHandle) ->
    FunctionSignature ->
    DuckDBV2FunctionSignatureHandle ->
    IO ()
configureSignature withType makeDefault FunctionSignature{signatureParameters, signatureReturnType} signature = do
    forM_ signatureParameters $ \FunctionParameter{parameterName, parameterType, parameterDefault, parameterKind} ->
        withIdentifier parameterName $ \name ->
            withType parameterType $ \dtype ->
                case parameterDefault of
                    Nothing -> checked (c_duckdb_v2_function_signature_add_parameter signature name dtype nullHandle parameterKind)
                    Just value -> bracket (makeDefault value) (destroy c_duckdb_v2_value_destroy) $ \native ->
                        checked (c_duckdb_v2_function_signature_add_parameter signature name dtype native parameterKind)
    forM_ signatureReturnType $ \dtype ->
        withType dtype $ checked . c_duckdb_v2_function_signature_set_return_type signature

configureScan :: ContextCallback DuckDBV2ReplacementScanInfoHandle -> DuckDBV2ReplacementScanHandle -> IO () -> IO ()
configureScan callback scan register =
    withCallbackResources
        (\allocate -> install allocate DuckDBV2ReplacementScanCallbackFn (DuckDBV2ReplacementScanCallbackFn_Aux (guardContext callback)) (c_duckdb_v2_replacement_scan_set_callback scan))
        (attachOpaque (c_duckdb_v2_replacement_scan_set_user_data scan))
        \installCallback -> installCallback >> register

guardCallback :: (info -> IO ()) -> info -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
guardCallback action info err = runCallback err (action info)

guardBind :: BindCallback result -> DuckDBV2FunctionBindInfoHandle -> result -> DuckDBV2ContextHandle -> Ptr DuckDBV2ErrorInfoHandle -> IO ()
guardBind action info result context err = runCallback err (action info result context)

validateName :: Text -> IO ()
validateName name =
    when (Text.null name || Text.any (== '\0') name) $
        throwInvalidArgument "registration name must be nonempty and must not contain NUL"
