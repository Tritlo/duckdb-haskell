{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.DuckDB.Simple.Typed.Aggregate
Description : Register an aggregate function as a fold over Haskell values.

The Haskell state of a group lives outside DuckDB. The native state holds a
'StablePtr' to an 'IORef' with the Haskell state. DuckDB can move a native
state with a byte copy. Thus the update and combine callbacks change only the
'IORef'. After init, only a release changes the native state: it writes null
to the native state.

The bind data of each call site holds a state table with the live stable
pointers. A release removes the stable pointer from the table, and only the
caller that removes it frees it. The destructor of the bind data frees the
stable pointers that remain. A call site without bind data has no table. Its
states then work, but only the destroy callback and a failed finalize release
them.
-}
module Database.DuckDB.Simple.Typed.Aggregate (
    Aggregate (..),
    createAggregateOn,
) where

import Control.Exception (evaluate, finally, mask_, onException, throwIO)
import Control.Monad (forM_, unless, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntSet as IntSet
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Database.DuckDB.Simple.Callback (attachOpaque, ignoreCallbackExceptions)
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (FieldNull))
import Database.DuckDB.Simple.FromRow (FromRow (..), parseRow)
import Database.DuckDB.Simple.Function.Internal (FunctionResult (..), ScalarType, duckTypeForScalar, prepareResultWriter)
import Database.DuckDB.Simple.LogicalRep (LogicalTypeRep (LogicalTypeScalar))
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.Typed (TypedRow (..), argumentName, conversionMessage, prepareArgumentReader)
import Database.DuckDB.Simple.V2.Registration (
    AggregateCallbacks (..),
    BindCallback,
    FunctionParameter (..),
    FunctionSignature (..),
    RegistrationTarget,
    TypeDeclaration (..),
    aggregateCallbacks,
    configureFunctionSignature,
    parameter,
    registerAggregateFunctionWith,
    withRegistrationFactory,
 )
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, intPtrToPtr, nullPtr, ptrToIntPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peek, peekElemOff, poke, sizeOf)

{- | An aggregate function as a fold. DuckDB keeps one state for each group, or
more than one partial state.
-}
data Aggregate state args result = Aggregate
    { aggregateInitial :: state
    -- ^ The state of a group with no rows.
    , aggregateStep :: state -> args -> IO state
    -- ^ Add one row to a state.
    , aggregateCombine :: state -> state -> IO state
    -- ^ Merge the second partial state of a group into the first.
    , aggregateResult :: state -> IO result
    -- ^ Give the result of a group.
    }

{- | The live states of one call site, as the keys of their stable pointers.

DuckDB 2.0.0 does not run the destroy callback on these states. In each case
the table releases them:

1. The states of a finalize call that fails, in a hash aggregate and in a
   window.
2. The states that no finalize call gets when LIMIT stops a parallel hash
   aggregate.
3. The states after a combine call fails in a parallel hash aggregate.
4. The states after a finalize call fails in a parallel hash aggregate.
-}
type StateTable = IORef IntSet.IntSet

-- | Register a typed aggregate function on a connection or a loading extension.
createAggregateOn :: forall state args result. (FromRow args, TypedRow args, FunctionResult result) => RegistrationTarget -> Text -> Aggregate state args result -> IO ()
createAggregateOn target name aggregate = do
    when (Text.null name || Text.any (== '\0') name) $
        throwInvalidArgument "aggregate function name must be nonempty and must not contain NUL"
    withRegistrationFactory target \factory -> registerAggregateFunctionWith target (configure factory) callbacks
  where
    columnTypes = rowColumnTypes (Proxy :: Proxy args)
    resultType = scalarReturnType (Proxy :: Proxy result)
    parameters =
        [ (parameter (argumentName index) (TypeExpression columnType)){parameterKind = DuckDBV2FunctionParameterKindPositionalOnly}
        | (index, columnType) <- zip [0 ..] columnTypes
        ]
    configure factory function = do
        withIdentifier name (checked . c_duckdb_v2_aggregate_function_set_name function)
        signature <- output (c_duckdb_v2_aggregate_function_get_signature function)
        configureFunctionSignature factory (FunctionSignature parameters (Just (StructuredType (LogicalTypeScalar (duckTypeForScalar resultType))))) signature
        checked (c_duckdb_v2_aggregate_function_set_property function DuckDBV2FunctionPropertyNullHandling DuckDBV2FunctionPropertyNullHandlingSpecial)
    callbacks =
        (aggregateCallbacks setStateSize (initializeStates aggregate) (updateStates name aggregate (length columnTypes)) (combineStates aggregate) (finalizeStates aggregate resultType))
            { aggregateBindCallback = Just bindStateTable
            , aggregateDestroyCallback = Just destroyStates
            }

-- | Make a native state the size of one pointer.
setStateSize :: DuckDBV2AggregateFunctionSizeInfoHandle -> IO ()
setStateSize info = checked (c_duckdb_v2_aggregate_function_size_set_state_size info (fromIntegral (sizeOf (nullPtr :: Ptr ()))))

-- | Make an empty state table and transfer it to the bind data of the call site.
bindStateTable :: BindCallback DuckDBV2AggregateFunctionBindInfoHandle
bindStateTable input _ _ = mask_ do
    stable <- newIORef IntSet.empty >>= newStablePtr
    attachOpaque (c_duckdb_v2_function_bind_set_bind_data input) (castPtr (castStablePtrToPtr stable)) stateTableDestructor
        `onException` freeStablePtr stable

-- | Read the state table from the bind data. Without bind data, give 'Nothing'.
readStateTable :: (Ptr (Ptr Void) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO (Maybe StateTable)
readStateTable getter = do
    raw <- output getter
    if raw == nullPtr then pure Nothing else Just <$> deRefStablePtr (castPtrToStablePtr (castPtr raw))

{- | Give each new native state the initial Haskell state. First set all native
states to null, so that an error leaves no native state uninitialized.
-}
initializeStates :: Aggregate state args result -> DuckDBV2AggregateFunctionInitInfoHandle -> IO ()
initializeStates Aggregate{aggregateInitial} info = do
    table <- readStateTable (c_duckdb_v2_aggregate_function_init_get_bind_data info)
    count <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_init_get_state_count info)
    states <- output (c_duckdb_v2_aggregate_function_init_get_states info)
    forM_ [0 .. count - 1] \index -> peekElemOff states index >>= \slot -> poke (castPtr slot) (nullPtr :: Ptr ())
    forM_ [0 .. count - 1] \index -> do
        slot <- peekElemOff states index
        mask_ do
            stable <- newIORef aggregateInitial >>= newStablePtr
            forM_ table \keys -> atomicModifyIORef' keys \live -> (IntSet.insert (stableKey stable) live, ())
            poke (castPtr slot) stable

{- | Add each row to its state. A row with NULL in an argument does not change
its state. A row that does not convert raises an error that names the
aggregate function.
-}
updateStates :: (FromRow args) => Text -> Aggregate state args result -> Int -> DuckDBV2AggregateFunctionUpdateInfoHandle -> IO ()
updateStates name Aggregate{aggregateStep} columns info = do
    count <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_update_get_row_count info)
    states <- output (c_duckdb_v2_aggregate_function_update_get_states info)
    readers <- mapFlat (\index -> output (c_duckdb_v2_aggregate_function_update_get_arg info (fromIntegral index)) >>= prepareArgumentReader index) [0 .. columns - 1]
    forM_ [0 .. count - 1] \row -> do
        fields <- mapM ($ row) readers
        unless (any ((== FieldNull) . fieldValue) fields) do
            arguments <- case parseRow fromRow fields of
                Ok arguments -> pure arguments
                Errors failures -> throwIO (userError ("duckdb-simple: " <> Text.unpack name <> ": " <> conversionMessage failures))
            cell <- peekElemOff states row >>= stateCell
            readIORef cell >>= (`aggregateStep` arguments) >>= evaluate >>= writeIORef cell

-- | Merge each source state into its target state. Do not change the source.
combineStates :: Aggregate state args result -> DuckDBV2AggregateFunctionCombineInfoHandle -> IO ()
combineStates Aggregate{aggregateCombine} info = do
    count <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_combine_get_state_count info)
    sources <- output (c_duckdb_v2_aggregate_function_combine_get_sources info)
    targets <- output (c_duckdb_v2_aggregate_function_combine_get_targets info)
    forM_ [0 .. count - 1] \index -> do
        source <- peekElemOff sources index >>= stateCell >>= readIORef
        target <- peekElemOff targets index >>= stateCell
        current <- readIORef target
        aggregateCombine current source >>= evaluate >>= writeIORef target

{- | Write the result of each state at its row of the result vector. DuckDB does
not destroy the states of a finalize call that fails, so release them here.
-}
finalizeStates :: (FunctionResult result) => Aggregate state args result -> ScalarType -> DuckDBV2AggregateFunctionFinalizeInfoHandle -> IO ()
finalizeStates Aggregate{aggregateResult} resultType info = do
    table <- readStateTable (c_duckdb_v2_aggregate_function_finalize_get_bind_data info)
    count <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_finalize_get_state_count info)
    states <- output (c_duckdb_v2_aggregate_function_finalize_get_states info)
    let finalize = do
            offset <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_finalize_get_result_offset info)
            write <- output (c_duckdb_v2_aggregate_function_finalize_get_result info) >>= prepareResultWriter resultType
            forM_ [0 .. count - 1] \index ->
                peekElemOff states index >>= stateCell >>= readIORef >>= aggregateResult >>= toScalarValue >>= write (offset + index)
    finalize `onException` forM_ [0 .. count - 1] \index -> peekElemOff states index >>= releaseState table

-- | Release each state.
destroyStates :: DuckDBV2AggregateFunctionDestroyInfoHandle -> IO ()
destroyStates info = do
    table <- readStateTable (c_duckdb_v2_aggregate_function_destroy_get_bind_data info)
    count <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_destroy_get_state_count info)
    states <- output (c_duckdb_v2_aggregate_function_destroy_get_states info)
    forM_ [0 .. count - 1 :: Int] \index -> peekElemOff states index >>= releaseState table

-- | Read the cell of a native state. A released native state raises an error.
stateCell :: Ptr Void -> IO (IORef state)
stateCell slot = do
    stable <- peek (castPtr slot)
    when (castStablePtrToPtr stable == nullPtr) $
        throwIO (userError "duckdb-simple: the aggregate state was released")
    deRefStablePtr stable

{- | Release the Haskell state of a native state, and set the native state to
null. A null native state does not change. With a table, only the caller that
removes the key from the table frees the stable pointer.
-}
releaseState :: Maybe StateTable -> Ptr Void -> IO ()
releaseState table slot = mask_ do
    stable <- peek (castPtr slot) :: IO (StablePtr ())
    unless (castStablePtrToPtr stable == nullPtr) do
        poke (castPtr slot) (nullPtr :: Ptr ())
        owner <- case table of
            Nothing -> pure True
            Just keys -> atomicModifyIORef' keys \live -> (IntSet.delete (stableKey stable) live, IntSet.member (stableKey stable) live)
        when owner (freeStablePtr stable)

-- | The key of a stable pointer in a state table.
stableKey :: StablePtr a -> Int
stableKey = fromIntegral . ptrToIntPtr . castStablePtrToPtr

{- | Free the stable pointers that remain in a state table, and then the table.
DuckDB calls this when it drops the bind data of a call site.
-}
releaseStateTable :: Ptr Void -> IO ()
releaseStateTable raw =
    mask_ $ ignoreCallbackExceptions $ unless (raw == nullPtr) do
        let stable = castPtrToStablePtr (castPtr raw) :: StablePtr StateTable
        remaining <- deRefStablePtr stable >>= readIORef
        forM_ (IntSet.toList remaining) (freeStablePtr . castPtrToStablePtr . intPtrToPtr . fromIntegral)
            `finally` freeStablePtr stable

foreign export ccall "duckdb_simple_release_aggregate_states"
    releaseStateTable :: Ptr Void -> IO ()

foreign import ccall "&duckdb_simple_release_aggregate_states"
    stateTableDestructorAddress :: FunPtr (Ptr Void -> IO ())

-- | Use the generated callback type for the static state table destructor.
stateTableDestructor :: DuckDBV2OpaqueDestroyFn
stateTableDestructor = DuckDBV2OpaqueDestroyFn (castFunPtr stateTableDestructorAddress)
