{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Database.DuckDB.Simple.Typed.Aggregate
-- Description : Register an aggregate function as a fold over Haskell values.
--
-- The Haskell state of a group lives outside DuckDB. The native state holds a
-- 'StablePtr' to an 'IORef' with the Haskell state. The bind data of each call
-- site holds a state table with the live stable pointers. See
-- Note [Aggregate state life cycle].
module Database.DuckDB.Simple.Typed.Aggregate (
  Aggregate (..),
  createAggregateOn,
) where

import Control.Exception (evaluate, finally, mask_, onException, throwIO)
import Control.Monad (forM_, unless, when)
import Data.Array (Array, elems, listArray, (!))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntSet as IntSet
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Database.DuckDB.Simple.Callback (attachOpaque, ignoreCallbackExceptions, targetCastFactory)
import Database.DuckDB.Simple.FromField (Field (..), FieldValue (FieldNull))
import Database.DuckDB.Simple.FromRow (FromRow (..), parseRow)
import Database.DuckDB.Simple.Function.Internal (FunctionResult (..), ScalarValue, prepareResultWriter)
import Database.DuckDB.Simple.Internal (Connection (..), defaultAggregateStripes)
import Database.DuckDB.Simple.Native
import Database.DuckDB.Simple.Ok (Ok (..))
import Database.DuckDB.Simple.ToField (DuckDBColumnType (..))
import Database.DuckDB.Simple.Typed (
  TypedRow (..),
  argumentName,
  checkArgumentType,
  conversionMessage,
  prepareArgumentReader,
  rowDeclarations,
 )
import Database.DuckDB.Simple.V2.Registration (
  AggregateCallbacks (..),
  BindCallback,
  FunctionParameter (..),
  FunctionSignature (..),
  RegistrationTarget (..),
  aggregateCallbacks,
  configureFunctionSignature,
  parameter,
  registerAggregateFunctionWith,
  withRegistrationFactory,
  withTypeDeclaration,
 )
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, intPtrToPtr, nullPtr, ptrToIntPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peek, peekElemOff, poke)
import HsBindgen.Runtime.Marshal (staticSizeOf)

-- | An aggregate function as a fold. DuckDB keeps one state for each group, or
-- more than one partial state.
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

{- Note [Aggregate state life cycle]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Haskell state of a group lives outside DuckDB. The native state of a group
has the size of one pointer (setStateSize). It holds a StablePtr to an IORef,
and the IORef holds the Haskell state.

These are the steps in the life of a state.

- Init (initializeStates). The callback first writes null to all native states
  of the call, so that an error leaves no native state uninitialized. Then for
  each state it makes the IORef and the StablePtr, adds the key of the
  StablePtr to the state table, and writes the StablePtr to the native state.

- Update and combine (updateStates and combineStates). DuckDB can move a native
  state with a byte copy. Thus these callbacks change only the IORef.

- Release (releaseState). After init, only a release changes the native state:
  it writes null to the native state. A release does not change a null native
  state. Thus the null write stops a second release of one native state. A read
  of a released native state raises an error (stateCell).

The bind data of each call site holds a state table with the keys of the live
stable pointers (StateTable and bindStateTable). A release removes the key from
the table, and only the caller that removes it frees the stable pointer. This
check stops a second free of a stable pointer that a byte copy of the native
state also holds. It does not stop the release of a byte copy after the runtime
has given its stable pointer to a new state. DuckDB 2.0.0 destroys each state at
one location. The destructor of the state table frees the stable pointers that
remain (releaseStateTable).

The registration always sets a bind callback, so each call site has bind data.
The code still reads the table as optional (readStateTable). Without bind data,
only the destroy callback and a failed finalize release the states, and a
release frees each stable pointer that it reads.

These are the states that DuckDB 2.0.0 destroys and does not destroy.

- The header says that the destroy callback runs on states that are discarded
  without being finalized. DuckDB 2.0.0 runs the destroy callback on finalized
  states too: a hash aggregate destroys each chunk of states after it finalizes
  them, and an ungrouped aggregate and a window destroy their states after
  finalize (radix_partitioned_hashtable.cpp, physical_ungrouped_aggregate.cpp,
  and window_segment_tree.cpp).

- DuckDB 2.0.0 does not run the destroy callback on these states:

  - The states of a finalize call that fails, in a hash aggregate and in a
    window.
  - The states that no finalize call gets when LIMIT stops a parallel hash
    aggregate.
  - The states after a combine call fails in a parallel hash aggregate.
  - The states after a finalize call fails in a parallel hash aggregate.

  The exception handler of finalizeStates releases the states of the finalize
  call that fails. The destructor of the state table frees the stable pointers
  of the other states.

DuckDB runs the destructor of the state table when it drops the bind data of a
call site. In DuckDB 2.0.0 a hash table and an ungrouped aggregate each hold a
copy of the bind data, and each copy shares the holder of the state table
through a shared pointer (CV2AggregateFunctionData::Copy and CV2UserData). Each
of them runs the destroy callback on its states before it drops its copy
(~GroupedAggregateHashTable and ~UngroupedAggregateState). In a window, the
same is true for WindowAggregateStates and WindowCustomAggregatorLocalState.
The naive aggregator and the segment tree use the copy of the window global
state, and run the destroy callback during the evaluation. Thus the destructor
runs after the last destroy callback of these owners that gets this bind data.

See Note [State table stripes] for the form of the table.
-}

{- Note [State table stripes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A state table holds the keys of the live states of one call site. Each new
state adds its key, and each release removes it. DuckDB makes and releases
states on several threads at the same time.

A stripe is one part of the table: an IORef with a set of keys. The table is an
array of stripes, and the key of a state selects its stripe (stateStripe). Thus
two threads change the same IORef only when their keys select the same stripe.

- Why the table has stripes. A change uses atomicModifyIORef', which stores a
  value that is not evaluated. If the runtime stops a thread during that
  evaluation, each thread that needs the value waits. With one IORef for all
  states, the DuckDB threads of a query with many groups waited for each other.

- The number of stripes. The option connectionAggregateStripes of
  ConnectionOptions sets the number for each aggregate that createAggregate
  registers through the connection. The registration keeps the number, also
  when another connection calls the aggregate. The default is
  defaultAggregateStripes, 64 stripes. A registration on an extension has no
  connection options, so it uses the default. The bind callback makes the
  table with this number (bindStateTable).

- No result depends on the number. It changes only how often two threads use
  the same stripe. A stripe costs one small IORef for each call site.

The destructor of the table reads all stripes (releaseStateTable).
-}

-- | The live states of one call site, as the keys of their stable pointers.
-- 'stateStripe' gives the stripe of a key. See Note [State table stripes].
--
-- With the table, the library frees the states that DuckDB 2.0.0 does not
-- destroy. See Note [Aggregate state life cycle].
type StateTable = Array Int (IORef IntSet.IntSet)

-- | The stripe that holds a key. In GHC 9.14 the key of a stable pointer is
-- one more than its index in the stable pointer table of the runtime
-- (@rts\/include\/rts\/StablePtr.h@). Thus consecutive keys go to different
-- stripes when the table has more than one stripe. The table is correct for
-- each distribution of the keys. The distribution changes only how often two
-- threads use the same stripe. See Note [State table stripes].
stateStripe :: StateTable -> Int -> IORef IntSet.IntSet
stateStripe table key = table ! (key `mod` length table)

-- | Register a typed aggregate function on a connection or a loading extension.
-- An argument type or a result type with no declaration, or an argument type
-- that is or contains VARIANT, raises an error at registration. On a loading
-- extension, an argument type that is or contains GEOMETRY also raises an
-- error at registration, because the update callback has no factory to read
-- it. See Note [Type declarations] in
-- "Database.DuckDB.Simple.ToField". A result type that is not raw also raises an
-- error at registration, because the finalize callback has no factory. See
-- Note [Result writers] in "Database.DuckDB.Simple.Function.Internal".
createAggregateOn
  :: forall state args result
   . (FromRow args, TypedRow args, FunctionResult result) => RegistrationTarget -> Text -> Aggregate state args result -> IO ()
createAggregateOn target name aggregate = do
  when (Text.null name || Text.any (== '\0') name) $
    throwInvalidArgument "aggregate function name must be nonempty and must not contain NUL"
  argumentTypes <- rowDeclarations name ["argument " <> argumentName index | index <- [0 ..]] columnTypes
  -- The update callback has no context. It reads a GEOMETRY argument with the
  -- factory of the instance, which only a connection target gives.
  casts <- targetCastFactory target
  resultType <- either (\reason -> throwInvalidArgument (name <> ": result: " <> reason)) pure (duckdbTypeDeclaration (Proxy :: Proxy result))
  toRaw <- case rawResult of
    Just raw -> pure raw
    Nothing ->
      throwInvalidArgument
        ( name
            <> ": result: the result type of an aggregate function must be BOOLEAN, TINYINT to BIGINT, UTINYINT to UBIGINT,"
            <> " FLOAT, DOUBLE, or VARCHAR (Bool, Int8 to Int64, Int, Word8 to Word64, Word, Float, Double, Text, String,"
            <> " or Maybe of one of them), because the finalize callback has no factory."
            <> " For a newtype over one of these types, derive FunctionResult with deriving newtype."
        )
  -- See Note [State table stripes]. Evaluate the count before the registration.
  -- An unevaluated count keeps the target, and thus the connection, reachable
  -- from the callbacks.
  stripes <- evaluate case target of
    ConnectionTarget connection -> connectionStripes connection
    ExtensionTarget{} -> defaultAggregateStripes
  let parameters =
        [ (parameter (argumentName index) argumentType){parameterKind = DuckDBV2FunctionParameterKindPositionalOnly}
        | (index, argumentType) <- zip [0 ..] argumentTypes
        ]
      callbacks =
        ( aggregateCallbacks
            setStateSize
            (initializeStates aggregate)
            (updateStates casts name aggregate (length columnTypes))
            (combineStates aggregate)
            (finalizeStates aggregate toRaw)
        )
          { aggregateBindCallback = Just (bindStateTable stripes)
          , aggregateDestroyCallback = Just destroyStates
          }
  withRegistrationFactory target \factory -> do
    -- The argument reader does not read VARIANT, and it reads GEOMETRY only with
    -- a factory. Thus reject such a type before the registration.
    forM_ (zip [0 ..] argumentTypes) \(index, argumentType) -> withTypeDeclaration factory argumentType (checkArgumentType (isJust casts) name index)
    registerAggregateFunctionWith target (configure parameters resultType factory) callbacks
  where
    columnTypes = rowColumnTypes (Proxy :: Proxy args)
    configure parameters resultType factory function = do
      withIdentifier name (checked . c_duckdb_v2_aggregate_function_set_name function)
      signature <- output (c_duckdb_v2_aggregate_function_get_signature function)
      configureFunctionSignature
        factory
        (FunctionSignature parameters (Just resultType))
        signature
      checked
        (c_duckdb_v2_aggregate_function_set_property function DuckDBV2FunctionPropertyNullHandling DuckDBV2FunctionPropertyNullHandlingSpecial)

-- | Make a native state the size of one pointer. See
-- Note [Aggregate state life cycle].
setStateSize :: DuckDBV2AggregateFunctionSizeInfoHandle -> IO ()
setStateSize info = checked (c_duckdb_v2_aggregate_function_size_set_state_size info (fromIntegral (staticSizeOf (Proxy @(Ptr ())))))

-- | Make an empty state table with the given number of stripes and transfer it
-- to the bind data of the call site. See Note [Aggregate state life cycle] and
-- Note [State table stripes].
bindStateTable :: Int -> BindCallback DuckDBV2AggregateFunctionBindInfoHandle
bindStateTable count input _ _ = mask_ do
  stripes <- mapM (const (newIORef IntSet.empty)) [1 .. count]
  -- Build the array here, so that no callback thread evaluates it.
  table <- evaluate (listArray (0, count - 1) stripes :: StateTable)
  stable <- newStablePtr table
  attachOpaque (c_duckdb_v2_function_bind_set_bind_data input) (castPtr (castStablePtrToPtr stable)) stateTableDestructor
    `onException` freeStablePtr stable

-- | Read the state table from the bind data. Without bind data, give 'Nothing'.
-- See Note [Aggregate state life cycle].
readStateTable :: (Ptr (Ptr Void) -> Ptr DuckDBV2ErrorInfoHandle -> IO DuckDBV2Error) -> IO (Maybe StateTable)
readStateTable getter = do
  raw <- output getter
  if raw == nullPtr then pure Nothing else Just <$> deRefStablePtr (castPtrToStablePtr (castPtr raw))

-- | Give each new native state the initial Haskell state. First set all native
-- states to null, so that an error leaves no native state uninitialized. See
-- Note [Aggregate state life cycle].
initializeStates :: Aggregate state args result -> DuckDBV2AggregateFunctionInitInfoHandle -> IO ()
initializeStates Aggregate{aggregateInitial} info = do
  table <- readStateTable (c_duckdb_v2_aggregate_function_init_get_bind_data info)
  count <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_init_get_state_count info)
  states <- output (c_duckdb_v2_aggregate_function_init_get_states info)
  forM_ [0 .. count - 1] \index -> peekElemOff states index >>= \slot -> poke (castPtr slot) (nullPtr @())
  forM_ [0 .. count - 1] \index -> do
    slot <- peekElemOff states index
    mask_ do
      stable <- newIORef aggregateInitial >>= newStablePtr
      let key = stableKey stable
      forM_ table \keys -> atomicModifyIORef' (stateStripe keys key) \live -> (IntSet.insert key live, ())
      poke (castPtr slot) stable

-- | Add each row to its state. A row with NULL in an argument does not change
-- its state. A row that does not convert raises an error that names the
-- aggregate function. A GEOMETRY argument reads with the factory of
-- 'targetCastFactory'. See Note [Aggregate state life cycle].
updateStates
  :: (FromRow args)
  => Maybe DuckDBV2FactoryHandle -> Text -> Aggregate state args result -> Int -> DuckDBV2AggregateFunctionUpdateInfoHandle -> IO ()
updateStates casts name Aggregate{aggregateStep} columns info = do
  count <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_update_get_row_count info)
  states <- output (c_duckdb_v2_aggregate_function_update_get_states info)
  readers <-
    mapM
      (\index -> output (c_duckdb_v2_aggregate_function_update_get_arg info (fromIntegral index)) >>= prepareArgumentReader casts index)
      [0 .. columns - 1]
  forM_ [0 .. count - 1] \row -> do
    fields <- mapM ($ row) readers
    unless (any ((== FieldNull) . fieldValue) fields) do
      arguments <- case parseRow fromRow fields of
        Ok arguments -> pure arguments
        Errors failures -> throwIO (userError ("duckdb-simple: " <> Text.unpack name <> ": " <> conversionMessage failures))
      cell <- peekElemOff states row >>= stateCell
      readIORef cell >>= (`aggregateStep` arguments) >>= evaluate >>= writeIORef cell

-- | Merge each source state into its target state. Do not change the source.
-- Change only the cell of the target. See Note [Aggregate state life cycle].
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

-- | Write the result of each state at its row of the result vector, as a raw
-- value. DuckDB does not destroy the states of a finalize call that fails, so
-- release them here. See Note [Aggregate state life cycle] and
-- Note [Result writers] in "Database.DuckDB.Simple.Function.Internal".
finalizeStates :: Aggregate state args result -> (result -> ScalarValue) -> DuckDBV2AggregateFunctionFinalizeInfoHandle -> IO ()
finalizeStates Aggregate{aggregateResult} toRaw info = do
  table <- readStateTable (c_duckdb_v2_aggregate_function_finalize_get_bind_data info)
  count <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_finalize_get_state_count info)
  states <- output (c_duckdb_v2_aggregate_function_finalize_get_states info)
  let finalize = do
        offset <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_finalize_get_result_offset info)
        write <- output (c_duckdb_v2_aggregate_function_finalize_get_result info) >>= prepareResultWriter Nothing
        forM_ [0 .. count - 1] \index ->
          peekElemOff states index >>= stateCell >>= readIORef >>= aggregateResult >>= write (offset + index) . toRaw
  finalize `onException` forM_ [0 .. count - 1] \index -> peekElemOff states index >>= releaseState table

-- | Release each state. See Note [Aggregate state life cycle].
destroyStates :: DuckDBV2AggregateFunctionDestroyInfoHandle -> IO ()
destroyStates info = do
  table <- readStateTable (c_duckdb_v2_aggregate_function_destroy_get_bind_data info)
  count <- fromIntegral <$> output (c_duckdb_v2_aggregate_function_destroy_get_state_count info)
  states <- output (c_duckdb_v2_aggregate_function_destroy_get_states info)
  forM_ [0 .. count - 1 :: Int] \index -> peekElemOff states index >>= releaseState table

-- | Read the cell of a native state. A released native state raises an error.
-- See Note [Aggregate state life cycle].
stateCell :: Ptr Void -> IO (IORef state)
stateCell slot = do
  stable <- peek (castPtr slot)
  when (castStablePtrToPtr stable == nullPtr) $
    throwIO (userError "duckdb-simple: the aggregate state was released")
  deRefStablePtr stable

-- | Release the Haskell state of a native state, and set the native state to
-- null. A null native state does not change. With a table, only the caller that
-- removes the key from the table frees the stable pointer. See
-- Note [Aggregate state life cycle].
releaseState :: Maybe StateTable -> Ptr Void -> IO ()
releaseState table slot = mask_ do
  stable <- peek (castPtr slot) :: IO (StablePtr ())
  unless (castStablePtrToPtr stable == nullPtr) do
    poke (castPtr slot) (nullPtr @())
    let key = stableKey stable
    owner <- case table of
      Nothing -> pure True
      Just keys -> atomicModifyIORef' (stateStripe keys key) \live -> (IntSet.delete key live, IntSet.member key live)
    when owner (freeStablePtr stable)

-- | The key of a stable pointer in a state table, from 'castStablePtrToPtr'
-- and 'ptrToIntPtr' of base. Base has no single function for the two steps.
-- 'stableFromKey' is the inverse.
stableKey :: StablePtr a -> Int
stableKey = fromIntegral . ptrToIntPtr . castStablePtrToPtr

-- | The stable pointer of a key in a state table, from 'intPtrToPtr' and
-- 'castPtrToStablePtr' of base. This is the inverse of 'stableKey'.
stableFromKey :: Int -> StablePtr a
stableFromKey = castPtrToStablePtr . intPtrToPtr . fromIntegral

-- | Free the stable pointers that remain in a state table, and then the table.
-- DuckDB calls this when it drops the bind data of a call site. See
-- Note [Aggregate state life cycle], and Note [Callback ownership] in
-- "Database.DuckDB.Simple.Callback" for the rules of a destructor.
releaseStateTable :: Ptr Void -> IO ()
releaseStateTable raw =
  mask_ $ ignoreCallbackExceptions $ unless (raw == nullPtr) do
    let stable = castPtrToStablePtr (castPtr raw) :: StablePtr StateTable
    remaining <- deRefStablePtr stable >>= fmap IntSet.unions . mapM readIORef . elems
    forM_ (IntSet.toList remaining) (freeStablePtr . stableFromKey)
      `finally` freeStablePtr stable

foreign export ccall "duckdb_simple_release_aggregate_states"
  releaseStateTable :: Ptr Void -> IO ()

foreign import ccall "&duckdb_simple_release_aggregate_states"
  stateTableDestructorAddress :: FunPtr (Ptr Void -> IO ())

-- | Use the generated callback type for the static state table destructor.
stateTableDestructor :: DuckDBV2OpaqueDestroyFn
stateTableDestructor = DuckDBV2OpaqueDestroyFn (castFunPtr stateTableDestructorAddress)
