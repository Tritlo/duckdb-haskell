-- |
-- Module      : Database.DuckDB.Simple.Conversion
-- Description : Effectful conversion from DuckDB values.
module Database.DuckDB.Simple.Conversion (
  Conversion,
  runConversion,
  liftConversion,
  conversionError,
  conversionErrorWith,
  mapConversionErrors,
  mapConversionErrorsWith,
) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception, SomeException, evaluate, toException)
import Control.Monad (MonadPlus (..), ap)
import qualified Control.Monad.Fail as Fail
import Database.DuckDB.Simple.Ok (Ok (..))

-- | A conversion with explicit IO and deferred conversion errors.
-- See Note [Conversion effects].
newtype Conversion a = Conversion (IO (Either (IO [SomeException]) a))

{- Note [Conversion effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A conversion can read native metadata through liftConversion. These reads run
only when the conversion demands them. IO exceptions, including cancellation,
propagate to the caller. They are not conversion errors.

A failed conversion stores an action that makes its diagnostics. Alternative
discards this action when the other branch succeeds. runConversion runs the
actions only after all branches fail. Thus a discarded error does not read
metadata, such as an ENUM dictionary, just to name the SQL type.

Field metadata uses Conversion too. Its native owner and successful-result
cache belong to the metadata reader. A child field maps its type projection
over the parent's conversion. No pure thunk performs IO or stores an IO
exception.

Each successful step forces its value to weak head normal form, as Ok does.
The force runs in IO with the caller's masking state. Thus a value nested in
the final row cannot defer its field conversion until after runConversion.
-}

instance Functor Conversion where
  {-# INLINE fmap #-}
  fmap f (Conversion action) = Conversion $ fmap mapResult action
    where
      mapResult (Left errors) = Left errors
      mapResult (Right value) = strictRight (f value)

instance Applicative Conversion where
  {-# INLINE pure #-}
  pure value = Conversion (pure (strictRight value))
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad Conversion where
  {-# INLINE (>>=) #-}
  Conversion action >>= next = Conversion $ do
    result <- action
    case result of
      Left errors -> pure (Left errors)
      Right value -> case next value of
        Conversion following -> following

instance Alternative Conversion where
  empty = Conversion (pure (Left (pure [])))
  Conversion first <|> Conversion second = Conversion $ do
    result <- first
    case result of
      Right value -> pure (Right value)
      Left firstErrors -> do
        alternative <- second
        pure $ case alternative of
          Right value -> Right value
          Left secondErrors -> Left ((<>) <$> firstErrors <*> secondErrors)

instance MonadPlus Conversion where
  mzero = empty
  mplus = (<|>)

instance Fail.MonadFail Conversion where
  fail = conversionError . userError

-- | Run a conversion and resolve the diagnostics if it fails.
-- Force a successful value to weak head normal form. IO exceptions propagate
-- to the caller.
runConversion :: Conversion a -> IO (Ok a)
{-# INLINE runConversion #-}
runConversion (Conversion action) = do
  result <- action
  case result of
    Left errors -> Errors <$> errors
    Right value -> evaluate (Ok value)

-- | Run an IO action when the conversion demands its result.
-- A decoder in a registered callback must use another connection for nested
-- SQL. Reads of the owned field type need no connection lock.
liftConversion :: IO a -> Conversion a
{-# INLINE liftConversion #-}
liftConversion action = Conversion (strictRight <$> action)

-- | Construct a success that forces its value when its result tag is read.
strictRight :: a -> Either e a
{-# INLINE strictRight #-}
strictRight value = value `seq` Right value

-- | Fail with a conversion error.
conversionError :: (Exception e) => e -> Conversion a
conversionError err = Conversion (pure (Left (pure [toException err])))

-- | Make an error only if the final conversion fails.
-- See Note [Conversion effects].
conversionErrorWith :: (Exception e) => Conversion e -> Conversion a
conversionErrorWith (Conversion diagnostic) = Conversion $ pure $ Left $ do
  result <- diagnostic
  case result of
    Left errors -> errors
    Right err -> pure [toException err]

-- | Change deferred diagnostics without running them.
mapConversionErrors :: ([SomeException] -> [SomeException]) -> Conversion a -> Conversion a
mapConversionErrors f (Conversion action) = Conversion $ do
  result <- action
  pure $ case result of
    Left errors -> Left (f <$> errors)
    Right value -> Right value

-- | Change the diagnostics with a conversion when the final conversion fails.
mapConversionErrorsWith :: ([SomeException] -> Conversion [SomeException]) -> Conversion a -> Conversion a
mapConversionErrorsWith f (Conversion action) = Conversion $ do
  result <- action
  pure $ case result of
    Right value -> Right value
    Left errors -> Left $ do
      original <- errors
      case f original of
        Conversion following -> do
          mapped <- following
          case mapped of
            Left failures -> failures
            Right replacement -> pure replacement
