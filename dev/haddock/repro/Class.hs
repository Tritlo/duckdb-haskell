-- | A class in a separate module.
module Class (C (..)) where

-- | A documented class.
class C a where
  -- | A documented method.
  method :: a -> Int

  -- | Compare two values.
  (%%) :: a -> a -> Int
