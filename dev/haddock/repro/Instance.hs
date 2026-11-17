-- | Two instances of an imported class.
module Instance (T (..), U (..)) where

import Class

-- | The first type.
data T = T

-- | The second type.
data U = U

instance C T where
  method _ = 0
  _ %% _ = 0

instance C U where
  method _ = 1
  _ %% _ = 1
