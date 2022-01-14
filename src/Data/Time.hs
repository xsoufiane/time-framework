{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Time 
    (
      Time
    , module Data.Chronon
    , module Data.Period
    ) where

import Data.Chronon
import Data.Period

-------------------------------------------

class Internal t => Time t
instance Internal t => Time t

-- | Internal Time Class
class Internal t

instance Chronon t => Internal t
instance Internal (Period c p)
