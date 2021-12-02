{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Time (Time) where

import Data.Chronon (Chronon)
import Data.Period (Period)

class Internal t => Time t
instance Internal t => Time t

-- | Internal Time Class
class Internal t

instance Chronon t => Internal t
instance Internal (Period c p)
