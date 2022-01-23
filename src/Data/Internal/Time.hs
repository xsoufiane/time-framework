{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Internal.Time (Time) where

import Data.Chronon (Chronon)
import Data.Period (Period)

--------------------------------

class Time t

instance Chronon t => Time t
instance Time (Period c t)
