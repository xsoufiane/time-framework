{-# LANGUAGE ConstrainedClassMethods #-}

module Data.Time (Time) where

import qualified Data.Internal.Time as I (Time)

-------------------------------------------

-- | Closed Time Type Class
class I.Time t => Time t where
