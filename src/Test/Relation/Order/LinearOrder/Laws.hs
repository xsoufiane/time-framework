{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Relation.Order.LinearOrder.Laws where

import Data.Bits (xor)
import Prelude hiding ((<), (>))
import Test.QuickCheck hiding ((===))

import Test.Relation.Order.LinearOrder (LinearOrder, (<), (>), (===))

-----------------------------------------------------------------------------------------------

type Constraints t = (Arbitrary t, LinearOrder t, Show t)

-- | Linear Order Properties :
prop_linear :: forall t. Constraints t => Property
prop_linear = property $ \(x :: t, y :: t) -> x < y `xor` x > y `xor` x === y

----------------------------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = [ ("LinearOrder", prop_linear @t) ]
