{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.TotalOrder.Laws (Constraints, laws) where

import Prelude hiding ((<=), (>=))
import Test.QuickCheck

import Structure.Order.PartialOrder ((<=), (>=))
import Structure.Order.TotalOrder

-----------------------------------------------------------------------------------------------

type Constraints t = (Arbitrary t, TotalOrder t, Show t)

-- | Total Order Properties :
prop_total :: forall t. Constraints t => Property
prop_total = property $ \(x :: t, y :: t) -> x <= y || x >= y

---------------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = [ ("TotalOrder", prop_total @t) ]
  