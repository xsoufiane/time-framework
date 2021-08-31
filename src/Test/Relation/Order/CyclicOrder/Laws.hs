{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Relation.Order.CyclicOrder.Laws (laws) where

import Data.Bits (xor)  
import Prelude hiding (cycle)
import Test.QuickCheck hiding ((===))

import Test.Relation.Identity
import Test.Relation.Order.CyclicOrder
import Test.Relation.Order.PartialCyclicOrder

---------------------------------------------------------

type Constraints t = (Arbitrary t, CyclicOrder t, Show t)

-- | Cyclic Order Properties :
prop_total :: forall t. Constraints t => Property
prop_total = property $ 
    \(x :: t, y :: t, z :: t) -> ((x === y) || (y === z) || (x === z)) `xor` cycle x y z  `xor` cycle z y x

--------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = [ ("Total", prop_total @t) ] 
