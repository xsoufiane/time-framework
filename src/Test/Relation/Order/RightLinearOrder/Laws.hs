{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Relation.Order.RightLinearOrder.Laws where

import Data.Bits (xor)
import Prelude hiding ((<), (>))
import Test.QuickCheck hiding ((===))

import Test.Relation.Order.RightLinearOrder (RightLinearOrder, (<), (>), (===))

-----------------------------------------------------------------------------------------------

type Constraints t = (Arbitrary t, RightLinearOrder t, Show t)

-- | Right Linear Order Properties :
prop_right_linear :: forall t. Constraints t => Property
prop_right_linear = forAll gen $ \(_, y, z) -> y < z `xor` y > z `xor` y === z 
  where
    gen :: Gen (t, t, t)
    gen = suchThat arbitrary $ \(x, y, z) -> x < y && x < z

----------------------------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = [ ("Right LinearOrder", prop_right_linear @t) ]
