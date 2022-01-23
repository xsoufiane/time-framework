{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.LeftLinearOrder.Laws where

import Data.Bits (xor)
import Prelude hiding ((<), (>))
import Test.QuickCheck hiding ((===))

import Structure.Order.LeftLinearOrder (LeftLinearOrder, (<), (>), (===))

-----------------------------------------------------------------------------------------------

type Constraints t = (Arbitrary t, LeftLinearOrder t, Show t)

-- | Left Linear Order Properties :
prop_left_linear :: forall t. Constraints t => Property
prop_left_linear = forAll gen $ \(_, y, z) -> y < z `xor` y > z `xor` y === z 
  where
    gen :: Gen (t, t, t)
    gen = suchThat arbitrary $ \(x, y, z) -> y < x && z < x

----------------------------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = [ ("Left LinearOrder", prop_left_linear @t) ]
