{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.PartialCyclicOrder.Laws (Constraints, laws) where

import Prelude hiding (cycle)
import Test.QuickCheck

import Structure.Order.PartialCyclicOrder

----------------------------------------------------

type Constraints t = (Arbitrary t, PartialCyclicOrder t, Show t)

prop_cyclic :: forall t. Constraints t => Property
prop_cyclic = forAll condition $ \(x, y, z) -> cycle y z x
  where condition :: Gen (t, t, t)
        condition = suchThat arbitrary $ \(x, y, z) -> cycle x y z

prop_asymmetric :: forall t. Constraints t => Property
prop_asymmetric = forAll condition $ \(x, y, z) -> not $ cycle z y x
  where condition :: Gen (t, t, t)
        condition = suchThat arbitrary $ \(x, y, z) -> cycle x y z

prop_transitive :: forall t. Constraints t => Property
prop_transitive = forAll gen $ \(x, y, _, h) -> cycle x y h
  where gen :: Gen (t, t, t, t)
        gen = suchThat arbitrary $ \(x, y, z, h) -> cycle x y z && cycle x z h

--------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws =
    [ ("Cyclic", prop_cyclic @t)
    , ("Asymmetric", prop_asymmetric @t)
    , ("Transitive", prop_transitive @t)
    ]
