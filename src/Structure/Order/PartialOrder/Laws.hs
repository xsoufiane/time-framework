{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.PartialOrder.Laws (laws) where

import Prelude hiding ((<=), (>=))
import Test.QuickCheck

import Structure.Order.PartialOrder

-------------------------------------------------------------

type Constraints t = (Arbitrary t, PartialOrder t, Show t)

-- | Partial Order Properties :
prop_reflexive :: forall t. Constraints t => Property
prop_reflexive = property (\(x :: t) -> x <= x)

prop_antisymmetric :: forall t. Constraints t => Property
prop_antisymmetric = property $ \(x :: t, y :: t) -> not (x <= y) || not (y <= x) || x == y

prop_transitive :: forall t. Constraints t => Property
prop_transitive = forAll gen $ \(x, _, z) -> x <= z
  where gen :: Gen (t, t, t)
        gen = suchThat (arbitrary :: Gen (t, t, t)) $ \(x, y, z) -> x <= y && y <= z

---------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws =
    [ ("Reflexive", prop_reflexive @t)
    , ("Antisymmetric", prop_antisymmetric @t)
    , ("Transitive", prop_transitive @t)
    ]
