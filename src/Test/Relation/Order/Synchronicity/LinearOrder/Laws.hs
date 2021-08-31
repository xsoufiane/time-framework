{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Relation.Order.Synchronicity.LinearOrder.Laws (laws) where

import Test.QuickCheck hiding ((===))

import Test.Relation.Order.Synchronicity (Synchronicity, synchronous)
import Test.Relation.Order.LinearOrder (LinearOrder, (===))

--------------------------------------------------------

type Constraints t = (Arbitrary t, LinearOrder t, Synchronicity t, Show t)

-- | LinearOder Synchronicity Properties :

prop_linearity :: forall t. Constraints t => Property
prop_linearity = forAll gen $ \(x :: t, y :: t) -> x === y
  where
    gen :: Gen (t, t)
    gen = suchThat arbitrary $ uncurry synchronous

--------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = [ ("LinearOrder", prop_linearity @t) ]
