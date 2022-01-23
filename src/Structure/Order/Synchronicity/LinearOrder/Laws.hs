{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.Synchronicity.LinearOrder.Laws (laws) where

import Test.QuickCheck hiding ((===))

import Structure.Order.Synchronicity (Synchronicity, synchronous)
import Structure.Order.LinearOrder (LinearOrder, (===))

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
