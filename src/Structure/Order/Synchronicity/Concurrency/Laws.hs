{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.Synchronicity.Concurrency.Laws (Constraints, laws) where

import Test.QuickCheck hiding ((===))

import Structure.Order.Concurrency
import Structure.Order.Synchronicity

--------------------------------------------------------

type Constraints t = (Arbitrary t, Concurrency t, Synchronicity t, Show t)

-- | Concurrent Synchronicity Properties :

prop_concurrency :: forall t. Constraints t => Property
prop_concurrency = forAll gen $ uncurry concurrent
  where
    gen :: Gen (t, t)
    gen = suchThat arbitrary $ uncurry synchronous

--------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = [ ("Concurrency", prop_concurrency @t) ]
