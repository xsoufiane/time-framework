{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Relation.Order.Concurrency.LinearOrder.Laws (laws) where

import Test.QuickCheck hiding ((===))

import Test.Relation.Order.Concurrency
import Test.Relation.Order.LinearOrder (LinearOrder, (===))

--------------------------------------------------------

type Constraints t = (Arbitrary t, LinearOrder t, Concurrency t, Show t)

-- | LinearOder Concurrrency Properties :

prop_linearity :: forall t. Constraints t => Property
prop_linearity = forAll gen $ uncurry (===)
  where
    gen :: Gen (t, t)
    gen = suchThat arbitrary $ uncurry concurrent

--------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = [ ("LinearOrder", prop_linearity @t) ]