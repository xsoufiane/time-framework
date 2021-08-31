{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Relation.Order.Synchronicity.Laws (laws) where

import Prelude hiding ((<))
import Test.QuickCheck

import Test.Relation.Order.Synchronicity (Synchronicity, synchronous, (<))

--------------------------------------------------------

type Constraints t = (Arbitrary t, Synchronicity t, Show t)

-- | Synchronicity Properties :
prop_synchronicity :: forall t. Constraints t => Property
prop_synchronicity = forAll cond $ \(x, y) -> not (x < y) .&&. prop x y
  where
    cond :: Gen (t, t)
    cond = suchThat arbitrary $ uncurry synchronous
    prop :: t -> t -> t -> Property
    prop x y = \z -> y < z ==> x < z
    
prop_reflexive :: forall t. Constraints t => Property
prop_reflexive = property $ \(x :: t) -> synchronous x x

--------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = 
    [ ("Syncrhonicity", prop_synchronicity @t)
    , ("Reflexive", prop_reflexive @t)
    ]
