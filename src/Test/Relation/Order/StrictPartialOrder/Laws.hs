{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Relation.Order.StrictPartialOrder.Laws 
    ( -- * Laws
      laws
      
      -- * Class
    , StrictPartialOrder(..)
    ) where

import Prelude hiding ((<), (>))
import Test.QuickCheck

import Test.Relation.Order.StrictPartialOrder  

---------------------------------------------------------------------

type Constraints t = (Arbitrary t, StrictPartialOrder t, Show t)

-- | Strict Partial Order Properties :
prop_irreflexive :: forall t. Constraints t => Property
prop_irreflexive = property (\(x :: t) -> not $ x < x)

prop_asymmetric :: forall t. Constraints t => Property
prop_asymmetric = forAll gen $ \(x, y) -> not $ y < x
  where gen :: Gen (t, t)
        gen = suchThat arbitrary $ uncurry (<)

prop_transitive :: forall t. Constraints t => Property
prop_transitive = forAll gen $ \(x, _, z) -> x < z
  where gen :: Gen (t, t, t)
        gen = suchThat (arbitrary :: Gen (t, t, t)) $ \(x, y, z) -> x < y && y < z

--------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws =
    [ ("Irreflexive", prop_irreflexive @t)
    , ("Asymmetric", prop_asymmetric @t)
    , ("Transitive", prop_transitive @t)
    ]
