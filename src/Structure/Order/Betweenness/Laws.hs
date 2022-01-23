{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.Betweenness.Laws (laws) where

import Prelude hiding ((<))
import Test.QuickCheck

import Structure.Order.Betweenness (Betweenness, betweenness, (<))

--------------------------------------------------------

type Constraints t = (Arbitrary t, Betweenness t, Show t)

-- | Betweenness Properties :
prop_betweenness :: forall t. Constraints t => Property
prop_betweenness = forAll gen $ \(x, y, z) -> (y < x && x < z) || (z < x && x < y)
  where
    gen :: Gen (t, t, t)
    gen = suchThat arbitrary $ \(x, y, z) ->  betweenness x y z

prop_xyz_xzy :: forall t. Constraints t => Property
prop_xyz_xzy = property $ \(x :: t, y :: t, z :: t)-> betweenness x y z == betweenness x z y

prop_xxy :: forall t. Constraints t => Property
prop_xxy = property $ \(x :: t, y :: t)-> not $ betweenness x x y

prop_xyy:: forall t. Constraints t => Property
prop_xyy = property $ \(x :: t, y :: t)-> not $ betweenness x y y

prop_xxx:: forall t. Constraints t => Property
prop_xxx = property $ \(x :: t)-> not $ betweenness x x x

--------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = 
    [ ("Betweenness", prop_betweenness @t)
    , ("xyz_xzy", prop_xyz_xzy @t)
    , ("xxy", prop_xxy @t)
    , ("xyy", prop_xyy @t)
    , ("xxx", prop_xxx @t)
    ]
