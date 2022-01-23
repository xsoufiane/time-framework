{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Identity.Laws (laws) where

import Test.QuickCheck hiding ((===))

import Structure.Identity

---------------------------------------------

type Constraints a = (Arbitrary a, Identity a, Show a)
  
-- | Identity Properties :
prop_reflexive :: forall a. Constraints a => Property
prop_reflexive = property $ \(x :: a) -> x === x

prop_symmetric :: forall a. Constraints a => Property
prop_symmetric = forAll (suchThat (arbitrary :: Gen (a, a)) $ uncurry (===)) $ \(x, y) -> y === x

prop_transitive :: forall a. Constraints a => Property
prop_transitive = forAll gen (\(x, _, z) -> x === z) 
  where gen :: Gen (a, a, a) 
        gen = (\x -> (x, x, x)) <$> (arbitrary :: Gen a)

---------------------------

laws :: forall a. (Arbitrary a, Identity a, Show a) => [(String, Property)]
laws =
    [ ("Irreflexive", prop_reflexive @a)
    , ("Asymmetric", prop_symmetric @a)
    , ("Transitive", prop_transitive @a)
    ]
