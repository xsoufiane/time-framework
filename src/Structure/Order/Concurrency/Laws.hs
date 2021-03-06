{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.Concurrency.Laws (Constraints, laws) where

import Prelude hiding ((<))  
import Test.QuickCheck
  
import Structure.Order.Concurrency  
  
--------------------------------------------------------

type Constraints t = (Arbitrary t, Concurrency t, Show t)

-- | Synchronicity Properties :
prop_concurrency :: forall t. Constraints t => Property
prop_concurrency = forAll cond $ \(x,y) -> not $ x < y && y < x
  where
    cond :: Gen (t, t)
    cond = suchThat arbitrary $ uncurry concurrent
    
prop_reflexive :: forall t. Constraints t => Property
prop_reflexive = property $ \(x :: t) -> concurrent x x
    
prop_symmetric :: forall t. Constraints t => Property
prop_symmetric = forAll cond $ \(x,y) -> concurrent y x
  where
    cond :: Gen (t,t)
    cond = suchThat arbitrary $ uncurry concurrent

--------------------------------

laws :: forall t. Constraints t => [(String, Property)]
laws = 
    [ ("Concurrency", prop_concurrency @t)
    , ("Reflexive", prop_reflexive @t)
    , ("Symmetric", prop_symmetric @t)
    ]
