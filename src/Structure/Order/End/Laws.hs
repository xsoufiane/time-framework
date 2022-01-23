{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.End.Laws (laws) where

import Prelude hiding ((<))
import Test.SmallCheck
import Test.SmallCheck.Series (Serial)

import Structure.Order.End (End, (<))

---------------------------

type Constraints m t = (Serial m t, End t, Show t)

-- | End Properties :
prop_end :: forall m t. Constraints m t => Property m
prop_end = exists $ \(x :: t) -> forAll $ \(y :: t) -> not $ y < x

--------------------------------

laws :: forall m t. Constraints m t => [(String, Property m)]
laws = [ ("End", prop_end @m @t) ]
