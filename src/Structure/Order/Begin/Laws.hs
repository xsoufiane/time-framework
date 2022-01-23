{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.Begin.Laws (laws) where

import Prelude hiding ((<))
import Test.SmallCheck
import Test.SmallCheck.Series (Serial)

import Structure.Order.Begin (Begin, (<))

---------------------------

type Constraints m t = (Serial m t, Begin t, Show t)

-- | begin Properties :
prop_begin :: forall m t. Constraints m t => Property m
prop_begin = exists $ \(x :: t) -> forAll $ \(y :: t) -> not $ y < x

--------------------------------

laws :: forall m t. Constraints m t => [(String, Property m)]
laws = [ ("Begin", prop_begin @m @t) ]
