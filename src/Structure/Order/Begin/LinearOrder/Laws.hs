{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.Begin.LinearOrder.Laws (Constraints, laws) where

import Prelude hiding ((<))
import Test.SmallCheck
import Test.SmallCheck.Series (Serial)

import Structure.Order.Begin (Begin, (<))
import Structure.Order.LinearOrder (LinearOrder)

---------------------------

type Constraints m t = (Serial m t, Begin t, LinearOrder t, Show t)

-- | Linear Begin Properties :
prop_linearity :: forall m t. Constraints m t => Property m
prop_linearity = existsUnique $ \(x :: t) -> forAll $ \(y :: t) -> not $ y < x

--------------------------------

laws :: forall m t. Constraints m t => [(String, Property m)]
laws = [ ("LinearOrder", prop_linearity @m @t) ]
