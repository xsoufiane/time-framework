{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Structure.Order.End.LinearOrder.Laws (Constraints, laws) where

import Prelude hiding ((<))
import Test.SmallCheck
import Test.SmallCheck.Series (Serial)

import Structure.Order.End (End, (<))
import Structure.Order.LinearOrder (LinearOrder)

---------------------------

type Constraints m t = (Serial m t, End t,LinearOrder t, Show t)

-- | Linear End Properties :
prop_linearity :: forall m t. Constraints m t => Property m
prop_linearity = existsUnique $ \(x :: t) -> forAll $ \(y :: t) -> not $ y < x

--------------------------------

laws :: forall m t. Constraints m t => [(String, Property m)]
laws = [ ("LinearOrder", prop_linearity @m @t) ]
