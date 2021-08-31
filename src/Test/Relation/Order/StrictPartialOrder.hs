module Test.Relation.Order.StrictPartialOrder
    ( -- * Observation
      StrictPartialOrder((<), (>))
    ) where

import Prelude hiding ((<))

----------------------------------------------

class StrictPartialOrder t where
    (<) :: t -> t -> Bool -- ^ Precedence

    (>) :: t -> t -> Bool -- ^ After
    x > y = y < x
