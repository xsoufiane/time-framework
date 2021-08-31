module Test.Relation.Order.Synchronicity
    ( -- * Observation
      Synchronicity(synchronous)
    , StrictPartialOrder((<))
    ) where

import Test.Relation.Order.StrictPartialOrder (StrictPartialOrder((<)))
  
----------------------------
  
class StrictPartialOrder t => Synchronicity t where
    synchronous :: t -> t -> Bool
