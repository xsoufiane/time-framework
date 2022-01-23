module Structure.Order.Synchronicity
    ( -- * Observation
      Synchronicity(synchronous)
    , StrictPartialOrder((<))
    ) where

import Structure.Order.StrictPartialOrder (StrictPartialOrder((<)))
  
----------------------------
  
class StrictPartialOrder t => Synchronicity t where
    synchronous :: t -> t -> Bool
