module Structure.Order.LeftLinearOrder
    ( -- * Type
      LeftLinearOrder
      
      -- * Observation
    , StrictPartialOrder((<), (>))
    , Identity((===))  
    )where

import Structure.Identity
import Structure.Order.StrictPartialOrder
  
-------------------------------------

class (Identity a, StrictPartialOrder a) => LeftLinearOrder a
