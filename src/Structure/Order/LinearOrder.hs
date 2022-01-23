module Structure.Order.LinearOrder
    ( -- * Type
      LinearOrder
      
      -- * Observation
    , StrictPartialOrder((<), (>))
    , Identity((===))  
    )where

import Structure.Identity
import Structure.Order.StrictPartialOrder
  
-------------------------------------

class (Identity a, StrictPartialOrder a) => LinearOrder a
