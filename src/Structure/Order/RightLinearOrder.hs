module Structure.Order.RightLinearOrder
    ( -- * Type
      RightLinearOrder
      
      -- * Observation
    , StrictPartialOrder((<), (>))
    , Identity((===))  
    )where

import Structure.Identity
import Structure.Order.StrictPartialOrder
  
-------------------------------------

class (Identity a, StrictPartialOrder a) => RightLinearOrder a
