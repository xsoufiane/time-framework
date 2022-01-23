module Structure.Order.End
    ( -- * Type
      End
      
      -- * Observation
    , StrictPartialOrder((<))
    ) where

import Structure.Order.StrictPartialOrder
  
----------------------------------  
  
class StrictPartialOrder a => End a