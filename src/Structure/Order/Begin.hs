module Structure.Order.Begin
    ( -- * Type
      Begin
      
      -- * Observation
    , StrictPartialOrder((<))
    ) where

import Structure.Order.StrictPartialOrder
  
----------------------------------  
  
class StrictPartialOrder a => Begin a