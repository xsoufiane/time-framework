module Test.Relation.Order.End
    ( -- * Type
      End
      
      -- * Observation
    , StrictPartialOrder((<))
    ) where

import Test.Relation.Order.StrictPartialOrder
  
----------------------------------  
  
class StrictPartialOrder a => End a