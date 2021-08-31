module Test.Relation.Order.RightLinearOrder
    ( -- * Type
      RightLinearOrder
      
      -- * Observation
    , StrictPartialOrder((<), (>))
    , Identity((===))  
    )where

import Test.Relation.Identity
import Test.Relation.Order.StrictPartialOrder
  
-------------------------------------

class (Identity a, StrictPartialOrder a) => RightLinearOrder a
