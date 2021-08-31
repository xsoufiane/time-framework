module Test.Relation.Order.LeftLinearOrder
    ( -- * Type
      LeftLinearOrder
      
      -- * Observation
    , StrictPartialOrder((<), (>))
    , Identity((===))  
    )where

import Test.Relation.Identity
import Test.Relation.Order.StrictPartialOrder
  
-------------------------------------

class (Identity a, StrictPartialOrder a) => LeftLinearOrder a
