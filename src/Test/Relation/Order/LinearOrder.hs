module Test.Relation.Order.LinearOrder
    ( -- * Type
      LinearOrder
      
      -- * Observation
    , StrictPartialOrder((<), (>))
    , Identity((===))  
    )where

import Test.Relation.Identity
import Test.Relation.Order.StrictPartialOrder
  
-------------------------------------

class (Identity a, StrictPartialOrder a) => LinearOrder a
