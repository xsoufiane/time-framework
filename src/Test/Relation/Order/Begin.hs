module Test.Relation.Order.Begin
    ( -- * Type
      Begin
      
      -- * Observation
    , StrictPartialOrder((<))
    ) where

import Test.Relation.Order.StrictPartialOrder
  
----------------------------------  
  
class StrictPartialOrder a => Begin a