module Test.Relation.Order.Concurrency
    ( -- * Observation
      Concurrency(concurrent),
      StrictPartialOrder((<))
    ) where

import Prelude hiding ((<))
    
import Test.Relation.Order.StrictPartialOrder (StrictPartialOrder((<)))  

--------------------------------------------------------
 
class StrictPartialOrder t => Concurrency t where
  concurrent :: t -> t -> Bool
  concurrent x y = not $ x < y && y < x
