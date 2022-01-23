module Structure.Order.Concurrency
    ( -- * Observation
      Concurrency(concurrent),
      StrictPartialOrder((<))
    ) where

import Prelude hiding ((<))
    
import Structure.Order.StrictPartialOrder (StrictPartialOrder((<)))  

--------------------------------------------------------
 
class StrictPartialOrder t => Concurrency t where
  concurrent :: t -> t -> Bool
  concurrent x y = not $ x < y && y < x
