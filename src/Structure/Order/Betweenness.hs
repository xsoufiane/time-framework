module Structure.Order.Betweenness
    ( -- * Observation
      Betweenness(betweenness)
    , StrictPartialOrder((<))
    ) where
      
import Structure.Order.StrictPartialOrder (StrictPartialOrder((<)))     
      
-----------------------------------------------

class StrictPartialOrder t => Betweenness t where
    betweenness :: t -> t -> t -> Bool
