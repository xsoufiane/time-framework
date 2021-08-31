module Test.Relation.Order.Betweenness
    ( -- * Observation
      Betweenness(betweenness)
    , StrictPartialOrder((<))
    ) where
      
import Test.Relation.Order.StrictPartialOrder (StrictPartialOrder((<)))     
      
-----------------------------------------------

class StrictPartialOrder t => Betweenness t where
    betweenness :: t -> t -> t -> Bool
