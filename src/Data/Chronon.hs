module Data.Chronon
    ( -- * Types  
      Chronon
             
      -- * Observations
    , CyclicChronon(cycle)
    , SynchronousChronon(synchronous)
    , ConcurrentChronon(concurrent)
    ) where

----------------------------------------------------------------------    

class Chronon t

-- | Observations  
class Chronon t => CyclicChronon t where
    cycle :: t -> t -> t -> Bool

class Chronon t => SynchronousChronon t where
    synchronous :: t -> t -> Bool

class Chronon t => ConcurrentChronon t where
    concurrent :: t -> t -> Bool
