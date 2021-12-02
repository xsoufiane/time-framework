{-# LANGUAGE ConstrainedClassMethods #-}

module Data.Chronon
    ( -- * Types  
      Chronon
             
      -- * Observations
    , ChrononObs((<), (>), betweenness, (===), (<=), (>=))
    , CyclicChronon(cycle)
    , SynchronousChronon(synchronous)
    , ConcurrentChronon(concurrent)
    ) where

import Prelude hiding ((<), (<=))

----------------------------------------------------------------------    

class Chronon t

-- | Observations
class Chronon t => ChrononObs t where
    (<) :: t -> t -> Bool
     
    (>) :: t -> t -> Bool
    x > y = y < x
    
    -- | x is between y z
    betweenness :: t -> t -> t -> Bool
    betweenness x y z = (y < x && x < z) || (z < x && x < y)

    (===) :: t -> t -> Bool

    (<=) :: Eq t => t -> t -> Bool
    x <= y = x < y || x == y

    (>=) :: Eq t => t -> t -> Bool
    x >= y = y <= x
    
class Chronon t => CyclicChronon t where
    cycle :: t -> t -> t -> Bool

class ChrononObs t => SynchronousChronon t where
    synchronous :: t -> t -> Bool

class ChrononObs t => ConcurrentChronon t where
    concurrent :: t -> t -> Bool
