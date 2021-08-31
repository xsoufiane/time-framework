{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

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

data family Chronon :: k

-- | Observations
class ChrononObs t where
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
    
class CyclicChronon t where    
    cycle :: t -> t -> t -> Bool

class SynchronousChronon t where
    synchronous :: t -> t -> Bool

class ConcurrentChronon t where
    concurrent :: t -> t -> Bool
