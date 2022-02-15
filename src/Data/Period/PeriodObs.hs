{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Period.PeriodObs
    ( -- * Observations
      PeriodObs(starts, finishes, during, contains, includedIn, overlaps)
    , Meets (meets)
    ) where

import Prelude hiding ((<))
  
import Data.Period (Period(sup, inf), PeriodType(Closed))  
import Relation.Order (Order((<)))
  
-----------------------------------------------------------
  
-- | Period Observations
class PeriodObs t where
    starts, finishes, during, overlaps :: t -> t -> Bool
   
    contains :: t -> t -> Bool
    contains x y = not $ during x y
    
    includedIn :: Eq t => t -> t -> Bool
    includedIn x y = during x y || x == y

class Meets t where  
    meets :: t -> t -> Bool

-- | Useful Instances
instance (Eq t, Order t) => PeriodObs (Period c t) where
    starts x y = inf x == inf y && sup x < sup y
    
    finishes x y = inf y < inf x && sup x == sup y
    
    during x y = inf y < inf x && sup x < sup y
    
    overlaps x y = inf x < inf y && inf y < sup x && sup x < sup y
  
instance Eq t => Meets (Period 'Closed t) where  
    meets x y = sup x == inf y
