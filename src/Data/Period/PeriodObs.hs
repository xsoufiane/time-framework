{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Period.PeriodObs
    ( -- * Observations
      PeriodObs(..)
    ) where

import Prelude hiding ((<), (==))
  
import Data.Chronon (ChrononObs)
import Data.Period (Period(inf, sup), PeriodType(..))
  
import qualified Prelude as Pr ((==))

import qualified Data.Chronon as C (ChrononObs((<), (<=), (===)))
  
-----------------------------------------------------------
  
-- | Period Observations
class ChrononObs t => PeriodObs (c :: PeriodType) t where
    (<) :: Period c t -> Period c t -> Bool
    
    (>) :: Period c t -> Period c t -> Bool
    x > y = y < x
    
    (==) :: Eq t => Period c t -> Period c t -> Bool
    x == y = (Pr.==) (inf x) (inf y) && (Pr.==) (sup x) (sup y)
    
    (===) :: Period c t -> Period c t -> Bool
    x === y = (C.===) (inf x) (inf y) && (C.===) (sup x) (sup y)

    starts :: Eq t => Period c t -> Period c t -> Bool
    starts x y = (Pr.==) (inf x) (inf y) && (C.<) (sup x) (sup y)
    
    finishes :: Eq t => Period c t -> Period c t -> Bool
    finishes x y = (C.<) (inf y) (inf x) && (Pr.==) (sup x) (sup y)
    
    during :: Period c t -> Period c t -> Bool
    during x y = (C.<) (inf y) (inf x) && (C.<) (sup x) (sup y)

    contains :: Period c t -> Period c t -> Bool
    contains x y = not $ during x y
    
    includedIn :: Eq t => Period c t -> Period c t -> Bool
    includedIn x y = during x y || x == y
    
    overlaps :: Period c t -> Period c t -> Bool
    overlaps x y = (C.<) (inf x) (inf y) && (C.<) (inf y) (sup x) && (C.<) (sup x) (sup y)

    meets :: Eq t => Period 'Closed t -> Period 'Closed t -> Bool
    meets x y = (Pr.==) (sup x) (inf y)

instance ChrononObs t => PeriodObs 'Closed t where
    x < y = (C.<) (sup x) (inf y)
    
instance (ChrononObs t, Eq t) => PeriodObs 'LeftClosed t where
    x < y = (C.<=) (sup x) (inf y)

instance (ChrononObs t, Eq t) => PeriodObs 'RightClosed t where
    x < y = (C.<=) (sup x) (inf y)

instance (ChrononObs t, Eq t) => PeriodObs 'Open t where
    x < y = (C.<=) (sup x) (inf y)
