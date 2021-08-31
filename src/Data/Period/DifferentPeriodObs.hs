{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Period.DifferentPeriodObs 
    ( -- * Observations
      PrecedenceObs(..)
    , PeriodObs(..)
    ) where

import Prelude hiding ((<))
  
import Data.Chronon (ChrononObs)
import Data.Period (Period(inf, sup), PeriodType(..))  
  
import qualified Data.Chronon as C (ChrononObs((<), (<=)))
  
-----------------------------------------------------------      

type family TypeNeq (a :: PeriodType) (b :: PeriodType) where
  TypeNeq a a = 'False
  TypeNeq _ _ = 'True
  
type family LeftEdgeEq a b where
    LeftEdgeEq 'LeftClosed 'Closed = 'True
    LeftEdgeEq 'Closed 'LeftClosed = 'True
    LeftEdgeEq 'Open 'RightClosed = 'True
    LeftEdgeEq 'RightClosed 'Open = 'True
    LeftEdgeEq _ _ = 'False

type family RightEdgeEq a b where
    RightEdgeEq 'LeftClosed 'Open = 'True
    RightEdgeEq 'Open 'LeftClosed = 'True
    RightEdgeEq 'Closed 'RightClosed = 'True
    RightEdgeEq 'RightClosed 'Closed = 'True
    RightEdgeEq _ _ = 'False

-- | Different Period Observations
class (TypeNeq a b ~ 'True, ChrononObs t) => PrecedenceObs (a :: PeriodType) (b :: PeriodType) t where
    (<) :: Period a t -> Period b t -> Bool

    (>) :: PrecedenceObs b a t => Period a t -> Period b t -> Bool
    x > y = y < x

instance (TypeNeq 'Open c ~ 'True, ChrononObs t, Eq t) => PrecedenceObs 'Open c t where
    x < y = (C.<=) (sup x) (inf y)

instance (ChrononObs t, Eq t) => PrecedenceObs 'Closed 'RightClosed t where
    x < y = (C.<=) (sup x) (inf y)

instance (ChrononObs t, Eq t) => PrecedenceObs 'Closed 'Open t where
    x < y = (C.<=) (sup x) (inf y)

instance ChrononObs t => PrecedenceObs 'Closed 'LeftClosed t where
    x < y = (C.<) (sup x) (inf y)

class (TypeNeq a b ~ 'True, ChrononObs t) => PeriodObs (a :: PeriodType) (b :: PeriodType) t where
    starts :: (LeftEdgeEq a b ~ 'True, Eq t) => Period a t -> Period b t -> Bool
    starts x y = inf x == inf y && (C.<) (sup x) (sup y)

    finishes :: (RightEdgeEq a b ~ 'True, Eq t) => Period a t -> Period b t -> Bool
    finishes x y = (C.<) (inf y) (inf x) &&  sup x == sup y

    meets :: Eq t => Period 'RightClosed t -> Period 'LeftClosed t -> Bool
    meets x y = sup x == inf y

    overlaps :: Period a t -> Period b t -> Bool
    overlaps x y = (C.<) (inf x) (inf y) && (C.<) (inf y) (sup x) && (C.<) (sup x) (sup y)

    during :: Period a t -> Period b t -> Bool

    contains :: Period a t -> Period b t -> Bool
    contains x y = not $ during x y

    includedIn :: Period a t -> Period b t -> Bool
    includedIn = during

instance (TypeNeq 'Open c ~ 'True, ChrononObs t, Eq t) => PeriodObs 'Open c t where
    during x y = (C.<) (inf y) (inf x) && (C.<) (sup x) (sup y)

instance (ChrononObs t, Eq t) => PeriodObs 'Closed 'RightClosed t where
    during x y = (C.<=) (inf y) (inf x) && (C.<) (sup x) (sup y)

instance (ChrononObs t, Eq t) => PeriodObs 'Closed 'Open t where
    during x y = (C.<=) (inf y) (inf x) && (C.<=) (sup x) (sup y)

instance (ChrononObs t, Eq t)  => PeriodObs 'Closed 'LeftClosed t where
    during x y = (C.<) (inf y) (inf x) && (C.<=) (sup x) (sup y)
    