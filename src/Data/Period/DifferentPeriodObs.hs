{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Period.DifferentPeriodObs 
    ( -- * Observations
      PrecedenceObs((<), (>))
    , PeriodObs(starts, finishes, meets, overlaps, during, contains, includedIn)
    ) where

import Prelude hiding ((<))
  
import Data.Chronon (Chronon)
import Data.Period (Period(inf, sup), PeriodType(..))  
  
import qualified Relation.Order as T (Order((<), (<=)))
  
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
class (TypeNeq a b ~ 'True, Chronon t) => PrecedenceObs (a :: PeriodType) (b :: PeriodType) t where
    (<) :: Period a t -> Period b t -> Bool

    (>) :: PrecedenceObs b a t => Period a t -> Period b t -> Bool
    x > y = y < x

instance (TypeNeq 'Open c ~ 'True, Chronon t, T.Order t, Eq t) => PrecedenceObs 'Open c t where
    x < y = (T.<=) (sup x) (inf y)

instance (Chronon t, T.Order t, Eq t) => PrecedenceObs 'Closed 'RightClosed t where
    x < y = (T.<=) (sup x) (inf y)

instance (Chronon t, T.Order t, Eq t) => PrecedenceObs 'Closed 'Open t where
    x < y = (T.<=) (sup x) (inf y)

instance (Chronon t, T.Order t) => PrecedenceObs 'Closed 'LeftClosed t where
    x < y = (T.<) (sup x) (inf y)

class (TypeNeq a b ~ 'True, Chronon t, T.Order t) => PeriodObs (a :: PeriodType) (b :: PeriodType) t where
    starts :: (LeftEdgeEq a b ~ 'True, Eq t) => Period a t -> Period b t -> Bool
    starts x y = inf x == inf y && (T.<) (sup x) (sup y)

    finishes :: (RightEdgeEq a b ~ 'True, Eq t) => Period a t -> Period b t -> Bool
    finishes x y = (T.<) (inf y) (inf x) &&  sup x == sup y

    meets :: Eq t => Period 'RightClosed t -> Period 'LeftClosed t -> Bool
    meets x y = sup x == inf y

    overlaps :: Period a t -> Period b t -> Bool
    overlaps x y = (T.<) (inf x) (inf y) && (T.<) (inf y) (sup x) && (T.<) (sup x) (sup y)

    during :: Period a t -> Period b t -> Bool

    contains :: Period a t -> Period b t -> Bool
    contains x y = not $ during x y

    includedIn :: Period a t -> Period b t -> Bool
    includedIn = during

instance (TypeNeq 'Open c ~ 'True, Chronon t, T.Order t, Eq t) => PeriodObs 'Open c t where
    during x y = (T.<) (inf y) (inf x) && (T.<) (sup x) (sup y)

instance (T.Order t, Chronon t, Eq t) => PeriodObs 'Closed 'RightClosed t where
    during x y = (T.<=) (inf y) (inf x) && (T.<) (sup x) (sup y)

instance (T.Order t, Chronon t, Eq t) => PeriodObs 'Closed 'Open t where
    during x y = (T.<=) (inf y) (inf x) && (T.<=) (sup x) (sup y)

instance (T.Order t, Chronon t, Eq t)  => PeriodObs 'Closed 'LeftClosed t where
    during x y = (T.<) (inf y) (inf x) && (T.<=) (sup x) (sup y)
    