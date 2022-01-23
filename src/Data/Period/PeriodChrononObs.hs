{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Period.PeriodChrononObs (PeriodChrononObs(member)) where

import Prelude hiding ((<), (<=))

import Data.Chronon (Chronon)
import Data.Period (Period(inf, sup), PeriodType(..))
import Relation.Order (Order((<), (<=)))

-------------------------------------------------------------------------------------------
 
-- | Period Chronon Observations
class Chronon t => PeriodChrononObs (c :: PeriodType) t where
    member :: t -> Period c t -> Bool

instance (Chronon t, Order t) => PeriodChrononObs 'Open t where
    member t p = inf p < t && t < sup p

instance (Chronon t, Eq t, Order t) => PeriodChrononObs 'RightClosed t where
    member t p = inf p < t && t <= sup p

instance (Chronon t, Eq t, Order t) => PeriodChrononObs 'LeftClosed t where
    member t p = inf p <= t && t < sup p

instance (Chronon t, Eq t, Order t) => PeriodChrononObs 'Closed t where
    member t p = inf p <= t && t <= sup p
