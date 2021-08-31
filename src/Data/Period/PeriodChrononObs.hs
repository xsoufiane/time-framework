{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Period.PeriodChrononObs
    ( -- * Observations
      PeriodChrononObs(..)
    ) where

import Prelude hiding ((<), (<=))
  
import Data.Chronon (ChrononObs((<), (<=)))
import Data.Period (Period(inf, sup), PeriodType(..))

-----------------------------------------------------------
 
-- | Period Chronon Observations
class ChrononObs t => PeriodChrononObs (c :: PeriodType) t where
    member :: t -> Period c t -> Bool

instance ChrononObs t => PeriodChrononObs 'Open t where
    member t p = inf p < t && t < sup p

instance (ChrononObs t, Eq t) => PeriodChrononObs 'RightClosed t where
    member t p = inf p < t && t <= sup p

instance (ChrononObs t, Eq t) => PeriodChrononObs 'LeftClosed t where
    member t p = inf p <= t && t < sup p

instance (ChrononObs t, Eq t) => PeriodChrononObs 'Closed t where
    member t p = inf p <= t && t <= sup p
