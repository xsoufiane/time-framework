{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Period
    ( -- * Data Types
      Period(inf, sup)
    , PeriodType(..)
    , SPeriodType(..)

      -- * Constructors
    , period
    , periodTH
    ) where

import Control.Exception.Base
import Data.Data (typeRep)
import Data.Singletons.TH
import Language.Haskell.TH.Syntax
import Prelude hiding ((<), (<=), (==))
import Refined

import Data.Chronon (ChrononObs)

import qualified Data.Chronon as C (ChrononObs((<)))

-----------------------------------------------------------

data PeriodType = Open | Closed | RightClosed | LeftClosed

$(genSingletons [''PeriodType])

data Period (c :: PeriodType) t = Period { inf :: t, sup :: t }

-- | Exceptions
data InvalidPeriod = InvalidPeriod deriving (Show)

instance Exception InvalidPeriod where
    displayException _ = "Period Bounds are Invalid!!!"

-- | Refinement
data ValidPeriodBounds

instance ChrononObs t => Predicate ValidPeriodBounds (Period c t) where
    validate _ (Period x y)
        | (C.<) x y = Nothing
        | otherwise = throwRefineSomeException
            (typeRep (Proxy :: Proxy ValidPeriodBounds))
            (SomeException InvalidPeriod)

-- | Smart Constructor
period
    :: Predicate ValidPeriodBounds (Period c t)
    => SPeriodType c
    -> t
    -> t
    -> Either RefineException (Period c t)
period _ x y = (refine_ @ValidPeriodBounds) $ Period x y

periodTH
    :: (ChrononObs t, Lift (Period c t))
    => SPeriodType c
    -> t
    -> t
    -> Q (TExp (Period c t))
periodTH _ x y = (refineTH_ @ValidPeriodBounds) $ Period x y
