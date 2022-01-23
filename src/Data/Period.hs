{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
    
      -- * Constructors
    , (==)
    , starts
    , finishes
    , during
    , contains
    , includedIn
    , overlaps
    , meets
    ) where

import Control.Exception.Base
import Data.Data (typeRep)
import Data.Singletons.TH
import Language.Haskell.TH.Syntax
import Prelude hiding ((<), (<=), (==))
import Refined

import Data.Chronon
import Relation.Order (Order((<), (<=)))
import Relation.Identity (Identity((===)))

import qualified Prelude as Pr ((==))

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

instance (Chronon t, Order t) => Predicate ValidPeriodBounds (Period c t) where
    validate _ (Period x y)
        | x < y = Nothing
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
    :: (Chronon t, Lift (Period c t), Order t)
    => SPeriodType c
    -> t
    -> t
    -> Q (TExp (Period c t))
periodTH _ x y = (refineTH_ @ValidPeriodBounds) $ Period x y

-- | Observations
(==) :: Eq t => Period c t -> Period c t -> Bool
x == y = (Pr.==) (inf x) (inf y) && (Pr.==) (sup x) (sup y)

starts :: (Eq t, Order t)=> Period c t -> Period c t -> Bool
starts x y = (Pr.==) (inf x) (inf y) && sup x < sup y

finishes :: (Eq t, Order t) => Period c t -> Period c t -> Bool
finishes x y = inf y < inf x && (Pr.==) (sup x) (sup y)

during :: Order t => Period c t -> Period c t -> Bool
during x y = inf y < inf x && sup x < sup y

contains :: Order t => Period c t -> Period c t -> Bool
contains x y = not $ during x y

includedIn :: (Eq t, Order t) => Period c t -> Period c t -> Bool
includedIn x y = during x y || x == y

overlaps :: Order t => Period c t -> Period c t -> Bool
overlaps x y = inf x < inf y && inf y < sup x && sup x < sup y

meets :: Eq t => Period 'Closed t -> Period 'Closed t -> Bool
meets x y = (Pr.==) (sup x) (inf y)

-- | Useful Instances
instance (Chronon t, Identity t) => Identity (Period c t) where
    x === y = inf x === inf y && sup x === sup y

instance (Chronon t, Order t) => Order (Period 'Closed t) where
    x < y = sup x < inf y
    
instance (Chronon t, Eq t, Order t) => Order (Period 'LeftClosed t) where
    x < y = sup x <= inf y

instance (Chronon t, Eq t, Order t) => Order (Period 'RightClosed t) where
    x < y = sup x <= inf y

instance (Chronon t, Eq t, Order t) => Order (Period 'Open t) where
    x < y = sup x <= inf y
