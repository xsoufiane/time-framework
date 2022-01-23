{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Period.Spec (spec) where

import Data.Either.Combinators (rightToMaybe)
import Data.String.Interpolate (i)
import Prelude hiding ((<), (==))
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import Test.Tasty

import Data.Chronon (Chronon)
import Data.Period (Period(inf, sup), PeriodType(Closed), SPeriodType(SClosed), period, includedIn)
import Relation.Order as T (Order((<)))
import Relation.Identity as I (Identity((===)))
import Structure.Order.PartialOrder (PartialOrder((<=)))
import Structure.Order.StrictPartialOrder (StrictPartialOrder((<)))

import qualified Prelude as Pr ((<), (==))

import qualified Structure.Order.PartialOrder.Laws as PartialOrder (laws)
import qualified Structure.Order.StrictPartialOrder.Laws as StrictPartialOrder (laws)

-----------------------------------------------------------------

-- | Data
newtype IntChronon = Chronon Int
instance Chronon IntChronon
newtype ClosedPeriod = ClosedPeriod (Period 'Closed IntChronon)

-- | Observations
instance Order IntChronon where
     Chronon x < Chronon y = (Pr.<) x y

instance Identity IntChronon where     
     Chronon x === Chronon y = (Pr.==) x y

-- | Instances
instance Arbitrary IntChronon where
    arbitrary = Chronon <$> arbitrary
    
instance Show IntChronon where
    show (Chronon x) = [i|Chronon #{x}|]
    
instance Arbitrary ClosedPeriod where
    arbitrary = ClosedPeriod <$> suchThatMap (applyArbitrary2 $ period SClosed) rightToMaybe
    
instance Show ClosedPeriod where
    show (ClosedPeriod p) = [i|Period(#{inf p}, #{sup p})|]

instance Eq IntChronon where
    Chronon x == Chronon y = (Pr.==) x y
    
instance Eq (Period 'Closed IntChronon) where
    x == y = (Pr.==) (inf x) (inf y) && (Pr.==) (sup x) (sup y)
    
instance Eq ClosedPeriod where
    ClosedPeriod x == ClosedPeriod y = (Pr.==) x y

-- | Axiomatization
instance StrictPartialOrder ClosedPeriod where
    ClosedPeriod x < ClosedPeriod y = (T.<) x y
    
instance PartialOrder ClosedPeriod where
    ClosedPeriod x <= ClosedPeriod y = includedIn x y    

-- | spec
spec :: TestTree
spec = testGroup "Closed Period Spec" [ orderLaws ]

orderLaws :: TestTree
orderLaws = testGroup "Order Laws"
    [  QC.testProperties "Precedence StrictPartialOrder" $ StrictPartialOrder.laws @ClosedPeriod
    ,  QC.testProperties "IncludedIn PartialOrder" $ PartialOrder.laws @ClosedPeriod
    ]
