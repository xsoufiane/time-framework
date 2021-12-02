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

import Data.Chronon (Chronon, ChrononObs(..))
import Data.Period (Period(inf, sup), PeriodType(Closed), SPeriodType(SClosed), period)
import Test.Relation.Order.PartialOrder (PartialOrder((<=)))
import Test.Relation.Order.StrictPartialOrder (StrictPartialOrder((<)))

import qualified Prelude as Pr ((<), (==))

import qualified Data.Period.PeriodObs as Period ((<), includedIn)
import qualified Test.Relation.Order.PartialOrder.Laws as PartialOrder (laws)
import qualified Test.Relation.Order.StrictPartialOrder.Laws as StrictPartialOrder (laws)

-----------------------------------------------------------------

-- | Data
newtype IntChronon = Chronon Int
instance Chronon IntChronon
newtype ClosedPeriod = ClosedPeriod (Period 'Closed IntChronon)

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

-- | Observations
instance ChrononObs IntChronon where
     Chronon x < Chronon y = (Pr.<) x y
     Chronon x === Chronon y = (Pr.==) x y

-- | Axiomatization
instance StrictPartialOrder ClosedPeriod where
    ClosedPeriod x < ClosedPeriod y = (Period.<) x y
    
instance PartialOrder ClosedPeriod where
    ClosedPeriod x <= ClosedPeriod y = Period.includedIn x y    

-- | spec
spec :: TestTree
spec = testGroup "Closed Period Spec" [ orderLaws ]

orderLaws :: TestTree
orderLaws = testGroup "Order Laws"
    [  QC.testProperties "Precedence StrictPartialOrder" $ StrictPartialOrder.laws @ClosedPeriod
    ,  QC.testProperties "IncludedIn PartialOrder" $ PartialOrder.laws @ClosedPeriod
    ]
