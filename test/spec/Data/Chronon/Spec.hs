{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Chronon.Spec (spec) where

import Test.QuickCheck hiding ((===))
import Test.SmallCheck.Series (cons1, Serial(series))
import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding ((===))
import Test.Tasty.SmallCheck as SC

import Prelude hiding ((<), cycle)
import qualified Prelude as P ((<), (==))

import Data.Chronon
import qualified Data.Chronon as Chronon ((<), (<=), (===), cycle, synchronous, concurrent, betweenness)

import Test.Relation.Identity (Identity((===)))
import qualified Test.Relation.Identity.Laws as Identity

import Test.Relation.Order.StrictPartialOrder (StrictPartialOrder)
import qualified Test.Relation.Order.StrictPartialOrder.Laws as StrictPartialOrder (laws, (<))

import Test.Relation.Order.LinearOrder (LinearOrder)
import qualified Test.Relation.Order.LinearOrder.Laws as LinearOrder

import Test.Relation.Order.RightLinearOrder (RightLinearOrder)
import qualified Test.Relation.Order.RightLinearOrder.Laws as RightLinearOrder

import Test.Relation.Order.LeftLinearOrder (LeftLinearOrder)
import qualified Test.Relation.Order.LeftLinearOrder.Laws as LeftLinearOrder

import Test.Relation.Order.PartialCyclicOrder (PartialCyclicOrder(cycle))
import qualified Test.Relation.Order.PartialCyclicOrder.Laws as PartialCyclicOrder

import Test.Relation.Order.CyclicOrder (CyclicOrder)
import qualified Test.Relation.Order.CyclicOrder.Laws as CyclicOrder

import Test.Relation.Order.PartialOrder (PartialOrder((<=)))
import qualified Test.Relation.Order.PartialOrder.Laws as PartialOrder

import Test.Relation.Order.TotalOrder (TotalOrder)
import qualified Test.Relation.Order.TotalOrder.Laws as TotalOrder

import Test.Relation.Order.Synchronicity (Synchronicity(synchronous))
import Test.Relation.Order.Synchronicity.Laws as Synchronicity
import Test.Relation.Order.Synchronicity.LinearOrder.Laws as LinearSynchronicity
import Test.Relation.Order.Synchronicity.Concurrency.Laws as ConcurrentSynchronicity

import Test.Relation.Order.Concurrency (Concurrency(concurrent))
import Test.Relation.Order.Concurrency.Laws as Concurrency
import Test.Relation.Order.Concurrency.LinearOrder.Laws as LinearConcurrency

import Test.Relation.Order.Betweenness (Betweenness(betweenness))
import Test.Relation.Order.Betweenness.Laws as Betweenness

import Test.Relation.Order.Begin (Begin)
import Test.Relation.Order.Begin.Laws as Begin
import Test.Relation.Order.Begin.LinearOrder.Laws as LinearBegin

import Test.Relation.Order.End (End)
import Test.Relation.Order.End.Laws as End
import Test.Relation.Order.End.LinearOrder.Laws as LinearEnd

-------------------------------------

newtype IntChronon = Chronon Int deriving (Eq, Show)
instance Chronon IntChronon

-- | Observations
instance ChrononObs IntChronon where
     Chronon x < Chronon y = (P.<) x y
     Chronon x === Chronon y = (P.==) x y
     
instance CyclicChronon IntChronon where     
     cycle x y z = x < y && y < z || y < z && z < x || z < x && x < y
     
instance SynchronousChronon IntChronon where
    synchronous x y = x == y

instance ConcurrentChronon IntChronon where
    concurrent x y = x == y
     
-- | Instances
instance Arbitrary IntChronon where
    arbitrary = Chronon <$> arbitrary

instance Serial IO IntChronon where
    series = cons1 Chronon
  
instance Identity IntChronon where
    (===) = (Chronon.===)

instance StrictPartialOrder IntChronon where
    (<) = (Chronon.<)

instance LinearOrder IntChronon

instance RightLinearOrder IntChronon

instance LeftLinearOrder IntChronon

instance PartialCyclicOrder IntChronon where
    cycle = Chronon.cycle

instance CyclicOrder IntChronon

instance PartialOrder IntChronon where
    (<=) = (Chronon.<=)

instance TotalOrder IntChronon

instance Synchronicity IntChronon where
    synchronous = Chronon.synchronous

instance Concurrency IntChronon where
    concurrent = Chronon.concurrent
    
instance Betweenness IntChronon where
    betweenness = Chronon.betweenness

instance Begin IntChronon

instance End IntChronon

-- | spec
spec :: TestTree
spec = testGroup "Chronon Spec" [ relationLaws, orderLaws ]

relationLaws :: TestTree
relationLaws = testGroup "Relation Laws"
    [ QC.testProperties "Identity" $ Identity.laws @IntChronon
    ]

orderLaws :: TestTree
orderLaws = testGroup "Order Laws"
    [ QC.testProperties "StrictPartialOrder" $ StrictPartialOrder.laws @IntChronon
    , QC.testProperties "LinearOrder" $ LinearOrder.laws @IntChronon
    , QC.testProperties "RightLinearOrder" $ RightLinearOrder.laws @IntChronon
    , QC.testProperties "LeftLinearOrder" $ LeftLinearOrder.laws @IntChronon
    , QC.testProperties "PartialCyclicOrder" $ PartialCyclicOrder.laws @IntChronon
    , QC.testProperties "CyclicOrder" $ CyclicOrder.laws @IntChronon
    , QC.testProperties "PartialOrder" $ PartialOrder.laws @IntChronon
    , QC.testProperties "TotalOrder" $ TotalOrder.laws @IntChronon
    , QC.testProperties "Synchronicity" $
          Synchronicity.laws @IntChronon ++ LinearSynchronicity.laws @IntChronon ++
              ConcurrentSynchronicity.laws @IntChronon
    , QC.testProperties "Concurrency" $ Concurrency.laws @IntChronon ++ LinearConcurrency.laws @IntChronon
    , QC.testProperties "Betweenness" $ Betweenness.laws @IntChronon
    , localOption (SmallCheckDepth 100) $
          testGroup "Begin" $ uncurry SC.testProperty <$> Begin.laws @IO @IntChronon ++ LinearBegin.laws @IO @IntChronon
    , localOption (SmallCheckDepth 100) $
          testGroup "End" $ uncurry SC.testProperty <$> End.laws @IO @IntChronon ++ LinearEnd.laws @IO @IntChronon
    ]
  