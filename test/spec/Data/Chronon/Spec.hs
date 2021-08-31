{-# OPTIONS_GHC -fno-warn-orphans #-}

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

data instance Chronon = Chronon Int deriving (Eq, Show)

-- | Observations
instance ChrononObs Chronon where
     Chronon x < Chronon y = (P.<) x y
     Chronon x === Chronon y = (P.==) x y
     
instance CyclicChronon Chronon where     
     cycle x y z = x < y && y < z || y < z && z < x || z < x && x < y
     
instance SynchronousChronon Chronon where
    synchronous x y = x == y

instance ConcurrentChronon Chronon where
    concurrent x y = x == y
     
-- | Instances
instance Arbitrary Chronon where
    arbitrary = Chronon <$> arbitrary

instance Serial IO Chronon where
    series = cons1 Chronon
  
instance Identity Chronon where
    (===) = (Chronon.===)

instance StrictPartialOrder Chronon where
    (<) = (Chronon.<)

instance LinearOrder Chronon

instance RightLinearOrder Chronon

instance LeftLinearOrder Chronon

instance PartialCyclicOrder Chronon where
    cycle = Chronon.cycle

instance CyclicOrder Chronon

instance PartialOrder Chronon where
    (<=) = (Chronon.<=)

instance TotalOrder Chronon

instance Synchronicity Chronon where
    synchronous = Chronon.synchronous

instance Concurrency Chronon where
    concurrent = Chronon.concurrent
    
instance Betweenness Chronon where
    betweenness = Chronon.betweenness

instance Begin Chronon

instance End Chronon

-- | spec
spec :: TestTree
spec = testGroup "Chronon Spec" [ relationLaws, orderLaws ]

relationLaws :: TestTree
relationLaws = testGroup "Relation Laws"
    [ QC.testProperties "Identity" $ Identity.laws @Chronon
    ]

orderLaws :: TestTree
orderLaws = testGroup "Order Laws"
    [ QC.testProperties "StrictPartialOrder" $ StrictPartialOrder.laws @Chronon
    , QC.testProperties "LinearOrder" $ LinearOrder.laws @Chronon
    , QC.testProperties "RightLinearOrder" $ RightLinearOrder.laws @Chronon
    , QC.testProperties "LeftLinearOrder" $ LeftLinearOrder.laws @Chronon
    , QC.testProperties "PartialCyclicOrder" $ PartialCyclicOrder.laws @Chronon
    , QC.testProperties "CyclicOrder" $ CyclicOrder.laws @Chronon
    , QC.testProperties "PartialOrder" $ PartialOrder.laws @Chronon
    , QC.testProperties "TotalOrder" $ TotalOrder.laws @Chronon
    , QC.testProperties "Synchronicity" $
          Synchronicity.laws @Chronon ++ LinearSynchronicity.laws @Chronon ++ ConcurrentSynchronicity.laws @Chronon
    , QC.testProperties "Concurrency" $ Concurrency.laws @Chronon ++ LinearConcurrency.laws @Chronon
    , QC.testProperties "Betweenness" $ Betweenness.laws @Chronon
    , localOption (SmallCheckDepth 100) $
          testGroup "Begin" $ uncurry SC.testProperty <$> Begin.laws @IO @Chronon ++ LinearBegin.laws @IO @Chronon
    , localOption (SmallCheckDepth 100) $
          testGroup "End" $ uncurry SC.testProperty <$> End.laws @IO @Chronon ++ LinearEnd.laws @IO @Chronon
    ]
  