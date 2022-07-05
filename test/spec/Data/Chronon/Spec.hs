{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Chronon.Spec (spec) where

import Prelude hiding ((<), (<=), cycle)
import Test.QuickCheck hiding ((===))
import Test.SmallCheck.Series (cons1, Serial(series))
import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding ((===))
import Test.Tasty.SmallCheck as SC

import Data.Chronon
import Relation.Order as Relation (Order((<), (<=), betweenness))
import Structure.Identity (Identity((===)))
import Structure.Order.Begin (Begin)
import Structure.Order.Begin.Laws as Begin
import Structure.Order.Begin.LinearOrder.Laws as LinearBegin
import Structure.Order.Betweenness (Betweenness(betweenness))
import Structure.Order.Betweenness.Laws as Betweenness
import Structure.Order.Concurrency (Concurrency(concurrent))
import Structure.Order.Concurrency.Laws as Concurrency
import Structure.Order.Concurrency.LinearOrder.Laws as LinearConcurrency
import Structure.Order.CyclicOrder (CyclicOrder)
import Structure.Order.End (End)
import Structure.Order.End.Laws as End
import Structure.Order.End.LinearOrder.Laws as LinearEnd
import Structure.Order.LeftLinearOrder (LeftLinearOrder)
import Structure.Order.LinearOrder (LinearOrder)
import Structure.Identity.Laws as Test.Identity
import Structure.Order.PartialCyclicOrder (PartialCyclicOrder(cycle))
import Structure.Order.RightLinearOrder (RightLinearOrder)
import Structure.Order.StrictPartialOrder (StrictPartialOrder)
import Structure.Order.Synchronicity (Synchronicity(synchronous))
import Structure.Order.Synchronicity.Concurrency.Laws as ConcurrentSynchronicity
import Structure.Order.Synchronicity.LinearOrder.Laws as LinearSynchronicity
import Structure.Order.TotalOrder (TotalOrder)

import qualified Prelude as P ((<), (==))

import qualified Data.Chronon as Chronon (cycle, synchronous, concurrent)
import qualified Structure.Order.CyclicOrder.Laws as CyclicOrder
import qualified Structure.Order.LeftLinearOrder.Laws as LeftLinearOrder
import qualified Structure.Order.LinearOrder.Laws as LinearOrder
import qualified Structure.Order.PartialCyclicOrder.Laws as PartialCyclicOrder
import qualified Structure.Order.PartialOrder as Test (PartialOrder((<=)))
import qualified Structure.Order.PartialOrder.Laws as PartialOrder
import qualified Structure.Order.RightLinearOrder.Laws as RightLinearOrder
import qualified Structure.Order.StrictPartialOrder as StrictPartialOrder (StrictPartialOrder((<)))
import qualified Structure.Order.StrictPartialOrder.Laws as StrictPartialOrder (laws)
import qualified Structure.Order.TotalOrder.Laws as TotalOrder
import qualified Structure.Order.Synchronicity.Laws as Synchronicity

-------------------------------------------

newtype IntChronon = Chronon Int deriving (Eq, Show)
instance Chronon IntChronon

-- | Observations
instance Relation.Order IntChronon where
     Chronon x < Chronon y = (P.<) x y

instance Identity IntChronon where  
     Chronon x === Chronon y = (P.==) x y
     
instance CyclicChronon IntChronon where     
     cycle x y z = x < y && y < z || y < z && z < x || z < x && x < y
     
instance SynchronousChronon IntChronon where
    synchronous x y = x == y

instance ConcurrentChronon IntChronon where
    concurrent x y = x == y
     
-- | Spec Instances
instance Arbitrary IntChronon where
    arbitrary = Chronon <$> arbitrary

instance Serial IO IntChronon where
    series = cons1 Chronon

instance StrictPartialOrder IntChronon where
    (<) = (Relation.<)

instance LinearOrder IntChronon

instance RightLinearOrder IntChronon

instance LeftLinearOrder IntChronon

instance PartialCyclicOrder IntChronon where
    cycle = Chronon.cycle

instance CyclicOrder IntChronon

instance Test.PartialOrder IntChronon where
    (<=) = (<=)

instance TotalOrder IntChronon

instance Synchronicity IntChronon where
    synchronous = Chronon.synchronous

instance Concurrency IntChronon where
    concurrent = Chronon.concurrent
    
instance Betweenness IntChronon where
    betweenness = Relation.betweenness

instance Begin IntChronon

instance End IntChronon

-- | spec
spec :: TestTree
spec = testGroup "Chronon Spec" [ relationLaws, orderLaws ]

relationLaws :: TestTree
relationLaws = testGroup "Relation Laws"
    [ QC.testProperties "Identity" $ Test.Identity.laws @IntChronon
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
  