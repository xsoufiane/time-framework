module Test.Relation.Order.CyclicOrder where

import Test.Relation.Identity
import Test.Relation.Order.PartialCyclicOrder

------------------------

class (Identity a, PartialCyclicOrder a) => CyclicOrder a
