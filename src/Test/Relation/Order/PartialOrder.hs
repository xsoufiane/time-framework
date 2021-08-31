module Test.Relation.Order.PartialOrder where

import Prelude hiding ((<=))  
  
-------------------------------

class (Eq a) => PartialOrder a where
  (<=) :: a -> a -> Bool
  
  (>=) :: a -> a -> Bool
  x >= y = y <= x
