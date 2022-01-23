module Structure.Order.PartialCyclicOrder where

--------------------

class PartialCyclicOrder a where
    cycle :: a -> a -> a -> Bool
