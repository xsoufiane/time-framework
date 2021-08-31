module Test.Relation.Identity 
    ( -- * Observation
      Identity((===))
    ) where
  
--------------------------------------

-- | Observation
class Identity a where
  (===) :: a -> a -> Bool  
  