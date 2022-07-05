module Structure.Identity (Identity((===))) where

----------------------------------------------

class Identity a where
  (===) :: a -> a -> Bool  
