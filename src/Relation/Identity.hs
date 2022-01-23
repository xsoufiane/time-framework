module Relation.Identity (Identity((===))) where

class Identity t where
    (===) :: t -> t -> Bool
