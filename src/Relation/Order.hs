{-# LANGUAGE ConstrainedClassMethods #-}

module Relation.Order (Order((<), (>), betweenness, (<=), (>=))) where

import Prelude hiding ((<), (<=), (>), (>=))

------------------------------------------------------------------

class Order t where
    (<) :: t -> t -> Bool
             
    (>) :: t -> t -> Bool
    x > y = y < x
    
    -- | x is between y z
    betweenness :: t -> t -> t -> Bool
    betweenness x y z = (y < x && x < z) || (z < x && x < y)

    (<=) :: Eq t => t -> t -> Bool
    x <= y = x < y || x == y

    (>=) :: Eq t => t -> t -> Bool
    x >= y = y <= x
