module Structure.Order.CyclicOrder where

import Structure.Identity
import Structure.Order.PartialCyclicOrder

------------------------

class (Identity a, PartialCyclicOrder a) => CyclicOrder a
