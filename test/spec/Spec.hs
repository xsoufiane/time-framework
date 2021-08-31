import Test.Tasty

import qualified Data.Chronon.Spec as ChrononSpec
import qualified Data.Period.Spec as PeriodSpec

-----------------------------------

main :: IO ()
main = defaultMain $ testGroup "Trajectory Algebra Spec"
    [ ChrononSpec.spec
    , PeriodSpec.spec
    ]

