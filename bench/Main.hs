import Test.Tasty.Bench (defaultMain)

import qualified Bench
import qualified Compare

main :: IO ()
main = defaultMain
  [ Bench.benches
  , Compare.benches
  ]
