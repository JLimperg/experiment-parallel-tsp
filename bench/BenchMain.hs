import           Criterion.Main
import qualified Data.Text.IO as Text
import           System.Random (StdGen, mkStdGen)

import           TSP


tspName :: String
tspName = "pcb442"

tspFile :: String
tspFile = "problems/" ++ tspName ++ ".tsp"

rng :: StdGen
rng = mkStdGen 0


main :: IO ()
main = do
  tsp <- parseTSP <$> Text.readFile tspFile
  let benchRandomTours' = benchRandomTours tsp
  defaultMain
    [ bgroup "randomTours"
      [ benchRandomTours' 100
      , benchRandomTours' 1000
      , benchRandomTours' 10000
      ]
    ]


benchRandomTours :: TSP -> Int -> Benchmark
benchRandomTours tsp n
    = bench name $ nf (randomTours rng tsp) n
  where
    name = unwords ["randomTours", tspName, show n]
