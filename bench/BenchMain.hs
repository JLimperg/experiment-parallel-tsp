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
main = defaultMain $ (:[]) $ env getTSP $ \tsp ->
    bgroup "tsp"
      [ bgroup "randomTours"      $ map (benchRandomTours tsp) ns
      , bgroup "randomToursPar:2" $ map (benchRandomToursPar tsp 2) ns
      , bgroup "randomToursPar:4" $ map (benchRandomToursPar tsp 4) ns
      ]
  where
    ns = [100, 1000, 10000]
    getTSP = parseTSP <$> Text.readFile tspFile


benchRandomTours :: TSP -> Int -> Benchmark
benchRandomTours tsp n
    = bench name $ nf (randomTours rng tsp) n
  where
    name = unwords ["randomTours", tspName, show n]


benchRandomToursPar :: TSP -> Int -> Int -> Benchmark
benchRandomToursPar tsp nCores n
    = bench name $ nf (randomToursPar rng tsp nCores) n
  where
    name = unwords ["randomToursPar/" ++ show nCores, tspName, show n]
