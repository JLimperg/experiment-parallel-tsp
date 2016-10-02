{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TSP
( module TSP.Types
, parseTSP
, randomTours
, randomToursPar
) where

import           Control.Monad.ST (ST, runST)
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Array.ST
  (MArray, STArray, getElems, newListArray, readArray, writeArray)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Random (RandomGen, randomR, split)

import           TSP.Calc
import           TSP.Types


randomTours :: (RandomGen g) => g -> TSP -> Int -> HashMap Int Int
randomTours gen (TSP distance nodes) n
    = mapRandomTours distance n nodes gen


splits :: (RandomGen g) => g -> [g]
splits = iterate (snd . split)


unionsWith
    :: (Eq k, Hashable k) => (v -> v -> v) -> [HashMap k v] -> HashMap k v
unionsWith f = foldr (Map.unionWith f) Map.empty


randomToursPar :: (RandomGen g) => g -> TSP -> Int -> Int -> HashMap Int Int
randomToursPar gen tsp nCores n
    = let gens  = take nCores $ splits gen
          n'    = n `div` nCores
          tours = parMap rdeepseq (\gen -> randomTours gen tsp n') gens
      in  unionsWith (+) tours


extractDistanceFunction :: [(Text, Text)] -> DistanceFunction
extractDistanceFunction
    = dispatchDistance . snd . head
    . dropWhile (\(opt, _) -> opt /= "EDGE_WEIGHT_TYPE")
  where
    dispatchDistance "EUC_2D" = Euclid2D
    dispatchDistance "GEO"    = Geo
    dispatchDistance d
        = error $ "Unknown distance function: " ++ Text.unpack d


mapRandomTours :: (RandomGen g) => DistanceFunction -> Int -> [Node] -> g -> HashMap Int Int
mapRandomTours f n tour = mapRandomTours' f n tour Map.empty
  where mapRandomTours' f 0 tour m g = m
        mapRandomTours' f n tour m g = let (randomTour, newGen) = shuffle tour g
                                           dist = tourLength f randomTour
                                           newMap = case Map.lookup dist m
                                                    of Nothing -> Map.insert dist 1 m
                                                       Just freq -> Map.adjust succ dist m
                                       in seq newMap mapRandomTours' f (n-1) tour newMap newGen

shuffle :: forall a g. (RandomGen g) => [a] -> g -> ([a], g)
shuffle xs gen = runST $ do
  arr <- newListArray (0, n-1) xs :: ST s (STArray s Int a)
  gen' <- shuffleRec arr 0 gen
  xs' <- getElems arr
  return (xs', gen')
  where n = length xs
        shuffleRec arr counter g = if counter >= (n-1)
                                   then return g
                                   else
                                     do
                                       let (pick, newGen) = randomR (counter, n-1) g
                                       pickVal <- readArray arr pick
                                       counterVal <- readArray arr counter
                                       writeArray arr pick counterVal
                                       writeArray arr counter pickVal
                                       shuffleRec arr (counter+1) newGen

-- a function to parse and interpret simple Traveling Salesman problems if given in the correct format. Doesn't work on all
-- .tsp files/texts. It's only written for basic TSP problems which have a specification section and a coordinate section.
-- we divide the text into specifications (what type of edge weight, what kind of problem, etc.) and nodes (consisting of
-- an integer ID, an x and a y coordinate), then we simply put the specs into pairs while reading the numeric values of the
-- nodes, before returning a pair containing a list of specs and a list of nodes.
parseTSP :: Text -> TSP
parseTSP spec
    = let (specs, nodes) = parseTSP' spec in
      TSP (extractDistanceFunction specs) nodes


parseTSP' :: Text -> ([(Text, Text)], [Node])
parseTSP' tspSpec = (map (\spec -> let (keyword, value) = Text.break (==' ') spec
                                   in (keyword, Text.drop 3 value))
                       specs
                    ,map (\node -> let vals = Text.words node
                                   in Node (read . Text.unpack $ head vals) (read . Text.unpack $ vals!!1) (read . Text.unpack $ vals!!2))
                       $ stripJunk nodes)
  where (specs, nodes) = Prelude.break (=="NODE_COORD_SECTION") $ Text.lines tspSpec
        stripJunk txt  = Prelude.dropWhile (\x -> x == "" || Text.isInfixOf "_SECTION" x)
                         (if last txt == "EOF" then init txt else txt)
