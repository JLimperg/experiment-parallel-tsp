{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TSP
( module TSP.Types
, module TSP.Parser
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
import           TSP.Parser
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
