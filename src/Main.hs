{-# LANGUAGE OverloadedStrings #-}

import           System.Environment (getArgs)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Random (getStdGen)

import           TSP

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then putStrLn "USAGE: ./tsp <file> <numberofrandomtours>"
    else do
      tsp <- parseTSP <$> Text.readFile (head args)
      gen <- getStdGen
      let tours = randomTours gen tsp (read $ args !! 1)
          mapString = Text.unlines $ map (Text.pack . show) $ Map.toList tours
      Text.writeFile (iterate init (head args) !! 4 ++ ".txt") mapString
