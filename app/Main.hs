{-# LANGUAGE TupleSections #-}

import           Control.Concurrent (getNumCapabilities)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.FilePath (dropExtension, (<.>), (</>))
import           System.Random (getStdGen)
import           Text.Read (readMaybe)
import           Text.Megaparsec (ParseError, Dec)
import qualified Text.Megaparsec as Parsec

import           TSP

main :: IO ()
main = do
    (tspFile, n) <- validateArgs =<< getArgs
    tsp <- validateTSP =<< parseTSPFile tspFile

    gen <- getStdGen
    nCores <- getNumCapabilities
    let tours = randomToursPar gen tsp nCores n
        mapString = Text.unlines $ map (Text.pack . show) $ Map.toList tours
        outFile = dropExtension tspFile <.> "txt"
    Text.writeFile outFile mapString
  where
    validateArgs :: [String] -> IO (FilePath, Int)
    validateArgs args
        = case args of
            [f, n] ->
              (f,) <$> maybe (die $ "Not a number: " ++ n) pure (readMaybe n)
            _  ->
              die "USAGE: ./tsp <file> <numberofrandomtours>"

    validateTSP :: Either (ParseError Char Dec) TSP -> IO TSP
    validateTSP = either (die . Parsec.parseErrorPretty) pure
