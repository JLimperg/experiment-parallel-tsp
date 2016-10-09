{-# LANGUAGE OverloadedStrings #-}

module TSP.Parser
( parseTSP
, parseTSPFile
) where

import           Control.Monad
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (maybe)
import           Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import           Text.Megaparsec
import           Text.Megaparsec.Lexer (integer, scientific, signed)
import           Text.Megaparsec.Text

import           TSP.Types


packParser :: Parser String -> Parser Text
packParser = fmap Text.pack


handleEitherA :: (Applicative f) => (a -> f b) -> Either a b -> f b
handleEitherA err = either err pure


scientificToDouble :: (Monad m) => Scientific -> m Double
scientificToDouble n = handleEitherA onError $ Scientific.toBoundedRealFloat n
  where
    onError _ = fail $ "Coordinate not representable as Double: " ++ show n


scientificDouble :: Parser Double
scientificDouble = scientificToDouble =<< scientific


config :: Parser (HashMap Text Text)
config = Map.fromList <$> many (try configItem)
  where
    configItem :: Parser (Text, Text)
    configItem = (,) <$> (configKey <* configSep) <*> (configVal <* eol)

    configKey :: Parser Text
    configKey = packParser $ some (alphaNumChar <|> char '_')

    configSep :: Parser ()
    configSep = void $ between space space (char ':')

    configVal :: Parser Text
    configVal = packParser $ some (noneOf ['\n', '\r'] <?> "end of line")


nodes :: Parser [Node]
nodes = beginNodeSection *> (some node <* endOfFile)
  where
    beginNodeSection :: Parser ()
    beginNodeSection = void $ string "NODE_COORD_SECTION" *> eol

    node :: Parser Node
    node =   Node
         <$> (fromIntegral <$> integer <* space)
         <*> (scientificDouble <* space)
         <*> (scientificDouble <* eol)

    endOfFile :: Parser ()
    endOfFile = string "EOF" *> optional eol *> eof


distanceFunction :: (Monad m) => HashMap Text Text -> m DistanceFunction
distanceFunction conf = do
    id <- maybe errNoDistFun pure $ Map.lookup "EDGE_WEIGHT_TYPE" conf
    case id of
      "EUC_2D" -> pure Euclid2D
      "GEO"    -> pure Geo
      _        -> fail $ "Unknown distance function: " ++ Text.unpack id
  where
    errNoDistFun = fail "Expected config key EDGE_WEIGHT_TYPE not found."


tsp :: Parser TSP
tsp = TSP
    <$> (distanceFunction =<< config)
    <*> nodes


parseTSP :: Text -> Either (ParseError Char Dec) TSP
parseTSP = parse tsp ""


parseTSPFile :: String -> IO (Either (ParseError Char Dec) TSP)
parseTSPFile sourceFile = do
  input <- Text.readFile sourceFile
  pure $ parse tsp sourceFile input
