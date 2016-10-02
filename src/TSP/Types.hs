{-# OPTIONS_GHC -funbox-small-strict-fields #-}
{-# LANGUAGE DeriveGeneric #-}

module TSP.Types where


import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)


data TSP
    = TSP
    { tspDistanceFunction :: DistanceFunction
    , tspNodes :: [Node]
    }
  deriving (Read, Show, Eq, Ord, Generic)

instance NFData TSP


data DistanceFunction = Euclid2D | Geo
  deriving (Read, Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData DistanceFunction


data Node = Node !Int !Double !Double
  deriving (Read, Show, Eq, Ord, Generic)

instance NFData Node
