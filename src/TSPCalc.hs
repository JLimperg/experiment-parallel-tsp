module TSPCalc ( Node, DistanceFunction
               , tourLength, euc_2d, geo
               ) where

import Data.List

type Node = (Int, Double, Double)
type DistanceFunction = Node -> Node -> Int

-- calculates the length of a given tour.
tourLength :: (Node -> Node -> Int) -> [Node] -> Int
tourLength f tour = snd $ foldl' (\(lastNode, total) node -> (node, total + f node lastNode)) (head tour, 0) tour

-- calculate the euclidean distance between two nodes
euc_2d :: Node -> Node -> Int
euc_2d (a, ax, ay) (b, bx, by) = round $ sqrt $ (ax - bx) ** 2 + (ay - by) ** 2

-- geo calculates the geographic distance between two nodes. Positive latitude is north, positive longitude is east.
-- The floating-point representation is just a handy shorthand, the digits after the decimal point are actually the
-- minutes, so we need to calculate the geographical latitude and longitude in radians from the degree/minute form.
-- geo also uses a couple of idealized constants declared below.

-- the approximate radius of the earth, as specified by the .tsp format
earthRadius :: Double
earthRadius = 6378.388

-- a less-precise version of pi, as specified by the .tsp format
pi2 :: Double
pi2 = 3.141592

-- converting GEO-type coordinates (format degrees.minutes) to the distance in radians from the equator and prime meridian.
-- We'll need to convert latitude and longitude with this for calculation of geographic distance.
toRad :: Double -> Double
toRad x = pi2 * (deg + 5.0 * min / 3.0) / 180
  where deg = fromIntegral $ truncate x
        min = x - deg

-- apply to a @Node@.
latitude (a, ax, ay) = toRad ax
longitude (a, ax, ay) = toRad ay

geo :: Node -> Node -> Int
geo a b = truncate $ earthRadius * (acos $ 0.5*((1.0 + q1)*q2 - (1.0 - q1)*q3)) + 1.0
  where q1 = cos (longitude a - longitude b)
        q2 = cos (latitude a - latitude b)
        q3 = cos (latitude a + latitude b)
