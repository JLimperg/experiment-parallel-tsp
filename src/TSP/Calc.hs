module TSP.Calc
( tourLength
) where


import           TSP.Types


distanceFunction :: DistanceFunction -> Node -> Node -> Int
distanceFunction Euclid2D = euc_2d
distanceFunction Geo      = geo


-- differencesWith f [x1, ..., xn] = [x2 `f` x1, x3 `f` x2, ..., xn `f` xn-1]
differencesWith :: (a -> a -> b) -> [a] -> [b]
differencesWith f [] = []
differencesWith f xs = zipWith f (tail xs) xs


-- calculates the length of a given tour.
tourLength :: DistanceFunction -> [Node] -> Int
tourLength f = sum . differencesWith (distanceFunction f)


-- calculate the euclidean distance between two nodes
euc_2d :: Node -> Node -> Int
euc_2d (Node a ax ay) (Node b bx by)
    = round $ sqrt $ (ax - bx) ** 2 + (ay - by) ** 2


-- geo calculates the geographic distance between two nodes. Positive latitude is north, positive longitude is east.
-- The floating-point representation is just a handy shorthand, the digits after the decimal point are actually the
-- minutes, so we need to calculate the geographical latitude and longitude in radians from the degree/minute form.
-- geo also uses a couple of idealized constants declared below.

-- the approximate radius of the earth, as specified by the .tsp format
tspEarthRadius :: Double
tspEarthRadius = 6378.388


-- a less-precise version of pi, as specified by the .tsp format
tspPi :: Double
tspPi = 3.141592


-- converting GEO-type coordinates (format degrees.minutes) to the distance in radians from the equator and prime meridian.
-- We'll need to convert latitude and longitude with this for calculation of geographic distance.
toRad :: Double -> Double
toRad x = tspPi * (deg + 5.0 * min / 3.0) / 180
  where
    deg = fromIntegral $ truncate x
    min = x - deg


latitude, longitude :: Node -> Double
latitude  (Node _ ax _) = toRad ax
longitude (Node _ _ ay) = toRad ay


geo :: Node -> Node -> Int
geo a b
    = truncate
    $ tspEarthRadius * acos (0.5*((1.0 + q1)*q2 - (1.0 - q1)*q3)) + 1.0
  where
    q1 = cos (longitude a - longitude b)
    q2 = cos (latitude a - latitude b)
    q3 = cos (latitude a + latitude b)
