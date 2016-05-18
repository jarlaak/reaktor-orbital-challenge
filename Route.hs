module Route where

import           Data.List
import           Quaternion
import           Satellite
import           Types

routeEndPoint route
  = dest
  where
    lastConnection = head (connections route)
    dest = destination lastConnection

newCall startPhone satellites endPhone
  = map (\node -> Route [(Connection startPhone node)]) endPoints
  where
    endPoints = newEndPoints startPhone satellites endPhone

-- Expand route
newRoutes route  satellites endPhone
  = map (\end -> (Route ((Connection n end):(connections route)))) endPoints
  where
    endPoints = newEndPoints n satellites endPhone
    lastConnection = head (connections route)
    (Connection _ n) = lastConnection

isCycle route
  = (length ids) /= (length (nub ids))
  where
    nodes = map destination (connections route)
    satellites = takeSatellites nodes
    ids = map name satellites

takeUniqueRoutes routes
  = filter (not.isCycle) routes

-- Eucledian distance between nodes, can also be any other distance
-- Example: discrete metric: "= 1", then distance is node count
distance connection
  = norm (q1 - q2)
  where
    q1 = location (source connection)
    q2 = location (destination connection)

routeDistance route
  = foldl (+) 0.0 (map distance (connections route))

compareRoute r1 r2
  | d1 < d2 = LT
  | d1 > d2 = GT
  | otherwise = EQ
  where
    d1 = routeDistance r1
    d2 = routeDistance r2

-- sorts routes from shortest to longest
sortRoutes routes
  = sortBy compareRoute routes

routeEnds route = isPhone (routeEndPoint route)

-- Iterate until shorters route ends to destination point.
-- This can be improved so that it remove routes that use same point
-- and take only shortest one
iterateRoute routes satellites pe
 | isPhone (routeEndPoint first) = first
 | otherwise                     = iterateRoute nextRound satellites pe
 where
   first = head routes
   rest = tail routes
   new = newRoutes first satellites pe
   allRoutes = new ++ rest
   nextRound = sortRoutes (takeUniqueRoutes allRoutes)

solveRoute (System _ satellites ps pe)
  = iterateRoute initRoutes satellites pe
  where
    initRoutes = sortRoutes (newCall ps satellites pe)
