module Parser where
import           Data.String.Utils
import           Satellite
import           Text.CSV
import           Types

parseSatellite (id:lat:long:height:ls) = Satellite id (toGlobal (read long) (read lat) (read height))

parseRoute (_:lat1:long1:lat2:long2:ls)
  = ((Phone (toGlobal (read long1) (read lat1) 0.0)),(Phone (toGlobal (read long2) (read lat2) 0.0)))

getRoute (l:ls)
  | startswith "ROUTE" (head l) = (parseRoute l)
  | otherwise = getRoute ls

getSatellites :: CSV -> [Node]
getSatellites [] = []
getSatellites (l:ls)
  | startswith "SAT" (head l) = (parseSatellite l):(getSatellites ls)
  | otherwise = getSatellites ls

parseSatelliteCSV sat
  = System seed satellites (fst route) (snd route)
  where
    seed = head (tail (split " " (head (head sat))))
    satellites = getSatellites sat
    route = getRoute sat
