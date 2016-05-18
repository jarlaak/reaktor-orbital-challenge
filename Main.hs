import           Data.Either.Combinators
import           Parser
import           Route
import           Satellite
import           Text.CSV
import           Types

data Solution = Solution { randomSeed::String, satellites::[String] }

commaSeparated "" s2 = s2
commaSeparated s1 "" = s1
commaSeparated s1 s2 = s1 ++ "," ++ s2

instance Show Solution where
  show solution = "Random seed: " ++ (randomSeed solution) ++ "\n" ++ "Route: " ++ (foldl (commaSeparated) (head sats) (tail sats))
    where
      sats = Main.satellites solution

call csvData
  = Solution (seed system) satellites
  where
    system = parseSatelliteCSV (fromRight [] csvData)
    route = solveRoute system
    (Route connections) = route
    satellites = reverse (map name (takeSatellites (map destination connections)))

main = do
  putStrLn "Solving problem..."
  csvFile <- parseCSVFromFile "generate.txt"
  putStrLn (show (call csvFile))
