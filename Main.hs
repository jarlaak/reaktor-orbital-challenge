import           Data.Either.Combinators
import           Parser
import           Route
import           Satellite
import           Solution
import           Text.CSV
import           Types

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
