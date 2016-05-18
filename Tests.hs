import           Test.HUnit

import           TestQuaternion
import           TestRoute
import           TestSatellite

tests = TestList (concat (map (\(TestList ls) -> ls)[testsQuaternion,testsSatellite,testRoute]))

main = do
  putStrLn "Run tests..."
  runTestTT tests
