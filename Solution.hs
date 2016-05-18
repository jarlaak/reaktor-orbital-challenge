module Solution where
import           Types

data Solution = Solution { randomSeed::String, satellites::[String] }

commaSeparated "" s2 = s2
commaSeparated s1 "" = s1
commaSeparated s1 s2 = s1 ++ "," ++ s2

instance Show Solution where
  show solution = "Random seed: " ++ (randomSeed solution) ++ "\n" ++ "Route: " ++ (foldr (commaSeparated) "" sats)
    where
      sats = Solution.satellites solution
