module TestRoute where
import           Quaternion
import           Route
import           Satellite
import           Test.HUnit
import           Types

ps = Phone (scalarQ earthRadius i)
pe1 = Phone (scalarQ earthRadius j)
pe2 = Phone (scalarQ earthRadius (-i))
s1 = Satellite "s1" (scalarQ (earthRadius + 100) (i+j))
s2 = Satellite "s2" (scalarQ (earthRadius + 100) (-i))
s3 = Satellite "s3" (scalarQ (earthRadius + 100) ((scalarQ 2 i) - j))
testSatellites = [s1,s2,s3]
c1 = Connection s1 s2
c2 = Connection s2 s3
r1 = Route [c2,c1]
r2 = Route [(Connection s3 s2),c2,c1]

test13 = TestCase (assertEqual "newCall"
                               ["s1","s3"]
                               (map name (map routeEndPoint (newCall ps testSatellites pe1))))
test14 = TestCase (assertEqual "route end point"
                               "s3"
                               (name (routeEndPoint (Route [(Connection s2 s3),(Connection s1 s2)]))))
test15 = TestCase (assertEqual "no cycle"
                               False
                               (isCycle (Route [(Connection s1 s2),(Connection s2 s3)])))
test16 = TestCase (assertEqual "cycle"
                               True
                               (isCycle (Route [(Connection s1 s2),(Connection s2 s3), (Connection s3 s2)])))
test17 = TestCase (assertEqual "unique routes"
                               ["s3"]
                               (map name (map routeEndPoint (takeUniqueRoutes [r1,r2]))))
test18 = TestCase (assertEqual "distance"
                               14469.59588240114
                               (distance c1))
test19 = TestCase (assertEqual "routeDistance"
                               34932.69462135072
                               (routeDistance r1))
test20 = TestCase (assertEqual "sort routes 1"
                               ["s3","s2"]
                               (map name (map routeEndPoint (sortRoutes [r1,r2]))))
test21 = TestCase (assertEqual "sort routes 2"
                               ["s3","s2"]
                               (map name (map routeEndPoint (sortRoutes [r2,r1]))))

testRoute = TestList [
                  TestLabel "new call" test13,
                  TestLabel "route end point" test14,
                  TestLabel "no cycle" test15,
                  TestLabel "cycle" test16,
                  TestLabel "unique routes" test17,
                  TestLabel "connection distance" test18,
                  TestLabel "route distance" test19,
                  TestLabel "sort routes 1" test20,
                  TestLabel "sort routes 2" test21
                      ]
