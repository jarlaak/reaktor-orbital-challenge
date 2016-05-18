module TestSatellite where
import           Quaternion
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

test1 = TestCase (assertEqual "rotate 90deg long and 45deg lat"
                              (Quaternion 0 0 (-sqrt(2)/2) (sqrt(2)/2))
                              (toGlobalCoordinate 90 45))
test2 = TestCase (assertEqual "rotate 90deg long"
                              (-j)
                              (rotate (rotationQuaternion (-k) (angleToRadians 90)) i))
test3 = TestCase (assertEqual "rotate 90deg lat"
                              (k)
                              (rotate (rotationQuaternion (-j) (angleToRadians 90)) i))
test4 = TestCase (assertEqual "phone see satellite"
                              True
                              (phoneSeeSatellite (Phone (Quaternion 0 1 0 0)) (Satellite "s" (Quaternion 0 2 0 0))))
test5 = TestCase (assertEqual "satellite in horizon"
                              True
                              (phoneSeeSatellite (Phone (Quaternion 0 1 0 0)) (Satellite "s" (Quaternion 0 1 1 1))))
test6 = TestCase (assertEqual "satellite below horizon"
                              False
                              (phoneSeeSatellite (Phone (Quaternion 0 1 0 0)) (Satellite "s" (Quaternion 0 0 1 1))))
test7 = TestCase (assertEqual "projection 1"
                              (scalarQ 2 i)
                              (projection (Quaternion 0 2 2 0) (scalarQ 6 i)))
test8 = TestCase (assertEqual "projection 2"
                              zero
                              (projection k (scalarQ 6 i)))
test9 = TestCase (assertEqual "projection 3"
                              (scalarQ (-2) i)
                              (projection (Quaternion 0 (-2) (-2) 0) (scalarQ 6 i)))
test10 = TestCase (assertEqual "phone see satellites"
                               ["s1","s3"]
                               (map name (newEndPoints ps testSatellites pe1)))
test11 = TestCase (assertEqual "satellite see satellites"
                               ["s1","s3"]
                               (map name (newEndPoints (head testSatellites) testSatellites pe2)))
test12 = TestCase (assertEqual "satellite see phone"
                               (scalarQ earthRadius j)
                               (head (map (\(Phone q) -> q) (newEndPoints (head testSatellites) testSatellites pe1))))


testsSatellite = TestList [TestLabel "latitude and longitude rotation"     test1,
                  TestLabel "long rotation 90deg" test2,
                  TestLabel "lat rotation 90deg" test3,
                  TestLabel "phone see satellite" test4,
                  TestLabel "satellite in horizon" test5,
                  TestLabel "satellite below horizon" test6,
                  TestLabel "projection 1" test7,
                  TestLabel "projection 2" test8,
                  TestLabel "projection 3" test9,
                  TestLabel "start 1" test10,
                  TestLabel "satellite lines" test11,
                  TestLabel "end 1" test12
                          ]
