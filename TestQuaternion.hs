module TestQuaternion where
import           Quaternion
import           Test.HUnit
import           Types

__a = Quaternion 1.0 2.0 3.0 4.0
__b = Quaternion 2.0 3.0 4.0 5.0
__c = Quaternion (2-6-12-20) (3+4+15-16) (4-10+6+12) (5+8-9+8)

test1 = TestCase (assertEqual "a+b-b=a"
                              __a
                              ((__a + __b) - __b))
test2 = TestCase (assertEqual "a-b+b=a"
                              __a
                              ((__a - __b) + __b))
test3 = TestCase (assertEqual "a=a"
                              __a
                              __a)
test4 = TestCase (assertEqual "a!=b"
                              False
                              (__a==__b))
test5 = TestCase (assertEqual "a*b=c"
                              __c
                              (__a*__b))
test6 = TestCase (assertEqual "0.5*(2*a)=a"
                              __a
                              (scalarQ 0.5 (scalarQ 2 __a)))
test7 = TestCase (assertEqual "a^*a=aa^*"
                              (__a*(conjugate __a))
                              ((conjugate __a)*__a) )
test8 = TestCase (assertEqual "i/|i|=i"
                              i
                              (toNorm i))
test9 = TestCase (assertEqual "|q/|q||=1"
                              True
                              (1-(norm (toNorm (Quaternion 1 3 4 5)))<epsilon))

testsQuaternion = TestList [TestLabel "a+b-b=a"     test1,
                  TestLabel "a-b+b=a"     test2,
                  TestLabel "a=a"         test3,
                  TestLabel "a!=b"        test4,
                  TestLabel "a*b=c"       test5,
                  TestLabel "0.5*(2*a)=2" test6,
                  TestLabel "a^*a=aa^*"   test7,
                  TestLabel "i/|i|=i"     test8,
                  TestLabel "|q/|q||=1"   test9]
