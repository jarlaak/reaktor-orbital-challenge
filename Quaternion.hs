module Quaternion where

import           Types

addQ (Quaternion t1 x1 y1 z1) (Quaternion t2 x2 y2 z2)
        = Quaternion (t1+t2) (x1+x2) (y1+y2) (z1+z2)
subQ (Quaternion t1 x1 y1 z1) (Quaternion t2 x2 y2 z2)
        = Quaternion (t1-t2) (x1-x2) (y1-y2) (z1-z2)

mulQ (Quaternion t1 x1 y1 z1) (Quaternion t2 x2 y2 z2)
        = Quaternion (t1*t2-x1*x2-y1*y2-z1*z2)
              (t1*x2+x1*t2+y1*z2-z1*y2)
              (t1*y2-x1*z2+y1*t2+z1*x2)
              (t1*z2+x1*y2-y1*x2+z1*t2)

scalarQ a (Quaternion t1 x1 y1 z1)
        = Quaternion (a*t1) (a*x1) (a*y1) (a*z1)

conjugate :: Quaternion -> Quaternion
conjugate q
        = scalarQ (-0.5) (q + i*q*i + j*q*j + k*q*k)

negateQ (Quaternion t i j k) = Quaternion (-t) (-i) (-j) (-k)

norm :: Quaternion -> Double
norm q = sqrt (dot q q)

toNorm q = scalarQ (1/(norm q)) q

rotationQuaternion q angle
    = (Quaternion (cos (0.5*angle)) 0 0 0)
      + scalarQ (sin (0.5*angle))
                (toNorm q)

rotate rotation q
   = rotation * q * (conjugate rotation)

dot (Quaternion t1 i1 j1 k1) (Quaternion t2 i2 j2 k2)
   = t1 * t2 + i1 * i2 + j1 * j2 + k1 * k2

instance Eq Quaternion where
  (==) (Quaternion t1 x1 y1 z1)
       (Quaternion t2 x2 y2 z2)
         = (abs (t1-t2) < epsilon) &&
           (abs (x1-x2) < epsilon) &&
           (abs (y1-y2) < epsilon) &&
           (abs (z1-z2) < epsilon)

instance Num Quaternion where
  (+)         = addQ
  (-)         = subQ
  (*)         = mulQ
  abs         = undefined
  negate      = negateQ
  signum      = undefined
  fromInteger = undefined

