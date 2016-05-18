module Satellite where

import           Quaternion
import           Types

longitudeRotation
  = rotationQuaternion (-k)

latitudeRotation
  = rotationQuaternion (-j)

angleToRadians ang = pi / 180.0 * ang

-- convert polar coordinates to cartesian coordinates
toGlobalCoordinate long lat
   = rotate longRot (rotate latRot i)
   where
      longRot = longitudeRotation (angleToRadians long)
      latRot = latitudeRotation (angleToRadians lat)

toGlobal long lat h
  = scalarQ (earthRadius + h) (toGlobalCoordinate long lat)

phoneSeeSatellite phone satellite
   = (dot pq (sq - pq)) >= 0.0
   where
     pq = location phone
     sq = location satellite

projection q1 q2 = scalarQ ((dot q1 q2) / (dot q2 q2)) q2

nodeLine n1 n2
   = q2 - q1
   where
     q1 = location n1
     q2 = location n2

satelliteLineProjection s1 s2
   = projection orig line
   where
      line = nodeLine s1 s2
      orig = -(location s1)

closestPointBetweenSatellites s1 s2
   = sameDirection  && beforeSecondSatellite
   where
      line = nodeLine s1 s2
      projection = satelliteLineProjection s1 s2
      sameDirection = (dot line projection) > 0
      sqLen q = dot q q
      beforeSecondSatellite = (sqLen projection)/(sqLen line)<1.0

-- Satellites can see each others
lineOutsideEarth s1 s2
  = distOutside || sameSide
  where
     projection = satelliteLineProjection s1 s2
     q1 = location s1
     radius = q1 + projection
     distOutside = ((dot radius radius) - earthRadius * earthRadius) > 0.0
     sameSide = not (closestPointBetweenSatellites s1 s2)

newEndPoints (Phone q) satellites _
  = filter (phoneSeeSatellite (Phone q)) satellites
newEndPoints  satellite satellites endPhone
  | end       = [endPhone]
  | otherwise = filter (lineOutsideEarth satellite) satellites
  where
    end = phoneSeeSatellite endPhone satellite

takeSatellites [] = []
takeSatellites ((Phone _):satellites) = takeSatellites satellites
takeSatellites (s:satellites) = s:(takeSatellites satellites)

isPhone (Phone _ ) = True
isPhone _ = False
