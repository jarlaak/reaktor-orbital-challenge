module Types where

epsilon = 0.00001
earthRadius = 6371.0

data Quaternion =
      Quaternion Double Double Double Double

i = Quaternion 0 1 0 0
j = Quaternion 0 0 1 0
k = Quaternion 0 0 0 1
zeroR = Quaternion 1 0 0 0
zero = Quaternion 0 0 0 0

type Location = Quaternion
type Distance = Double
type Id = String

data Node = Phone {location::Location} | Satellite {name::String, location::Location } deriving Show
data Connection = Connection {source::Node, destination::Node}

type Connections = [Connection]
data Route = Route{ connections::Connections }

data System = System { seed::String, satellites::[Node], start::Node, end::Node } deriving Show

instance Show Quaternion where
  show (Quaternion t x y z)
         = "("++show t++","++show x++"i,"
              ++show y++"j,"++show z++"k)"
