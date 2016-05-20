# reaktor-orbital-challenge
Solution to Reaktor Orbital Challenge puzzle (https://reaktor.com/orbital-challenge/)

Task of the puzzle was to take list of satellites and start and end point of the call and
made legal route between those. There are normal restrictions, nodes need to see each others.
Earth is assumed to be perfect ball and its radius is 6371km.

Program takes generate.txt file, which contains satellites ID,latitude,longitude, altitude
and route ROUTE,latitude,longitude,latitude,longitude, where first latitude and longitude
pair is start point of call and second end point.
