import Data.List (sortBy)

-- Synonym for brevity
type Point = (Double, Double)

{--
 9. Consider three two-dimensional points a, b, and c. If we look 
    at the angle formed by the line segment from a to b and the line 
    segment from b to c, it either turns left, turns right, or forms 
    a straight line. Define a Direction data type that lets you 
    represent these possibilities.
--}    

data Direction = TurnLeft
               | TurnRight
               | GoStraight
               | GoBack
                 deriving (Eq, Show)

{--
10. Write a function that calculates the turn made by three 2D points
    and returns a Direction.
--}    

-- Without looking up the topic in literature, I produced a solution
-- using the atan2 function (but had to look up the order of its arguments)
turn0 :: Point -> Point -> Point -> Direction
turn0 a b c 
      | delta > 0 = TurnLeft
      | delta < 0 = TurnRight
      | otherwise = GoStraight
      where 
        delta = normalize (theta a c - theta a b)
        -- Angle that vector (x1, y1) (x2, y2) forms with the X axis
        theta :: Point -> Point -> Double
        theta (x1, y1) (x2, y2) = atan2 (y2-y1) (x2-x1)
        -- Difference of two (normalized) angles can go beyond [-pi, pi],
        -- but never beyond [-pi*2, pi*2]
        normalize angle 
                  | angle < -pi = angle + pi * 2
                  | angle >  pi = angle - pi * 2
                  | otherwise   = angle

-- Using cross product:
turn :: Point -> Point -> Point -> Direction
turn a b c
     | cp a b c > 0 = TurnLeft
     | cp a b c < 0 = TurnRight
     | otherwise = if b `between` (a,c)
                   then GoStraight
                   else GoBack
       where cp (x1,y1) (x2,y2) (x3,y3) = 
                (x2-x1) * (y3-y1) - (y2-y1) * (x3-x1)
             between :: Point -> (Point, Point) -> Bool
             between (x,y) ((x1,y1), (x2,y2)) = 
                dx1 /= 0 && signum dx1 /= signum dx2 ||
                dy1 /= 0 && signum dy1 /= signum dy2
                where dx1 = x1-x
                      dx2 = x2-x
                      dy1 = y1-y
                      dy2 = y2-y

{--
11. Define a function that takes a list of 2D points and computes 
     the direction of each successive triple. Given a list of points 
     [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], 
     then the turn made by [b,c,d], then [c,d,e]. Your function 
     should return a list of Direction.
--}

dirs :: [Point] -> [Direction]
dirs []         = []
dirs (_:[])     = []
dirs (_:_:[])   = []
dirs (a:b:c:ps) = turn a b c : dirs (b:c:ps)

        
{--        
12. Using the code from the preceding three exercises, implement 
    Graham's scan algorithm for the convex hull of a set of 2D points. 
    You can find good description of what a convex hull. is, 
    and how the Graham scan algorithm should work, on Wikipedia.
    http://en.wikipedia.org/wiki/Convex_hull
    http://en.wikipedia.org/wiki/Graham_scan
    
    My comment: Wikipedia article does not discuss degenerative
    cases, i.e. when there are coindident and collinear points
    in the input set. An in-depth discussion of convex hulls
    with implementation of Graham scan in Java can be found at 
    http://www.cs.brown.edu/courses/cs016/docs/old_lectures/ConvexHull-Notes.pdf
    which in turn is reprinted from "Algorithm Design" 
    by M. T. Goodrich and R. Tamassia.
    
    The slides at
    http://www.cs.princeton.edu/courses/archive/spr03/cs226/lectures/geo.4up.pdf
    also mention quick elimination - an optional step on which 
    the "inside" points that may not be convex hull are identified 
    and removed from further consideration.
    
    My implementation does not do quick elimination 
    and assumes that there are no coincident points in the set.
--}

grahamScan :: [Point] -> [Point]

-- Trivial cases:
grahamScan []       = []
grahamScan (a:[])   = [a]
grahamScan (a:b:[]) = [a,b]

-- The general case:                  
grahamScan (a:as)   = 
        let (p:ps) = gsSort a [] as
        in scan p (p:[]) ps
        where
          -- p is the starting point
          -- ch is the hull computed so far
          -- q:qs is the list of remaining points
          -- If the list is not empty, try to add its head to the hull
          scan p ch (q:qs) = scan p (addPoint q ch) qs
          -- When there are no points left:
          scan p ch [] = 
              -- If there are just two points in ch, that means
              -- all points in the input set are collinear,
              -- so these two points constitute the convex hull.
              if null (drop 2 ch) 
              then ch
              -- Otherwise, add and drop the starting point. 
              -- If it is collinear with the last two points 
              -- in ch, addPoint will drop the "middle" one.
              else drop 1 (addPoint p ch)

-- Add a point to the convex hull
addPoint :: Point -> [Point] -> [Point]

-- Second point may always be added
addPoint q (p:[]) = q:p:[]

-- Otherwise, ensure that the last three points form a left turn
addPoint q (v1:v0:vs) = case turn v0 v1 q of
                           TurnLeft   -> q:v1:v0:vs
                           TurnRight  -> addPoint q (v0:vs)
                           GoStraight -> q:v0:vs
                           GoBack     -> v1:v0:vs

-- Preliminary steps of Graham scan:
gsSort :: Point -> [Point] -> [Point] -> [Point]

-- Scan the list to identify the leftmost of points with minimum y-coordinate
gsSort p ps (q:qs) = if   p `isBetterThan` q 
                     then gsSort p (q:ps) qs
                     else gsSort q (p:ps) qs
                     where
                         isBetterThan (x1, y1) (x2, y2) =
                           y1 < y2 || y1 == y2 && x1 < x2

-- Then sort the remaining points:
gsSort p ps [] = p : sortBy (increasingAngle) ps
                 where
                     increasingAngle a b = 
                         case turn p a b of
                             TurnLeft  -> LT
                             TurnRight -> GT
                             otherwise -> EQ
                                 
