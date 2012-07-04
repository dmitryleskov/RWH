import Data.List (sortBy)

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
                 deriving (Show)

{--
10. Write a function that calculates the turn made by three 2D points
    and returns a Direction.
--}    

-- Without looking up the topic in literature, I produced a solution
-- using the atan2 function (but had to look up the order of its arguments)
turn :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Direction
turn a b c 
    | delta > 0 = TurnLeft
    | delta < 0 = TurnRight
    | otherwise = GoStraight
    where 
        delta = normalize (theta a c - theta a b)
        -- Angle that vector (x1, y1) (x2, y2) forms with the X axis
        theta :: (Double, Double) -> (Double, Double) -> Double
        theta (x1, y1) (x2, y2) = atan2 (y2-y1) (x2-x1)
        -- Difference of two (normalized) angles can go beyond [-pi, pi],
        -- but never beyond [-pi*2, pi*2]
        normalize angle 
                  | angle < -pi = angle + pi * 2
                  | angle >  pi = angle - pi * 2
                  | otherwise   = angle

-- Using cross product:
turn1 :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Direction
turn1 a b c
      | cp a b c > 0 = TurnLeft
      | cp a b c < 0 = TurnRight
      | otherwise = GoStraight
        where cp (x1,y1) (x2,y2) (x3,y3) = 
               (x2-x1) * (y3-y1) - (y2-y1) * (x3-x1)

{--
11. Define a function that takes a list of 2D points and computes 
     the direction of each successive triple. Given a list of points 
     [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], 
     then the turn made by [b,c,d], then [c,d,e]. Your function 
     should return a list of Direction.
--}

dirs :: [(Double, Double)] -> [Direction]
dirs []         = []
dirs (_:[])     = []
dirs (_:_:[])   = []
dirs (a:b:c:ps) = turn1 a b c : dirs (b:c:ps)

        
{--        
12. Using the code from the preceding three exercises, implement 
    Graham's scan algorithm for the convex hull of a set of 2D points. 
    You can find good description of what a convex hull. is, 
    and how the Graham scan algorithm should work, on Wikipedia.
    http://en.wikipedia.org/wiki/Convex_hull
    http://en.wikipedia.org/wiki/Graham_scan
--}

gs :: [(Double, Double)] -> [(Double, Double)]
gs []         = []
gs (a:[])     = [a]
gs (a:b:[])   = [a,b]
gs (a:b:c:[]) = [a,b,c]
gs (p:ps) = [bottomLeft p ps]
   where bottomLeft p [] = p
         bottomLeft p (q:qs) = if   p `isBetterThan` q 
                               then bottomLeft p qs
                               else bottomLeft q qs
                               where
                                 isBetterThan (x1, y1) (x2, y2) =
                                    y1 < y2 || y1 == y2 && x1 < x2

cosine (x1, y1) (x2, y2) = x2-x1 / sqrt ( (x2-x1)^2+(y2-y1)^2 )


{--
sortBy bottomLeft ps
          where bottomLeft (x1, y1) (x2, y2) 
                        | y1 < y2   = LT
                        | y1 > y2   = GT
                        | x1 < x2   = LT
                        | x1 > x2   = GT
                        | otherwise = EQ
--}              
