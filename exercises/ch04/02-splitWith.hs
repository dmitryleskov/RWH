{-
 2. Write a function splitWith that acts similarly to words, 
    but takes a predicate and a list of any type, and splits
    its input list on every element for which the predicate 
    returns False.
-}    

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = let (first,rest) = splitOne p (skip p xs)
                  in if null first
                     then []
                     else first : (splitWith p rest)
                 where 
                   -- Takes elements while the predicate returns True,
                   -- then drops while the predicate returns False.
                   -- Returns tuple (taken elements, remaining elements)
                   splitOne :: (a -> Bool) -> [a] -> ([a],[a])
                   splitOne p [] = ([],[])
                   splitOne p (x:xs) = if p x
                                       then (x:ys,rest)
                                       else ([], skip p xs)
                                       where (ys,rest) = splitOne p xs
                   -- Drops list elements while predicate returns False
                   skip :: (a -> Bool) -> [a] -> [a]
                   skip p [] = []
                   skip p (x:xs) = if p x 
                                   then x:xs
                                   else skip p xs
