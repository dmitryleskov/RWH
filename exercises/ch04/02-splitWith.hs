{-
 2. Write a function splitWith that acts similarly to words, 
    but takes a predicate and a list of any type, and splits
    its input list on every element for which the predicate 
    returns False.
-}    

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = if null y
                 then []
                 else y : (splitWith p ys)
                 where 
                   (y,ys) = splitOne p (skip p xs)

splitOne :: (a -> Bool) -> [a] -> ([a],[a])
splitOne p [] = ([],[])
splitOne p (x:xs) = if p x
                    then (x:ys,zs)
                    else ([],x:xs)
                    where (ys,zs) = splitOne p xs
skip :: (a -> Bool) -> [a] -> [a]
skip p [] = []
skip p (x:xs) = if p x 
                then x:xs
                else skip p xs
