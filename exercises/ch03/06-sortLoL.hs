{--
 6. Create a function that sorts a list of lists based on the length
    of each sublist. (You may want to look at the sortBy function 
    from the Data.List module.)
--}

import Data.List (sortBy, sort)
import Data.Function (on)

-- Could not invent anything else without using material
-- from later chapters.
-- Works well when sizes of lists are substantially different.
sortLoL :: [[a]] -> [[a]]
sortLoL x = sortBy cmp x
            where cmp :: [a] -> [a] -> Ordering
                  cmp [] [] = EQ
                  cmp [] _  = LT
                  cmp _  [] = GT
                  cmp (x:xs) (y:ys) = cmp xs ys


-- One-liner by gerg...
sortByLength xss = map snd (sort (zip (map length xss) xss))

-- ... generalized by Ivan so that "dsuSort length list" sorts
-- by length, "dsuSort sum list" sorts by sum, and so on.

-- gerg's implementation uses decorate-sort-undecorate. 
-- One step and it's general:
dsuSort :: (Ord a, Ord a1) => (a1 -> a) -> [a1] -> [a1]
dsuSort decFunc a = map snd (sort (zip (map decFunc a) a))

-- Solution by Psdb, does essentially the same, but
-- is less readable to my taste.
-- Uses map, on, and lambda
sortLists :: [[a]] -> [[a]]
sortLists list = getLists $ sortBy length' mappedlist
                 where
                   mappedlist = map (\x -> (length x, x)) list
                   length' = compare `on` fst
                   getLists = map snd
