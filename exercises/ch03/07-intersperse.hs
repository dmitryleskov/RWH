{--
 7. Define a function that joins a list of lists together using
    a separator value. The separator should appear between elements
    of the list, but should not follow the last element. 
--}    

-- Straightforward, but quite fast, most likely due to lazy evaluation
-- and right associativeness of (++)
intersperse :: a -> [[a]] -> [a]
intersperse _    []  = []
intersperse _ (x:[]) = x 
intersperse s (x:xs) = x ++ [s] ++ intersperse s xs

-- Variation (same speed)
intersperse1 :: a -> [[a]] -> [a]
intersperse1 _    []  = []
intersperse1 s (x:xs) = x ++ intersperse1' s xs
                        where 
                          intersperse1' _ []     = [] 
                          intersperse1' s (x:xs) = (s:x) ++ intersperse1' s xs

-- Tail recursive attempt, will lazy evaluation help?
-- Turned out it does not help, this is much slower, 
-- and it seems that making the accumulator strict 
-- would only make it worse.
interspersetr :: a -> [[a]] -> [a]
interspersetr _    []  = []
interspersetr _ (x:[]) = x 
interspersetr s (x:xs) = helper [s] x xs
                         where
                           helper s h (t:[]) = h++s++t
                           helper s h (t:ts) = helper s (h++s++t) ts

-- Tail-recursive by Joel Neely 
-- Still a bit slower, but not only because it uses reverse
intersp v wss = inters v (reverse wss) []
                where inters _ [] ws = ws
                      inters v (ws:wss) [] = inters v wss ws
                      inters v (ws:wss) xs = inters v wss (ws ++ v:xs)

-- Using future material, by Denis:
intersperseDenis x l = tail (foldl joiner [] l)
                       where joiner y z = y ++ (x : z)

-- By Helge Milde 
intersperseHelge sep = foldl1 $ (++) . (++ sep)

-- Correction by Nelson (fails on empty list)
intersperseHelgeNelson sep = foldl1 $ (++) . (++ [sep])

main = do
        print $ test 10000 10000
         where
           test m n = sum $ intersp 0 [[1..n] | i <- [1..m]]
