{--
 4. Turn a list into a palindrome, i.e. it should read the same 
    both backwards and forwards. For example, given the list 
    [1,2,3], your function should return [1,2,3,3,2,1].
--}

-- Solution that I thought is based on the material of the first three chapters
p :: [a] -> [a]
p [] = []
p list = q [] list
  where q t (x:xs)  -- t accumulates tail of list
          | null xs   = x:x:t  
            -- duplicate last element in front of t 
          | otherwise = x : (q (x:t) xs) 
            -- prepend t with x, repeat for xs, and put x back in front                          

-- Then I read that the (++) operator was in fact introduced
-- in Chapter 1 alongside (:)
p1 :: [a] -> [a]
p1 [] = []
p1 (x:xs) = x:p1 xs ++ [x]
-- However, I have doubts about performance of p1 on long lists
-- specifically about it appending single-element lists to tail

-- Obvious after Chapter 4, but again what about performance?
p2 :: [a] -> [a]
p2 x = x ++ reverse x

-- Notable solutions from comments at http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html

-- Looks clever but works incorrectly (yields [3,2,1,1,2,3]) by Beatrix
palindrome xs = myReverser xs xs
  where myReverser         [] result = result
        myReverser (first:xs) result = myReverser xs (first : result)

-- Claimed to be as fast as "x ++ reverse x" by Andrew Pritchard
palindromize3 l = palindromize3' l []
palindromize3' [] fs = fs
palindromize3' (x:xs) fs = x:palindromize3' xs (x:fs)
