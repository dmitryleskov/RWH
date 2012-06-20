{--
 4. Turn a list into a palindrome, i.e. it should read the same 
    both backwards and forwards. For example, given the list 
    [1,2,3], your function should return [1,2,3,3,2,1].
--}

-- Solution based on the material of the first three chapters
p :: [a] -> [a]
p [] = []
p list = q [] list
  where q t (x:xs)  -- t accumulates tail of list
          | null xs   = x:x:t  
            -- duplicate last element in front of t 
          | otherwise = x : (q (x:t) xs) 
            -- prepend t with x, repeat for xs, and put x back in front                          

-- Obvious after Chapter 4, but what about performance?
p1 :: [a] -> [a]
p1 x = x ++ reverse x
