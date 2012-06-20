{--
 1. Write a function that computes the number of elements in a list. 
    To test it, ensure that it gives the same answers as the standard
    length function.
    
 2. Add a type signature for your function to your source file. 
    To test it, load the source file into ghci again.
--}

-- This version yields the same result as the built-in length,
-- but is way slower. Maybe because the built-in one is precompiled?
-- At the same time, it does not produce stack overflows in ghci, 
-- which, according to the comments on book.realworldhaskell.org,
-- was the case with older versions.

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1+myLength xs
