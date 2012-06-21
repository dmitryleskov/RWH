{--
 5. Write a function that determines whether its input list is a palindrome.
--}

-- Baseline implementation.
-- The function 'reverse' is introduced only in the next chapter though.
isPal0 :: Eq a => [a] -> Bool
isPal0 list = list == (reverse list)                  

-- First attempt, too big and compares the entire reversed list
isPal :: Eq a => [a] -> Bool
isPal [] = True
isPal (x:[]) = False
isPal (x:xs) = scan (x:xs) xs [x]
                where 
                 scan src []     rev  = compare src rev
                 scan src (x:xs) rev  = scan src xs (x:rev)
                 compare [] [] = True
                 compare [x] [y] = x == y
                 compare (x:xs) (y:ys) = x == y && compare xs ys

{--
   On each recursive call, the scan function drops *two* elements 
   from the original list passed as third parameter,  
   so when that "probe" "reaches" the end of the list,
   the first parameter contains the second half of the list, 
   and the second parameter contains the first half reversed, 
   hence they can be directly compared.
   
   Note: The exercise does not state whether palindromes 
   of odd length are permitted.

   isPal1 only returns True for palindromes of even length, and [].
   isPal2 handles both "even" and "odd" palindromes.
   
   As expected, it is faster than the isPal0 naive implementation
   when compiled, and uses less memory.
--}

isPal1 :: Eq a => [a] -> Bool
isPal1 list = scan list [] list
                where 
                  scan   tail revHead         [] = tail == revHead
                  scan      _       _     (_:[]) = False
                  scan (x:xs) revHead (p0:p1:ps) = scan xs (x:revHead) ps
                  
isPal2 list = scan list [] list
                where 
                  scan   tail revHead         [] = tail == revHead
                  scan (x:xs) revHead     (_:[]) = xs   == revHead
                  scan (x:xs) revHead (p0:p1:ps) = scan xs (x:revHead) ps
                  

main = do
--    args <- getArgs
    print $ show $ isPal2 ([1..5000000]++[5000000,4999999..1])
     
    
        
