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
   
   isPal1 only returns True for palindromes of even length, and [].
   The spec in the book is not clear enough.
--}

isPal1 :: Eq a => [a] -> Bool
isPal1 list = scan list [] list
                where 
                  scan   tail revHead         [] = tail == revHead
                  scan    _         _     (_:[]) = False
                  scan (x:xs) revHead (p0:p1:ps) = scan xs (x:revHead) ps
                  

