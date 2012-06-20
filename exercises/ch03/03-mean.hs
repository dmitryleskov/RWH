{--
 3. Write a function that computes the mean of a list, i.e. the sum
    of all elements in the list divided by its length. (You may need
    to use the fromIntegral function to convert the length of the list
    from an integer into a floating point number.)
--}

-- My variations

mean :: [Double] -> Double
stat [] = (0,0)
stat [x] = (x,1)
stat (x:xs) = (x+sum, 1+len)
              where (sum, len) = stat xs

mean xs 
      | null xs   = 0
      | otherwise = sum / len
         where (sum, len) = stat xs
  
mean1 :: [Double] -> Double
mean1 xs 
       | null xs = 0
       | otherwise = let (sum, len) = s xs
                         s [] = (0,0)
                         s (x:xs) = (x+sum, 1+len)
                                    where (sum, len) = s xs
                     in sum / len

mean2 :: [Double] -> Double
mean2 xs = sum / len
           where (sum, len) = s xs
                 s xs       = 
                   case xs of
                     []     -> (0,0)
                     (x:xs) -> let (sum, len) = s xs
                               in (x+sum, 1+len)


-- Noticeable solutions from comments at http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html

-- The clever one by Rob Grainger (yields NaN on empty list)
meanRG :: [Double] -> Double
meanRG l = snd(inner l) / fst(inner l)
           where inner [] = (0,0)
                 inner (x:xs) = (fst(inner xs) + 1, snd(inner xs) + x)

{--
 -- This is what I think is the best a reader of _only_ the first three
 -- chapters could come up with. There are many variations;
 -- chronologically the first one was given by "horia314"
 -- Somehow, it did not come to my mind that accumulators can be passed
 -- as parameters. :(
 --}
meanh314 list =
    let temp accm size (head:tail) = temp (accm + head) (size + 1) tail
        temp accm size [] = (fromIntegral accm) / (fromIntegral size)
    in temp 0 0 list
