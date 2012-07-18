import Criterion.Main --(defaultMain)

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

-- Variation with 2-in-1 aux function

splitWith1 :: (a -> Bool) -> [a] -> [[a]]
splitWith1 _ [] = []
splitWith1 p xs = let (first,rest) = splitOne p True xs
                   in if null first
                      then []
                      else first : (splitWith p rest)
                  where  
                        -- The second parameter means 
                        -- "drop or stop when predicate returns false"
                        splitOne :: (a -> Bool) -> Bool -> [a] -> ([a],[a])
                        splitOne _ _    []     = ([],[])
                        splitOne p skip (x:xs) = 
                            if p x
                            then let (suffix,rest) = splitOne p False xs
                                  in (x:suffix,rest)
                            else if skip 
                                 then splitOne p True xs
                                 else ([],xs)

-- Variation with break and span
splitWith2 :: (a -> Bool) -> [a] -> [[a]]
splitWith2 _ [] = []
splitWith2 p xs = let (first,rest) = span p (snd (break p xs))
                   in if null first
                      then []
                      else first : (splitWith p rest)

-- Variation with span, dropWhile and function composition,
-- introduced later in the chapter.
splitWith3 :: (a -> Bool) -> [a] -> [[a]]
splitWith3 _ [] = []
splitWith3 p xs = let (first,rest) = span p (dropWhile (not . p) xs)
                   in if null first
                      then []
                      else first : (splitWith p rest)


{-----------------------------------------------------------------------

Given that we don't need the first element of the tuple
returned by break, it would be interesting to compare 
splitWith2 and splitWith3 on large inputs. Will that element
be computed at all?

In GHCi (:set +s), splitWith3 is considerably faster and uses 
less memory if p is False for init xs:

λ> splitWith2 even ([1,3..100000000]++[2])
[[2]]
(7.40 secs, 9202677944 bytes)
λ> splitWith3 even ([1,3..100000000]++[2])
[[2]]
(6.37 secs, 6800704644 bytes)

As expected, splitWith2 performs substantially worse when break
produces a lot of throwaway data, otherwise it is on par with splitWith3.
It is interesting to note that splitWith and splitWith1 are not equivalent.

-----------------------------------------------------------------------}

-- Without a helper function (something I could not do), by Michael Ashton
-- One little problem - the original version splitted by (not . pred),
-- so I fixed that.
splitWithMA _ [] = []
splitWithMA pred l =
   case break (not . pred) l of
        ([], []) -> []
        (a,  []) -> [a]
        ([], b)  -> splitWithMA pred (tail b)
        (a,  b)  -> a : (splitWithMA pred (tail b))

-- Even shorter one by Neo@NHNG (http://nhng.de/)
splitWithNeo func xs = case span func xs of
    ([], []) -> []
    ([], _:ys) -> splitWithNeo func ys
    (x, ys) -> x : splitWithNeo func ys

-- Other variations are either less effective (calculate length, apply 
-- dropWhile and takeWhile to the same list, etc.) or too advanced
-- (use filter, lambdas, etc.)

main = defaultMain [
    bench "splitWith allEven"    $ nf (splitWith even) allEven
  , bench "splitWith1 allEven"   $ nf (splitWith1 even) allEven
  , bench "splitWith2 allEven"   $ nf (splitWith2 even) allEven
  , bench "splitWith3 allEven"   $ nf (splitWith3 even) allEven
  , bench "splitWith allOdd"     $ nf (splitWith even) allOdd
  , bench "splitWith1 allOdd"    $ nf (splitWith1 even) allOdd
  , bench "splitWith2 allOdd"    $ nf (splitWith2 even) allOdd
  , bench "splitWith3 allOdd"    $ nf (splitWith3 even) allOdd
  , bench "splitWith evenFirst"  $ nf (splitWith even) evenFirst
  , bench "splitWith1 evenFirst" $ nf (splitWith1 even) evenFirst
  , bench "splitWith2 evenFirst" $ nf (splitWith2 even) evenFirst
  , bench "splitWith3 evenFirst" $ nf (splitWith3 even) evenFirst
  , bench "splitWith oddFirst"   $ nf (splitWith even) oddFirst
  , bench "splitWith1 oddFirst"  $ nf (splitWith1 even) oddFirst
  , bench "splitWith2 oddFirst"  $ nf (splitWith2 even) oddFirst
  , bench "splitWith3 oddFirst"  $ nf (splitWith3 even) oddFirst
  , bench "splitWith normal"     $ nf (splitWith even) normal
  , bench "splitWith1 normal"    $ nf (splitWith1 even) normal
  , bench "splitWith2 normal"    $ nf (splitWith2 even) normal
  , bench "splitWith3 normal"    $ nf (splitWith3 even) normal
  ]
  where problemSize = 10000000
        allEven   = [2::Int,4,problemSize * 2]
        allOdd    = [1::Int,3,problemSize * 2]
        evenFirst = (take (problemSize `div` 2) allEven) ++ 
                    (take (problemSize `div` 2) allOdd)
        oddFirst  = (take (problemSize `div` 2) allOdd) ++ 
                    (take (problemSize `div` 2) allEven)
        normal    = [1::Int,problemSize]
