{-
 1. Write your own “safe” definitions of the standard partial
    list functions, but make sure that yours never fail. 
    As a hint, you might want to consider using the following types:
-}

safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

-- It is unclear whether it is enough to wrap the respective unsafe
-- standard functions using if-then-else:
safeHead xs = if null xs
              then Nothing
              else Just (head xs)

-- ... or case-of:
safeHead' xs = case xs of
               []        -> Nothing
               otherwise -> Just (head xs)
              
-- ... or pattern matching:
safeTail []     = Nothing
safeTail (_:xs) = Just (tail xs)

-- or implement the entire logic of the function in question:
safeLast []     = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

-- which is not so trivial for safeInit:
safeInit []     = Nothing
safeInit xs     = Just (init' xs)
                  where init' (x:[]) = []
                        init' (x:xs) = x : (init' xs)

-- Notable solutions from online book comments:

-- Generic safe wrapper by 'path'. Requires knowledge of partial 
-- function application, introduced slightly later in the chapter.
safeListFunc func [] = Nothing
safeListFunc func xs = Just (func xs)

safeHeadpath = safeListFunc head
safeTailpath = safeListFunc tail
safeLastpath = safeListFunc last
safeInitpath = safeListFunc init

-- Some comment authors used fromMaybe, but that function must be imported
-- from Data.Maybe. Then Nikolay Artamonov wrote:
-- Btw, no one mentioned function 'maybe' which imports by module 
-- Prelude, so we haven'n import module Data.Maybe to use function
-- fromMaybe. With 'maybe' we can implement most "difficult" safeInit
-- as there:
safeInitNA :: [a] -> Maybe [a]
safeInitNA (x:[]) = Just []
safeInitNA (x:xs) = Just (x : maybe [] id (safeInit xs))
safeInitNA []     = Nothing
-- I'd say this is not very readable though

-- There were also crazy solutions using take, drop, and reverse.
-- Then people started exercising their knowledge of folds, lambda, etc.,
-- then R\"{}ord recalled that Chapter 4 began with introduction
-- of the do clause:
safeInitR [] = Nothing
safeInitR [_] = Just []
safeInitR (x : xs) = do xs' <- safeInit xs
                        Just $ x : xs'

-- an it all culminated with dewinant's solution:

-- Using `fmap`, as Maybe is a Functor:
safeInitd :: [a] -> Maybe [a]
safeInitd [] = Nothing
safeInitd (_:[]) = Just []
safeInitd (a:as) = fmap (a:) (safeInit as)
-- Needless to say, Functors and fmap are introduced only in Chapter 10.
