{--
 8. Using the binary tree type that we defined earlier in this chapter,
    write a function that will determine the height of the tree. 
    The height is the largest number of hops from the root to an Empty. 
    For example, the tree Empty has height zero; Node "x" Empty Empty 
    has height one; Node "x" Empty (Node "y" Empty Empty) has height two; 
    and so on.
--}

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- Obvious solution.
treeHeight :: Tree a -> Integer
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right) 
                                 -- max has not been intorduced yet, 
                                 -- but it is trivial
                                 where max :: (Ord a) => a -> a -> a
                                       max a b = if a > b then a else b

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)
