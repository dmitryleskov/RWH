{-
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)
    
main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
                                                  
myFunction = transpose
-}

-- The transpose operation cannot be defined for arbitrary texts 
-- so that transpose (transpose text) == text

-- The narrowest restriction is that the text must be "rectangular",
-- i.e. all lines have the same length and the last line ends with
-- the newline character.

-- Using no "material from the future":
transposeRect :: String -> String
transposeRect text = unlines (tr (lines text))
  where
    tr []          = []
    tr ((y:ys):[]) = [y] : tr [ys]
    tr (x:xs)      = zipWith (:) x (tr xs)

-- Using map:
transposeRectMap :: String -> String
transposeRectMap text = unlines (tr (lines text))
  where
    tr []     = []
    tr (x:[]) = map (:[]) x
    tr (x:xs) = zipWith (:) x (tr xs)

-- Using repeat:
transposeRectRepeat :: String -> String
transposeRectRepeat text = unlines (tr (lines text))
  where
    tr []     = []
    tr (x:[]) = zipWith (:) x (repeat [])
    tr (x:xs) = zipWith (:) x (tr xs)

-- The transpose openarion can also be defined in a completely
-- reversible manner for 'convex' files, in which no line is longer
-- than the preceding one and no trailing empty lines are present.

transposeConvex :: String -> String
transposeConvex text = unlines (tr (lines text))
  where
    tr []     = []
    tr (x:xs) = zipWith (:) x (tr xs ++ repeat [])

-- Finally, the least restrictive version can handle files
-- in which lines have no trailing blanks and the last line 
-- is not empty and ends with a newline character.

transpose :: String -> String
transpose text = unlines (tr (lines text))
  where 
    tr [] = []
    tr (x:xs) = zipNPad x (tr xs)
      where
        -- zipNPad line lines is equivalent to zipWith (:) line lines
        -- with either line padded with trailing blanks, or lines padded
        -- with trailing empty Strings so that length line == length lines.
        zipNPad :: String -> [String] -> [String]
        zipNPad ""     []     = []
        zipNPad (c:cs) []     = trimCat c   "" : zipNPad cs []
        zipNPad ""     (l:ls) = trimCat ' ' l  : zipNPad "" ls
        zipNPad (c:cs) (l:ls) = trimCat c   l  : zipNPad cs ls
          where
            -- Ensures that there are no trailing blanks
            trimCat c l = if c == ' ' && null l then "" else c:l
