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
                                                  
-- replace "id" with the name of our function below
myFunction = firstWordsmap

firstWords :: String -> String
firstWords text = unlines (scan (lines text))
  where scan []     = []
        scan (l:ls) = getFirst (words l) : scan ls
        getFirst []    = ""
        getFirst (w:_) = w

firstWordsmap :: String -> String
firstWordsmap text = unlines (map first (map words (lines text)))
  where first ws = if null ws then "" else head ws


