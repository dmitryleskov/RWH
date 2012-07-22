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
                                                  
myFunction = id --transpose
-}

transpose :: String -> String
transpose text = unlines (scan (lines text) [])

scan lines rows = if noMore lines
                  then rows
                  else scan tails (rows ++ [row])
                  where (row, tails) = getRow lines
                        noMore [] = True
                        noMore (l:ls) = if null l 
                                        then noMore ls
                                        else False

getRow :: [String] -> (String, [String])
getRow []           = ("", [])
getRow (line:lines) = let (h,t) = decapitate line
                          (hs, ts) = getRow lines
                       in (h : hs, if null t && null ts
                                   then []
                                   else t : ts) 
  where 
        decapitate line = if null line 
                          then (' ', "") 
                          else (head line, tail line)
