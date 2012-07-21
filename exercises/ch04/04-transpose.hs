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

transpose :: String -> String
transpose text = unlines (scan (lines text) [])

scan lines rows = if noMore lines
                  then rows
                  else scan l (rows ++ [r])
                  where (r, l) = getRow lines
                        noMore [] = True
                        noMore (l:ls) = if null l 
                                        then noMore ls
                                        else False

getRow :: [String] -> (String, [String])
getRow []           = ("", [])
getRow (line:lines) = let (h,t) = decapitate line
                       in (h : hs, t : ts)
  where (hs, ts) = getRow lines
        decapitate line = if null line 
                          then (' ', "") 
                          else (head line, tail line)
