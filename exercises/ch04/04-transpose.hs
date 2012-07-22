
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
transpose text = unlines (scan (removeTrailingNewlines (lines text)) [])
  where
    removeTrailingNewlines lines = 
      case lines of
        []     -> []
        (l:[]) -> if null l then [] else [l]
        (l:ls) -> let ps = removeTrailingNewlines ls
                   in if null ps && null l
                      then []
                      else l:ps

    -- Builds a row from the heads of lines, appends it to rows and
    -- calls itself for tails until there are no more tails
    scan :: [String] -> [String] -> [String]
    scan []    rows = rows
    scan lines rows = let (row, tails) = getRow lines
                       in scan tails (rows ++ [row])

    -- Given a list of Strings, returns a tuple contaning a String
    -- formed by their heads and a list of their tails. 
    -- Any trailing empty tails are trimmed.
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
