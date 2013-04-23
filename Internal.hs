module Internal where

import System.Directory
import System.FilePath

import FOULParser
import TextCursorKeys

{- TODO: write the toProg function in FOUL.hs 
         then change putStrLn $ parseFOUL tc to
         putStrLn $ compile tc-}

-- initial function to compile 
-- checks that the function parses , then displays parsed code if it does
comp :: TextCursor -> IO ()
comp tc = let pf = parseFOUL tc in 
  case pf of
    "Error!" -> putStrLn "Foul code invalid! Exiting..."
    _ -> putStrLn $ pf

--compile :: TextCursor -> Prog
--compile tc = toProg nme (parseFOUL tc)
--  where
--    nme = do
--      putStrLn "What is your program called?"
--      n <- getLine
--      return n
      

--Function to parse the FOUL code
-- Takes text cursor, converts to string then parses.
parseFOUL :: TextCursor -> String
parseFOUL = in2out . tc2str

{- algorithm for tc2str:
1) deactivate inner cursor
2) append to one of the lists in the outer cursor
3) replace inner cursor with a Here
4) deactivate the whole thing
5) grab list and use unlines to piece together
-}

tc2str :: TextCursor -> String
tc2str (czz, cur, css) = unlines strs
  where
    (_, cs) = deactivate cur
    (_, strs) = deactivate (czz, Here, cs : css)



{-
inputs: path or file name, text cursor, flag
The flag is set to true when the user is saving to the current directory
and is only specifing the file path.
The flas is set to false when the user is saving to a specified path.
-}

saveFile :: String -> TextCursor -> Bool -> IO ()
saveFile file tc flag = let tc' = tc2str tc in
  if flag  
  then
    do
      iPath <- getCurrentDirectory
      let fPath = iPath ++ [pathSeparator] ++ file
      writeFile file tc'
  else
    writeFile file tc'