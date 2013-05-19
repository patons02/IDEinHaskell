module Internal where


import Data.Char
import Data.List
import System.Directory
import System.FilePath

import FOULParser
import TextCursorKeys
import qualified GUI as GUI

            


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
    
promptComp tc = do
  putStrLn "Would you like to compile a FOUL script now? (Y/n)"
  c <- getChar
  putStrLn " "
  case (toUpper c) of
    'Y' -> do
      comp tc
    _ -> return ()
  
promptSave tc = do
  putStrLn "Would you like to save the file? (Y/n)"
  c <- getChar
  putStrLn " "
  case (toUpper c) of
     'Y' -> do
       putStrLn "Would you like this file to be saved in the current directory."
       c' <- getChar
       case (toUpper c') of
         'Y' -> do
           putStrLn "Please enter the file name to save to:"
           fNme <- getLine
           saveFile fNme tc True
         'N' -> do
           putStrLn "Please enter the full path of the file to be saved:"
           pNme <- getLine     
           saveFile pNme tc False
         _ -> return ()
     _ -> return ()

isSupportedFileType :: [FilePath] -> Bool
isSupportedFileType [] = False
isSupportedFileType (fp:_)  = isInfixOf ".foul" fp 

runGUI xs = GUI.runGUI xs
