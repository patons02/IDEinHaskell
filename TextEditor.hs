module TextEditor where

{-
Ommited pragmas as they're depreciated: 
{-# INCLUDE <mycurses.h> #-}
{-# OPTIONS_GHC -fffi #-}


Use flag -lncurses to compile

-}

import Control.Monad
import Data.Char
import Foreign
import Foreign.C (CInt(..))
import System.Exit
import System.IO
import System.Environment


import Internal
import ANSIEscapes
import Block
import Overlay
import TextCursorKeys

import FOULParser

data Window = Window
type WindowPtr = Ptr Window

{----------------------------------------------
            Foreign imports 
-----------------------------------------------}
foreign import ccall
  initscr :: IO () 

foreign import ccall safe "static endwin"
  endwin :: IO CInt

foreign import ccall safe "static refresh"
  refresh :: IO CInt

foreign import ccall "&LINES"
  linesPtr :: Ptr CInt

foreign import ccall "&COLS"
  colsPtr :: Ptr CInt

scrSize :: IO (Int, Int)
scrSize = do
    lines <- peek linesPtr
    cols <- peek colsPtr
    return (fromIntegral cols, fromIntegral lines)

copies :: Int -> a -> [a]
copies n a = take n (repeat a)

crlf :: IO ()
crlf = putStr "\r\n"

putLn :: String -> IO ()
putLn x = putStr x >> crlf

type ScreenState = (Point, Size)
  -- position in buffer of top left corner of screen, screen size

onScreen :: Point -> ScreenState -> ScreenState
onScreen (cx, cy) ((px, py), s@(sw, sh))
  = (( intoRange px cx sw, intoRange py cy sh), s)
  where
    intoRange i j x
      | i <= j && j <= i + x = i   -- in range, no change
      | otherwise = max 0 (j - div x 2)

getEscapeKey :: [(String, Key)] -> IO (Maybe Key)
getEscapeKey [] = return Nothing
getEscapeKey sks = case lookup "" sks of
  Just k -> return (Just k)
  _ -> do
    c <- getChar
    getEscapeKey [(cs, k) | (d : cs, k) <- sks, d == c]

keyReady :: IO (Maybe Key)
keyReady = do
  b <- hReady stdin
  if not b then return Nothing else do
    c <- getChar
    case c of
      '\n' -> return $ Just Return
      '\r' -> return $ Just Return
      '\b' -> return $ Just Backspace
      '\DEL' -> return $ Just Backspace
      _ | c >= ' ' -> return $ Just (CharKey c)
      '\ESC' -> do
        b <- hReady stdin
        if not b then return $ Just Quit else do
          c <- getChar
          case c of
            '[' -> getEscapeKey escapeKeys
            _ -> return $ Just Quit
--      '\c' = do     
--        b <- hReady stdin
--        if not b then return $ Nothing else do
--          c <- getChar
--          case c of
--            's' -> Just Quit
--            'l' -> undefined
      _ -> return $ Nothing

outer :: ScreenState -> TextCursor -> IO ()
outer ps tc = inner ps tc (whatAndWhere tc) LotsChanged
  where
  inner ps@(p, s) tc lc@(l, c@(cx, cy)) d = do
    refresh
    s' <- scrSize
    let ps'@((px, py), (sw, _)) = onScreen c (p, s')
    let d' = if ps /= ps' then LotsChanged else d
    case d' of
      LotsChanged -> do
        clearScreen
        resetCursor
        mapM_ putStr (layout (cropLay cropBox ps' l))
      LineChanged -> do
        resetCursor
        down (cy - py)
        mapM_ putStr (layout (cropLay cropBox ((px, cy), (sw, 1)) l))
      _ -> return ()
    if d' > NoChange then do
      resetCursor
      forward (cx - px)
      down (cy - py)
     else return ()
    mc <- keyReady
    case mc of
      Nothing -> inner ps' tc lc NoChange
      Just Quit ->  do        
        endwin
        promptSave tc
        promptComp tc
      Just k -> case handleKey k tc of
        Nothing -> inner ps' tc lc NoChange
        Just (d, tc') -> inner ps' tc' (whatAndWhere tc') d

{-
Loads a file into the IDE
inputs: path or file name
-}
loadFile ::[FilePath] -> IO ()
loadFile xs = do
  s <- case xs of
    [] -> return ""
    (x : _) -> case isSupportedFileType [x] of
                   True -> readFile x
                   False -> do 
                     unsupportedPromptLd
                     return " "
  let (l, ls) = case lines s of
        [] -> ("", [])
        (l : ls) -> (l, ls)
  initscr
  outer ((0, 0), (-1, -1)) (B0, (B0, Here, l), ls)

unsupportedPromptLd :: IO ()
unsupportedPromptLd = do
  putStrLn "You have tried to load an unsupported file type."
  putStrLn "Would you like to load another file? (Y/n)"
  c <- getChar
  putStrLn " "
  case (toUpper c) of
    'Y' -> do
      putStrLn "Please type in the full path of the file."
      line <- getLine
      loadFile [line]
    'N' -> do
      return ()

checkMode :: [FilePath] -> IO ()
checkMode xs = do
  putStrLn "Select your mode:"
  putStrLn "Press 1 for command line editor, and 2 for GUI Editor."
  c <- getChar
  putStrLn " "
  case c of
    '1' -> loadFile xs
    '2' -> runGUI xs
    otherwise -> do
      putStrLn "Please select 1 or 2."
      checkMode xs

--This is the main file called in Main.hs
textEditor :: IO ()
textEditor = do 
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  xs <- getArgs
  checkMode xs

