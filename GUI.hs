module GUI where

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import FOULParser
import qualified GUISave as Save

runGUI :: [FilePath] -> IO ()
runGUI xs = do
  initGUI
  builder <- initBuilder
  
  {------------------------------------------
          Main Window
  ------------------------------------------}
  main_window <- builderGetObject builder castToWindow "IDEWindow"

  cont <- builderGetObject builder castToTextView "codeView"  
  
  mComp <- builderGetObject builder castToMenuItem "compileFOUL"
  on mComp menuItemActivate $ do
    prog <- getText cont
    Save.saveFile "temp0.foul" prog
    --clearTV cont
    let compiledCode = "Compiled code: \n ------------------------------ \n" ++ in2out prog
    setNewTextBuffer cont compiledCode

  mNew <- builderGetObject builder castToMenuItem "newFile"
  on mNew menuItemActivate $ clearTV cont

  mSave <- builderGetObject builder castToMenuItem "saveFile"
  on mSave menuItemActivate $ loadSaveWin cont

  mQuit <- builderGetObject builder castToMenuItem "quitIDE"
  on mQuit menuItemActivate $ runCloseProcedure main_window  

  onDestroy main_window mainQuit

  widgetShowAll main_window
  mainGUI -- run main GUI loop 

initBuilder :: IO Builder
initBuilder = do
  builder <- builderNew
  builderAddFromFile builder "GUI.glade"
  return builder

runCloseProcedure :: WidgetClass self => self -> IO ()
runCloseProcedure window = do
	widgetDestroy window
	mainQuit

getText cont = do
	buff <- textViewGetBuffer cont
	txt <- get buff textBufferText
	return txt

clearTV cont = do
	buff <- textBufferNew Nothing
	textViewSetBuffer cont buff

setNewTextBuffer cont compiledCode = do
  buff <- textBufferNew Nothing
  textBufferSetText buff compiledCode
  textViewSetBuffer cont buff

loadSaveWin cont = Save.runGUI (unsafePerformIO $ getText cont)

