module GUISave where

import System.Directory
import System.FilePath

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

runGUI :: String -> IO ()
runGUI s = do
  initGUI
  builder <- initBuilder
  
  {------------------------------------------
          Main Window
  ------------------------------------------}
  main_window <- builderGetObject builder castToWindow "saveWin"

  pathEntry <- builderGetObject builder castToEntry "txtFile"

  btnSave <- builderGetObject builder castToButton "btnSave"
  onClicked btnSave $ do
    path <- entryGetText pathEntry
    saveFile path s
    destroyWindow main_window

  btnCancel <- builderGetObject builder castToButton "btnCancel"
  onClicked btnCancel $ destroyWindow main_window

  onDestroy main_window mainQuit 

  widgetShowAll main_window
  mainGUI -- run main GUI loop 
	
saveFile path s = do
  guiSave path s	

guiSave :: String -> String -> IO ()
guiSave file txt = do
  iPath <- getCurrentDirectory
  let fPath = iPath ++ [pathSeparator] ++ file
  writeFile file txt

destroyWindow :: WidgetClass self => self -> IO ()
destroyWindow win = do
	widgetDestroy win
	mainQuit

initBuilder :: IO Builder
initBuilder = do
  builder <- builderNew
  builderAddFromFile builder "saveBox.glade"
  return builder
