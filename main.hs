import Command

import Sequence

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import System.FilePath ((</>))

import Pretty

data GUI = GUI {
  mainWin :: Window,
  dir :: FileChooserButton,
  filePatt :: TextBuffer,
  selectFile :: TextBuffer
}

windowX = 800
windowY = 400


main = do
  initGUI
  builder <- builderNew
  gui <- loadFromFile "window.glade"
  windowSetDefaultSize (mainWin gui) windowX windowY
  addEvent gui
  widgetShowAll (mainWin gui)
  mainGUI


loadFromFile :: FilePath -> IO GUI
loadFromFile file =
  do
    builder <- builderNew
    builderAddFromFile builder file
    let getBuffer name = builderGetObject builder castToTextView name >>= textViewGetBuffer
    main <- builderGetObject builder castToWindow "window"
    directory <- builderGetObject builder castToFileChooserButton "directory"
    patt <- getBuffer "filepattern"
    select <-getBuffer "selectedfile"
    return (GUI main directory patt select)


addEvent gui = do
  onDestroy (mainWin gui) mainQuit
  onBufferChanged (filePatt gui) (pattChanged gui)

pattChanged gui = do
  patt <- getBufferContent (filePatt gui)
  let pattern = '*' : (patt ++ "*")
  directory <- currentDirectory gui
  files <- eval (File (directory </> pattern))
  textBufferSetText (selectFile gui) (prettify files)

getBufferContent :: TextBuffer -> IO String
getBufferContent buffer = do
  st <- textBufferGetStartIter buffer
  en <- textBufferGetEndIter buffer
  textBufferGetText buffer st en True

currentDirectory gui =
  do
    Just path <- fileChooserGetFilename (dir gui)
    return path
