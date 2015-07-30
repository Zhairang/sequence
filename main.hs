import Command

import Sequence

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Builder

import System.FilePath ((</>))

import Control.Applicative ((<*>))

import Pretty

import Control.Monad (guard)

data GUI = GUI {
  mainWin :: Window,
  dir :: FileChooserButton,
  filePatt :: TextBuffer,
  selectFile :: TextBuffer,
  revBut :: CheckButton,
  comBut :: CheckButton,
  revComBut :: CheckButton,
  suff :: TextBuffer
}

windowX = 800
windowY = 400

defaultSuffix = "seq"

main = do
  initGUI
  builder <- builderNew
  gui <- loadFromFile "window.glade"
  addEvent gui
  widgetShowAll (mainWin gui)
  mainGUI


loadFromFile :: FilePath -> IO GUI
loadFromFile file =
  do
    builder <- builderNew
    builderAddFromFile builder file
    --load window
    main <- builderGetObject builder castToWindow "window"
    --windowSetDefaultSize main windowX windowY
    directory <- builderGetObject builder castToFileChooserButton "directory"
    --load text buffer
    let getBuffer name = builderGetObject builder castToTextView name >>= textViewGetBuffer
    patt <- getBuffer "filepattern"
    select <-getBuffer "selectedfile"
    suffix <- getBuffer "suffix"
    textBufferSetText suffix defaultSuffix
    --load checkbuttons
    let getCheckButton = builderGetObject builder castToCheckButton
    [rev , com , revCom] <- mapM getCheckButton ["reverse" , "complement" , "complementreverse"]
    return (GUI main directory patt select rev com revCom suffix)


addEvent gui = do
  onDestroy (mainWin gui) mainQuit
  onBufferChanged (filePatt gui) (pattChanged gui)
  onBufferChanged (suff gui) (pattChanged gui)
  addCheckEvent gui (revBut gui) Rev'
  addCheckEvent gui (comBut gui) Com'
  addCheckEvent gui (revComBut gui) ComRev'

pattChanged gui = do
  files <- getSelectedFile gui
  textBufferSetText (selectFile gui) (prettify files)

addCheckEvent :: GUI -> CheckButton -> (Exp -> Exp) -> IO ()
addCheckEvent gui button foo = do
  onToggled button $ do
    patt <- getFilePattern gui
    if (patt /= "")
      then do
        active <- toggleButtonGetActive button
        if active
          then do
            eval (foo (File patt))
            return ()
          else do
            eval (Id (File patt))
            return ()
      else return ()

  return ()

getBufferContent :: TextBuffer -> IO String
getBufferContent buffer = do
  st <- textBufferGetStartIter buffer
  en <- textBufferGetEndIter buffer
  textBufferGetText buffer st en True

currentDirectory gui =
  do
    Just path <- fileChooserGetFilename (dir gui)
    return path

getFilePattern gui = do
  patt <- getBufferContent (filePatt gui)
  if (isPass patt)
    then do
      suffix <- getBufferContent (suff gui)
      let pattern = '*' : (patt ++ ('*' : suffix))
      directory <- currentDirectory gui
      return (directory </> pattern)
    else return ""
    where
      isPass :: String -> Bool
      isPass s = (s /= "") && (isBalance s)
      isBalance s = countBy (== '[') s == countBy (==']') s
      --countBy :: (a -> Bool) -> [a] -> Int
      countBy f = length . (filter f)

getSelectedFile gui = do
  patt <- getFilePattern gui
  eval (File patt)
