module  Clip where

import System.Clipboard
import Graphics.UI.Gtk
import Content
import Control.Monad(when,liftM)
import GuiContent
import Pretty

data Clipwindow = Clipwindow {
  clipWindow :: Window,
  fileView :: TextView,
  fileList :: TextBuffer,
  currentFile :: SpinButton,
  write :: Button,
  exit :: Button,
  fileBuffer :: TextBuffer
}

windowX = 175
windowY = 175

clipLoad :: Builder -> IO Clipwindow
clipLoad builder = do
  clipWindow <- builderGetObject builder castToWindow "clipboard"
  fileView <- builderGetObject builder castToTextView "filelist"
  fileList <- textViewGetBuffer fileView
  currentFile <- builderGetObject builder castToSpinButton "currentfile"
  write <- builderGetObject builder castToButton "write"
  exit <- builderGetObject builder castToButton "exit"
  fileBuffer <- textBufferNew Nothing
  return (Clipwindow clipWindow fileView fileList currentFile write exit fileBuffer)

clipInitialize :: Clipwindow -> IO ()
clipInitialize clip = do
  let window = clipWindow clip
  onClicked (exit clip) (widgetHide window)
  onValueSpinned (currentFile clip) (spinEvent clip)
  onClicked (write clip) (writeToClipboard clip)
  textViewSetEditable (fileView clip) False
  widgetSetSizeRequest window windowX windowY
  windowSetResizable window False
  windowSetDeletable window False

spinEvent clip = do
  current <- spinButtonGetValueAsInt (currentFile clip)
  selectLine (fileList clip) current
  mark <- textBufferGetSelectionBound (fileList clip)
  textViewScrollToMark (fileView clip) mark 0.15 Nothing

writeToClipboard clip = do
  now <- spinButtonGetValueAsInt (currentFile clip)
  when (now /= 0) $ do
    file <- liftM (!!(now -1)) (getCurrentFiles clip)
    readFile' file >>= setClipboardString
    spinButtonSpin (currentFile clip) SpinStepForward 1.0

clipStart :: Clipwindow -> [FilePath] -> IO ()
clipStart clip files = do
  initialize clip files
  widgetShowAll (clipWindow clip)
  windowSetKeepAbove (clipWindow clip) True
  return ()

initialize clip files = do
  old <- getCurrentFiles clip
  when (old /= files) $ do
    textBufferSetText (fileBuffer clip) (show files)
    let new = stringNumbering (getFilesNames files)
        num = length files
    old <- getBufferContent (fileList clip)
    textBufferSetText (fileList clip) new
    let spin = currentFile clip
    spinButtonSetRange spin 1 (fromIntegral num)
    spinButtonSetValue spin 0.0

getCurrentFiles :: Clipwindow -> IO [String]
getCurrentFiles clip = do
  files <- getBufferContent (fileBuffer clip)
  if files == ""
    then return [""]
    else return (read files)
