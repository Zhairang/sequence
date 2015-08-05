module GuiContent
    (
    getBufferContent,
    selectLine
    ) where
import Graphics.UI.Gtk

getBufferContent :: TextBuffer -> IO String
getBufferContent buffer = do
  st <- textBufferGetStartIter buffer
  en <- textBufferGetEndIter buffer
  textBufferGetText buffer st en True

selectLine :: TextBuffer -> Int -> IO ()
selectLine buffer n = do
  st <- textBufferGetIterAtLine buffer (n - 1)
  en <-textBufferGetIterAtLine buffer n
  textBufferSelectRange buffer st en
