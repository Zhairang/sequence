import Command

import Sequence

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Builder

import System.FilePath ((</>))

import Control.Applicative ((<*>))

import Pretty

import Control.Monad (guard,when ,zipWithM_,liftM)

import Clip

import GuiContent

import FileState

import Groupbuffer

data GUI = GUI {
  mainWin :: Window,
  directory :: FileChooserButton,
  filePattern :: TextBuffer,
  selectFile :: TextBuffer,
  fileButtons :: [CheckButton],
  suffix :: TextBuffer,
  groupNumber :: SpinButton,
  regularExpressin :: Entry,
  group ::TextBuffer,
  groupBuffer :: TextBuffer,
  addGroup :: Button,
  output :: Button,
  clipButton :: Button,
  clip :: Clipwindow,
  fileState :: TextBuffer,
  about :: Button,
  aboutDialog :: AboutDialog,
  groupContentBuffer :: GroupBuffer,
  outputToBuffer :: Button
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
    windowSetResizable main False
    directory <- builderGetObject builder castToFileChooserButton "directory"
    --load text buffer
    let getBuffer name = builderGetObject builder castToTextView name >>= textViewGetBuffer
    patt <- getBuffer "filepattern"
    select <-getBuffer "selectedfile"
    suffix <- getBuffer "suffix"
    textBufferSetText suffix defaultSuffix
    group <- getBuffer "group"
    --load checkbuttons
    let getCheckButton = builderGetObject builder castToCheckButton
    buttons <- mapM getCheckButton ["reverse" , "complement" , "complementreverse","origin"]
    --load spinbuttons
    number <- builderGetObject builder castToSpinButton "groupnumber"
    --load group pattern
    regExp <- builderGetObject builder castToEntry "regularexpression"
    entrySetText regExp "\\(.*\\)"
    --create text buffer to store group information
    grpBuff <- textBufferNew Nothing
    --add to group button
    let getButton = builderGetObject builder castToButton
    addGrp <- getButton "addtogroup"
    --add output button
    output <- getButton "output"
    --load clipbord window
    clipButton <- getButton "clipbutton"
    clip <- clipLoad builder
    clipInitialize clip
    --textbuffer to store file state
    fileState <- textBufferNew Nothing
    --about dialog
    about <- getButton "about"
    aboutDialog <- builderGetObject builder castToAboutDialog "aboutdialog"
    --load group buffer
    groupContentBuffer <- groupBufferLoad builder
    outputToBuffer <- getButton "outputtobuffer"
    return (GUI main directory patt select buttons suffix number regExp group grpBuff addGrp
      output clipButton clip fileState about aboutDialog groupContentBuffer outputToBuffer)


addEvent gui = do
  onDestroy (mainWin gui) mainQuit
  onBufferChanged (filePattern gui) (pattChanged gui)
  onBufferChanged (suffix gui) (pattChanged gui)

  let funList = zipWith (addCheckEvent gui) (fileButtons gui) [Rev' , Com' , ComRev' , Id]
  zipWithM_ ($) funList [revStr , comStr , revComStr , oriStr]

  checkButtonLink (fileButtons gui) -- at most one checkbutton get active
  onEntryActivate (groupNumber gui) (grpByNum gui)
  onEntryActivate (regularExpressin gui) (grpByReg gui)
  onClicked (addGroup gui) (addToGrp gui)
  onClicked (output gui) (writeGrp gui)
  --clip window start
  onClicked (clipButton gui) (clipEvent gui)
  --about dialog
  onClicked (about gui) (runAboutDialog gui)
  onClicked (outputToBuffer gui) (groupBufferStart gui)
pattChanged gui = do
  selectBufferRefresh gui
  mapToFileButtons gui (`toggleButtonSetActive` False)

addCheckEvent :: GUI -> CheckButton -> (Exp -> Exp) -> String -> IO ()
addCheckEvent gui button foo stStr = do
  onToggled button $ do
    patt <- getFilePattern gui
    when (patt /= "") $
      do
        --mapToFileButtons gui (`toggleButtonSetInconsistent` False)
        active <- toggleButtonGetActive button
        when active $
          do
            Files files <- eval (foo (File patt))
            let buffer = fileState gui
            dic <- loadFileState buffer
            writeFileState (changeFilesStates stStr files dic) buffer
            --refresh the selection file buffer
            selectBufferRefresh gui
            return ()
  return ()

checkButtonLink buttons = do
  let tupples = [(a , b) | a <- buttons , b <- buttons , a /= b]
  mapM_ link tupples
    where
      link (a , b) =
        onToggled a $ do
          active <- toggleButtonGetActive a
          when active $
            toggleButtonSetActive b False

grpByNum gui = do
  num <- spinButtonGetValueAsInt (groupNumber gui)
  files <- getSelectedFile gui
  let grps = groupEvery num files
  writeGroup gui grps

grpByReg gui = do
  patt <- entryGetText (regularExpressin gui)
  files <- getSelectedFile gui
  writeGroup gui (groupByString patt files)

addToGrp gui = do
  groups <- getGroup gui
  files <- getSelectedFile gui
  let newGroup = addToGroup files groups
  writeGroup gui newGroup

writeGrp gui = do
  groups <- getGroup gui
  out' groups

writeGroup gui groups = do
  dic <- loadFileState (fileState gui)
  textBufferSetText (group gui) (prettify groups dic)
  textBufferSetText (groupBuffer gui) (show groups)

clipEvent gui = do
  Files files <- getSelectedFile gui
  clipStart (clip gui) files

runAboutDialog gui = do
  let dialog = aboutDialog gui
  dialogRun dialog
  widgetHide dialog

groupBufferStart :: GUI -> IO ()
groupBufferStart gui = do
  groups <- getGroup gui
  contents <- (out1' groups)
  runGroupBuffer (groupContentBuffer gui) contents

selectBufferRefresh gui = do
  files <- getSelectedFile gui
  dic <- loadFileState (fileState gui)
  textBufferSetText (selectFile gui) (prettify files dic)

currentDirectory gui =
  do
    Just path <- fileChooserGetFilename (directory gui)
    return path

getGroup :: GUI -> IO Value
getGroup gui =
  do
    grp <- getBufferContent (groupBuffer gui)
    if grp == ""
      then return (Grps [[]])
      else return (read grp)


getFilePattern gui = do
  patt <- getBufferContent (filePattern gui)
  if isPass patt
    then do
      suffix <- getBufferContent (suffix gui)
      let pattern = '*' : (patt ++ ('*' : suffix))
      directory <- currentDirectory gui
      return (directory </> pattern)
    else return ""
    where
      isPass :: String -> Bool
      isPass s = s /= "" && isBalance s
      isBalance s = countBy (== '[') s == countBy (==']') s
      --countBy :: (a -> Bool) -> [a] -> Int
      countBy f = length . filter f

getSelectedFile gui = do
  patt <- getFilePattern gui
  eval (File patt)

mapToFileButtons gui foo =
  mapM_ foo (fileButtons gui)
