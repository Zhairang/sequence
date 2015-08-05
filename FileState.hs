module FileState where

import GuiContent

import Graphics.UI.Gtk

import qualified Data.Map.Strict as Map

import Control.Monad.Trans.State

import Data.Maybe (fromMaybe)

oriStr ="__"
comStr = "C_"
revStr = "R_"
revComStr = "RC"

type Dictionary = Map.Map FilePath String

loadFileState :: TextBuffer -> IO Dictionary
loadFileState buffer = do
  content <- getBufferContent buffer
  let dic = if null content
                then Map.fromList []
                else read content
  return dic

getFileState :: Dictionary -> FilePath -> String
getFileState dic file =
  fromMaybe oriStr (Map.lookup file dic)

changeFileState :: String -> FilePath -> Dictionary -> ((), Dictionary)
changeFileState new file dic =
  ((),Map.insert file new dic)

changeFilesStates :: String -> [FilePath] -> Dictionary ->Dictionary
changeFilesStates new files dic =
  let change = [state $ changeFileState new file | file <- files] in
    execState (sequence_ change) dic

writeFileState :: Dictionary -> TextBuffer -> IO ()
writeFileState dic buff =
  textBufferSetText buff (show dic)
