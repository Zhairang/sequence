module Pretty
  (prettify,
  stringNumbering)
  where

import Command
import System.FilePath (splitFileName)
import Data.List(intercalate)
import Control.Applicative((<*>))
import Content(getFilesNamesWithStates)
import FileState (Dictionary)

prettify :: Value -> Dictionary ->String
prettify (Files files) dic=
  let output = getFilesNamesWithStates dic "\n\t" files
    in
      intercalate "\n\n" output
prettify (Grps groups) dic=
  let groupNames = ["Group" ++ show i ++ ":" | i <- [1..]]
      fileNames = map (getFilesNamesWithStates dic "\n\t\t") groups
      fileNames' = [([indent] <*>)] <*> fileNames
      combine = zipWith (:) groupNames fileNames'
      addDivider = intercalate [divider] combine
    in
      intercalate "\n" addDivider
        where
          indent = ('\t' :)

divider = replicate 50 '-'

stringNumbering ss =
  intercalate "\n" (zipWith (\n s -> show n ++ "." ++ s) [1..] ss) ++ "\n"
