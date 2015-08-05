module Pretty
  (prettify,
  stringNumbering)
  where

import Command
import System.FilePath (splitFileName)
import Data.List(intercalate)
import Control.Applicative((<*>))
import Content(getFilesNames)

prettify :: Value -> String
prettify (Files files) =
  let output = getFilesNames files
    in
      intercalate "\n" output
prettify (Grps groups) =
  let groupNames = ["Group" ++ show i ++ ":" | i <- [1..]]
      fileNames = map getFilesNames groups
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
