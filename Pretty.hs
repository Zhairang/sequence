module Pretty
  (prettify)
  where

import Command
import System.FilePath (splitFileName)
import Data.List(intercalate)

prettify :: Value -> String
prettify (Files files) =
  let output = map (snd . splitFileName) files
    in
      intercalate "\n" output
