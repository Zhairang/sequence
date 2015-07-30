module Content where

import Data.List(intercalate)

--import Sequence

import Control.DeepSeq
import Control.Exception
import Control.Monad

import System.Directory (doesFileExist)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

temp = "temporary"

getInitialContent :: FilePath -> IO String
getInitialContent file = do
  let tempFile = temp </> snd (splitFileName file)
  exist <- doesFileExist tempFile
  if exist
    then
      readFile' tempFile
    else do
      content <- readFile' file
      writeFile tempFile content
      return content

tag :: [String] -> IO [()]
tag =
  mapM addTag
  where addTag file = do
          input <- readFile' file
          unless (head input == '>') $
            writeFile file (('>':file ++ "\n") ++ input)

fileMapFromTo :: (String -> String) -> FilePath -> FilePath -> IO ()
fileMapFromTo f from to = do
  input <- readFile' from
  writeFile to (f input)

fileMap :: (String -> String) -> String -> IO () --map a string transforming function to a file
fileMap f file = fileMapFromTo f file file

initialMap :: (String -> String) -> FilePath -> IO () --map a function to the file's initial content
initialMap f file = do
  input <- getInitialContent file
  writeFile file (f input)

getAllContents :: [String] -> IO String
getAllContents files = do
  tag files
  contents <- mapM readFile' files
  return (intercalate "\n" contents)

getGroupName :: [String] -> String
getGroupName files =
  let (_,ftype) = break (== '.') (head files)
      names = map (takeWhile (not . (== '.'))) files in
   foldr (++) ftype names

outputGroup files = do
  contents <- getAllContents files
  let name = getGroupName files
  writeFile name contents
  return name

readFile' file = do
  input <- readFile file
  evaluate (force input)
  return input
