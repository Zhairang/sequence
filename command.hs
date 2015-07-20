module Command
       (Exp,
        eval)
       where

import System.IO

import Control.Monad

import Sequence

import Glob

import Control.DeepSeq
import Control.Exception

import Data.List(union,groupBy,sortBy,foldl1')

import Text.Regex.Posix((=~))

data Exp =
  File String  -- file pattern
  |Com Exp --complement of sequence
  |Rev Exp --reverse of sequence
  |ComRev Exp
  |RevCom Exp
  |Union Exp Exp --union of the file
  |GrpBy String Exp
  |Tag Exp
  |Add Exp Exp --add files to groups
  |Out Exp
  deriving(Read,Show)

data Value = Files [String]
           |Grps [[String]]
              deriving(Show)

eval :: Exp -> IO Value
eval (File patt) = do
  files <- namesMatching patt
  return (Files files)
eval (Com exp) = changeDna seqCom exp
eval (Rev exp) = changeDna seqRev exp
eval (ComRev exp) = changeDna comRev exp
eval (RevCom exp) = eval (ComRev exp)
eval (Union exp1 exp2) = do
  Files f1 <- (eval exp1)
  Files f2 <- (eval exp2)
  return (Files (f1 `union` f2))
eval (GrpBy s exp) = do
  Files files <- (eval exp)
  return (Grps (grpBy ((=~ s)::(String -> String)) files))
eval (Tag exp) = do
  Files files <- (eval exp)
  tag files
  return (Files files)
eval (Add exp1 exp2) = do
  Files files <- (eval exp1)
  Grps groups <- (eval exp2)
  return (Grps (map (files++) groups))
eval (Out exp) =
  do
    Grps groups <- (eval exp)
    mapM outputGroup groups
    return (Grps groups)
  


changeDna :: (Dna -> Dna) -> Exp -> IO Value
changeDna f exp = do
  Files files <- (eval exp)
  mapM (fileMap (toStr . f . fromStr)) files
  return (Files files)




fileMap :: (String -> String) -> String -> IO () --map a string transforming function to a file
fileMap f file = do
  input <- readFile file
  evaluate (force input)
  writeFile file (f input)

grpBy :: (Ord a) => (b -> a) -> [b] -> [[b]]
grpBy f lis =
  groupBy fSame (sortBy fComp lis)
  where fComp a b = compare (f a) (f b)
        fSame a b = (f a) == (f b)

tag :: [String] -> IO [()]
tag files =
  mapM addTag files
  where addTag file = do
          input <- readFile file
          evaluate (force input)
          if head(input) == '>'
            then return ()
            else
            writeFile file (('>':file ++ "\n") ++ input)

getAllContents :: [String] -> IO String
getAllContents files = do
  tag files
  contents <- mapM readFile files
  return (foldl1' merge contents)
    where merge x y =
            x ++ "\n" ++ y

getGroupName :: [String] -> String
getGroupName files =
  let (_,ftype) = break (== '.') (head files)
      names = map (fst . break (=='.')) files in
   foldr (++) ftype names
   
outputGroup files = do
  contents <- getAllContents files
  writeFile (getGroupName files) contents
