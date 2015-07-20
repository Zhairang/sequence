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

data Exp =
  File String  -- file pattern
  |Com Exp --complement of sequence
  |Rev Exp --reverse of sequence
  |ComRev Exp
  deriving(Read,Show)

data Value = Files [String]
              deriving(Show)

eval :: Exp -> IO Value
eval (File patt) = do
  files <- namesMatching patt
  return (Files files)
eval (Com exp) = changeDna seqCom exp
eval (Rev exp) = changeDna seqRev exp
eval (ComRev exp) = changeDna comRev exp



  

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
     
