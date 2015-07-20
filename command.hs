module Command
       (Exp,
        Value)
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
eval (Com exp) = do
  Files files <- (eval exp)
  -- modified <- forM files (makeChange (seqCom :: Dna -> Dna))
  dnaChange seqCom files
  return (Files files)

dnaChange :: (Dna -> Dna) -> [String] -> IO [()]
dnaChange f =
  let foo = fileMap (toStr . f . fromStr) in
   mapM foo 




fileMap :: (String -> String) -> String -> IO () --map a string transforming function to a file
fileMap f file = do
  input <- readFile file
  evaluate (force input)
  writeFile file (f input)
     
