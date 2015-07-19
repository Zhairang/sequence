module Sequence
       (
         Dna,
         Sequence
       )where

import Data.Char

import System.IO

import Control.Monad

class Sequence a where
  toStr :: a -> String  --sequence in string form
  
  seqCom :: a -> a  --the complement of a sequence
  
  seqRev :: a -> a --the reverse sequence
  
  fromStr :: String -> a
  
  -- fromFile :: String -> IO a
  -- fromFile file = do
  --   inh <- openFile file ReadMode
    
  revAndCom :: a -> a
  revAndCom = seqCom . seqRev

  makeChange :: (Sequence a) => (a -> a) -> String -> IO String --make change from a file and return the filename
  makeChange f file = do
    input <- readFile file
    seq <- return $ f . fromStr $ input
    writeFile ("m" ++ file) (toStr seq)
    return file
    
newtype Dna = Dna String
            deriving(Show,Eq,Read)

instance Sequence Dna where
  toStr (Dna s) = s

  fromStr = Dna . (filter (`elem`legal)) . (map toUpper) . (filter (`notElem`blank))
    where
      legal = "AGCT"
      blank = "\n \t"
  
  seqCom (Dna s) = Dna (map comp s)
    where
      comp 'A' = 'T'
      comp 'T' = 'A'
      comp 'G' = 'C'
      comp 'C' = 'G'
      comp _ = error "Invalid DNA sequence"
      
  seqRev (Dna s) = Dna $ reverse s
