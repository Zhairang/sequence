module Sequence
       (
         Dna,
         Sequence
       )where

import Data.Char

class Sequence a where
  toStr :: a -> String  --sequence in string form
  seqCom :: a -> a  --the complement of a sequence
  comRev :: a -> a --the reverse sequence
  fromStr :: String -> a
  revAndCom :: a -> a
  revAndCom = seqCom . comRev

newtype Dna = Dna String
            deriving(Show,Eq,Read)

instance Sequence Dna where
  toStr (Dna s) = s

  fromStr = Dna . (map toUpper)
  
  seqCom (Dna s) = Dna (map comp s)
    where
      comp 'A' = 'T'
      comp 'T' = 'A'
      comp 'G' = 'C'
      comp 'C' = 'G'
      comp _ = error "Invalid DNA sequence"
      
  comRev (Dna s) = Dna $ reverse s
