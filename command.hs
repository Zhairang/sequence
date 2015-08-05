module Command
       (Exp(..),
        eval,
        Value(..),
        groupEvery,
        groupByString,
        addToGroup,
        out')
       where

import Sequence

import Glob

import Content

import Data.List

import Text.Regex.Posix((=~))

data Exp =
  File String  -- file pattern
  |Com Exp --complement of sequence
  |Rev Exp --reverse of sequence
  |ComRev Exp
--  |RevCom Exp
  |Union Exp Exp --union of the file
  |GrpBy String Exp
  |Grp Exp
  |Tag Exp
  |Add Exp Exp --add files to groups
  |Out Exp
  |Out1 String Exp
  |Com' Exp
  |Rev' Exp
  |ComRev' Exp
  |Id Exp
  deriving(Read,Show)

data Value = Files [String]
           |Grps [[String]]
              deriving(Show,Read)

eval :: Exp -> IO Value
eval (File patt) = do
  files <- namesMatching patt
  return (Files files)
eval (Com exp) = changeDna seqCom exp
eval (Rev exp) = changeDna seqRev exp
eval (ComRev exp) = changeDna comRev exp
--eval (RevCom exp) = eval (ComRev exp)
eval (Union exp1 exp2) = do
  Files f1 <- eval exp1
  Files f2 <- eval exp2
  return (Files (f1 `union` f2))
eval (GrpBy s exp) = do
  Files files <- eval exp
  return (Grps (grpBy ((=~ s)::(String -> String)) files))
eval (Grp exp) = do
  Files f <- eval exp
  return (Grps [f])
eval (Tag exp) = do
  Files files <- eval exp
  tag files
  return (Files files)
eval (Add exp1 exp2) = do
  Files files <- eval exp1
  Grps groups <- eval exp2
  return (Grps (map (files++) groups))
eval (Out exp) =
  do
    Grps groups <- eval exp
    files <- mapM outputGroup groups
    return (Files files)
    --return (Grps groups)
eval (Out1 name exp) =
   do
     Grps groups <- eval exp
     allCont <- mapM getAllContents groups
     let output = foldl1' (++) (intersperse divider allCont)
     writeFile name output
     return (Files [name])
   where divider = '\n' : replicate 50 '*' ++ "\n"
eval (Com' exp) = changeDna' seqCom exp
eval (Rev' exp) = changeDna' seqRev exp
eval (ComRev' exp) = changeDna' comRev exp
eval (Id exp) = changeDna' id exp

groupEvery :: Int -> Value -> Value
groupEvery n (Files files) =
  Grps (partition' n files)

groupByString :: String -> Value -> Value
groupByString patt (Files files) =
  Grps (grpBy ((=~ patt)::(String -> String)) files)

addToGroup (Files files) (Grps groups) =
  Grps (map (`union` files) groups)

out' (Grps groups) =
  mapM_ outputGroup groups

partition' _ [] = []
partition' n xs =
  let (a , b) = splitAt n xs in
    a : partition' n b

changeDna :: (Dna -> Dna) -> Exp -> IO Value
changeDna f exp = do
  Files files <- eval exp
  mapM_ (fileMap (toStr . f . fromStr)) files
  return (Files files)

changeDna' :: (Dna -> Dna) -> Exp -> IO Value
changeDna' f exp = do
  Files files <- eval exp
  mapM_ (initialMap (toStr . f .fromStr)) files
  return (Files files)

grpBy :: (Ord a) => (b -> a) -> [b] -> [[b]]
grpBy f lis =
  groupBy fSame (sortBy fComp lis)
  where fComp a b = compare (f a) (f b)
        fSame a b = f a == f b
