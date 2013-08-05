{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import System.Environment
import System.IO
import Data.Word
import BuildDFA
import IncLex
import Data.FingerTree hiding (reverse)
import Alex.AbsSyn
import Alex.UTF8
import AbsSyn
import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Array
import Data.Monoid

--Only supports the first 256 characters of UTF-8 atm.
instance Measured LexedTokens (Byte,DFA' SNum Code) where
  measure (c,dfa) = let t = (dfa'_states dfa) ! (fromEnum c)
                    in L $ M.map (\os -> (Single ([toEnum (fromEnum c)], snd os),os)) t

main :: IO ()
main = do args <- getArgs
          case args of
            [alex_file,code_file] -> lexFile alex_file code_file >>= print . measure
            _ -> putStrLn "Usage: <This programs name> <alex file> <code file>"

-- Constructs a DFA from an Alex specidification (alex_file) and uses it to lex
-- the code in the file 'code_file' and returns a token tree
lexFile :: FilePath -> FilePath -> IO TokenTree
lexFile alex_file code_file = do
  dfa <- build alex_file
  prg <- readCode code_file
  let code = fromList $ zip prg (repeat dfa)
  return code

readCode :: FilePath -> IO [Byte]
readCode code_file = do h <- openFile code_file ReadMode
                        hSetEncoding h utf8
                        hGetContents h >>= return . concatMap encode


-- Functions for some debugging
insertStart :: Byte -> TokenTree -> TokenTree
insertStart b str = (b,dfa) <| (h,dfa) <| str'
  where ((h,dfa) :< str') = viewl str

insertEnd :: TokenTree -> Byte -> TokenTree
insertEnd str b = str' |> (l,dfa) |> (b,dfa)
  where (str' :> (l,dfa)) = viewr str

headF :: TokenTree -> Byte
headF f = fst h
  where h :< _ = viewl f
