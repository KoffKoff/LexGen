{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import System.Environment
import System.IO
import BuildDFA
import IncLex
import Data.FingerTree
import Alex.AbsSyn
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.IntMap as IM

type TokenTree = FingerTree TOKANS (Char,DFA' SNum)

--Only supports the first 256 characters of UTF-8 atm.
instance Measured TOKANS (Char,DFA' SNum) where
  measure (c,dfa) = let t = (dfa'_states dfa) IM.! (fromEnum c)
                    in T $ S.singleton (Token t [c] []
                                        (getTokenId (head $dfa'_start_states dfa) t))

main :: IO ()
main = do args <- getArgs
          case args of
            [alex_file,code_file] -> lexFile alex_file code_file >>= print
            _ -> putStrLn "Usage: <This programs name> <alex file> <code file>"

lexFile :: FilePath -> FilePath -> IO TOKANS
lexFile alex_file code_file = do
  dfa <- build alex_file
  h <- openFile code_file ReadMode
  prg <- hGetContents h
  hSetEncoding h utf8
  return . measure . fromList $ zip prg (repeat dfa)