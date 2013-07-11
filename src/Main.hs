{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import System.IO
import BuildDFA
import IncLex
import Data.FingerTree
import Alex.AbsSyn
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.IntMap as IM

type TokenTree = FingerTree TOKANS (Char,DFA' SNum)

instance Measured TOKANS (Char,DFA' SNum) where
  measure (c,dfa) = let t = (dfa'_states dfa) IM.! (fromEnum c)
                    in T $ S.singleton (Token t [c] (M.lookup (head $ dfa'_start_states dfa) t))

lex :: FilePath -> FilePath -> IO TOKANS
lex alex_file code_file = do
  dfa <- build alex_file
  h <- openFile code_file ReadMode
  prg <- hGetContents h
  return . measure . fromList $ zip prg (repeat dfa)