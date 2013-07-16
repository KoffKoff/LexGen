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
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.IntMap as IM

type Byte = Word8
type TokenTree = FingerTree TOKANS (Byte,DFA' SNum)

--Only supports the first 256 characters of UTF-8 atm.
instance Measured TOKANS (Byte,DFA' SNum) where
  measure (c,dfa) = let t = (dfa'_states dfa) IM.! (fromEnum c)
                    in T $ S.singleton (Token t (B.singleton c) []
                                        (getTokenId (head $dfa'_start_states dfa) t))

main :: IO ()
main = do args <- getArgs
          case args of
            [alex_file,code_file] -> lexFile alex_file code_file >>= print . measure
            _ -> putStrLn "Usage: <This programs name> <alex file> <code file>"

lexFile :: FilePath -> FilePath -> IO TokenTree
lexFile alex_file code_file = do
  dfa <- build alex_file
  prg <- readCode code_file
--  return (T S.empty)
  return . fromList $ zip prg (repeat dfa)

readCode :: FilePath -> IO [Byte]
readCode code_file = do h <- openFile code_file ReadMode
                        hSetEncoding h utf8
                        hGetContents h >>= return . concatMap encode