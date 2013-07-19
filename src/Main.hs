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
type TokenTree = FingerTree TOKANS (Byte,DFA' SNum Code)

--Only supports the first 256 characters of UTF-8 atm.
instance Measured TOKANS (Byte,DFA' SNum Code) where
  measure (c,dfa) = let t = (dfa'_states dfa) IM.! (fromEnum c)
                    in T $ S.singleton (Token t (B.singleton c) [] False False
                                        (getTokenId (head $dfa'_start_states dfa) t))

main :: IO ()
main = do args <- getArgs
          case args of
            [alex_file,code_file] -> lexFile alex_file code_file >>= print . measure
            _ -> putStrLn "Usage: <This programs name> <alex file> <code file>"

lexFileTest :: FilePath -> FilePath -> IO (TokenTree,DFA' SNum Code)
lexFileTest alex_file code_file = do
  dfa <- build alex_file
  prg <- readCode code_file
  return (fromList $ zip (0:prg) (repeat dfa),dfa)

lexFile :: FilePath -> FilePath -> IO TokenTree
lexFile alex_file code_file = do
  dfa <- build alex_file
  prg <- readCode code_file
  return . fromList $ zip (0:prg) (repeat dfa)

readCode :: FilePath -> IO [Byte]
readCode code_file = do h <- openFile code_file ReadMode
                        hSetEncoding h utf8
                        hGetContents h >>= return . concatMap encode


-- Functions for some debugging
insertStart :: Byte -> TokenTree -> TokenTree
insertStart b str = (h,dfa) <| (b,dfa) <| str'
  where ((h,dfa) :< str') = viewl str

insertEnd :: TokenTree -> Byte -> TokenTree
insertEnd str b = str' |> (l,dfa) |> (b,dfa)
  where (str' :> (l,dfa)) = viewr str

headF :: TokenTree -> Byte
headF f = fst h
  where h :< _ = viewl f

lastTransition :: TOKANS -> Transition
lastTransition (T tree) = edges t
  where _ S.:> t = S.viewr tree