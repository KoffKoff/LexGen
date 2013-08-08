{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module IncLex2 where

import Prelude as P
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import BuildDFA
import Alex.AbsSyn hiding (State)
--import AbsSyn
import AbsSyn2
import Data.Sequence as S
import Text.Printf
import Data.Foldable

instance Monoid Size where
   mempty = Size 0
   Size m `mappend` Size n = Size (m+n)
   
instance Monoid LexedTokens where
  mempty = L $ \_ -> (Single ("",[]),(-1,[]))
  mappend = combineTokens

instance Show Token where
  show (Token s id) = printf "%-11s:%s" (show_id id) (fix_lex s)

instance Show Tokens where
  show (Single token) = show token
  show (Toks suf toks pre) = show (makeTok suf) ++ "\n" ++
                             unlines (map show (toList toks)) ++ "\n" ++
                             show (makeTok pre)

instance Show LexedTokens where
  show (L seqs) = case seqs start_state of
    (toks,_) -> show toks

-- Hardcoded atm, needs to be checked from the DFA
start_state :: State
start_state = 0

-- Merges every possible combinations of two token sequences
combineTokens :: LexedTokens -> LexedTokens -> LexedTokens
combineTokens (L seqs1) lseqs2 = combineSequence lseqs2 seqs1

-- Checks wether a token sequnce can be combined with a set of token sequences.
combineSequence :: LexedTokens -> (SNum -> (Tokens,OutState)) -> LexedTokens
combineSequence (L seqs2) f = L $ \is -> let (toks1,(os,acc)) = f is
                                         in case seqs2 os of
  (toks2,(-1,acc2)) -> case (acc,seqs2 start_state) of
    ([],(_,(-1,_))) -> error "Lexical error"
    (accs,(toks2,out)) -> (appendTokens toks1 toks2,out)
  (toks2,out) -> (mergeTokens toks1 toks2,out)

-- Combines the right partial token with the left partial token and returns a
-- sequence of tokens.
mergeTokens :: Tokens -> Tokens -> Tokens
mergeTokens (Single (tok1,_)) (Single (tok2,acc)) = Single (tok1 ++ tok2,acc)
mergeTokens (Single (tok1,_)) (Toks (tok2,acc) ts tend) = Toks (tok1 ++ tok2,acc) ts tend
mergeTokens (Toks tstart ts (tok1,_)) (Single (tok2,acc)) = Toks tstart ts (tok1++tok2,acc)
mergeTokens (Toks tstart ts1 (tok1,_)) (Toks (tok2,acc) ts2 tend) =
  Toks tstart ((ts1 |> (Token (tok1 ++ tok2) acc)) >< ts2) tend

-- Appends the second token sequence to the first token sequence
appendTokens :: Tokens -> Tokens -> Tokens
appendTokens (Single tstart) (Single tend) = Toks tstart S.empty tend
appendTokens (Single tstart) (Toks suf ts tend) = Toks tstart (makeTok suf <| ts) tend
appendTokens (Toks tstart ts pre) (Single tend) = Toks tstart (ts |> makeTok pre) tend
appendTokens (Toks tstart ts1 pre) (Toks suf ts2 tend) =
  Toks tstart ((ts1 |> makeTok pre) >< (makeTok suf <| ts2)) tend

-- Constructs a token of a string and a list of accepting states
makeTok :: (String,[Accept Code]) -> Token
makeTok (lex,acc) = Token lex acc

-- Pretty print for final (accepting) states
show_id :: [Accept Code] -> String
show_id []   = "No Token"
show_id accs = P.filter ('\"' /=) . show $ map show_acc_num accs
  where show_acc_num (Acc p _ _ _) = "Acc " ++ show p

-- Changes line breaks to '\n' in a string
fix_lex :: String -> String
fix_lex lex = P.foldl (\str c -> if c == '\n' then str ++ "\\n" else str ++ [c]) "" lex
