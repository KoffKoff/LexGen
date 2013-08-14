{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module IncLex where

import Prelude as P
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import BuildDFA
import Alex.AbsSyn hiding (State)
import AbsSyn
import Data.Sequence as S
import Text.Printf
import Data.Foldable

instance Monoid Size where
   mempty = Size 0
   Size m `mappend` Size n = Size (m+n)
   
instance Monoid LexedTokens where
  mempty = L $ \_ -> (Single ("",[]),-1)
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
combineTokens toks1 toks2 = L $ \in_state ->
  let (seq1,mid_state) = getMap toks1 $ in_state
  in case (mid_state,getMap toks2 $ mid_state) of
    (-1,_) -> if isSingle seq1 in_state
              then append seq1
              else getMap mempty $ -1
    (_,(_,-1)) -> if isAccepting seq1
                  then append seq1
                  else getMap mempty $ -1
    (_,(seq2,out_state)) -> if isAccepting seq2
                            then (mergeTokens seq1 seq2,out_state)
                            else getMap mempty $ -1
  where append seq1 = let (seq2,out_state) = getMap toks2 $ start_state
                      in (appendTokens seq1 seq2,out_state)

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

isAccepting :: Tokens -> Bool
isAccepting (Single (_,acc)) = acc /= []
isAccepting (Toks _ _ (_,acc)) = acc /= []

isSingle :: Tokens -> Int -> Bool
isSingle (Single (lex,_)) 0 = P.length lex == 1
isSingle (Toks _ _ (lex,_)) 0 = P.length lex == 1
isSingle _ _ = False

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
