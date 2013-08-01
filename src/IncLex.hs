{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module IncLex where

import Prelude as P
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Alex.AbsSyn hiding (State)
import Data.Sequence as S
import Text.Printf

-- combines state maps into one state map
tabulate :: Transition -> Transition -> Transition
tabulate e1 e2 = Map.foldlWithKey f Map.empty e1
  where f es' is (s,_) = case Map.lookup s e2 of
          Just os -> Map.insert is os es'
          _ -> es'

-- Hardcoded atm, needs to be checked from the DFA
start_state :: State
start_state = 0

type MidTransition = [Token]
type OutState = (State,[Accept Code])
type State = Int
type Transition = Edges State Code
data Token = Token {transitions :: Transition
                   ,lexeme      :: String
                   ,sub_tokens  :: MidTransition
                   ,token_id    :: Maybe [Accept Code]}
--type Tid = Int

data TOKANS = T (Seq Token )

instance Show TOKANS where
  show (T tokans) = foldlWithIndex (\str _ tok -> str ++ "\n" ++ show tok) "" tokans

instance Monoid TOKANS where
  mempty = T mempty
  (T ts1) `mappend` (T ts2) = T $ combineTokens ts1 ts2

-- Merges two sequences of tokens
combineTokens :: Seq Token -> Seq Token -> Seq Token
combineTokens toks1 toks2 = case viewr toks1 of
  EmptyR         -> toks2
  toks1' :> tok1 -> let toks2' = tokenAppender tok1 toks2
                    in toks1' >< toks2'

tokenAppender :: Token -> Seq Token -> Seq Token
tokenAppender tok1 toks2 = case viewl toks2 of
  EmptyL         -> singleton tok1
  tok2 :< toks2' -> let e = getTransition $ tabulate (transitions tok1) (transitions tok2)
                    in if Map.null e
                       then divideAppender tok1 tok2 >< toks2'
                       else tokenAppender (mergeToken tok1 tok2) toks2'

divideAppender :: Token -> Token -> Seq Token
divideAppender tok1 tok2 = case sub_tokens tok2 of
  [] -> splitToken tok1 tok2
  [subtok1,subtok2] ->
    let e = getTransition $ tabulate (transitions tok1) (transitions subtok1)
    in if Map.null e
       then case sub_tokens subtok1 of
         [] -> splitToken tok1 tok2
         mt -> combineTokens (divideAppender tok1 subtok1) (singleton subtok2)
       else divideAppender (mergeToken tok1 subtok1) subtok2

splitToken :: Token -> Token -> Seq Token
splitToken tok1 tok2 = case token_id tok1 of
  Just _  -> fromList [tok1,tok2]
  Nothing -> case sub_tokens tok1 of
    [] -> fromList [tok1,tok2]
    [subtok1,subtok2] ->
      let subsubtok2 :< toks2 = viewl $ tokenAppender subtok2 (singleton tok2)
      in splitToken subtok1 subsubtok2 >< toks2

mergeToken :: Token -> Token -> Token
mergeToken tok1 tok2 = 
  let e = tabulate (transitions tok1) (transitions tok2)
  in Token e (lexeme tok1 `mappend` lexeme tok2) [tok1,tok2] (getTokenId start_state e)

instance Show Token where
  show (Token tab s mts id) = printf "%-11s:%s" (show_id id) (fix_lex s)

show_id :: Maybe [Accept Code] -> String
show_id Nothing     = "Nothing"
show_id (Just accs) = P.filter ('\"' /=) . show $ map show_acc_num accs
  where show_acc_num (Acc p _ _ _) = "Acc " ++ show p

fix_lex :: String -> String
fix_lex lex = foldl (\str c -> if c == '\n' then str ++ "\\n" else str ++ [c]) "" lex

-- Returns Just Tid if that state is accepting
getTokenId :: State -> Transition -> Maybe [Accept Code]
getTokenId s t = case Map.lookup s t of
  Just (id,a:as) -> Just (a:as)
  _              -> Nothing

getTransition :: Transition -> Transition
getTransition t = case Map.lookup start_state t of
  Just osa -> Map.singleton start_state osa
  _ -> Map.empty
