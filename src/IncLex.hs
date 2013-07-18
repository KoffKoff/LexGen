{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module IncLex where

import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (toString)
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Alex.AbsSyn hiding (State)

import Data.Sequence as S

-- combines state maps into one state map
tabulate :: Edges State -> Edges State -> Edges State
tabulate e1 e2 = Map.foldlWithKey f Map.empty e1
  where f es' is (s,_) = case Map.lookup s e2 of
          Just os -> Map.insert is os es'
          _ -> es'

-- Hardcoded atm, needs to be checked from the DFA
start_state :: State
start_state = 0

type MidTransition = [Token]
type OutState = (State,Bool)
type State = Int
data Token = Token {edges     :: (Edges State)
                   ,str       :: B.ByteString
                   ,mid_trans :: MidTransition
                   ,token_id  :: (Maybe Tid)}
type Tid = Int

data TOKANS = T (Seq Token )

instance Show TOKANS where
  show (T tokans) = foldlWithIndex (\str _ tok -> str ++ "\n" ++ show tok) "" tokans

instance Monoid TOKANS where
  mempty = T mempty
  (T ts1) `mappend` (T ts2) = T $ combineTOKANS ts1 ts2

-- Merges two sequences of tokens
combineTOKANS :: Seq Token -> Seq Token -> Seq Token
combineTOKANS toks1 toks2 = case (viewr toks1,viewl toks2) of
  (EmptyR,_) -> toks2
  (_,EmptyL) -> toks1
  (ts1 :> t1,t2 :< ts2) -> ts1 >< combineToken t1 t2 >< ts2

-- Tries to combine to tokens
combineToken :: Token -> Token -> Seq Token
combineToken t1 t2 = let e = tabulate (edges t1) (edges t2)
                   in case Map.null e of
                     False -> singleton $ mergeToken e t1 t2
                     True  -> combineToken' t1 t2

-- Combines the first token with as much of the second as possible
combineToken' :: Token -> Token -> Seq Token
combineToken' t1 tt2 = case mid_trans tt2 of
  [] -> checkEnd t1 |> tt2
  [t2,t3] -> let e = tabulate (edges t1) (edges t2)
             in case Map.null e of
               True  -> case mid_trans t2 of
                 [] -> checkEnd t1 >< combineTOKANS (checkStart t2) (singleton t3)
                 mt -> combineToken' t1 t2 >< checkStart t3
               False -> case mid_trans t3 of
                 mt -> combineToken' (mergeToken e t1 t2) t3

-- Removes all edges but the one starting in start_state if no such edge exist
-- It breaks the token up into smaller pieces
checkStart :: Token -> Seq Token
checkStart token = case Map.lookup start_state (edges token) of
  Nothing        -> case mid_trans token of 
    [] -> singleton $ token {edges = Map.empty}
    [t1,t2] -> combineTOKANS (checkStart t1) (singleton t2)
  Just out_state -> singleton $ token {edges = Map.singleton start_state out_state}

-- Removes all edges but the ones ending in an accepting state if no such edge
-- exist it breaks the token up into smaller pieces
checkEnd :: Token -> Seq Token
checkEnd token = let e' = Map.filter snd (edges token)
             in case Map.null e' of
  True -> case mid_trans token of
    [] -> singleton $ token {edges = Map.empty}
    [t1,t2] -> combineTOKANS (singleton t1) (checkEnd t2)
  False -> singleton $ token {edges = e'}

-- Merges two tokens using the edges e
mergeToken :: Edges State -> Token -> Token -> Token
mergeToken e t1 t2 = let os = case Map.lookup start_state e of
                           Just (os',True) -> Just os'
                           _ -> Nothing
                     in Token e (str t1 `mappend` str t2) [t1,t2] os

instance Show Token where
  show (Token tab s mts id) = show s ++ ":" ++ if isJust id then show (fromJust id) else show id

-- Returns Just Tid if that state is accepting
getTokenId :: State -> Edges State -> Maybe Tid
getTokenId s t = case Map.lookup s t of
  Just (id,True) -> Just id
  _ -> Nothing
