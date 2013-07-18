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

-- Tries to merge the tokens in the edges of the sequences
combineTOKANS :: Seq Token -> Seq Token -> Seq Token
combineTOKANS toks1 toks2 = case (viewr toks1,viewl toks2) of
  (EmptyR,_) -> toks2
  (_,EmptyL) -> toks1
  (ts1 :> t1,t2 :< ts2) -> ts1 >< combineToken t1 t2 >< ts2

-- Combines token1 with as much of token2 as possible
combineToken :: Token -> Token -> Seq Token
combineToken t1 t2 = let e = tabulate (edges t1) (edges t2)
                   in case Map.null e of
                     False -> singleton $ mergeToken e t1 t2
                     True  -> case mid_trans t2 of
                                   [] -> fromList [t1,t2]
                                   mt -> combineToken' t1 mt

combineToken' :: Token -> MidTransition -> Seq Token
combineToken' t1 ts2 = let t2 = head ts2
                           t3 = head $ tail ts2
                           e = tabulate (edges t1) (edges t2)
                       in case Map.null e of
                         True  -> case mid_trans t2 of
                                    [] -> t1 <| combineTOKANS (checkStates t2) (singleton t3)
                                    mt -> combineToken' t1 mt >< checkStates t3
                         False -> case mid_trans t3 of
                                    [] -> mergeToken e t1 t2 <| checkStates t3
                                    mt -> combineToken' (mergeToken e t1 t2) mt

-- Removes all edges but the one starting in start_state if no such edge exist
-- It breaks the token up into smaller pieces
checkStates :: Token -> Seq Token
checkStates token = case Map.lookup start_state (edges token) of
  Nothing        -> case mid_trans token of 
    [] -> singleton $ token {edges = Map.empty}
    [t1,t2] -> combineTOKANS (checkStates t1) (singleton t2)
  Just out_state -> singleton $ token {edges = Map.singleton start_state out_state}

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
