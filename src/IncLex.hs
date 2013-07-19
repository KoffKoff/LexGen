{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module IncLex where

import Prelude as P
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
data Token = Token {edges      :: Transition
                   ,str        :: B.ByteString
                   ,mid_trans  :: MidTransition
                   ,start_done :: Bool
                   ,end_done   :: Bool
                   ,token_id   :: (Maybe Tid)}
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
  (ts1 :> t1,t2 :< ts2) -> ts1 >< combineToken (t1) (t2) >< ts2

-- Tries to combine two tokens
combineToken :: Token -> Token -> Seq Token
combineToken t1 t2 = let e = tabulate (edges t1) (edges t2)
                         e' = getTransition (start_done t1) (end_done t2) e
                   in case Map.null e' of
                     False -> singleton $ mergeToken t1 t2
                     True  -> combineToken' t1 t2

-- Combines the first token with as much of the second as possible
combineToken' :: Token -> Token -> Seq Token
combineToken' t1 tt2 = case mid_trans tt2 of
  [] -> t1 {end_done = True} <| singleton tt2
  [t2,t3] -> let e = tabulate (edges t1) (edges t2)
                 e' = getTransition (start_done t1) (end_done t2) e
             in case Map.null e' of
               True  -> case mid_trans t2 of
                 [] -> t1 {end_done = True} <| combineToken (t2 {start_done = True}) t3
                 mt -> combineToken' t1 t2 >< singleton (t3 {start_done = True})
               False -> case mid_trans t3 of
                 mt -> combineToken' (mergeToken t1 t2) t3
{-
-- Removes all edges but the one starting in start_state if no such edge exist
-- It breaks the token up into smaller pieces
checkStart :: Token -> Seq Token
checkStart token = case Map.lookup start_state (edges token) of
  Nothing        -> case mid_trans token of 
    [] -> singleton $ token --{start_done = True}
    [t1,t2] -> combineTOKANS (checkStart t1) (singleton t2)
  Just out_state -> singleton $ token --{start_done = True}

-- Removes all edges but the ones ending in an accepting state if no such edge
-- exist it breaks the token up into smaller pieces
checkEnd :: Token -> Seq Token
checkEnd token = let e' = Map.filter (not . P.null . snd) (edges token)
                 in case Map.null e' of
  True -> case mid_trans token of
    [] -> singleton $ token --{end_done = True}
    [t1,t2] -> combineTOKANS (singleton t1) (checkEnd t2)
  False -> singleton $ token --{end_done = True}
-}
-- Merges two tokens using the edges e
mergeToken :: Token -> Token -> Token
mergeToken t1 t2 = Token e (str t1 `mappend` str t2) [t1 {end_done = False},t2 {start_done = False}] (start_done t1)
                     (end_done t2) (getTokenId start_state e)
  where e = tabulate (edges t1) (edges t2)

instance Show Token where
  show (Token tab s mts _ _ id) = show s ++ ":" ++ if isJust id then show (fromJust id) else show id

-- Returns Just Tid if that state is accepting
getTokenId :: State -> Transition -> Maybe Tid
getTokenId s t = case Map.lookup s t of
  Just (id,a:as) -> Just id
  _ -> Nothing

getTransition :: Bool -> Bool -> Transition -> Transition
getTransition True True t = case Map.lookup start_state t of
  Just (os,a:as) -> Map.singleton start_state (os,a:as)
  _ -> Map.empty
getTransition True _    t = case Map.lookup start_state t of
  Just osa -> Map.singleton start_state osa
  _ -> Map.empty
getTransition _    True t = Map.filter (\(_,as) -> P.null as) t
getTransition _    _    t = t