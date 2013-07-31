{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module IncLex where

import Data.Maybe
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
                   ,str        :: String
                   ,mid_trans  :: MidTransition
                   ,token_id   :: Maybe Tid}
type Tid = Int

data TOKANS = T (Seq Token )

instance Show TOKANS where
  show (T tokans) = foldlWithIndex (\str _ tok -> str ++ "\n" ++ show tok) "" tokans

instance Monoid TOKANS where
  mempty = T mempty
  (T ts1) `mappend` (T ts2) = T $ combineTOKANS ts1 ts2

-- Merges two sequences of tokens
combineTOKANS :: Seq Token -> Seq Token -> Seq Token
combineTOKANS toks1 toks2 = case viewr toks1 of
  (EmptyR) -> toks2
  (ts1 :> t1) -> ts1 >< ts2'
    where ts2' = combineToken t1 toks2

-- Tries to combine two tokens, if it does, it calls itself with the new token
-- and the rest of the sequence
combineToken :: Token -> Seq Token -> Seq Token
combineToken t1 ts2 | S.null ts2 = singleton t1
                    | otherwise =
                      let t2 :< ts2' = viewl ts2
                          e = getTransition $ tabulate (edges t1) (edges t2)
                      in case Map.null e of
                        False -> combineToken (mergeToken t1 t2) ts2'
                        True  -> combineToken' t1 t2 >< ts2'

-- Combines the first token with as much of the second as possible
combineToken' :: Token -> Token -> Seq Token
combineToken' t1 tt2 = case mid_trans tt2 of
  [] -> case token_id t1 of
    Just _  -> fromList [t1,tt2]
    Nothing -> case mid_trans t1 of
      [] -> fromList [t1,tt2]
      [t11,t12] -> combineToken' t11 t12 |> tt2
  [t2,t3] -> let e = getTransition $ tabulate (edges t1) (edges t2)
             in case Map.null e of
               True  -> case mid_trans t2 of
                 [] -> t1 <| singleton tt2 --combineToken t2 (singleton t3)
                 mt -> combineTOKANS (combineToken' t1 t2) (singleton t3)
               False -> case mid_trans t3 of
                 mt -> combineToken' (mergeToken t1 t2) t3

-- Merges two tokens
mergeToken :: Token -> Token -> Token
mergeToken t1 t2 = Token e (str t1 `mappend` str t2) [t1,t2]
                         (getTokenId start_state e)
  where e = tabulate (edges t1) (edges t2)

instance Show Token where
  show (Token tab s mts id) = show s ++ ":" ++ if isJust id then show (fromJust id) else show id

-- Returns Just Tid if that state is accepting
getTokenId :: State -> Transition -> Maybe Tid
getTokenId s t = case Map.lookup s t of
  Just (id,a:as) -> Just id
  _ -> Nothing

getTransition :: Transition -> Transition
getTransition t = case Map.lookup start_state t of
  Just osa -> Map.singleton start_state osa
  _ -> Map.empty
