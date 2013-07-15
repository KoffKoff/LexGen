{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module IncLex where

--import qualified Data.Array as B
import Data.Maybe
--import Data.Array.Unboxed
import qualified Data.Foldable as F
import Data.Monoid
--import Data.FingerTree
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map as Map hiding (Map)
import qualified Data.IntMap as IM hiding (IntMap)
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
data Token = Token {edges :: (Edges State)
                   ,str   :: String
                   ,mid_trans :: MidTransition
                   ,token_id ::(Maybe Tid)}
type Tid = Int

-- Alternative produces the same result as the data above but uses sequence.
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
                           ts2' = tail ts2
                           e = tabulate (edges t1) (edges t2)
                       in case Map.null e of
                         True  -> case mid_trans t2 of
                                    [] -> singleton t1 >< fromList ts2
                                    mt -> combineToken' t1 mt >< fromList (tail ts2)
                         False -> case mid_trans (head ts2') of
                                    [] -> mergeToken e t1 t2 <| fromList ts2'
                                    mt -> combineToken' (mergeToken e t1 t2) mt

-- Merges two tokens using the state sent
mergeToken :: Edges State -> Token -> Token -> Token
mergeToken e t1 t2 = let os = case Map.lookup start_state e of
                             Just (os',True) -> Just os'
                             _ -> Nothing
                       in Token e (str t1 ++ str t2) [t1,t2] os

instance Show Token where
  show (Token tab s mts id) = concatMap (\c -> if c == '\n' then "\\n" else [c]) s
                              ++ ":" ++ show id

getTokenId :: State -> Edges State -> Maybe State
getTokenId s t = case Map.lookup s t of
  Just (id,True) -> Just id
  _ -> Nothing
