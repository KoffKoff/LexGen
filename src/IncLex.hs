{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module IncLex where

--import qualified Data.Array as B
import Data.Maybe
--import Data.Array.Unboxed
import qualified Data.Foldable as F
import Data.Monoid
import Data.FingerTree
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map as Map hiding (Map)
import qualified Data.IntMap as IM hiding (IntMap)
import Alex.AbsSyn hiding (State)

import qualified Data.Sequence as S

-- combines state maps into one state map
tabulate :: State -> Edges State -> Edges State -> Edges State
tabulate start_state e1 e2 = Map.foldlWithKey f Map.empty e1
  where f es' is (s,_) = case Map.lookup s e2 of
          Just os -> Map.insert is os es'
          _ -> es'

type MidTransition = [Token]
type OutState = (State,Bool)
type State = Int
data Token = Token {edges :: (Edges State)
                   ,str   :: String
                   ,mid_trans :: MidTransition
                   ,token_id ::(Maybe Tid)}
type Tid = Int

-- Alternative produces the same result as the data above but uses sequence.
data TOKANS = T (S.Seq Token)

instance Show TOKANS where
  show (T tokans) = S.foldlWithIndex (\str _ tok -> str ++ "\n" ++ show tok) "" tokans

instance Monoid TOKANS where
  mempty = T mempty
  (T ts1) `mappend` (T ts2) = T $ combineTOKANS ts1 ts2

combineTOKANS :: S.Seq Token -> S.Seq Token -> S.Seq Token
combineTOKANS toks1 toks2 = case (S.viewr toks1,S.viewl toks2) of
  (S.EmptyR,_) -> toks2
  (_,S.EmptyL) -> toks1
  (ts1 S.:> t1@(Token tab1 s1 mt1 id1),t2@(Token tab2 s2 mt2 id2) S.:< ts2) ->
    let t = tabulate 0 tab1 tab2
    in case Map.null t of
      True -> let toks1' = case S.null ts1 of
                    True -> checkMidTrans t1
                    _ -> combineTOKANS ts1 $ checkMidTrans t1
                  toks2' = case S.null ts2 of
                    True -> checkMidTrans t2
                    _ -> combineTOKANS (checkMidTrans t2) ts2
              in toks1' `mappend` toks2'
      _    -> let mts = [t1,t2]
              in (ts1 S.|> Token t (s1 ++ s2) mts (getTokenId 0 t)) `mappend` ts2

checkMidTrans :: Token -> S.Seq Token
checkMidTrans tok@(Token _ _ _ (Just _)) = S.singleton tok
checkMidTrans tok = case mid_trans tok of
  [] -> S.singleton tok
  [t1,t2] -> combineTOKANS (checkMidTrans t1) (checkMidTrans t2)
{-
fix_mid_trans :: [MidTransition] -> [MidTransition] -> String -> String ->
                 Edges State -> [MidTransition]
fix_mid_trans mt1 mt2 s1 s2 tab2 = map f1 mt1 -- ++ map f2 mt2
  where f1 (tok,str,(s,_)) = (tok,str++s2,tab2 Map.! s)
        f2 (tok,str,os) = (tok,str,os) --more needs to be done
-}
instance Show Token where
  show (Token tab s mts id) = s ++ ":" ++ show id{- ++ "++" ++ drop 2
                                (foldl (\s1 mt -> s1 ++ "--" ++ str mt ++ ":" ++ show (token_id mt)) "" mts) ++ "\n"--}

getTokenId :: (Num s,Ord s) => s -> Edges s -> Maybe s
getTokenId s t = case Map.lookup s t of
  Just (id,True) -> Just id
  _ -> Nothing
{-
-- Below code are for use in the statemachine sm. Only for simply tests atm.
-- Make the type more polymorphic?
type FingerLex = FingerTree TOKANS Char


sm :: Int -> Edges State
sm c = case charType (toEnum c) of
  Letter -> Map.fromList [(0,(1,True)),(1,(1,True))]
  Space -> Map.fromList [(0,(2,True)),(2,(2,True))]
  Digit -> Map.fromList [(0,(3,True)),(1,(1,True)),(3,(3,True))]

toFinger = F.foldl' (|>) empty
string i = toFinger $ take i $ cycle "the quick b2own 22 a22fox22a 22a jumped over the lazy dog"

-- A mapping from char to a table for that char, depends on the statemachine
letters :: IntMap (Edges State)
letters = IM.fromList [(i,sm i) | i <- [0..255]]

data OMG = Letter | Space | Digit

charType :: Char -> OMG
charType ' ' = Space
charType c | '0' < c && c < '9' = Digit
charType _ = Letter -}
tst = "hej"
--}