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
tabulate :: Edges State -> Edges State -> Edges State
tabulate e1 e2 = Map.foldlWithKey f Map.empty e1
  where f es' is s = case Map.lookup s e2 of
          Just os -> Map.insert is os es'
          _ -> es'

type State = Int
data Token = Token (Edges State) String (Maybe Tid)
type Tid = Int
-- Should DEFFINIATLY NOT BE LIST, is sequence better in this instance?
data Tokens = Empty
            | One Token
            | Many Token [Token] Token

-- Alternative produces the same result as the data above but uses sequence.
newtype TOKANS = T (S.Seq Token)

instance Show TOKANS where
  show (T tokans) = show tokans

instance Monoid TOKANS where
  mempty = T mempty
  (T ts1) `mappend` (T ts2) = T $ combineTOKANS ts1 ts2

combineTOKANS :: S.Seq Token -> S.Seq Token -> S.Seq Token
combineTOKANS toks1 toks2 = case (S.viewr toks1,S.viewl toks2) of
  (S.EmptyR,_) -> toks2
  (_,S.EmptyL) -> toks1
  (ts1 S.:> (Token tab1 s1 id1),(Token tab2 s2 id2) S.:< ts2) ->
    let t = tabulate tab1 tab2
    in case Map.null t of
      True -> toks1 `mappend` toks2
      _    -> (ts1 S.|> Token t (s1 ++ s2) (Map.lookup 0 t)) `mappend` ts2

instance Show Tokens where
  show Empty = []
  show (One t) = show [t]
  show (Many t1 ts t2) = show (t1:ts ++ [t2])

instance Monoid Tokens where
  mempty = Empty
  mappend = combineTokens

combineTokens :: Tokens -> Tokens -> Tokens
combineTokens Empty toks2 = toks2
combineTokens toks1 Empty = toks1
combineTokens (One (Token t1 s1 id1)) (One (Token t2 s2 id2)) =
  let t = tabulate t1 t2
  in case Map.null t of
    True -> Many (Token t1 s1 id1) mempty (Token t2 s2 id2)
    _ -> One $ Token t (s1++s2) (Map.lookup 0 t)
combineTokens (Many t1 t1s tt1) (Many tt2 t2s t2) =
  case combineTokens (One tt1) (One tt2) of
    One t -> Many t1 (t1s `mappend` (t:t2s)) t2
    Many t1' _ t2' -> Many t1 (t1s `mappend` (t1':t2':t2s)) t2
combineTokens (Many t1 t1s tt1) oneT2 =
  case combineTokens (One tt1) oneT2 of
    One t -> Many t1 t1s t
    Many t _ t2 -> Many t1 (t1s `mappend` [t]) t2
combineTokens oneT1 (Many tt2 t2s t2) =
  case combineTokens oneT1 (One tt2) of
    One t -> Many t t2s t2
    Many t1 _ t -> Many t1 (t:t2s) t2

instance Show Token where
  show (Token _ str id) = show (str,id)

-- How do we put the good stuff in (letter in the code below)
instance Measured Tokens Char where
  measure c = let t = letters IM.! (fromEnum c)
              in One $ Token t [c] (Map.lookup 0 t)

-- Below code are for use in the statemachine sm. Only for simply tests atm.
-- Make the type more polymorphic?
type FingerLex = FingerTree TOKANS Char


sm :: Int -> Edges State
sm c = case charType (toEnum c) of
  Letter -> Map.fromList [(0,1),(1,1)]
  Space -> Map.fromList [(0,2),(2,2)]
  Digit -> Map.fromList [(0,3),(1,1),(3,3)]

toFinger = F.foldl' (|>) empty
string i = toFinger $ take i $ cycle "the quick b2own 22 a22fox22a 22a jumped over the lazy dog"

-- A mapping from char to a table for that char, depends on the statemachine
letters :: IntMap (Edges State)
letters = IM.fromList [(i,sm i) | i <- [0..255]]

data OMG = Letter | Space | Digit

charType :: Char -> OMG
charType ' ' = Space
charType c | '0' < c && c < '9' = Digit
charType _ = Letter
