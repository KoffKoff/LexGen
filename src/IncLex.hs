{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
module IncLex where

--import qualified Data.Array as B
import Data.Maybe
import Data.Array.Unboxed
import qualified Data.Foldable as F
import Data.Monoid
import Data.FingerTree


-- Placeholder until we decision on wether to use Map or Array as Table
tabulate :: (State -> Maybe State) -> Table
tabulate f = array (0,5) [(x,f x) | x <- [0..5]]

type State = Int
type Size = Sum Int
type Table = Array State (Maybe State)
data Token = Token Table Tid
type Tid = Int
-- Make the type more polymorphic?
type FingerLex = FingerTree Tokens Char
-- Size mighgt not be needed; newtype since we want to use our instance of Monoid.
newtype Tokens = T (FingerTree Size Token)

instance Show Token where
  show (Token _ id) = show id

instance Show Tokens where
  show (T ft) = show ft

instance Measured Size Token where
  measure t = Sum 1

instance Monoid Tokens where
  mempty = T mempty
  (T ts1) `mappend` (T ts2) =
-- left most element of the right token sequence and right most element of the left sequence
    let (ts1' :> Token t1 _) = viewr ts1
        (Token t2 _ :< ts2') = viewl ts2
    in T $ case glue t1 t2 of
      Just s -> (ts1' |> Token (tabulate (\s -> glue t1 t2)) s) >< ts2'
      Nothing -> (ts1' |> Token t1 (fromJust (t1 ! 0))) >< (Token t2 (fromJust (t2 ! 0)) <| ts2')

-- Some index measure might be needed when we insert elements in later stages of the project
instance Measured Tokens Char where
  measure c = let t = letters ! c
              in T $ singleton $ Token t (fromJust (t ! 0))

glue :: Table -> Table -> Maybe State
glue t1 t2 = case t1 ! 0 of
  Just s -> t2 ! s
  _      -> error $ "state: " ++ show t1

--Simple state machine to demonstrate the idea
sm :: Char -> State -> Maybe State
sm c 0 = case charType c of
  Letter -> Just 1
  Space -> Just 2
  Digit -> Just 3
sm c 1 = case charType c of
  Letter -> Just 1
  Digit -> Just 1
  _ -> Nothing
sm c 2 = case charType c of
  Space -> Just 2
  _ -> Nothing
sm c 3 = case charType c of
  Digit -> Just 3
  _ -> Nothing

toFinger = F.foldl' (|>) empty
string i = toFinger $ take i $ cycle "the quick b2own 22 a22fox22a 22 jumped over the lazy dog"

-- A mapping from char to a table for that char, depends on the statemachine
letters :: Array Char Table
letters = array (' ','z') [(i,tabulate (sm i)) | i <- [' '..'z']]

-- Below code are for use in the statemachine sm.
data OMG = Letter | Space | Digit

charType :: Char -> OMG
charType ' ' = Space
charType c | '0' < c && c < '9' = Digit
charType _ = Letter
