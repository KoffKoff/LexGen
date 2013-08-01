module AbsSyn where

import Data.Monoid
import Data.Array
import Data.Map (Map)
import Alex.AbsSyn hiding (State)
import Data.Sequence as S
import Data.Word
import Data.FingerTree

-- Edges represents the transitions possible for a given character
data DFA' s a = DFA'
  { dfa'_start_states :: [s]
  , dfa'_states       :: Array Int (Edges s a) }

instance (Show s, Show a) => Show (DFA' s a) where
  show (DFA' ss dfas) = "Initial States: " ++ show ss ++ "\nState map:\n" ++ show dfas

type Edges s a = Map s (s,[Accept a])

type MidTransition = [Token]
type OutState = (State,[Accept Code])
type State = Int
type Transition = Edges State Code
data Token = Token {transitions :: Transition
                   ,lexeme      :: String
                   ,sub_tokens  :: MidTransition
                   ,token_id    :: Maybe [Accept Code]}
data TOKANS = T (Seq Token )
type Byte = Word8
type TokenTree = FingerTree TOKANS (Byte,DFA' SNum Code)
