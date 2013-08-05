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
  , dfa'_states       :: Array Int (Map s (s,[Accept a]))}
--  , dfa'_accept       :: Map s [Accept a]}

instance (Show s, Show a) => Show (DFA' s a) where
  show (DFA' ss dfas) = "Initial States: " ++ show ss ++ "\nState map:\n" ++ show dfas

type OutState = (State,[Accept Code])
type State = Int
data Token = Token {lexeme      :: String
                   ,token_id    :: [Accept Code]}
data Tokens = Single (String,[Accept Code])
--                  Suffix of a token                  Prefix of a token
            | Toks (String,[Accept Code]) (Seq Token) (String,[Accept Code])
newtype LexedTokens = L {getMap :: Map SNum (Tokens ,OutState)}

type Byte = Word8

type TokenTree = FingerTree LexedTokens (Byte,DFA' SNum Code)
