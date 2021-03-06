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
  { start_states :: [s]
  , dfa_states   :: Array Int (Map s s)
  , accepts      :: Array s [Accept a]}

instance (Ix s, Show s, Show a) => Show (DFA' s a) where
  show (DFA' ss dfas accs) = "Initial States: " ++ show ss ++ "\nState map:\n" ++ show dfas ++ "\nAccepts:\n" ++ show accs

type State = Int
data Token = Token {lexeme      :: String
                   ,token_id    :: [Accept Code]}
data Tokens = Single (String,[Accept Code])
--                  Suffix of a token                  Prefix of a token
            | Toks (String,[Accept Code]) (Seq Token) (String,[Accept Code])
newtype LexedTokens = L {getMap :: SNum -> (Tokens ,State)}

type Byte = Word8

type TokenTree = FingerTree (LexedTokens, Size) (Byte,DFA' SNum Code)
data Size = Size Int
    deriving (Eq,Ord,Show)
