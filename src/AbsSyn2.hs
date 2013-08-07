module AbsSyn2 ( DFA' (..)
               , State
               , OutState
               , Token (..)
               , Tokens (..)
               , Byte
               , TokenTree (..)
               , LexedTokens (..))where

import Data.Monoid
import Data.Array
import Data.Map (Map)
import Alex.AbsSyn hiding (State)
import Data.Sequence as S
import Data.Word
import Data.FingerTree
import AbsSyn hiding (LexedTokens,TokenTree)

newtype LexedTokens = L {getMap :: SNum -> (Tokens ,OutState)}
type TokenTree = FingerTree LexedTokens (Byte,DFA' SNum Code)