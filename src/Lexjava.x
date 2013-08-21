-- -*- haskell -*-
-- This Alex file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
module Lexjava where

import Data.FingerTree (FingerTree)
import qualified Data.FingerTree as F
import Data.Sequence
import Data.Monoid
import Data.Bits
}

-- %wrapper "posn"

$l = [a-zA-Z\192 - \255] # [\215 \247]    -- isolatin1 letter FIXME
$c = [A-Z\192-\221] # [\215]    -- capital isolatin1 letter FIXME
$s = [a-z\222-\255] # [\247]    -- small isolatin1 letter FIXME
$d = [0-9]                -- digit
$i = [$l $d _ ']          -- identifier character
$u = [\0-\255]          -- universal: any character

@rsyms =    -- symbols and non-identifier-like reserved words
   \. \* | \{ | \} | \; | \( | \) | \: | \= | \, | "else" \  "if" | \[ \] | \. | \? | \| \| | \& \& | \| | \^ | \& | \= \= | \! \= | \< | \> | \< \= | \> \= | \< \< | \> \> | \> \> \> | \+ | \- | \* | \/ | \% | \+ \+ | \- \- | \[ | \] | \. "this" | \. "class" | \~ | \! | \* \= | \/ \= | \% \= | \+ \= | \- \= | \< \< \= | \> \> \= | \> \> \> \= | \& \= | \^ \= | \| \=

:-
"//" [.]* ; -- Toss single line comments
"/*" ([$u # \*] | \* [$u # \/])* ("*")+ "/" ; 


$white+ ;
@rsyms { tok (\p s -> PT p (TS $ share s)) }
[1 2 3 4 5 6 7 8 9]$d * (u | U) { tok (\p s -> PT p (eitherResIdent (T_Unsigned . share) s)) }
[1 2 3 4 5 6 7 8 9]$d * (l | L) { tok (\p s -> PT p (eitherResIdent (T_Long . share) s)) }
[1 2 3 4 5 6 7 8 9]$d * (u l | U L) { tok (\p s -> PT p (eitherResIdent (T_UnsignedLong . share) s)) }
0 (x | X)($d | [a b c d e f]| [A B C D E F]) + { tok (\p s -> PT p (eitherResIdent (T_Hexadecimal . share) s)) }
0 (x | X)($d | [a b c d e f]| [A B C D E F]) + (u | U) { tok (\p s -> PT p (eitherResIdent (T_HexUnsigned . share) s)) }
0 (x | X)($d | [a b c d e f]| [A B C D E F]) + (l | L) { tok (\p s -> PT p (eitherResIdent (T_HexLong . share) s)) }
0 (x | X)($d | [a b c d e f]| [A B C D E F]) + (u l | U L) { tok (\p s -> PT p (eitherResIdent (T_HexUnsLong . share) s)) }
0 [0 1 2 3 4 5 6 7]* { tok (\p s -> PT p (eitherResIdent (T_Octal . share) s)) }
0 [0 1 2 3 4 5 6 7]* (u | U) { tok (\p s -> PT p (eitherResIdent (T_OctalUnsigned . share) s)) }
0 [0 1 2 3 4 5 6 7]* (l | L) { tok (\p s -> PT p (eitherResIdent (T_OctalLong . share) s)) }
0 [0 1 2 3 4 5 6 7]* (u l | U L) { tok (\p s -> PT p (eitherResIdent (T_OctalUnsLong . share) s)) }
($d + \. | \. $d +)((e | E)\- ? $d +)? | $d + (e | E)\- ? $d + | $d + \. $d + E \- ? $d + { tok (\p s -> PT p (eitherResIdent (T_JDouble . share) s)) }
($d + \. $d + | $d + \. | \. $d +)((e | E)\- ? $d +)? (f | F)| $d + (e | E)\- ? $d + (f | F) { tok (\p s -> PT p (eitherResIdent (T_JFloat . share) s)) }
($d + \. $d + | $d + \. | \. $d +)((e | E)\- ? $d +)? (l | L)| $d + (e | E)\- ? $d + (l | L) { tok (\p s -> PT p (eitherResIdent (T_JLongDouble . share) s)) }
\' \\ u ($d | [a b c d e f]| [A B C D E F]) ($d | [a b c d e f]| [A B C D E F]) ($d | [a b c d e f]| [A B C D E F]) ($d | [a b c d e f]| [A B C D E F]) \' { tok (\p s -> PT p (eitherResIdent (T_UnicodeChar . share) s)) }
\' ($u # [\' \\]| \\ [\' \\ n t r]) \' { tok (\p s -> PT p (eitherResIdent (T_JChar . share) s)) }

$l $i*   { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }
\" ([$u # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t)))* \"{ tok (\p s -> PT p (TL $ share $ unescapeInitTail s)) }

$d+      { tok (\p s -> PT p (TI $ share s))    }
$d+ \. $d+ (e (\-)? $d+)? { tok (\p s -> PT p (TD $ share s)) }

{

tok f p s = f p s

share :: String -> String
share = id

data Tok =
   TS !String     -- reserved words and symbols
 | TL !String     -- string literals
 | TI !String     -- integer literals
 | TV !String     -- identifiers
 | TD !String     -- double precision float literals
 | TC !String     -- character literals
 | T_Unsigned !String
 | T_Long !String
 | T_UnsignedLong !String
 | T_Hexadecimal !String
 | T_HexUnsigned !String
 | T_HexLong !String
 | T_HexUnsLong !String
 | T_Octal !String
 | T_OctalUnsigned !String
 | T_OctalLong !String
 | T_OctalUnsLong !String
 | T_JDouble !String
 | T_JFloat !String
 | T_JLongDouble !String
 | T_UnicodeChar !String
 | T_JChar !String

 deriving (Eq,Show,Ord)

data Token = 
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

tokenPos (PT (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

posLineCol (Pn _ l c) = (l,c)
mkPosToken t@(PT p _) = (posLineCol p, prToken t)

prToken t = case t of
  PT _ (TS s) -> s
  PT _ (TI s) -> s
  PT _ (TV s) -> s
  PT _ (TD s) -> s
  PT _ (TC s) -> s
  PT _ (T_Unsigned s) -> s
  PT _ (T_Long s) -> s
  PT _ (T_UnsignedLong s) -> s
  PT _ (T_Hexadecimal s) -> s
  PT _ (T_HexUnsigned s) -> s
  PT _ (T_HexLong s) -> s
  PT _ (T_HexUnsLong s) -> s
  PT _ (T_Octal s) -> s
  PT _ (T_OctalUnsigned s) -> s
  PT _ (T_OctalLong s) -> s
  PT _ (T_OctalUnsLong s) -> s
  PT _ (T_JDouble s) -> s
  PT _ (T_JFloat s) -> s
  PT _ (T_JLongDouble s) -> s
  PT _ (T_UnicodeChar s) -> s
  PT _ (T_JChar s) -> s

  _ -> show t

data BTree = N | B String Tok BTree BTree deriving (Show)

eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

resWords = b "int" (b "double" (b "catch" (b "break" (b "boolean" (b "abstract" N N) N) (b "case" (b "byte" N N) N)) (b "continue" (b "class" (b "char" N N) N) (b "do" (b "default" N N) N))) (b "float" (b "false" (b "extends" (b "else" N N) N) (b "finally" (b "final" N N) N)) (b "implements" (b "if" (b "for" N N) N) (b "instanceof" (b "import" N N) N)))) (b "static" (b "package" (b "native" (b "long" (b "interface" N N) N) (b "null" (b "new" N N) N)) (b "public" (b "protected" (b "private" N N) N) (b "short" (b "return" N N) N))) (b "throws" (b "synchronized" (b "switch" (b "super" N N) N) (b "throw" (b "this" N N) N)) (b "try" (b "true" (b "transient" N N) N) (b "while" (b "volatile" N N) N))))
   where b s = B s (TS s)

unescapeInitTail :: String -> String
unescapeInitTail = unesc . tail where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A divide and conquer wrapper.
-------------------------------------------------------------------

type State = Int
newtype Transition  = Trans {getSeq :: State -> (Seq PartToken,State)}
data Size       = Size Int
type LexTree    = FingerTree (Transition,Size) Char
data PartToken = Token { lexeme      :: String
                       , token_id    :: [AlexAcc (Posn -> String -> Tok) ()]}

instance Monoid Size where
  mempty = Size 0
  Size m `mappend` Size n = Size (m+n)

instance Monoid Transition where
  mempty = Trans $ \_ -> (mempty,-1)
  mappend = combineTokens

instance F.Measured (Transition,Size) Char where
  measure c =
    let bytes = encode c
    in (Trans $ \is -> case foldl automata is bytes of
      -1 -> (singleton (Token [c] [AlexAccNone]),-1)
      os -> (singleton (Token [c] (alex_accept ! os)),os),Size 1)

combineTokens :: Transition -> Transition -> Transition
combineTokens toks1 toks2 = Trans $ \in_state ->
  let (seq1,mid_state) = getSeq toks1 $ in_state
      append seq1 = let (seq2,out_state) = getSeq toks2 $ head startState
                    in (appendTokens seq1 seq2,out_state)
  in case (mid_state,getSeq toks2 $ mid_state) of
    (-1,_) -> -- Illegal left hand-side
      if isSingle seq1 in_state
      then error $ "Illegal character: " ++ show lastToken 
      else (mempty,-1) 
    (_,(_,-1)) -> -- Uncombinable tokens
      if isAccepting seq1
      then append seq1
      else if isSingle seq1 in_state
           then error $ "Unfinnished token: " ++ show lastToken
           else (mempty,-1) -- This is an illegal substring
    (_,(seq2,out_state)) -> if isAccepting seq2
                            then (mergeTokens seq1 seq2,out_state)
                            else (mempty,-1)

mergeTokens :: Seq PartToken -> Seq PartToken -> Seq PartToken
mergeTokens toks1 toks2 = 
  let toks1' :> token1 = viewr toks1
      token2 :< toks2' = viewl toks2
  in (toks1' |> Token (lexeme token1 ++ lexeme token2) (token_id token2)) >< toks2'

appendTokens :: Seq PartToken -> Seq PartToken -> Seq PartToken
appendTokens = mappend

makeTree :: String -> LexTree
makeTree = F.fromList

treeToTokens :: LexTree -> Transition
treeToTokens  = fst . F.measure

isAccepting :: Seq Token -> Bool
isAccepting toks = case token_id tok of 
  AlexAccNone -> False
  _           -> True
  where _ :> tok = viewr toks

isSingle :: Seq PartToken -> Int -> Bool
isSingle seq1 0 = let _ :> tok = viewr seq1
                  in Prelude.length (lexeme tok) == 1
isSingle _ _ = False

insertAtIndex :: String -> Int -> LexTree -> LexTree
insertAtIndex str i tree = 
  if i < 0
  then error "index must be >= 0"
  else l F.>< (lexCode str) F.>< r
     where (l,r) = F.split (\(_,Size n) -> n>i) tree

automata :: Int -> Byte -> Int
automata s c = let base   = alex_base ! s
                   ord_c  = fromEnum c
                   offset = base + ord_c
                   check  =  alex_check ! offset
               in if (offset >= (0)) && (check == ord_c)
                  then alex_table ! offset
                  else alex_deflt ! s

encode :: Char -> [Word8]
encode  = map fromIntegral . go . fromEnum
 where
  go oc
   | oc <= 0x7f       = [oc]
   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]
   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

-- Show instances for testing purposes

instance Show Transition where
  show = show . fst . (flip getSeq) 0

instance Show PartToken where
  show (Token lex accs) = case map (\acc -> case acc of 
    AlexAcc f -> show $ f (Pn 0 0 0) lex
    AlexAccSkip -> "Skip:" ++ show lex) accs of
                            [] -> "No Token:" ++ show lex ++ "\n"
                            toks -> unlines toks
}