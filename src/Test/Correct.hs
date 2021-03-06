module Test.Correct where

import Prelude as P
import Test.Java as J
import Test.Lexjava as A
import Data.FingerTree as F
import Data.Foldable (toList)
import Data.Monoid
import Data.List (inits,tails)
import Test.QuickCheck
import Data.Tuple (swap)

-- Checks wether if 2 strings lexes to the same if they are merged before or
-- after the lexing
propMerge :: String -> String -> Bool
propMerge s1 s2 = treeToTokens merge == treeToTokens preMerged
  where merge = makeTree s1 <> makeTree s2
        preMerged = makeTree (s1 <> s2)
        tokens = access (fst $ measure preMerged) 0

-- Checks so that a string lexes to something accepting (can be no tokens)
propResult :: String -> Bool
propResult s = isAccepting $ access (fst . measure $ makeTree s) 0

subStrings :: String -> Gen (String,String)
subStrings s = do
  m <- choose (0,length s)
  l <- choose (0,m)
  r <- choose (m,length s)
  return (drop l $ take m s, drop m $ take r s)

subShrinker :: Eq a => ([a],[a]) -> [([a],[a])]
subShrinker ([],[]) = [([],[])]
subShrinker ([],bs) = zip (repeat []) (listShrinker bs)
subShrinker (as,[]) = zip (listShrinker as) (repeat [])
subShrinker (as,bs) = [(a,b) | a <- [as,tail as], b <- [bs,init bs]
                             , (a,b) /= (as,bs)]

listShrinker :: [a] -> [[a]]
listShrinker [] = []
listShrinker [a] = []
listShrinker as = [init as,tail as]

propMergeResult :: String -> Property
propMergeResult s = forAllShrink (subStrings s) subShrinker $ \(s1,s2) ->
  propResult (s1 ++ s2) .&&. propMerge s1 s2

splits xs = zip (inits xs) (tails xs)

test_string x = unlines $ map show $ 
  [ (xs,ys, measureToTokens $ measure (makeTree xs) <> measure(makeTree ys))
  | (x',y) <- splits x,
    (xs,ys) <- splits y]

splitToTree :: String -> [(LexTree,LexTree)]
splitToTree s = map (splitToTree' s) [0..length s]
  where splitToTree' str i = let (pre,suf) = splitAt i str
                             in (makeTree pre,makeTree suf)

checkSplitMerge :: String -> [Bool]
checkSplitMerge str = let tree = makeTree str
                          incToks = map (treeToList . uncurry (<>)) (treeSplitter tree)
                          seqToks = alexScanTokens str
                          checkToks incT = checkListToken incT seqToks
                      in map checkToks incToks

treeToList :: LexTree -> [J.Token]
treeToList tree = toList $ treeToTokens tree

checkMerge :: String -> [Bool]
checkMerge str = let incToks = map (treeToList . uncurry (<>)) (splitToTree str)
                     seqToks = alexScanTokens str
                     checkToks incT = checkListToken incT seqToks
                 in map checkToks incToks

treeSplitter :: LexTree -> [(LexTree,LexTree)]
treeSplitter tree = map (flip splitTreeAt tree) [0..size tree]

checkSplit :: String -> [(Bool,Bool)]
checkSplit str = let strSplits = map (flip splitAt str) [0..length str]
                     treeSplits = treeSplitter $ makeTree str
                 in map (uncurry bla) $ zip treeSplits strSplits
  where bla (tree1,tree2) (str1,str2) = (checkListToken (treeToList tree1) (alexScanTokens str1)
                                        ,checkListToken (treeToList tree2) (alexScanTokens str2))

checkListToken :: [J.Token] -> [A.Token] -> Bool
checkListToken tok1 tok2 = and $ map (uncurry checker) $ zip tok1 tok2

checker :: J.Token -> A.Token -> Bool
checker (J.PT _ tok1) (A.PT _ tok2) = checker' tok1 tok2
  where checker' (J.TS s1) (A.TS s2) = toList s1 == s2
        checker' (J.TL s1) (A.TL s2) = toList s1 == s2
        checker' (J.TI s1) (A.TI s2) = toList s1 == s2
        checker' (J.TV s1) (A.TV s2) = toList s1 == s2
        checker' (J.TD s1) (A.TD s2) = toList s1 == s2
        checker' (J.TC s1) (A.TC s2) = toList s1 == s2
        checker' (J.T_Unsigned s1) (A.T_Unsigned s2) = toList s1 == s2
        checker' (J.T_Long s1) (A.T_Long s2) = toList s1 == s2
        checker' (J.T_UnsignedLong s1) (A.T_UnsignedLong s2) = toList s1 == s2
        checker' (J.T_Hexadecimal s1) (A.T_Hexadecimal s2) = toList s1 == s2
        checker' (J.T_HexUnsigned s1) (A.T_HexUnsigned s2) = toList s1 == s2
        checker' (J.T_HexLong s1) (A.T_HexLong s2) = toList s1 == s2
        checker' (J.T_HexUnsLong s1) (A.T_HexUnsLong s2) = toList s1 == s2
        checker' (J.T_Octal s1) (A.T_Octal s2) = toList s1 == s2
        checker' (J.T_OctalUnsigned s1) (A.T_OctalUnsigned s2) = toList s1 == s2
        checker' (J.T_OctalLong s1) (A.T_OctalLong s2) = toList s1 == s2
        checker' (J.T_OctalUnsLong s1) (A.T_OctalUnsLong s2) = toList s1 == s2
        checker' (J.T_JDouble s1) (A.T_JDouble s2)= toList s1 == s2
        checker' (J.T_JFloat s1) (A.T_JFloat s2) = toList s1 == s2
        checker' (J.T_JLongDouble s1) (A.T_JLongDouble s2) = toList s1 == s2
        checker' (J.T_UnicodeChar s1) (A.T_UnicodeChar s2) = toList s1 == s2
        checker' (J.T_JChar s1) (A.T_JChar s2) = toList s1 == s2
        checker' _ _ = False

hurp :: Tokens -> Suffix
hurp (Tokens _ suff _) = suff
hurp _ = Str mempty

core002 :: String
core002 = "import datastructures.*;\n" ++
          "/* a comment */\n\n" ++
          "// hej\n" ++
          "int foo.thur() {\n" ++
          "printString(foo);\n" ++
          "return if true then else if 1 else null;\n" ++
          "}"
