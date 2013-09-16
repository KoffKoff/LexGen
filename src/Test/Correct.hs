module Test.Correct where

import Prelude as P
import Test.Java as J
import Test.Lexjava as A
import Data.FingerTree as F
import Data.Foldable (toList)
import Data.Monoid
import Data.List (inits,tails)

splits xs = zip (inits xs) (tails xs)

test_string x = unlines $ map show $ 
  [ (xs,ys,y', measureToTokens $ measure (makeTree xs) <> measure(makeTree ys)
               <> measure (makeTree y')) | 
    (_,x') <- splits x,
    (y,y') <- splits x',
    (xs,ys) <- splits y]

splitToTree :: String -> [(LexTree,LexTree)]
splitToTree s = map (splitToTree' s) [0..length s]
  where splitToTree' str i = let (pre,suf) = splitAt i str
                             in (makeTree pre,makeTree suf)

mergeTrees :: LexTree -> LexTree -> LexTree
mergeTrees t1 t2 = t1 F.>< t2

treeToList :: LexTree -> [J.Token]
treeToList tree = toList $ treeToTokens tree

checkMerge :: String -> [Bool]
checkMerge str = let incToks = map (treeToList . uncurry mergeTrees) (splitToTree str)
                     seqToks = alexScanTokens str
                     checkToks incT = checkListToken incT seqToks
                 in map checkToks incToks

treeSplitter :: LexTree -> [(LexTree,LexTree)]
treeSplitter tree = map (flip splitTreeAt tree) [0..n]
  where Size n = snd (measure tree)

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
  where checker' (J.TS s1) (A.TS s2) = s1 == s2
        checker' (J.TL s1) (A.TL s2) = s1 == s2
        checker' (J.TI s1) (A.TI s2) = s1 == s2
        checker' (J.TV s1) (A.TV s2) = s1 == s2
        checker' (J.TD s1) (A.TD s2) = s1 == s2
        checker' (J.TC s1) (A.TC s2) = s1 == s2
        checker' (J.T_Unsigned s1) (A.T_Unsigned s2) = s1 == s2
        checker' (J.T_Long s1) (A.T_Long s2) = s1 == s2
        checker' (J.T_UnsignedLong s1) (A.T_UnsignedLong s2) = s1 == s2
        checker' (J.T_Hexadecimal s1) (A.T_Hexadecimal s2) = s1 == s2
        checker' (J.T_HexUnsigned s1) (A.T_HexUnsigned s2) = s1 == s2
        checker' (J.T_HexLong s1) (A.T_HexLong s2) = s1 == s2
        checker' (J.T_HexUnsLong s1) (A.T_HexUnsLong s2) = s1 == s2
        checker' (J.T_Octal s1) (A.T_Octal s2) = s1 == s2
        checker' (J.T_OctalUnsigned s1) (A.T_OctalUnsigned s2) = s1 == s2
        checker' (J.T_OctalLong s1) (A.T_OctalLong s2) = s1 == s2
        checker' (J.T_OctalUnsLong s1) (A.T_OctalUnsLong s2) = s1 == s2
        checker' (J.T_JDouble s1) (A.T_JDouble s2)= s1 == s2
        checker' (J.T_JFloat s1) (A.T_JFloat s2) = s1 == s2
        checker' (J.T_JLongDouble s1) (A.T_JLongDouble s2) = s1 == s2
        checker' (J.T_UnicodeChar s1) (A.T_UnicodeChar s2) = s1 == s2
        checker' (J.T_JChar s1) (A.T_JChar s2) = s1 == s2
        checker' _ _ = False

core002 :: String
core002 = "/* a comment */\n\n" ++
          "// hej\n" ++
          "int foo.thur() {\n" ++
          "printString(foo);\n" ++
          "return if true then else if 1 else null;\n" ++
          "}"
