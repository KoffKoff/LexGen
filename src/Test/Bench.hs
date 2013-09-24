{-# LANGUAGE FlexibleInstances #-}
module Main where

import Prelude as P hiding (mapM_,foldl1)
import System.Environment
import System.IO
import Criterion.Main
import Criterion.Config
import Test.Java as J
import Test.Lexjava
import Control.DeepSeq
import Data.FingerTree as F
import Data.Map as M (foldrWithKey',insert,empty)
import Data.Foldable (mapM_,foldl1)
import Data.Sequence as S hiding (zip)
import Data.Monoid
import Data.Char
import Data.Array

instance (NFData v,NFData a,Measured v a) => NFData (FingerTree v a) where
  rnf tree = foldl1 (seq . rnf) tree `seq` rnf (measure tree)

instance NFData a => NFData (Seq a) where
  rnf = flip seq () . foldl1 (seq . rnf)
   
instance NFData a => NFData (Sum a) where
  rnf (Sum a) = rnf a

instance NFData Tokens where
  rnf (Tokens toks suff _) = rnf toks `seq` rnf suff
  rnf _ = ()

instance NFData IntToken where
  rnf (Token lex _) = rnf lex

instance NFData Suffix where
  rnf (Str s) = rnf s
  rnf (One t) = rnf t
  rnf (Multi toks) = rnf toks

allTest :: [String]
allTest = ["Alex","Update"]

main = do
  args <- getArgs
  let (n,inFiles,tests',arg) = sortArgs args
  (files,codes) <- case inFiles of
    [] -> return (["internal"],[core002])
    _  -> do lazyCode <- mapM (\f -> openFile f ReadMode >>= hGetContents) inFiles
             return (inFiles,force lazyCode)
  let tests = case tests' of
        [] -> allTest
        _  -> tests'
      codes' = map (concat . P.take n . repeat) codes  -- multiply the size of each file by 20
      trees' = map J.lexCode codes' -- lexing the input using the bottom-up method
      trees = do
        code <- codes'
        let tree = J.lexCode code
            halfSize = size tree `div` 2
            (left,right) = splitTreeAt halfSize tree
        return (left F.>< right)
      measures = do
        code <- codes'
        let tree = J.lexCode code
            halfSize = size tree `div` 2
            (left,right) = splitTreeAt halfSize tree
        return (measure left, measure right)
      -- trees'' = map (\tree -> tree !! (P.length trees `div` 2)) trees
      sizeTrees = map (\tree -> foldl (helperIncBuilder tree) [] [1..10]) trees'
      testFuns =
        [ ("Alex",map (benchStuff (show . alexScanTokens)) (zip codes' files))
        , ("IncLex",map (benchStuff (J.lexCode)) (zip codes' files))
        -- , ("Update",map (benchStuff J.tokens) (zip trees files))
        , ("Update;Measures",map (benchStuff (uncurry mappend)) (zip measures files))
--        , ("Sizes",[bgroup name (map (benchStuff (\tree' -> J.tokens (tree' <> tree'))) (tree))
--                    | (tree,name) <- zip sizeTrees files])
--        , ("AllUp",[bgroup name (map (benchStuff J.tokens) (zip tree (map show [0..])))
--                    | (tree,name) <- zip trees files ])
        ]
  measures `deepseq` withArgs (tests ++ arg) $
    defaultMain [ bgroup name tests | (name,tests) <- testFuns ]

repeatTree :: Int -> LexTree -> LexTree
repeatTree 0 _ = mempty
repeatTree 1 tree = tree
repeatTree i tree = tree <> repeatTree (i-1) tree

helperIncBuilder :: LexTree -> [(LexTree,String)] -> Int -> [(LexTree,String)]
helperIncBuilder tree sizeTrees i = sizeTrees ++ [(repeatTree i tree, show i)]

benchStuff :: NFData b => (a -> b) -> (a,String) -> Benchmark
benchStuff f (x,name) = bench name $ nf f x

sortArgs :: [String] -> (Int,[String],[String],[String])
sortArgs args = foldl sortArg (10,[],[],[]) args
  where sortArg (num,files,tests,args) arg@('-':_) =
          (num,files,tests,(arg):args)
        sortArg (num,files,tests,args) ('+':test)  =
          (num,files,if test `elem` "Sizes":"AllUp":"IncLex":allTest
                     then test:tests
                     else tests
                               ,args)
        sortArg (num,files,tests,args) arg | and $ map isDigit arg = (read arg,files,tests,args)
                                           | otherwise = (num,arg:files,tests,args)

core002 :: String
core002 = "/*@ @@*/\n\n" ++
          "// hej\n" ++
          "int foo.thur() {\n" ++
          "printString(\"foo\");\n" ++
          "return if true then else if 1 else null;\n" ++
          "}"
          
simpleTest = mapM_ putStrLn $ fmap (J.prToken) $ treeToTokens $ makeTree core002

