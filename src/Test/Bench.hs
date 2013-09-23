{-# LANGUAGE FlexibleInstances #-}
module Main where

import Prelude as P hiding (mapM_)
import System.Environment
import System.IO
import Criterion.Main
import Criterion.Config
import Test.Java as J
import Test.Lexjava
import Control.DeepSeq
import Data.FingerTree as F
import Data.Map as M (foldrWithKey',insert,empty)
import Data.Foldable (mapM_)
import Data.Sequence as S hiding (zip)
import Data.Monoid

instance NFData J.LexTree where
  rnf = (flip seq) () . unsafeFmap rnf

allTest :: [String]
allTest = ["Alex","IncLex","Update"]

main = do
  args <- getArgs
  let (inFiles,tests',arg) = sortArgs args
  (files,codes) <- case inFiles of
    [] -> return (["internal"],[core002])
    _  -> do lazyCode <- mapM (\f -> openFile f ReadMode >>= hGetContents) inFiles
             return (inFiles,force lazyCode)
  let tests = case tests' of
        [] -> allTest
        _  -> tests'
      codes' = concat . P.take 20 $ repeat codes
      trees' = map J.lexCode codes
      trees = map (\tree -> map ((uncurry (F.><)) . flip splitTreeAt tree) [0..size tree]) trees'
      trees'' = map (\tree -> tree !! (P.length trees `div` 2)) trees
      sizeTrees = map (\tree -> foldl (helperIncBuilder tree) [] [1..10]) trees'
      testFuns =
        [ ("Alex",map (benchStuff alexScanTokens) (zip codes' files))
        , ("IncLex",map (benchStuff (J.tokens . J.lexCode)) (zip codes' files))
        , ("Update",map (benchStuff J.tokens) (zip trees'' files))
        , ("Sizes",[bgroup name (map (benchStuff (\tree' -> J.tokens (tree' <> tree'))) (tree))
                   | (tree,name) <- zip sizeTrees files])
        , ("AllUp",[bgroup name (map (benchStuff J.tokens) (zip tree (map show [0..])))
                   | (tree,name) <- zip trees files ])
        ]
  trees' `deepseq` sizeTrees `deepseq` withArgs (tests ++ arg) $
    defaultMain [ bgroup name tests | (name,tests) <- testFuns ]

repeatTree :: Int -> LexTree -> LexTree
repeatTree 0 _ = mempty
repeatTree 1 tree = tree
repeatTree i tree = tree <> repeatTree (i-1) tree

helperIncBuilder :: LexTree -> [(LexTree,String)] -> Int -> [(LexTree,String)]
helperIncBuilder tree sizeTrees i = sizeTrees ++ [(repeatTree i tree, show i)]

benchStuff :: Show b => (a -> b) -> (a,String) -> Benchmark
benchStuff f (x,name) = bench name $ nf (show . f) x

sortArgs :: [String] -> ([String],[String],[String])
sortArgs args = foldl sortArg ([],[],[]) args
  where sortArg (files,tests,args) arg@('-':_) = (files,tests,arg:args)
        sortArg (files,tests,args) ('+':test)  = (files,if test `elem` "Sizes":"AllUp":allTest
                                                        then test:tests
                                                        else tests
                                                  ,args)
        sortArg (files,tests,args) file        = (file:files,tests,args)

core002 :: String
core002 = "/*@ @@*/\n\n" ++
          "// hej\n" ++
          "int foo.thur() {\n" ++
          "printString(\"foo\");\n" ++
          "return if true then else if 1 else null;\n" ++
          "}"
          
simpleTest = mapM_ putStrLn $ fmap (J.prToken) $ treeToTokens $ makeTree core002

