{-# LANGUAGE FlexibleInstances #-}
module Main where

import Prelude hiding (mapM_)
import System.Environment
import System.IO
import Criterion.Main
import Criterion.Config
import Test.Java as J
-- import Test.JavaOld as JO
import Test.Lexjava
import Control.DeepSeq
import Data.FingerTree
import Data.Map as M (foldrWithKey',insert,empty)
import Data.Foldable (mapM_)

instance NFData J.LexTree where
  rnf = (flip seq) () . unsafeFmap rnf

allTest :: [String]
allTest = ["Alex","IncLex","Update","JavaOld"]

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
  let trees = force $ map J.lexCode codes
      testFuns =
        [ map (benchStuff alexScanTokens) (zip codes files)
        , map (benchStuff (J.tokens . J.lexCode)) (zip codes files)
        , map (benchStuff J.tokens) (zip trees files)
        -- , map (benchStuff (JO.tokens . JO.lexCode)) (zip codes files)
        ]
  trees `deepseq` withArgs (tests ++ arg) $
    defaultMain [ bgroup name tests | (name,tests) <- zip allTest testFuns ]

    
benchStuff :: Show b => (a -> b) -> (a,String) -> Benchmark
benchStuff f (x,name) = bench name $ nf (show . f) x

sortArgs :: [String] -> ([String],[String],[String])
sortArgs args = foldl sortArg ([],[],[]) args
  where sortArg (files,tests,args) arg@('-':_) = (files,tests,arg:args)
        sortArg (files,tests,args) ('+':test)  = (files,if test `elem` allTest
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

