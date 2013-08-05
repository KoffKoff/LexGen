{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Environment
import System.IO
import Criterion.Main
import Criterion.Config
import Test.Java
import Test.Lexjava
import Control.DeepSeq
import Data.FingerTree
import Data.Map as M (foldrWithKey',insert,empty)

instance NFData LexTree where
  rnf = (flip seq) () . unsafeFmap rnf

main = do
  args <- getArgs
  let (inFiles,arg) = sortArgs args
  (files,codes) <- case inFiles of
    [] -> return (["internal"],[core002])
    _ -> do lazyCode <- mapM (\f -> openFile f ReadMode >>= hGetContents) inFiles
            return (inFiles,force lazyCode)
  let trees = force $ map lexCode codes
  withArgs ("IncLex":"Alex":"Update":arg) $
    defaultMain
    [ bgroup "Alex" $ map (benchStuff alexScanTokens) (zip codes files)
    , bgroup "IncLex" $ map (benchStuff (tokens . lexCode)) (zip codes files)
    , bgroup "Update" $ map (benchStuff tokens) (zip trees files)
    ]

benchStuff :: Show b => (a -> b) -> (a,String) -> Benchmark
benchStuff f (x,name) = bench name $ nf (show . f) x

sortArgs :: [String] -> ([String],[String])
sortArgs args = foldl sortArg ([],[]) args
  where sortArg (files,args) arg@('-':_) = (files,arg:args)
        sortArg (files,args) file        = (file:files,args)

core002 :: String
core002 = "/*@ @@*/\n\n" ++
          "// hej\n" ++
          "int foo.thur() {\n" ++
          "printString(\"foo\");\n" ++
          "return if true then else if 1 else null;\n" ++
          "}"