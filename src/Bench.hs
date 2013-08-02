{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Environment
import System.IO
import Criterion.Main
import Criterion.Config
import Java
import Lexjava
import Control.DeepSeq
import Data.FingerTree
import Data.Map as M (foldrWithKey',insert,empty)

instance NFData LexTree where
  rnf = (flip seq) () . unsafeFmap rnf

main = do
  args <- getArgs
  let (files,arg) = sortArgs args
  codesLazy <- mapM (\f -> openFile f ReadMode >>= hGetContents) files
  let trees = force $ map lexCode codes
      codes = force codesLazy
  withArgs ("IncLex":"Alex":"Update":arg) $
    defaultMain --With defaultConfig (startParams codes trees)
    [ bgroup "Alex" $ map (benchStuff alexScanTokens) (zip codes files)
    , bgroup "IncLex" $ map (benchStuff (tokens . lexCode)) (zip codes files)
    , bgroup "Update" $ map (benchStuff tokens) (zip trees files)
    ]
{-  where --startParams :: [String] -> [LexTree] -> Criterion ()
        startParams codes trees = return ()-}

benchStuff :: Show b => (a -> b) -> (a,String) -> Benchmark
benchStuff f (x,name) = bench name $ nf (show . f) x

sortArgs :: [String] -> ([String],[String])
sortArgs args = foldl sortArg ([],[]) args
  where sortArg (files,args) arg@('-':_) = (files,arg:args)
        sortArg (files,args) file        = (file:files,args)
