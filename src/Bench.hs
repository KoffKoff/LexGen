module Main where

import System.Environment
import System.IO
import Criterion.Main
import Java
--import Lexjava

main = do
  args <- getArgs
  let (files,arg) = sortArgs args
  lexTrees <- mapM lexFile files
--  codes <- mapM (flip . openFile ReadMode >>= hGetContents) files
  withArgs ("IncLex"{-:"Alex"-}:arg) $
    defaultMain [ bgroup "IncLex" $
                  map (\(tree,file) -> bench file $ nf (show . tokens) tree) (zip lexTrees files)
--                , bgroup "Alex" $
--                  map (\(tree,file) -> bench file $ nf (show . tokens) tree) (zip codes files)
                ]

sortArgs :: [String] -> ([String],[String])
sortArgs args = foldl sortArg ([],[]) args
  where sortArg (files,args) arg@('-':_) = (files,arg:args)
        sortArg (files,args) file        = (file:files,args)