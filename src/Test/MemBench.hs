{-# LANGUAGE FlexibleInstances #-}
module Main where

import Prelude as P hiding (mapM_,foldl1)
import System.Environment
import System.IO
import Test.Java as J
import Test.Lexjava as A
import Control.DeepSeq
import Data.FingerTree as F
import Data.Map as M (foldrWithKey',insert,empty)
import Data.Foldable (mapM_,foldl1)
import Data.Sequence as S hiding (zip)
import Data.Monoid
import Data.Char
import Data.Array

instance NFData A.Token where
  rnf = rnf . A.prToken

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

testSizes :: [Int]
testSizes = 10:[100,200..1900]

main = do
  args <- getArgs
  let (n,inFile,arg) = sortArgs args
  code <- case inFile of
    "" -> return core002
    _  -> openFile inFile ReadMode >>= hGetContents
  let code' = concat . P.take n $ repeat code
--      tokens = {-# SCC "alex" #-} alexScanTokens code
      tree = {-# SCC "inc" #-} makeTree code'
  print . getOutState $ measure tree
--  print $ last tokens

sortArgs args = foldl sortArg (10,[],[]) args
  where sortArg (num,files,args) arg@('-':_) = (num,files,(arg):args)
        sortArg (num,files,args) arg | and $ map isDigit arg = (read arg,files,args)
                                     | otherwise = (num,arg,args)

getOutState :: (Table State Tokens,Size) -> Int
getOutState (table,_) = case access table 0 of
  Tokens _ _ s -> s
  NoTokens -> 0
  _ -> -1

core002 :: String
core002 = "/*@ @@*/\n\n" ++
          "// hej\n" ++
          "int foo.thur() {\n" ++
          "printString(\"foo\");\n" ++
          "return if true then else if 1 else null;\n" ++
          "}"
