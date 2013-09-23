module Main where

import Criterion.Main
import Data.Sequence hiding (zip)
import Test.QuickCheck
import Data.Monoid ((<>))
import Control.DeepSeq
import qualified Data.Foldable as F

--instance NFData a => NFData (FingerTree a) where
--    rnf (Empty) = ()
--    rnf (Single x) = rnf x
--    rnf (Deep _ pr m sf) = rnf pr `seq` rnf m `seq` rnf sf

main :: IO ()
main = do
  seqs <- sample' $ funChar [10,100,1000,10000,100000]
  let seqs' = head seqs
      benchies = [bench (show i) $ nf (show . uncurry (<>)) seqs' | (i,seqs') <- seqs' ]
  print seqs'
  defaultMain [bgroup "appendTest" benchies]

funChar :: [Int] -> Gen [(Int,(Seq Char,Seq Char))]
funChar = fun

fun :: Arbitrary a => [Int] -> Gen [(Int,(Seq a,Seq a))]
fun [] = return []
fun (i:is) = do
  seqs <- fun is
  list1 <- vectorOf i arbitrary
  list2 <- vectorOf i arbitrary
  return $ (i,(fromList list1,fromList list2)) : seqs