module Test where

import Text.Printf
import System.IO

f :: Integer -> Integer
f n = s1 0
  where s1 x | s2 x 0 < n  = n - s2 x 0 + s1 (x+1)
             | otherwise = 0
        s2 x y | y < x = 2^y + s2 x (y+1)
        s2 _ _ | True  = 0

lol :: [String] -> IO ()
lol xs = writeFile "data.csv" $ printf "%-25s%s" "n" "f(n)" ++ "\n" ++ unlines xs

pps :: [String]
pps = map pp [10^x | x <- [0..]]
  where pp n = printf "%-25d%d" n (f n)