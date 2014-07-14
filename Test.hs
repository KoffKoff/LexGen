module Test where

import Text.Printf
import System.IO

--f :: Integer -> Integer
--f n = sum sum1
--  where sum1 = [n - s2 | s2 <- takeWhile (<= n) pow]

check2 x = sum (map (flip (-) 1) (take x pow)) == 2^(x) - x - 1

check x = sum (take x pow) == 2^x -1

pow = [2^y | y <- [0..]]

lol :: [String] -> IO ()
lol xs = writeFile "data.csv" $ printf "%-25s%s" "n" "f(n)" ++ "\n" ++ unlines xs

--pps :: [String]
--pps = map pp [10^x | x <- [0..]]
--  where pp n = printf "%-25d%d" n (f n)