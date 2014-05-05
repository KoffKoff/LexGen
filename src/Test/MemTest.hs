module Test.MemTest where

import System.Environment
import System.IO (readFile)
import Data.ByteString.Lazy (writeFile)
import Test.Java hiding (encode)
import Test.Lexjava
import Data.FingerTree
import Data.Monoid
import Data.Binary as Bin
import Prelude hiding (writeFile)

main = do
  args <- getArgs
  print "usage: MemTets <iterations> <code file> <output file(s)>"
  memTest (read $ head args) (args !! 1) (args !! 2)

memTest :: Int -> String -> String -> IO ()
memTest iter inFile outFile = do
  code <- readFile inFile
  fileWriter iter (outFile ++ ".inclex") (makeTree code) measure

fileWriter :: (Binary b,Monoid a) => Int -> String -> a -> (a -> b) -> IO ()
fileWriter i file code f | i <= 0 = return ()
                         | otherwise = do
  writeFile (file ++ "." ++ show i) $ (encode . f) code
  fileWriter (i-1) file (code <> code) f

instance Binary Tokens where
  put NoTokens = put (0 :: Word8)
  put (InvalidTokens seq) = do put (1 :: Word8)
                               put seq
  put (Tokens seq suff int) = do put (2 :: Word8)
                                 put seq
                                 put suff
                                 put int

  get = return NoTokens

instance Binary Suffix where
  put (Str seq) = do put (0 :: Word8)
                     put seq
  put (One tok) = do put (1 :: Word8)
                     put tok
  put (Multi toks) = do put (2 :: Word8)
                        put toks

  get = return (Str mempty)

instance Binary IntToken where
  put (Token seq accs) = put seq

  get = return (Token mempty [])

instance Binary a => Binary (Sum a) where
  put (Sum a) = put a

  get = get