module Test.MemTest where

import System.Environment
import System.IO (readFile)
import Data.ByteString.Lazy (writeFile)
import Test.Java as J hiding (encode)
import Test.Lexjava
import Data.FingerTree
import Data.Monoid
import Data.Binary as Bin
import Prelude hiding (writeFile)
import Control.Monad

main = do
  args <- getArgs
  print "usage: MemTets <iterations> <code file> <output file(s)>"
  memTest (read $ head args) (args !! 1) (args !! 2)

memReader :: Int -> String -> String -> IO ()
memReader i inFile outFile = do
  code <- readFile inFile
  memTest i code outFile

memTest :: Int -> String -> String -> IO ()
memTest iter code outFile = do
  fileWriter iter (outFile ++ ".inclex") (makeTree code) measure

fileWriter :: (Binary b,Monoid a) => Int -> String -> a -> (a -> b) -> IO ()
fileWriter i file code f | i <= 0 = return ()
                         | otherwise = do
  writeFile (file ++ "." ++ show i) $ (encode . f) code
  fileWriter (i-1) file (code <> code) f

-- Serialization for the Tokens data structure
instance Binary Tokens where
  put NoTokens = put (0 :: Word8)
  put (InvalidTokens seq) = do put (1 :: Word8)
                               put seq
  put (Tokens seq suff int) = do put (2 :: Word8)
                                 put seq
                                 put suff
                                 put int

  get = do tag <- getWord8
           case tag of
             0 -> return NoTokens
             1 -> liftM InvalidTokens get
             2 -> liftM3 Tokens get get get

instance Binary Suffix where
  put (Str seq) = do put (0 :: Word8)
                     put seq
  put (One tok) = do put (1 :: Word8)
                     put tok
  put (Multi toks) = do put (2 :: Word8)
                        put toks

  get = do tag <- getWord8
           case tag of
             0 -> liftM Str get
             1 -> liftM One get
             2 -> liftM Multi get

instance Binary IntToken where
  put (Token seq accs) = do put seq
                            put accs

  get = liftM2 Token get get

instance Binary (J.AlexAcc a b) where
  put (J.AlexAcc a) = put (0 :: Word8)
  put J.AlexAccSkip = put (1 :: Word8)
  
  get = do tag <- getWord8
           case tag of
             0 -> return J.AlexAccSkip
             1 -> return J.AlexAccSkip

instance Binary a => Binary (Sum a) where
  put (Sum a) = put a

  get = get

core002 :: String
core002 = "/*@ @@*/\n\n" ++
          "// hej\n" ++
          "int foo.thur() {\n" ++
          "printString(\"foo\");\n" ++
          "return if true then else if 1 else null;\n" ++
          "}"